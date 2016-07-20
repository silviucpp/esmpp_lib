-module(esmpp_lib_worker).
-author('Alexander Zhuk <aleksandr.zhuk@privatbank.ua>').

-include("esmpp_lib.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([submit/2, data_sm/2, unbind/1, query_sm/2, cancel_sm/2, replace_sm/2]).

start_link(Param) ->
    gen_server:start_link(?MODULE, Param, []).

-spec submit(WorkerPid::pid(), List::list()) -> {ok, SegmentsSequenceNumber::list(), Segments::integer()} | {error, Reason::term()}.
submit(WorkerPid, List) ->
    gen_server:call(WorkerPid, {submit, List}, infinity).

-spec data_sm(WorkerPid::pid(), List::list()) -> {ok, SegmentsSequenceNumber::list(), Segments::integer()} | {error, Reason::term()}.
data_sm(WorkerPid, List) ->
    gen_server:call(WorkerPid, {data_sm, List}, infinity).

-spec query_sm(WorkerPid::pid(), List::list()) -> {ok, SequenceNumber::integer()} | {error, Reason::term()}.
query_sm(WorkerPid, List) ->
    gen_server:call(WorkerPid, {query_sm, List}).

-spec replace_sm(WorkerPid::pid(), List::list()) ->  {ok, SequenceNumber::integer()} | {error, Reason::term()}.
replace_sm(WorkerPid, List) ->
    gen_server:call(WorkerPid, {replace_sm, List}).

-spec cancel_sm(WorkerPid::pid(), List::list()) -> {ok, SequenceNumber::integer()} | {error, Reason::term()}.
cancel_sm(WorkerPid, List) ->
    gen_server:call(WorkerPid, {cancel_sm, List}).

-spec unbind(pid()) -> ok.  
unbind(WorkerPid) ->
    gen_server:cast(WorkerPid, {unbind, []}).


init(Param) ->
    WorkerPid = self(),
    {ok, _} = timer:send_after(10, {bind, esmpp_utils:lookup(mode, Param)}),
    {ok, ProcessingPid} = esmpp_lib_submit_processing:start_link([{parent_pid, WorkerPid} | Param]),
    State = [{processing_pid, ProcessingPid}, {sar, 0}, {seq_n, 0}, {worker_pid, WorkerPid} | Param],
    {ok,  State}.

handle_call({submit, List}, _From, State) ->
    SmsList = esmpp_lib_encoder:encode(submit_sm, State, List),

    case send_sms(SmsList, State) of
        {ok, LastSeqNumber, MsgSegmentsSeqNumbers} ->
            {reply, {ok, MsgSegmentsSeqNumbers, length(SmsList)}, replace_seq_num(State, LastSeqNumber)};
        UnexpectedResponse ->
            {reply, UnexpectedResponse, State}
    end;
handle_call({data_sm, List}, _From, State) ->
    SmsList = esmpp_lib_encoder:encode(data_sm, State, List),

    case send_sms([SmsList], State) of
        {ok, LastSeqNumber, MsgSegmentsSeqNumbers} ->
            {reply, {ok, MsgSegmentsSeqNumbers, 1}, replace_seq_num(State, LastSeqNumber)};
        UnexpectedResponse ->
            {reply, UnexpectedResponse, State}
    end;
handle_call({query_sm, List}, _From, State) ->
    Transport = get_transport(State),
    Socket = esmpp_utils:lookup(socket, State),
    Bin = esmpp_lib_encoder:encode(query_sm, State, List),
    SequenceNumber = esmpp_utils:lookup(seq_n, State),

    case send(Transport, Socket, Bin) of
        ok ->
            {reply, {ok, SequenceNumber}, accumulate_seq_num(State)};
        UnexpectedResponse ->
            {reply, UnexpectedResponse, State}
    end;
handle_call({replace_sm, List}, _From, State) ->
    Transport = get_transport(State),
    Socket = esmpp_utils:lookup(socket, State),
    Bin = esmpp_lib_encoder:encode(replace_sm, State, List),
    SequenceNumber = esmpp_utils:lookup(seq_n, State),

    case send(Transport, Socket, Bin) of
        ok ->
            {reply, {ok, SequenceNumber}, accumulate_seq_num(State)};
        UnexpectedResponse ->
            {reply, UnexpectedResponse, State}
    end;
handle_call({cancel_sm, List}, _From, State) ->
    Transport = get_transport(State),
    Socket = esmpp_utils:lookup(socket, State),
    Bin = esmpp_lib_encoder:encode(cancel_sm, State, List),
    SequenceNumber = esmpp_utils:lookup(seq_n, State),

    case send(Transport, Socket, Bin) of
        ok ->
            {reply, {ok, SequenceNumber}, accumulate_seq_num(State)};
        UnexpectedResponse ->
            {reply, UnexpectedResponse, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({unbind, []}, State) ->
    Transport = get_transport(State),
    HandlerPid = esmpp_utils:lookup(handler_pid, State),
    WorkerPid = esmpp_utils:lookup(worker_pid, State),
    Socket = esmpp_utils:lookup(socket, State),
    Bin = esmpp_lib_encoder:encode(unbind, State),
    ok = try_send(Transport, Socket, Bin, WorkerPid, HandlerPid),
    esmpp_utils:send_notification(HandlerPid, {unbind, WorkerPid}),
    WorkerPid ! {terminate, unbind},
    {noreply, accumulate_seq_num(State)}; 
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({bind, Mode}, Param) ->
    Transport = get_transport(Param),
    WorkerPid = esmpp_utils:lookup(worker_pid, Param),
    ProcessingPid = esmpp_utils:lookup(processing_pid, Param),
    HandlerPid = esmpp_utils:lookup(handler_pid, Param),
    State1 = case bind(Mode, Param) of
        {error, Reason} ->
            esmpp_utils:send_notification(HandlerPid, {network_error, WorkerPid, Reason}),
            WorkerPid ! {terminate, Reason},
            Param;
        Socket ->
            esmpp_utils:send_notification(HandlerPid, {bind_completed, WorkerPid}),
            Param1 = accumulate_seq_num([{socket, Socket}|Param]),
            ListenPid = spawn_link(fun() -> loop_tcp(<<>>, Transport, Socket, WorkerPid, HandlerPid, ProcessingPid) end),
            case esmpp_utils:lookup(enquire_timeout, Param1) of
                undefined ->
                    ok;
                _ ->
                    spawn_link(fun() -> enquire_link(Param1) end)
            end,
            [{mode, Mode},{listen_pid, ListenPid}|Param1]
    end,
    {noreply, State1};
handle_info({update_state, {Name, NewEntry}}, State) ->
    {noreply, esmpp_utils:replace(Name, NewEntry, State)};
handle_info({get_state, ListenPid}, State) ->
    ListenPid ! {state, State},
    {noreply, State};
handle_info({terminate, Reason}, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    ?LOG_INFO("Massage for nothing ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, State) -> 
    ?LOG_INFO("Process terminate with reason ~p state is ~p~n", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Function Definitions

bind(Mode, Param) ->
    Transport = get_transport(Param),
    case connect(Param) of
        {error, Reason} ->
            {error, Reason};
        {_, Socket} ->
            Bin = esmpp_lib_encoder:encode(Mode, Param),
            Resp = Transport:send(Socket, Bin),
            case handle_bind(Resp, Socket, Transport) of
                ok ->
                    ?LOG_INFO("Socket ~p mode ~p~n", [Socket, Mode]),
                    Socket;
                {error, Reason} ->    
                    {error, Reason}
            end
    end. 

connect(Param) ->
    Transport = get_transport(Param),
    Ip = esmpp_utils:lookup(host, Param),
    Port = esmpp_utils:lookup(port, Param),
    Transport:connect(Ip, Port, [binary, {active, false}, {keepalive, true}, {reuseaddr, true}, {packet, 0}], 2000).

handle_bind(Resp, Socket, Transport) ->
    case Resp of
        ok ->
            exam_bind_resp(Socket, Transport);
        {error, Reason} ->
            {error, Reason}
    end.

send_sms(List, State) ->
    Transport = get_transport(State),
    WorkerPid = esmpp_utils:lookup(worker_pid, State),
    ProcessingPid = esmpp_utils:lookup(processing_pid, State),
    Socket = esmpp_utils:lookup(socket, State),
    HandlerPid = esmpp_utils:lookup(handler_pid, State),
    SequenceNumber = esmpp_utils:lookup(seq_n, State),
    send_sms(List, SequenceNumber, WorkerPid, ProcessingPid, Socket, HandlerPid, Transport, []).

send_sms([Bin|T], NextSeqNumber, WorkerPid, ProcessingPid, Socket, HandlerPid, Transport, AccSeq) ->
    <<_:12/binary, NextSeqNumber:32/integer, _/binary>> = Bin,

    case send(Transport, Socket, Bin) of
        ok ->
            esmpp_lib_submit_processing:push_submit(ProcessingPid, NextSeqNumber, esmpp_utils:now()),
            send_sms(T, get_next_seq_nr(NextSeqNumber), WorkerPid, ProcessingPid, Socket, HandlerPid, Transport, [NextSeqNumber | AccSeq]);
        UnexpectedResponse ->
            UnexpectedResponse
    end;

send_sms([], NextSeqNumber, _WorkerPid, _ProcessingPid, _Socket, _HandlerPid, _Transport, AccSeq) ->
    {ok, NextSeqNumber, lists:reverse(AccSeq)}.

loop_tcp(Buffer, Transport, Socket, WorkerPid, HandlerPid, ProcessingPid) ->
    case Transport:recv(Socket, 0) of 
        {ok, Bin} ->
            try esmpp_lib_decoder:decode(<<Buffer/bitstring, Bin/bitstring>>, []) of
                [{undefined, Name}|_] ->
                    ?LOG_WARNING("Unsupported smpp packet ~p~n", [Name]),
                    loop_tcp(<<>>, Transport, Socket, WorkerPid, HandlerPid, ProcessingPid);
                List ->
                    ok = create_resp(List, Transport, Socket, WorkerPid, HandlerPid, ProcessingPid),
                    loop_tcp(<<>>, Transport, Socket, WorkerPid, HandlerPid, ProcessingPid)
            catch
                _Class:Reason ->
                    case byte_size(Bin)>1535 of
                        true ->
                            esmpp_utils:send_notification(HandlerPid, {decoder_error, WorkerPid, Bin}),
                            WorkerPid ! {terminate, Reason};
                        false ->
                            loop_tcp(<<Buffer/bitstring, Bin/bitstring>>, Transport, Socket, WorkerPid, HandlerPid, ProcessingPid)
                    end
            end;
        {error, Reason} ->
            esmpp_utils:send_notification(HandlerPid, {network_error, WorkerPid, Reason}),
            WorkerPid ! {terminate, Reason}
    end.              

create_resp([], _Transport, _Socket, _WorkerPid, _HandlerPid, _ProcessingPid) ->
    ok;
create_resp([H|T], Transport, Socket, WorkerPid, HandlerPid, ProcessingPid) ->
	{Name, Code, SeqNum, List} = H,
    Resp = assemble_resp({Name, Code, SeqNum, List}, Socket, WorkerPid, HandlerPid, ProcessingPid),
    case Resp of
        ok ->
            ok;
        {close_session, Bin} ->
            ok = try_send(Transport, Socket, Bin, WorkerPid, HandlerPid),
            esmpp_utils:send_notification(HandlerPid, {network_error, Socket, close_session});
        _ ->
            ok = try_send(Transport, Socket, Resp, WorkerPid, HandlerPid)
    end,
    create_resp(T, Transport, Socket, WorkerPid, HandlerPid, ProcessingPid).

assemble_resp({Name, Status, SeqNum, List}, Socket, WorkerPid, HandlerPid, ProcessingPid) ->
    case Name of
        enquire_link -> 
            esmpp_lib_encoder:encode(enquire_link_resp, [], [{sequence_number, SeqNum}]);
        enquire_link_resp ->
            ok; 
        deliver_sm -> 
            MsgId = esmpp_utils:lookup(receipted_message_id, List),
            esmpp_utils:send_notification(HandlerPid, {deliver_sm, WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            esmpp_lib_encoder:encode(deliver_sm_resp, [], [{sequence_number, SeqNum}, {message_id, MsgId}, {status, 0}]); 
        submit_sm_resp ->
            Params = [{sequence_number, SeqNum}, {command_status, Status} | List],
            esmpp_lib_submit_processing:processing_submit(ProcessingPid, SeqNum, Params, submit_sm_resp),
            ok;
        data_sm_resp ->
            Params = [{sequence_number, SeqNum}, {command_status, Status} | List],
            esmpp_lib_submit_processing:processing_submit(ProcessingPid, SeqNum, Params, data_sm_resp),
            ok; 
        data_sm ->                                                                                  
            MsgId = esmpp_utils:lookup(receipted_message_id, List),
            esmpp_utils:send_notification(HandlerPid, {data_sm, WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            esmpp_lib_encoder:encode(data_sm_resp, [], [{sequence_number, SeqNum}, {message_id, MsgId}, {status, 0}]);
        query_sm_resp ->
            esmpp_utils:send_notification(HandlerPid, {query_sm_resp, WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            ok;
        replace_sm_resp ->
            esmpp_utils:send_notification(HandlerPid, {replace_sm_resp, WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            ok;
        cancel_sm_resp ->
            esmpp_utils:send_notification(HandlerPid, {cancel_sm_resp, WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            ok;
        alert_notification ->
            ok;
        outbind ->
            esmpp_utils:send_notification(HandlerPid, {outbind, WorkerPid, Socket});
        generic_nack ->
            ?LOG_ERROR("Generic nack error code ~p~n", [Status]);
        unbind_resp ->
            ?LOG_ERROR("Unbind session ~p~n", [WorkerPid]),
            Bin = esmpp_lib_encoder:encode(unbind_resp, [], [{sequence_number, SeqNum}]),
            {close_session, Bin};
        unbind ->
            ?LOG_ERROR("Unbind session ~p~n", [WorkerPid]),
            Bin = esmpp_lib_encoder:encode(unbind_resp, [], [{sequence_number, SeqNum}]),
            {close_session, Bin}
    end.

exam_bind_resp(Socket, Transport) ->
    case Transport:recv(Socket, 0, 5000) of 
        {ok, Bin} ->
            [{_Name, Code, _SeqNum, _List}] = esmpp_lib_decoder:decode(Bin, []),
            case Code of 
                0 ->
                    ok;
                Resp ->
                    ?LOG_ERROR("Error bind packet code ~p ~n", [Code]),
                    {error, Resp}  
            end;
        {error, Reason} ->
            ?LOG_ERROR("Error bind, tcp connect fail ~p ~n", [Reason]),  
            {error, Reason}
    end.

enquire_link(State) ->
    EnquireTimeout = esmpp_utils:lookup(enquire_timeout, State)*1000,
    ok = timer:sleep(EnquireTimeout),
    Transport = get_transport(State),
    Socket = esmpp_utils:lookup(socket, State),
    WorkerPid = esmpp_utils:lookup(worker_pid, State),
    HandlerPid = esmpp_utils:lookup(handler_pid, State),
    Bin = esmpp_lib_encoder:encode(enquire_link, State),
    ok = try_send(Transport, Socket, Bin, WorkerPid, HandlerPid),
    enquire_link(accumulate_seq_num(State)).                       
    
get_transport(Param) ->
    case esmpp_utils:lookup(transport, Param) of
        tcp -> gen_tcp;
        undefined -> gen_tcp;
        ssl -> ssl
    end.
     
accumulate_seq_num(State) ->
    replace_seq_num(State, next_seq_num(State)).

replace_seq_num(State, SeqNumber) ->
    esmpp_utils:replace(seq_n, SeqNumber, State).

next_seq_num(State) ->
    get_next_seq_nr(esmpp_utils:lookup(seq_n, State)).

get_next_seq_nr(Nr) ->
    case Nr of
        999999 ->
            1;
        _ ->
            Nr + 1
    end.
     
try_send(Transport, Socket, Bin, WorkerPid, HandlerPid) ->
    case Transport:send(Socket, Bin) of
        ok -> ok;
        {error, Reason} ->
            esmpp_utils:send_notification(HandlerPid, {network_error, WorkerPid, Reason}),
            WorkerPid ! {terminate, Reason}, 
            ok
    end.

send(Transport, Socket, Bin) ->
    case Transport:send(Socket, Bin) of
        ok ->
            ok;
        {error, Reason} = UnexpectedError ->
            self() ! {terminate, Reason},
            UnexpectedError
    end.