-module(esmpp_connection).
-author('Alexander Zhuk <aleksandr.zhuk@privatbank.ua>').

-include("esmpp.hrl").
-include("esmpp_records.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([update_sar/2]).
-export([submit/2, data_sm/2, cancel_sm/2, replace_sm/2, query_sm/2, unbind/1]).

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
    ConnectionPid = self(),
    HandlerPid = esmpp_utils:lookup(handler_pid, Param),
    SubmitTimeout = esmpp_utils:lookup(submit_timeout, Param, 60),

    erlang:send_after(10, self(), binding),
    {ok, ProcessingPid} = esmpp_submit_queue:start_link([HandlerPid, ConnectionPid, SubmitTimeout]),

    State = #conn_state {
        processing_pid = ProcessingPid,
        connection_pid = ConnectionPid,
        sar = 0,
        seq_n = 0,

        host = esmpp_utils:lookup(host, Param),
        port = esmpp_utils:lookup(port, Param),
        password = esmpp_utils:lookup(password, Param),
        system_id = esmpp_utils:lookup(system_id, Param),
        transport = get_transport(esmpp_utils:lookup(transport, Param)),
        interface_version = esmpp_utils:lookup(interface_version, Param),
        enquire_timeout = esmpp_utils:lookup(enquire_timeout, Param),
        submit_timeout = SubmitTimeout,
        system_type = esmpp_utils:lookup(system_type, Param),
        service_type = esmpp_utils:lookup(service_type, Param),
        addr_ton = esmpp_utils:lookup(addr_ton, Param),
        addr_npi = esmpp_utils:lookup(addr_npi, Param),
        source_addr_ton = esmpp_utils:lookup(source_addr_ton, Param),
        source_addr_npi = esmpp_utils:lookup(source_addr_npi, Param),
        dest_addr_ton = esmpp_utils:lookup(dest_addr_ton, Param),
        dest_addr_npi = esmpp_utils:lookup(dest_addr_npi, Param),
        mode = esmpp_utils:lookup(mode, Param),
        data_coding = esmpp_utils:lookup(data_coding, Param),
        handler_pid = HandlerPid
    },

    {ok, State}.

handle_call({submit, List}, _From, State) ->
    SmsList = esmpp_encoder:encode(submit_sm, State, List),

    case send_sms(SmsList, State) of
        {ok, LastSeqNumber, MsgSegmentsSeqNumbers} ->
            {reply, {ok, MsgSegmentsSeqNumbers, length(SmsList)}, State#conn_state{seq_n = LastSeqNumber}};
        UnexpectedResponse ->
            {reply, UnexpectedResponse, State}
    end;
handle_call({data_sm, List}, _From, State) ->
    SmsList = esmpp_encoder:encode(data_sm, State, List),

    case send_sms([SmsList], State) of
        {ok, LastSeqNumber, MsgSegmentsSeqNumbers} ->
            {reply, {ok, MsgSegmentsSeqNumbers, 1}, State#conn_state{seq_n = LastSeqNumber}};
        UnexpectedResponse ->
            {reply, UnexpectedResponse, State}
    end;
handle_call({query_sm, List}, _From, State) ->
    Bin = esmpp_encoder:encode(query_sm, State, List),

    case send(State#conn_state.transport, State#conn_state.socket, Bin) of
        ok ->
            {reply, {ok, State#conn_state.seq_n}, accumulate_seq_num(State)};
        UnexpectedResponse ->
            {reply, UnexpectedResponse, State}
    end;
handle_call({replace_sm, List}, _From, State) ->
    Bin = esmpp_encoder:encode(replace_sm, State, List),

    case send(State#conn_state.transport, State#conn_state.socket, Bin) of
        ok ->
            {reply, {ok, State#conn_state.seq_n}, accumulate_seq_num(State)};
        UnexpectedResponse ->
            {reply, UnexpectedResponse, State}
    end;
handle_call({cancel_sm, List}, _From, State) ->
    Bin = esmpp_encoder:encode(cancel_sm, State, List),

    case send(State#conn_state.transport, State#conn_state.socket, Bin) of
        ok ->
            {reply, {ok, State#conn_state.seq_n}, accumulate_seq_num(State)};
        UnexpectedResponse ->
            {reply, UnexpectedResponse, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({unbind, []}, State) ->
    Bin = esmpp_encoder:encode(unbind, State),
    ok = try_send(State#conn_state.transport, State#conn_state.socket, Bin, State#conn_state.connection_pid, State#conn_state.handler_pid),
    esmpp_utils:send_notification(State#conn_state.handler_pid, {unbind, State#conn_state.connection_pid}),
    State#conn_state.connection_pid ! {terminate, unbind},
    {noreply, accumulate_seq_num(State)}; 
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(binding, State) ->
    case bind(State) of
        {error, Reason} ->
            esmpp_utils:send_notification(State#conn_state.handler_pid, {network_error, State#conn_state.connection_pid, Reason}),
            State#conn_state.connection_pid ! {terminate, Reason},
            {noreply, State};
        Socket ->
            esmpp_utils:send_notification(State#conn_state.handler_pid, {bind_completed, State#conn_state.connection_pid}),

            ListenPid = spawn_link(fun() -> loop_tcp(<<>>, State#conn_state.transport, Socket, State#conn_state.connection_pid, State#conn_state.handler_pid, State#conn_state.processing_pid) end),
            case State#conn_state.enquire_timeout of
                undefined ->
                    ok;
                _ ->
                    spawn_link(fun() -> enquire_link(accumulate_seq_num(State)) end)
            end,

            {noreply, State#conn_state {socket = Socket, listen_pid = ListenPid, seq_n = esmpp_utils:get_next_sequence_number(State#conn_state.seq_n)}}
    end;
handle_info({update_sar, Value}, State) ->
    {noreply, State#conn_state{sar = Value}};
handle_info({terminate, Reason}, State) ->
    {stop, Reason, State};
handle_info(Info, State) ->
    ?LOG_INFO("received unexpected message ~p", [Info]),
    {noreply, State}.

terminate(Reason, State) -> 
    ?LOG_INFO("connection process terminate with reason ~p state is ~p", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Function Definitions

update_sar(Pid, Value) ->
    Pid ! {update_sar, Value}.

bind(State) ->
    Transport = State#conn_state.transport,
    ConnectionOptions = [binary, {active, false}, {keepalive, true}, {reuseaddr, true}, {packet, 0}],

    case Transport:connect(State#conn_state.host, State#conn_state.port, ConnectionOptions, 2000) of
        {error, Reason} ->
            {error, Reason};
        {_, Socket} ->
            Bin = esmpp_encoder:encode(State#conn_state.mode, State),
            Resp = Transport:send(Socket, Bin),
            case handle_bind(Resp, Socket, Transport) of
                ok ->
                    ?LOG_INFO("socket ~p mode ~p~n", [Socket, State#conn_state.mode]),
                    Socket;
                {error, Reason} ->    
                    {error, Reason}
            end
    end. 


handle_bind(Resp, Socket, Transport) ->
    case Resp of
        ok ->
            exam_bind_resp(Socket, Transport);
        {error, Reason} ->
            {error, Reason}
    end.

send_sms(List, State) ->
    Transport = State#conn_state.transport,
    WorkerPid = State#conn_state.connection_pid,
    ProcessingPid = State#conn_state.processing_pid,
    Socket = State#conn_state.socket,
    HandlerPid = State#conn_state.handler_pid,
    SequenceNumber = State#conn_state.seq_n,
    send_sms(List, SequenceNumber, WorkerPid, ProcessingPid, Socket, HandlerPid, Transport, []).

send_sms([Bin|T], NextSeqNumber, WorkerPid, ProcessingPid, Socket, HandlerPid, Transport, AccSeq) ->
    <<_:12/binary, NextSeqNumber:32/integer, _/binary>> = Bin,

    case send(Transport, Socket, Bin) of
        ok ->
            esmpp_submit_queue:push_submit(ProcessingPid, NextSeqNumber, esmpp_utils:now()),
            send_sms(T, esmpp_utils:get_next_sequence_number(NextSeqNumber), WorkerPid, ProcessingPid, Socket, HandlerPid, Transport, [NextSeqNumber | AccSeq]);
        UnexpectedResponse ->
            UnexpectedResponse
    end;

send_sms([], NextSeqNumber, _WorkerPid, _ProcessingPid, _Socket, _HandlerPid, _Transport, AccSeq) ->
    {ok, NextSeqNumber, lists:reverse(AccSeq)}.

loop_tcp(Buffer, Transport, Socket, WorkerPid, HandlerPid, ProcessingPid) ->
    case Transport:recv(Socket, 0) of 
        {ok, Bin} ->
            try esmpp_decoder:decode(<<Buffer/bitstring, Bin/bitstring>>) of
                [{undefined, Name}|_] ->
                    ?LOG_WARNING("unsupported smpp packet ~p", [Name]),
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
            esmpp_encoder:encode(enquire_link_resp, [], [{sequence_number, SeqNum}]);
        enquire_link_resp ->
            ok; 
        deliver_sm -> 
            MsgId = esmpp_utils:lookup(receipted_message_id, List),
            esmpp_utils:send_notification(HandlerPid, {deliver_sm, WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            esmpp_encoder:encode(deliver_sm_resp, [], [{sequence_number, SeqNum}, {message_id, MsgId}, {status, 0}]);
        submit_sm_resp ->
            Params = [{sequence_number, SeqNum}, {command_status, Status} | List],
            esmpp_submit_queue:process_submit(ProcessingPid, SeqNum, Params, submit_sm_resp),
            ok;
        data_sm_resp ->
            Params = [{sequence_number, SeqNum}, {command_status, Status} | List],
            esmpp_submit_queue:process_submit(ProcessingPid, SeqNum, Params, data_sm_resp),
            ok; 
        data_sm ->                                                                                  
            MsgId = esmpp_utils:lookup(receipted_message_id, List),
            esmpp_utils:send_notification(HandlerPid, {data_sm, WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            esmpp_encoder:encode(data_sm_resp, [], [{sequence_number, SeqNum}, {message_id, MsgId}, {status, 0}]);
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
            ?LOG_ERROR("generic nack error code ~p", [Status]);
        unbind_resp ->
            ?LOG_ERROR("unbind session ~p", [WorkerPid]),
            Bin = esmpp_encoder:encode(unbind_resp, [], [{sequence_number, SeqNum}]),
            {close_session, Bin};
        unbind ->
            ?LOG_ERROR("unbind session ~p", [WorkerPid]),
            Bin = esmpp_encoder:encode(unbind_resp, [], [{sequence_number, SeqNum}]),
            {close_session, Bin}
    end.

exam_bind_resp(Socket, Transport) ->
    case Transport:recv(Socket, 0, 5000) of 
        {ok, Bin} ->
            [{_Name, Code, _SeqNum, _List}] = esmpp_decoder:decode(Bin),
            case Code of 
                0 ->
                    ok;
                Resp ->
                    ?LOG_ERROR("error bind packet code ~p", [Code]),
                    {error, Resp}  
            end;
        {error, Reason} ->
            ?LOG_ERROR("error bind, tcp connect fail ~p", [Reason]),
            {error, Reason}
    end.

enquire_link(State) ->
    ok = timer:sleep(State#conn_state.enquire_timeout*1000),
    Bin = esmpp_encoder:encode(enquire_link, State),
    ok = try_send(State#conn_state.transport, State#conn_state.socket, Bin, State#conn_state.connection_pid, State#conn_state.handler_pid),
    enquire_link(accumulate_seq_num(State)).                       
    
get_transport(Transport) ->
    case Transport of
        undefined ->
            gen_tcp;
        tcp ->
            gen_tcp;
        ssl ->
            ssl
    end.
     
accumulate_seq_num(State) ->
    State#conn_state { seq_n = esmpp_utils:get_next_sequence_number(State#conn_state.seq_n) }.

try_send(Transport, Socket, Bin, WorkerPid, HandlerPid) ->
    case Transport:send(Socket, Bin) of
        ok ->
            ok;
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