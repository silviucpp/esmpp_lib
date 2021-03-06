-module(esmpp_connection).
-author('Alexander Zhuk <aleksandr.zhuk@privatbank.ua>').

-include("esmpp.hrl").
-include("esmpp_records.hrl").

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([update_sar/2]).
-export([submit_sm/2, data_sm/2, cancel_sm/2, replace_sm/2, query_sm/2, unbind/1]).

start_link(Param) ->
    gen_server:start_link(?MODULE, Param, []).

-spec submit_sm(ConnectionPid::pid(), List::list()) -> {ok, SegmentsSequenceNumber::list(), Segments::integer()} | {error, Reason::term()}.
submit_sm(ConnectionPid, List) ->
    gen_server:call(ConnectionPid, {submit_sm, List}, infinity).

-spec data_sm(ConnectionPid::pid(), List::list()) -> {ok, SegmentsSequenceNumber::list(), Segments::integer()} | {error, Reason::term()}.
data_sm(ConnectionPid, List) ->
    gen_server:call(ConnectionPid, {data_sm, List}, infinity).

-spec query_sm(ConnectionPid::pid(), List::list()) -> {ok, SequenceNumber::integer()} | {error, Reason::term()}.
query_sm(ConnectionPid, List) ->
    gen_server:call(ConnectionPid, {query_sm, List}).

-spec replace_sm(ConnectionPid::pid(), List::list()) ->  {ok, SequenceNumber::integer()} | {error, Reason::term()}.
replace_sm(ConnectionPid, List) ->
    gen_server:call(ConnectionPid, {replace_sm, List}).

-spec cancel_sm(ConnectionPid::pid(), List::list()) -> {ok, SequenceNumber::integer()} | {error, Reason::term()}.
cancel_sm(ConnectionPid, List) ->
    gen_server:call(ConnectionPid, {cancel_sm, List}).

-spec unbind(pid()) -> ok.  
unbind(ConnectionPid) ->
    gen_server:cast(ConnectionPid, {unbind, []}).

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

handle_call({submit_sm, List}, _From, State) ->
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
    ?LOG_INFO(<<"send unbind on connection: ~p">>, [State#conn_state.connection_pid]),
    send(State#conn_state.transport, State#conn_state.socket, esmpp_encoder:encode(unbind, State)),
    %we are the one asking for closing the connection. make less noise in logs by closing normal
    State#conn_state.connection_pid ! {terminate, normal},
    {noreply, accumulate_seq_num(State)}; 
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(binding, State) ->
    case bind(State) of
        {error, Reason} ->
            esmpp_utils:send_notification(State#conn_state.handler_pid, {bind_failed, State#conn_state.connection_pid, Reason}),
            State#conn_state.connection_pid ! {terminate, Reason},
            {noreply, State};
        Socket ->
            esmpp_utils:send_notification(State#conn_state.handler_pid, {bind_completed, State#conn_state.connection_pid}),

            ListenPid = spawn_link(fun() -> loop_tcp(<<>>, State#conn_state.transport, Socket, State#conn_state.connection_pid, State#conn_state.handler_pid, State#conn_state.processing_pid) end),
            State2 = State#conn_state {socket = Socket, listen_pid = ListenPid, seq_n = esmpp_utils:get_next_sequence_number(State#conn_state.seq_n)},

            case State#conn_state.enquire_timeout of
                undefined ->
                    ok;
                _ ->
                    spawn_link(fun() -> enquire_link(State2) end)
            end,

            {noreply, State2}
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
    ConnectionPid = State#conn_state.connection_pid,
    ProcessingPid = State#conn_state.processing_pid,
    Socket = State#conn_state.socket,
    HandlerPid = State#conn_state.handler_pid,
    SequenceNumber = State#conn_state.seq_n,
    send_sms(List, SequenceNumber, ConnectionPid, ProcessingPid, Socket, HandlerPid, Transport, []).

send_sms([Bin|T], NextSeqNumber, ConnectionPid, ProcessingPid, Socket, HandlerPid, Transport, AccSeq) ->
    <<_:12/binary, NextSeqNumber:32/integer, _/binary>> = Bin,

    case send(Transport, Socket, Bin) of
        ok ->
            esmpp_submit_queue:push(ProcessingPid, NextSeqNumber, esmpp_utils:now()),
            send_sms(T, esmpp_utils:get_next_sequence_number(NextSeqNumber), ConnectionPid, ProcessingPid, Socket, HandlerPid, Transport, [NextSeqNumber | AccSeq]);
        UnexpectedResponse ->
            UnexpectedResponse
    end;

send_sms([], NextSeqNumber, _ConnectionPid, _ProcessingPid, _Socket, _HandlerPid, _Transport, AccSeq) ->
    {ok, NextSeqNumber, lists:reverse(AccSeq)}.

loop_tcp(Buffer, Transport, Socket, ConnectionPid, HandlerPid, ProcessingPid) ->
    case Transport:recv(Socket, 0) of 
        {ok, Bin} ->
            try esmpp_decoder:decode(<<Buffer/bitstring, Bin/bitstring>>) of
                [{undefined, Name}|_] ->
                    ?LOG_WARNING("unsupported smpp packet ~p", [Name]),
                    loop_tcp(<<>>, Transport, Socket, ConnectionPid, HandlerPid, ProcessingPid);
                List ->
                    ok = create_resp(List, Transport, Socket, ConnectionPid, HandlerPid, ProcessingPid),
                    loop_tcp(<<>>, Transport, Socket, ConnectionPid, HandlerPid, ProcessingPid)
            catch
                _Class:Reason ->
                    case byte_size(Bin) > 1535 of
                        true ->
                            ?LOG_ERROR("failed to decode ~p on connection ~p", [Bin, ConnectionPid]),
                            ConnectionPid ! {terminate, Reason};
                        false ->
                            loop_tcp(<<Buffer/bitstring, Bin/bitstring>>, Transport, Socket, ConnectionPid, HandlerPid, ProcessingPid)
                    end
            end;
        {error, Reason} ->
            ?LOG_ERROR("connection: ~p read error: ~p", [ConnectionPid, Reason]),
            ConnectionPid ! {terminate, Reason}
    end.

create_resp([], _Transport, _Socket, _ConnectionPid, _HandlerPid, _ProcessingPid) ->
    ok;
create_resp([H|T], Transport, Socket, ConnectionPid, HandlerPid, ProcessingPid) ->
	{Name, Code, SeqNum, List} = H,

    case assemble_resp({Name, Code, SeqNum, List}, ConnectionPid, HandlerPid, ProcessingPid) of
        ok ->
            ok;
        {close_session, Bin} ->
            send(Transport, Socket, Bin);
        Resp ->
            send(Transport, Socket, Resp)
    end,
    create_resp(T, Transport, Socket, ConnectionPid, HandlerPid, ProcessingPid).

assemble_resp({Name, Status, SeqNum, List}, ConnectionPid, HandlerPid, ProcessingPid) ->
    case Name of
        enquire_link -> 
            esmpp_encoder:encode(enquire_link_resp, [], [{sequence_number, SeqNum}]);
        enquire_link_resp ->
            ok; 
        deliver_sm -> 
            MsgId = esmpp_utils:lookup(receipted_message_id, List),
            esmpp_utils:send_notification(HandlerPid, {deliver_sm, ConnectionPid, [{sequence_number, SeqNum}, {command_status, Status} | List]}),
            esmpp_encoder:encode(deliver_sm_resp, [], [{sequence_number, SeqNum}, {message_id, MsgId}, {status, 0}]);
        submit_sm_resp ->
            Params = [{sequence_number, SeqNum}, {command_status, Status} | List],
            esmpp_submit_queue:process(ProcessingPid, SeqNum, Params, submit_sm_resp),
            ok;
        data_sm_resp ->
            Params = [{sequence_number, SeqNum}, {command_status, Status} | List],
            esmpp_submit_queue:process(ProcessingPid, SeqNum, Params, data_sm_resp),
            ok; 
        data_sm ->                                                                                  
            MsgId = esmpp_utils:lookup(receipted_message_id, List),
            esmpp_utils:send_notification(HandlerPid, {data_sm, ConnectionPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            esmpp_encoder:encode(data_sm_resp, [], [{sequence_number, SeqNum}, {message_id, MsgId}, {status, 0}]);
        query_sm_resp ->
            esmpp_utils:send_notification(HandlerPid, {query_sm_resp, ConnectionPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            ok;
        replace_sm_resp ->
            esmpp_utils:send_notification(HandlerPid, {replace_sm_resp, ConnectionPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            ok;
        cancel_sm_resp ->
            esmpp_utils:send_notification(HandlerPid, {cancel_sm_resp, ConnectionPid, [{sequence_number, SeqNum}, {command_status, Status}|List]}),
            ok;
        alert_notification ->
            ok;
        outbind ->
            esmpp_utils:send_notification(HandlerPid, {outbind, ConnectionPid});
        generic_nack ->
            ?LOG_ERROR("generic nack error code ~p", [Status]);
        unbind_resp ->
        	%we don't care if we get this or not after sending the unbind we close the connection instantly
        	ok;
        unbind ->
            ?LOG_ERROR("received unbind connection ~p", [ConnectionPid]),
            esmpp_utils:send_notification(ConnectionPid, {unbind, ConnectionPid}),
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
    case send(State#conn_state.transport, State#conn_state.socket, Bin) of
        ok ->
            enquire_link(accumulate_seq_num(State));
        _ ->
            ok
    end.

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

send(Transport, Socket, Bin) ->
    case Transport:send(Socket, Bin) of
        ok ->
            ok;
        {error, Reason} = UnexpectedError ->
            self() ! {terminate, Reason},
            UnexpectedError
    end.