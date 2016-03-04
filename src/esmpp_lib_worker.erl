-module(esmpp_lib_worker).
-author('Alexander Zhuk <aleksandr.zhuk@privatbank.ua>').

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("esmpp_lib.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([submit/2, data_sm/2, unbind/1, query_sm/2, cancel_sm/2,
         replace_sm/2]).
-export([loop_tcp/2, processing_submit/5, enquire_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Callback Function Definitions
%% ------------------------------------------------------------------

-callback submit_sm_resp_handler(pid(), list()) -> ok.
-callback data_sm_handler(pid(), list()) -> ok.
-callback data_sm_resp_handler(pid(), list()) -> ok.
-callback deliver_sm_handler(pid(), list()) -> ok.
-callback query_sm_handler(pid(), list()) -> ok.
-callback unbind_handler(pid()) -> ok.
-callback outbind_handler(pid(), term()) -> ok.
-callback network_error(pid(), term()) -> ok.
-callback decoder_error(pid(), term()) -> ok.
-callback submit_error(pid(), pid(), term(), term()) -> ok.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Param) ->
    gen_server:start_link(?MODULE, Param, []).


-spec submit(pid(), list()) -> ok.
submit(WorkerPid, List) ->
    gen_server:cast(WorkerPid, {submit, List}),
    ?LOG_INFO("Send msg submit ~p~n ", [List]).

-spec data_sm(pid(), list()) -> ok.
data_sm(WorkerPid, List) ->
    gen_server:cast(WorkerPid, {data_sm, List}),
    ?LOG_INFO("Send msg data_sm ~p~n ", [List]).

-spec query_sm(pid(), list()) -> ok.
query_sm(WorkerPid, List) ->
    gen_server:cast(WorkerPid, {query_sm, List}),
    ?LOG_INFO("Send msg query_sm ~p~n ", [List]).

-spec replace_sm(pid(), list()) -> ok.
replace_sm(WorkerPid, List) ->
    gen_server:cast(WorkerPid, {replace_sm, List}),
    ?LOG_INFO("Send msg replace_sm ~p~n ", [List]).

-spec cancel_sm(pid(), list()) -> ok.
cancel_sm(WorkerPid, List) ->
    gen_server:cast(WorkerPid, {cancel_sm, List}),
    ?LOG_INFO("Send msg cancel_sm ~p~n ", [List]).

-spec unbind(pid()) -> ok.  
unbind(WorkerPid) ->
    gen_server:cast(WorkerPid, {unbind, []}),
    ?LOG_INFO("Send msg unbind for pid ~p~n ", [WorkerPid]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Param) ->
    Mode = proplists:get_value(mode, Param),
    SubmitTimeout = get_timeout(submit_timeout, Param),
    {ok, _} = timer:send_after(10, {bind, Mode}),
    {ok, _} = timer:send_interval(60000, {exam_submit, SubmitTimeout}),
    WorkerPid = self(),
    Param1 = [{submit_check, []}, {sar, 0}, {seq_n, 0}, {worker_pid, WorkerPid}|Param],            
    {ok, Param1}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({submit, List}, State) ->   
    SmsList = esmpp_lib_encoder:encode(submit_sm, State, List),
    ok = send_sms(SmsList, State),
    {noreply, accumulate_seq_num(State)}; 
handle_cast({query_sm, List}, State) ->
    Transport = get_transport(State),
    Socket = proplists:get_value(socket, State),  
    Bin = esmpp_lib_encoder:encode(query_sm, State, List),
    ok = Transport:send(Socket, Bin),
    {noreply, accumulate_seq_num(State)}; 
handle_cast({replace_sm, List}, State) ->
    Transport = get_transport(State),
    Socket = proplists:get_value(socket, State),  
    Bin = esmpp_lib_encoder:encode(replace_sm, State, List),
    ok = Transport:send(Socket, Bin),
    {noreply, accumulate_seq_num(State)}; 
handle_cast({cancel_sm, List}, State) ->
    Transport = get_transport(State),
    Socket = proplists:get_value(socket, State),  
    Bin = esmpp_lib_encoder:encode(cancel_sm, State, List),
    ok = Transport:send(Socket, Bin),
    {noreply, accumulate_seq_num(State)}; 
handle_cast({data_sm, List}, State) ->
    SmsList = esmpp_lib_encoder:encode(data_sm, State, List),
    ok = send_sms([SmsList], State),
    {noreply, accumulate_seq_num(State)}; 
handle_cast({unbind, []}, State) ->
    Transport = get_transport(State),
    Handler = proplists:get_value(handler, State),
    WorkerPid = proplists:get_value(worker_pid, State),
    Socket = proplists:get_value(socket, State),
    Bin = esmpp_lib_encoder:encode(unbind, State),
    Resp = Transport:send(Socket, Bin),
    _ = handle_bind(Resp, Socket, Transport),
    ok = Handler:unbind_handler(WorkerPid),
    WorkerPid ! {terminate, unbind},
    {noreply, accumulate_seq_num(State)}; 
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({bind, Mode}, State) ->
    Param = State,
    State1 = case bind(Mode, Param) of
        {error, Reason} ->
            Handler = proplists:get_value(handler, Param),
            WorkerPid = proplists:get_value(worker_pid, Param),
            ok = Handler:network_error(WorkerPid, Reason),
            WorkerPid ! {terminate, Reason},
            Param;
        Socket ->
            Param1 = [{socket, Socket}|Param],
            ListenPid = spawn_link(?MODULE, loop_tcp, [<<>>, Param1]),
            case proplists:get_value(enquire_timeout, Param1) of
                undefined ->
                    ok;
                _ ->
                    _ = spawn_link(?MODULE, enquire_link, [Param1])
            end,
            [{mode, Mode},{listen_pid, ListenPid}|Param1]
    end,
    {noreply, State1};
handle_info({exam_submit, SubmitTimeout}, State) ->
    ListSubmit = proplists:get_value(submit_check, State),
    ok = exam_submit(SubmitTimeout, State, ListSubmit, []),
    {noreply, State};
handle_info({update_state, {add_submit, Value}}, State) ->
    ListSubmit = proplists:get_value(submit_check, State),
    State1 = lists:keyreplace(submit_check, 1, State, {submit_check, [Value|ListSubmit]}),
    {noreply, State1};
handle_info({update_state, {delete_submit, SeqNum}}, State) ->
    ListSubmit = proplists:delete(SeqNum, proplists:get_value(submit_check, State)),
    State1 = lists:keyreplace(submit_check, 1, State, {submit_check, ListSubmit}),
    {noreply, State1};
handle_info({update_state, {Name, NewEntry}}, State) ->
    State1 = lists:keyreplace(Name, 1, State, {Name, NewEntry}),
    {noreply, State1};
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

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

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
    Ip = proplists:get_value(host, Param),
    Port = proplists:get_value(port, Param),
    Transport:connect(Ip, Port, [binary, {active, false},
                        {keepalive, true}, {reuseaddr, true}, {packet, 0}]).

handle_bind(Resp, Socket, Transport) ->
    case Resp of
        ok ->
            exam_bind_resp(Socket, Transport);
        {error, Reason} ->
            {error, Reason}
    end.

send_sms([], _State) ->
    ok;
send_sms([Bin|T], State) ->
    Transport = get_transport(State),
    WorkerPid = proplists:get_value(worker_pid, State),
    Socket = proplists:get_value(socket, State),
    Handler = proplists:get_value(handler, State),
    <<_:12/binary, SeqNum:32/integer, _/binary>> = Bin,
    ok = Transport:send(Socket, Bin),
    {_, Ts, _} = now(),
    WorkerPid ! {update_state, {add_submit, {SeqNum, {Handler, Ts, Socket}}}},
    send_sms(T, State).

loop_tcp(Buffer, Param) ->
    Transport = get_transport(Param),
    Handler = proplists:get_value(handler, Param),
    Socket = proplists:get_value(socket, Param),
    WorkerPid = proplists:get_value(worker_pid, Param),
    case Transport:recv(Socket, 0) of 
        {ok, Bin} ->
            try esmpp_lib_decoder:decode(Bin, []) of
                {undefined, Name} ->
                    ?LOG_WARNING("Unsupported smpp packet ~p~n", [Name]),
                    loop_tcp(<<>>, Param);
                List ->
                    ok = create_resp(List, Param),
                    loop_tcp(<<>>, Param)
            catch
                _Class:Reason ->
                    case byte_size(Bin)>1535 of
                        true ->
                            Handler:decoder_error(WorkerPid, Bin),                        
                            WorkerPid ! {terminate, Reason};
                        false ->
                            loop_tcp(<<Buffer/bitstring, Bin/bitstring>>, Param)
                    end
            end;
        {error, closed} ->
            ok = Handler:network_error(WorkerPid, closed),
            WorkerPid ! {terminate, closed};
        {error, Reason} ->
            ok = Handler:network_error(WorkerPid, Reason),
            WorkerPid ! {terminate, Reason}
    end.              

create_resp([], _Param) ->
    ok;
create_resp([H|T], Param) ->
    Transport = get_transport(Param),
    Socket = proplists:get_value(socket, Param),
    Handler = proplists:get_value(handler, Param),
	{Name, Code, SeqNum, List} = H,
    Resp = assemble_resp({Name, Code, SeqNum, List}, Param),
    case Resp of
        ok ->
            ok;
        {close_session, Bin} ->
            _ = Transport:send(Socket, Bin),
            ok = Handler:network_error(Socket, close_session);
        _ ->
            ok = Transport:send(Socket, Resp)
    end,
    create_resp(T, Param).  

assemble_resp({Name, Status, SeqNum, List}, Param) ->
    Handler = proplists:get_value(handler, Param),
    Socket = proplists:get_value(socket, Param),
    WorkerPid = proplists:get_value(worker_pid, Param),
    case Name of
        enquire_link -> 
            esmpp_lib_encoder:encode(enquire_link_resp, [], [{sequence_number, SeqNum}]);
        enquire_link_resp ->
            ok; 
        deliver_sm -> 
            MsgId = proplists:get_value(receipted_message_id, List),
            ok = Handler:deliver_sm_handler(WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]),
            esmpp_lib_encoder:encode(deliver_sm_resp, [], [{sequence_number, SeqNum}, {message_id, MsgId}, {status, 0}]); %%TODO status
        submit_sm_resp ->
            spawn(?MODULE, processing_submit, [Param, List, SeqNum, submit_sm_resp_handler, Status]), 
            ok;
        data_sm_resp ->
            spawn(?MODULE, processing_submit, [Param, List, SeqNum, data_sm_resp_handler, Status]), 
            ok;
        data_sm ->                                                                                  %% TODO, need testing data_sm !!!
            MsgId = proplists:get_value(receipted_message_id, List),
            ok = Handler:data_sm_handler(WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]),
            esmpp_lib_encoder:encode(data_sm_resp, [], [{sequence_number, SeqNum}, {message_id, MsgId}, {status, 0}]); %%TODO status
        query_sm_resp ->
            ok = Handler:query_sm_handler(WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]);
        alert_notification ->
            ok;
        outbind ->
            ok = Handler:outbind_handler(WorkerPid, Socket);
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
    case Transport:recv(Socket, 0, 2000) of 
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
    EnquireTimeout = proplists:get_value(enquire_timeout, State)*1000,
    timer:sleep(EnquireTimeout),
    Transport = get_transport(State),
    Socket = proplists:get_value(socket, State),
    Bin = esmpp_lib_encoder:encode(enquire_link, State),
    ok = Transport:send(Socket, Bin),
    enquire_link(State).                       
    
        
exam_submit(_Timeout, State, [], Acc) ->
    WorkerPid = proplists:get_value(worker_pid, State),
    WorkerPid ! {update_state, {submit_check, Acc}},
    ok;
exam_submit(Timeout, State, [H|T], Acc) ->
    WorkerPid = proplists:get_value(worker_pid, State),
    Handler = proplists:get_value(handler, State),
    {_, TsNow, _} = now(),
    {Key, {Handler, TsOld, Socket}} = H,
    Time = TsNow - TsOld,
    Acc1 = case Time > Timeout of
        true ->
            ok = Handler:submit_error(WorkerPid, Socket, Key),
            Acc;
        false ->
            [H|Acc]
    end,
    exam_submit(Timeout, State, T, Acc1).

get_transport(Param) ->
    case proplists:get_value(transport, Param) of 
        tcp -> gen_tcp;
        undefined -> gen_tcp;
        ssl -> ssl
    end.
    
get_timeout(Key, Param) ->
    Value = proplists:get_value(Key, Param),
    case is_integer(Value) of 
        true ->
            Value;
        false ->
            39
    end.
          
accumulate_seq_num(State) ->
    SeqNum = proplists:get_value(seq_n, State),
    Value = case SeqNum of 
        999999 ->
            1;
        SeqNum ->
            SeqNum + 1
    end,
    lists:keyreplace(seq_n, 1, State, {seq_n, Value}).
    
processing_submit(Param, List, SeqNum, OperationHandler, Status) ->
    Handler = proplists:get_value(handler, Param),
    Socket = proplists:get_value(socket, Param),
    WorkerPid = proplists:get_value(worker_pid, Param),
    State = get_state(WorkerPid),
    ListSubmit = proplists:get_value(submit_check, State),
    case is_tuple(proplists:get_value(SeqNum, ListSubmit)) of
        true ->
            ok = Handler:OperationHandler(WorkerPid, [{sequence_number, SeqNum}, {command_status, Status}|List]),
            timer:sleep(2000),
            WorkerPid ! {update_state, {delete_submit, SeqNum}},
            ok;
        false ->
            ok = Handler:submit_error(WorkerPid, undefined, Socket, SeqNum)
    end.        

get_state(WorkerPid) ->
    Pid = self(),
    WorkerPid ! {get_state, Pid},
    receive
        {state, State} -> State
    end.
    
     
        
 
