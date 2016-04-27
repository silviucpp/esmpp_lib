-module(esmpp_lib_submit_processing).
-author('Alexander Zhuk <aleksandr.zhuk@privatbank.ua>').

-behaviour(gen_server).

-include("esmpp_lib.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([processing_submit/5, add_submit/1, delete_submit/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(State) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, State, []).

processing_submit(Handler, List, SeqNum, OperationHandler, Status) ->
    ok = gen_server:cast(?MODULE, {processing_submit, Handler, List, SeqNum, OperationHandler, Status}).

add_submit({SeqNum, {Handler, Ts, Socket}}) ->
    ok = gen_server:cast(?MODULE, {update_state, {add_submit, {SeqNum, {Handler, Ts, Socket}}}}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(State) ->
    WorkerPid = self(),
    SubmitTimeout = get_timeout(submit_timeout, State),
    {ok, TRef} = timer:send_after(60000, {exam_submit, SubmitTimeout}),
    {ok, [{submit_check, []}, {worker_pid, WorkerPid}, {submit_tref, TRef}|State]}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast({processing_submit, Handler, List, 
                                SeqNum, OperationHandler, Status}, State) ->
    WorkerPid = proplists:get_value(worker_pid, State),
    ok = submit_handler(Handler, WorkerPid, List, SeqNum, OperationHandler, Status, State), 
    {noreply, State}; 
handle_cast({update_state, {add_submit, Value}}, State) ->
    ListSubmit = proplists:get_value(submit_check, State),
    State1 = lists:keyreplace(submit_check, 1, State, {submit_check, [Value|ListSubmit]}),
    {noreply, State1};
handle_cast(Msg, State) ->
    ?LOG_DEBUG("Unknown cast msg ~p~n", [Msg]),
    {noreply, State}.

handle_info({exam_submit, SubmitTimeout}, State) ->
    OldTRef = proplists:get_value(submit_tref, State),
    {ok, cancel} = timer:cancel(OldTRef),
    ListSubmit = proplists:get_value(submit_check, State),
    ok = exam_submit(SubmitTimeout, State, ListSubmit, []),
    {ok, NewTRef} = timer:send_after(60000, {exam_submit, SubmitTimeout}),
    State1 = lists:keyreplace(submit_tref, 1, State, {submit_tref, NewTRef}),
    {noreply, State1};
handle_info({update_state, {submit_check, Acc}}, State) ->
    State1 = lists:keyreplace(submit_check, 1, State, {submit_check, Acc}),
    {noreply, State1};
handle_info({update_state, {delete_submit, SeqNum}}, State) ->
    ListSubmit = proplists:delete(SeqNum, proplists:get_value(submit_check, State)),
    State1 = lists:keyreplace(submit_check, 1, State, {submit_check, ListSubmit}),
    {noreply, State1};
handle_info(Info, State) ->
    ?LOG_DEBUG("Unknown info msg ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

submit_handler(Handler, WorkerPid, List, SeqNum, OperationHandler, Status, State) ->
    ListSubmit = proplists:get_value(submit_check, State),
    case is_tuple(proplists:get_value(SeqNum, ListSubmit)) of
        true ->
            ok = Handler:OperationHandler(WorkerPid, [{sequence_number, SeqNum},
                        {command_status, Status}|List]),
            _ = spawn(?MODULE, delete_submit, [WorkerPid, SeqNum]),
            ok;
        false ->
            Socket = proplists:get_value(socket, State),
            ok = Handler:submit_error(WorkerPid, Socket, SeqNum)
    end.  

exam_submit(_Timeout, State, [], Acc) ->
    WorkerPid = proplists:get_value(worker_pid, State),
    WorkerPid ! {update_state, {submit_check, Acc}},
    ok;
exam_submit(Timeout, State, [H|T], Acc) ->
    WorkerPid = proplists:get_value(worker_pid, State),
    Handler = proplists:get_value(handler, State),
    TsNow = os:timestamp(),
    {Key, {Handler, TsOld, Socket}} = H,
    Acc1 = case timer:now_diff(TsNow, TsOld) > Timeout*1000000 of
        true ->
            ok = Handler:submit_error(WorkerPid, Socket, Key),
            Acc;
        false ->
            [H|Acc]
    end,
    exam_submit(Timeout, State, T, Acc1).

delete_submit(WorkerPid, SeqNum) ->
    ok = timer:sleep(2000),
    WorkerPid ! {update_state, {delete_submit, SeqNum}}.

get_timeout(Key, Param) ->
    Value = proplists:get_value(Key, Param),
    case is_integer(Value) of 
        true ->
            Value;
        false ->
            39
    end.
