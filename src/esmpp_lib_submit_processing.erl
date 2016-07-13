-module(esmpp_lib_submit_processing).
-author('Alexander Zhuk <aleksandr.zhuk@privatbank.ua>').

-include("esmpp_lib.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 39).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([processing_submit/6, push_submit/2]).

start_link(State) ->
    gen_server:start_link(?MODULE, State, []).

-record(state, {
    submit_check,
    submit_tref,
    handler
}).

processing_submit(Pid, Handler, List, SeqNum, OperationHandler, Status) ->
    Pid ! {processing_submit, Handler, List, SeqNum, OperationHandler, Status}.

push_submit(Pid, Value) ->
    Pid ! {push_submit, Value}.

init(Opt) ->
    SubmitTimeout = get_timeout(submit_timeout, Opt),
    Handler = esmpp_utils:lookup(handler, Opt),
    {ok, TRef} = timer:send_after(60000, {exam_submit, SubmitTimeout}),
    {ok, #state{submit_check = [], submit_tref = TRef, handler = Handler}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(Msg, State) ->
    ?LOG_DEBUG("Unknown cast msg ~p~n", [Msg]),
    {noreply, State}.

handle_info({processing_submit, Handler, List, SeqNum, OperationHandler, Status}, State) ->
    NewSubmitCheck = case is_tuple(esmpp_utils:lookup(SeqNum, State#state.submit_check)) of
        true ->
            ok = Handler:OperationHandler(self(), [{sequence_number, SeqNum}, {command_status, Status} | List]),
            esmpp_utils:delete(SeqNum, State#state.submit_check);
        false ->
            ok = Handler:submit_error(self(), SeqNum),
            State#state.submit_check
    end,
    {noreply, State#state{submit_check = NewSubmitCheck}};
handle_info({push_submit, Value}, State) ->
    {noreply, State#state{submit_check = [Value | State#state.submit_check]}};
handle_info({exam_submit, SubmitTimeout}, State) ->
    {ok, cancel} = timer:cancel(State#state.submit_tref),
    NewSubmitCheck = exam_submit(SubmitTimeout, os:timestamp(), State),
    {ok, NewTRef} = timer:send_after(60000, {exam_submit, SubmitTimeout}),
    {noreply, State#state{submit_tref = NewTRef, submit_check = NewSubmitCheck}};
handle_info(Info, State) ->
    ?LOG_DEBUG("Unknown info msg ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Function Definitions

exam_submit(SubmitTimeout, TsNow, State) ->
    exam_submit(SubmitTimeout, TsNow, State#state.handler, State#state.submit_check, []).

exam_submit(_Timeout, _TsNow, _Handler, [], Acc) ->
    Acc;
exam_submit(Timeout, TsNow, Handler, [H|T], Acc) ->

    {Key, {Handler, TsOld, Socket}} = H,
    Acc1 = case timer:now_diff(TsNow, TsOld) > Timeout*1000000 of
        true ->
            ok = Handler:submit_error(self(), Socket, Key),
            Acc;
        false ->
            [H|Acc]
    end,
    exam_submit(Timeout, TsNow, Handler, T, Acc1).

get_timeout(Key, Param) ->
    case esmpp_utils:lookup(Key, Param) of
        Value when is_integer(Value) ->
            Value;
        false ->
            ?DEFAULT_TIMEOUT
    end.