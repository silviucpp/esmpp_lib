-module(esmpp_lib_submit_processing).
-author('Alexander Zhuk <aleksandr.zhuk@privatbank.ua>').

-include("esmpp_lib.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 60).
-define(DEFAULT_CHECK_INTERVAL, 60000).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([processing_submit/4, push_submit/3]).

start_link(State) ->
    gen_server:start_link(?MODULE, State, []).

-record(state, {
    submit_check,
    submit_timeout_ms,
    handler,
    parent_pid
}).

processing_submit(Pid, SeqNum, List, OperationHandler) ->
    Pid ! {processing_submit, SeqNum, OperationHandler, List}.

push_submit(Pid, SeqNum, Ts) ->
    Pid ! {push_submit, {SeqNum, Ts}}.

init(Opt) ->
    HandlerPid = esmpp_utils:lookup(handler_pid, Opt),
    ParentPid = esmpp_utils:lookup(parent_pid, Opt),
    SubmitTimeoutMs = get_timeout(submit_timeout, Opt)*1000,
    erlang:send_after(?DEFAULT_CHECK_INTERVAL, self(), check_not_ack_submit),
    {ok, #state{submit_check = [], submit_timeout_ms = SubmitTimeoutMs, handler = HandlerPid, parent_pid = ParentPid}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(Msg, State) ->
    ?LOG_DEBUG("Unknown cast msg ~p~n", [Msg]),
    {noreply, State}.

handle_info({processing_submit, SeqNum, MessageTag, List}, State) ->
    case esmpp_utils:lookup(SeqNum, State#state.submit_check) of
        undefined ->
            %most probably the message was already removed because was not acknowledged in the submit_timeout timeframe
            %in this case the error signal ewas already triggered.
            ?LOG_ERROR("ignore processing_submit for unknown seq_number: ~p", [SeqNum]),
            {noreply, State};
        _ ->
            esmpp_utils:send_notification(State#state.handler, {MessageTag, State#state.parent_pid, List}),
            NewSubmitList = esmpp_utils:delete(SeqNum, State#state.submit_check),
            {noreply, State#state{submit_check = NewSubmitList}}
    end;
handle_info({push_submit, Message}, State) ->
    {noreply, State#state{submit_check = [Message | State#state.submit_check]}};
handle_info(check_not_ack_submit, State) ->
    NewSubmitList = check_submit_expired(State),
    erlang:send_after(?DEFAULT_CHECK_INTERVAL, self(), check_not_ack_submit),
    {noreply, State#state{submit_check = NewSubmitList}};
handle_info(Info, State) ->
    ?LOG_DEBUG("Unknown info msg ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Function Definitions

check_submit_expired(State) ->
    check_submit_expired(State#state.submit_timeout_ms, esmpp_utils:now(), State#state.parent_pid, State#state.handler, State#state.submit_check, []).

check_submit_expired(_Timeout, _NowMs, _ParentPid,  _HandlerPid, [], Acc) ->
    Acc;
check_submit_expired(Timeout, NowMs, ParentPid, HandlerPid, [{SeqNum, MessageTs} = H|T], Acc) ->
    Acc1 = case NowMs - MessageTs >= Timeout of
        true ->
            esmpp_utils:send_notification(HandlerPid, {submit_error, ParentPid, SeqNum}),
            Acc;
        _ ->
            [H|Acc]
    end,
    check_submit_expired(Timeout, NowMs, ParentPid, HandlerPid, T, Acc1).

get_timeout(Key, Param) ->
    case esmpp_utils:lookup(Key, Param) of
        Value when is_integer(Value) ->
            Value;
        false ->
            ?DEFAULT_TIMEOUT
    end.