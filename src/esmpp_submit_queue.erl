-module(esmpp_submit_queue).

-include("esmpp.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(MIN_EXPIRE_ACCURACY_MS, 5000).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([process/4, push/3]).

start_link(State) ->
    gen_server:start_link(?MODULE, State, []).

-record(state, {
    submit_check,
    submit_timeout_ms,
    handler_pid,
    connection_pid,
    timer_ref
}).

process(Pid, SeqNum, List, OperationHandler) ->
    Pid ! {processing_submit, SeqNum, OperationHandler, List}.

push(Pid, SeqNum, Ts) ->
    Pid ! {push_submit, {SeqNum, Ts}}.

init([HandlerPid, ConnectionPid, SubmitTimeout]) ->
    SubmitTimeoutMs = SubmitTimeout*1000,
    {ok, #state{submit_check = [], submit_timeout_ms = SubmitTimeoutMs, handler_pid = HandlerPid, connection_pid = ConnectionPid}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(Msg, State) ->
    ?LOG_DEBUG("unknown cast msg ~p", [Msg]),
    {noreply, State}.

handle_info({processing_submit, SeqNum, MessageTag, List}, State) ->
    case esmpp_utils:lookup(SeqNum, State#state.submit_check) of
        undefined ->
            %most probably the message was already removed because was not acknowledged in the submit_timeout timeframe
            %in this case the error signal ewas already triggered.
            ?LOG_ERROR("ignore processing_submit for unknown seq_number: ~p", [SeqNum]),
            {noreply, State};
        _ ->
            esmpp_utils:send_notification(State#state.handler_pid, {MessageTag, State#state.connection_pid, List}),
            NewSubmitList = esmpp_utils:delete(SeqNum, State#state.submit_check),
            {noreply, State#state{submit_check = NewSubmitList}}
    end;
handle_info({push_submit, {_SeqNumber, Ts} = Message}, State) ->

    TimerRef = case State#state.timer_ref =:= undefined andalso State#state.submit_check =:= [] of
        true ->
            erlang:send_after(next_timer_inteval(Ts, State#state.submit_timeout_ms), self(), check_not_ack_submit);
        _ ->
            undefined
    end,

    {noreply, State#state{submit_check = [Message | State#state.submit_check], timer_ref = TimerRef}};
handle_info(check_not_ack_submit, State) ->

    {NewSubmitList, LowestTs} = check_submit_expired(State),

    TimerRef = case LowestTs of
        undefined ->
            undefined;
        _ ->
            erlang:send_after(next_timer_inteval(LowestTs, State#state.submit_timeout_ms), self(), check_not_ack_submit)
    end,

    {noreply, State#state{submit_check = NewSubmitList, timer_ref = TimerRef}};
handle_info(Info, State) ->
    ?LOG_DEBUG("unknown info msg ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Function Definitions

check_submit_expired(State) ->
    check_submit_expired(State, esmpp_utils:now(), State#state.submit_check, [], undefined).

check_submit_expired(_State, _NowMs, [], Acc, NewTs) ->
    {Acc, NewTs};
check_submit_expired(State, NowMs, [{SeqNum, MessageTs} = H|T], Acc, AccTs) ->
    case NowMs - MessageTs >= State#state.submit_timeout_ms of
        true ->
            esmpp_utils:send_notification(State#state.handler_pid, {submit_error, State#state.connection_pid, SeqNum}),
            check_submit_expired(State, NowMs, T, Acc, AccTs);
        _ ->
            check_submit_expired(State, NowMs, T, [H|Acc], get_new_timestamp(AccTs, MessageTs))
    end.

next_timer_inteval(NextMsgExpireTs, SubmitTimeoutMs) ->
    erlang:max(?MIN_EXPIRE_ACCURACY_MS, (NextMsgExpireTs + SubmitTimeoutMs) - esmpp_utils:now()).
    
get_new_timestamp(undefined, NewValue) ->
    NewValue;
get_new_timestamp(CurrentValue, NewValue) ->
    erlang:min(CurrentValue, NewValue).