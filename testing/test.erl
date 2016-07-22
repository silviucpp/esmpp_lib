-module(test).
-author("silviu").

-include("esmpp.hrl").

-export([new_connection/0, send/4]).

response_loop() ->
    receive
        {submit_sm_resp, Pid, List} ->
            ?LOG_INFO("submit_sm_resp pid ~p msg: ~p", [Pid, List]);
        {deliver_sm, Pid, List} ->
            ?LOG_INFO("deliver_sm pid ~p msg: ~p", [Pid, List]);
        {data_sm, Pid, List} ->
            ?LOG_INFO("data_sm pid ~p msg: ~p", [Pid, List]);
        {data_sm_resp, Pid, List} ->
            ?LOG_INFO("data_sm_resp pid ~p msg: ~p", [Pid, List]);
        {query_sm_resp, Pid, List} ->
            ?LOG_INFO("query_sm_resp pid ~p msg: ~p", [Pid, List]);
        {replace_sm_resp, Pid, List} ->
            ?LOG_INFO("replace_sm_resp pid ~p msg: ~p", [Pid, List]);
        {cancel_sm_resp, Pid, List} ->
            ?LOG_INFO("cancel_sm_resp pid ~p msg: ~p", [Pid, List]);
        {unbind, Pid} ->
            ?LOG_INFO("unbind pid ~p", [Pid]);
        {outbind, Pid} ->
            ?LOG_INFO("outbind pid ~p", [Pid]);
        {submit_error, Pid, SeqNum} ->
            ?LOG_INFO("submit_error pid ~p seqnum ~p", [Pid, SeqNum]);
        {bind_failed, Pid, Error} ->
            ?LOG_INFO("bind_failed ~p return error tcp ~p", [Pid, Error]);
        {bind_completed, Pid} ->
            ?LOG_INFO("bind_completed ~p", [Pid])
    end,
    response_loop().

new_connection() ->
    HandlerPid = spawn(fun() -> response_loop() end),

    Config = [
        {host, {127,0,0,1}},
        {port, 2775},
        {password, <<"test">>},
        {system_id, <<"test">>},
        {transport, tcp},
        {interface_version, <<"3.4">>},
        {enquire_timeout, 30},
        {submit_timeout, 60},
        {system_type, <<"smpp">>},
        {service_type, <<"">>},
        {addr_ton, 5},
        {addr_npi, 0},
        {source_addr_ton, 5},
        {source_addr_npi, 0},
        {dest_addr_ton, 1},
        {dest_addr_npi, 1},
        {mode, transceiver},
        {data_coding, 3},
        {handler_pid, HandlerPid}
    ],

    {ok, Pid} = esmpp_connection:start_link(Config),
    Pid.

send(Pid, From, To, Msg) ->
    esmpp_connection:submit_sm(Pid, [{source_addr, From}, {dest_addr, To}, {text, Msg}]).