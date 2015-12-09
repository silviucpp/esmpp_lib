-module(my_sms).

-behaviour(esmpp_lib_worker).

-include("esmpp_lib.hrl").

-define(SERVER, ?MODULE).


%% ------------------------------------------------------------------
%% Callback Function Exports
%% ------------------------------------------------------------------

-export([submit_sm_resp_handler/2, deliver_sm_handler/2, data_sm_handler/2, 
            data_sm_resp_handler/2, query_sm_handler/2, unbind_handler/1,
            outbind_handler/2, submit_error/4, network_error/2, decoder_error/2]).


%% ------------------------------------------------------------------
%% Behaviour Function Definitions
%% ------------------------------------------------------------------

submit_sm_resp_handler(Pid, List) ->
    ?LOG_INFO("Submit resp pid ~p msg: ~p~n", [Pid, List]).

deliver_sm_handler(Pid, List) ->
    ?LOG_INFO("Deliver pid ~p msg: ~p~n", [Pid, List]).

data_sm_handler(Pid, List) ->
    ?LOG_INFO("Data_sm pid ~p msg: ~p~n", [Pid, List]).

data_sm_resp_handler(Pid, List) ->
    ?LOG_INFO("Data_sm resp pid ~p msg: ~p~n", [Pid, List]).

query_sm_handler(Pid, List) ->
    ?LOG_INFO("Query resp pid ~p msg: ~p~n", [Pid, List]).

unbind_handler(Pid) ->
    ?LOG_INFO("Link unbind ~p~n", [Pid]).

outbind_handler(Pid, Socket) ->
    ?LOG_INFO("Link pid ~p outbind ~p~n", [Pid, Socket]).

submit_error(Pid, undefined, Socket, SeqNum) ->
    ?LOG_INFO("Error submit pid ~p socket~p seqnum ~p~n", [Pid, Socket, SeqNum]).

network_error(Pid, Error) ->
    ?LOG_INFO("Pid ~p return error tcp ~p~n", [Pid, Error]).

decoder_error(Pid, Error) ->
    ?LOG_INFO("Pid ~p return decoder error ~p~n", [Pid, Error]).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
