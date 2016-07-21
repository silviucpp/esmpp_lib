-module(esmpp_utils).
-author("silviu").

-export([
    now/0,
    lookup/2,
    lookup/3,
    delete/2,
    replace/3,
    send_notification/2,
    revert_bin_data/1
]).

now() ->
    {Mega, Sec, Micro} = erlang:timestamp(),
    trunc((Mega * 1000000 * 1000000 + Sec * 1000000 + Micro)/1000).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} ->
            Result;
        false ->
            Default
    end.

lookup(Key, List) ->
    lookup(Key, List, undefined).

delete(Key, List) ->
    lists:keydelete(Key, 1, List).

replace(Key, NewValue, List) ->
    lists:keyreplace(Key, 1, List, {Key, NewValue}).

revert_bin_data(Bin) ->
    revert_bin_data(Bin, <<>>).

revert_bin_data(<<>>, Acc) ->
    Acc;
revert_bin_data(<<H:1/binary, T/binary>>, Acc) ->
    revert_bin_data(T, <<H:1/binary, Acc/binary>>).

send_notification(HandlerPid, Message) ->
    HandlerPid ! Message.