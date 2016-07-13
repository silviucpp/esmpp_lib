-module(esmpp_utils).
-author("silviu").

-export([lookup/2, lookup/3, delete/2, replace/3]).

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