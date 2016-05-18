-module(liga_intmap).
-include("types.hrl").
-export([new/0, increment/2]).

new() ->
    #{}.

-spec increment(mnode() | medge(), intmap()) -> intmap().
increment(Key, Map) ->
    N = maps:get(Key, Map, 0), 
    maps:put(Key, N+1, Map).

