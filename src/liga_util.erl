-module(liga_util).
-include("types.hrl").

-export([string_to_trigrams/1
	,trigrams_to_edges/1
	,get_likely/1
	]).

-define(POINT_START, 32).
-define(POINT_END, 32).


-spec string_to_trigrams(string()) -> list(trigram()).
string_to_trigrams([C]) ->
    [{?POINT_START, C, ?POINT_END}];
string_to_trigrams([X,Y|_] = S) ->
    [{?POINT_START, X, Y} | get_trigrams(S)].

-spec trigrams_to_edges(list(mnode())) -> list(medge()).
trigrams_to_edges([_]) ->
    [];
trigrams_to_edges([H1, H2 | T]) ->
    [{H1, H2} | trigrams_to_edges([H2 | T])].

-spec get_likely(liga_score()) -> (liga_score()).
get_likely([]) -> [];
get_likely(Ls) ->
    Mean = lists:sum([N || {_,N} <- Ls])  /  length(Ls),
    [L || L <- lists:filter(fun({_,N}) -> N >= Mean end, Ls)].

%%%% private

-spec get_trigrams(string()) -> list(trigram()).
get_trigrams([S1, S2]) ->
    [{S1, S2, ?POINT_END}];
get_trigrams([H1, H2, H3 | T]) ->
    [{H1, H2, H3} | get_trigrams([H2, H3 | T])].

