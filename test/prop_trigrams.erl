-module(prop_trigrams).
-include_lib("proper/include/proper.hrl").


%%%% properties

prop_length() ->
    ?FORALL(T, trigram(), tuple_size(T) =:= 3).

prop_string_to_trigrams() ->
    ?FORALL(S, non_empty(string()),
	    begin
		TS = trigrams:string_to_trigrams(S),
		true = length(TS) =:= length(S),
		unpack_trigrams(TS) =:= S
	    end).

prop_trigrams_to_edges() ->
    ?FORALL(TS, non_empty(list(trigram())),
	    begin
		Es = trigrams:trigrams_to_edges(TS),
		true = length(Es) =:= (length(TS) - 1),
		Expected = case TS of
		    [_] -> [];
		    _   -> TS
		end,
		unpack_edges(Es) =:= Expected
	    end).

%%%% helpers

unpack_edges([]) -> [];
unpack_edges([{A,B}]) ->
    [A,B];
unpack_edges([{A,_}|T]) ->
    [A | unpack_edges(T)].

unpack_trigrams([{_,X,_}]) -> [X];
unpack_trigrams([_|TS]) ->
    lists:reverse(tl(lists:reverse(really_unpack_trigrams(TS)))).

really_unpack_trigrams([{A,B,C}]) ->
    [A,B,C];
really_unpack_trigrams([{A,_,_}|T]) ->
    [A | really_unpack_trigrams(T)].

%%%% generators

trigram() ->
    {char(), char(), char()}.
