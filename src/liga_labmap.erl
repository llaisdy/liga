-module(liga_labmap).
-include("types.hrl").

%%-define(VERSION, original).
-define(VERSION, llaisdy).

-export([new/0
	,get_labels/1
	,get_weights/1
	,merge/1
	,put/3
	,score/3
	,submap/2]).


new() ->
    #{}.

-spec get_labels(labmap()) -> list(label()).
get_labels(LabMap) ->
    maps:fold(fun(_N, Ls, Acc1) ->
		      maps:fold(fun(L, _W, Acc2) ->
					case lists:member(L, Acc2) of
					    true  -> Acc2;
					    false -> [L | Acc2]
					end
				end,
				Acc1, Ls)
	      end,
	      [], LabMap).

-spec get_weights(labmap()) -> nonegint().
get_weights(LabMap) ->
    IntMaps = maps:values(LabMap),
    lists:sum([lists:sum(maps:values(M)) || M <- IntMaps]).

-spec merge(list(labmap())) -> labmap().
merge([Head | Tail]) ->
    lists:foldl(fun(LabMap, LabMapAcc) ->
			maps:merge(LabMapAcc, 
				   maps:map(fun(Lab, IntMap) ->
						    maybe_intmap_sum(IntMap,
								     maps:find(Lab, LabMapAcc))
					    end, 
					    LabMap)) 
		end,
		Head,
		Tail).

-spec put(mnode() | medge(), label(), labmap()) -> labmap().
put(Key, Lab, LabMap) ->
    ValMap = maps:get(Key, LabMap, liga_intmap:new()),
    NewValMap = liga_intmap:increment(Lab, ValMap),
    maps:put(Key, NewValMap, LabMap).

-spec score(nonegint(), liga_score(), labmap()) -> liga_score().
score(Weights, ScoreSoFar, LabMap) ->
    maps:fold(fun(_, IntMap, ScoreAcc) ->
		      D = versioned_weights(?VERSION, Weights, IntMap),
		      score_merge(D, IntMap, ScoreAcc)
	      end, ScoreSoFar, LabMap).

-spec submap(list(), labmap()) -> labmap().
submap(Ts, LabMap) ->
    lists:foldl(fun(T, Acc) ->
			case maps:find(T, LabMap) of
			    {ok, V} ->
				maps:put(T, V, Acc);
			    error ->
				Acc
			end
		end, new(), Ts).


%%%% private

versioned_weights(original, Weights, _) ->
    Weights;
versioned_weights(llaisdy, Weights, IntMap) ->
    Weights * maps:size(IntMap).

-spec score_merge(float() | integer(), intmap(), liga_score()) -> liga_score().
score_merge(N, IntMap, Score) ->
    maps:fold(fun(Lab, Int, ScoreAcc) ->
		      case lists:keyfind(Lab, 1, ScoreAcc) of
			  false ->
			      [{Lab, Int/N} | ScoreAcc];
			  {Lab, W} ->
			      [{Lab, W + (Int/N)} | lists:delete({Lab, W}, ScoreAcc)]
		      end
	      end, Score, IntMap).

%%%% intmap

maybe_intmap_sum(IM1, {ok, IM2}) -> intmap_sum(IM1, IM2);
maybe_intmap_sum(IM1, _)         -> IM1.

-spec intmap_sum(intmap(), intmap()) -> intmap().
intmap_sum(IM1, IM2) ->
    maps:fold(fun(Key, IntX, Acc) ->
		      case maps:find(Key, Acc) of
			  {ok, IntY} -> maps:put(Key, IntX + IntY, Acc);
			  error      -> Acc
		      end
	      end, IM1, IM2).


