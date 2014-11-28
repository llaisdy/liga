-module(liga).

-export([new/0, 
	 import_string/3, 
	 classify/2,
	 get_likely/1
	]).

-define(POINT_START, 32).
-define(POINT_END, 32).

%% nb use "ligaModel" not liga_model in case of clashes with
%% other uses of "liga_model"

-type ligaModel() :: #{labels => list(label()),
		       node_weights => non_neg_integer(), 
		       edge_weights => non_neg_integer(), 
		       nodes => map(), 
		       edges => map()}.
-type label() :: atom().
-type score() :: float().
-type label_score() :: {label(), score()}.
-type liga_score()  :: list(label_score()). % sorted by score
-type mnode() :: {non_neg_integer(),non_neg_integer(),non_neg_integer()}.
-type medge() :: {mnode(), mnode()}.
-type trigram() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

%%%% api

-spec new() -> ligaModel().
new() ->
    #{labels => [], node_weights => 0, edge_weights => 0, nodes => #{}, edges => #{}}.

-spec import_string(ligaModel(), string(), label()) -> ligaModel().
import_string(Model, String, Label) ->
    Ts = string_to_trigrams(String),
    M1 = lists:foldl(fun(T, Acc1) ->
			      import_node(Acc1, T, Label)
		     end, 
		     Model, Ts),
    M2 = lists:foldl(fun(E, Acc2) ->
			import_edge(Acc2, E, Label)
		end, 
		M1, get_edges(Ts)),
    update_labels(M2, Label).

-spec classify(atom() | ligaModel(), string()) -> liga_score().
classify(Model, String) ->
    SG = make_sub_graph(Model, String),
    score(SG).

-spec get_likely(liga_score()) -> (liga_score()).
get_likely([]) -> [];
get_likely(Ls) ->
    Mean = lists:sum([N || {_,N} <- Ls])  /  length(Ls),
    [L || L <- lists:filter(fun({_,N}) -> N >= Mean end, Ls)].

%%%% private

-spec update_labels(ligaModel(), label()) -> ligaModel().
update_labels(#{labels:=[]}=M, L) ->
    M#{labels:=[L]};
update_labels(#{labels:=Ls}=M, L) ->
    case lists:member(L, Ls) of
	true ->
	    M;
	false ->
	    M#{labels:=[L|Ls]}
    end.

-spec import_node(ligaModel(), mnode(), label()) -> ligaModel().
import_node(#{nodes:=Nodes, node_weights:=NW}=Model, Node, Label) ->
    NewNodes = import_x(Nodes, Node, Label),
    Model#{nodes:=NewNodes, node_weights:=NW+1}.

-spec import_edge(ligaModel(), medge(), label()) -> ligaModel().
import_edge(#{edges:=Edges, edge_weights:=EW}=Model, Edge, Label) ->
    NewEdges = import_x(Edges, Edge, Label),
    Model#{edges:=NewEdges, edge_weights:=EW+1}.

-spec import_x(map(), mnode() | medge(), label()) -> map().
import_x(Keys, Key, Val) ->
    NewKeys = case maps:get(Key, Keys, x) of
		   x ->
		       maps:put(Key, maps:put(Val, 1, #{}), Keys);
		   ValMap ->
		       NewValMap = case maps:get(Val, ValMap, x) of
					 x ->
					     maps:put(Val, 1, ValMap);
					 N ->
					     maps:put(Val, N+1, ValMap)
				     end,
		       maps:put(Key, NewValMap, Keys)
	       end,
    NewKeys.

-spec make_sub_graph(atom() | ligaModel(), string()) -> ligaModel().
make_sub_graph(M, S) when is_atom(M) ->
    LG = M:model(),
    make_sub_graph(LG, S);
make_sub_graph(#{nodes:=LNs, edges:=LEs}, S) ->
    Ts = string_to_trigrams(S),
    Ns = pull(Ts, LNs),
    Es = pull(get_edges(Ts), LEs),
    NW = get_weights(Ns),
    EW = get_weights(Es),
    Ls = get_labels(Ns),
    #{labels => Ls, node_weights => NW, edge_weights => EW, nodes => Ns, edges => Es}.

-spec string_to_trigrams(string()) -> list(trigram()).
string_to_trigrams([C]) ->
    [{?POINT_START, C, ?POINT_END}];
string_to_trigrams([X,Y|_] = S) ->
    [{?POINT_START, X, Y} | get_trigrams(S)].

-spec get_edges(list(mnode())) -> list(medge()).
get_edges([_]) ->
    [];
get_edges([H1, H2 | T]) ->
    [{H1, H2} | get_edges([H2 | T])].

-spec get_trigrams(string()) -> list(trigram()).
get_trigrams([S1, S2]) ->
    [{S1, S2, ?POINT_END}];
get_trigrams([H1, H2, H3 | T]) ->
    [{H1, H2, H3} | get_trigrams([H2, H3 | T])].

-spec get_weights(map()) -> non_neg_integer().
get_weights(Xs) ->
    Ms = maps:values(Xs),
    lists:sum([lists:sum(maps:values(M)) || M <- Ms]).

-spec get_labels(map()) -> list(label()).
get_labels(Ns) ->
    maps:fold(fun(_N, Ls, Acc1) ->
		      maps:fold(fun(L, _W, Acc2) ->
					case lists:member(L, Acc2) of
					    true  -> Acc2;
					    false -> [L | Acc2]
					end
				end,
				Acc1, Ls)
	      end,
	      [], Ns).

-spec merge(float() | integer(), map(), list()) -> list().
merge(N, M, L) ->
    maps:fold(fun(K, V, Acc) ->
		      case lists:keyfind(K, 1, Acc) of
			  false ->
			      [{K, V/N} | Acc];
			  {K, W} ->
			      [{K, W + (V/N)} | lists:delete({K,W},Acc)]
		      end
	      end, L, M).

-spec pull(list(), map()) -> map().
pull(X, M) ->
    lists:foldl(fun(T, Acc) ->
			case maps:find(T, M) of
			    {ok, V} ->
				maps:put(T, V, Acc);
			    error ->
				Acc
			end
		end, #{}, X).

-spec score(ligaModel()) -> liga_score().
score(#{node_weights:=NW, edge_weights:=EW,nodes:=Ns,edges:=Es}) ->
    S1 = score_acc(Ns, NW, []),
    S2 = score_acc(Es, EW, S1),
    lists:sort(fun({_,V},{_,W}) -> V > W end,
	       lists:map(fun({K,V}) -> {K, V/2} end, S2)).

-spec score_acc(map(), non_neg_integer(), liga_score()) 
	       -> liga_score().
score_acc(Xs, W, S) ->
    maps:fold(fun(_, V, Acc) -> 
		      D = W * maps:size(V),  % Ivan's original research
		      merge(D, V, Acc)
	      end, S, Xs).

