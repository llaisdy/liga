-module(liga).

-compile([export_all]).

-export([classify/1, classify/2, 
	 get_likely/1,
	 new/0, import_string/3, 
	 update_model/1, update_model/2]).

-define(POINT_START, 32).
-define(POINT_END, 32).


-record(liga_model, {labels :: list(),
		     node_weights :: non_neg_integer(), 
		     edge_weights :: non_neg_integer(), 
		     nodes :: list(), 
		     edges :: list()}).

-type label() :: atom().
-type score() :: float().
-type label_score() :: {label(), score()}.
-type liga_score()  :: list(label_score()). % sorted by score
-type liga_model() :: #liga_model{}.
-type mnode() :: any().
-type medge() :: {node(), node()}.
-type trigram() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type weighted_item() :: {any(), non_neg_integer()}.

new() ->
    {liga_model, {labels, []}, {node_weights, 0}, {edge_weights, 0},
     {nodes, []}, {edges, []}}.

-spec import_string(liga_model(), string(), label()) -> liga_model().
import_string(Model, String, Label) ->
    Ts = string_to_trigrams(String),
    M1 = lists:foldl(fun(T, Acc1) ->
			      import_node(T, Label, Acc1)
		     end, 
		     Model, Ts),
    lists:foldl(fun(E, Acc2) ->
			import_edge(E, Label, Acc2)
		end, 
		M1, get_edges(Ts)).

-spec import_node(liga_model(), mnode(), label()) -> liga_model().
import_node(Model=#liga_model{nodes=NL, node_weights=NW}, Node, Label) ->
    Nodes = ofl(NL),
    NewNodes = orddict:update(X, fun(Ws) ->
					 orddict:update(Label, fun incr/1, 1, ofl(Ws))
				 end, [{Label,1}], Nodes),
    Model#liga_model{nodes=NewNodes, node_weights=incr(NW)}.

-spec import_edge(liga_model(), medge(), label()) -> liga_model().
import_edge(Model=#liga_model{edges=EL, edge_weights=EW}, Edge, Label) ->
    Edges = ofl(EL),
    NewEdges = orddict:update(X, fun(Ws) ->
					 orddict:update(Label, fun incr/1, 1, ofl(Ws))
				 end, [{Label,1}], Edges),
    Model#liga_model{edges=NewEdges, edge_weights=incr(EW)}.

ofl(L) ->
    orddict:from_list(L).



-spec classify(string()) -> liga_score().
classify(String) ->
    classify(String, liga_model).

-spec classify(string(), atom()) -> liga_score().
classify(String, ModelName) ->
    SG = make_sub_graph(String, ModelName),
    score(SG).

-spec get_likely(liga_score()) -> list(label()).
get_likely([]) -> [];
get_likely(Ls) ->
    Mean = lists:sum([Y || {_,Y} <- Ls])  /  length(Ls),
    [L || {L,_} <- lists:filter(fun({_,N}) -> N >= Mean end, Ls)].

-spec new() -> liga_model().
new() ->
    orddict:from_list([{node_weights, 0}, {edge_weights, 0}, 
		       {nodes, []}, 
		       {edges, []}]).

-spec update_model(string()) -> ok | {error, tuple()}.
update_model(Filename) ->
    update_model(erl, Filename).

-spec update_model(erl | beam, string()) -> ok | {error, tuple()}.
update_model(erl, Filename) ->
    try
	{ok, M} = compile:file(Filename),
	code:add_path("."),
	update_model(beam, M),
	ok
    catch
	E:R ->
	    {error, {E,R}}
    end;
update_model(beam, M) ->
    try
	false = code:purge(M),
	{module, M} = code:load_file(M),
	ok
    catch
	E:R ->
	    {error, {E,R}}
    end.


%%%% private

-spec make_sub_graph(string(), atom() | liga_model()) -> liga_model().
make_sub_graph(S, M) when is_atom(M) ->
    LG = M:model(),
    make_sub_graph(S, LG);

make_sub_graph(S, LG) ->
    Ts = string_to_trigrams(S),
    {nodes, LNs} = lists:keyfind(nodes, 1, LG),
    {edges, LEs} = lists:keyfind(edges, 1, LG),
    Ns = pull(Ts, LNs),
    Es = pull(get_edges(Ts), LEs),
    NW = get_weights(Ns),
    EW = get_weights(Es),
    [{node_weights, NW}, {edge_weights, EW},{nodes, Ns},{edges, Es}].

-spec string_to_trigrams(string()) -> list(trigram()).
string_to_trigrams([C]) ->
    [{?POINT_START, C, ?POINT_END}];
string_to_trigrams([X,Y|_] = S) ->
    [{?POINT_START, X, Y} | get_trigrams(S)].


%%%% helpers

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

-spec get_weights(list(weighted_item())) -> non_neg_integer().
get_weights(Xs) ->
    D = lists:foldl(fun({_,V}, Acc) ->
			    merge(1, V, Acc)
		    end, [], Xs),
    lists:sum([Y || {_,Y} <- D]).

-spec incr(non_neg_integer()) -> non_neg_integer().
incr(X) -> X+1.

-spec merge(float() | integer(), list(), list()) -> list().
merge(N, L1, L2) ->
    lists:foldl(fun({K, V}, Acc) ->
			case lists:keyfind(K, 1, Acc) of
			    false ->
				[{K, V/N} | Acc];
			    {K, W} ->
				[{K, W + (V/N)} | lists:delete({K,W},Acc)]
			end
		end, L2, L1).

-spec pull(list(mnode() | medge()), list(weighted_item())) 
	  -> list(weighted_item()).
pull(X, L) ->
    lists:foldl(fun(T, Acc) ->
			V = lists:keyfind(T, 1, L),
			W = lists:keyfind(T, 1, Acc),
			case {V,W} of 
			    {false, _} -> Acc;
			    {_, false} -> [V | Acc];
			    _ -> Acc
			end
		end, [], X).

-spec score(liga_model()) -> liga_score().
score([{node_weights, NW}, {edge_weights, EW},{nodes, Ns},{edges, Es}]) ->
    S1 = score_acc(Ns, NW, []),
    S2 = score_acc(Es, EW, S1),
    lists:sort(fun({_,V},{_,W}) -> V > W end,
	       lists:map(fun({K,V}) -> {K, V/2} end, S2)).

-spec score_acc(list(weighted_item()), non_neg_integer(), liga_score()) 
	       -> liga_score().
score_acc(Xs, W, S) ->
    lists:foldl(fun({_,V}, Acc) -> 
			D = W * length(V),  % Ivan's original research
			merge(D, V, Acc)
		end, S, Xs).

