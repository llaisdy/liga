-module(liga).

-compile([export_all]).

-export([get_likely/1,
	 new/0, import_string/3, 
	 classify/1, classify/2,
	 export_erl/2,
	 update_model/1, update_model/2]).

-define(POINT_START, 32).
-define(POINT_END, 32).

%% nb use "ligaModel" not liga_model in case of clashes with
%% other uses of "liga_model"
-record(ligaModel, {labels=[] :: list(),
		     node_weights=0 :: non_neg_integer(), 
		     edge_weights=0 :: non_neg_integer(), 
		     nodes=[] :: list(), 
		     edges=[] :: list()}).

-type ligaModel() :: #ligaModel{}.
-type label() :: atom().
-type score() :: float().
-type label_score() :: {label(), score()}.
-type liga_score()  :: list(label_score()). % sorted by score
-type mnode() :: any().
-type medge() :: {node(), node()}.
-type trigram() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type weighted_item() :: {any(), non_neg_integer()}.

-spec new() -> ligaModel().
new() ->
    #ligaModel{}.

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

update_labels(M=#ligaModel{labels=[]}, L) ->
    M#ligaModel{labels=[L]};
update_labels(M=#ligaModel{labels=Ls}, L) ->
    case lists:member(L, Ls) of
	true ->
	    M;
	false ->
	    M#ligaModel{labels=[L|Ls]}
    end.

-spec import_node(ligaModel(), mnode(), label()) -> ligaModel().
import_node(Model=#ligaModel{nodes=NL, node_weights=NW}, Node, Label) ->
    Nodes = ofl(NL),
    NewNodes = orddict:update(Node, fun(Ws) ->
					 orddict:update(Label, fun incr/1, 1, ofl(Ws))
				 end, [{Label,1}], Nodes),
    Model#ligaModel{nodes=NewNodes, node_weights=incr(NW)}.

-spec import_edge(ligaModel(), medge(), label()) -> ligaModel().
import_edge(Model=#ligaModel{edges=EL, edge_weights=EW}, Edge, Label) ->
    Edges = ofl(EL),
    NewEdges = orddict:update(Edge, fun(Ws) ->
					 orddict:update(Label, fun incr/1, 1, ofl(Ws))
				 end, [{Label,1}], Edges),
    Model#ligaModel{edges=NewEdges, edge_weights=incr(EW)}.

ofl(L) ->
    orddict:from_list(L).



-spec classify(string()) -> liga_score().
classify(String) ->
    classify(ligaModel, String).

-spec classify(atom() | ligaModel(), string()) -> liga_score().
classify(Model, String) ->
    SG = make_sub_graph(Model, String),
    score(SG).

-spec get_likely(liga_score()) -> list(label()).
get_likely([]) -> [];
get_likely(Ls) ->
    Mean = lists:sum([Y || {_,Y} <- Ls])  /  length(Ls),
    [L || {L,_} <- lists:filter(fun({_,N}) -> N >= Mean end, Ls)].

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

-spec export_erl(ligaModel(), atom()) -> ok.
export_erl(Model, Name) ->
    todo.

%%%% private

-spec make_sub_graph(atom() | ligaModel(), string()) -> ligaModel().
make_sub_graph(M, S) when is_atom(M) ->
    LG = M:model(),
    make_sub_graph(LG, S);

make_sub_graph(#ligaModel{nodes=LNs, edges=LEs}, S) ->
    Ts = string_to_trigrams(S),
    Ns = pull(Ts, LNs),
    Es = pull(get_edges(Ts), LEs),
    NW = get_weights(Ns),
    EW = get_weights(Es),
    Ls = get_labels(Ns),
    #ligaModel{labels=Ls, node_weights=NW, edge_weights=EW, nodes=Ns, edges=Es}.

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

get_labels(Ns) ->
    lists:foldl(fun({_,V}, Acc) ->
			case lists:member(V, Acc) of
			    true ->
				Acc;
			    false ->
				[V|Acc]
			end
		end, [], Ns).

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

-spec score(ligaModel()) -> liga_score().
score(#ligaModel{node_weights=NW, edge_weights=EW,nodes=Ns,edges=Es}) ->
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

