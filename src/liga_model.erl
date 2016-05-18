-module(liga_model).
-include("types.hrl").

-export([new/0
	,make/3
	,import_string/3
	,classify/2]).

-spec new() -> ligaModel().
new() ->
    make([], liga_labmap:new(), liga_labmap:new()).

-spec make(list(label()), labmap(), labmap()) -> ligaModel().
make(Labels, NodeMap, EdgeMap) ->
    #{labels => Labels, 
      node_weights => liga_labmap:get_weights(NodeMap),
      edge_weights => liga_labmap:get_weights(EdgeMap),
      nodes => NodeMap,
      edges => EdgeMap}.

-spec import_string(ligaModel(), string(), label()) -> ligaModel().
import_string(Model, String, Label) ->
    M1 = import_nodes_edges(Model, String, Label),
    update_labels(M1, Label).

-spec classify(atom() | ligaModel(), string()) -> liga_score().
classify(Model, String) ->
    SG = make_sub_graph(Model, String),
    score(SG).

%%%% private

-spec make_sub_graph(atom() | ligaModel(), string()) -> ligaModel().
make_sub_graph(M, S) when is_atom(M) ->
    LG = M:model(),
    make_sub_graph(LG, S);
make_sub_graph(#{nodes:=NodeMap, edges:=EdgeMap}, S) ->
    Ts = liga_util:string_to_trigrams(S),
    Ns = liga_labmap:submap(Ts, NodeMap),
    Es = liga_labmap:submap(liga_util:trigrams_to_edges(Ts), EdgeMap),
    Ls = liga_labmap:get_labels(Ns),
    make(Ls, Ns, Es).

-spec score(ligaModel()) -> liga_score().
score(#{node_weights:=NW, edge_weights:=EW, nodes:=NodeMap, edges:=EdgeMap}) ->
    S1 = liga_labmap:score(NW, [], NodeMap),
    S2 = liga_labmap:score(EW, S1, EdgeMap),
    lists:sort(fun({_,V},{_,W}) -> V > W end,
	       lists:map(fun({K,V}) -> {K, V/2} end, S2)).

-spec import_nodes_edges(ligaModel(), string(), label()) -> ligaModel().
import_nodes_edges(Model, String, Label) ->
    Ts = liga_util:string_to_trigrams(String),
    M1 = lists:foldl(fun(T, Acc1) ->
			     import_node(Acc1, T, Label)
		     end, 
		     Model, Ts),
    lists:foldl(fun(E, Acc2) ->
			import_edge(Acc2, E, Label)
		end, 
		M1, liga_util:trigrams_to_edges(Ts)).

-spec update_labels(ligaModel(), label()) -> ligaModel().
update_labels(#{labels:=Ls}=M, L) ->
    case lists:member(L, Ls) of
	true  -> M;
	false -> M#{labels:=[L|Ls]}
    end.

-spec import_node(ligaModel(), mnode(), label()) -> ligaModel().
import_node(#{nodes:=Nodes, node_weights:=NW}=Model, Node, Label) ->
    Model#{nodes := liga_labmap:put(Node, Label, Nodes),
	   node_weights := NW+1}.

-spec import_edge(ligaModel(), medge(), label()) -> ligaModel().
import_edge(#{edges:=Edges, edge_weights:=EW}=Model, Edge, Label) ->
    Model#{edges := liga_labmap:put(Edge, Label, Edges),
	   edge_weights := EW+1}.
