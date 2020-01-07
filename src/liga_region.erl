-module(liga_region).
-include("types.hrl").

-export([init_regions/1
	,node_to_region/2
	,edge_to_region/2
	]).

init_regions(0) ->
    [];
init_regions(N) ->
    M = N - 1,
    [{emap, M}, {nmap, M} | init_regions(M)].

-spec node_to_region(mnode(), nonegint()) -> region().
node_to_region({X, Y, Z}, N) ->
    region(nmap, {X, Y, Z}, N).

-spec edge_to_region(medge(), nonegint()) -> region().
edge_to_region({{X, _, Y}, {_, Z, _}}, N) ->
    region(emap, {X, Y, Z}, N).

    
%%%% private

-spec region(atom(), {nonegint(), nonegint(), nonegint()}, nonegint()) -> region().
region(Type, {X, Y, Z}, N) ->
    {Type, lists:sum([X, Y, Z]) rem N}.
