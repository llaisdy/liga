-module(liga_train).
-include("types.hrl").

-export([build_model/1
	,build_model/2
	]).

%%%% parallel bit
-export([broadcast_label/5
	,loop_make_region_map/4
	,broadcast_string/5
	,send_trigrams/5
	,send_edges/5
	]).

%%%% api

-spec build_model(dict:dict(label(), list(string()))) -> 
			 ligaModel().
build_model(LD) ->
    build_model(LD, 27).

build_model(LD, NumRegions) ->
    Regions = liga_region:init_regions(NumRegions),
    ProcDict = spawn_map_builders(Regions),
    Labels = dict:fold(fun(Label, Strings, Acc) ->
			      spawn(?MODULE, broadcast_label,
				    [Label, Strings, NumRegions, ProcDict,
				     self()]),
			      [Label|Acc]
		      end, [], LD),
    recv_all_sent(Labels),
    ok = send_ready(ProcDict),
    {NMs, EMs} = recv_labmaps(Regions, {[], []}),
    NMap = liga_labmap:merge(NMs),
    EMap = liga_labmap:merge(EMs),
    liga_model:make(Labels, NMap, EMap).

%%%% private

-spec spawn_map_builders(list(region())) -> dict:dict(region(), pid()).
spawn_map_builders(Regions) ->
    lists:foldl(fun(Region, Acc) ->
			Pid = spawn(?MODULE, loop_make_region_map, 
				    [Region, liga_labmap:new(), 0, self()]),
			dict:store(Region, Pid, Acc)
		end,
		dict:new(),
		Regions).

loop_make_region_map(Region, Map, Weights, BossPid) ->
    receive
	{Item, Label} ->
	    NewMap = liga_labmap:put(Item, Label, Map),
	    loop_make_region_map(Region, NewMap, Weights+1, BossPid);
	ready ->
	    BossPid ! {Region, Map}
    end.

recv_all_sent([]) ->
    ok;
recv_all_sent(Labels) ->
    receive
	{done, Label} ->
	    recv_all_sent(lists:delete(Label, Labels))
    end.

send_ready(ProcDict) ->
    lists:foreach(
      fun({_, Pid}) -> Pid ! ready end,
      dict:to_list(ProcDict)).

recv_labmaps([], Models) ->
    Models;
recv_labmaps(Regions, {NMs, EMs}) ->
    receive
	{{emap, _} = Region, Map} ->
	    recv_labmaps(lists:delete(Region, Regions),
				 {NMs, [Map|EMs]});
	{{nmap, _} = Region, Map} ->
	    recv_labmaps(lists:delete(Region, Regions),
				 {[Map|NMs], EMs})
    end.

broadcast_label(Label, Strings, NumRegions, ProcDict, BossPid) ->
    lists:foreach(fun(String) ->
			  spawn(?MODULE, broadcast_string,
				[Label, String, NumRegions, ProcDict, self()])
		  end,
		  Strings),
    recv_done(length(Strings)),
    BossPid ! {done, Label}.

broadcast_string(Label, String, NRs, ProcDict, BossPid) ->
    Ts = trigrams:string_to_trigrams(String),
    spawn(?MODULE, send_trigrams, 
	  [Label, Ts, NRs, ProcDict, self()]),
    Es = trigrams:trigrams_to_edges(Ts),
    spawn(?MODULE, send_edges, 
	  [Label, Es, NRs, ProcDict, self()]),
    recv_done(2),
    BossPid ! done.

send_trigrams(Label, Ts, NRs, ProcDict, BossPid) ->
    lists:foreach(fun(T) ->
			  send_trigram(Label, T, NRs, ProcDict)
		  end, Ts),
    BossPid ! done.
    
send_edges(Label, Es, NRs, ProcDict, BossPid) ->
    lists:foreach(fun(E) ->
			  send_edge(Label, E, NRs, ProcDict)
		  end, Es),
    BossPid ! done.
    
send_trigram(Label, T, NRs, ProcDict) ->
    Region = liga_region:node_to_region(T, NRs),
    send_item(Label, T, Region, ProcDict).

send_edge(Label, E, NRs, ProcDict) ->
    Region = liga_region:edge_to_region(E, NRs),
    send_item(Label, E, Region, ProcDict).

send_item(Label, Item, Region, ProcDict) ->
    Pid = dict:fetch(Region, ProcDict),
    Pid ! {Item, Label}.

recv_done(0) -> ok;
recv_done(N) -> receive done -> recv_done(N-1) end.
