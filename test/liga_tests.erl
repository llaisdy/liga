-module(liga_tests).

-include_lib("eunit/include/eunit.hrl").

build_model_small_test_() ->
    {ligaModel, ELs, NW, EW, ENs, EWs} = get_model_small(),
    {ligaModel, ELs, NW, EW, ANs, AWs} = lists:foldl(fun({String, Label}, Model) ->
							      liga:import_string(Model, 
										 String, 
										 Label)
						      end,
						      liga:new(),
						      [{"is dit een test", nl},
						       {"is this a test", en}]
						     ),
    [?_assertEqual(lists:sort(ENs), lists:sort(ANs)),
     ?_assertEqual(lists:sort(EWs), lists:sort(AWs))].

classify_small_test_() ->
    M = get_model_small(),
    Exp = [{nl,0.5744047619047619}, {en,0.14583333333333331}],
    Act = liga:classify(M, "is dit ook een test"),
    ?_assertEqual(Exp, Act).

export_erl_small_test_() ->
    M = get_model_small(),
%%    ok = liga:export_erl(M, liga_model_small),
    ok = liga:update_model("test_data/liga_model_small.erl"),
    Exp = liga:classify(M, "is dit ook een test"),
    Act = liga:classify(liga_model_small, "is dit ook een test"),
    ?_assertEqual(Exp, Act).


%%%% helpers

get_model_small() ->
    {ligaModel,
     [en, nl], 29, 27, 
     [
      {{32, 97, 32}, [{en, 1}]},
      {{32, 100, 105}, [{nl, 1}]},
      {{32, 101, 101}, [{nl, 1}]},
      {{32, 105, 115}, [{en, 1}, {nl, 1}]},
      {{32, 116, 101}, [{en, 1}, {nl, 1}]},
      {{32, 116, 104}, [{en, 1}]},
      {{97, 32, 116}, [{en, 1}]},
      {{100, 105, 116}, [{nl, 1}]},
      {{101, 101, 110}, [{nl, 1}]},
      {{101, 110, 32}, [{nl, 1}]},
      {{101, 115, 116}, [{en, 1}, {nl, 1}]},
      {{104, 105, 115}, [{en, 1}]},
      {{105, 115, 32}, [{en, 2}, {nl, 1}]},
      {{105, 116, 32}, [{nl, 1}]},
      {{110, 32, 116}, [{nl, 1}]},
      {{115, 116, 32}, [{en, 1}, {nl, 1}]},
      {{115, 32, 100}, [{nl, 1}]},
      {{115, 32, 116}, [{en, 1}]},
      {{115, 32, 97}, [{en, 1}]},
      {{116, 101, 115}, [{en, 1}, {nl, 1}]},
      {{116, 104, 105}, [{en, 1}]},
      {{116, 32, 101}, [{nl, 1}]}
     ],
     [
      {{{32, 97, 32}, {97, 32, 116}}, [{en, 1}]},
      {{{32, 100, 105}, {100, 105, 116}}, [{nl, 1}]},
      {{{32, 101, 101}, {101, 101, 110}}, [{nl, 1}]},
      {{{32, 105, 115}, {105, 115, 32}}, [{en, 1}, {nl, 1}]},
      {{{32, 116, 101}, {116, 101, 115}}, [{en, 1}, {nl, 1}]},
      {{{32, 116, 104}, {116, 104, 105}}, [{en, 1}]},
      {{{97, 32, 116}, {32, 116, 101}}, [{en, 1}]},
      {{{100, 105, 116}, {105, 116, 32}}, [{nl, 1}]},
      {{{101, 101, 110}, {101, 110, 32}}, [{nl, 1}]},
      {{{101, 110, 32}, {110, 32, 116}}, [{nl, 1}]},
      {{{101, 115, 116}, {115, 116, 32}}, [{en, 1}, {nl, 1}]},
      {{{104, 105, 115}, {105, 115, 32}}, [{en, 1}]},
      {{{105, 115, 32}, {115, 32, 100}}, [{nl, 1}]},
      {{{105, 115, 32}, {115, 32, 116}}, [{en, 1}]},
      {{{105, 115, 32}, {115, 32, 97}}, [{en, 1}]},
      {{{105, 116, 32}, {116, 32, 101}}, [{nl, 1}]},
      {{{110, 32, 116}, {32, 116, 101}}, [{nl, 1}]},
      {{{115, 32, 100}, {32, 100, 105}}, [{nl, 1}]},
      {{{115, 32, 116}, {32, 116, 104}}, [{en, 1}]},
      {{{115, 32, 97}, {32, 97, 32}}, [{en, 1}]},
      {{{116, 101, 115}, {101, 115, 116}}, [{en, 1}, {nl, 1}]},
      {{{116, 104, 105}, {104, 105, 115}}, [{en, 1}]},
      {{{116, 32, 101}, {32, 101, 101}}, [{nl, 1}]}
     ]
    }.
