-module(liga_tests).

-include_lib("eunit/include/eunit.hrl").

build_model_small_test_() ->
    Exp = liga_test_utils:get_model_small(),
    Act = build_model_small(),
    ?_assertEqual(Exp, Act).

classify_small_test_() ->
    M = liga_test_utils:get_model_small(),
    Exp = [{nl,0.5744047619047619}, {en,0.14583333333333331}],
    Act = liga:classify(M, "is dit ook een test"),
    ?_assertEqual(Exp, Act).

build_model_small() ->
    lists:foldl(fun({String, Label}, Model) ->
			liga:import_string(Model, String, Label)
		end,
		liga:new(),
		[{"is dit een test", nl},
		 {"is this a test", en}]
	       ).

