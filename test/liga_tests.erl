-module(liga_tests).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

build_small_long_test_() ->
    Data = [{en, "is this a test"}
	   ,{nl, "is dit een test"}
	   ],
    Model = lists:foldl(fun({Lab, Str}, Acc) ->
				liga_model:import_string(Acc, Str, Lab)
			end, liga_model:new(), Data),
    Act = liga_train:build_model(small_data()),
    ?_assertEqual(Model, Act).

build_model_small_test_() ->
    Exp = liga_test_utils:get_model_small(),
    Act = liga_train:build_model(small_data()),
    ?_assertEqual(sort_labels(Exp), sort_labels(Act)).

classify_small_test_() ->
    M = liga_test_utils:get_model_small(),
    Exp = [{nl,0.5744047619047619}, {en,0.14583333333333331}],
    Act = liga_model:classify(M, "is dit ook een test"),
    ?_assertEqual(Exp, Act).

small_data() ->
    dict:from_list([{en, ["is this a test"]}
		   ,{nl, ["is dit een test"]}
		   ]).

sort_labels(#{labels:=Labs} = LM) ->
    maps:put(labels, lists:sort(Labs), LM).
