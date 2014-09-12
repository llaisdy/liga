-module(liga_writer_tests).

-include_lib("eunit/include/eunit.hrl").

write_small_test_() ->
    M = liga_test_utils:get_model_small(),
    ok = liga_writer:write_model(M, liga_test_model_small),
    ?_assertEqual(1,1).

write_use_small_test_() ->
    write_small_test_(),
    Exp = load_liga_test_model_small(),
    Act = load_liga_model_small(),
    ?_assertEqual(Exp, Act).

%%%% 

load_liga_model_small() ->
    ok = liga_writer:read_model("test_data/liga_model_small.erl"),
    liga_model_small:model().

load_liga_test_model_small() ->
    ok = liga_writer:read_model("liga_test_model_small.erl"),
    liga_test_model_small:model().


