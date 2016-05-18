-module(liga_writer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, 
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2
	 ]).

-export([write_small/1
	,write_use_small/1
	]).

all() ->
     [write_small
     ,write_use_small
     ].

%%%% set up

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    [{model_name, liga_test_model_small} | Config].

end_per_testcase(_, _Config) ->
    Fn = atom_to_list(?config(model_name)) ++ ".erl",
    ok = file:delete(Fn).


%%%% tests

write_small(Config) ->
    M = liga_test_utils:get_model_small(),
    MName = ?config(model_name, Config),
    PrivDir = ?config(priv_dir, Config),
    Act = liga_writer:write_model(M, MName, PrivDir),
    ?_assertEqual(ok, Act).

write_use_small(Config) ->
    write_small(Config),
    Exp = load_liga_test_model_small(Config),
    Act = load_liga_model_small(Config),
    ?_assertEqual(Exp, Act).

%%%% private

load_liga_model_small(Config) ->
    DataDir = ?config(data_dir, Config),
    ok = liga_writer:read_model(DataDir ++ "/liga_model_small.erl"),
    liga_model_small:model().

load_liga_test_model_small(Config) ->
    PrivDir = ?config(priv_dir, Config),
    ok = liga_writer:read_model(PrivDir ++ "/liga_test_model_small.erl"),
    liga_test_model_small:model().


