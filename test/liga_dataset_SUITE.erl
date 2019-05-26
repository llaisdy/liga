-module(liga_dataset_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, 
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2
	 ]).

-export(
   [
    load_data/1,
    private_function/1,

    import_data/1,
    get_with_complement/1
   ]).

all() ->
    [
     load_data,
     private_function,
     import_data,
     get_with_complement
    ].


%%%% set up

init_per_suite(Config) ->
    DataSet = liga,
    DataDir = ?config(data_dir, Config) ++ "LIGA_test_dataset",
    {ok, Pid} = data_server:start(DataSet, DataDir),
    [{data_server, Pid}, {data_set, DataSet} | Config].

end_per_suite(_Config) ->
    stopped = data_server:stop(),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%%% tests

load_data(_Config) ->
    [de, en, es, fr, it, nl] = lists:sort(data_server:get_labels()),
    ok.

private_function(_Config) ->
    Data = [{as,"dfg"},{qw,"ert"},{zx,"cvb"},{qw,"yui"},{as,"hjk"}],
    Exp = [{as,["dfg","hjk"]},{qw,["ert","yui"]},{zx,["cvb"]}],
    Labdict = calibration:dataset_to_labdict(Data),
    Act = lists:sort(dict:to_list(Labdict)),
    ?assertEqual(Exp, Act).

import_data(Config) ->
    DDir = ?config(data_dir, Config) ++ "LIGA_test_dataset/" ++ "fr_FR" ++ "/",
    DE = data_server:import_data({liga, "fr_FR"}, DDir),
    DEL = dict:to_list(DE),
    Exp = 10, %% 10 accounts in each langauge
    Act = length(DEL),
    ?assertEqual(Exp, Act).

get_with_complement(_Config) ->
    {L1, L2} = data_server:get_with_complement(fr,all,{pc,10},100),
    ?assert(length(L1) > 0),
    ?assert(length(L2) > 0),
    ok.
