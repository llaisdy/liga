-module(calibration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, 
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2
	 ]).

-export([sample_size/1
	,specialisation/1
	,generalisation/1
	,holdouts/1
	]).

all() ->
     [
      sample_size,
      specialisation,
      generalisation,
      holdouts
     ].


%%%% set up

init_per_suite(Config) ->
    DataSet = liga,
    DataDir = ?config(data_dir, Config) ++ "LIGA_test_dataset",
    {ok, Pid} = data_server:start(DataSet, DataDir),
    [{data_set, DataSet}, {data_server, Pid} | Config].

end_per_suite(_Config) ->
    stopped = data_server:stop(),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.


%%%% tests

sample_size(_Config) ->
    %% for each language:
    %% training data: random % of each language
    %% test data: random NTests from rest
    TrainPCs = [5,10,25,50],
    NTests = 100,
    lists:foreach(
      fun(TrPC) ->
	      TestFrame = calibration:setup(sample_size, TrPC, NTests),
	      run_report(TestFrame)
      end, TrainPCs),
    ok.

specialisation(_Config) ->
    %% for each language:
    %% training data = 2/3 of all data of one single account
    %% test data = all rest of data from that account
    NTests = 10,
    TestFrame = calibration:setup(spec_gen, spec, NTests),
    run_report(TestFrame),
    ok.

generalisation(_Config) ->
    %% for each language:
    %% training data = 2/3 of all data of one single account
    %% test data = data from all other accounts
    NTests = 10,
    TestFrame = calibration:setup(spec_gen, gen, NTests),
    {_TestLabel, {TrainingData, TestData}} = TestFrame,
    ?assert(length(TrainingData) > 0),
    ?assert(length(TestData) > 0),
    run_report(TestFrame),
    ok.

holdouts(_Config) ->
    %% for each language:
    %% training data = 2/3 of data from all accounts except holdout account(s)
    %% test data = data from holdout account(s)
    NTests = 10,
    NHoldouts = 1,
    TestFrame = calibration:setup(holdouts, NHoldouts, NTests),
    {_TestLabel, {TrainingData, TestData}} = TestFrame,
    ?assert(length(TrainingData) > 0),
    ?assert(length(TestData) > 0),
    run_report(TestFrame),
    ok.

%%%% private

run_report(TestFrame) ->
    {ok, {Label, PcCorrect}} = calibration:run_frame(TestFrame),
    ct:pal("~p: correct: ~.3f%",[Label, PcCorrect]).

