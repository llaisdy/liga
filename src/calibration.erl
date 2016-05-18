-module(calibration).
-include("types.hrl").

-export([calibrate/0
	,calibrate/2
	,calibrate/3
	,calibrate/4
	,setup/3
	,run_frame/1
	]).


calibrate() ->
    %% For each setup, do fifty different ten-fold validation tests & take average.
    NTrials = 50,
    NTestsPerTrial = 10,
    calibrate(NTrials, NTestsPerTrial).

calibrate(NTrials, NTestsPerTrial) ->
    TestSets = [{sample_size, [5, 10, 25, 50]}, 
                {spec_gen, [spec, gen]},
                {holdouts, [1, 2]}],
    calibrate(NTrials, NTestsPerTrial, TestSets).

calibrate(NTrials, NTestsPerTrial, TestSets) ->
    DataSet = liga,
    calibrate(NTrials, NTestsPerTrial, TestSets, DataSet).

calibrate(NTrials, NTestsPerTrial, TestSets, DataSet) ->
    data_server:start_link(DataSet),
    lists:map(fun({Type, Sets}) -> 
		      lists:map(fun(Set) ->
					do_trials(NTrials, NTestsPerTrial, Type, Set)
				end,
				Sets)
              end,
              TestSets).

-spec setup(atom(), atom(), non_neg_integer()) ->
		   {[labelled_string()], [labelled_string()]}.
setup(sample_size, PCage, NTests) ->
    %% for each language:
    %% training data: random x% of each language
    %% test data: random NTests from rest
    F = fun(Lab) ->
		data_server:get_with_complement(Lab, {pc,PCage}, NTests)
	end,
    package_data({sample_size, PCage}, F);

setup(spec_gen, spec, NTests) ->
    %% for each language:
    %% training data = 2/3 of all data of one single account
    %% test data 1 (specialisation) = all rest of data from that account
    F = fun(Lab) ->
		Acc = data_server:get_account(Lab),
		data_server:get_with_complement(Lab, Acc, {pc,67}, NTests)
	end,
    package_data(spec, F);

setup(spec_gen, gen, NTests) ->
    %% for each language:
    %% training data = 2/3 of all data of one single account
    %% test data 2 (generalisation) = data from all other accounts
    F = fun(Lab) ->
		[Acc | Rest] = data_server:shuffle_accounts(Lab),
		{TrainingData,_} = data_server:get_with_complement(Lab, Acc, 
								   {pc,67}, 0),
		TestData = data_server:get_from_accs(Lab, Rest, NTests),
		{TrainingData, TestData}
	end,
    package_data(gen, F);

setup(holdouts, NHoldouts, NTests) ->
    %% for each language:
    %% training data = 2/3 of data from all accounts except holdout account(s)
    %% test data = data from holdout account(s)
    F = fun(Lab) ->
		{HOs, NHOs} = lists:split(NHoldouts,
					  data_server:shuffle_accounts(Lab)),
		TrainingData = data_server:get_from_accs(Lab, NHOs, {pc, 67}),
		TestData = data_server:get_from_accs(Lab, HOs, {nm, NTests}),
		{TrainingData, TestData}
	 end,
    package_data({holdouts, NHoldouts}, F);

setup(Type, Set, _) ->
    {e_not_implemented, Type, Set}.

-spec run_frame({tuple(), {[labelled_string()], [labelled_string()]}}) -> any().
run_frame({TestLabel, {TrainingData, TestData}}) ->
    LabDict = dataset_to_labdict(TrainingData),
    M = liga_train:build_model(LabDict),
    Results = lists:map(fun({Lab, Str}) ->
			  util:annotated_trial(M, Lab, Str)
		  end,
		  TestData),
    PC = util:pc_correct(Results),
    {TestLabel, PC}.


%%%% private

do_trials(0, _, _, _) ->
    [];
do_trials(N, NTestsPerTrial, Type, Set) ->
    [do_tests(Type, Set, NTestsPerTrial) |
     do_trials(N - 1, NTestsPerTrial, Type, Set)].

do_tests(Type, Set, N) ->
    Frame = setup(Type, Set, N),
    run_frame(Frame).

-spec dataset_to_labdict(list({atom(), string()})) -> 
				dict:dict(atom(), list(string())).
dataset_to_labdict(TrainingData) ->
    lists:foldl(fun({Lab, Str}, Acc) ->
			dict:append(Lab, Str, Acc)
		end, 
		dict:new(), 
		TrainingData).

package_data(TestLabel, F) ->
    Labs = data_server:get_labels(),
    Xs = lists:map(fun(Lab) ->
			   F(Lab)
		   end,
		   Labs),
    {TestLabel, split_data(Xs)}.

split_data(Xs) ->
    lists:foldl(fun({A, B}, {C, D}) ->
			{A++C, B++D}
		end,
		{[],[]},
		Xs).

