-module(calibration).
-compile([export_all]).

-type labelled_string() :: {atom(), string()}.

%%%% For each setup, do fifty different ten-fold validation tests & take average.

calibrate() ->
    register_data_server(),
    NTrials = 50,
    NTestsPerTrial = 10,
    TestSets = [{sample_size, [5, 10, 25, 50]}, 
                {spec_gen, [spec, gen]},
                {holdouts, [1, 2]}],
    lists:map(fun({Type, Sets}) -> 
		      lists:map(fun(Set) ->
					do_trials(NTrials, NTestsPerTrial, Type, Set)
				end,
				Sets)
              end,
              TestSets).

register_data_server() ->
    %% gen_server to serve up LIGA Tromp test data
    data_server:start_link().

do_trials(0, _, _, _) ->
    [];
do_trials(N, NTestsPerTrial, Type, Set) ->
    [do_tests(Type, Set, NTestsPerTrial) |
     do_trials(N - 1, NTestsPerTrial, Type, Set)].

do_tests(Type, Set, N) ->
    Frames = setup(Type, Set, N),
    run_frames(Frames).

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
		{TrainingData,_} = data_server:get_with_complement(Lab, Acc, {pc,67}, 0),
		TestData = data_server:get_from_accs(Lab, Rest, NTests),
		{TrainingData, TestData}
	end,
    package_data(gen, F);

setup(holdouts, NHoldouts, NTests) ->
    %% for each language:
    %% training data = data from all accounts except holdout account(s) - (how much?)
    %% test data = data from holdout account(s) - (how much?)
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

run_frames([]) -> [];
run_frames([H|T]) -> 
    [run_one(H) | run_frames(T)].

-spec run_one({tuple(), {[labelled_string()], [labelled_string()]}}) -> any().
run_one({TestLabel, {TrainingData, TestData}}) ->
    M = build_model(TrainingData),
    R = lists:map(fun({Lab, Str}) ->
			  Res = liga:classify(M, Str),
			  {Lab, Res}
			  %% {correct(Res, Lab), Res}
		  end,
		  TestData),
    {TestLabel, R}.

build_model(Ls) ->
    lists:foldl(fun({Label, String}, Model) ->
			liga:import_string(Model, String, Label)
		end,
		liga:new(),
		Ls).

correct([{L,_}|_],L) ->
    true;
correct(_,_) ->
    false.

package_data(TestLabel, F) ->
    Labs = data_server:get_labels(),
    Xs = lists:map(fun(Lab) ->
			   F(Lab)
		   end,
		   Labs),
    [{TestLabel, split_data(Xs)}].

split_data(Xs) ->
    lists:foldl(fun({A, B}, {C, D}) ->
			{A++C, B++D}
		end,
		{[],[]},
		Xs).

