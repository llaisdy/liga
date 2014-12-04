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
                {holdouts, [holdout1, holdout2]}],
    lists:map(fun({Type, Sets}) -> 
		      lists:map(fun(Set) ->
					do_trials(NTrials, {NTestsPerTrial, Type, Set})
				end,
				Sets)
              end,
              TestSets).

register_data_server() ->
    %% gen_server to serve up LIGA Tromp test data
    data_server:start_link().

do_trials(0, _) ->
    [];
do_trials(N, {NTestsPerTrial, Type, Set}) ->
    [do_tests(Type, Set, NTestsPerTrial) |
     do_trials(N - 1, {NTestsPerTrial, Type, Set})].

do_tests(Type, Set, N) ->
    Frames = setup(Type, Set, N),
    run_frames(Frames).


-spec setup(atom(), atom(), non_neg_integer()) -> {[labelled_string()], [labelled_string()]}.
setup(sample_size, PCage, NTests) ->
    %% for each language:
    %% training data: random x% of each language
    %% test data: random NTests from rest
    Labs = data_server:get_labels(),
    Xs = lists:map(fun(Lab) ->
			   data_server:get_with_complement(Lab, PCage, NTests)
		   end,
		   Labs),
    [{{sample_size, PCage}, 
      lists:foldl(fun({A, B}, {C, D}) ->
			  {A++C, B++D}
		  end,
		  {[],[]},
		  Xs)}];

setup(spec_gen, spec, NTests) ->
    %% for each language:
    %% training data = 2/3 of all data of one single account
    %% test data 1 (specialisation) = all rest of data from that account
    Labs = data_server:get_labels(),
    Xs = lists:map(fun(Lab) ->
			   Acc = data_server:get_account(Lab),
			   data_server:get_with_complement(Lab, Acc, 67, NTests)
		   end,
		   Labs),
    [{spec,
      lists:foldl(fun({A, B}, {C, D}) ->
			  {A++C, B++D}
		  end,
		  {[],[]},
		  Xs)}];

setup(spec_gen, gen, NTests) ->
    %% for each language:
    %% training data = 2/3 of all data of one single account
    %% test data 2 (generalisation) = data from all other accounts
    Labs = data_server:get_labels(),
    Xs = lists:map(fun(Lab) ->
			   [Acc | Rest] = data_server:shuffle_accounts(Lab),
			   TrainingData = data_server:get_data(Lab, Acc, 67),
			   TestData = lists:foldl(fun(Ra, Ac) ->
							  data_server:get_data(Lab, Ra, NTests) ++ Ac
						  end,
						  [],
						  Rest),
			   {TrainingData, TestData}
			   
		   end,
		   Labs),
    [{gen,
      lists:foldl(fun({A, B}, {C, D}) ->
			  {A++C, B++D}
		  end,
		  {[],[]},
		  Xs)}];
    


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
