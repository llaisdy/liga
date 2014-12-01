-module(calibration).
-compile([export_all]).

-type labelled_string() :: {atom(), string()}.

-define(LABELS, [de, en, es, fr, it, nl]).

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
    {TrainingData, TestData} = setup(Type, Set, N),
    {Type, Set, run(TrainingData, TestData)}.


%%%% training data: random x% of each language
%%%% test data: random ten from rest (all six languages)

-spec setup(atom(), atom(), non_neg_integer()) -> {[labelled_string()], [labelled_string()]}.
setup(sample_size, PCage, NTests) ->
    Xs = lists:map(fun(Lab) ->
			  Size = data_server:size(Lab),
			  NTrain = Size * PCage div 100,
			  data_server:get_with_complement(Lab, NTrain, NTests)
		  end,
		  ?LABELS),
    lists:foldl(fun({A, B}, {C, D}) ->
			{A++C, B++D}
		end,
	       {[],[]},
	       Xs);

setup(Type, Set, _) ->
    {e_not_implemented, Type, Set}.

-spec run([labelled_string()], [labelled_string()]) -> any().
run(TrainingData, TestData) ->
    M = build_model(TrainingData),
    lists:map(fun({Lab, Str}) ->
		      Res = liga:classify(M, Str),
		      {correct(Res, Lab), Res}
	      end,
	      TestData).

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
