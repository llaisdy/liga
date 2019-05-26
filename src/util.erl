-module(util).
-include("types.hrl").

-export([
	 annotated_trial/3,
	 incr/3,
	 pc_correct/1,
	 read_utf8/1,
	 read_file_utf8/1
	]).

-spec annotated_trial(ligaModel(), label(), string()) ->
			     {boolean(), label(), liga_score()}.
annotated_trial(M, Lab, Str) ->
    Res = liga_model:classify(M, Str),
    {is_correct(Lab, Res), Lab, Res}.

-spec incr(list(non_neg_integer()), non_neg_integer(), non_neg_integer()) ->
		  list(non_neg_integer()).
incr([], _, _)    -> [];
incr([Top|T], Bot, Top) -> [Bot|incr(T, Bot, Top)];
incr([X|T], _, _) -> [X+1|T].

-spec pc_correct(list({atom(), atom(), list()})) -> float().
pc_correct(Rs) ->
    100 * length([true || {true,_,_} <- Rs]) / length(Rs).

-spec read_utf8(term()) -> list(string()).
read_utf8(File) ->
    case file:read_line(File) of
        {ok, Data} ->
	    [unicode:characters_to_list(Data, utf8) | read_utf8(File)];
	eof        -> []
    end.

-spec read_file_utf8(string()) -> string().
read_file_utf8(Fn) ->
    {ok, B} = file:read_file(Fn),
    unicode:characters_to_list(B, utf8).

%%%% private

-spec is_correct(term(), list()) -> true | false.
is_correct(L, [{L,_}|_]) -> true;
is_correct(_,_)          -> false.

%% -spec mean_std_dev(list(float())) -> {float(), float()}.
%% mean_std_dev(Vs) ->
%%     L = length(Vs),
%%     M = lists:sum(Vs) / L,
%%     X = lists:sum([(V-M)*(V-M) || V <- Vs]) / L,
%%     S = math:sqrt(X),
%%     {M,S}.

%%%% tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

incr_test(X, Bot, Top) ->
    Y = incr(X, Bot, Top),
    io:format("~p~n~n",[Y]),
    case lists:all(fun(Z) -> Z =:= Bot end, Y) of
	false ->
	    timer:sleep(333),
	    incr_test(Y, Bot, Top);
	true ->
	    ok
    end.

-endif.
