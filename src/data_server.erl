-module(data_server).
-behaviour(gen_server).
-include("types.hrl").

-export([start_link/1, start_link/2,
	 start/2, %% just for common_test
	 stop/0,
	 size/1, size/2,
	 get_labels/0,
	 get_accounts/1, shuffle_accounts/1,
	 get_account/1,
	 get_label_account/0,
	 get_with_complement/3,
	 get_with_complement/4,
	 get_from_accs/3
	]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-ifdef(TEST).
%% Make private functions accessible to common_test.
-compile([export_all]).
-endif.

-record(state, {data_set :: atom(), 
		data_dir :: string(), 
		data     :: dict:dict(label(), 
				      dict:dict(string(), list(string())))
	       }).

%%%% API

start_link(DataSet) ->
    start_link(DataSet, "LIGA_test_dataset").

start_link(DataSet, DataDir) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [DataSet, DataDir], []).

start(DataSet, DataDir) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [DataSet, DataDir], []).

stop() ->
    gen_server:call(?MODULE, stop).

size(Lab) ->
    gen_server:call(?MODULE, {size, Lab}).

size(Lab, Acc) ->
    gen_server:call(?MODULE, {size, Lab, Acc}).

get_labels() ->
    gen_server:call(?MODULE, get_labels).

get_accounts(Lab) ->
    gen_server:call(?MODULE, {get_accounts, Lab}).

shuffle_accounts(Lab) ->
    Accs = get_accounts(Lab),
    shuffle(Accs).

get_account(Lab) ->
    choose_one(get_accounts(Lab)).

get_label_account() ->
    Lab = choose_one(get_labels()),
    Acc = choose_one(get_accounts(Lab)),
    {Lab, Acc}.

-spec get_with_complement(atom(), pcage(), non_neg_integer() | all) 
			 -> {[labelled_string()],[labelled_string()]}.
get_with_complement(Lab, PCage, NComp) ->
    gen_server:call(?MODULE, {get_with_complement, Lab, all, PCage, NComp}).

-type pcage() :: non_neg_integer().
-spec get_with_complement(atom(), any(), pcage(), non_neg_integer() | all) 
			 -> {[labelled_string()],[labelled_string()]}.
get_with_complement(Lab, Acc, PCage, NComp) ->
    gen_server:call(?MODULE, {get_with_complement, Lab, Acc, PCage, NComp}).

get_from_accs(Lab, Accs, NTests) ->
    lists:foldl(fun(Ra, Ac) ->
			{X,_} = get_with_complement(Lab, Ra, {nm, NTests}, 0),
			X ++ Ac
		end,
		[],
		Accs).
	
%%%% gen_server callbacks

init([DataSet, DataDir]) ->
    Data = get_data(DataSet, DataDir),
    {ok, #state{data_set=DataSet, data_dir=DataDir,data=Data}}.

handle_call(get_labels, _From, State=#state{data=Data}) ->
    Reply = dict:fetch_keys(Data),
    {reply, Reply, State};

handle_call({get_accounts, Lab}, _From, State=#state{data=Data}) ->
    F = fun(D) ->
		dict:fetch_keys(D)
	end,
    E = [],
    Reply = do_if_valid_key(Lab, Data, F, E),
    {reply, Reply, State};

handle_call({size, Lab}, _From, State=#state{data=Data}) ->
    F = fun(D) ->
		get_size(D)
	end,
    E = 0,
    Reply = do_if_valid_key(Lab, Data, F, E),
    {reply, Reply, State};
			       
handle_call({size, Lab, Acc}, _From, State=#state{data=Data}) ->
    F = fun(D) ->
		case dict:find(Acc, D) of
		    error -> 0;
		    {ok, AL} -> length(AL)
		end
	end,
    E = 0,
    Reply = do_if_valid_key(Lab, Data, F, E),
    {reply, Reply, State};

%% {pc, PCage}
%% {nm, N}
handle_call({get_with_complement, Lab, Acc, NPC, NComp}, _From,
	    State=#state{data=Data}) ->
    F = fun(D) ->
		All = get_all_or_empty(D, Acc),
		Size = length(All),
		NGet = valid_nget(NPC, Size),
		Ngc = case NComp of 
			  all -> Size;
			  _   -> NGet + NComp
		      end,
		DecL = lists:sort([{rand:uniform(), {Lab,N}} || N <- All]),
		{Sel,_} = lists:split(Ngc, [X||{_,X} <- DecL]),
		lists:split(NGet, Sel)
	end,
    E =  {[],[]},
    Reply = do_if_valid_key(Lab, Data, F, E),
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%% Private

choose_one(L) ->
    lists:nth(rand:uniform(length(L)), L).

do_if_valid_key(Lab, Data, F, Else) ->
    case dict:find(Lab, Data) of
	error    -> Else;
	{ok, LD} -> F(LD)
    end.

get_all_or_empty(D, all) ->
    dict:fold(fun(_, V, A) -> V ++ A end, [], D);
get_all_or_empty(D, Acc) ->
    case dict:find(Acc, D) of
	error    -> [];
	{ok, AL} -> AL
    end.

get_size(D) ->
    dict:fold(fun(_, V, A) -> 
		      A + length(V)
	      end,
	      0, D).

get_data(liga, DataDir) ->
    LabDirs = [{de, "de_DE"},
	       {en, "en_UK"},
	       {es, "es_ES"},
	       {fr, "fr_FR"},
	       {it, "it_IT"},
	       {nl, "nl_NL"}
	      ],
    lists:foldl(fun({Lab, Dir}, Dict) ->
			LabDir = DataDir ++ "/" ++ Dir,
			dict:store(Lab, import_data({liga, Dir}, LabDir), Dict)
		end,
		dict:new(),
		LabDirs);
get_data(Set, Dir) ->
    {e_not_implemented, Set, Dir}.


-spec import_data(atom() | tuple(), string()) -> dict:dict().
import_data({liga, FPref}, Dir) ->
    true = filelib:is_dir(Dir),
    Regex = FPref ++ "_\\w+",
    filelib:fold_files(Dir, Regex, false,
		       fun(Fn, D) ->
			       {ok, File} = file:open(Fn, [read, binary]),
			       Lines = util:read_utf8(File),
			       dict:store(Fn, Lines, D)
		       end, 
		       dict:new());
import_data(Set, Dir) ->
    {e_not_implemented, Set, Dir}.
    

shuffle(L) ->
    [X||{_,X} <- lists:sort([{rand:uniform(), N} || N <- L])].

valid_nget({nm, N}, Size) ->
    lists:min([N, Size]);
valid_nget({pc, P}, Size) ->
    Size * P div 100.
