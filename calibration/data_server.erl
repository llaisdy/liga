-module(data_server).
-behaviour(gen_server).
-compile([export_all]).

-type labelled_string() :: {atom(), string()}.

-export([start_link/0, stop/0,
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

-define(SERVER, ?MODULE).
%%-define(DATA_DIR, "LIGA_Benelearn11_dataset").
-define(DATA_DIR, "LIGA_test_dataset").
-define(LAB_DIRS, [
		   {de, "de_DE"},
		   {en, "en_UK"},
		   {es, "es_ES"},
		   {fr, "fr_FR"},
		   {it, "it_IT"},
		   {nl, "nl_NL"}
		  ]).

%% data = dict {lang : {acct : [str]}}
-record(state, {data}).

%%%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

size(Lab) ->
    gen_server:call(?SERVER, {size, Lab}).

size(Lab, Acc) ->
    gen_server:call(?SERVER, {size, Lab, Acc}).

get_labels() ->
    gen_server:call(?SERVER, get_labels).

get_accounts(Lab) ->
    gen_server:call(?SERVER, {get_accounts, Lab}).

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
    gen_server:call(?SERVER, {get_with_complement, Lab, all, PCage, NComp}).

-type pcage() :: non_neg_integer().
-spec get_with_complement(atom(), any(), pcage(), non_neg_integer() | all) 
			 -> {[labelled_string()],[labelled_string()]}.
get_with_complement(Lab, Acc, PCage, NComp) ->
    gen_server:call(?SERVER, {get_with_complement, Lab, Acc, PCage, NComp}).

get_from_accs(Lab, Accs, NTests) ->
    lists:foldl(fun(Ra, Ac) ->
			{X,_} = get_with_complement(Lab, Ra, {nm, NTests}, 0),
			X ++ Ac
		end,
		[],
		Accs).
	
%%%% gen_server callbacks

init([]) ->
    Data = lists:foldl(fun({Lab, Dir}, Dict) ->
			       LabDir = ?DATA_DIR ++ "/" ++ Dir,
			       dict:store(Lab, import_data(LabDir), Dict)
		       end,
		       dict:new(),
		       ?LAB_DIRS),
    {ok, #state{data=Data}}.

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
handle_call({get_with_complement, Lab, Acc, NPC, NComp}, _From, State=#state{data=Data}) ->
    F = fun(D) ->
		All = get_all_or_empty(D, Acc),
		Size = length(All),
		NGet = valid_nget(NPC, Size),
		Ngc = case NComp of 
			  all -> Size;
			  _   -> NGet + NComp
		      end,
		DecL = lists:sort([{random:uniform(), {Lab,N}} || N <- All]),
		{Sel,_} = lists:split(Ngc, [X||{_,X} <- DecL]),
		lists:split(NGet, Sel)
	end,
    E =  {[],[]},
    Reply = do_if_valid_key(Lab, Data, F, E),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

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
    lists:nth(random:uniform(length(L)), L).

do_if_valid_key(Lab, Data, F, Else) ->
    case dict:find(Lab, Data) of
	error -> Else;
	{ok, LD} -> F(LD)
    end.

get_all_or_empty(D, all) ->
    dict:fold(fun(_, V, A) -> V ++ A end, [], D);
get_all_or_empty(D, Acc) ->
    case dict:find(Acc, D) of
	error ->
	    [];
	{ok, AL} -> AL
    end.

get_size(D) ->
    dict:fold(fun(_, V, A) -> 
		      A + length(V)
	      end,
	      0, D).

-spec import_data(string()) -> dict:dict().
import_data(Dir) ->
    filelib:fold_files(Dir, "\\d+_\\d+.txt", false,
		       fun(Fn, D) ->
			       Ac = string:sub_word(string:sub_word(Fn, 3, $/), 1, $_),
			       {ok, B} = file:read_file(Fn),
			       Str = unicode:characters_to_list(B, utf8),
			       dict:append(Ac, Str, D)
		       end, 
		       dict:new()).

shuffle(L) ->
    [X||{_,X} <- lists:sort([{random:uniform(), N} || N <- L])].

valid_nget({nm, N}, Size) ->
    lists:min([N, Size]);
valid_nget({pc, P}, Size) ->
    Size * P div 100.
				 