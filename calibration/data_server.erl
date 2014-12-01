-module(data_server).
-behaviour(gen_server).

-compile([export_all]).

-export([start_link/0, stop/0,
	 size/1, size/2,
	 get_labels/0, get_accounts/1,
	 get_with_complement/3
	]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DATA_DIR, "LIGA_Benelearn11_dataset").
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

get_with_complement(Lab, NGet, NComp) ->
    gen_server:call(?SERVER, {get_with_complement, Lab, NGet, NComp}).

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
		dict:fold(fun(_, V, A) -> 
				  A + length(V)
			  end,
			  0, D)
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

handle_call({get_with_complement, Lab, NGet, NComp}, _From, State=#state{data=Data}) ->
    F = fun(D) ->
		All = dict:fold(fun(_, V, A) -> V ++ A end, [], D),
		Ngc = NGet + NComp,
		{Sel,_} = lists:split(Ngc, [X||{_,X} <- lists:sort([{random:uniform(), {Lab,N}} || N <- All])]),
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

% fold_files(Dir, RegExp, Recursive, Fun, AccIn)

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

do_if_valid_key(Lab, Data, F, Else) ->
    case dict:find(Lab, Data) of
	error -> Else;
	{ok, LD} -> F(LD)
    end.

