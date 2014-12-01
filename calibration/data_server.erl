-module(data_server).
-behaviour(gen_server).

-export([start_link/0,
	 size/1, size/2,
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

-record(state, {data}).

%%%% API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

size(Lab) ->
    gen_server:call(?SERVER, {size, Lab}).

size(Lab, Acc) ->
    gen_server:call(?SERVER, {size, Lab, Acc}).

get_with_complement(Lab, N1, N2) ->
    gen_server:call(?SERVER, {get_with_complement, Lab, N1, N2}).

%%%% gen_server callbacks

init([]) ->
    Data = lists:foldl(fun({Lab, Dir}, Dict) ->
			       LabDir = ?DATA_DIR ++ "/" ++ Dir,
			       import_data(Dict, LabDir)
		       end,
		       dict:new(),
		       ?LAB_DIRS),
    {ok, #state{data=Data}}.

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

% fold_files(Dir, RegExp, Recursive, Fun, AccIn)

-spec import_data(dict(), string()) -> dict().
import_data(Dict, Dir) ->
    filelib:fold_files(Dir, "\\d+_\\d+.txt", false,
		       fun(Fn, D) ->
			       Ac = string:sub_word(Fn, 1, $_),
			       todo, % read file, bin to list (utf8)
			       dict:do(D, Ac, 
		       end, 
		       Dict).
