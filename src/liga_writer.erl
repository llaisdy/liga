-module(liga_writer).
-export([write_model/2, write_model/3]).
-export([read_model/1, read_model/2]).

-spec write_model(map(), module()) -> ok.
write_model(Model, ModuleName) ->
    write_model(Model, ModuleName, ".").

-spec write_model(map(), module(), string()) -> ok.
write_model(Model, ModuleName, Directory) ->
    file:write_file(make_fn(ModuleName, Directory),
		    make_module(Model, ModuleName)).

-spec read_model(string()) -> ok | {error, tuple()}.
read_model(Filename) ->
    read_model(erl, Filename).

-spec read_model(erl | beam, string()) -> ok | {error, tuple()}.
read_model(erl, Filename) ->
    try
	{ok, M} = compile:file(Filename),
	code:add_path("."),
	read_model(beam, M),
	ok
    catch
    	E:R ->
    	    {error, {E,R}}
    end;
read_model(beam, M) ->
    try
	false = code:purge(M),
	{module, M} = code:load_file(M),
	ok
    catch
	E:R ->
	    {error, {E,R}}
    end.


%%%% private

-spec make_fn(module(), string()) -> string().
make_fn(ModuleName, Directory) ->
    Directory ++ "/" ++ atom_to_list(ModuleName) ++ ".erl".

-spec make_module(map(), module()) -> string().
make_module(Model, ModuleName) ->
    lists:flatten([make_header(ModuleName),
		   make_model_fun(Model)]).

-spec make_header(atom()) -> string().
make_header(ModName) ->
    lists:concat(
      ["-module(", atom_to_list(ModName), ").\n",
       "-export([version/0, model/0]).\n\n",
       "version() -> \"", stringify_calendar_local_time(), "\".\n\n"
      ]).

-spec make_model_fun(map()) -> string().
make_model_fun(Model) ->
    io_lib:fwrite("model() -> ~p.\n\n", [Model]).

stringify_calendar_local_time() ->
    {{Y,Mo,D},{H,Mi,_}} = calendar:local_time(),
    io_lib:format("~p-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B", 
		  [Y, Mo, D, H, Mi]).
