-module(liga_writer).
-export([write/2, write/3]).

-spec write(map(), module()) -> ok.
write(Model, ModuleName) ->
    write(Model, ModuleName, ".").

-spec write(map(), module(), string()) -> ok.
write(Model, ModuleName, Directory) ->
    file:write_file(make_fn(ModuleName, Directory),
		    make_module(Model, ModuleName)).

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

-spec make_model_fun(map(), string()).
make_model_fun(Model) ->
    io_lib:fwrite("model() -> ~p.\n\n", [Model]).

stringify_calendar_local_time() ->
    {{Y,Mo,D},{H,Mi,_}} = calendar:local_time(),
    io_lib:format("gpm version: ~p-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B",
		  [Y, Mo, D, H, Mi]),
