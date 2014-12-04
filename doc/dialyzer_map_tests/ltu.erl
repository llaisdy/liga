-module(ltu).
-export([get_map/0]).


-spec get_map() -> map().
get_map() ->
    #{labels => [one, two], 
      number => 27,
      [1,2,3] => wer,        %% ok
      {4,5,6} => sdf,        %% ok
      kvok => #{
	a => qwe,
	2 => asd,
	[1,2,3] => wer,  %% bad
	{4,5,6} => sdf,  %% bad
	"abc" => zxc 
       }
     }.
