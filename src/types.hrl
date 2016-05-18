%% nb use "ligaModel" not liga_model in case of clashes with
%% other uses of "liga_model"

-type ligaModel() :: #{labels => list(label()),
		       node_weights => nonint(), 
		       edge_weights => nonint(), 
		       nodes => labmap(), 
		       edges => labmap()}.

-type config() :: {nonint(), n|e, string()}.
-type label() :: any().
-type labelled_string() :: {label(), string()}.
-type label_score() :: {label(), score()}.
-type liga_score()  :: list(label_score()). % sorted descending by score
-type medge() :: {mnode(), mnode()}.
-type mnode() :: trigram().
-type nonint() :: nonint().
-type region() :: {emap|nmap, nonint()}.
-type score() :: float().
-type trigram() :: {nonint(), nonint(), nonint()}.
-type tuplist() :: list(tuple()).

-type intmap() :: map().  %% {term() -> non_neg_integer()}
-type labmap() :: map().  %% {term() -> intmap()}

