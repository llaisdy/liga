%% nb use "ligaModel" not liga_model in case of clashes with
%% other uses of "liga_model"

-type nonegint() :: non_neg_integer().
-type ligaModel() :: #{labels => list(label()),
		       node_weights => nonegint(),
		       edge_weights => nonegint(),
		       nodes => labmap(), 
		       edges => labmap()}.

-type config() :: {nonegint(), n|e, string()}.
-type label() :: any().
-type labelled_string() :: {label(), string()}.
-type label_score() :: {label(), score()}.
-type liga_score()  :: list(label_score()). % sorted descending by score
-type medge() :: {mnode(), mnode()}.
-type mnode() :: trigram().
-type region() :: {emap|nmap, nonegint()}.
-type score() :: float().
-type trigram() :: {nonegint(), nonegint(), nonegint()}.
-type tuplist() :: list(tuple()).

-type intmap() :: map().  %% {term() -> nonegint()}
-type labmap() :: map().  %% {term() -> intmap()}

