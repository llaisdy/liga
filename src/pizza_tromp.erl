-module(pizza_tromp).
-export([data_dir/1, pizzas/0, slice_label/1, header_tsv/0]).

data_dir(test) ->
    "LIGA_test_dataset/pizzas/";
data_dir(full) ->
    "LIGA_Benelearn11_dataset/pizzas/".

pizzas() ->
    [{"de_DE", ten_slices()}
    ,{"en_UK", ten_slices()}
    ,{"es_ES", ten_slices()}
    ,{"fr_FR", ten_slices()}
    ,{"it_IT", ten_slices()}
    ,{"nl_NL", ten_slices()}
    ].

slice_label(N) ->
    lists:nth(N, ten_slices()).

header_tsv() ->
    ["Score"
    ,[["\t", P] || {P,_} <- pizzas()]
    ,"\n"
    ].

%%%% private

ten_slices() ->
    ["aa","ab","ac","ad","ae","af","ag","ah","ai","aj"].


