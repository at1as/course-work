-module(helpers).
-export([flatten/1, filter/2, unique/1, flatten_strings/1, any/2, list_to_tuple/1]).


%% helpers:flatten([[1,[2,[5,7]]], [3,4]]).     => [1,2,5,7,3,4]

flatten([]) ->
  [];
flatten([X|Xs]) when is_list(X) ->
  flatten(X) ++ flatten(Xs);
flatten([X|Xs]) ->
  [X] ++ flatten(Xs).


%% helpers:flatten(["hello"], ["world"]]).     => ["hello", "world"]

flatten_strings([Y|Ys]) ->
  lists:foldr(fun(X, ACC) -> X ++ ACC end, [], [Y|Ys]).


%% helpers:filter(9, [1,2,3,4,5,6,7,8,9,0,1,2,9,9,4,56]).     => [1,2,3,4,5,6,7,8,0,1,2,4,56]

filter(_X, []) ->
  [];
filter(X, [X|Xs]) ->
  filter(X, Xs);
filter(X, [Y|Ys]) ->
  [Y | filter(X, Ys)].


%% helpers:unique([1,2,3,4,1,2,3,4,5,3,2,4,1]).     => [1,2,3,4,5]

unique([]) ->
  [];
unique([X|Xs]) ->
  [X | unique(filter(X, Xs))].


%% helpers:any(3, [1,2,3,4])    => true

any(_X, []) ->
  false;
any(X, [Y|Ys]) ->
  case X of
    Y -> true;
    _ -> any(X, Ys)
  end.


%% helpers:list_to_tuple([1,2,3,6,7,9,14,15]).
%%    => [{1,3},{6,7},{9,9},{14,15}]

list_to_tuple([X|Xs]) ->
  list_to_tuple(Xs, X, X).

list_to_tuple([], Last, Begin) ->
  [{Begin, Last}];

list_to_tuple([X|Xs], Last, Begin) ->
  case Last + 1 of
    X ->
      list_to_tuple(Xs, X, Begin);
    _ ->
      [{Begin, Last} | list_to_tuple(Xs, X, X)]
  end.
