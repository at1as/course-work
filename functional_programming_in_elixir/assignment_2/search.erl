-module(search).
-export([find_on_lines/2, list_to_tuple/1]).


%% search:find_on_lines("Hello", [["Bye", "Hi", "Hello"], ["Hello", "Hi"], ["Z", "Y"], ["Hello"]]).
%%    =>    [1, 2, 4]


find_on_lines(Word, [Line|Lines]) ->
  find_on_lines(Word, [Line|Lines], [], 1).

find_on_lines(_Word, [], Hits, _Index) ->
  Hits;

find_on_lines(Word, [Line|Lines], Hits, Index) ->
  case helpers:any(Word, Line) of
    true ->
      find_on_lines(Word, Lines, Hits ++ [Index], Index + 1);
    false ->
      find_on_lines(Word, Lines, Hits, Index + 1)
  end.
