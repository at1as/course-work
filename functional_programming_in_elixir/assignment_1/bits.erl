-module(bits).
-export([bits/1, bitsNoTailRecursion/1, startTests/0]).

%% Functional Programming in Erlang
%% Kent University
%% 21 February 2017
%% Exercise 1 : Summing the Bits


%%
%% Convert String of Binary representation to List of Strings
%%    (String) -> List(String)
%%   
%%    Ex. "1101" -> ["1", "1", "0", "1"]
%%
%%    Limitations : "string:substr(S, 2, 10000)" is very lazy. It assumes no input over 10000 characters
%%                  This conversion is quite convoluted. Surely there is a better way to do this
%%

stringToList(S) ->
  stringToList(S, []).
stringToList("", X) ->
  X;
stringToList(S, []) ->
  stringToList(string:substr(S, 2, 10000), [string:substr(S, 1, 1)]);
stringToList(S, Xs) ->
  stringToList(string:substr(S, 2, 10000), Xs ++ [string:substr(S, 1, 1)]).


%%
%% Sum the number of Binary 1's for a given base 10 integer
%%    (Integer) -> Integer
%%      This Method with Tail Recursion
%%
%%    Ex. 7 -> 3    (0111)
%%

bits(X) ->
  sumOfBits(stringToList(integer_to_list(X, 2))).


sumOfBits([X|Xs]) ->
  sumOfBits([X|Xs], 0).

sumOfBits([], S) ->
  S;
sumOfBits([X|Xs], S) ->
  case X == "1" of
    true ->
      sumOfBits(Xs, S + 1);
    false ->
      sumOfBits(Xs, S)
  end.



%%
%% Sum the number of Binary 1's for a given base 10 integer
%% (Integer) -> Integer
%%   Solution without Tail Recursion
%%
%%  Ex. 3 -> 2    (011)
%%

bitsNoTailRecursion(X) ->
  sumOfBitsR(stringToList(integer_to_list(X, 2))).

sumOfBitsR([]) ->
  0;
sumOfBitsR([X|Xs]) ->
  case X == "1" of
    true ->
      1 + sumOfBitsR(Xs);
    false ->
      sumOfBitsR(Xs)
  end.




%%
%% Tests
%%

startTests() ->

  %% Functions using tail recursion

  io:fwrite("Should be 3 (input 7)  : ~w\n", [bits:bits(7)]),
  io:fwrite("Should be 1 (input 1)  : ~w\n", [bits:bits(1)]),
  io:fwrite("Should be 2 (input 3)  : ~w\n", [bits:bits(3)]),
  io:fwrite("Should be 1 (input 8)  : ~w\n", [bits:bits(8)]),
  io:fwrite("Should be 1 (input 64) : ~w\n", [bits:bits(64)]),

  %% Functions without tail recursion
  
  io:fwrite("Should be 3 (input 7)  : ~w\n", [bits:bitsNoTailRecursion(7)]),
  io:fwrite("Should be 1 (input 1)  : ~w\n", [bits:bitsNoTailRecursion(1)]),
  io:fwrite("Should be 2 (input 3)  : ~w\n", [bits:bitsNoTailRecursion(3)]),
  io:fwrite("Should be 1 (input 8)  : ~w\n", [bits:bitsNoTailRecursion(8)]),
  io:fwrite("Should be 1 (input 64) : ~w\n", [bits:bitsNoTailRecursion(64)]).


%%
%% OUPUT of startTests()
%%

%% 211> bits:startTests()
%% 211> .
%% Should be 3 (input 7)  : 3
%% Should be 1 (input 1)  : 1
%% Should be 2 (input 3)  : 2
%% Should be 1 (input 8)  : 1
%% Should be 1 (input 64) : 1
%% Should be 3 (input 7)  : 3
%% Should be 1 (input 1)  : 1
%% Should be 2 (input 3)  : 2
%% Should be 1 (input 8)  : 1
%% Should be 1 (input 64) : 1

