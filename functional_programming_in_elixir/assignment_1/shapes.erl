-module(shapes).
-export([perimeter/1, area/1, enclose/1, startTests/0]).

% For Functional Programming in Erlang
% Kent University
% 21 February 2017


%%
%% Triangle Inequality Theorem defines valid Triangles
%%

validTriangle({A, B, C}) when A + B > C, B + C > A, A + C > B ->
  true;
validTriangle({_A, _B, _C}) ->
  false.


%%
%% Perimeter Formula for Square, Rectangle, Circle and Triangle
%%

perimeter({square, X}) when is_integer(X) or is_float(X) ->
  {ok, X * 4};

perimeter({rectangle, X, Y}) when (is_integer(X) or is_float(X)) and (is_integer(Y) or is_float(Y)) ->
  {ok, X * 2 + Y * 2};

perimeter({circle, R}) when is_integer(R) or is_float(R) ->
  {ok, 2 * math:pi() * R};

perimeter({triangle, A, B, C}) ->
  case validTriangle({A, B, C}) of
    true ->
      {ok, A + B + C};
    false ->
      {error, -1}
  end.



%%
%% Area Formula for Square, Rectangle, Circle and Triangle
%%

area({square, X}) when is_integer(X) or is_float(X) ->
  {ok, X * X};

area({rectangle, X, Y}) when (is_integer(X) or is_float(X)) and (is_integer(Y) or is_float(Y)) ->
  {ok, X * Y};

area({circle, R}) when is_integer(R) or is_float(R) ->
  {ok, math:pi() * R * R};

area({triangle, A, B, C}) ->        %% validTriangle/3 will guard against non number inputs
  case validTriangle({A, B, C}) of
    true ->                         %% See Heron's Formula. This is valid for all triangle types
      S = (A+B+C)/2,
      {ok, math:sqrt(S*(S-A)*(S-B)*(S-C))};
    false ->
      {error, -1}
    end.



%%
%% Enclosing Box Formula for Square, Rectangle, Circle and Triangle
%%

enclose({square, X}) when is_integer(X) or is_float(X) ->
  {ok, {0, 0}, {0, X}, {X, 0}, {X, X}};

enclose({rectangle, X, Y}) ->
  {ok, {0, 0}, {0, X}, {Y, 0}, {Y,X}};

enclose({circle, R}) when is_integer(R) or is_float(R) ->
  {ok, {0, 0}, {0, 2 * R}, {2 * R, 0}, {2 * R, 2 * R}};

enclose({triangle, A, B, C}) ->         %% validTriangle/3 will guard against non number inputs
  case validTriangle({A, B, C}) of
    true ->
      S = (A+B+C)/2,                    %% Heron's formula also yields triangle height given dimensions
      H = (2/C) * math:sqrt(S*(S-A)*(S-B)*(S-C)),
      {ok, {0, 0}, {0, H}, {C, 0}, {C, H}};
    false ->
      {error, -1}
  end.


%%
%% TESTS
%%

printTest(TESTNAME, EXPECTED, OUTPUT) ->
  {ok, R} = OUTPUT,
  io:fwrite("~s should be \t\t ~w -> ~w\n", [TESTNAME, EXPECTED, R]).

printTest(TESTNAME, EXPECTED, OUTPUT, _TYPE) ->
  {ok, A, B, C, D} = OUTPUT,
  io:fwrite("~s should be \t\t ~w -> ~w,~w,~w,~w\n", [TESTNAME, EXPECTED, A, B, C, D]).


startTests() ->

  %% Perimeter Tests

  printTest("Perimeter : Square : Integer ", 8, perimeter({square, 2})),
  printTest("Perimeter : Square : Float", 16.0, perimeter({square, 4.0})),
  printTest("Perimeter : Rectangle : Integer ", 14, perimeter({rectangle, 3, 4})),
  printTest("Perimeter : Rectangle : Float", 20.0, perimeter({rectangle, 9.0, 1.0})),
  printTest("Perimeter : Circle : Integer ", 2 * math:pi()*10, perimeter({circle, 10})),
  printTest("Perimeter : Circle : Float", 2 * math:pi()*20.0, perimeter({circle, 20.0})),
  printTest("Perimeter : Triangle : Integer ", 31, perimeter({triangle, 15, 9, 7})),
  printTest("Perimeter : Triangle : Float ", 31.0, perimeter({triangle, 15.0, 9.0, 7.0})),
  io:fwrite("\n"),

  %% Area Test
  
  printTest("Area : Square : Integer ", 4, area({square, 2})),
  printTest("Area : Square : Float", 25.0, area({square, 5.0})),
  printTest("Area : Rectangle : Integer ", 12, area({rectangle, 3, 4})),
  printTest("Area : Rectangle : Float", 9.0, area({rectangle, 9.0, 1.0})),
  printTest("Area : Circle : Integer ", math:pi()*10*10, area({circle, 10})),
  printTest("Area : Circle : Float", math:pi()*20.0*20.0, area({circle, 20.0})),
  printTest("Area : Triangle : Integer ", 20.69269194667528, area({triangle, 15, 9, 7})),
  printTest("Area : Triangle : Float ", 20.69269194667528, area({triangle, 15.0, 9.0, 7.0})),
  io:fwrite("\n"),

  %% Enclosing Square Test

  printTest("Enclosing Square : Square : Integer ", '{0,0},{0,2},{2,0},{2,2}', enclose({square, 2}), "Enclosing"),
  printTest("Enclosing Square : Square : Float", '{0,0},{0,5.0},{5.0,0},{5.0,5.0}', enclose({square, 5.0}), "Enclosing"),
  printTest("Enclosing Square : Rectangle : Integer ", '{0,0},{0,3},{4,0},{4,3}', enclose({rectangle, 3, 4}), "Enclosing"),
  printTest("Enclosing Square : Rectangle : Float", '{0,0},{0,9.0},{1.0,0},{1.0,9.0}', enclose({rectangle, 9.0, 1.0}), "Enclosing"),
  printTest("Enclosing Square : Circle : Integer ", '{0,0},{0,20},{20,0},{20,20}', enclose({circle, 10}), "Enclosing"),
  printTest("Enclosing Square : Circle : Float", '{0,0},{0,40.0},{40.0,0},{40.0,40.0}', enclose({circle, 20.0}), "Enclosing"),
  printTest("Enclosing Square : Triangle : Integer ", '{0,0},{0,5.912197699050079},{7,0},{7,5.912197699050079}', enclose({triangle, 15, 9, 7}), "Enclosing"),
  printTest("Enclosing Square : Triangle : Float ", '{0,0},{0,5.912197699050079},{7,0},{7,5.912197699050079}', enclose({triangle, 15.0, 9.0, 7.0}), "Enclosing").



%% OUTPUT FROM TESTS

%% 197> shapes:startTests().                                    
%% Perimeter : Square : Integer  should be          8 -> 8
%% Perimeter : Square : Float should be             16.0 -> 16.0
%% Perimeter : Rectangle : Integer  should be       14 -> 14
%% Perimeter : Rectangle : Float should be          20.0 -> 20.0
%% Perimeter : Circle : Integer  should be          62.83185307179586 -> 62.83185307179586
%% Perimeter : Circle : Float should be             125.66370614359172 -> 125.66370614359172
%% Perimeter : Triangle : Integer  should be        15 -> 31
%% Perimeter : Triangle : Float  should be          15.0 -> 31.0

%% Area : Square : Integer  should be               4 -> 4
%% Area : Square : Float should be                  25.0 -> 25.0
%% Area : Rectangle : Integer  should be            12 -> 12
%% Area : Rectangle : Float should be               9.0 -> 9.0
%% Area : Circle : Integer  should be               314.1592653589793 -> 314.1592653589793
%% Area : Circle : Float should be                  1256.6370614359173 -> 1256.6370614359173
%% Area : Triangle : Integer  should be             20.69269194667528 -> 20.69269194667528
%% Area : Triangle : Float  should be               20.69269194667528 -> 20.69269194667528

%% Enclosing Square : Square : Integer  should be           '{0,0},{0,2},{2,0},{2,2}' -> {0,0},{0,2},{2,0},{2,2}
%% Enclosing Square : Square : Float should be              '{0,0},{0,5.0},{5.0,0},{5.0,5.0}' -> {0,0},{0,5.0},{5.0,0},{5.0,5.0}
%% Enclosing Square : Rectangle : Integer  should be        '{0,0},{0,3},{4,0},{4,3}' -> {0,0},{0,3},{4,0},{4,3}
%% Enclosing Square : Rectangle : Float should be           '{0,0},{0,9.0},{1.0,0},{1.0,9.0}' -> {0,0},{0,9.0},{1.0,0},{1.0,9.0}
%% Enclosing Square : Circle : Integer  should be           '{0,0},{0,20},{20,0},{20,20}' -> {0,0},{0,20},{20,0},{20,20}
%% Enclosing Square : Circle : Float should be              '{0,0},{0,40.0},{40.0,0},{40.0,40.0}' -> {0,0},{0,40.0},{40.0,0},{40.0,40.0}
%% Enclosing Square : Triangle : Integer  should be         '{0,0},{0,5.912197699050079},{7,0},{7,5.912197699050079}' -> {0,0},{0,5.912197699050079},{7,0},{7,5.912197699050079}
%% Enclosing Square : Triangle : Float  should be           '{0,0},{0,5.912197699050079},{7,0},{7,5.912197699050079}' -> {0,0},{0,5.912197699050079},{7.0,0},{7.0,5.912197699050079}

