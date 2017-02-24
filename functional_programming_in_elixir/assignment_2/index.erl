-module(index).
-export([get_file_contents/1,show_file_contents/1,individual_words/1, make_word_list/1, runner/0]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.


individual_words(File) ->
    lists:map(fun (X) -> string:tokens(X, " ") end, File).





%% INPUT IS -> index:get_file_contents("dickens-christmas.txt")

make_word_list(File) ->
  X1 = individual_words(File),
  X2 = helpers:flatten_strings(X1),
  helpers:unique(X2).

find_occurance_of_words([], _Lines) ->
  [];
find_occurance_of_words([Word|Words], Lines) ->
  [ { Word, helpers:list_to_tuple(search:find_on_lines(Word, Lines)) } | find_occurance_of_words(Words, Lines)].




runner() ->
  Text  = get_file_contents("dickens-christmas.txt"),
  Words = make_word_list(Text),
  Lines = individual_words(index:get_file_contents("dickens-christmas.txt")),
  find_occurance_of_words(Words, Lines).
