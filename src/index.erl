%%%-----------------------------------------------------------------------------
%%% @author Sampath Singamsetty <>
%%% @copyright (C) 2017, Sampath Singamsetty
%%% @doc Indexing a file
%%% Using the template provided in  the lecture with the below pre-defined
%%% functions.
%%%      get_file_contents/1, get_all_lines/1, show_file_contents/1
%%%      Rest of the functions are defined as a part of exercise.
%%%
%%% === Problem description ===
%%% The aim of this  exercise is to index a text file,  by line number. We
%%% can think of the  input being a list of text  strings, and below we’ve
%%% provided  an outline  Erlang module  that reads  text files  into this
%%% format, as well as a couple of example files to process.
%%%
%%% The output of the main function should be a list of entries consisting
%%% of a word and a list of the ranges of lines on which it occurs.
%%%
%%% For example,  the entry {  "foo" , [{3,5},{7,7},{11,13}] }  means that
%%% the word "foo" occurs on lines 3, 4, 5, 7, 11, 12 and 13 in the file.
%%%
%%% To take the problem further, you  might like to think about these ways
%%% of refining the solution.
%%%
%%% Removing all  short words (e.g.  words of length  less than 3)  or all
%%% common words (you‘ll have to think about how to define these).
%%%
%%% Sorting the output so that the words occur in lexicographic order.
%%%
%%% Normalising the words so that  capitalised ("Foo") and non capitalised
%%% versions ("foo") of a word are identified.
%%%
%%% Normalising so that common endings, plurals etc. identified.
%%%
%%% (Harder)  Thinking how  you could  make the  data representation  more
%%% efficient than  the one you first  chose. This might be  efficient for
%%% lookup only, or for both creation and lookup.
%%%
%%% Can you think of other ways that you might extend your solution?
%%% @end
%%% Created : 12 Mar 2017 by Sampath Singamsetty <>
%%%-----------------------------------------------------------------------------
-module(index).
-include_lib("eunit/include/eunit.hrl").
-export([get_file_contents/1,show_file_contents/1]).
-export([main/1]).

%% These below exports are only for testing and can be removed
%% from the exports later
-export([get_file_contents_linum/1, get_all_words/1]).
-export([get_word_index/3, rem_dups/1, get_wl_list/1]).
-export([consolidate_keys/1 ,consolidate_values/1]).
-export([get_value_pairs/1]).
%% ------------------------------------------------------------

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

%%%-----------------------------------------------------------------------------
%%%  PROCESSING SECTION

% get the contents of each line along with the line number
% get_file_contents template function will be re-used
% Each line obtained from the file has to be split into
% individual comma delimited words and formulated into a
% word list. Since some lines have characters such as a
% , . ' " <tab> <newline> etc they have to be removed
% filter these characters "\b\t\r\n!\"',-.:;?[\\]^_`"
% their equivalent  ascii codes are as follows
% [8,9,13,10,33,34,39,44,45,46,58,59,63,91,92,93,94,95,96]

get_file_contents_linum(Name) ->
    FileContents = get_file_contents(Name),
    Size = length(FileContents),
    Filters = [8,9,13,10,33,34,39,44,45,46,58,59,63,91,92,93,94,95,96],
    Lines = [lists:filter(fun(Str) -> not(lists:member(Str, Filters)) end, FC) || FC <- FileContents],
    lists:zip(lists:seq(1, Size), Lines).

% Now tokenize each word of the line, by filtering out
% the built-in function from strings library for regex parsing
% has been used with tokenizing applied with whitespace.
% the words whose length is less than 3. This length parameter
% should be some GLOBAL variable defined, but not sure now...

get_all_words({Num, Line}) ->
    SLine = string:strip(Line, both, $\s),
    case re:split(SLine, "\s+", [{return, list}]) of
        [[]] ->
            [];
        WordList ->
            get_word_index(Num, rem_dups(WordList), [])
    end.

%
% helper function which is not exported
% this helper function takes the line number and the list of
% words on that line and returns the same after applying the
% necessary filters
% TODO: Global variable for length of word

get_word_index(_, [], Acc) ->
    Acc;
get_word_index(Num, [Word | Words], Acc) ->
    case length(Word) >= 3 of
        true ->
            NormalizedWord = string:to_lower(Word),
            get_word_index(Num, Words, [{NormalizedWord, Num} | Acc]);
        false ->
            get_word_index(Num, Words, Acc)
    end.

%
% helper function for removing the duplicates from a list

rem_dups([]) ->
    [];
rem_dups([X | Xs]) ->
    [X | rem_dups(rem_dups(X, Xs))].

rem_dups(_, []) ->
    [];
rem_dups(X, [Y | Ys]) ->
    case X =:= Y of
        true ->
            rem_dups(X, Ys);
        false ->
            [Y | rem_dups(X, Ys)]
    end.

%
% helper function for Word, Line tuple creation.
% get a list of tuples containing the Word and Line number
% with each {Word, Line} tuple set forming a list
%
% as per the erlang documentation, we can consider a list
% of tuples as a proplist, so used the same libs here for
% for parsing the key,value pair list

get_wl_list(Name) ->
    F = get_file_contents_linum(Name),
    L = lists:map(fun(X) -> get_all_words(X) end, F),
    PropList = lists:concat(L),
    Keys = proplists:get_keys(PropList),
    KVPairs = [lists:filter(fun({K, _}) -> K =:= Str end, PropList) || Str <- Keys],
    lists:sort(KVPairs).

%
% helper function for getting all the words from the
% file after proper filtering. It takes a Filename as
% parameter and returns all the words.
% Not using this now...

consolidate_keys(Name) ->
    P = [proplists:get_keys(L) || L <- get_wl_list(Name)],
    lists:concat(P).

%
% helper function for processing a {key, value} pair list
% useful for getting the list of line numbers on which the
% words are present
%
consolidate_values([]) ->
    [{}];
consolidate_values([{_K, V}]) ->
    [{V, V}];
consolidate_values([{K, V1}, {K, V2} | KV]) ->
    case V1 =:= V2 of
        true ->
            [{V1, V1} | consolidate_values([{K, V2} | KV])];
        false ->
            [{V1, V1} | consolidate_values([{K, V2} | KV])]
    end.

%
% this helper function is used for further processing the
% values list from the above function, as the above function
% only lists all line numbers and does not provide any range
% for the consecutive lines.
% here we process the value pairs to check which line numbers
% are consecutive and accordingly represent as range

get_value_pairs([]) ->
    [{}];
get_value_pairs([V]) ->
    [V];
get_value_pairs([{Va1, Va2}, {Vb1, Vb2} | V]) ->
    %% check if difference of current pair second element and next
    %% pair first element is 1, is so they are consecutive
    case Vb1 - Va2 =:= 1 of
        true ->
            get_value_pairs([{Va1, Vb2} | V]);
        false ->
            [{Va1, Va2} | get_value_pairs([{Vb1, Vb2} | V])]
    end.

%%%-----------------------------------------------------------------------------
% This is out main function for processing the File and
% generating the required range.
%%%-----------------------------------------------------------------------------
main(Name) ->
    I = get_wl_list(Name),
    [{lists:concat(proplists:get_keys(X)), get_value_pairs(consolidate_values(X))} || X <- I].


%%% ----------------------------------------------------------------------------
%%% TODO: Write EUnit Test cases
%%% ----------------------------------------------------------------------------


%% (distel@localhost)12> index:main("../src/gettysburg-address.txt").
%% [{"above",[{16,16}]},
%%  {"add",[{16,16}]},
%%  {"advanced",[{20,20}]},
%%  {"ago",[{1,1}]},
%%  {"all",[{3,3}]},
%%  {"altogether",[{10,10}]},
%%  {"and",[{1,1},{3,3},{6,6},{10,10},{15,15},{27,27}]},
%%  {"any",[{6,6}]},
%%  {"are",[{3,3},{5,5},{7,7}]},
%%  {"battlefield",[{7,7}]},
%%  {"before",[{22,22}]},
%%  {"birth",[{26,26}]},
%%  {"brave",[{15,15}]},
%%  {"brought",[{1,1}]},
%%  {"but",[{13,13},{18,18}]},
%%  {"can",[{7,7},{13,14},{18,18}]},
%%  {"cause",[{23,23}]},
%%  {"civil",[{5,5}]},
%%  {"come",[{8,8}]},
%%  {"conceived",[{2,2},{6,6}]},
%%  {"consecrate",[{14,14}]},
%%  {"consecrated",[{16,16}]},
%%  {"continent",[{2,2}]},
%%  {"created",[{3,3}]},
%%  {"dead",[{15,...},{...}|...]},
%%  {"dedicate",[{...}|...]},
%%  {"dedicated",[...]},
%%  {[...],...},
%%  {...}|...]
