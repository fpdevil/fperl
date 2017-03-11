%%%-----------------------------------------------------------------------------
%%% @author Sampath Singamsetty <>
%%% @copyright (C) 2017, Sampath Singamsetty
%%% @doc Check if a string or sentence is a palindrome.
%%% Define a function palindrome, that returns true or false depending on
%%% whether the list is a palindrome - the same when read right to left
%%% as when read left to right.
%%% @end
%%% Created : 06 Mar 2017 by Sampath Singamsetty <>
%%%-----------------------------------------------------------------------------
-module(palindrome).
-include_lib("eunit/include/eunit.hrl").
-export([palindrome/1]).

%% palindrome of string or sentence
%% for the string or the sentence, we will first strip all the below characters
%% "\t !\"',.:;?[\\]^_`" which is the ascii list [9,32,33,34,39,44,46,58,59,63,91,92,93,94,95,96]
palindrome([]) ->
    true;
palindrome([_]) ->
    true;
palindrome(X) ->
    %% filtering out the unneeded characters from string
    C = lists:filter(fun(Str) -> not(lists:member(Str,[9,32,33,34,39,44,46,58,59,63,91,92,93,94,95,96])) end, X),
    %% convert the filtered string to all lower case
    palindrome_str(string:to_lower(C)).

%% helper function which does the actual palindrome check
palindrome_str([X|Xs]) ->
    Rest = trimmed(Xs),
    (X =:= hd(lists:reverse(Xs))) and palindrome(Rest).

%% helper function for getting the string between first and last values
trimmed(Xs) ->
    lists:reverse(tl(lists:reverse(Xs))).

palindrome_test_() ->
    [?_assertEqual(true, palindrome("Madam I\'m Adam")),
     ?_assertEqual(true, palindrome("A man, a plan, a canal, Panama!")),
     ?_assertEqual(true, palindrome("abc 1 cba")),
     ?_assertEqual(true, palindrome("Salisbury moor, sir, is roomy. Rub Silas.")),
     ?_assertEqual(true, palindrome("No, it is opposed; Art sees Trade's opposition.")),
     ?_assertException(error, function_clause, palindrome(123))
    ].

%% 1> eunit:test(palindrome,[verbose]).
%% ======================== EUnit ========================
%% module 'palindrome'
%%   palindrome:38: palindrome_test_...ok
%%   palindrome:39: palindrome_test_...ok
%%   palindrome:40: palindrome_test_...ok
%%   palindrome:41: palindrome_test_...ok
%%   palindrome:42: palindrome_test_...ok
%%   palindrome:43: palindrome_test_...ok
%%   [done in 0.018 s]
%% =======================================================
%%   All 6 tests passed.
%% ok
