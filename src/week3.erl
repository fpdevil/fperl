%%%-----------------------------------------------------------------------------
%%% @author Sampath Singamsetty <>
%%% @copyright (C) 2017, Sampath Singamsetty
%%% @doc
%%% https://www.futurelearn.com/courses/functional-programming-erlang/1/steps/161066
%%% Using higher-order functions
%%% @end
%%% Created : 19 Mar 2017 by Sampath Singamsetty <>
%%%-----------------------------------------------------------------------------
-module(week3).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%% double all the elements of a list
doubleAll(L) ->
    lists:map(fun(X) -> 2*X end, L).

%% filter out the even numbers
evens(L) ->
    lists:filter(fun(X) -> X rem 2 =:= 0 end, L).

%% product of a list of integers
product(L) ->
    lists:foldl(fun(A, B) -> A*B end, 1, L).

%% Zipping
%% a) Define a function zip/2 that “zips together” pairs of
%%    elements from two lists
zip([A | As], [B | Bs]) ->
    [{A, B} | zip(As, Bs)];
zip(_, []) ->
    [];
zip([], _) ->
    [].

%% b) Define a function zip_with/3 that “zips together” pairs of
%%    elements from two lists using the function in the first argument
zip_with(F, [A | As], [B | Bs]) ->
    [F(A, B) | zip_with(F, As, Bs)];
zip_with(_, [], []) ->
    [].

%% c) Re-define the function zip_with/3 using zip and lists:map
zip_with_redefined(F, L1, L2) ->
    lists:map(F, zip(L1, L2)).

%% d) Re-define zip/2 using zip_with/3
zip_redefined(L1, L2) ->
    zip_with(fun(A, B) -> [A, B | []] end, L1, L2).

%% EUnit Test cases
doubleAll_test_() ->
    [?_assertEqual([2,4,6], doubleAll([1,2,3])),
     ?_assertEqual([], doubleAll([])),
     ?_assertException(error, function_clause, doubleAll(0))
    ].

evens_test_() ->
    [?_assertEqual([], evens([])),
     ?_assertEqual([30,32,34,36,38,40], evens(lists:seq(30,40))),
     ?_assertException(error, function_clause, evens(100))
    ].

product_test_() ->
    [?_assertEqual(92279715720192000, product(lists:seq(30,40))),
     ?_assertEqual(1, product([])),
     ?_assertException(error, function_clause, product(1))
    ].

zip_test_() ->
    [?_assertEqual([{1,2}, {3,4}], zip([1,3,5,7], [2,4])),
     ?_assertEqual([{97,100},{98,101},{99,102}], zip("abc", "def")),
     ?_assertException(error, function_clause, zip(1, 2))
    ].

zip_with_test() ->
    [?_assertEqual([3,7], zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4])),
     ?_assertEqual([[1,4],[2,5],[3,6]], zip_with(fun(A,B) -> [A,B|[]] end, [1,2,3], [4,5,6])),
     ?_assertException(error, function_clause, zip_with(fun(X,Y) -> X div Y end, 1, 2))
    ].
