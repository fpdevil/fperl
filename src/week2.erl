%%%-----------------------------------------------------------------------------
%%% @author Sampath Singamsetty <>
%%% @copyright (C) 2017, Sampath Singamsetty
%%% @doc Week2 exercises
%%%      https://www.futurelearn.com/courses/functional-programming-erlang/1/steps/155134
%%% @end
%%% Created : 04 Mar 2017 by Sampath Singamsetty <>
%%%-----------------------------------------------------------------------------
-module(week2).
-include_lib("eunit/include/eunit.hrl").
-export([product/1, tail_product/1, maximum/1, tail_maximum/1]).


%%%-----------------------------------------------------------------------------
%% Combining list elements: the product of a list
%% Base case  is when  the list is  empty. The product  of an  empty list
%% should be  1, otherwise  when recursive  call comes  to the  base case
%% after condensing  at each iteration will  end up by multiplied  with 0
%% giving 0 as the final result.
%% === solution using direct or body recursion
-spec product(List) -> number() when
      List :: [number()].
product([])     -> 1;
product([X|Xs]) -> X * product(Xs).

%% === solution using tail recursion
-spec tail_product(List) -> number() when
      List :: [number()].
tail_product(X) ->
    tail_product(X, 1).

tail_product([], P) ->
    P;
tail_product([X | Xs], P) ->
    tail_product(Xs, X*P).

%%%-----------------------------------------------------------------------------
%% Combining list elements: the maximum of a list
%% Define an Erlang function to give the maximum of a list of numbers.
%% since maximum of empty list is not clear, used an error message

%% === solution using direct or body recursion
%% using the inbuilt max/2 function

-spec maximum(List) -> Max when
      List :: [T,...],
      Max :: T,
      T :: term().
maximum([]) ->
    error_logger:error_msg("maximum of empty list~n");
maximum([X]) ->
    X;
maximum([X|Xs]) ->
    max(X, maximum(Xs)).


%% === solution using Tail recursion

-spec tail_maximum(List) -> Acc when
      List :: [T,...],
      Acc :: T,
      T :: term().
tail_maximum([]) ->
    error_logger:error_msg("maximum of empty list~n");
tail_maximum([X|Xs]) ->
    tail_maximum(Xs, X).

tail_maximum([X|Xs], Acc) when X > Acc ->
    tail_maximum(Xs, X);
tail_maximum([_|Xs], Acc) ->
    tail_maximum(Xs, Acc);
tail_maximum([], Acc) ->
    Acc.

%%%-----------------------------------------------------------------------------
%%% eunit test cases
%%%-----------------------------------------------------------------------------
product_test_() ->
    [?_assertEqual(1, product([])),
     ?_assertEqual(24, product([1,2,3,4])),
     ?_assert(product([1,2,3]) =:= 6),
     ?_assertError(function_clause, product(123))
    ].

tail_product_test_() ->
    [?_assertEqual(6, tail_product([1,2,3])),
     ?_assert(tail_product([1,2,3,4]) =:= 24),
     ?_assertEqual(120, tail_product([1,2,3,4,5])),
     ?_assertException(error, function_clause, tail_product(100))
    ].

combine_product_test_() ->
    [?_assert(product(lists:seq(1, X)) =:= tail_product(lists:seq(1, X))) || X <- lists:seq(1, 10)].

maximum_test_() ->
    [?_assert(maximum([5,1,2,71,9,10,0]) =:= 71),
     ?_assertEqual(12, maximum(lists:seq(6,12)))
    ].

tail_maximum_test_() ->
    [?_assertEqual(55, tail_maximum([11,13,6,44,55,22,10])),
     ?_assertException(error, function_clause, tail_maximum(1, 2))
    ].

%% Test case run
%% ======================== EUnit ========================
%% directory "ebin"
%%   module 'week1'
%%   module 'week2'
%%     week2:73: product_test_...ok
%%     week2:74: product_test_...ok
%%     week2:75: product_test_...ok
%%     week2:76: product_test_...ok
%%     week2:80: tail_product_test_...ok
%%     week2:81: tail_product_test_...ok
%%     week2:82: tail_product_test_...ok
%%     week2:83: tail_product_test_...ok
%%     week2:87: combine_product_test_...ok
%%     week2:87: combine_product_test_...ok
%%     week2:87: combine_product_test_...ok
%%     week2:87: combine_product_test_...ok
%%     week2:87: combine_product_test_...ok
%%     week2:87: combine_product_test_...ok
%%     week2:87: combine_product_test_...ok
%%     week2:87: combine_product_test_...ok
%%     week2:87: combine_product_test_...ok
%%     week2:87: combine_product_test_...ok
%%     week2:90: maximum_test_...ok
%%     week2:91: maximum_test_...ok
%%     week2:95: tail_maximum_test_...ok
%%     week2:96: tail_maximum_test_...ok
%%     [done in 0.066 s]
