%%%-----------------------------------------------------------------------------
%%% @author Sampath Singamsetty <>
%%% @copyright (C) 2017, Sampath Singamsetty
%%% @doc week2_lists
%%% https://www.futurelearn.com/courses/functional-programming-erlang/1/steps/155225
%%% Constructing lists with recursive functions
%%% @end
%%% Created : 04 Mar 2017 by Sampath Singamsetty <>
%%%-----------------------------------------------------------------------------
-module(week2_lists).
-include_lib("eunit/include/eunit.hrl").
-export([double/1, evens/1, median/1, mode/1]).


%% Transforming list elements
%% Define an Erlang function double/1 to double the elements of a list of numbers.
-spec double(ListX) -> ListY when
      ListX :: [integer()],
      ListY :: [integer()].
double([]) ->
    [];
double([X|Xs]) ->
    [2*X | double(Xs)].

%% Filtering lists
%% Define a function evens/1 that extracts the even numbers from a list of integers.
%% we will use a helper function filter for filtering out all the even numbers
-spec evens(ListX) -> ListY when
      ListX :: [T],
      ListY :: [T],
      T :: integer().
evens([]) ->
    [];
evens(L) ->
    filter(fun(X) -> X rem 2 =:= 0 end, L).

%% helper function definition
-spec filter(Predicate, ListX) -> ListY when
      Predicate :: fun((Elem :: T) -> boolean()),
      ListX :: [T],
      ListY :: [T],
      T :: term().
filter(_, []) ->
    [];
filter(Predicate, [Y|Ys]) ->
    case Predicate(Y) of
        true ->
            [Y | filter(Predicate, Ys)];
        false ->
            filter(Predicate, Ys)
        end.

%% the median of a  list of numbers: this is the  middle element when the
%% list is ordered (if the list is  of even length you should average the
%% middle two)
%% strategy - get the length of the list first
%% if length is even, get the (n+1)th element of the list
%% else get average of nth and (n+1)th elements of the list
median([]) ->
    0;
median([X]) ->
    X;
median(L) ->
    N = length(L) div 2,
    case length(L) rem 2 =:= 0 of
        true ->
            avg(lists:nth(N, L), lists:nth(N+1, L));
        false ->
            lists:nth(N+1, L)
    end.

%% helper function for calculating average of 2 numbers
avg(A, B) ->
    (A + B) / 2.

%% Mode
%% the modes  of a  list of  numbers: this  is a  list consisting  of the
%% numbers that  occur most frequently in  the list; if there  is is just
%% one, this will be a list with one element only
mode([]) ->
    void;
mode(L) ->
    % We will sort the list and take only the unique elements first
    Sort = lists:sort(L),
    % used functions from sets module to get unique elements
    U = sets:to_list(sets:from_list(L)),
    % get a key-value pair of list of elements, with the
    % key being count of how many times element appears
    % and value being the element itself
    KV = [get_count(X, Sort) || X <- U],
    % filter the key-val pairs for all keys exceeding 1
    % in other words, we are picking all element appearing more that once
    Fkv = filter(fun({K, _}) -> K > 1 end, KV),
    % check if the filtered list has any repeated elements from list
    case Fkv =:= [] of
        true ->
            void;
        false ->
            MaxKey = lists:max(proplists:get_keys(Fkv)),
            ResList = filter(fun({Y, _}) -> Y =:= MaxKey end, Fkv),
            {_, Res} = lists:unzip(ResList),
            Res
    end.

%% helper function for getting the count of number of occurrences
%% of an element in a list
get_count(X, []) ->
    {0, X};
get_count(X, [H|T]) ->
    count({0, X}, [H|T]).

count({Acc, H}, [H|T]) ->
    count({Acc+1, H}, T);
count({Acc, H}, [_|T]) ->
    count({Acc, H}, T);
count({Acc, H}, []) ->
    {Acc, H}.


%%-----------------------------------------------------------------------------
%% eunit test cases for the above functions
%%-----------------------------------------------------------------------------
double_test_() ->
    [?_assertEqual([2,4,6,10,12], double([1,2,3,5,6])),
     ?_assert(double([]) =:= []),
     ?_assertException(error, function_clause, double(20))
    ].

evens_test_() ->
    [?_assertEqual([2,4,6,8,10], evens(lists:seq(1,10))),
     ?_assertException(error, function_clause, evens(500)),
     ?_assert([] =:= evens([]))
    ].

median_test_() ->
    [?_assertEqual(33, median([1,2,21,22,33,34,44,45,99])),
     ?_assertEqual(27.5, median([1,2,21,22,33,34,44,45])),
     ?_assert(median([]) =:= 0),
     ?_assertException(error, badarg, median(-2))
    ].

mode_test_() ->
    [?_assertEqual([38,23], mode([10,2,38,23,38,23,21])),
     ?_assertEqual(void, mode([1,2,3,4,5,8,9,99,1000])),
     ?_assertEqual([3,2,1], mode([1,2,3,3,2,1])),
     ?_assert(mode(lists:seq(1,10)) =:= void),
     ?_assertException(error, function_clause, mode(123))
    ].

%% ======================== EUnit ========================
%% module 'week2_lists'
%%   week2_lists:131: mode_test_...ok
%%   week2_lists:132: mode_test_...ok
%%   week2_lists:133: mode_test_...ok
%%   week2_lists:134: mode_test_...ok
%%   week2_lists:112: double_test_...ok
%%   week2_lists:113: double_test_...ok
%%   week2_lists:114: double_test_...ok
%%   week2_lists:118: evens_test_...ok
%%   week2_lists:119: evens_test_...ok
%%   week2_lists:120: evens_test_...ok
%%   week2_lists:124: median_test_...ok
%%   week2_lists:125: median_test_...ok
%%   week2_lists:126: median_test_...ok
%%   week2_lists:127: median_test_...ok
%%   [done in 0.042 s]
%% =======================================================
%%   All 14 tests passed.
%% ok
