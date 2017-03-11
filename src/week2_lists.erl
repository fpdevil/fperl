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
-export([evens_new/1]).
%% additional work..
-export([take/2, nub/1]).
-export([nubs/1]).
-export([palindrome/1]).


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

%% evens without filter
evens_new(L) ->
    [X || X <- L, X rem 2 =:= 0].

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
%% Define a function take that takes the first N elements from a list
-spec take(N, ListX) -> ListY when
      N :: pos_integer(),
      ListX :: [T],
      ListY :: [T],
      T :: term().
take(0, _) ->
    [];
take(_, []) ->
    [];
take(N, L) ->
    lists:reverse(take(N, L, [])).

take(0, _, Acc) ->
    Acc;
take(_, [], Acc) ->
    Acc;
take(N, [X|Xs], Acc) ->
    take(N-1, Xs, [X|Acc]).


%% The 'nub' function
%% Define a  function nub  to remove  all the  duplicate elements  from a
%% list.  This  could  remove  all repeated  elements  except  the  first
%% instance, or all repeated elements except the final instance.
-spec nub(ListX) -> ListY when
      ListX :: [T],
      ListY :: [T],
      T :: term().
nub([]) ->
    [];
nub([X | Xs]) ->
    [X | nub(nub(X, Xs))].

nub(_, []) ->
    [];
nub(X, [Y | Ys]) ->
    case X =:= Y of
        true ->
            nub(X, Ys);
        false ->
            [Y | nub(X, Ys)]
    end.

%% another take at nub using sets module
nubs([]) ->
    [];
nubs(L) ->
    sets:to_list(sets:from_list(L)).

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

take_test_() ->
    [?_assertEqual([], take(0,"hello")),
     ?_assertEqual("hell", take(4,"hello")),
     ?_assertEqual("hello", take(5,"hello")),
     ?_assertEqual("hello", take(9,"hello")),
     ?_assertException(error, function_clause, take(1, 2))
    ].

nub_test_() ->
    [?_assertEqual([2,4,1,3], nub([2,4,1,3,3,1])),
     ?_assertEqual([2,4,3,1], nub([2,4,3,3,1,1])),
     ?_assertException(error, function_clause, nub(1))
    ].

palindrome_test_() ->
    [?_assertEqual(true, palindrome("Madam I\'m Adam")),
     ?_assertEqual(true, palindrome("A man, a plan, a canal, Panama!")),
     ?_assertEqual(true, palindrome("abc 1 cba")),
     ?_assertEqual(true, palindrome("Salisbury moor, sir, is roomy. Rub Silas.")),
     ?_assertEqual(true, palindrome("No, it is opposed; Art sees Trade's opposition.")),
     ?_assertException(error, function_clause, palindrome(123))
    ].

%% 1> eunit:test(week2_lists,[verbose]).
%% ======================== EUnit ========================
%% module 'week2_lists'
%%   week2_lists:206: double_test_...ok
%%   week2_lists:207: double_test_...ok
%%   week2_lists:208: double_test_...ok
%%   week2_lists:212: evens_test_...ok
%%   week2_lists:213: evens_test_...ok
%%   week2_lists:214: evens_test_...ok
%%   week2_lists:218: median_test_...ok
%%   week2_lists:219: median_test_...ok
%%   week2_lists:220: median_test_...ok
%%   week2_lists:221: median_test_...ok
%%   week2_lists:225: mode_test_...ok
%%   week2_lists:226: mode_test_...ok
%%   week2_lists:227: mode_test_...ok
%%   week2_lists:228: mode_test_...ok
%%   week2_lists:229: mode_test_...ok
%%   week2_lists:233: take_test_...ok
%%   week2_lists:234: take_test_...ok
%%   week2_lists:235: take_test_...ok
%%   week2_lists:236: take_test_...ok
%%   week2_lists:237: take_test_...ok
%%   week2_lists:241: nub_test_...ok
%%   week2_lists:242: nub_test_...ok
%%   week2_lists:243: nub_test_...ok
%%   week2_lists:247: palindrome_test_...ok
%%   week2_lists:248: palindrome_test_...ok
%%   week2_lists:249: palindrome_test_...ok
%%   week2_lists:250: palindrome_test_...ok
%%   week2_lists:251: palindrome_test_...ok
%%   [done in 0.084 s]
%% =======================================================
%%   All 28 tests passed.
%% ok
