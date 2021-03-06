%%%-----------------------------------------------------------------------------
%%% @author Sampath Singamsetty <>
%%% @copyright (C) 2017, Sampath Singamsetty
%%% @doc
%%% Consolidation: functions over lists
%%% @end
%%% Created : 07 Mar 2017 by Sampath Singamsetty <>
%%%-----------------------------------------------------------------------------
-module(week2_ext_lists).
-include_lib("eunit/include/eunit.hrl").
-export([join/2, concat/1, member/2]).
-export([insertsort/1, quicksort/1, mergesort/1, selectsort/1]).
-export([perms/1]).

%% Joining lists together
join([], Ys) ->
    Ys;
join([X|Xs], Ys) ->
    [X | join(Xs, Ys)].

%% concatenating list of lists together
%% using the join function defined above...
concat([]) ->
    [];
concat([X]) ->
    X;
concat([X|Xs]) ->
    join(X, concat(Xs)).

%% Testing membership
member(_, []) ->
    false;
member(X, [Y | Ys]) ->
    case X =:= Y of
        true ->
            true;
        false ->
            false or member(X, Ys)
    end.


%% sorting of the lists
%% quick sort
%% consider an element as a Pivot and partition all
%% the elements lesser and greater than the Pivot and
%% append on either sides of the Pivot. After a few
%% iterations, the list will be sorted.
quicksort([]) ->
    [];
quicksort([X | Xs]) ->
    Small = [Y || Y <- Xs, Y =< X],
    Large = [Y || Y <- Xs, Y > X],
    quicksort(Small) ++ [X | quicksort(Large)].

%% merge sort
%% split a list into parts and sort each individual list
%% merge all the lists together.
mergesort([]) ->
    [];
mergesort([X]) ->
    [X];
mergesort(Xs) ->
    {Ys, Zs} = split(Xs),
    merge(mergesort(Ys), mergesort(Zs)).

%% helper function for splitting the list
split([]) ->
    {[], []};
split([A]) ->
    {[A], []};
split([A, B | Cs]) ->
    {As, Bs} = split(Cs),
    {[A | As], [B | Bs]}.

%% helper function for merging 2 lists
merge(X, []) ->
    X;
merge([], Y) ->
    Y;
merge([X | Xs], [Y | Ys]) ->
    case X =< Y of
        true ->
            [X | merge(Xs, [Y | Ys])];
        false ->
            [Y | merge([X | Xs], Ys)]
    end.


%% insertion sort
%% take each element from the list and insert the item into
%% appropriate position into a new sorted list. It needs
%% shifting of the following elements over by one.
insertsort([]) ->
    [];
insertsort([X|Xs]) ->
    insert(X, insertsort(Xs)).

%% helper function for inserting an element into a list
%% at appropriate position based on value.
insert(X, []) ->
    [X];
insert(X, [Y|Ys]) ->
    case X =< Y of
        true ->
            [X | [Y | Ys]];
        false ->
            [Y | insert(X, Ys)]
        end.

%% selection sort
%% It is in-place comparison. It finds the minimum value of list
%% swaps it with the value in first place and repeats the steps
%% for the rest of the list.
selectsort([]) ->
    [];
selectsort([X]) ->
    [X];
selectsort(Xs) ->
    L = least(Xs),
    [L | selectsort(del(L, Xs))].

%% helper function for picking the least element of list
least([]) ->
    void;
least([Z]) ->
    Z;
least([Z | Zs]) ->
    min(Z, least(Zs)).

%% helper function for deleting an element from list
del(_, []) ->
    [];
del(W, [Z|Zs]) ->
    case W =:= Z of
        true ->
            Zs;
        false ->
            [Z | del(W, Zs)]
    end.

%% === permutations
%% we will reuse the delete function defined above
perms([]) ->
    [[]];
perms(Xs) ->
    [[Y | Ys] || Y <- Xs, Ys <- perms(del(Y, Xs))].



%%-----------------------------------------------------------------------------
%% eunit test cases for the above functions
%%-----------------------------------------------------------------------------
join_test_() ->
    [?_assertEqual("hello", join("hel", "lo")),
     ?_assertEqual([1,2,3,4], join([1,2], [3, 4])),
     ?_assertException(error, function_clause, join(1, 2))
    ].

concat_test_() ->
    [?_assertEqual("goodbye", concat(["goo","d","","by","e"])),
     ?_assertEqual([1,2,3,4], concat([[1,2],[3,4]])),
     ?_assertException(error, function_clause, concat(123))
    ].

member_test_() ->
    [?_assertEqual(true, member(3, [1,2,3,4,5])),
     ?_assertEqual(true, member(2,[2,0,0,1])),
     ?_assertEqual(false, member(20,[2,0,0,1])),
     ?_assertException(error, function_clause, member(1, 2))
    ].

quicksort_test_() ->
    [?_assertEqual([1,2,2,3,3,4,4,5,6,7,8,9,10], quicksort([10, 2, 5, 3, 1, 6, 7, 4, 2, 3, 4, 8, 9])),
     ?_assertEqual("        abcdeeefghhijklmnoooopqrrsttuuvwxyz", quicksort("the quick brown fox jumps over the lazy dog")),
     ?_assertException(error, function_clause, quicksort(100))
    ].

insertsort_test_() ->
    [?_assertEqual([1,2,2,3,3,4,4,5,6,7,8,9,10], insertsort([10, 2, 5, 3, 1, 6, 7, 4, 2, 3, 4, 8, 9])),
     ?_assertEqual("        abcdeeefghhijklmnoooopqrrsttuuvwxyz", insertsort("the quick brown fox jumps over the lazy dog")),
     ?_assertException(error, function_clause, insertsort(100))
    ].

mergesort_test_() ->
    [?_assertEqual([1,2,2,3,3,4,4,5,6,7,8,9,10], mergesort([10, 2, 5, 3, 1, 6, 7, 4, 2, 3, 4, 8, 9])),
     ?_assertEqual("        abcdeeefghhijklmnoooopqrrsttuuvwxyz", mergesort("the quick brown fox jumps over the lazy dog")),
     ?_assertException(error, function_clause, mergesort(100))
    ].

selectsort_test_() ->
    [?_assertEqual([1,2,2,3,3,4,4,5,6,7,8,9,10], selectsort([10, 2, 5, 3, 1, 6, 7, 4, 2, 3, 4, 8, 9])),
     ?_assertEqual("        abcdeeefghhijklmnoooopqrrsttuuvwxyz", selectsort("the quick brown fox jumps over the lazy dog")),
     ?_assertException(error, function_clause, selectsort(100))
    ].

perms_test_() ->
    [?_assertEqual([[]], perms([])),
     ?_assertEqual([[1]], perms([1])),
     ?_assertEqual(["abc","acb","bac","bca","cab","cba"], perms("abc")),
     ?_assertEqual([[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]], perms([1,2,3])),
     ?_assertException(error, function_clause, perms(100))
].


%% (distel@localhost)54> eunit:test(week2_ext_lists,[verbose]).
%% ======================== EUnit ========================
%% module 'week2_ext_lists'
%%   week2_ext_lists:166: member_test_...ok
%%   week2_ext_lists:167: member_test_...ok
%%   week2_ext_lists:168: member_test_...ok
%%   week2_ext_lists:169: member_test_...ok
%%   week2_ext_lists:197: perms_test_...ok
%%   week2_ext_lists:198: perms_test_...ok
%%   week2_ext_lists:199: perms_test_...ok
%%   week2_ext_lists:200: perms_test_...ok
%%   week2_ext_lists:201: perms_test_...ok
%%   week2_ext_lists:173: quicksort_test_...ok
%%   week2_ext_lists:174: quicksort_test_...ok
%%   week2_ext_lists:175: quicksort_test_...ok
%%   week2_ext_lists:179: insertsort_test_...ok
%%   week2_ext_lists:180: insertsort_test_...ok
%%   week2_ext_lists:181: insertsort_test_...ok
%%   week2_ext_lists:185: mergesort_test_...ok
%%   week2_ext_lists:186: mergesort_test_...ok
%%   week2_ext_lists:187: mergesort_test_...ok
%%   week2_ext_lists:191: selectsort_test_...ok
%%   week2_ext_lists:192: selectsort_test_...ok
%%   week2_ext_lists:193: selectsort_test_...ok
%%   week2_ext_lists:154: join_test_...ok
%%   week2_ext_lists:155: join_test_...ok
%%   week2_ext_lists:156: join_test_...ok
%%   week2_ext_lists:160: concat_test_...ok
%%   week2_ext_lists:161: concat_test_...ok
%%   week2_ext_lists:162: concat_test_...ok
%%   [done in 0.081 s]
%% =======================================================
%%   All 27 tests passed.
%% ok
