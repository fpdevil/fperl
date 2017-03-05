%%%-----------------------------------------------------------------------------
%%% @doc Week1 Assignment
%%%
%%% @end
%%% Created : 24 Feb 2017 by Sampath Singamsetty <>
%%%-----------------------------------------------------------------------------
-module(week1_assignment).
-include_lib("eunit/include/eunit.hrl").
-export([perimeter/1, area/1, bits/1, binary_list/1]).
-export([tailbits/1]).


% Define a function perimeter/1 which takes a shape and returns the
% perimeter of the shape.
% C = {X, Y} is the origin or center, which being not used has been
% marked with an _ to nullify the warnings
perimeter({circle, _C, R}) ->
    2 * math:pi() * R;
perimeter({rectangle, _C, H, W}) ->
    2 * (W + H);
perimeter({triangle, _C, A, B, C}) ->
    A + B + C.

% areas of figures
area({circle, _C, R}) ->
    math:pi() * R * R;
area({rectangle, _C, H, W}) ->
    H * W;
area({triangle, _C, A, B, C}) ->
    S = perimeter({triangle, A, B, C}) / 2,
    math:sqrt(S * (S - A) * (S - B) * (S - C)).

%% Non Tail recursive function for bits
bits(0) ->
    0;
bits(N) ->
    lists:sum(binary_list(N)).

%% helper function to be used by the bits function above
%% it uses repeated Division and Remainder By base 2
binary_list(0) ->
    [];
binary_list(X) ->
    [(X rem 2) | binary_list(X div 2)].

%% Tail recursive version of the bits function.
%% In this case the recursive calls eat more memory due
%% to repeated function calls, but the tailrecursive
%% function runs in a constant space.
tailbits(0) ->
    0;
tailbits(N) ->
    lists:sum(bits_list(N, [])).

%% auxilliary tail recursive function using an Accumulator
%% Acc. It starts with an empty list and grows with each value
%% prepended to it after each subsequent call.
%% It runs in a constant space and the result of the list
%% comes in the reverse order.
bits_list(0, Acc) ->
    Acc;
bits_list(N, Acc) ->
    bits_list(N div 2, [N rem 2 | Acc]).

%% EUnit test cases

perimeter_test() ->
    ?assertEqual(18.84955592153876, perimeter({circle, {0, 0}, 3})),
    ?assertEqual(24, perimeter({rectangle, {0, 0}, 7, 5})),
    ?assertEqual(12, perimeter({triangle, {0, 0}, 3, 4, 5})),
    ?assertError(function_clause, perimeter({rhombus, {0, 0}, 2, 4, 4})).

area_test() ->
    [?_assert(area({circle, {0, 0}, 5}) =:= 78.53981633974483),
     ?_assert(area({rectangle, {0, 0}, 3, 4}) =:= 12),
     ?_assert(area({triangle, {0, 0}, 4, 5}) =:= 10.0),
     ?_assert(area({circle, {0, 0}, 3}) =:= 28.274333882308138)
    ].

bits_test() ->
    [?_assert(bits(21) =:= 3),
     ?_assert(bits(8) =:= 1),
     ?_assert(bits(7) =:= 3),
     ?_assertError(function_clause, bits(-5))].

tailbits_test() ->
    [?_assert(tailbits(33) =:= 2),
     ?_assert(tailbits(54) =:= 4),
     ?_assert(tailbits(66) =:= 2),
     ?_assert(tailbits(0) =:= 0),
     ?_assertError(function_clause, tailbits(-9))].

%% > eunit:test(week1_assignment, [verbose]).
%% ======================== EUnit ========================
%% module 'week1_assignment'
%%   week1_assignment: tailbits_test...ok
%%   week1_assignment: bits_test...ok
%%   week1_assignment: area_test...ok
%%   week1_assignment: perimeter_test...ok
%%   [done in 0.012 s]
%% =======================================================
%%   All 4 tests passed.
%% ok
