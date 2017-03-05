%%%-----------------------------------------------------------------------------
%%% @author Sampath Singamsetty <>
%%% @copyright (C) 2017, Sampath Singamsetty
%%% @doc Week1 exercises
%%% www.futurelearn.com/courses/functional-programming-erlang
%%% @end
%%% Created : 23 Feb 2017 by Sampath Singamsetty <>
%%%-----------------------------------------------------------------------------
-module(week1).
-export([fib/1, pieces/1]).
-export([fibonacci/1, perfect/1]).
-export([fib_tail/1, perfect_tail/1]).

%% fibonacci numbers
%% helper functions for fast Fibonacci
fibStep({X, Y}) ->
    {Y, X + Y}.

fibPair(0) ->
    {0, 1};
fibPair(N) when N > 0 ->
    fibStep(fibPair(N-1)).

%% get first value of a pair
getFirst({X, _}) ->
    X.

%% actual Fibonacci number
fib(0) ->
    0;
fib(N) when N > 0 ->
    getFirst(fibPair(N)).

%% tail recursive Fibonacci
fibonacci(N) ->
    tail_fib(N, 0, 0, 0).

tail_fib(Acc, Acc, X, Y) -> X + Y;
tail_fib(Acc, 0, _, _) -> tail_fib(Acc, 1, 0, 0);
tail_fib(Acc, 1, _, _) -> tail_fib(Acc, 2, 1, 0);
tail_fib(Acc, N, X, Y) -> tail_fib(Acc, N+1, Y + X, X).

fib_tail(N) ->
    fib_tail(N, 0, 1).

fib_tail(0, P, _) ->
    P;
fib_tail(N, P, Acc) ->
    fib_tail(N-1, Acc, P+Acc).

%% N cuts problem
%% given N lines, return the number of parts plane is divided into.
%% lines    pieces
%% f(0)         1
%% f(1)         2
%% f(2)         4
%% ..           ..
%% f(n-2)       (n-2) + f(n-3)
%% f(n-1)       (n-1) + f(n-2)
%% f(n)            n  + f(n-1)
%% ---------------------------
%% add on both left and right side, cancel common terms
%% This gives the below general formula
%% f(n) = SUM (n) + 1
%% f(n) = (2 + n*n + n)/2
%%
pieces(0) ->
    1;
pieces(N) when N > 0 ->
    (2 + N*N + N) div 2.

%% perfect numbers
perfect(N) ->
    L = lists:filter(fun(X) -> N rem X =:= 0 end, lists:seq(1, N-1)),
    lists:sum(L) =:= N.

perfect_tail(N) ->
    perfect_tail(N, 1, 0).

perfect_tail(N, N, Acc) ->
    N == Acc;
perfect_tail(N, M, Acc) when N rem M == 0 ->
    perfect_tail(N, M+1, Acc+M);
perfect_tail(N, M, Acc) ->
    perfect_tail(N, M+1, Acc).
