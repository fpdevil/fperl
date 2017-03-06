%%%-----------------------------------------------------------------------------
%%% @author Sampath Singamsetty <>
%%% @copyright (C) 2017, Sampath Singamsetty
%%% @doc
%%%
%%% @end
%%% Created : 05 Mar 2017 by Sampath Singamsetty <>
%%%-----------------------------------------------------------------------------
-module(nub).
-include_lib("eunit/include/eunit.hrl").
-export([nub/1, nubs/1]).

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

%% tests
nub_test_() ->
    [?_assertEqual([2,4,1,3], nub([2,4,1,3,3,1])),
     ?_assertEqual([2,4,3,1], nub([2,4,3,3,1,1])),
     ?_assertException(error, function_clause, nub(1))
    ].
