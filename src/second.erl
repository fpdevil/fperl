-module(second).
-export([hypotenuse/2, perimeter/2, area/2]).

hypotenuse(A, B) ->
    math:sqrt(first:square(A) + first:square(B)).

perimeter(A, B) ->
    C = hypotenuse(A, B),
    A + B + C.

area(A, B) ->
    0.5 * first:mult(A, B).
