-module(shuffle).

-export([list/1]).

list([])     -> [];
list([Elem]) -> [Elem];
list(List)   -> list(List, length(List), []).

list([], 0, Result) ->
    Result;
list(List, Len, Result) ->
    {Elem, Rest} = nth_rest(random:uniform(Len), List),
    list(Rest, Len - 1, [Elem|Result]).

nth_rest(N, List) -> nth_rest(N, List, []).

nth_rest(1, [E|List], Prefix) -> {E, Prefix ++ List};
nth_rest(N, [E|List], Prefix) -> nth_rest(N - 1, List, [E|Prefix]).