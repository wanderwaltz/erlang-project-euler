-module(pe2).
-export([solve_1/0, solve_2/0, benchmark/0]).

% Each new term in the Fibonacci sequence is generated 
% by adding the previous two terms. 
% By starting with 1 and 2, the first 10 terms will be:

% 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

% By considering the terms in the Fibonacci sequence 
% whose values do not exceed four million, find 
% the sum of the even-valued terms.
%--------------------------

%=== SOLUTION 1
solve_1() -> sum_1(0,2,1,4000000).


%sum(Acc, Fib, Prev, Bound)
%
% This function does multiple things at once for the sake of effectiveness
%
% Acc argument accumulates the sum and is returned when the recursion breaks.
%
% Fib and Prev arguments denote the current Fibonacci sequence number and
% the previous one correspondingly.
%
% Bound argument provides means to break recursion - function matches the 1st
% clause only while Fib <= Bound.
%
% The (1-Fib rem 2) bit is needed to sum only even Fibonacci sequence elements.
% We cannot skip odd ones though in order to calculate the sequence in a proper
% way, so this cannot go into guards (though we can actually do that by adding
% one more clause - see solution 2)
sum_1(Acc, Fib, Prev, Bound) when Fib =< Bound -> sum_1(Acc + Fib * (1-Fib rem 2), Fib+Prev, Fib, Bound);
sum_1(Acc, _, _, _) -> Acc.



%=== SOLUTION 2
solve_2() -> sum_2(0,2,1,4000000).

% The same as sum_1, but without division at the cost of more clauses
%
% At this point I do not know which of these solutions is more effective
% from computational point of view.
sum_2(Acc, Fib, _, Bound)    when Fib > Bound -> Acc;
sum_2(Acc, Fib, Prev, Bound) when Fib rem 2 == 0 -> sum_2(Acc + Fib, Fib+Prev, Fib, Bound);
sum_2(Acc, Fib, Prev, Bound)                     -> sum_2(Acc      , Fib+Prev, Fib, Bound).



%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve_1, []),
    io:format("solve_1: ~p (~p ms)~n", [S1Result, S1Time/1000]),

    {S2Time, S2Result} = timer:tc(?MODULE, solve_2, []),
    io:format("solve_2: ~p (~p ms)~n", [S2Result, S2Time/1000]).