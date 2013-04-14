-module(pe1).
-export([solve_oneline/0, solve_simple/0, benchmark/0]).

% If we list all the natural numbers below 10 that are multiples of 
% 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
% Find the sum of all the multiples of 3 or 5 below 1000.
%--------------------------

%=== ONE LINE SOLUTION
% Came to this solution while studying the Erlang book
% Don't think this is actually a better solution, since
% the memory management is not very effective - we retain
% the whole list in memory to count its sum.
% But this has some beauty in it.
% Probaby the same kind of beauty as C ternary operators have
% You know, 'never use this in production code' kind of beauty.
solve_oneline() -> lists:sum([X || X <- lists:seq(3,999), (X rem 3 == 0) or (X rem 5 == 0)]).


%=== SIMPLE SOLUTION
% The sum function does count the desired sum in a 
% pretty much straightforward way, but includes 
% the N value itself.
% So we subtract 1000 from the result when computing the solution.
solve_simple() -> sum(1000)-1000.

sum(2) -> 0;
sum(N) when N rem 3 == 0 -> N+sum(N-1);
sum(N) when N rem 5 == 0 -> N+sum(N-1);
sum(N) -> sum(N-1).



%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve_oneline, []),
    io:format("solve_oneline: ~p (~p ms)~n", [S1Result, S1Time/1000]),

    {S2Time, S2Result} = timer:tc(?MODULE, solve_simple, []),
    io:format("solve_simple: ~p (~p ms)~n", [S2Result, S2Time/1000]).