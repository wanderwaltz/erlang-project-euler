-module(pe6).
-export([solve/0, benchmark/0]).

% The sum of the squares of the first ten natural numbers is,
%
% 12 + 22 + ... + 102 = 385
% The square of the sum of the first ten natural numbers is,
%
% (1 + 2 + ... + 10)2 = 552 = 3025
% Hence the difference between the sum of the squares of the 
% first ten natural numbers and the square of the sum is 3025  385 = 2640.
%
% Find the difference between the sum of the squares of 
% the first one hundred natural numbers and the square of the sum.
%--------------------------


%=== STRAIGHTFORWARD SOLUTION
% Don't think there is a need to implement something fancy in this problem

solve() -> square_sum(100, 0) - sum_squares(100).


sum_squares(0) -> 0;
sum_squares(X) -> X*X + sum_squares(X-1).

square_sum(0, Sum) -> Sum * Sum;
square_sum(X, Sum) -> square_sum(X-1, Sum + X).


%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).