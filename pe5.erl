-module(pe5).
-export([solve/0, benchmark/0]).

% 2520 is the smallest number that can be divided by each 
% of the numbers from 1 to 10 without any remainder.
%
% What is the smallest positive number that is evenly 
% divisible by all of the numbers from 1 to 20?
%--------------------------

%=== STRAIGHTFORWARD SOLUTION
% Counting the least common multiple of X and Y as X*Y/gcd(X,Y)
% Iterating over the given numbers to compute the result.
solve() -> smallest(20,1).


gcd(A,0) -> A;
gcd(A,B) -> gcd(B, A rem B).


smallest(1, N) -> N;
smallest(X, N) -> smallest(X-1, N * X div gcd(X,N)).

%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).