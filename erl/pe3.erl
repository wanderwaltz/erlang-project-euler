-module(pe3).
-export([solve_1/0, benchmark/0]).


%The prime factors of 13195 are 5, 7, 13 and 29.
%What is the largest prime factor of the number 600851475143 ?
%--------------------------


%=== SOLUTION 1
% Pretty much straightforward brute-force solution,
% we divide out value by all odd natural numbers starting
% with 3 until we can. Eventually we reduce the value
% to 1. Our maximum divider is the answer.
solve_1() -> largest(600851475143, 3, 2).


defactor(Val, Factor) when Val rem Factor > 0 -> Val;
defactor(Val, Factor) -> defactor(Val div Factor, Factor).

largest(Val, Factor, Largest) when Factor > Val -> Largest;
largest(Val, Factor, _) -> largest(defactor(Val, Factor), Factor+2, Factor).


%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve_1, []),
    io:format("solve_1: ~p (~p ms)~n", [S1Result, S1Time/1000]).