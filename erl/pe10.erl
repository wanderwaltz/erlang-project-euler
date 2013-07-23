-module(pe10).
-export([solve/0, benchmark/0]).

% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
%
% Find the sum of all the primes below two million.
%--------------------------
% Answer: 142913828922

solve() -> sum_sieve(2000000).

% This function is just a shortcut to count the sum of primes returned by primes:sieve/1
sum_sieve(N) -> lists:sum(primes:sieve(N)).

%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).