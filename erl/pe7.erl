-module(pe7).
-export([solve/0, solve_sieve/0, prime/1, benchmark/0]).

% By listing the first six prime numbers: 2, 3, 5, 7, 11, 
% and 13, we can see that the 6th prime is 13.
%
% What is the 10001st prime number?
%--------------------------


%=== BRUTE FORCE SOLUTION (more efficient solution can be found below)
% Generate each prime until we reach 10001th.
%
% Primes are stored in a list and prime testing
% involves division only by previously found primes.
solve() -> prime(10001).


prime(1) -> 2;
prime(N) -> prime_helper(N, 2, 3, [2]).


prime_helper(Stop, N, Candidate, Primes) when Stop == N ->
	Prime = not divides_primes(Candidate, Primes),
	if Prime -> Candidate;
		true -> prime_helper(Stop, N, Candidate+2, Primes)
	end;

prime_helper(Stop, N, Candidate, Primes) ->
	Prime = not divides_primes(Candidate, Primes),
	if Prime -> prime_helper(Stop, N+1, Candidate+2, [Candidate|Primes]);
		true -> prime_helper(Stop, N, Candidate+2, Primes)
	end.



divides_primes(_, []) -> false;
divides_primes(X, [H|_]) when X rem H == 0 -> true;
divides_primes(X, [_|T]) -> divides_primes(X, T).



%=== SIEVE OF ERATOSTHENES SOLUTION
% Assume we empirically found the upper bound of the 10001th prime,
% so we compute the list of primes less than 200000.

solve_sieve() -> 
    Sieve = primes:sieve(200000),
    lists:nth(10001, Sieve).


%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]),

    {S2Time, S2Result} = timer:tc(?MODULE, solve_sieve, []),
    io:format("solve_sieve: ~p (~p ms)~n", [S2Result, S2Time/1000]).