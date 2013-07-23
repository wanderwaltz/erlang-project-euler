-module(pe27).
-export([solve/0, benchmark/0, num_seq_primes/4]).

% Euler discovered the remarkable quadratic formula:
%
%     n^2 + n + 41
%
% It turns out that the formula will produce 40 primes for the 
% consecutive values n = 0 to 39. However, when n = 40, 
% 40^2 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly 
% when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.
%
% The incredible formula  n^2 - 79n + 1601 was discovered, which 
% produces 80 primes for the consecutive values n = 0 to 79. 
% The product of the coefficients, 79 and 1601, is 126479.
%
% Considering quadratics of the form:
%
% n^2 + an + b, where |a| < 1000 and |b| < 1000
%
% where |n| is the modulus/absolute value of n
% e.g. |11| = 11 and |-4| = 4
% Find the product of the coefficients, a and b, for the quadratic 
% expression that produces the maximum number of primes for 
% consecutive values of n, starting with n = 0.
%--------------------------
% Answer: -59231 (a = -61, b = 971, 71 consecutive primes)
%
% Notes:
% When n == 0, our quadratic is equal to b, so we should check only
% prime b values. Not sure about the negative ones, are negative numbers
% even considered as being prime or not?
%
% After already solving the problem in a less efficient way, I know
% that b is 971 and is positive, so let's optimize the whole thing and
% iterate only through positive prime b values.
%
% Also we know that when n == b we get b^2 + ab + b, which is divisable
% by b, so there would be b consecutive primes maximum. We should start
% at b = 43 since we know that (a,b) == (1,41) gives us 40 numbers.

solve() -> 
	Sieve = primes:sieve(empiric_upper_Sieve()),
	{N, A, B} = iterate_quadratics(lists:seq(-1000,1000), Sieve),
	{product, A * B, primes, N, a, A, b, B}.

iterate_quadratics(AList, Sieve) ->
	find_maximum({0, 0, 0}, [{num_seq_primes(Sieve, 0, A, B), A, B} || A <- AList, B <- Sieve, B =< 1000]).

find_maximum(Result, []) -> Result;
find_maximum({N, _, _}, [{M, A1, B1} | List]) when N < M -> find_maximum({M,A1,B1}, List);
find_maximum({N, A, B}, [_ | List]) -> find_maximum({N, A, B}, List).


empiric_upper_Sieve() -> 1000000.

num_seq_primes(Sieve, N, A, B) -> 
	Prime = primes:is_prime(Sieve, N * N + A * N + B),
	if Prime == true -> 1 + num_seq_primes(Sieve, N+1, A, B);
		true -> 0
	end.




%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).