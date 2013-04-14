-module(pe9).
-export([solve/0, benchmark/0]).

% A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
% 
% a^2 + b^2 = c^2
% For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
% 
% There exists exactly one Pythagorean triplet for which a + b + c = 1000.
% Find the product abc.
%--------------------------


%=== STRAIGHTFORWARD SOLUTION
% Just a nested loop enumerating all pithagorean triplets
solve() -> iterate_a(500).


iterate_a(0) -> not_found;
iterate_a(A) ->
	Result = iterate_b(A,500),
	if Result /= not_found -> Result;
		true -> iterate_a(A-1)
	end.


iterate_b(_, 0) -> not_found;
iterate_b(A, B) ->
	C = math:sqrt(A*A+B*B),
	if trunc(C) == C, A+B+C == 1000 -> {A, B, trunc(C), A*B*trunc(C)};
		true -> iterate_b(A,B-1)
	end.


%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).