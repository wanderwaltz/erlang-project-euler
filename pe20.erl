-module(pe20).
-export([solve/0, sum_digits_fact/1, benchmark/0]).

% n! means n * (n-1) * ... * 3 * 2 * 1
%
% For example, 10! = 10 * 9 * ... * 3 * 2 * 1 = 3628800,
% and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
% 
% Find the sum of the digits in the number 100!
%--------------------------


%=== THIS KIND OF PROBLEM IS TRIVIAL IN ERLANG
solve() -> sum_digits_fact(100).

sum_digits_fact(N)->sum_digits(fact(N)).


fact(1) -> 1;
fact(N) -> N * fact(N-1).

sum_digits(0) -> 0;
sum_digits(N) -> N rem 10 + sum_digits(N div 10).

%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).