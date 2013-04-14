-module(pe16).
-export([solve/0, sum/1, pow/1, benchmark/0]).

% 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
%
% What is the sum of the digits of the number 2^1000?
%--------------------------

%=== TRIVIAL SOLUTION
% Nothing special since we have long arithmetic in Erlang.
solve() -> sum(1000).


sum(N) -> 
	List = integer_to_list(pow(N)),
	lists:sum(List)-$0*length(List).


pow(1) -> 2;
pow(N) -> 2 * pow(N-1).


%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).