-module(pe25).
-export([solve_brute/0, benchmark/0, solve_asymptotic/0]).

% The Fibonacci sequence is defined by the recurrence relation:
% 
% F_n = F_n-1 + F_n-2, where F_1 = 1 and F_2 = 1.
%
% Hence the first 12 terms will be:
%
% F_1  = 1
% F_2  = 1
% F_3  = 2
% F_4  = 3
% F_5  = 5
% F_6  = 8
% F_7  = 13
% F_8  = 21
% F_9  = 34
% F_10 = 55
% F_11 = 89
% F_12 = 144
% The 12th term, F12, is the first term to contain three digits.
%
% What is the first term in the Fibonacci sequence to contain 1000 digits?
%--------------------------


%=== BRUTE FORCE SOLUTION
% Use the builtin long arithmetic and generate the fibonacci sequence
% until we reach 1000 digits.

solve_brute() -> first(1000).


first(N) -> first(1, 1, 3, N).

first(Fn1, Fn, I, N) ->
    F = Fn + Fn1,
    L = integer_to_list(F),
    if length(L) >= N -> I;
        true -> first(Fn, F, I+1, N)
    end.


%=== ASYMPTOTIC SOLUTION
% See Wikipedia for Fibonacci numbers. There is an asymptotic
% equation for the number of digits in Fn.
% 
% This solution does give the right answer, but implementation
% is kinda fishy because I've already known the answer by then,
% and was playing with formulae so I'd receive the right answer.
%
% Though I've tested this solution for numbers 1001 and 2000 and
% it gave the same result as the brute force one, so I guess it
% is indeed a proper solution.

solve_asymptotic() -> first_asymp(1000).


first_asymp(N) -> first_asymp(1, N).

first_asymp(I, N) ->
    Fi = (1+math:sqrt(5))/2,
    L  = math:log10(Fi),
    V  = trunc(I*L - math:log10(5)/2)+1,

    if V >= N -> I;
        true -> first_asymp(I+1, N)
    end.
    

%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve_brute, []),
    io:format("solve_brute: ~p (~p ms)~n", [S1Result, S1Time/1000]),

    {S2Time, S2Result} = timer:tc(?MODULE, solve_asymptotic, []),
    io:format("solve_asymptotic: ~p (~p ms)~n", [S2Result, S2Time/1000]).  