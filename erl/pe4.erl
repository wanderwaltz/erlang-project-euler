-module(pe4).
-export([solve_brute/0, solve_heu/0, benchmark/0]).

%A palindromic number reads the same both ways. 
%The largest palindrome made from the product of two 2-digit numbers is 9009 = 91*99.
%
%Find the largest palindrome made from the product of two 3-digit numbers.
%--------------------------

%=== BRUTE FORCE SOLUTION
% Another brute force solution
% We actually generate all the palyndromes and find the maximum

solve_brute() -> lists:max(multipliers(100,999)).


palindrome(X) -> 
	A =  X div 100000,
	B = (X rem 100000) div 10000,
	C = (X rem 10000) div 1000,
	D = (X rem 1000) div 100,
	E = (X rem 100) div 10,
	F =  X rem 10,
	(A == F) andalso (B == E) andalso (C == D).


multipliers(From, To) -> [Z || X <- lists:seq(From, To), 
                               Y <- lists:seq(From, X), 
                               Z <- [X*Y],
                               palindrome(Z)].


%=== HEURISTIC SOLUTION
% We generate palindromes from maximum possible down to 1,
% we find if the generated palindrome has a 3-digit divider,
% if it does, we've got the result.
%
% That's not actually my solution, I've looked for it on
% the Project Euler forums. Shame of me that I did not think
% of anything better than brute force. But since all I do
% is just an excersise in Erlang, I'll just code this solution too.

solve_heu() -> pali_dividers(999).

% Generator ~ 10#ABC
gen_pali(Generator) ->
	A =  Generator div 100,
	B = (Generator rem 100) div 10,
	C = (Generator rem 10),
	Generator * 1000 + C * 100 + B * 10 + A.

has_dividers(_, 99) -> false; 											   % Fail if we got to 2-digit divider;
has_dividers(X, Div) when X rem Div == 0 andalso X div Div < 1000 -> true; % true if we have two 3-digit dividers;
has_dividers(X, Div) -> has_dividers(X,Div-1).							   % recursively check other dividers.

pali_dividers(100) -> 1000001;
pali_dividers(Generator) ->
	Pali = gen_pali(Generator),
	Has  = has_dividers(Pali, 999),
	if Has == true -> Pali;
		true -> pali_dividers(Generator-1)
	end.


%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve_brute, []),
    io:format("solve_brute: ~p (~p ms)~n", [S1Result, S1Time/1000]),

    {S2Time, S2Result} = timer:tc(?MODULE, solve_heu, []),
    io:format("solve_heu: ~p (~p ms)~n", [S2Result, S2Time/1000]).