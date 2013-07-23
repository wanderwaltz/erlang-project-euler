-module(pe23).
-export([solve/0, 
		 benchmark/0,
	     sum_divisors/1, 
	     is_abundant/1, 
	     is_deficient/1, 
	     is_perfect/1, 
	     classify/1,
	     list/2,
	     upper_limit/0,
	     list_all_abundants/0,
	     list_sums/2,
	     list_all_sums_of_abundants/0,
	     array_not_representible/0,
	     all_candidates/0,
	     sum_not_representible/0,
	     list_not_representible/0,
	     zero_entries_from_list/2]).


% A perfect number is a number for which the sum of its proper divisors
% is exactly equal to the number. For example, the sum of the proper 
% divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28
% is a perfect number.
%
% A number n is called deficient if the sum of its proper divisors is
% less than n and it is called abundant if this sum exceeds n.
%
% As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the 
% smallest number that can be written as the sum of two abundant numbers 
% is 24. By mathematical analysis, it can be shown that all integers
% greater than 28123 can be written as the sum of two abundant numbers. 
% However, this upper limit cannot be reduced any further by analysis 
% even though it is known that the greatest number that cannot be 
% expressed as the sum of two abundant numbers is less than this limit.
%
% Find the sum of all the positive integers which cannot be written as 
% the sum of two abundant numbers.
%--------------------------
% Note: proper divisor of n is a divisor which is different from n
% 
% Research brought me here: 
%    
% [A048242] Numbers that are not the sum of two abundant numbers.
%    http://oeis.org/A048242
% 
% [A005101] Abundant numbers (sum of divisors of n exceeds 2n)	 
%    http://oeis.org/A005101
%
% Useful for checking the intermediate results.
% 
% Right answer: 4179871
%--------------------------


% A slow straightforward brute force method.
% First it finds all abundant number below the given upper limit, then
% generates all of the possible sums and marks the numbers representible
% as a sum of abundants in a certain array. The last step is to count
% the sum of the remaining array elements.
%
% This methods performs in roughly a quarter of a minute on my Mac, so it is 
% obviously not optimized enough. Will have to work on it later to improve
% the performance.
solve() -> sum_not_representible().


% As stated in the problem description
upper_limit() -> 28123.

% Array of all numbers from 1 to upper limit
all_candidates() -> array:from_list(lists:seq(1,upper_limit())).

% Returns a list of all abundant numbers from 1 to upper limit
list_all_abundants() -> list(abundant, upper_limit()).

% Generates a list of all sums of abundant numbers from 1 to upper limit.
% Does not include sums that exceed the upper limit.
list_all_sums_of_abundants() -> list_sums(list_all_abundants(), upper_limit()).

% Generates a list of sums of elements from the given list not exceeding the upper bound.
list_sums(List, UpperBound) -> [A+B || A <- List, B <- List, A =< B, A+B =< UpperBound].

% Zeroes elements representible by a sum of two abundant numbers from the all_candidates array
array_not_representible() -> zero_entries_from_list(all_candidates(), list_all_sums_of_abundants()).

% Counts the sum of the array_not_representible elements
sum_not_representible() -> array:foldl(fun(_,X,A)->A+X end, 0, array_not_representible()).

% Iterates through the given list and zeroes elements in the given array at the indices from the list
zero_entries_from_list(Array, []) -> Array;
zero_entries_from_list(Array, [X|List]) -> zero_entries_from_list(array:set(X-1,0,Array), List).

% Converts array_not_representible to a list filtering out zero entries (used for debugging)
list_not_representible() -> [X || X <- array:to_list(array_not_representible()), X > 0].


% Returns the sum of proper divisors for a given number
sum_divisors(1) -> 0;
sum_divisors(N) -> 1 + sum_divisors_iterate(N, 2, math:sqrt(N)).

sum_divisors_iterate(_, I, Max) when I > Max -> 0;
sum_divisors_iterate(N, I, Max) when N rem I == 0, N div I == I -> I           + sum_divisors_iterate(N, I+1, Max);
sum_divisors_iterate(N, I, Max) when N rem I == 0               -> I + N div I + sum_divisors_iterate(N, I+1, Max);
sum_divisors_iterate(N, I, Max) -> sum_divisors_iterate(N, I+1, Max).



% Individual classification functions, were used for debugging
is_abundant(N)  -> (sum_divisors(N)  > N).
is_deficient(N) -> (sum_divisors(N)  < N).
is_perfect(N)   -> (sum_divisors(N) == N).

% Classifies the number which is passed as the sole parameter to the function
classify(N) ->
	Sum = sum_divisors(N),
	if Sum  < N -> deficient;
	   Sum == N -> perfect;
	   true     -> abundant
	end.

% Lists all number up to the given limit which fall in the category passed
% as the first parameter. Categories should match these returned by classify
% function.
list(_, 0) -> [];
list(Type, Limit) ->
	Which = classify(Limit),
	if Which == Type -> [Limit | list(Type, Limit-1)];
		true -> list(Type, Limit-1)
	end.


%=== BENCHMARK
benchmark() ->

	io:format("Profiling indiviual steps of the straightforward algorithm:~n",[]),

	UpperBound = upper_limit(),

	{AbGenTime, AllAbundants} = timer:tc(?MODULE, list_all_abundants, []),
	io:format("list_all_abundants: ~p ms, list length: ~p~n", [AbGenTime/1000, length(AllAbundants)]),

	{AbSumGenTime, AllSumsOfAbundants} = timer:tc(?MODULE, list_sums, [AllAbundants, UpperBound]),
	io:format("list_sums(AllAbundants, UpperBound): ~p ms, list length: ~p~n", [AbSumGenTime/1000, length(AllSumsOfAbundants)]),

	{AllCandidatesGenTime, AllCandidates} = timer:tc(?MODULE, all_candidates, []),
	io:format("all_candidates: ~p ms~n", [AllCandidatesGenTime/1000]),

	{ZeroCandidatesTime, _} = timer:tc(?MODULE, zero_entries_from_list, [AllCandidates, AllSumsOfAbundants]),
	io:format("zero_entries_from_list: ~p ms~n", [ZeroCandidatesTime/1000]),

	io:format("Profiling the straightforward algorithm as a whole:~n", []),

    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).