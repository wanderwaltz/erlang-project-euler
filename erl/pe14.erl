-module(pe14).
-export([solve/0, benchmark/0]).

% The following iterative sequence is defined for the set of positive integers:
% 
% n -> n/2    (n is even)
% n -> 3n + 1 (n is odd)
%
% Using the rule above and starting with 13, we generate the following sequence:
% 
% 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
%
% It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
% Although it has not been proved yet (Collatz Problem), it is thought that all starting 
% numbers finish at 1.
%
% Which starting number, under one million, produces the longest chain?
%
% NOTE: Once the chain starts the terms are allowed to go above one million.
%--------------------------

%=== DYNAMIC PROGRAMMING SOLUTION
%
% Since the sequence is recursive, we can store the lengths of already
% found sequences in array for later use. 

solve() -> longest(1000000).


longest(Max) -> longest(Max, 0, 0, 1, array:new(1000000, {default, 0})).

longest(Max, _,       LI, I,     _) when I > Max -> LI;
longest(Max, Longest, LI, I, Array) ->
	
	{Length, NewArr} = sequence(I, Array),
	if Length > Longest -> longest(Max, Length, I, I+1, NewArr);
		true -> longest(Max, Longest, LI, I+1, NewArr)
	end.


% Had some trouble implementing the dynamic approach in Erlang
% We cannot have the array as global var as I would done in other
% languages where the variables are mutable. So we have to pass
% the array to each sequence calculation call and return both
% the (possibly) modified array and the sequence length as a result.
%
% I guess this kind of behavior is not very effective since it
% involves copying the array each time (does it really?)

sequence(1, Array) -> {1, Array};

sequence(N, Array) when N < 1000000 ->
	Stored = array:get(N, Array),

	if Stored > 0 -> {Stored, Array};
		true ->
			if N rem 2 == 0 ->
				{Len, NewArr} = sequence(N div 2, Array);
			true ->
				{Len, NewArr} = sequence(3*N+1, Array)
			end,
			{Len+1, array:set(N, Len+1, NewArr)}
	end;

sequence(N, Array) when N rem 2 == 0 ->
	{Len, NewArr} = sequence(N div 2, Array),
	{Len+1, NewArr};

sequence(N, Array) ->
	{Len, NewArr} = sequence(3*N+1, Array),
	{Len+1, NewArr}.

%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).