-module(pe24).
-export([solve/0,
		 benchmark/0,
		 make_perm/2]).

% A permutation is an ordered arrangement of objects. For example, 
% 3124 is one possible permutation of the digits 1, 2, 3 and 4. 
% If all of the permutations are listed numerically or alphabetically,
% we call it lexicographic order. The lexicographic permutations of 
% 0, 1 and 2 are:
%
%                 012   021   102   120   201   210
%
% What is the millionth lexicographic permutation of the digits 
% 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
%--------------------------


solve() -> make_perm([0,1,2,3,4,5,6,7,8,9], 1000000).

make_perm(List, Index) -> 
	{_, Perm, _} = make_perm(not_found, [], List, 1, Index-1),
	lists:reverse(Perm).

% First param: status atom. When it gets 'found' value, recursion should stop.
% Second param: current permutation list
% Third param: list of available digits
% Fourth param: number of digits to skip from the list when taking a new one to the permutation
% Fifth param: permutation index starting from 0
make_perm( found, Current,    _,    _,     _) -> {found, Current, 0}; % Stop recursion if found the answer
make_perm(     _, Current,   [],    _,     0) -> {found, Current, 0}; % Stop recursion if index got the value 0 and digit pool is empty

% Digit pool is empty, but the permutation index is not yet 0, so we've found some permutation, 
% but had not found the right one yet. Decrement the index, but return 'not_found' status.
make_perm(     _, Current,   [],    _, Index) -> {not_found, Current, Index-1};

% If we try to skip more digits from the pool than there is available, skip this failed attempt
% and continue without altering the state.
make_perm(Status, Current, List, Skip, Index) when Skip > length(List) -> {Status, Current, Index};

% The main recursive method
make_perm(Status, Current, List, Skip, Index) ->

	% Add the current digit to the permutation, remove it from the pool and go deeper
	X = lists:nth(Skip, List),
	{Status1, Perm1, Index1} = make_perm(Status, [$0+X | Current], List--[X], 1, Index),

	% When we return from the bottom of the recursion, we either have found the solution,
	if Status1 == found -> {found, Perm1, 0};
		% Or not found it. Then try to skip one more digit from the available pool and
		% dive into the recursive depths once more.
		true -> make_perm(Status1, Current, List, Skip+1, Index1)
	end.



%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).