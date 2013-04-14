-module(pe10).
-export([solve/0, sieve/1, benchmark/0]).

% The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
%
% Find the sum of all the primes below two million.
%--------------------------


%=== SIEVE OF ERATOSTHENES
% A straightforward implementation of the Erathosthenes' sieve
% using the array module. 
% My own implementation of the sieve using lists sucked apparently,
% because I've never managed to actually generate the primes to
% one million (not even mentioning that the problem required 2 mil).

solve() -> sum_sieve(2000000).


% This function is just a shortcut to count the sum of primes returned by sieve/1
sum_sieve(N) -> lists:sum(sieve(N)).


% Returns a list of primes <= N
sieve(N) -> sieve_private([2], default_sieve(N)).


% Initial values stored in the sieve - all 1's except the first two.
% Indexes of this array correspond the natural numbers tested for primeness,
% so we should create an array of size N+1, so that the index N is valid.
default_sieve(N) -> array:set(0, 0, array:set(1,0, array:new(N+1, {default,1}))).

% We must provide an initial nonempty list for this function to work
sieve_private(Primes, Sieve) ->
	% The current prime is expected to be the head of Primes list,
	% that's why we must provide a nonempty initial value.
 	Prime  = hd(Primes),								

 	% Mark multipliers of the current prime in the array, 
 	% but do not mark the prime itself (this actually is not
 	% necessary to skip the prime itself since these values
 	% are never used later, but whatever).
	Marked = mark_multipliers(Sieve, Prime, Prime * 2),


	% The next prime will be an index in array, whose value is still 1
	Next   = find_1_after(Prime+1, Sieve),


	% If there are no more 1s in the array, stop
	if Next == {not_found} -> Primes;

	% else continue, adding our next prime to the list.
		true -> sieve_private([Next|Primes], Marked)
	end.




% Mark multipliers of the Value starting with Current in array Sieve
mark_multipliers(Sieve, Value, Current) -> 
	Size = array:size(Sieve),
	if Current < Size -> mark_multipliers(array:set(Current, 0, Sieve), Value, Current+Value);
		true -> Sieve
	end.


% Find the first index after I which has Sieve[I] == 1
find_1_after(I, Sieve) ->
	Size = array:size(Sieve),
	if I < Size ->
		Value = array:get(I, Sieve),
		if Value == 1 -> I;
			true -> find_1_after(I+1, Sieve)
		end;

		true -> {not_found}
	end.


%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).