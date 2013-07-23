-module(primes).
-export([is_prime/2, sieve/1]).

% Contains functions which could be useful for other Project Euler problems
% and are related to prime numbers.

is_prime([P |     _], N) when N == P -> true;
is_prime([P | Sieve], N) when N  > P -> is_prime(Sieve, N);
is_prime(         _,  _) 	 		 -> false.



% Returns a list of primes <= N in ascending order
sieve(N) -> lists:reverse(sieve_private([2], default_sieve(N))).


%------------------------------------------------------------------------------------
% PRIVATE
%------------------------------------------------------------------------------------

%=== SIEVE OF ERATOSTHENES
% A straightforward implementation of the Erathosthenes' sieve
% using the array module. 
% My own implementation of the sieve using lists sucked apparently,
% because I've never managed to actually generate the primes to
% one million (not even mentioning that the problem required 2 mil).


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