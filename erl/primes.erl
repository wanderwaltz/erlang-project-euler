-module(primes).
-export([is_prime/2, sieve/1, benchmark/0]).

% Contains functions which could be useful for other Project Euler problems
% and are related to prime numbers.

is_prime([P |     _], N) when N == P -> true;
is_prime([P | Sieve], N) when N  > P -> is_prime(Sieve, N);
is_prime(         _,  _) 	 		 -> false.



% Returns a list of primes <= N in ascending order
sieve(N) -> lists:reverse(sieve_private([3|[2|[]]], N, default_sieve(N))).


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
sieve_private([not_found|Primes], _, _) -> Primes;
sieve_private(Primes, N, Sieve) ->
	% The current prime is expected to be the head of Primes list,
	% that's why we must provide a nonempty initial value.
 	Prime  = hd(Primes),								

 	% Mark multipliers of the current prime in the array, 
 	% starting from the Prime^2. We don't have to check
 	% values between Prime and Prime^2 since these are 
 	% already marked when searching for previous primes.
 	Marked = mark_multipliers(N, Sieve, Prime, Prime * Prime),


	% The next prime will be an index in array, whose value is still 1
	Next   = find_1_after(Prime+2, N, Sieve),
	sieve_private([Next|Primes], N, Marked).




% Mark multipliers of the Value starting with Current in array Sieve
mark_multipliers(Size, Sieve, Value, Current) when Current < Size -> 
	mark_multipliers(Size, array:set(Current, 0, Sieve), Value, Current+Value);
mark_multipliers(_, Sieve, _, _) -> Sieve.

%======
% Find the first index after Candidate which has Sieve[Candidate] == 1
% Do steps of 2 skipping even numbers

% find_1_after/3 checks that the candidate for the next prime does not exceed
% the limit and passes the Sieve[Candidate] to find_1_after/4
find_1_after(Candidate, Limit, Sieve) when Candidate =< Limit -> find_1_after(Candidate, array:get(Candidate, Sieve), Limit, Sieve);
find_1_after(_, _, _) -> not_found. % Return not_found when overflown the limit

% find_1_after/4 checks whether the value passed as the second parameter is 1
% (which would mean that the next prime number is found). If not, pass the next
% prime candidate to find_1_after/3 for bounds checking.
find_1_after(    Prime, 1, _, _) -> Prime;
find_1_after(Not_Prime, _, Limit, Sieve) -> find_1_after(Not_Prime+2, Limit, Sieve).



%=== BENCHMARK

benchmark() ->
	io:format("Primes module benchmark: computing sieve(2000000). "),
    {Time, Result} = timer:tc(?MODULE, sieve, [2000000]),
    io:format("Time: ~p ms, found ~p primes~n", [Time/1000, length(Result)]).