-module(pe29).
-export([solve/0, benchmark/0]).

% Consider all integer combinations of a,b for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:
%
%     2^2=4,  2^3=8,   2^4=16,  2^5=32
%     3^2=9,  3^3=27,  3^4=81,  3^5=243
%     4^2=16, 4^3=64,  4^4=256, 4^5=1024
%     5^2=25, 5^3=125, 5^4=625, 5^5=3125
% If they are then placed in numerical order, with any repeats removed,
% we get the following sequence of 15 distinct terms:
%
% 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
%
% How many distinct terms are in the sequence generated by a^b for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
%--------------------------
% Answer: 9183

%==========================
% Let us try to find in which case we'll have non-distinct values in the list of all powers:
%
%   n^a = m^b, n = m^(b/a)
%
% So we see that this can actually happen only in one case: when m is some power of n.
%
% Our solution will be the following: we maintain a set of values which we've not yet
% considered at all. We'll iterate these values in ascending order.
%
% For that we have the iterate_distincts function which accepts the ordered list of values
% as the third parameter and the set of not-yet-considered values as the fourth parameter.
%
% The Allowed_Set will always shrink until we have an empty set there. We won't consider
% the number if it is not in Allowed_Set.
%
% The next major function is count_distinct, it counts the distinct numbers we can get
% by raising a given number to all powers up to MaxPower. Obviously this value would be
% equal to MaxPower (since we don't consider 1), but we don't stop on that; for the given N
% we also consider all powers of N^2, i.e. (N^2)^2 = N^(2*2), (N^2)^3 = N^(2*3) etc.
% So we take a list of powers (2,...,MaxPower) and multiply it by 2 to get powers of N^2 in
% terms of N: (4, 6, ..., MaxPower*2). We place all these numbers into the same set to get rid
% of the duplicates. Then we consider N^3 and all its powers etc. We stop at upper_bound value
% where upper_bound is calculated as a log(MaxValue)/log(N).
%
% Having the all the considered powers in the single set allows us to do two things:
% 1) We count the elements in this set to add to the resulting value
% 2) We raise N to all powers in this set and remove all these numbers from the Allowed_Set to
%    make sure we won't consider these numbers twice.
%
% Then we return to the iterate_distincts function and continue with the next number from the list.
%
% In the result we iterate through all numbers in 2..MaxValue interval, consider each value only
% once and count all its powers thus giving the answer to the problem.

%=== BENCHMARK

solve() -> count_all_distincts(100, 100).

default_set(Max) -> append_list(sets:new(), enumerate(Max)).

count_all_distincts(MaxValue, MaxPower) ->
    iterate_distincts(MaxValue, MaxPower, enumerate(MaxValue), default_set(MaxValue)).

iterate_distincts(_, _, [], _) -> 0;
iterate_distincts(MaxValue, MaxPower, [H|T], Allowed_Set) ->
    Current_Number_Allowed = sets:is_element(H, Allowed_Set),
    if Current_Number_Allowed -> [Count, Iterated_Set] = count_distinct(MaxValue, MaxPower, H);
      true -> [Count, Iterated_Set] = [0, sets:new()]
    end,
    Count + iterate_distincts(MaxValue, MaxPower, T, sets:subtract(Allowed_Set, Iterated_Set)).

count_distinct(MaxValue, MaxPower, N) ->
    Set = append_multiplies(sets:new(), enumerate(MaxPower), upper_bound(MaxValue, N)),
    [sets:size(Set), sets:from_list(powers(N, sets:to_list(Set)))].

power(_, 0) -> 1;
power(N, K) -> N*power(N,K-1).

powers(N, List) -> [power(N, K) || K <- List].

% appends all lists multiplied by 1..Multiplier to the given set
append_multiplies(Set, _, 0) -> Set;
append_multiplies(Set, List, Multiplier) ->
    append_multiplies(append_list(Set, multiply(List, Multiplier)), List, Multiplier-1).

% finds K such as N^K <= MaxValue, N^(K+1) > MaxValue
upper_bound(_, 1) -> 0;
upper_bound(MaxValue, N) -> trunc(math:log(MaxValue)/math:log(N)).

enumerate(Max) -> [X || X <- lists:seq(2,Max)].

% multiplies each element of the list by the given multiplier
multiply(List, Multiplier) -> [X*Multiplier || X <- List].

% appends all elements of a list to the given set
append_list(Set, []) -> Set;
append_list(Set,[H|T]) -> append_list(sets:add_element(H, Set), T).

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).