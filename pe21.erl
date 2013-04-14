-module(pe21).
-export([solve/0, benchmark/0]).

% Let d(n) be defined as the sum of proper divisors of n 
% (numbers less than n which divide evenly into n).
% If d(a) = b and d(b) = a, where a  b, then a and b are 
% an amicable pair and each of a and b are called amicable numbers.
%
% For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 
% 20, 22, 44, 55 and 110; therefore d(220) = 284. 
% The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
%
% Evaluate the sum of all the amicable numbers under 10000.
%--------------------------

%=== BRUTE FORCE SOLUTION
% Check all the numbers below 10000.
% Though with some optimizations, the brute force solution actually
% works rather well.
%
% Actually Wikipedia tells us that the known amicable pairs
% below 10000 are:
%  220 and 284
% 1184 and 1210
% 2620 and 2924
% 5020 and 5564
% 6232 and 6368
% 
% We could just print the sum without any computations

solve() -> sum_amicable(10000).

sum_amicable(1) -> 0;
sum_amicable(X) ->
    Ami = amicable(X),
    if Ami == false -> sum_amicable(X-1);
        true -> {A,B} = Ami, A+B+sum_amicable(X-1)
    end.


amicable(X) ->
    DX = divisors(X),
    Y  = lists:sum(DX),

    % In a pair of amicable numbers one will always be less than another
    % we check this condition in order not to count a pair twice
    if Y > X -> 
        DY = divisors(Y),
        S  = lists:sum(DY),

        if S == X, Y /= X-> {X,Y};
            true -> false
        end;

        true -> false
    end.



divisors(X) -> divisors(X, 2, [1], math:sqrt(X)).

divisors(_, I, List, Max) when I > Max -> List;
divisors(X, I, List, Max) when X rem I == 0 -> divisors(X, I+1, [X div I|[I|List]], Max);
divisors(X, I, List, Max) -> divisors(X, I+1, List, Max).

%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).