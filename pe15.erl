-module(pe15).
-export([solve/0, routes/1, benchmark/0]).

% Starting in the top left corner of a 22 grid, there are 6 routes 
% (without backtracking) to the bottom right corner.
%
% (see original Problem 15 page for the image)
%
% How many routes are there through a 2020 grid?
%--------------------------


%=== STRAIGHTFORWARD SOLUTION
% In a grid NxN we'll need to do exactly N steps down and N steps right
% to reach the bottom right corner, but the order of these steps is 
% unknown. 
% 
% We can say that 1 is a step down and 0 is a step right, so the path
% in the grid will be denoted as a binary number of 2N digits: 1011...0
%
% The problem now is to count binary numbers of 2N digits with exactly
% N 1s and N 0s. So we have 2N elements, with number of permutations of
% (2N)!, and we do not count the N! permutations of 1s with each other
% and N! permutations of 0s. The answer is (2N)!/N!/N! then. 

solve() -> routes(20).


% To reduce the computation we count a (2N)!/N! directly as
% (N+1)*(N+2)*...*(2N) here.
routes(N) -> multiply(N+1, 2 * N) div multiply(1, N).

multiply(Min, N)   when Min == N -> N;
multiply(From, To) when From < To -> To * multiply(From, To-1).

%=== BENCHMARK

benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).