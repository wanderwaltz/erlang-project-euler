-module(pe28).
-export([solve/0, benchmark/0]).

% Starting with the number 1 and moving to the right 
% in a clockwise direction a 5 by 5 spiral is formed as follows:
%
% 21 22 23 24 25
% 20  7  8  9 10
% 19  6  1  2 11
% 18  5  4  3 12
% 17 16 15 14 13
%
% It can be verified that the sum of the numbers on the diagonals is 101.
% 
% What is the sum of the numbers on the diagonals 
% in a 1001 by 1001 spiral formed in the same way?
%--------------------------


%=== STRAIGHTFORWARD SOLUTION
solve() -> sum_diag(501).


sum_diag(Max) -> 1 + sum_diag(Max, 2, 1).

sum_diag(Max, Level, _) when Level > Max -> 0;
sum_diag(Max, Level, Last) ->
    Offset = 2 * (Level-1),
    BR = Last + Offset, 
    BL = BR + Offset,
    UL = BL + Offset,
    UR = UL + Offset,

    BR + BL + UL + UR + sum_diag(Max, Level+1, UR).

%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).