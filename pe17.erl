-module(pe17).
-export([solve/0, benchmark/0c]).

% If the numbers 1 to 5 are written out in words: 
% one, two, three, four, five, then there are 
% 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
% 
% If all the numbers from 1 to 1000 (one thousand) 
% inclusive were written out in words, how many 
% letters would be used?
% 
% 
% NOTE: Do not count spaces or hyphens. 
% For example, 342 (three hundred and forty-two) 
% contains 23 letters and 115 (one hundred and fifteen) 
% contains 20 letters. 
% The use of "and" when writing out numbers is 
% in compliance with British usage.
%--------------------------

%=== SOLUTION WITH ACTUAL WRITING
% I've decided to generate the actual strings for all of the
% numbers, because it's easier to debug that way.

solve() -> count(1000).

% Counts all letters needed to write all numbers from 1 to N
count(0) -> 0;
count(N) -> count_number(write(N)) + count(N-1).


% Counts all letters in a string representing number,
% skipping spaces and hyphens
count_number([]) -> 0;
count_number([H|T]) when H /= $\s andalso H /= $- -> 1+count_number(T);
count_number([_|T]) -> count_number(T).


% Generates a string representing the given number
write(1)    -> "one";
write(2)    -> "two";
write(3)    -> "three";
write(4)    -> "four";
write(5)    -> "five";
write(6)    -> "six";
write(7)    -> "seven";
write(8)    -> "eight";
write(9)    -> "nine";
write(10)   -> "ten";
write(11)   -> "eleven";
write(12)   -> "twelve";
write(13)   -> "thirteen";
write(14)   -> "fourteen";
write(15)   -> "fifteen";
write(16)   -> "sixteen";
write(17)   -> "seventeen";
write(18)   -> "eighteen";
write(19)   -> "nineteen";
write(20)   -> "twenty";
write(30)   -> "thirty";
write(40)   -> "forty";
write(50)   -> "fifty";
write(60)   -> "sixty";
write(70)   -> "seventy";
write(80)   -> "eighty";
write(90)   -> "ninety";
write(1000) -> "one thousand";
write(N) when N > 20, N < 100 -> write(N - N rem 10) ++ "-" ++ write(N rem 10);
write(N) when N >= 100, N rem 100 == 0 -> write(N div 100) ++ " hundred";
write(N) when N >= 100, N < 1000 -> write(N - N rem 100) ++ " and " ++ write(N rem 100).

%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).