-module(pe26).
-export([solve/0, divide/2, benchmark/0, test_divide/0]).

% A unit fraction contains 1 in the numerator. 
% The decimal representation of the unit fractions 
% with denominators 2 to 10 are given:
%
% 1/2 =   0.5
% 1/3 =   0.(3)
% 1/4 =   0.25
% 1/5 =   0.2
% 1/6 =   0.1(6)
% 1/7 =   0.(142857)
% 1/8 =   0.125
% 1/9 =   0.(1)
% 1/10    =   0.1
%
% Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. 
% It can be seen that 1/7 has a 6-digit recurring cycle.
%
% Find the value of d < 1000 for which 1/d contains the longest 
% recurring cycle in its decimal fraction part.
%--------------------------


%=== STRAIGHTFORWARD SOLUTION
% Accurately calculate the division result, and store the
% intermediate numerators in a set. When we encounter a numerator
% which we already have seen, stop - we found our period.
%
% Check the periods for all d  and find the one with the longest
% period.
solve() -> find_max(1000, 0, 0).


% The search function
%
% First argument is N - the maximum value of d to check.
% We search down until we reach N == 1.
find_max(1, MaxLen, Result) -> {result, Result, period, MaxLen};
find_max(N, MaxLen, Result) ->
    Period    = divide(1,N),
    PeriodLen = length(Period),

    if MaxLen < PeriodLen -> find_max(N-1, PeriodLen, N);
       true -> find_max(N-1, MaxLen, Result)
   end.


% A binary alias to divide/5
divide(A, B) -> divide({A, B, {integer, []}, sets:new()}).

% This function returns a result of division A/B as string.
% For periodic decimals, this function returns only the first
% period, without the brackets, i.e. 1/7 = "0.142857"
divide({0, _, Result, _}) -> value_string(Result);

% When result does not have a decimal point yet, but should have in the next call
divide({Numer, Denom, Result, Numers}) when Numer < Denom -> 
    divide({Numer*10, Denom, 
            addDecimalPoint(Result), 
            sets:add_element(Numer, Numers)});

% Most general case
divide(Params = {Numer, _, Result, Numers}) ->

    case sets:is_element(Numer, Numers) of
        true  -> value_string(Result); % Break if we already encountered this Numer
        false -> divide_safe(Params)   % Else do everything as usual
    end.



% At this point we are sure that we did not hit peroidic 
% part of the result
divide_safe({Numer, Denom, Result = {Type, _}, Numers}) ->

    {Digit, DigitMultiplier, Rest} = divisionSubResult(Numer, Denom),

    case Rest == 0 of
        true  -> value_string(addZeros(DigitMultiplier, addDigit(Digit, Result)));
        false ->
            case Type of 
                integer -> NextNumerFactor = 1;
                decimal -> NextNumerFactor = 10
            end,

            divide({Rest * NextNumerFactor, Denom, 
                    addDigit(Digit, Result), 
                    sets:add_element(Numer, Numers)})
    end.



divisionSubResult(Numer, Denom) ->
    {Trunc, DigitMultiplier} = truncFactor(Numer, Denom),

    Digit = Trunc div Denom, 
    Sub   = Digit * Denom * DigitMultiplier,
    Rest  = Numer-Sub,

    % Return result
    {Digit, DigitMultiplier, Rest}.



value_string({_, Value}) -> lists:reverse(Value).



addDecimalPoint({integer, []})     -> {decimal, ".0"};
addDecimalPoint({integer, String}) -> {decimal, [$.|String]};
addDecimalPoint({decimal, String}) -> {decimal, [$0|String]}.



addDigit(X, {T, V}) -> {T, [X+$0|V]}.



addZeros(1, X) -> X;
addZeros(N, {T, V}) when N > 1 -> addZeros(N div 10, {T, [$0|V]}).



truncFactor(Numer, Denom) -> truncFactor(Numer, Denom, 1).

truncFactor(Numer, Denom, Factor) when Numer div 10 >= Denom -> 
    truncFactor(Numer div 10, 
                Denom, 
                Factor * 10);

truncFactor(Numer, _, Factor) -> {Numer, Factor}.


%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).


%=== UNIT TEST FOR DIVIDE FUNCTION
test_divide() -> 
    assert_divide(   6,   2,  "3"),
    assert_divide(   1,   3,  "0.3"),
    assert_divide(   1, 100,  "0.01"),
    assert_divide(   1,   8,  "0.125"),
    assert_divide(   9,   3,  "3"),
    assert_divide(  12,   4,  "3"),
    assert_divide(  12,   3,  "4"),
    assert_divide( 100,   2, "50"),
    assert_divide( 121,  11, "11"),
    assert_divide(  25,   5,  "5"),
    assert_divide(  25,   2, "12.5"),
    assert_divide(   1,   4,  "0.25"),
    assert_divide(   1,   5,  "0.2"),
    assert_divide( 100,  10,  "10"),
    assert_divide(1000,  10,  "100"),
    assert_divide(   1,   7,  "0.142857").


assert_divide(A, B, Expectation) ->
    Result = divide(A,B),
    if Result == Expectation -> ok;
        true -> 
            io:format("divide(~p, ~p) == ~p, expected: ~p~n", [A,B,Result,Expectation]),
            error
    end.
