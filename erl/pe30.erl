-module(pe30).
-export([solve/0, benchmark/0]).

% Surprisingly there are only three numbers that can be written
% as the sum of fourth powers of their digits:
%
%    1634 = 1^4 + 6^4 + 3^4 + 4^4
%    8208 = 8^4 + 2^4 + 0^4 + 8^4
%    9474 = 9^4 + 4^4 + 7^4 + 4^4
%
% As 1 = 14 is not a sum it is not included.
%
% The sum of these numbers is 1634 + 8208 + 9474 = 19316.
%
% Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
%--------------------------
% Answer: 443839

%==========================
solve() -> count(5).

count(Power) -> iterate(max_n_digit_number(upper_bound(Power)), Power)-1.

iterate(0, _) -> 0;
iterate(N, Power) ->
  Sum = sum_digit_powers(N, Power),
  if N == Sum ->
        N + iterate(N-1, Power);
    true -> iterate(N-1, Power)
  end.


max_n_digit_number(0) -> 0;
max_n_digit_number(N) -> max_n_digit_number(N-1)+9*zeros(N-1).

zeros(0) -> 1;
zeros(N) -> zeros(N-1)*10.

power(_, 0) -> 1;
power(N, K) -> N*power(N,K-1).

% counts sum of digit powers for the number N, breaks the computation early and returns 0 if
% the intermediate sum gets greater than original number (optimisation of the iteration above)
sum_digit_powers(N, Power) -> sum_digit_powers(N, N, Power, 0).

% counts the true sum of digit powers for the number N (i.e. without breaking early)
sum_digit_powers_unbound(N, Power) -> sum_digit_powers(-1, N, Power, 0).

sum_digit_powers(_, 0, _, Acc) -> Acc;
sum_digit_powers(Orig_N, N, Power, Acc) when Acc =< Orig_N; Orig_N =< 0 ->
    sum_digit_powers(Orig_N, N div 10, Power, Acc+power(N rem 10, Power));
sum_digit_powers(Orig_N, _, _, Acc) when Acc > Orig_N -> 0.

upper_bound(Power) -> upper_bound(Power, 2).

upper_bound(Power, Digits) ->
  Max_Number = max_n_digit_number(Digits),
  Sum_Powers = sum_digit_powers_unbound(Max_Number, Power),
  if Sum_Powers < Max_Number -> Digits;
    true -> upper_bound(Power, Digits+1)
  end.

%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).