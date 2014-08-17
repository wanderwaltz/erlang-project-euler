-module(pe31).
-export([solve/0, benchmark/0, number_of_sums/1]).

% In England the currency is made up of pound, £, and pence, p,
% and there are eight coins in general circulation:
%
% 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
%
% It is possible to make £2 in the following way:
%
% 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
% How many different ways can £2 be made using any number of coins?
%--------------------------
% Answer: 73682

%==========================
solve() -> number_of_sums(200).

coins() -> [1, 2, 5, 10, 20, 50, 100, 200].

number_of_sums(X) -> {Val, _} = number_of_sums(X, coins()), Val.

number_of_sums(X, Coins) -> number_of_sums(X, Coins, dict:new()).

number_of_sums(X, _, Map) when X =< 0 -> {0, Map};
number_of_sums(_, [], Map) -> {0, Map};
number_of_sums(X, [X | _], Map) -> {1, Map};
number_of_sums(X, [V | _], Map) when X < V -> {0, Map};
number_of_sums(X, [V | Coins], Map) ->
  case dict:find({X, [V|Coins]}, Map) of
    error -> {N1, Updated_Map1} = number_of_sums(X-V, [V|Coins], Map),
             {N2, Updated_Map2} = number_of_sums(X, Coins, Updated_Map1),
             Total = N1+N2,
             {Total, dict:store({X, [V|Coins]}, Total, Updated_Map2)};

    {ok, Value} -> {Value, Map}
  end.


%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).