-module(pe19).
-export([solve/0, day_info/3, benchmark/0]).

% You are given the following information, but you may prefer to do some research for yourself.
% 
% 1 Jan 1900 was a Monday.
%
% Thirty days has September,
% April, June and November.
% All the rest have thirty-one,
% Saving February alone,
% Which has twenty-eight, rain or shine.
% And on leap years, twenty-nine.
%
% A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
%
% How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
%--------------------------


%=== STRAIGHTFORWARD SOLUTION
% The solution may be somewhat overcomplicated, but it's easier to debug it that way.

solve() -> {_, _, _, _, Count} = calendar(1, january, 1900, monday, 1, january, 2001, 0), Count.


day_info(Date, Month, Year) ->
	calendar(1, january, 1900, monday, Date, Month, Year, 0).	

calendar(Date, Month, Year, Weekday, Date, Month, Year, Count) -> {Date, Month, Year, Weekday, Count};
calendar(Date, Month, Year, Weekday, EndDate, EndMonth, EndYear, Count) ->

	if Weekday == sunday, Date == 1, Year >= 1901, Year =< 2000 -> 
			%io:format("~p ~p ~p~n", [Date, Month, Year]),
			Value = 1;
		true -> Value = 0
	end,

	case Date+1 > daysInMonth(Month, Year) of
		true  -> 
			if Month == december -> calendar(1,          january, Year+1, nextDay(Weekday), EndDate, EndMonth, EndYear, Value+Count);
				            true -> calendar(1, nextMonth(Month), Year  , nextDay(Weekday), EndDate, EndMonth, EndYear, Value+Count)
			end;


		false -> calendar(Date+1, Month, Year, nextDay(Weekday), EndDate, EndMonth, EndYear, Value+Count)
	end.
 

nextDay(monday)    -> thursday;
nextDay(thursday)  -> wednesday;
nextDay(wednesday) -> tuesday;
nextDay(tuesday)   -> friday;
nextDay(friday)    -> saturday;
nextDay(saturday)  -> sunday;
nextDay(sunday)    -> monday.


nextMonth(january)   -> february;
nextMonth(february)  -> march;
nextMonth(march)     -> april;
nextMonth(april)     -> may;
nextMonth(may)       -> june;
nextMonth(june)      -> july;
nextMonth(july)      -> august;
nextMonth(august)    -> september;
nextMonth(september) -> october;
nextMonth(october)   -> november;
nextMonth(november)  -> december;
nextMonth(december)  -> january.



leapYear(Year) when Year rem 100 == 0 -> (Year rem 400 == 0);
leapYear(Year) -> (Year rem 4 == 0).


daysInMonth(january,  _) -> 31;
daysInMonth(february, Year) -> 
	case leapYear(Year) of
		true  -> 29;
		false -> 28
	end;
daysInMonth(march,     _) -> 31;
daysInMonth(april,     _) -> 30;
daysInMonth(may,       _) -> 31;
daysInMonth(june,      _) -> 30;
daysInMonth(july,      _) -> 31;
daysInMonth(august,    _) -> 31;
daysInMonth(september, _) -> 30;
daysInMonth(october,   _) -> 31;
daysInMonth(november,  _) -> 30;
daysInMonth(december,  _) -> 31.


%=== BENCHMARK
benchmark() ->
    {S1Time, S1Result} = timer:tc(?MODULE, solve, []),
    io:format("solve: ~p (~p ms)~n", [S1Result, S1Time/1000]).

