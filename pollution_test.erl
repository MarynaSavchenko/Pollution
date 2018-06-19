%% @author marina
%% @doc @todo Add description to pollution_test.


-module(pollution_test).

%% ====================================================================
%% API functions
%% ====================================================================
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).




%% ====================================================================
%% Internal functions
%% ====================================================================

createMonitor_test() ->
	Expected = [],
	Actual = pollution:createMonitor(),
	?assertEqual(Expected, Actual).


addStation_test() ->
	Expected = [{station,"New",{coordinate,1,2},[]}],
	Monitor = pollution:createMonitor(),
	Actual = pollution:addStation("New", {1,2}, Monitor),
	?assertEqual(Expected, Actual).

addValue_test() ->
	Expected = {error,"The station with such name doesnt exist!"},
	Monitor = pollution:createMonitor(),
	P = pollution:addStation("New", {1,2}, Monitor),
	Actual = pollution:addValue("New2", "12-12-12", "Mp4", 67, P),
	?assertEqual(Expected, Actual).

addValue_test2() ->
	Expected = [{station,"New",
          {coordinate,1,2},
          [{measure,"12-11-78","Mp74",167}]}],
	Monitor = pollution:createMonitor(),
	P = pollution:addStation("New", {1,2}, Monitor),
	Actual = pollution:addValue("New", "12-11-78", "Mp74", 167, P),
	?assertEqual(Expected, Actual).

removeValue_test() ->
	Expected = [{station,"New",{coordinate,1,2},[]}],
	Monitor = pollution:createMonitor(),
	P = pollution:addStation("New", {1,2}, Monitor),
	Actual = pollution:removeValue("New", "12-11-78", "Mp74", P),
	?assertEqual(Expected, Actual).
	

getOneValue_test() ->
	Expected = 10,
	Monitor = pollution:createMonitor(),
	P = pollution:addStation("New", {1,2}, Monitor),
	P1 = pollution:addValue("New", "11-53-12", "Mp467", 10, P),
	Actual = pollution:getOneValue("New","Mp467","11-53-12",P1),
	?assertEqual(Expected, Actual).
					   

getStationMean_test() ->
	Expected = 8.0,
	P = pollution:createMonitor(),
	P1 = pollution:addStation("New", {1,2}, P),
	P9 = pollution:addValue("New", "11-53-12", "Mp467", 10, P1),
	P15 = pollution:addValue("New", "11-53-12", "Mp467", 6, P9),
	Actual = pollution:getStationMean("New","Mp467",P15),
	?assertEqual(Expected, Actual).

	

