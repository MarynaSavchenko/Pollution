%% @author marina
%% @doc @todo Add description to pollution_server_test.


-module(pollution_server_test).

%% ====================================================================
%% API functions
%% ====================================================================
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).



%% ====================================================================
%% Internal functions
%% ====================================================================


start_test() ->
	Expected = true,
	Actual = pollution_server:start(),
	pollution_server:stop(),
	?assertEqual(Expected, is_pid(Actual)).

stop_test() ->
	Expected = stop,
	pollution_server:start(),
	Actual = pollution_server:stop(),
	?assertEqual(Expected, Actual).

addStation_test() ->
	Expected = {addStation,{"New",{1,2}}},
	pollution_server:start(),
	Actual = pollution_server:addStation("New", {1,2}),
	pollution_server:stop(),
	?assertEqual(Expected, Actual).

addValue_test() ->
	Expected = {addValue,{"New2","12-12-12","Mp4",67}},
	pollution_server:start(),
	Actual = pollution_server:addValue("New2", "12-12-12", "Mp4", 67),
	pollution_server:stop(),
	?assertEqual(Expected, Actual).

removeValue_test() ->
	Expected = {removeValue,{"New","12-11-78","Mp74"}},
	pollution_server:start(),
	Actual = pollution_server:removeValue("New", "12-11-78", "Mp74"),
	pollution_server:stop(),
	?assertEqual(Expected, Actual).

getOneValue_test() ->
	Expected = {getOneValue,{"New","Mp467","11-0"}},
	pollution_server:start(),
	Actual = pollution_server:getOneValue("New","Mp467","11-0"),
	pollution_server:stop(),
	?assertEqual(Expected, Actual).


getStationMean_test() ->
	Expected = {getStationMean,{"New","Mp467"}},
	pollution_server:start(),
	Actual = pollution_server:getStationMean("New","Mp467"),
	pollution_server:stop(),
	?assertEqual(Expected, Actual).
	
	
	
	
	
	
	
	
	
%%addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2
