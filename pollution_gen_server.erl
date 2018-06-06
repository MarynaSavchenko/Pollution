
%% @author marina
%% @doc @todo Add description to pollution_gen_server.


-module(pollution_gen_server).
-behaviour(gen_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,init/1,start/0,addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2,crash/0,handle_cast/2,handle_call/3,stop/0,terminate/2]).

start_link(empty) ->
gen_server:start_link(
{local,pollution_gen_server},pollution_gen_server, empty, []).

init(_)->
	Monitor = pollution:createMonitor(),
	{ok, Monitor}.

start() ->
	start_link(empty).

addStation(Name,Coordinate) ->
	gen_server:call(pollution_gen_server, {addStation,{Name,Coordinate}}).

addValue(Station,Date,Type,Value) ->
	gen_server:call(pollution_gen_server, {addValue,{Station,Date,Type,Value}}).

removeValue(Station, Date,Type) ->
	gen_server:call(pollution_gen_server, {removeValue,{Station, Date,Type}}).	

getOneValue(Station, Type,Date) ->
	gen_server:call(pollution_gen_server, {getOneValue,{Station, Type,Date}}).	

getStationMean(Station,Type) ->
	gen_server:call(pollution_gen_server, {getStationMean,{Station, Type}}).

stop() ->
	gen_server:call(pollution_gen_server, terminate).

crash() -> 		
	gen_server:cast(pollution_gen_server, crash).

handle_call({addStation,{Name,Coordinate}},_From, Monitor) ->
	NewMonitor = pollution:addStation(Name, Coordinate, Monitor),
			if 
				is_tuple(NewMonitor) ->
					io:format("~p~n", [NewMonitor]),
					{reply, {Name,Coordinate}, Monitor};
				true ->
					io:format("Monitor after adding new Station : ~p~n",[NewMonitor]),
					{reply, {Name,Coordinate}, NewMonitor}
			end;

handle_call(terminate, _From, Monitor) ->
	{stop, normal, ok, Monitor};

handle_call({addValue,{Station,Date,Type,Value}},_From, Monitor) ->
	NewMonitor = pollution:addValue(Station,Date,Type,Value, Monitor),
			if 
				is_tuple(NewMonitor) ->
					io:format("~p~n", [NewMonitor]),
					{reply, {Station,Date,Type,Value}, Monitor};
				true->
					io:format("Monitor after adding new Station : ~p~n",[NewMonitor]),
					{reply, {Station,Date,Type,Value}, NewMonitor}
			end;

handle_call({removeValue,{Station, Date,Type}}, _From, Monitor) ->
	NewMonitor = pollution:removeValue(Station, Date,Type, Monitor),
			if 
				is_tuple(NewMonitor) ->
					io:format("~p~n", [NewMonitor]),
					{reply,{Station, Date,Type}, Monitor};
				true->
					io:format("Monitor after adding new Station : ~p~n",[NewMonitor]),
					{reply, {Station, Date,Type}, NewMonitor}
			end;

handle_call({getOneValue,{Station, Type,Date}}, _From, Monitor) ->
	Result = pollution:getOneValue(Station, Type,Date, Monitor),
			if 
				is_tuple(Result) ->
					io:format("~p~n", [Result]),	
					{reply,Result, Monitor};
				true->
					io:format("Answer is : ~w~n",[Result]),
					{reply,Result, Monitor}
			end;

handle_call({getStationMean,{Station, Type}}, _From, Monitor) ->

	Result = pollution:getStationMean(Station, Type, Monitor),
			if 
				is_tuple(Result) ->
					io:format("~p~n", [Result]),	
					{reply,Result, Monitor};
				true->
					io:format("Answer is : ~w~n",[Result]),
					{reply,Result, Monitor}
			end.

handle_cast(crash, Monitor)->
	1/0,
	{noreply,Monitor}.

terminate(Reason, Value) ->
	io:format("Server: exit with vможет alue ~p~n", [Value]),
	Reason.
	
	





