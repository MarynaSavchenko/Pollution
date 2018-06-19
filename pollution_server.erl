%% @author marina
%% @doc @todo Add description to pollution_server.


-module(pollution_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,stop/0,init/0,loop/1,addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2,crash/0,loop_sup/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================



init()->
	Monitor = pollution:createMonitor(),
	loop(Monitor).

stop() ->
	server ! stop.

loop(Monitor)->
	receive		
		{addStation,{Name,Coordinate}} ->
			NewMonitor = pollution:addStation(Name, Coordinate, Monitor),
			if 
				is_tuple(NewMonitor) ->
					io:format("~p~n", [NewMonitor]),
					loop(Monitor);
				true ->
					io:format("Monitor after adding new Station : ~p~n",[NewMonitor]),
					loop(NewMonitor)
			end;

		{addValue,{Station,Date,Type,Value}} ->
			NewMonitor = pollution:addValue(Station,Date,Type,Value, Monitor),
			if 
				is_tuple(NewMonitor) ->
					io:format("~p~n", [NewMonitor]),
					loop(Monitor);
				true->
					io:format("Monitor after adding new Station : ~p~n",[NewMonitor]),
					loop(NewMonitor)
			end;

			
		{removeValue,{Station, Date,Type}} ->
			NewMonitor = pollution:removeValue(Station, Date,Type, Monitor),
			if 
				is_tuple(NewMonitor) ->
					io:format("~p~n", [NewMonitor]),
					loop(Monitor);
				true->
					io:format("Monitor after adding new Station : ~p~n",[NewMonitor]),
					loop(NewMonitor)
			end;


		{getOneValue,{Station, Type,Date}} ->
			Result = pollution:getOneValue(Station, Type,Date, Monitor),
			if 
				is_tuple(Result) ->
					io:format("~p~n", [Result]);				
				true->
					io:format("Answer is : ~w~n",[Result])
			end,
			loop(Monitor);
	
		{getStationMean,{Station, Type}} ->
			Result = pollution:getStationMean(Station, Type, Monitor),
			if 
				is_tuple(Result) ->
					io:format("~p~n", [Result]);				
				true->
					io:format("Answer is : ~w~n",[Result])
			end,
			loop(Monitor);
		
		stop->
			ok;
		
		crash->
			1/0
	end.
			
start() ->

	sup,spawn(?MODULE, loop_sup, []).	
	
loop_sup() ->
	process_flag(trap_exit, true),
	register(server,spawn_link(?MODULE, init, [])),
	receive
		 {'EXIT', _, _}->
		 loop_sup()
	end.

		

addStation(Name,Coordinate) ->
	server ! {addStation,{Name,Coordinate}}.

addValue(Station,Date,Type,Value) ->
	server ! {addValue,{Station,Date,Type,Value}}.

removeValue(Station, Date,Type) ->
	server ! {removeValue,{Station, Date,Type}}.	

getOneValue(Station, Type,Date) ->
	server ! {getOneValue,{Station, Type,Date}}.	

getStationMean(Station,Type) ->
		server ! {getStationMean,{Station, Type}}.

crash() -> 		
		server ! crash.




<<<<<<< HEAD
=======

>>>>>>> abc07e5a82afd90e2611ab482b2be15d6790715a
