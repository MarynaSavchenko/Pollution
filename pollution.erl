%% @author marina

-module(pollution).
-export ([createMonitor/0,addStation/3,getStation/3,addValue/5,addValueName/5,addValueCoo/5,removeValue2/4,removeValue/4,removeMeasure/3,getOneValue/4,getValue/3,getStationMean/3,getMean/4,importFromJson/3]).
-record(coordinate, {x,y}).
-record(measure, {data, type, state}).
-record(station, {name, coordinate , measures = []}).

createMonitor() -> [].

addStation(Name, {A,B}, P) ->
	case getStation(Name, {A,B}, P) of
	 	false -> [#station{name = Name, coordinate = #coordinate{x = A,y = B}}|P];
		_ -> {error, "This station is created already!"}
	end.
	

getStation(_,_,[]) -> 
	false;
getStation(_, {A,B}, [#station{coordinate = #coordinate{x = A,y = B}}|_]) -> 
	true;
getStation(Name, _, [#station{name = Name}|_]) -> 
	true;
getStation(_, _, [_|[]]) -> 
	false;
getStation(Name, {A,B}, [_|T]) ->
	getStation(Name, {A,B}, T);
getStation({A,B}, _, [_|T]) -> 
	getStation({A,B}, [], T);
getStation(Name, _, [_|T]) -> 
	getStation(Name, [], T).



addValue({A,B},Date,Type, State, P) ->
	case getStation([], {A,B}, P) of 
		false -> {error, "The station with such name doesnt exist!"};
		true -> addValueCoo({A,B},Date,Type, State, P)
	end;
addValue(Name,Date,Type, State, P) ->
	case getStation(Name, [], P) of 
		false -> {error, "The station with such name doesnt exist!"};
		true -> addValueName(Name,Date,Type, State, P)
	end.

addValueName(Name, Date,Type,State,[H|T]) when H#station.name == Name ->
	[H#station{measures = [#measure{data = Date,type = Type,state = State}|H#station.measures]}|T];
addValueName(Name, Date,Type,State,[H|[]]) when H#station.name == Name ->
	[H#station{measures = [#measure{data = Date,type = Type,state = State}|H#station.measures]}];
addValueName(Name, Date,Type,State,[H|T]) ->
	[H|addValueName(Name, Date,Type,State,T)].


addValueCoo({A,B}, Date,Type,State,[H|T]) when (H#station.coordinate#coordinate.x == A) and (H#station.coordinate#coordinate.y == B) ->
	[H#station{measures = [#measure{data = Date,type = Type,state = State}|H#station.measures]}|T];
addValueCoo({A,B}, Date,Type,State,[H|[]]) when (H#station.coordinate#coordinate.x == A) and (H#station.coordinate#coordinate.y == B) ->
	[H#station{measures = [#measure{data = Date,type = Type,state = State}|H#station.measures]}];
addValueCoo({A,B}, Date,Type,State,[H|T]) ->
	[H|addValueCoo({A,B}, Date,Type,State,T)].

removeValue({A,B}, Date, Type, P) ->
	case getStation([],{A,B}, P) of
		false -> {error, "No station with such coo"};
		_ -> removeValue2({A,B}, Date, Type, P)
	end;
removeValue(Name, Date, Type, P) ->
	case getStation(Name, [], P) of
		false -> {error, "No station with such name"};
		_ -> removeValue2(Name, Date, Type, P)
	end.


removeValue2({A,B}, Date, Type, [H|T]) when (H#station.coordinate#coordinate.x == A) and (H#station.coordinate#coordinate.y == B) ->
	[H#station{measures=removeMeasure(H#station.measures,Date,Type)}|T];
removeValue2({A,B}, Date, Type, [H|[]]) when (H#station.coordinate#coordinate.x == A) and (H#station.coordinate#coordinate.y == B) ->
	[H#station{measures=removeMeasure(H#station.measures,Date,Type)}];
removeValue2({A,B}, Date, Type, [H|T])  ->
	[H|removeValue2({A,B}, Date, Type, T)];
removeValue2(Name, Date, Type, [H|T]) when H#station.name == Name ->
	[H#station{measures=removeMeasure(H#station.measures,Date,Type)}|T];
removeValue2(Name, Date, Type, [H|[]]) when H#station.name == Name ->
	[H#station{measures=removeMeasure(H#station.measures,Date,Type)}];
removeValue2(Name, Date, Type, [H|T])  ->
	[H|removeValue2(Name, Date, Type, T)].

removeMeasure([],_,_)->
	[];
removeMeasure([H|T],Date,Type) when (H#measure.data == Date) and (H#measure.type == Type) ->
	[T];
removeMeasure([H|[]],Date,Type) when (H#measure.data == Date) and (H#measure.type == Type) -> 
	[];
removeMeasure([H|[]],_,_)  -> 
	[H];
removeMeasure([H|T],Date,Type) ->
	[H|removeMeasure(T,Date,Type)].



getOneValue({A,B},Type,Date, [H|_]) when (H#station.coordinate#coordinate.x == A) and (H#station.coordinate#coordinate.y == B) -> 
	getValue(H#station.measures, Type,Date);	
getOneValue({A,B},Type,Date, [H|[]]) when (H#station.coordinate#coordinate.x == A) and (H#station.coordinate#coordinate.y == B) -> 
	getValue(H#station.measures, Type,Date);
getOneValue({_,_},_,_, [_|[]]) ->
	{error,"There is not such station "};
getOneValue({A,B},Type,Date, [_|T]) ->
	getOneValue({A,B},Type,Date, T);

getOneValue(Name,Type,Date, [H|_]) when H#station.name == Name ->
	getValue(H#station.measures, Type,Date);
getOneValue(Name,Type,Date, [H|[]]) when H#station.name == Name -> 
	getValue(H#station.measures, Type,Date);
getOneValue(_,_,_, [_|[]]) ->
	{error,"There is not such station "};
getOneValue(Name,Type,Date, [_|T]) ->
	getOneValue(Name,Type,Date, T).


getValue([],_,_)->
	{error,"There is no such date and type at this station "};
getValue([H|_],Type, Date) when (H#measure.data == Date) and (H#measure.type == Type) ->
	H#measure.state;
getValue([H|[]],Type, Date) when (H#measure.data == Date) and (H#measure.type == Type) ->
	H#measure.state;
getValue([_|[]],_,_) ->
	{error,"There is no such date and type at this station "};
getValue([_|T],Type, Date) ->
	getValue(T,Type, Date).


getStationMean({A,B},Type, [H|_]) when (H#station.coordinate#coordinate.x == A) and (H#station.coordinate#coordinate.y == B) -> 
	getMean(H#station.measures, Type,0,0);	
getStationMean({A,B},Type, [H|[]]) when (H#station.coordinate#coordinate.x == A) and (H#station.coordinate#coordinate.y == B) -> 
	getMean(H#station.measures, Type,0,0);
getStationMean({_,_},_, [_|[]]) ->
	{error,"There is not such station "};
getStationMean({A,B},Type, [_|T]) ->
	getStationMean({A,B},Type, T);

getStationMean(Name,Type, [H|_]) when H#station.name == Name ->
	getMean(H#station.measures, Type,0,0);
getStationMean(Name,Type, [H|[]]) when H#station.name == Name -> 
	getMean(H#station.measures, Type,0,0);
getStationMean(_,_, [_|[]]) ->
	{error,"There is not such station "};
getStationMean(Name,Type,[_|T]) ->
	getStationMean(Name,Type, T).


getMean([H|[]],Type,Sum,Num) when (H#measure.type == Type) ->
	(H#measure.state+Sum)/(Num+1);
getMean([H|T],Type,Sum, Num) when H#measure.type == Type ->
	getMean(T,Type,Sum + H#measure.state,Num+1) ;
getMean([_|[]],_,Sum,Num) ->
	Sum/Num;
getMean([_|T],Type,Sum, Num) ->
	getMean(T,Type,Sum,Num).


	
importFromJson(A,JSON,P)->
	JS = mochijson2:decode(JSON),
	{struct, JsonData} = JS,
	Name = proplists:get_value(<<"Name">>, JsonData),
	X = proplists:get_value(<<"X">>, JsonData),
	Y = proplists:get_value(<<"Y">>, JsonData),
	Date = proplists:get_value(<<"Date">>, JsonData),
	Type = proplists:get_value(<<"Type">>, JsonData),
	State= proplists:get_value(<<"State">>, JsonData),	
	case A of
		addStation -> addStation(Name,{X,Y},P);
		addValue ->
			case Name of
				undefined -> addValue({X,Y},Date,Type,State,P);
				_ -> addValue(Name,Date,Type,State,P)
			end;
		
		removeValue ->
			case Name of
				undefined -> removeValue({X,Y},Date,Type,P);
				_ -> removeValue(Name,Date,Type,P)
			end;
		getOneValue ->
			case Name of
				undefined -> getOneValue({X,Y},Type,Date,P);
				_ -> getOneValue(Name,Type,Date,P)
			end;
		getStationMean ->
			case Name of
				undefined -> getStationMean({X,Y},Type,P);
				_ -> getStationMean(Name,Type,P)
			end
		end.
			
	




