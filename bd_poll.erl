%% @author marina
%% @doc @todo Add description to bd_poll.

-module(bd_poll).
-record(station, {name, coor}).
-record(measure, {id, type, date, state, stat_name}).
-compile({parse_transform,qlc}).


-export([init/0, add_station/2, get_station/1, add_value/4, print_table/1, print_list/1, start/0, get_value/3, delete_value/3, get_station_mean/2, count_mean/3]).

init() ->
	
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(station,
        [ {disc_copies, [node()] },
             {attributes,      
                record_info(fields,station)} ]),
	mnesia:create_table(measure,
        [ {disc_copies, [node()] },
             {attributes,      
                record_info(fields,measure)} ]).


 
add_station( Name, {A, B}) ->
	
	Insert = 
        fun() -> 
	mnesia:write( 
	#station{ name = Name,
                   coor = {A, B}   } )
        end, 
	
	Station = get_station({A,B}),
	case Station of
		[] -> Station2 = get_station(Name),
			  
			  case Station2 of
				  [] -> {atomic, Results} = mnesia:transaction(Insert), 
						Results;
				  _ -> io:format("Station with such name already exists \n")
			  end;
		
		_  -> io:format("Station with such coordinates already exists \n")
			 
	end.
     
	


get_station({A,B}) ->
	
    P = fun() ->
            mnesia:match_object({station, '_' , {A,B}} )
        end,
	
    {atomic, Results} = mnesia:transaction(P),
    Results;



get_station(Name) ->
	
    Station = fun() ->
	            mnesia:read({station, Name})
	          end,
	
	{atomic, Result} = mnesia:transaction(Station),
	Result.


add_value(Type, State, Date, Station_id) ->	  
	
	Station = get_station(Station_id),
	
	case Station of	
		 []                 -> io:format("Station with such id doesnt exist\n");
		
		 [{station, Name, _}] -> Value = get_value(Type, Date, Name),
								 case Value of
									 [] -> {atomic, Results} = mnesia:transaction(fun() -> 
															mnesia:write( 
															#measure{ id = erlang:now(),
																	  type = Type,
																	  state = State, 
																	  date = Date, 
																	  stat_name = Name
																	  } )
											        		end),
														Results; 
									 _ -> io:format("The same measures already exist\n")
								 end
	end.



get_value(Type, Date, Station_id) ->
	
	Station = get_station(Station_id),
		
	case Station of	
		 []                   -> io:format("Station with such id doesnt exist\n");
		
		 [{station, Name, _}] -> {atomic, Results} = mnesia:transaction(fun() ->
												            mnesia:match_object({measure, '_' , Type, Date, '_', Name})
												        end),
    												Results
	end.
	
    

delete_value(Type, Date, Station_id) ->
	
	Value = get_value(Type, Date, Station_id),
	case Value of
		
		[]                           -> io:format("There are no such measures!\n");
		
		[{measure, Key, _, _, _, _}] -> {atomic, Results} = mnesia:transaction(fun() -> 
															mnesia:delete({measure, Key})		
															end),
															Results
								 
	end.

get_station_mean(Station_id, Type)->
	
	Station = get_station(Station_id),
	case Station of
		[] -> io:format("There is no such station!\n");
		[{station, Name, _}] -> {atomic, Results} = mnesia:transaction(fun() ->
												            mnesia:match_object({measure, '_' , Type, '_', '_', Name})
												        end),
    												count_mean(Results,0,0)
	end.

count_mean([],Sum, Counter) -> Sum/Counter;
count_mean([{_,_,_,_,Mean,_}|T],Sum, Counter)->count_mean(T,Sum+Mean, Counter+1).


print_table(Table) -> 
    
    {atomic, Results} = mnesia:transaction(
	  fun() ->
        P = qlc:e( qlc:q(
            [ X || X <- mnesia:table(Table) ] 
        )),
		print_list(P)
    end),
	Results.
	
 
print_list([]) -> {ok};
print_list([H|T]) -> io:format(" ~p ~n", [H]),
					print_list(T).

start() ->

	%%clean data
	mnesia:delete_table(station), 	
	mnesia:delete_table(measure),
	
	%%create BD with tables
	bd_poll:init(),

	%%add good stations
	bd_poll:add_station("first", {1,1}),
	bd_poll:add_station("second", {2,2}),
	
	%%add wrong stations
	io:format("\n\nTry to add wrong stations:\n"),
	bd_poll:add_station("first", {11,11}),
	bd_poll:add_station("try_second", {2,2}),
	
	%%add good values
	bd_poll:add_value(mp34, 56, "2018-12-10", "first"),
	bd_poll:add_value(mp34, 78, "2018-12-11", "first"),
	bd_poll:add_value(mp34, 54, "2018-12-12", {1,1}),
	bd_poll:add_value(mp34, 56, "2018-12-10", "second"),
	bd_poll:add_value(mp34, 55, "2018-12-11", "second"),
	bd_poll:add_value(mp34, 54, "2018-12-12", {2,2}),
	
	%%add wrong values
	io:format("\n\nTry to add wrong values:\n"),
	bd_poll:add_value(mp34, 24, "2018-12-11", "second"),
	bd_poll:add_value(mp34, 78, "2018-12-12", {1,1}),
	
	%%delete measures
	bd_poll:delete_value(mp34, "2018-12-11", "second"),
	
	%%delete wrong measures
	io:format("\n\nTry to delete wrong measures:\n "),
	bd_poll:delete_value(mp12, "2018-12-11", "second"),
	
	
	%%print tables
	io:format("\nPrint table-station:\n"),
	bd_poll:print_table(station),
	
	io:format("\nPrint table-measure:\n"),
	bd_poll:print_table(measure),
	
	%get station mean
	io:format("\n\nAVG for station: 'second' for pollution: mp34: "),
	bd_poll:get_station_mean("second", mp34 ).
	
	

	
    

