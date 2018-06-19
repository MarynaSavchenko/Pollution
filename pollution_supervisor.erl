%% @author marina
%% @doc @todo Add description to pollution_supervisor.


-module(pollution_supervisor).
-version('1.0').
-behaviour(supervisor).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1,init/1,start/0]).



%% ====================================================================
%% Internal functions
%% ====================================================================

start_link(_) ->
	supervisor:start_link({local, varSupervisor},?MODULE, []).

init(_) -> {ok, {{one_for_all, 2, 3},
[ {pollution_gen__server,
{pollution_gen_server, start, []},
permanent, brutal_kill, worker, [pollution_gen_server]}
]}
}.

start() ->
	start_link([]).


<<<<<<< HEAD
=======

>>>>>>> abc07e5a82afd90e2611ab482b2be15d6790715a
