%% ported from Native/sample.c
%%
-module(tiltfive_test).

-export([start/0]).
-export([wait_for_service/1]).

-include("../include/tiltfive.hrl").

glasses_names() ->
    #{
      "0P9T-0801-0AE8" => "Player 1",
      "0P9T-0801-0A9C" => "Player 2"
     }.

start() ->
    case tiltfive:get_gameboard_size(?kT5_GameboardType_XE_Raised) of
	{error, Reason} ->
	    io:format("Failed to get gameboard size: ~p\n", [Reason]),
	    wait_for_service(#t5_gameboard_size{});
	GameBoardSize ->
	    io:format("Gameboard size: ~p\n", [GameBoardSize]),
	    wait_for_service(GameBoardSize)
    end.


wait_for_service(GameBoardSize) ->
    case tiltfive:get_system_utf8_param(?kT5_ParamSys_UTF8_Service_Version) of
	{error, Reason} ->
	    io:format("Failed to get service version: ~p\n", [Reason]),
	    timer:sleep(1000),
	    wait_for_service(GameBoardSize);
	Version ->
	    io:format("Service version: ~p\n", [Version]),
	    handle_service([], GameBoardSize, #{})
    end.

%% scan for new glasses or removed glasses
handle_service(GlassesList0, GameBoardSize, Map) ->
    case tiltfive:list_glasses() of
	{error, Reason} ->
	    io:format("Failed to list glasses: ~p\n", [Reason]),
	    handle_service_(GlassesList0, GameBoardSize, Map);
	GlassesList1 ->
	    Added = GlassesList1 -- GlassesList0,
	    Removed = GlassesList0 -- GlassesList1,
	    Map1 = stop_glasses(Removed, Map),
	    Map2 = start_glasses(Added, Map1),
	    handle_service_(GlassesList1, GameBoardSize, Map2)
    end.

handle_service_(GlassesList, GameBoardSize, Map) ->
    receive
	{'DOWN', Mon, _, _, _} ->
	    case maps:get(Mon, Map, undefined) of
		undefined ->
		    handle_service(GlassesList, GameBoardSize, Map);
		Pid ->
		    Glasses = maps:get(Pid, Map),
		    io:format("~s: Glasses terminated\n", [Glasses]),
		    Map1 = maps:remove(Glasses, Map),
		    Map2 = maps:remove(Pid, Map1),
		    Map3 = maps:remove(Mon, Map2),
		    handle_service_(GlassesList--[Glasses], GameBoardSize, Map3)
	    end
    after 1000 ->
	    handle_service(GlassesList, GameBoardSize, Map)
    end.

start_glasses([], Map) -> Map;
start_glasses([Glasses|Added], Map) ->
    {Pid,Mon} = spawn_monitor(fun() ->
				init_glasses(Glasses)
			      end),
    Map1 = Map#{Glasses => {Pid,Mon}, Mon => Pid, Pid => Glasses },
    start_glasses(Added, maps:put(Glasses, Pid, Map1)).

stop_glasses([], Map) -> Map;
stop_glasses([Glasses|Removed], Map) -> 
    Pid = maps:get(Glasses, Map),
    Pid ! stop,
    stop_glasses(Removed, maps:remove(Glasses, Map)).

init_glasses(Glasses) ->
    DisplayName = maps:get(Glasses, glasses_names(), "NoName"),
    io:format("~s: Init Glasses: name=~s\n", [Glasses, DisplayName]),
    case tiltfive:create_glasses(Glasses) of
	GlassesRef when is_reference(GlassesRef) ->
	    io:format("~s: Glasses created: ~p\n", [Glasses, GlassesRef]),
	    case tiltfive:reserve_glasses(GlassesRef, DisplayName) of
		ok ->
		    ready_glasses(GlassesRef, DisplayName);
		{error, already_connected} ->
		    ready_glasses(GlassesRef, DisplayName);
		{error,Reason} ->
		    io:format("~s: Failed to reserve glasses ~p\n", 
			      [Glasses, Reason])
	    end;
	{error, Reason} ->
	    io:format("~s: Failed to create glasses: ~p\n", [Glasses, Reason])
    end.

ready_glasses(GlassesRef, DisplayName) ->
    case tiltfive:ensure_glasses_ready(GlassesRef) of
	ok ->
	    io:format("~s: ready\n", [DisplayName]),
	    check_wands(GlassesRef, DisplayName);
	{error, try_again} ->
	    io:format("~s: Glasses not ready, try again\n", [DisplayName]),
	    timer:sleep(1000),
	    ready_glasses(GlassesRef, DisplayName);
	{error, Reason} ->
	    io:format("Failed to ensure glasses ready: ~p\n", [Reason])
    end.

check_wands(GlassesRef, DisplayName) ->
    case tiltfive:list_wands_for_glasses(GlassesRef) of
	{error, Reason} ->
	    io:format("Failed to list wands: ~p\n", [Reason]),
	    timer:sleep(1000),
	    check_wands(GlassesRef, DisplayName);
	[] ->
	    timer:sleep(1000),
	    process_glasses(GlassesRef, DisplayName, []);
	Wands = [_Wand|_] ->
	    io:format("~s: Wands: ~p\n", [DisplayName, Wands]),
	    R = tiltfive:configure_wand_stream_for_glasses(GlassesRef, 
							    #t5_wand_stream_config{enabled=true}),
	    io:format("~s: configure_wand_stream_for_glasses: ~p\n", 
		      [DisplayName, R]),
	    process_glasses(GlassesRef, DisplayName, Wands)
    end.


process_glasses(GlassesRef, DisplayName, Wands) ->
    case tiltfive:get_glasses_pose(GlassesRef, 
				   ?kT5_GlassesPoseUsage_GlassesPresentation) of
	{error, not_connected} ->
	    io:format("Glasses not connected\n"),
	    ok;
	{error, try_again} ->
	    io:format("Glasses not ready, try again\n"),
	    if Wands =:= [] ->
		    check_wands(GlassesRef, DisplayName);
	       true ->
		    process_wands(GlassesRef, DisplayName, Wands)
	    end;
	{error, Reason} ->
	    io:format("Failed to get glasses pose: ~p\n", [Reason]),
	    process_wands(GlassesRef, DisplayName, Wands);
	GlassesPose ->
	    io:format("Glasses pose: ~p\n", [GlassesPose]),
	    process_wands(GlassesRef, DisplayName, Wands)
    end.

process_wands(GlassesRef, DisplayName, Wands) ->
    case tiltfive:read_wand_stream_for_glasses(GlassesRef, 100) of
	{error, not_connected} ->
	    io:format("Glasses not connected\n"),
	    ok;
	{error, try_again} ->
	    io:format("Glasses not ready, try again\n"),
	    timer:sleep(1000),
	    process_glasses(GlassesRef, DisplayName, Wands);
	{error, Reason} ->
	    io:format("Failed to read wand stream: ~p\n", [Reason]),
	    timer:sleep(1000),
	    process_glasses(GlassesRef, DisplayName, Wands);
	Event ->
	    io:format("Wand event: ~p\n", [Event]),
	    timer:sleep(100),
	    process_wands(GlassesRef, DisplayName, Wands)
    end.

check_connection(GlassesRef, _DisplayName) ->
    case tiltfive:get_glasses_connection_state(GlassesRef) of
	?kT5_ConnectionState_ExclusiveConnection ->
	    io:format("Glasses connected\n"),
	    true;
	?kT5_ConnectionState_ExclusiveReservation ->
	    io:format("Glasses reserved\n"),
	    true;
	?kT5_ConnectionState_NotExclusivelyConnected ->
	    io:format("Glasses not exclusively connected\n"),
	    true;
	?kT5_ConnectionState_Disconnected ->
	    io:format("Glasses disconnected\n"),
	    false;
	_ ->
	    io:format("Unknown connection state\n"),
	    false
    end.
