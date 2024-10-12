%% ported from Native/sample.c
%%
-module(tiltfive_test).

-export([start/0]).
-export([wait_for_service/0]).
-export([handle_service/0]).

-include("../include/tiltfive.hrl").

start() ->
    case tiltfive:get_gameboard_size(?kT5_GameboardType_XE_Raised) of
	{error, Reason} ->
	    io:format("Failed to get gameboard size: ~p\n", [Reason]);
	GameBoardSize ->
	    io:format("Gameboard size: ~p\n", [GameBoardSize])
    end,
    wait_for_service().

wait_for_service() ->
    case tiltfive:get_system_utf8_param(?kT5_ParamSys_UTF8_Service_Version) of
	{error, Reason} ->
	    io:format("Failed to get service version: ~p\n", [Reason]),
	    timer:sleep(1000),
	    wait_for_service();
	Version ->
	    io:format("Service version: ~p\n", [Version]),
	    handle_service()
    end.

handle_service() ->
    case tiltfive:list_glasses() of
	{error, Reason} ->
	    io:format("Failed to list glasses: ~p\n", [Reason]),
	    timer:sleep(1000),
	    wait_for_service();
	[] ->
	    io:format("No glasses found\n"),
	    timer:sleep(1000),
	    handle_service();
	GlassesList ->
	    handle_glasses(GlassesList)
    end.

handle_glasses(GlassesList0=[Glasses|GlassesList]) ->
    io:format("Glasses: ~p\n", [GlassesList0]),
    case tiltfive:create_glasses(Glasses) of
	GlassesRef when is_reference(GlassesRef) ->
	    io:format("Glasses created: ~p\n", [GlassesRef]),
	    case tiltfive:reserve_glasses(GlassesRef, "Hello") of
		ok ->
		    case tiltfive:ensure_glasses_ready(GlassesRef) of
			ok ->
			    io:format("Glasses ready\n"),
			    check_connection(GlassesRef),
			    process_glasses(GlassesRef);
			{error, Reason} ->
			    io:format("Failed to ensure glasses ready: ~p\n", [Reason])
		    end;
		{error,Reason} ->
		    io:format("Failed to set glasses display name: ~p\n", [Reason])
	    end;
	{error, Reason} ->
	    io:format("Failed to create glasses: ~p\n", [Reason]),
	    handle_glasses(GlassesList)
    end.


process_glasses(GlassesRef) ->
    process_glasses(GlassesRef, 100).

process_glasses(_GlassesRef, 0) ->
    ok;
process_glasses(GlassesRef, N) ->
    case tiltfive:get_glasses_pose(GlassesRef, ?kT5_GlassesPoseUsage_GlassesPresentation) of
	{error, Reason} ->
	    io:format("Failed to get glasses pose: ~p\n", [Reason]),
	    timer:sleep(1000),
	    process_glasses(GlassesRef, N-1);
	GlassesPose ->
	    io:format("Glasses pose: ~p\n", [GlassesPose]),
	    timer:sleep(100),
	    process_glasses(GlassesRef, N-1)
    end.

    
check_connection(GlassesRef) ->
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

