%% ported from Native/sample.c
%%
-module(tiltfive_test).

-export([start/0, start/1]).
-export([wait_for_service/1]).

-include("../include/tiltfive.hrl").

-define(defaultWidth, 1216).
-define(defaultHeight, 768).
-define(defaultFOV, 48.0).

-define(radians(Deg), Deg * math:pi() / 180.0).

start() ->
    start(tiltfive_wx).

start(Mod) ->
    application:load(tiltfive),
    GameBoardSize =
	case application:get_env(tiltfive, gameboard_size, xe_raised) of
	    undefined -> ?kT5_GameboardType_None;
	    none -> ?kT5_GameboardType_None;
	    le -> ?kT5_GameboardType_LE;
	    xe -> ?kT5_GameboardType_XE;
	    xe_raised -> ?kT5_GameboardType_XE_Raised
	end,
    case tiltfive:get_gameboard_size(GameBoardSize) of
	{error, Reason} ->
	    io:format("Failed to get gameboard size: ~p\n", [Reason]),
	    {error, no_gameboard};
	T5GameBoardSize ->
	    io:format("Gameboard size: ~p\n", [GameBoardSize]),
	    init_shcedulers(),
	    wait_for_service(#{mod=>Mod, gameboard_size=>T5GameBoardSize})
    end.

wait_for_service(St) ->
    case tiltfive:get_system_utf8_param(?kT5_ParamSys_UTF8_Service_Version) of
	{error, Reason} ->
	    io:format("Failed to get service version: ~p\n", [Reason]),
	    timer:sleep(1000),
	    wait_for_service(St);
	Version ->
	    io:format("Service version: ~p\n", [Version]),
	    handle_service(St, [], #{})
    end.

%% scan for new glasses or removed glasses
handle_service(St, GlassesList0, Map) ->
    case tiltfive:list_glasses() of
	{error, Reason} ->
	    io:format("Failed to list glasses: ~p\n", [Reason]),
	    handle_service_(St, GlassesList0, Map);
	GlassesList1 ->
	    Added = GlassesList1 -- GlassesList0,
	    Removed = GlassesList0 -- GlassesList1,
	    Map1 = stop_glasses(St, Removed, Map),
	    Map2 = start_glasses(St, Added, Map1),
	    handle_service_(St, GlassesList1, Map2)
    end.

handle_service_(St, GlassesList, Map) ->
    receive
	{'DOWN', Mon, _, _, _} ->
	    case maps:get(Mon, Map, undefined) of
		undefined ->
		    handle_service(St, GlassesList, Map);
		Pid ->
		    Glasses = maps:get(Pid, Map),
		    io:format("~s: Glasses terminated\n", [Glasses]),
		    Map1 = maps:remove(Glasses, Map),
		    Map2 = maps:remove(Pid, Map1),
		    Map3 = maps:remove(Mon, Map2),
		    handle_service_(St, GlassesList--[Glasses],Map3)
	    end
    after 1000 ->
	    handle_service(St, GlassesList, Map)
    end.

start_glasses(_St, [], Map) -> Map;
start_glasses(St, [ID|Added], Map) ->
    Si = select_scheduler(Map),
    {Pid,Mon} = spawn_opt(fun() ->
				  init_glasses(St#{ glasses_id => ID})
			  end, [monitor, {scheduler, Si}]),
    Map1 = Map#{ ID => {Pid,Mon}, Mon => Pid, Pid => ID },
    start_glasses(St, Added, Map1).

stop_glasses(_St, [], Map) -> Map;
stop_glasses(St, [ID|Removed], Map) ->
    {Pid,_Mon} = maps:get(ID, Map),
    Pid ! stop,
    stop_glasses(St, Removed, maps:remove(ID, Map)).

sid() ->
    erlang:system_info(scheduler_id).

init_shcedulers() ->
    N = nprocs(),
    List = lists:seq(1, N) -- [sid()],
    io:format("Schedulers: ~p\n", [List]),
    put(schedulers, List).

%% round robin scheduler selection
select_scheduler(_Map) ->
    [Si|Schedulers] = get(schedulers),
    put(schedulers, Schedulers++[Si]),
    Si.

get_glasses_params(GlassesRef) ->
    FriendlyName = case tiltfive:get_glasses_utf8_param(GlassesRef, 0, ?kT5_ParamGlasses_UTF8_FriendlyName) of
		       {error, _} -> "NoName";
		       Name -> Name
		   end,
    IPD = case tiltfive:get_glasses_float_param(GlassesRef, 0, ?kT5_ParamGlasses_Float_IPD) of
	      {error, _} -> 0.059; %% m (59mm)
	      IPD0 -> IPD0
	  end,
    #{ friendly_name => FriendlyName, ipd => IPD }.


init_glasses(GSt=#{ glasses_id := ID}) ->

    case tiltfive:create_glasses(ID) of
	GlassesRef when is_reference(GlassesRef) ->
	    Params = get_glasses_params(GlassesRef),
	    io:format("~s: Params = ~p\n", [ID, Params]),
	    Names = application:get_env(tiltfive, names, #{}),
	    ApplicationName = application:get_env(tiltfive, application_name, 
						  "NoName"),
	    DisplayName = case maps:get(ID, Names, undefined) of
			      undefined -> 
				  maps:get(friendly_name, Params, ID);
			      Name -> Name
			  end,

	    io:format("~s: Init Glasses: name=~s, scheduler=~w\n", 
		      [ID, DisplayName, sid()]),
	    io:format("~s: Glasses created: ~p\n", [ID, GlassesRef]),
	    case tiltfive:reserve_glasses(GlassesRef, ApplicationName) of
		ok ->
		    ready_glasses(GSt#{ glasses_ref=>GlassesRef, 
					display_name=>DisplayName,
					application_name=>ApplicationName
				      });
		{error, already_connected} ->
		    ready_glasses(GSt#{ glasses_ref=>GlassesRef,
					display_name=>DisplayName,
					application_name=>ApplicationName
				      });
		{error,Reason} ->
		    io:format("~s: Failed to reserve glasses ~p\n", 
			      [ID, Reason])
	    end;
	{error, Reason} ->
	    io:format("~s: Failed to create glasses: ~p\n", [ID, Reason])
    end.

ready_glasses(GSt=#{ glasses_ref := GlassesRef, display_name := DisplayName}) ->
    case tiltfive:ensure_glasses_ready(GlassesRef) of
	ok ->
	    io:format("~s: ready\n", [DisplayName]),
	    check_wands(GSt);
	{error, try_again} ->
	    io:format("Glasses ~s not ready, try again\n", [DisplayName]),
	    timer:sleep(1000),
	    ready_glasses(GSt);
	{error, Reason} ->
	    io:format("Failed to ensure glasses ready: ~p\n", [Reason])
    end.

check_wands(GSt=#{ glasses_ref := GlassesRef, display_name := DisplayName}) ->
    case tiltfive:list_wands_for_glasses(GlassesRef) of
	{error, Reason} ->
	    io:format("Failed to list wands: ~p\n", [Reason]),
	    timer:sleep(1000),
	    check_wands(GSt);
	[] ->
	    timer:sleep(1000),
	    process_init(GSt#{ wands => [] });
	Wands = [_Wand|_] ->
	    io:format("~s: Wands: ~p\n", [DisplayName, Wands]),
	    R = tiltfive:configure_wand_stream_for_glasses(GlassesRef, 
							   #t5_wand_stream_config{enabled=true}),
	    io:format("~s: configure_wand_stream_for_glasses: ~p\n", 
		      [DisplayName, R]),
	    process_init(GSt# { wands => Wands })
    end.

process_init(GSt = #{ glasses_ref := GlassesRef }) ->
    GSt1 = GSt#{ width => ?T5_MIN_CAM_IMAGE_BUFFER_WIDTH,
		 height => ?T5_MIN_CAM_IMAGE_BUFFER_HEIGHT },
    GSt2 = call_user(GSt1, init, []),

%%    GLContext = #t5_graphics_context_gl{
%%		   textureMode = ?kT5_GraphicsApi_GL_TextureMode_Pair,
%%		   leftEyeArrayIndex = 0,
%%		   rightEyeArrayIndex = 0
%%		  },
%%    R = tiltfive:init_glasses_graphics_context(GlassesRef,
%%					       ?kT5_GraphicsApi_GL,
%%					       null),
      R = tiltfive:enqueue_init_glasses_graphics_context(GlassesRef,
							 ?kT5_GraphicsApi_GL,
							 null),
    io:format("Init glasses graphics context: ~p, SID=~w\n", [R, sid()]),
    process_glasses(GSt2).

%% alternate between processing glasses and wands
process_glasses(GSt=#{ glasses_ref := GlassesRef,
		       display_name := DisplayName }) ->
    case tiltfive:get_glasses_pose(GlassesRef,
				   ?kT5_GlassesPoseUsage_GlassesPresentation) of
	{error, not_connected} ->
	    io:format("Glasses ~s not connected\n", [DisplayName]),
	    _GSt1 = call_user(GSt, terminate, []),
	    ok;
	{error, try_again} ->
	    %% no pose go check for wands events
	    process_wands(GSt);
	{error, Reason} ->
	    io:format("Failed to get glasses pose: ~p\n", [Reason]),
	    process_wands(GSt);
	GlassesPose when is_record(GlassesPose, t5_glasses_pose) ->
	    GSt1 = call_user(GSt, pose, [GlassesPose]),
	    process_wands(GSt1)
    end.

%% fixme: flag if wands are needed or not?
process_wands(GSt = #{ wands := []}) ->
    GSt1 = call_user(GSt, run, []),
    send_frame(GSt1),
    timer:sleep(100),  %% fix this
    process_glasses(GSt1);
process_wands(GSt= #{ glasses_ref := GlassesRef,
		      display_name := DisplayName }) ->
    case tiltfive:read_wand_stream_for_glasses(GlassesRef, 10) of
	{error, not_connected} ->
	    io:format("Glasses ~s not connected\n", [DisplayName]),
	    _GSt1 = call_user(GSt, terminate, []),
	    ok;
	{error, try_again} ->
	    %% no wand event go check for glasses pose
	    GSt1 = call_user(GSt, run, []),
	    send_frame(GSt1),
	    timer:sleep(100),
	    process_glasses(GSt1);
	{error, Reason} ->
	    io:format("Failed to read wand stream: ~p\n", [Reason]),
	    timer:sleep(1000),
	    process_glasses(GSt);
	Event ->
	    io:format("Wand event: ~p\n", [Event]),
	    GSt1 = call_user(GSt, handle_event, [Event]),
	    timer:sleep(100),
	    process_wands(GSt1)
    end.

send_frame(_GSt= #{ glasses_ref := GlassesRef,
		    textures := [Tl,Tr]
		  }) ->
    Height = ?defaultHeight,
    Width  = ?defaultWidth,
    Y = -math:tan(?radians(?defaultFOV)) * 0.5,
    X = Y * Width/Height,
    VCI = #vci{ startY_VCI = Y,
		startX_VCI = X,
		width_VCI = -2 * X,
		height_VCI = -2 * Y
	      },

%%    leftPos = leftEyePose.TransformPointToParentFrame(glm::vec3(0,0,0));
%%    leftPos = headPose.TransformPointToParentFrame(leftPos);

%%    frameInfo.posLVC_GBD = T5W::toT5(leftPos);
%%    frameInfo.rotToLVC_GBD = T5W::toT5(headPose.GetOrientation());

%%    rightPos = rightEyePose.TransformPointToParentFrame(glm::vec3(0, 0, 0));
%%    rightPos = headPose.TransformPointToParentFrame(rightPos);

%%    frameInfo.posRVC_GBD = T5W::toT5(rightPos);
%%    frameInfo.rotToRVC_GBD = T5W::toT5(headPose.GetOrientation());

    Info = #t5_frame_info{leftTexHandle = Tl, rightTexHandle = Tr,
			  texWidth = Width, texHeight = Height,
			  isSrgb = false, isUpsideDown = false,
			  vci = VCI,
			  rotToLVC_GDB = #t5_quat{ x = 0.0, y = 0.0, z = 0.0, w = 1.0 },
			  posLVC_GDB = #t5_vec3{ x = 0.0, y = 0.0, z = 0.0 },
			  rotToRVC_GDB = #t5_quat{ x = 0.0, y = 0.0, z = 0.0,
						   w = 1.0 },
			  posRVC_GDB = #t5_vec3{ x = 0.0, y = 0.0, z = 0.0 }
			 },
    case tiltfive:validate_frameinfo(GlassesRef, Info) of
	ok ->
	    %% tiltfive:send_frame_to_glasses(GlassesRef, Info);
	    tiltfive:enqueue_frame_to_glasses(GlassesRef, Info);
	{error,Reason} ->
	    io:format("invalid frame ~p\n", [Reason])
    end.

call_user(GSt = #{ mod := undefined }, _Func, _Args) -> GSt;
call_user(GSt = #{ mod := Mod }, Func, Args) 
  when is_atom(Mod), is_atom(Func), is_list(Args) ->
    try apply(Mod, Func, [GSt|Args]) of
	GSt1 -> GSt1
    catch
	error:Reason:Stack ->
	    io:format("Failed to call ~p:~p(~p)\n", [Mod, Func, Args]),
	    io:format("Crash: reason=~p stack=~p\n", [Reason, Stack]),
	    GSt
    end.

-ifdef(not_used).
check_connection(GSt=#{ glasses_ref := GlassesRef }) ->
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
-endif.

%% number of schedulers online
nprocs() ->
	nprocs(infinity).    
nprocs(infinity) ->
    nprocs(16#ffffffff);
nprocs(0) -> 0;
nprocs(K) when is_integer(K), K > 0 ->
    case erlang:system_info(smp_support) of
	false -> 1;
	true ->
	    N = erlang:system_info(logical_processors),
	    if N =:= 1 -> 1;
	       true ->
		    M = erlang:system_info(schedulers_online),
		    min(K,min(N,M))
	    end
    end.
