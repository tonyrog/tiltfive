%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%      Tilt Five api
%%%
%%% Right now erlang must be started like
%%%   LD_PRELOAD=$TILT_FIVE_SDK/lib/linux/x86_64/libTiltFiveNative.so erl
%%%
%%% @end
%%% Created : 10 Oct 2024 by Tony Rogvall <tony@rogvall.se>

-module(tiltfive).

-on_load(init/0).

-export([new_wx_ref/0]).
-export([list_glasses/0]).
-export([create_glasses/1]).
-export([get_system_integer_param/1]).
-export([get_system_float_param/1]).
-export([get_system_utf8_param/1]).
-export([get_changed_system_params/0]).
-export([get_gameboard_size/1]).
-export([reserve_glasses/2]).
-export([set_glasses_display_name/2]).
-export([ensure_glasses_ready/1]).
-export([get_glasses_connection_state/1]).
-export([get_glasses_identifier/1]).
-export([get_glasses_pose/2]).
-export([init_glasses_graphics_context/3]).
-export([enqueue_init_glasses_graphics_context/3]).
-export([send_frame_to_glasses/2]).
-export([configure_camera_stream_for_glasses/2]).
-export([get_filled_cam_image_buffer/1]).
-export([submit_empty_cam_image_buffer/2]).
-export([cancel_cam_image_buffer/2]).
-export([validate_frameinfo/2]).
-export([get_glasses_integer_param/3]).
-export([get_glasses_float_param/3]).
-export([get_glasses_utf8_param/3]).
-export([get_changed_glasses_params/1]).
-export([get_projection/7]).
-export([list_wands_for_glasses/1]).
-export([send_impulse/4]).
-export([configure_wand_stream_for_glasses/2]).
-export([read_wand_stream_for_glasses/2]).

-export([enqueue_frame_to_glasses/2]).	

%% -compile(export_all).
-include("../include/tiltfive.hrl").

-define(nif_stub,nif_stub_error(?LINE)).

%% -define(dbg(F,A), io:format((F), (A))).
-define(dbg(F,A), ok).

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(?MODULE), tiltfive_nif), none).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-spec new_wx_ref() -> reference().
new_wx_ref() ->
    ?nif_stub.

-spec list_glasses() -> [string()].
list_glasses() ->
    ?nif_stub.

-spec create_glasses(Id::string()) -> t5_glasses().
create_glasses(_Id) ->
    ?nif_stub.

-spec get_system_integer_param(Param::t5_param_sys()) ->
	  integer().
get_system_integer_param(_Param) ->
    ?nif_stub.

-spec get_system_float_param(Param::t5_param_sys()) ->
	  integer().
get_system_float_param(_Param) ->
    ?nif_stub.

-spec get_system_utf8_param(Param::t5_param_sys()) ->
	  string().
get_system_utf8_param(_Param) ->
    ?nif_stub.

-spec get_changed_system_params() ->
	  [t5_param_sys()].
get_changed_system_params() ->
    ?nif_stub.

-spec get_gameboard_size(GameboardType::t5_gameboard_type()) ->
	  t5_gameboard_size().
get_gameboard_size(_GameboardType) ->
    ?nif_stub.

-spec reserve_glasses(Glasses::t5_glasses(), DisplayName::string()) -> ok.
reserve_glasses(_Glasses, _DisplayName) -> 
    ?nif_stub.

-spec set_glasses_display_name(Glasses::t5_glasses(), DisplayName::string()) ->
	  ok.
set_glasses_display_name(_Glasses, _DisplayName) ->
    ?nif_stub.
    
-spec ensure_glasses_ready(Glasses::t5_glasses()) -> 
	  ok.
ensure_glasses_ready(_Glasses) ->
    ?nif_stub.

-spec get_glasses_connection_state(Glasses::t5_glasses()) -> 
	  ConnectionState::t5_connection_state().
get_glasses_connection_state(_Glasses) ->
    ?nif_stub.    

-spec get_glasses_identifier(Glasses::t5_glasses()) -> 
	  string().
get_glasses_identifier(_Glasses) ->
    ?nif_stub.

-spec get_glasses_pose(Glasses::t5_glasses(),Usage::t5_glasses_pose_usage()) ->
	  Pose::t5_glasses_pose().
get_glasses_pose(_Glasses, _Usage) ->
    ?nif_stub.

-spec init_glasses_graphics_context(Glasses::t5_glasses(),
				    GraphicsApi::t5_graphics_api(),
				    GraphicsContext::t5_graphics_context()
				   ) -> ok.
init_glasses_graphics_context(_Glasses, _GraphicsApi, _GraphicsContext) ->
	?nif_stub.

-spec enqueue_init_glasses_graphics_context(Glasses::t5_glasses(),
					    GraphicsApi::t5_graphics_api(),
					    GraphicsContext::t5_graphics_context()
					   ) -> ok.
enqueue_init_glasses_graphics_context(Glasses,
				      GraphicsApi,
				      GraphicsContext) ->
    wxe_util:queue_dyn_cmd(init_glasses_graphics_context,
			   {Glasses,GraphicsApi,GraphicsContext},
			   tiltfive, glasses, Glasses),
    wxe_util:rec_dyn().

-spec configure_camera_stream_for_glasses(Glasses::t5_glasses(),
					  Config::t5_camera_stream_config()) ->
	  ok.
configure_camera_stream_for_glasses(_Glasses, _Config) ->
	?nif_stub.
	  
-spec get_filled_cam_image_buffer(Glasses::t5_glasses()) ->
	  Image::t5_cam_image().
get_filled_cam_image_buffer(_Glasses) ->
	?nif_stub.

-spec submit_empty_cam_image_buffer(Glasses::t5_glasses(),
				    Image::t5_cam_image()) -> 
	  ok.
submit_empty_cam_image_buffer(_Glasses, _Image) ->
	?nif_stub.

-spec cancel_cam_image_buffer(Glasses::t5_glasses(), Buffer::t5_handle()) ->
	  ok.
cancel_cam_image_buffer(_Glasses, _Buffer) ->
	?nif_stub.

-spec send_frame_to_glasses(Glasses::t5_glasses(), Info::t5_frame_info()) ->
	  ok.
send_frame_to_glasses(_Glasses, _Info) ->
	?nif_stub.

-spec enqueue_frame_to_glasses(Glasses::t5_glasses(), Info::t5_frame_info()) ->
	  ok.
enqueue_frame_to_glasses(Glasses, Info) ->
    wxe_util:queue_dyn_cmd(send_frame_to_glasses, {Glasses, Info},
			   tiltfive, glasses, Glasses),
    wxe_util:rec_dyn().

-spec validate_frameinfo(Glasses::t5_glasses(), Info::t5_frame_info())
			-> ok | {error,Detail::string()}.
validate_frameinfo(_Glasses, _Info) ->
    ?nif_stub.

-spec get_glasses_integer_param(Glasses::t5_glasses(), 
				WandHandle::t5_wand_handle(),
				Param::t5_param_glasses()) -> 
	  Value::integer().
get_glasses_integer_param(_Glasses,  _WandHandle,  _Param) -> 
    ?nif_stub.

-spec get_glasses_float_param(Glasses::t5_glasses(), 
			      WandHandle::t5_wand_handle(),
			      Param::t5_param_glasses()) -> 
	  Value::float().
get_glasses_float_param(_Glasses, _WandHandle, _Param) -> 
    ?nif_stub.

-spec get_glasses_utf8_param(Glasses::t5_glasses(),
			     WandHandle::t5_wand_handle(),
			     Param::t5_param_glasses()) -> 
	  Value::string().
get_glasses_utf8_param(_Glasses, _WandHandle, _Param) ->
    ?nif_stub.

-spec get_changed_glasses_params(Glasses::t5_glasses()) ->
	  [t5_param_glasses()].
get_changed_glasses_params(_Glasses) ->
	?nif_stub.
				 
-spec get_projection(Glasses::t5_glasses(),
		     Handedness::t5_cartesian_coordinate_handedness(),
		     DepthRange::t5_depth_range(),
		     MatrixOrder::t5_matrix_order(),
		     NearPlane::float(),
		     FarPlane::float(), 
		     WorldScale::float()) ->
	  Info::t5_projection_info().
get_projection(_Glasses, _Handedness, _DepthRange, _MatrixOrder,
	       _NearPlane, _FarPlane, _WorldScale) ->
	?nif_stub.

-spec list_wands_for_glasses(Glasses::t5_glasses()) ->
	  [t5_wand_handle()].
list_wands_for_glasses(_Glasses) ->
    ?nif_stub.

-spec send_impulse(Glasses::t5_glasses(),Wand::t5_wand_handle(),
		   Amplitude::float(), Duration::integer()) -> ok.
send_impulse(_Glasses, _Wand, _Amplitude, _Duration) ->
	?nif_stub.

-spec configure_wand_stream_for_glasses(Glasses::t5_glasses(),
					Config::t5_wand_stream_config()) -> ok.
configure_wand_stream_for_glasses(_Glasses, _Config) ->
    ?nif_stub.

-spec read_wand_stream_for_glasses(Glasses::t5_glasses(),TimeoutMs::integer()) -> t5_wand_stream_event().
read_wand_stream_for_glasses(_Glasses, _TimeoutMs) ->
    ?nif_stub.
