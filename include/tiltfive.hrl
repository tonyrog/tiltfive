%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%	  Tilt Five structs and constant
%%% @end
%%% Created : 11 Oct 2024 by Tony Rogvall <tony@rogvall.se>

-ifndef(__TILTFIVE_HRL__).

%% t5_gameboard_type()
-define(kT5_GameboardType_Nonem, 1).
-define(kT5_GameboardType_LE, 2).
-define(kT5_GameboardType_XE, 3).
-define(kT5_GameboardType_XE_Raised,4).

%% t5_param_sys()
-define(kT5_ParamSys_UTF8_Service_Version, 1).

%% \brief Non-zero if the control panel requires user interaction
%% (E.g. Important firmware update) - Integer, boolean
-define(kT5_ParamSys_Integer_CPL_AttRequired, 2).

%% t5_param_glasses()
-define(kT5_ParamGlasses_Float_IPD, 1).
-define(kT5_ParamGlasses_UTF8_FriendlyName, 6).

%% t5_graphics_api().
%% \brief No graphics API (for clients that don't send frames)
-define(kT5_GraphicsApi_None, 1).
%% \brief OpenGL
-define(kT5_GraphicsApi_GL, 2).
%% \brief Direct3D 11 (Windows Only)
-define(kT5_GraphicsApi_D3D11, 3).
%% \brief Vulkan
-define(kT5_GraphicsApi_Vulkan, 4).

%% t5_wand_stream_event_type()
%% \brief Wand connected
-define(kT5_WandStreamEventType_Connect, 1).
%% \brief Wand disconnected
-define(kT5_WandStreamEventType_Disconnect, 2).
%% \brief Stream has desynchronized
-define(kT5_WandStreamEventType_Desync, 3).
%% \brief Wand report (Pose, Buttons, Trigger, Stick, Battery)
-define(kT5_WandStreamEventType_Report, 4).
 
%% t5_hand()
%% \brief Hand unknown
-define(kT5_Hand_Unknownm, 0).
%% \brief Left hand
-define(kT5_Hand_Left, 1).
%% \brief Right hand
-define(kT5_Hand_Right, 2).

%% t5_matrix_order()
-define(kT5_MatrixOrder_RowMajor,  1).
-define(kT5_MatrixOrder_ColumnMajor, 2).

%% t5_depth_range()
-define(kT5_DepthRange_MinusOneToOne, 1).
-define(kT5_DepthRange_ZeroToOne, 2).

%% t5_connection_state()
%% \brief Glasses are connected for exclusive use
-define(kT5_ConnectionState_ExclusiveConnection, 1).
%% \brief Glasses are reserved for exclusive use
-define(kT5_ConnectionState_ExclusiveReservation, 2).
%% \brief Glasses have not been exclusively connected or reserved
-define(kT5_ConnectionState_NotExclusivelyConnected, 3).
%% \brief Glasses were previously exclusively connected, but the device has disconnected
-define(kT5_ConnectionState_Disconnected,  4).

-define(kT5_GlassesPoseUsage_GlassesPresentation, 1).
-define(kT5_GlassesPoseUsage_SpectatorPresentation, 2).

%% add if we implement multi-context for some reason :-)
%% -type t5_context() :: reference().  

-type t5_glasses() :: reference().
-type t5_param_sys() :: integer().
-type t5_gameboard_type() :: integer().
-type t5_connection_state() :: integer().
-type t5_glasses_pose() :: term().
-type t5_glasses_pose_usage() :: integer().
-type t5_graphics_api() :: integer().
%% -type t5_graphics_context() :: reference().
%% -type t5_image() :: term().
-type t5_frame_info() :: term().
-type t5_wand_handle() :: reference().
-type t5_param_glasses() :: integer().
-type t5_depth_range() :: integer().
-type t5_matrix_order() :: integer().
-type t5_wand_stream_config() :: term().
-type t5_cam_image() :: term().
-type t5_camera_stream_config() :: term().
-type t5_cartesian_coordinate_handedness() :: term().
-type t5_wand_stream_event_type() :: integer().
-type t5_hand() :: integer().
-type t5_handle() :: term().
-type uint8_t() :: byte().

-record(t5_gameboard_size,
	{
	 %%  \brief The distance in meters from the gameboard origin to the edge of the viewable area in
	 %% the positive X direction.
	 viewableExtentPositiveX :: float(),
	 %% \brief The distance in meters from the gameboard origin to the edge of the viewable area in
	 %% the negative X direction.
	 viewableExtentNegativeX :: float(),

	 %% \brief The distance in meters from the gameboard origin to the edge of the viewable area in
	 %% the positive Y direction.
	 viewableExtentPositiveY :: float(),

	 %% \brief The distance in meters from the gameboard origin to the edge of the viewable area in
	 %% the negative Y direction.
	 viewableExtentNegativeY :: float(),

	 %% \brief The distance in meters above the gameboard origin that the viewable area extends in
	 %% the positive Z direction.
	 viewableExtentPositiveZ :: float()
	}).
-type t5_gameboard_size() :: #t5_gameboard_size{}.

-record(t5_projection_info,
	{
	 matrix :: tuple(),
	 fieldOfView :: float(),
	 aspectRatio :: float(),
	 framebufferWidth :: integer(),
	 framebufferHeight :: integer()
	}).
-type t5_projection_info() :: #t5_projection_info{}.
-record(t5_wand_buttons,
	{
	 t5 :: boolean(),
	 one :: boolean(),
	 two :: boolean(),
	 three :: boolean(),
	 a :: boolean(),
	 b :: boolean(),
	 x :: boolean(),
	 y :: boolean()
	}).

-type t5_vec2() :: {X::float(), Y::float()}.
-type t5_vec3() :: {X::float(), Y::float(),Z::float()}.
-type t5_quat() :: {W::float(),X::float(), Y::float(),Z::float()}.

-record(t5_wand_report,
	{
	 timestampNanos :: integer(),
	 analogValid :: boolean(),
	 batteryValid :: boolean(),
	 buttonsValid :: boolean(),
	 poseValid  :: boolean(),
	 trigger  :: float(),
	 stick :: t5_vec2(),
	 battery :: byte(), %% 0..255,
	 buttons :: #t5_wand_buttons{},
	 rotToWND_GBD :: t5_quat(),
	 posAim_GBD :: t5_vec3(),
	 posFingertips_GBD :: t5_vec3(),
	 posGrip_GBD :: t5_vec3(),
	 hand :: t5_hand()
	}).
-type t5_wand_report() :: #t5_wand_report{}.

-record(t5_wand_stream_event,
	{
	 wandId :: t5_wand_handle(),
	 type   :: t5_wand_stream_event_type(),
	 timestampNanos :: integer(),
	 report :: t5_wand_report()
	}).
-type t5_wand_stream_event() :: #t5_wand_stream_event{}.


-record(t5_glasses_pose,
	{
	 %% \brief The timestamp of the pose.
	 timestampNanos :: integer(),
	 %% \brief The position of the origin of the GLS (glasses) frame relative to the GBD (gameboard)
	 %% frame.
	 posGLS_GBD :: t5_vec3(),

	 %% \brief The rotation that transforms points in the GBD (gameboard) frame orientation to the
	 %% GLS (glasses) frame orientation.
	 rotToGLS_GBD :: t5_quat(),

	 %% \brief The type of gameboard visible for this pose
	 gameboardType :: t5_gameboard_type()
	}).
-type t5_glasses_pos() :: #t5_glasses_pose {}.
-endif.

