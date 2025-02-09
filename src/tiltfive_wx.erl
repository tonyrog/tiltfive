%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Test openGL with wxErlang
%%% @end
%%% Created : 14 Nov 2024 by Tony Rogvall <tony@rogvall.se>

-module(tiltfive_wx).

%%
%% Calls from tiltfive_test
%% 
-export([init/1]).
-export([terminate/1]).
-export([pose/2]).
-export([run/1]).
-export([handle_event/2]).

%% Test start
-export([start/0]).


-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 
%% -include_lib("epx/include/epx_image.hrl").
-include("../include/tiltfive.hrl").

-define(TEST, true).
-define(WIDTH,  512).
-define(HEIGHT, 512).

-define(defaultWidth, 1216).
-define(defaultHeight, 768).
-define(defaultFOV, 48.0).
-define(radians(Deg), (((Deg)*math:pi()) / 180.0)).

-include("transform.hrl").

-record(cube, 
	{
	 id,
	 framebuffer,
	 texture, 
	 png,
	 mvp
	}).

-type glasses_state() :: #{ wx => wx:wx_object(),
			    frame => wx:wx_object(),
			    canvas => wx:wx_object(),
			    context => wx:wx_object(),
			    framebuffers => [gl:gl_object()],
			    textures => [gl:gl_object()],
			    angle1 => float(),
			    step1 => float(),
			    angle2 => float(),
			    step2 => float(),
			    transform => transform()
			  }.

t5_vec_to_vecf(#t5_vec3{x=X,y=Y,z=Z}) ->
    {X,Y,Z}.

t5_quat_to_quatf(#t5_quat{w=W,x=X,y=Y,z=Z}) ->
    {{X,Y,Z}, W}.


init(GSt = #{ glasses_ref := Glasses }) ->
    io:format("tiltfive_wx:init: ~p~n", [GSt]),
    IPD = tiltfive:get_glasses_float_param(Glasses,0,
					   ?kT5_ParamGlasses_Float_IPD),
    GameboardPose = tiltfive_transform:new(),
    HeadPose = tiltfive_transform:new(),
    LeftEyePose = tiltfive_transform:set_position(
		    tiltfive_transform:new(), -IPD/2.0, 0.0, 0.0),
    RightEyePose = tiltfive_transform:set_position(
		     tiltfive_transform:new(), IPD/2.0, 0.0, 0.0),
    GSt1 = wx_init(GSt),
    GSt1#{ start_time => erlang:system_time(micro_seconds),
	   angle1 => 45.0, step1 => 2.0,
	   angle2 => 45.0, step2 => -2.0,
	   %% transforms needed for view matrix
	   gameboardPose => GameboardPose,
	   headPose => HeadPose,
	   leftEyePose => LeftEyePose, 
	   rightEyePose => RightEyePose
	 }.

terminate(GSt) ->
    io:format("tiltfive_wx:terminate: ~p~n", [GSt]),
    ok.
%%
%% Glasses pose updated
%% 
-spec pose(GSt::glasses_state(), Pose::t5_glasses_pose()) -> 
	  glasses_state().
pose(GSt, Pose) ->
    io:format("tiltfive_wx:pose: ~p pose=~p~n", [GSt,Pose]),
    Position = t5_vec_to_vecf(Pose#t5_glasses_pose.posGLS_GBD),
    Orientation = t5_quat_to_quatf(Pose#t5_glasses_pose.rotToGLS_GBD),
    #{ headPose := HeadPose } = GSt,
    HeadPos1 = HeadPose#transform{ position = Position,
				   orientation = Orientation },
    GSt#{ headPose => HeadPos1 }.

%%
%% Handle events like wand button press etc
%%
-spec handle_event(GSt::glasses_state(), 
		   Event::t5_wand_stream_event()) -> 
	  glasses_state().
handle_event(GSt, Event) ->
    io:format("tiltfive_wx:handle_event: ~p event=~p~n", [GSt,Event]),
    GSt.

%%
%% Model view projection
%%
%%   v' = P*V*M*v
%% where
%%  P = perspective matrix
%%  V = view matrix
%%  M = model matrix
%%
%%  model matrix M is composed of
%%   
%%   M = T*R*S
%% where
%%   T = translation matrix
%%   R = rotation matrix
%%   S = scale matrix
%%

run(GSt = #{ start_time := T0, 
	     headPose := HeadPose,
	     leftEyePose := LeftEyePose,
	     rightEyePose := RightEyePose,
	     gameboardPose := GameboardPose,
	     width := W, height := H,
	     canvas := Canvas, context := Context,  
	     framebuffers := [L,R], textures := [Tl,Tr],
	     angle1 := A1, step1 := Step1,
	     angle2 := A2, step2 := Step2 }) ->

    Time = (erlang:system_time(micro_seconds) - T0) / 1000000,
    Ratio = ?defaultWidth / ?defaultHeight,

    %% Model transform (rotate the model atoun z-axis)
    %% 1 radian per second
    ModelTran = tiltfive_transform:set_euler(tiltfive_transform:new(),
					     0, 0, Time),
    ModelTran1 = tiltfive_transform:set_scale(ModelTran, 0.1),
    ModelMat = tiltfive_transform:matrix_to_parent_frame(ModelTran1),

    GameMat = tiltfive_transform:matrix_to_local_frame(GameboardPose),

    HeadMat = tiltfive_transform:matrix_to_local_frame(HeadPose),
    LLF = tiltfive_transform:matrix_to_local_frame(LeftEyePose),
    ViewLeftMat = tiltfive_transform:mul3(LLF, HeadMat, GameMat),
    RLF = tiltfive_transform:matrix_to_local_frame(RightEyePose),
    ViewRightMat = tiltfive_transform:mul3(RLF, HeadMat,GameMat),
    Transform = e3d_transform:perspective(?radians(?defaultFOV),Ratio,
					  0.1,100.0),
    PerspectiveMat = e3d_transform:matrix(Transform),

    %% Render left eye
    MvpLeft = tiltfive_transform:mul3(PerspectiveMat, ViewLeftMat, ModelMat),
    
    %% Render right eye
    MvpRight = tiltfive_transform:mul3(PerspectiveMat, ViewRightMat,ModelMat),


    true = wxGLCanvas:setCurrent(Canvas, Context),

    Cube1 = #cube{id=left,framebuffer=L,texture=Tl, mvp=MvpLeft},
    render_cube(Cube1, A1, W, H),
    Cube2 = #cube{id=right,framebuffer=R,texture=Tr, mvp=MvpRight},
    render_cube(Cube2, A1, W, H),

    setup_screen(W, H),
    draw_screen(Cube1#cube.texture, Cube2#cube.texture),
    wxGLCanvas:swapBuffers(Canvas),

    GSt#{ angle1 => math:fmod(A1+Step1, 360.0),
	  angle2 => math:fmod(A2+Step2, 360.0) }.
    
%%
%% Setup wx frame and glcanvas
%% prepare for stereo rendering
%%
wx_init(GSt) ->
    wxe_master:start(false),
    wxe_master:init_opengl(),
    Wx = wx:new(),
    
    #{ width := W, height := H } = GSt,
    Name = maps:get(display_name, GSt, "Tilt"),
    Frame = wxFrame:new(Wx, ?wxID_ANY, Name, [{size,{2*W,H}}]),

    GLAttrib = [{attribList, [
			      ?WX_GL_CORE_PROFILE,
			      ?WX_GL_RGBA,
			      ?WX_GL_DOUBLEBUFFER,
			      ?WX_GL_MIN_RED,8,
			      ?WX_GL_MIN_GREEN,8,
			      ?WX_GL_MIN_BLUE,8,
			      ?WX_GL_DEPTH_SIZE,24,
			      0]}],
    Canvas = wxGLCanvas:new(Frame, GLAttrib),
    ok = wxGLCanvas:setSize(Canvas, 2*W, H),
    Context = wxGLContext:new(Canvas),
    true = wxFrame:show(Frame),
    timer:sleep(1000), %% fixme: wait for show to complete
    true = wxGLCanvas:setCurrent(Canvas, Context),
    [L,R] = gl:genFramebuffers(2),
    [Tl,Tr] = gl:genTextures(2),
    init_framebuffer(L, Tl, W, H),
    init_framebuffer(R, Tr, W, H),
    GSt#{ wx => Wx, frame => Frame, canvas => Canvas, context => Context,
	  framebuffers => [L,R], textures => [Tl,Tr] }.


%% Test start
start() ->
    W = ?WIDTH,
    H = ?HEIGHT,
    GSt = wx_init(#{ width => W, height => H}),
    #{ canvas := Canvas, framebuffers := [L,R], textures := [Tl,Tr] } = GSt,

    init_framebuffer0(2*W, H),

    %% Draw first time 
    Cube1 = #cube{id=left,framebuffer=L,texture=Tl},
    A1 = 45.0,
    render_cube(Cube1, A1, W, H),
    %% dump_texture(Cube1, W, H)

    Cube2 = #cube{id=right,framebuffer=R,texture=Tr},
    A2 = 45.0 + 10.0,
    render_cube(Cube2, A2, W, H),
    %% dump_texture(Cube2, W, H)

    setup_screen(W, H),
    draw_screen(Cube1#cube.texture, Cube2#cube.texture),

    wxGLCanvas:swapBuffers(Canvas),
    timer:sleep(1000),

    Step1 = 2.0,
    Step2 = -2.0,
    %% run a loop
    draw_loop(W,H,GSt,Cube1,Cube2,A1,A2,Step1,Step2).


draw_loop(W, H, GSt, Cube1, Cube2, A1, A2, Step1,Step2) ->
    A11 = math:fmod(A1+Step1, 360.0),
    A21 = math:fmod(A2+Step2, 360.0),

    render_cube(Cube1, A11, W, H),
    render_cube(Cube2, A21, W, H),
    
    setup_screen(W, H),
    draw_screen(Cube1#cube.texture, Cube2#cube.texture),

    wxGLCanvas:swapBuffers(maps:get(canvas, GSt)),
    timer:sleep(50),

    draw_loop(W, H, GSt, Cube1, Cube2, A11, A21, Step1, Step2).

init_framebuffer0(_W, _H) ->
    gl:bindFramebuffer(?GL_FRAMEBUFFER, 0),
%%    [C] = gl:genRenderbuffers(1),
%%    gl:bindRenderbuffer(?GL_RENDERBUFFER, C),
%%    gl:renderbufferStorage(?GL_RENDERBUFFER, ?GL_RGBA, W, H),
%%    gl:framebufferRenderbuffer(?GL_FRAMEBUFFER, ?GL_COLOR_ATTACHMENT0, 
%%			       ?GL_RENDERBUFFER, C),
    ok.

%% setup framebuffer used to render cube
%% use one for each cube to be able to render in parallel. (possible?)
init_framebuffer(B, T, W, H) ->
    gl:bindFramebuffer(?GL_FRAMEBUFFER, B),
    gl:bindTexture(?GL_TEXTURE_2D, T),

    gl:texImage2D(?GL_TEXTURE_2D,0,?GL_RGBA,W,H,0,?GL_RGBA,
		  ?GL_UNSIGNED_BYTE,0),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_S, ?GL_CLAMP_TO_EDGE),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_WRAP_T, ?GL_CLAMP_TO_EDGE),
    gl:framebufferTexture2D(?GL_FRAMEBUFFER, ?GL_COLOR_ATTACHMENT0, 
			    ?GL_TEXTURE_2D, T, 0),
    gl:drawBuffer(?GL_COLOR_ATTACHMENT0),

    %% setup depth buffer
    [D] = gl:genRenderbuffers(1),
    gl:bindRenderbuffer(?GL_RENDERBUFFER, D),
    gl:renderbufferStorage(?GL_RENDERBUFFER, ?GL_DEPTH24_STENCIL8, W, H),
    gl:framebufferRenderbuffer(?GL_FRAMEBUFFER, ?GL_DEPTH_STENCIL_ATTACHMENT, 
			       ?GL_RENDERBUFFER, D),

    Status = case gl:checkFramebufferStatus(?GL_FRAMEBUFFER) of
		 ?GL_FRAMEBUFFER_COMPLETE -> ok; 
		 Status0 -> Status0
	     end,
    io:format("status = ~w~n", [Status]),
    gl:bindRenderbuffer(?GL_RENDERBUFFER, 0).

-ifdef(not_used).
save_image(B, W, H, Filename) ->
    gl:bindFramebuffer(?GL_FRAMEBUFFER, B),
    Pixels = wx:create_memory(W*H*4),
    gl:readPixels(0, 0, W, H, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Pixels),
    Data = wx:get_memory_bin(Pixels),
    save_png(Data, W, H, rgb, Filename),
    gl:bindFramebuffer(?GL_FRAMEBUFFER, 0),
    ok.
save_png(Pixels, W, H, Format, Filename) ->
    Px = epx:pixmap_create(W, H, Format),
    epx:pixmap_put_pixels(Px,0,0,W,H,Format,Pixels),
    Image = #epx_image{type=epx_image_png,
		       filename = Filename,
		       width = W,
		       height = H,
		       depth = 32,
		       pixmaps=[Px]},
    epx_image:save(Image).
-endif.

-ifdef(not_used).
dump_texture(Cube, W, H) ->
    io:format("bump 32 pixels from ~s\n", [Cube#cube.id]),
    gl:bindTexture(?GL_TEXTURE_2D, Cube#cube.texture),
    Pixels = create_texture(W, H, {0,0,0}),
    gl:getTexImage(?GL_TEXTURE_2D, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Pixels),
    SubPixels = binary:part(Pixels, 0, 32),
    io:format("Data = ~p\n", [SubPixels]),
    gl:bindTexture(?GL_TEXTURE_2D, 0).
-endif.


render_cube(#cube{id=_ID,framebuffer=F, texture=_T, png=_Png, mvp=MVP}, A, W, H) ->
    gl:bindFramebuffer(?GL_FRAMEBUFFER, F),
    setup_cube(W, H, MVP),
    draw_cube(A),
    %% if Png /= undefined -> save_image(F, W, H, Png); true -> ok end,
    gl:bindFramebuffer(?GL_FRAMEBUFFER, 0).

setup_cube(W, H, MVP) ->
    gl:clearColor(0.0, 0.0, 0.0, 0.0),  %% totally transparent?
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:enable(?GL_DEPTH_TEST),
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    if MVP =:= undefined ->
	    gl:loadIdentity(),
	    gl:ortho( -2.0, 2.0, -2.0*H/W, 2.0*H/W, -20.0, 20.0);
       true ->
	    gl:loadMatrixd(MVP)
    end,
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:depthFunc(?GL_LESS),
    ok.

-define(BLACK,  0.0, 0.0, 0.0).
-define(GRAY,   0.5, 0.5, 0.5).
-define(RED,    1.0, 0.0, 0.0).
-define(GREEN,  0.0, 1.0, 0.0).
-define(BLUE,   0.0, 0.0, 1.0).
-define(ORANGE, 1.0, 0.5, 0.0).
-define(YELLOW, 1.0, 1.0, 0.0).
-define(MAGENTA,1.0, 0.0, 1.0).
-define(WHITE,  1.0, 1.0, 1.0).

-define(TOP,    ?GREEN).
-define(BOTTOM, ?ORANGE).
-define(FRONT,  ?RED).
-define(BACK,   ?YELLOW).
-define(LEFT,   ?BLUE).
-define(RIGHT,  ?MAGENTA).

color(R, G, B) ->
    gl:color4f(R, G, B, 1.0).

draw_cube(A) ->
    gl:translatef(0.0, 0.0, -6.0),
    gl:rotatef(A, 1.0, 0.0, 0.0),
    gl:rotatef(A, 0.0, 1.0, 0.0),
    gl:rotatef(A, 0.0, 0.0, 1.0),

    %% Begin drawing the color cube with 6 quads

    %% Top face (y = 1.0f)
    %% Define vertices in counter-clockwise (CCW) order with normal pointing out
    color(?TOP),
    gl:'begin'(?GL_QUADS),
    gl:vertex3f(1.0, 1.0, -1.0),
    gl:vertex3f(-1.0, 1.0, -1.0),
    gl:vertex3f(-1.0, 1.0, 1.0),
    gl:vertex3f(1.0, 1.0, 1.0),
    gl:'end'(),

    %% Bottom face (y = -1.0)
    color(?BOTTOM),
    gl:'begin'(?GL_QUADS),
    gl:vertex3f(1.0, -1.0, 1.0),
    gl:vertex3f(-1.0, -1.0, 1.0),
    gl:vertex3f(-1.0, -1.0, -1.0),
    gl:vertex3f(1.0, -1.0, -1.0),
    gl:'end'(),

    %% Front face  (z = 1.0)
    color(?FRONT),
    gl:'begin'(?GL_QUADS),
    gl:vertex3f(1.0, 1.0, 1.0),
    gl:vertex3f(-1.0, 1.0, 1.0),
    gl:vertex3f(-1.0, -1.0, 1.0),
    gl:vertex3f(1.0, -1.0, 1.0),
    gl:'end'(),

    %% Back face (z = -1.0)
    color(?BACK),
    gl:'begin'(?GL_QUADS),
    gl:vertex3f(1.0, -1.0, -1.0),
    gl:vertex3f(-1.0, -1.0, -1.0),
    gl:vertex3f(-1.0, 1.0, -1.0),
    gl:vertex3f(1.0, 1.0, -1.0),
    gl:'end'(),

    %% Left face (x = -1.0)
    color(?LEFT),
    gl:'begin'(?GL_QUADS),
    gl:vertex3f(-1.0, 1.0, 1.0),
    gl:vertex3f(-1.0, 1.0, -1.0),
    gl:vertex3f(-1.0, -1.0, -1.0),
    gl:vertex3f(-1.0, -1.0, 1.0),
    gl:'end'(),

    %% Right face (x = 1.0)
    color(?RIGHT),
    gl:'begin'(?GL_QUADS),
    gl:vertex3f(1.0, 1.0, -1.0),
    gl:vertex3f(1.0, 1.0, 1.0),
    gl:vertex3f(1.0, -1.0, 1.0),
    gl:vertex3f(1.0, -1.0, -1.0),
    gl:'end'(),

    gl:flush(),
    %% gl:finish(),
    ok.

setup_screen(W, H) ->
    gl:viewport(0,0,2*W,H),
    gl:clearColor(0.5, 0.5, 0.5, 1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:disable(?GL_DEPTH_TEST),
    gl:loadIdentity(),
    gl:scalef(2.0, 2.0, 1.0).

%% draw two colored quads on screen
-ifdef(not_used).
draw_screen0(_TL, _TR) ->
    gl:'begin'(?GL_QUADS),
    gl:color3f(1.0, 0.0, 0.0),     %% Red
    gl:vertex2f(-1.0, -1.0),
    gl:vertex2f(0.0, -1.0),
    gl:vertex2f(0.0,  1.0),
    gl:vertex2f(-1.0,  1.0),
    gl:'end'(),
    gl:'begin'(?GL_QUADS),
    gl:color3f(0.0, 1.0, 0.0),     %% Green
    gl:vertex2f(0.0, -1.0),
    gl:vertex2f(1.0, -1.0),
    gl:vertex2f(1.0,  1.0),
    gl:vertex2f(0.0,  1.0),
    gl:'end'(),
    gl:finish().
-endif.


seqf(F, T, Inc) when F =< T ->
    [F|seqf(F+Inc, T, Inc)];
seqf(F, T, _Inc) when F > T -> [].


%% draw two textures TL and TR on screen
draw_screen(TL, TR) ->
    gl:texEnvi(?GL_TEXTURE_ENV,?GL_TEXTURE_ENV_MODE,?GL_REPLACE),


    gl:'begin'(?GL_LINES),
    color(?WHITE),
    lists:foreach(
      fun(Y) ->
	      gl:vertex2f(1.0, Y), gl:vertex2f(-1.0, Y)
      end, seqf(-1.0, 1.0, 0.1)),

    lists:foreach(
      fun(X) ->
	      gl:vertex2f(X, 1.0), gl:vertex2f(X, -1.0)
      end, seqf(-1.0, 1.0, 0.1)),

    gl:'end'(),

    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),


    gl:bindTexture(?GL_TEXTURE_2D, TL),
    gl:enable(?GL_TEXTURE_2D),

    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0.0, 0.0), gl:vertex2f(-1.0, -1.0),
    gl:texCoord2f(1.0, 0.0), gl:vertex2f(0.0, -1.0),
    gl:texCoord2f(1.0, 1.0), gl:vertex2f(0.0,  1.0),
    gl:texCoord2f(0.0, 1.0), gl:vertex2f(-1.0,  1.0),
    gl:'end'(),
    gl:bindTexture(?GL_TEXTURE_2D, 0),

    gl:bindTexture(?GL_TEXTURE_2D, TR),
    gl:'begin'(?GL_QUADS),
    gl:texCoord2f(0.0, 0.0), gl:vertex2f(0.0, -1.0),
    gl:texCoord2f(1.0, 0.0), gl:vertex2f(1.0, -1.0),
    gl:texCoord2f(1.0, 1.0), gl:vertex2f(1.0,  1.0),
    gl:texCoord2f(0.0, 1.0), gl:vertex2f(0.0,  1.0),
    gl:'end'(),
    gl:disable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, 0),
    gl:flush().
    %% gl:finish().

-ifdef(not_used).
create_texture(W, H, {R,G,B}) ->
    Pixel = <<(trunc(R*255)),(trunc(G*255)),(trunc(B*255)),255>>,
    iolist_to_binary(lists:duplicate(W*H, Pixel)).
-endif.

