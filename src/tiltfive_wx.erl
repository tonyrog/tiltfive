%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%    Test openGL with wxErlang
%%% @end
%%% Created : 14 Nov 2024 by Tony Rogvall <tony@rogvall.se>

-module(tiltfive_wx).

-export([test/0]).

-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 
-include_lib("epx/include/epx_image.hrl").

-define(WIDTH, 640).
-define(HEIGHT, 480).
test() ->
    wxe_master:start(false),
    wxe_master:init_opengl(),
    Wx = wx:new(),
    io:format("wx = ~w\n",[Wx]),
    
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Tilt", [{size,{640,480}}]),
    io:format("Frame = ~w\n",[Frame]),

    Gx = wxGLCanvas:new(Frame),
    io:format("Gx = ~w\n",[Gx]),

    ok = wxGLCanvas:setSize(Gx, ?WIDTH, ?HEIGHT),
    Cx = wxGLContext:new(Gx),
    io:format("Cx = ~w\n",[Cx]),

    true = wxFrame:show(Frame),
    timer:sleep(2000),

    true = wxGLCanvas:setCurrent(Gx, Cx),
    [L,R] = gl:genFramebuffers(2),
    io:format("framebuffers ~w ~w~n",[L,R]),
    [Tl,Tr] = gl:genTextures(2),
    io:format("textures ~w ~w~n",[Tl,Tr]),
    create_framebuffer(L, Tl, ?WIDTH, ?HEIGHT),
    create_framebuffer(R, Tr, ?WIDTH, ?HEIGHT),

    gl:bindFramebuffer(?GL_FRAMEBUFFER, L),
    setup(?WIDTH, ?HEIGHT),
    draw_cube({1.0, 0.0, 0.0}, 45.0),  %% red background
    get_image(L, ?WIDTH, ?HEIGHT, "left.png"),

    gl:bindFramebuffer(?GL_FRAMEBUFFER, R),
    setup(?WIDTH, ?HEIGHT),
    draw_cube({0.0, 1.0, 0.0}, 45.0),  %% green background
    get_image(R, ?WIDTH, ?HEIGHT, "right.png"),
    ok.

create_framebuffer(B, T, W, H) ->
    gl:bindFramebuffer(?GL_FRAMEBUFFER, B),
    gl:bindTexture(?GL_TEXTURE_2D, T),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, W, H, 0,
		  ?GL_RGBA, ?GL_UNSIGNED_BYTE, 0),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    %% Create depth buffer
    [D] = gl:genRenderbuffers(1),
    io:format("renderbuffer ~w~n",[D]),
    gl:renderbufferStorage(?GL_RENDERBUFFER, ?GL_DEPTH_COMPONENT, W, H),
    gl:framebufferRenderbuffer(?GL_FRAMEBUFFER, ?GL_DEPTH_ATTACHMENT, 
			       ?GL_RENDERBUFFER, D),
    %% Set render target
    gl:framebufferTexture2D(?GL_FRAMEBUFFER, ?GL_COLOR_ATTACHMENT0,
			    ?GL_TEXTURE_2D, T, 0),
    Status = case gl:checkFramebufferStatus(?GL_FRAMEBUFFER) of
		 ?GL_FRAMEBUFFER_COMPLETE -> ok; 
		 Status0 -> Status0
	     end,
    io:format("status = ~w~n", [Status]),
    gl:bindRenderbuffer(?GL_RENDERBUFFER, 0).


get_image(B, W, H, Filename) ->
    gl:bindFramebuffer(?GL_FRAMEBUFFER, B),
    Pixels = wx:create_memory(W*H*4),
    gl:readPixels(0, 0, W, H, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Pixels),
    Data = wx:get_memory_bin(Pixels),
    save_png(Data, W, H, rgba, Filename),
    %% io:format("Data ~w~n",[Data]),
    %% wxImage:saveFile(wxImage:new(W, H, Data), File, ?wxBITMAP_TYPE_PNG),
    gl:bindFramebuffer(?GL_FRAMEBUFFER, 0).

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

setup(W, H) ->
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:ortho( -2.0, 2.0, -2.0*H/W, 2.0*H/W, -20.0, 20.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    ok.

draw_cube({R,G,B}, A) ->
    gl:enable(?GL_DEPTH_TEST),
    gl:clearColor(R, G, B, 1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:loadIdentity(),
    gl:translatef(0.0, 0.0, -6.0),
    gl:rotatef(A, 1.0, 0.0, 0.0),
    gl:rotatef(A, 0.0, 1.0, 0.0),
    
    %% Begin drawing the color cube with 6 quads
    gl:'begin'(?GL_QUADS),
    %% Top face (y = 1.0f)
    %% Define vertices in counter-clockwise (CCW) order with normal pointing out
    gl:color3f(0.0, 1.0, 0.0),     %% Green
    gl:vertex3f(1.0, 1.0, -1.0),
    gl:vertex3f(-1.0, 1.0, -1.0),
    gl:vertex3f(-1.0, 1.0, 1.0),
    gl:vertex3f(1.0, 1.0, 1.0),

    %% Bottom face (y = -1.0)
    gl:color3f(1.0, 0.5, 0.0),     %% Orange
    gl:vertex3f(1.0, -1.0, 1.0),
    gl:vertex3f(-1.0, -1.0, 1.0),
    gl:vertex3f(-1.0, -1.0, -1.0),
    gl:vertex3f(1.0, -1.0, -1.0),

    %% Front face  (z = 1.0)
    gl:color3f(1.0, 0.0, 0.0),     %% Red
    gl:vertex3f(1.0, 1.0, 1.0),
    gl:vertex3f(-1.0, 1.0, 1.0),
    gl:vertex3f(-1.0, -1.0, 1.0),
    gl:vertex3f(1.0, -1.0, 1.0),

    %% Back face (z = -1.0)
    gl:color3f(1.0, 1.0, 0.0),     %% Yellow
    gl:vertex3f(1.0, -1.0, -1.0),
    gl:vertex3f(-1.0, -1.0, -1.0),
    gl:vertex3f(-1.0, 1.0, -1.0),
    gl:vertex3f(1.0, 1.0, -1.0),

    %% Left face (x = -1.0)
    gl:color3f(0.0, 0.0, 1.0),     %% Blue
    gl:vertex3f(-1.0, 1.0, 1.0),
    gl:vertex3f(-1.0, 1.0, -1.0),
    gl:vertex3f(-1.0, -1.0, -1.0),
    gl:vertex3f(-1.0, -1.0, 1.0),

    %% Right face (x = 1.0)
    gl:color3f(1.0, 0.0, 1.0),     %% Magenta
    gl:vertex3f(1.0, 1.0, -1.0),
    gl:vertex3f(1.0, 1.0, 1.0),
    gl:vertex3f(1.0, -1.0, 1.0),
    gl:vertex3f(1.0, -1.0, -1.0),
    gl:'end'(),  %% End of drawing color-cube

    gl:flush(),
    ok.

