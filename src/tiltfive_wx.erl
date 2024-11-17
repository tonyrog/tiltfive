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
%% -include_lib("epx/include/epx_image.hrl").

-define(TEST, true).
-define(WIDTH,  512).
-define(HEIGHT, 512).

-record(cube, 
	{
	 id,
	 framebuffer,
	 texture, 
	 color, 
	 png
	}).

test() ->
    wxe_master:start(false),
    wxe_master:init_opengl(),
    Wx = wx:new(),
    
    Frame = wxFrame:new(Wx, ?wxID_ANY, "Tilt", [{size,{2*?WIDTH,?HEIGHT}}]),

    GLAttrib = [{attribList, [
			      ?WX_GL_RGBA,
			      ?WX_GL_DOUBLEBUFFER,
			      ?WX_GL_MIN_RED,8,
			      ?WX_GL_MIN_GREEN,8,
			      ?WX_GL_MIN_BLUE,8,
			      ?WX_GL_DEPTH_SIZE,24,
			      0]}],
    Canvas = wxGLCanvas:new(Frame, GLAttrib),

    ok = wxGLCanvas:setSize(Canvas, 2*?WIDTH, ?HEIGHT),
    Context = wxGLContext:new(Canvas),

    true = wxFrame:show(Frame),
    timer:sleep(1000), %% fixme: wait for show to complete

    true = wxGLCanvas:setCurrent(Canvas, Context),
    [L,R] = gl:genFramebuffers(2),
    [Tl,Tr] = gl:genTextures(2),
    Color1 = {1.0, 0.0, 0.0},
    Color2 = {0.0, 1.0, 0.0},
    init_framebuffer0(2*?WIDTH, ?HEIGHT),
    init_framebuffer(L, Tl, ?WIDTH, ?HEIGHT),
    init_framebuffer(R, Tr, ?WIDTH, ?HEIGHT),

    %% Draw first time 

    Cube1 = #cube{id=left,framebuffer=L,texture=Tl,color=Color1},
    A1 = 45.0,
    render_cube(Cube1, A1, ?WIDTH, ?HEIGHT),

    Cube2 = #cube{id=right,framebuffer=R,texture=Tr,color=Color2},
    A2 = 45.0 + 10.0,
    render_cube(Cube2, A2, ?WIDTH, ?HEIGHT),

    setup_screen(?WIDTH, ?HEIGHT),
    draw_screen(Cube1#cube.texture, Cube2#cube.texture),

    wxGLCanvas:swapBuffers(Canvas),
    timer:sleep(1000),

    Step = 2.0,
    %% run a loop
    draw_loop(?WIDTH,?HEIGHT,Frame,Canvas,Cube1,Cube2,A1,Step).

draw_loop(W, H, Frame, Canvas, Cube1, Cube2, A, Step) ->
    A1 = math:fmod(A+Step, 360.0),
    A2 = math:fmod(A1+10.0, 360.0),

    render_cube(Cube1, A1, W, H),
    render_cube(Cube2, A2, W, H),
    
    setup_screen(W, H),
    draw_screen(Cube1#cube.texture, Cube2#cube.texture),

    wxGLCanvas:swapBuffers(Canvas),
    timer:sleep(50),

    draw_loop(W, H, Frame, Canvas, Cube1, Cube2, A1, Step).

init_framebuffer0(W, H) ->
    gl:bindFramebuffer(?GL_FRAMEBUFFER, 0),
    [C] = gl:genRenderbuffers(1),
    gl:bindRenderbuffer(?GL_RENDERBUFFER, C),
    gl:renderbufferStorage(?GL_RENDERBUFFER, ?GL_RGBA, W, H),
    gl:framebufferRenderbuffer(?GL_FRAMEBUFFER, ?GL_COLOR_ATTACHMENT0, 
			       ?GL_RENDERBUFFER, C),
    ok.

%% setup framebuffer used to render cube
%% use one for each cube to be able to render in parallel. (possible?)
init_framebuffer(B, T, W, H) ->
    gl:bindFramebuffer(?GL_FRAMEBUFFER, B),
    gl:bindTexture(?GL_TEXTURE_2D, T),
    gl:texStorage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, W, H),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, W, H, 0,
		  ?GL_RGBA, ?GL_UNSIGNED_BYTE, 0),

    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    %% Create depth buffer / color buffer
    [D,C] = gl:genRenderbuffers(2),
    %% setup color buffer with a target texture
    gl:bindRenderbuffer(?GL_RENDERBUFFER, C),
    gl:renderbufferStorage(?GL_RENDERBUFFER, ?GL_RGBA, W, H),
    gl:framebufferRenderbuffer(?GL_FRAMEBUFFER, ?GL_COLOR_ATTACHMENT0, 
			       ?GL_RENDERBUFFER, C),
    gl:framebufferTexture(?GL_FRAMEBUFFER, ?GL_COLOR_ATTACHMENT0, T, 0),

    %% setup depth buffer
    gl:bindRenderbuffer(?GL_RENDERBUFFER, D),
    gl:renderbufferStorage(?GL_RENDERBUFFER, ?GL_DEPTH_COMPONENT, W, H),
    gl:framebufferRenderbuffer(?GL_FRAMEBUFFER, ?GL_DEPTH_ATTACHMENT, 
			       ?GL_RENDERBUFFER, D),


    gl:enable(?GL_DEPTH_TEST),

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

render_cube(#cube{id=ID,framebuffer=F, texture=T, color=C, png=Png}, A, W, H) ->
    gl:bindFramebuffer(?GL_FRAMEBUFFER, F),
    setup_cube(C, W, H),
    draw_cube(A),
    %% set_texture(ID,T,C,W,H),
    %% if Png /= undefined -> save_image(F, W, H, Png); true -> ok end,
    gl:bindFramebuffer(?GL_FRAMEBUFFER, 0).


set_texture(ID, T, C={R,G,B}, W, H) ->
    C1 = case get({ID,color}) of
	     undefined -> {trunc(R*255),trunc(G*255),trunc(B*255)};
	     Color -> inc_color(Color, {5,5,5})
	 end,
    put({ID,color}, C1),
    Data = create_texture(W, H, C1),
    gl:bindTexture(?GL_TEXTURE_2D, T),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, W, H, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Data),
    ok.

setup_cube({R,G,B}, W, H) ->
    %% gl:disable(?GL_BLEND), 
    gl:viewport(0,0,W,H),
    gl:clearColor(R, G, B, 1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    gl:ortho( -2.0, 2.0, -2.0*H/W, 2.0*H/W, -20.0, 20.0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:depthFunc(?GL_LESS),
    ok.

draw_cube(A) ->
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

    %% gl:flush(),
    gl:finish(),
    ok.

setup_screen(W, H) ->
    gl:viewport(0,0,2*W,H),
    gl:clearColor(0.5, 0.5, 0.5, 1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:loadIdentity(),
    gl:scalef(2.0, 2.0, 1.0).

%% draw two colored quads on screen
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

%% draw two textures TL and TR on screen
draw_screen(TL, TR) ->
    gl:enable(?GL_TEXTURE_RECTANGLE_ARB), %% _EXT enable texturing
    gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, TL),
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
    gl:disable(?GL_TEXTURE_RECTANGLE_ARB), %% EXT);
    %% gl:flush().
    gl:finish().

create_texture(W, H, {R,G,B}) ->
    Pixel = <<(trunc(R*255)),(trunc(G*255)),(trunc(B*255)),255>>,
    iolist_to_binary(lists:duplicate(W*H, Pixel)).

random_color() ->
    {rand:uniform(256)-1, rand:uniform(256)-1, rand:uniform(256)-1}.

inc_color({R,G,B}, {Ri,Gi,Bi}) ->
    {if R =:= 0 -> 0; true -> max((R+Ri) rem 256,1) end,
     if G =:= 0 -> 0; true -> max((G+Gi) rem 256,1) end,
     if B =:= 0 -> 0; true -> max((B+Bi) rem 256,1) end};

inc_color({R,G,B}, Inc) when is_integer(Inc) ->
    Color1 = R*256*256 + G*256 + B + Inc,
    {Color1 bsr 16, (Color1 bsr 8) band 255, Color1 band 255}.

    
