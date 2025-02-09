%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2025, Tony Rogvall
%%% @doc
%%%	  TiltFive transform functions
%%% @end
%%% Created : 13 Jan 2025 by Tony Rogvall <tony@rogvall.se>

-module(tiltfive_transform).

-export([new/0,
	 set_position/2, set_position/4, position/1,
	 set_orientation/2, orientation/1,
	 set_scale/2, scale/1,
	 set_angle_axix/3,
	 set_euler/2, set_euler/4,
	 point_to_local_frame/2,
	 point_to_parent_frame/2,
	 matrix_to_local_frame/1,
	 matrix_to_parent_frame/1
	]).
-export([mul3/3]).

-include("transform.hrl").

%% "missing" utils
e3d_vec_invert({X,Y,Z}) ->
    {1/X,1/Y,1/Z}.

e3d_vec_mul({X1,Y1,Z1}, {X2,Y2,Z2}) ->
    {X1*X2,Y1*Y2,Z1*Z2}.

e3d_vec_div({X1,Y1,Z1}, {X2,Y2,Z2}) ->
    {X1/X2,Y1/Y2,Z1/Z2}.


new() ->
    #transform{position = e3d_vec:zero(),
	       orientation = e3d_q:identity(),
	       scale = {1.0,1.0,1.0}
	       }.

set_position(T, Pos) when tuple_size(Pos) =:= 3 ->
    T#transform{position = Pos}.
set_position(T, X, Y, Z) when is_float(X), is_float(Y), is_float(Z) ->
    T#transform{position = {X,Y,Z}}.

position(T) ->
    T#transform.position.

set_orientation(T, Q) when tuple_size(Q) =:= 2 ->
    T#transform{orientation = Q}.

orientation(T) ->
    T#transform.orientation.

set_scale(T, Scale) when is_float(Scale) ->
    T#transform{scale = {Scale, Scale, Scale}}.

scale(T) ->
    T#transform.scale.

%% angle in radians
-spec set_angle_axix(T::transform(), Angle::number(), Axis::vecf()) -> 
	  transform().
set_angle_axix(T, Angle, Axis) when is_float(Angle), tuple_size(Axis) =:= 3 ->
    T#transform{ orientation = e3d_q:from_angle_axis_rad(Angle, Axis) }.

-spec set_euler(T::transform(), EulerAngles::vecf()) ->
	  transform().
set_euler(T, EulerAngles) when tuple_size(EulerAngles) =:= 3 ->
    T#transform { orientation = e3d_q:from_euler_rad(EulerAngles) }.

-spec set_euler(T::transform(), X::number(), Y::number(), Z::number()) ->
	  transform().
set_euler(T, R, P, Y) when is_number(R), is_number(P), is_number(Y) ->
    T#transform{ orientation = e3d_q:from_euler_rad(R,P,Y) }.

-spec point_to_local_frame(T::transform(), P::vecf()) -> vecf().
point_to_local_frame(T, P) ->
    L1 = e3d_vec:sub(P, T#transform.position),
    L2 = e3d_vec_div(L1, T#transform.scale),
    e3d_q:vec_rotate(L2, T#transform.orientation).

-spec point_to_parent_frame(T::transform(), P::vecf()) -> vecf().
point_to_parent_frame(T, P) ->
    P1 = e3d_q:vec_rotate(P, e3d_q:inverse(T#transform.orientation)),
    P2 = e3d_vec_mul(P1, T#transform.scale),
    e3d_vec:add(T#transform.position, P2).

mul3(A,B,C) ->
    e3d_mat:mul(e3d_mat:mul(A,B),C).

-spec matrix_to_local_frame(T::transform()) -> matf().
matrix_to_local_frame(T) ->
    M0 = e3d_mat:translate(e3d_vec:neg(T#transform.position)),
    M1 = e3d_mat:scale(e3d_vec_invert(T#transform.scale)),
    M2 = e3d_q:to_rotation_matrix(T#transform.orientation),
    mul3(M0, M1, M2).


-spec matrix_to_parent_frame(T::transform()) -> matf().
matrix_to_parent_frame(T) ->
    M2 = e3d_q:to_rotation_matrix(e3d_q:inverse(T#transform.orientation)),
    M1 = e3d_mat:scale(T#transform.scale),
    M0 = e3d_mat:translate(T#transform.position),
    mul3(M0, M1, M2).


