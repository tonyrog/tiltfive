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

-include("transform.hrl").

new() ->
    #transform{position = vecf:new(0,0,0),
	       orientation = quatf:new(1,0,0,0), %% x-vector 0 degrees
	       scale = vecf:new(1,1,1)}.

set_position(T, Pos) ->
    T#transform{position = Pos}.
set_position(T, X, Y, Z) ->
    T#transform{position = vecf:new(X,Y,Z)}.

position(T) ->
    T#transform.position.

set_orientation(T, Q) ->
    T#transform{orientation = Q}.

orientation(T) ->
    T#transform.orientation.

set_scale(T, Scale) ->
    T#transform{scale = vecf:new(Scale, Scale, Scale)}.

scale(T) ->
    T#transform.scale.

-spec set_angle_axix(T::transform(), Angle::number(), Axis::vecf()) -> 
	  transform().
set_angle_axix(T, Angle, Axis) ->
    T#transform{ orientation = vecf:new(Axis, Angle) }.

-spec set_euler(T::transform(), EulerAngles::vecf()) ->
	  transform().
set_euler(T, EulerAngles) ->
    T#transform { orientation = quatf:from_euler(EulerAngles) }.


-spec set_euler(T::transform(), X::number(), Y::number(), Z::number()) ->
	  transform().
set_euler(T, R, P, Y) when is_number(R), is_number(P), is_number(Y) ->
    T#transform{ orientation = quatf:from_euler(R,P,Y) }.
    
point_to_local_frame(T, P) ->
    L1 = vecf:subtract(P, T#transform.position),
    L2 = vecf:divide(L1, T#transform.scale),
    quatf:rotate(T#transform.orientation, L2).

point_to_parent_frame(T, P) ->
    P1 = quatf:rotate(quatf:inverse(T#transform.orientation), P),
    P2 = vecf:multiply(P1, T#transform.scale),
    vecf:add(T#transform.position, P2).

matrix_to_local_frame(T) ->
    M0 = matf:translate(vecf:negate(T#transform.position)),
    M1 = matf:multiply(matf:scale(vecf:divide(1,T#transform.scale)), M0),
    M2 = matf:multiply(matf:from_quat(T#transform.orientation), M1),
    M2.

matrix_to_parent_frame(T) ->
    M2 = matf:from_quat(quatf:inverse(T#transform.orientation)),
    M1 = matf:multiply(matf:scale(T#transform.scale), M2),
    M0 = matf:multiply(matf:translate(T#transform.position), M1),
    M0.
    
