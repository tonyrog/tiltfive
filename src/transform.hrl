-ifndef(TRANSFORM_HRL).
-define(TRANSFORM_HRL, true).

-type vecf() :: vecf:vec4f().
-type matf() :: matf:mat44f().
-type quatf() :: vecf:vec4f().

-record(transform, 
	{
	 position = vecf:new(0,0,0) :: vecf(),
	 orientation = quatf:new(1,0,0,0) :: quatf(),
	 scale = vecf:new(1,1,1) :: vecf()
	}).
-type transform() :: #transform{}.

-endif.
