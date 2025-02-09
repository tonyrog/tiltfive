-ifndef(TRANSFORM_HRL).
-define(TRANSFORM_HRL, true).

-type vecf() :: e3d_vec:vector().
-type matf() :: e3d_mat:matrix().
-type quatf() :: {vecf(), float()}.

-record(transform, 
	{
	 position = e3d_vec:zero() :: vecf(),
	 orientation = e3d_q:identity() :: quatf(),
	 scale = {1.0,1.0,1.0} :: vecf()
	}).
-type transform() :: #transform{}.

-endif.
