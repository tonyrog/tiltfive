/* tiltfive_nif.c */

#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>

#include "erl_nif.h"
#include "erl_driver.h"

#include "TiltFiveNative.h"

#define IDENTIFIER_BUFFER_SIZE 1024
#define GLASSES_BUFFER_SIZE    1024
#define PARAM_BUFFER_SIZE      1024
#define WAND_BUFFER_SIZE       4
#define PARAM_SYS_NUMBER       256
#define PARAM_GLASSES_NUMBER   256
#define DETAIL_BUFfER_SIZE     1024

// #define DEBUG
// #define NIF_TRACE

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif

#define UNUSED(a) ((void) a)

#ifdef DEBUG
#define DEBUGF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#else
#define DEBUGF(f,a...)
#endif
#define INFOF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#define ERRORF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#define BADARG(env) enif_fprintf(stderr, "%s: badarg line=%d\r\n", __FILE__, __LINE__), enif_make_badarg((env))

#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define NIF_LIST \
    NIF("list_glasses",0,tiltfive_list_glasses)		\
    NIF("create_glasses",1,tiltfive_create_glasses)	\
    NIF("get_system_integer_param",1,tiltfive_get_system_integer_param) \
    NIF("get_system_float_param",1,tiltfive_get_system_float_param) \
    NIF("get_system_utf8_param",1,tiltfive_get_system_utf8_param) \
    NIF("get_changed_system_params",0,tiltfive_get_changed_system_params) \
    NIF("get_gameboard_size",1,tiltfive_get_gameboard_size) \
    NIF("reserve_glasses",2,tiltfive_reserve_glasses) \
    NIF("set_glasses_display_name",2,tiltfive_set_glasses_display_name) \
    NIF("ensure_glasses_ready",1,tiltfive_ensure_glasses_ready)		\
    NIF("get_glasses_connection_state",1,tiltfive_get_glasses_connection_state) \
    NIF("get_glasses_identifier",1,tiltfive_get_glasses_identifier) \
    NIF("get_glasses_pose",2,tiltfive_get_glasses_pose) \
    NIF("init_glasses_graphics_context",3,tiltfive_init_glasses_graphics_context) \
    NIF("send_frame_to_glasses",2,tiltfive_send_frame_to_glasses) \
    NIF("configure_camera_stream_for_glasses",2,tiltfive_configure_camera_stream_for_glasses) \
    NIF("get_filled_cam_image_buffer",1,tiltfive_get_filled_cam_image_buffer) \
    NIF("submit_empty_cam_image_buffer",2,tiltfive_submit_empty_cam_image_buffer) \
    NIF("cancel_cam_image_buffer",2,tiltfive_cancel_cam_image_buffer) \
    NIF("validate_frameinfo",2,tiltfive_validate_frameinfo) \
    NIF("get_glasses_integer_param",3,tiltfive_get_glasses_integer_param) \
    NIF("get_glasses_float_param",3,tiltfive_get_glasses_float_param) \
    NIF("get_glasses_utf8_param",3,tiltfive_get_glasses_utf8_param) \
    NIF("get_changed_glasses_params",1,tiltfive_get_changed_glasses_params) \
    NIF("get_projection",7,tiltfive_get_projection) \
    NIF("list_wands_for_glasses",1,tiltfive_list_wands_for_glasses) \
    NIF("send_impulse",4,tiltfive_send_impulse) \
    NIF("configure_wand_stream_for_glasses",2,tiltfive_configure_wand_stream_for_glasses) \
    NIF("read_wand_stream_for_glasses",2,tiltfive_read_wand_stream_for_glasses)

typedef struct
{
    T5_Context context;    // currently only ONE context per node!
    T5_ClientInfo info;
    void* platformContext;
} nif_ctx_t;

// THIS need to be shared between tiltfive and wx! how?
typedef struct
{
    int argc;
    const ERL_NIF_TERM* argv;
    ERL_NIF_TERM result;
} dyncallarg_t;

typedef struct
{
    T5_Glasses glasses;
} tiltfive_glasses_t;

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(unknown);
DECL_ATOM(null);
DECL_ATOM(true);
DECL_ATOM(false);
DECL_ATOM(arg0);
DECL_ATOM(arg1);
DECL_ATOM(arg2);
DECL_ATOM(arg3);
DECL_ATOM(arg4);
DECL_ATOM(arg5);
DECL_ATOM(arg6);

DECL_ATOM(t5_vec2);
DECL_ATOM(t5_vec3);
DECL_ATOM(t5_quat);
DECL_ATOM(t5_glasses_pose);
DECL_ATOM(t5_gameboard_size);
DECL_ATOM(t5_wand_buttons);
DECL_ATOM(t5_wand_report);
DECL_ATOM(t5_wand_stream_event);
DECL_ATOM(t5_projection_info);
DECL_ATOM(t5_cam_image);
DECL_ATOM(t5_frame_info);
DECL_ATOM(t5_camera_stream_config);
DECL_ATOM(t5_wand_stream_config);
DECL_ATOM(t5_graphics_context_gl);
DECL_ATOM(vci);

// error codes
DECL_ATOM(timeout);
DECL_ATOM(no_context);
DECL_ATOM(no_library);
DECL_ATOM(internal);
DECL_ATOM(no_service);
DECL_ATOM(io_failure);
DECL_ATOM(request_id_unknown);
DECL_ATOM(invalid_args);
DECL_ATOM(device_lost);
DECL_ATOM(target_not_found);
DECL_ATOM(invalid_state);
DECL_ATOM(setting_unknown);
DECL_ATOM(setting_wrong_type);
DECL_ATOM(misc_remote);
DECL_ATOM(overflow);
DECL_ATOM(graphics_api_unavailable);
DECL_ATOM(unsupported);
DECL_ATOM(decode_error);
DECL_ATOM(invalid_gfx_context);
DECL_ATOM(gfx_context_init_fail);
DECL_ATOM(try_again);
DECL_ATOM(unavailable);
DECL_ATOM(already_connected);
DECL_ATOM(not_connected);
DECL_ATOM(string_overflow);
DECL_ATOM(service_incompatible);
DECL_ATOM(permission_denied);
DECL_ATOM(invalid_buffer_size);
DECL_ATOM(invalid_geometry);
// dyncall
DECL_ATOM(send_frame_to_glasses);
DECL_ATOM(init_glasses_graphics_context);

ErlNifResourceType *glasses_r;

// Data types and records

static ERL_NIF_TERM make_error(ErlNifEnv* env, T5_Result err)
{
    ERL_NIF_TERM code;
    switch(err) {
    case T5_SUCCESS: code = ATOM(ok); break;
    case T5_TIMEOUT: code = ATOM(timeout); break;
    case T5_ERROR_NO_CONTEXT: code = ATOM(no_context); break;
    case T5_ERROR_NO_LIBRARY: code = ATOM(no_library); break;
    case T5_ERROR_INTERNAL: code = ATOM(internal); break;
    case T5_ERROR_NO_SERVICE: code = ATOM(no_service); break;
    case T5_ERROR_IO_FAILURE: code = ATOM(io_failure); break;
    case T5_ERROR_REQUEST_ID_UNKNOWN: code = ATOM(request_id_unknown); break;
    case T5_ERROR_INVALID_ARGS: code = ATOM(invalid_args); break;
    case T5_ERROR_DEVICE_LOST: code = ATOM(device_lost); break;
    case T5_ERROR_TARGET_NOT_FOUND: code = ATOM(target_not_found); break;
    case T5_ERROR_INVALID_STATE: code = ATOM(invalid_state); break;
    case T5_ERROR_SETTING_UNKNOWN: code = ATOM(setting_unknown); break;
    case T5_ERROR_SETTING_WRONG_TYPE: code = ATOM(setting_wrong_type); break;
    case T5_ERROR_MISC_REMOTE: code = ATOM(misc_remote); break;
    case T5_ERROR_OVERFLOW: code = ATOM(overflow); break;
    case T5_ERROR_GRAPHICS_API_UNAVAILABLE: code = ATOM(graphics_api_unavailable); break;
    case T5_ERROR_UNSUPPORTED: code = ATOM(unsupported); break;
    case T5_ERROR_DECODE_ERROR: code = ATOM(decode_error); break;
    case T5_ERROR_INVALID_GFX_CONTEXT: code = ATOM(invalid_gfx_context); break;
    case T5_ERROR_GFX_CONTEXT_INIT_FAIL: code = ATOM(gfx_context_init_fail); break;
    case T5_ERROR_TRY_AGAIN: code = ATOM(try_again); break;
    case T5_ERROR_UNAVAILABLE: code = ATOM(unavailable); break;
    case T5_ERROR_ALREADY_CONNECTED: code = ATOM(already_connected); break;
    case T5_ERROR_NOT_CONNECTED: code = ATOM(not_connected); break;
    case T5_ERROR_STRING_OVERFLOW: code = ATOM(string_overflow); break;
    case T5_ERROR_SERVICE_INCOMPATIBLE: code = ATOM(service_incompatible); break;
    case T5_PERMISSION_DENIED: code = ATOM(permission_denied); break;
    case T5_ERROR_INVALID_BUFFER_SIZE: code = ATOM(invalid_buffer_size); break;
    case T5_ERROR_INVALID_GEOMETRY: code = ATOM(invalid_geometry); break;
    default: code = ATOM(unknown);
    }
    return enif_make_tuple2(env, ATOM(error), code);
}

static ERL_NIF_TERM make_boolean(ErlNifEnv* env, int value)
{
	return value ? ATOM(true) : ATOM(false);
}

static ERL_NIF_TERM make_t5_vec2(ErlNifEnv* env, T5_Vec2 vec)
{
    return enif_make_tuple3(env,
			    ATOM(t5_vec2),
			    enif_make_double(env, vec.x),
			    enif_make_double(env, vec.y));
}

static ERL_NIF_TERM make_t5_vec3(ErlNifEnv* env, T5_Vec3 vec)
{
    return enif_make_tuple4(env,
			    ATOM(t5_vec3),
			    enif_make_double(env, vec.x),
			    enif_make_double(env, vec.y),
			    enif_make_double(env, vec.z));
}

static ERL_NIF_TERM make_t5_quat(ErlNifEnv* env, T5_Quat quat)
{
    return enif_make_tuple5(env,
			    ATOM(t5_quat),
			    enif_make_double(env, quat.w),			    
			    enif_make_double(env, quat.x),
			    enif_make_double(env, quat.y),
			    enif_make_double(env, quat.z));
}


static ERL_NIF_TERM make_t5_glasses_pose(ErlNifEnv* env, T5_GlassesPose* pose)
{
    return enif_make_tuple5(env,
			    ATOM(t5_glasses_pose),
			    enif_make_uint64(env, pose->timestampNanos),
			    make_t5_vec3(env, pose->posGLS_GBD),
			    make_t5_quat(env, pose->rotToGLS_GBD),
			    enif_make_int(env, pose->gameboardType));
}

static ERL_NIF_TERM make_t5_wand_report(ErlNifEnv* env, T5_WandReport* report)
{
    ERL_NIF_TERM buttons;

    buttons = enif_make_tuple9(env,
			       ATOM(t5_wand_buttons),
			       make_boolean(env, report->buttons.t5),
			       make_boolean(env, report->buttons.one),
			       make_boolean(env, report->buttons.two),
			       make_boolean(env, report->buttons.three),
			       make_boolean(env, report->buttons.a),
			       make_boolean(env, report->buttons.b),
			       make_boolean(env, report->buttons.x),
			       make_boolean(env, report->buttons.y));
    return enif_make_tuple(env, 15,
			   ATOM(t5_wand_report),
			   enif_make_uint64(env, report->timestampNanos),
			   make_boolean(env, report->analogValid),
			   make_boolean(env, report->batteryValid),
			   make_boolean(env, report->buttonsValid),
			   make_boolean(env, report->poseValid),
			   enif_make_double(env, report->trigger),
			   make_t5_vec2(env, report->stick),
			   enif_make_int(env, report->battery),
			   buttons,
			   make_t5_quat(env, report->rotToWND_GBD),
			   make_t5_vec3(env, report->posAim_GBD),
			   make_t5_vec3(env, report->posFingertips_GBD),
			   make_t5_vec3(env, report->posGrip_GBD),
			   enif_make_int(env, (int) report->hand));
}


static ERL_NIF_TERM make_t5_wand_stream_event(ErlNifEnv* env,
					      T5_WandStreamEvent* event)
{
    return enif_make_tuple5(env,
			    ATOM(t5_wand_stream_event),
			    enif_make_int(env, event->wandId),
			    enif_make_int(env, event->type),
			    enif_make_uint64(env, event->timestampNanos),
			    make_t5_wand_report(env, &event->report));
}

static ERL_NIF_TERM make_t5_projection_info(ErlNifEnv* env,
					    T5_ProjectionInfo* projectionInfo)
{
    return enif_make_tuple6(env,
			   ATOM(t5_projection_info),
			   enif_make_tuple(env, 16,
					   projectionInfo->matrix[0],
					   projectionInfo->matrix[1],
					   projectionInfo->matrix[2],
					   projectionInfo->matrix[3],
					   projectionInfo->matrix[4],
					   projectionInfo->matrix[5],
					   projectionInfo->matrix[6],
					   projectionInfo->matrix[7],
					   projectionInfo->matrix[8],
					   projectionInfo->matrix[9],
					   projectionInfo->matrix[10],
					   projectionInfo->matrix[11],
					   projectionInfo->matrix[12],
					   projectionInfo->matrix[13],
					   projectionInfo->matrix[14],
					   projectionInfo->matrix[15]),
			   // \brief Field of View (Y Axis in Degrees)
			   enif_make_double(env, projectionInfo->fieldOfView),
			   /// \brief Aspect Ratio
			   enif_make_double(env, projectionInfo->aspectRatio),
			   /// \brief Framebuffer Width
			   enif_make_uint(env, projectionInfo->framebufferWidth),
			   /// \brief Framebuffer Height
			   enif_make_uint(env, projectionInfo->framebufferHeight));
}


static ERL_NIF_TERM make_t5_cam_image(ErlNifEnv* env, T5_CamImage* image)
{
    return enif_make_tuple(env, 10,
			   ATOM(t5_cam_image),
			   enif_make_uint(env, image->imageWidth),
			   enif_make_uint(env, image->imageHeight),
			   enif_make_uint(env, image->imageStride),
			   enif_make_uint(env, image->cameraIndex),
			   enif_make_uint(env, image->illuminationMode),
			   enif_make_uint(env, image->bufferSize),
			   // FIXME!!!
			   enif_make_ulong(env, (uintptr_t)image->pixelData),
			   make_t5_vec3(env, image->posCAM_GBD),
			   make_t5_quat(env, image->rotToCAM_GBD));
}

static int get_boolean(ErlNifEnv* env, ERL_NIF_TERM arg, bool* value)
{
    unsigned int ival;
    if (arg == ATOM(true))
	*value = 1;
    else if (arg == ATOM(false))
	*value = 0;
    else if (enif_get_uint(env, arg, &ival))
	*value = (ival != 0);
    else
	return 0;
    return 1;
}

static int get_t5_vec3(ErlNifEnv* env, ERL_NIF_TERM arg, T5_Vec3* vec)
{
    const ERL_NIF_TERM* elem;
    int arity;
    double x, y, z;
    
    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 4))
	return 0;
    if (elem[0] != ATOM(t5_vec3))
	return 0;
    if (!enif_get_double(env, elem[1], &x))
	return 0;
    if (!enif_get_double(env, elem[2], &y))
	return 0;
    if (!enif_get_double(env, elem[3], &z))
	return 0;
    vec->x = x;
    vec->y = y;
    vec->z = z;
    return 1;
}

static int get_t5_quat(ErlNifEnv* env, ERL_NIF_TERM arg, T5_Quat* quat)
{
    const ERL_NIF_TERM* elem;
    int arity;
    double w, x, y, z;    
    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 5))
	return 0;
    if (elem[0] != ATOM(t5_quat))
	return 0;
    if (!enif_get_double(env, elem[1], &w))
	return 0;
    if (!enif_get_double(env, elem[2], &x))
	return 0;
    if (!enif_get_double(env, elem[3], &y))
	return 0;
    if (!enif_get_double(env, elem[4], &z))
	return 0;
    quat->w = w;
    quat->x = x;
    quat->y = y;
    quat->z = z;    
    return 1;
}


static int get_t5_camera_stream_config(ErlNifEnv* env, ERL_NIF_TERM arg, T5_CameraStreamConfig* config)
{
    const ERL_NIF_TERM* elem;
    int arity;
    unsigned int cameraIndex;
    
    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 3))
	return 0;
    if (elem[0] != ATOM(t5_camera_stream_config))
	return 0;
    if (!enif_get_uint(env, elem[1], &cameraIndex))
	return 0;
    if (!get_boolean(env, elem[2], &config->enabled))
	return 0;
    config->cameraIndex = cameraIndex;
    return 1;
}

static int get_t5_wand_stream_config(ErlNifEnv* env, ERL_NIF_TERM arg, T5_WandStreamConfig* config)
{
    const ERL_NIF_TERM* elem;
    int arity;
	
    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 2))
	return 0;
    if (elem[0] != ATOM(t5_wand_stream_config))
	return 0;
    if (!get_boolean(env, elem[1], &config->enabled))
	return 0;
    return 1;
}

static int get_t5_cam_image(ErlNifEnv* env, ERL_NIF_TERM arg, T5_CamImage* image)
{
    const ERL_NIF_TERM* elem;
    int arity;
    unsigned int imageWidth;
    unsigned int imageHeight;
    unsigned int imageStride;
    unsigned int cameraIndex;
    unsigned int illuminationMode;
    unsigned int bufferSize;
    uint64_t pixelData;
    
    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 10))
	return 0;
    if (elem[0] != ATOM(t5_cam_image))
	return 0;
    if (!enif_get_uint(env, elem[1], &imageWidth))
	return 0;
    if (!enif_get_uint(env, elem[2], &imageHeight))    
	return 0;
    if (!enif_get_uint(env, elem[3], &imageStride))
	return 0;
    if (!enif_get_uint(env, elem[4], &cameraIndex))
	return 0;
    if (!enif_get_uint(env, elem[5], &illuminationMode))
	return 0;
    if (!enif_get_uint(env, elem[6], &bufferSize))
	return 0;    
    // FIXME!!!?
    if (!enif_get_uint64(env, elem[7], &pixelData))
	return 0;
    if (!get_t5_vec3(env, elem[8], &image->posCAM_GBD))
	return 0;
    if (!get_t5_quat(env, elem[9], &image->rotToCAM_GBD))
	return 0;
    image->imageWidth = imageWidth;
    image->imageHeight = imageHeight;
    image->imageStride = imageStride;
    image->cameraIndex = cameraIndex;
    image->illuminationMode = illuminationMode;
    image->bufferSize = bufferSize;
    image->pixelData = (void*)pixelData;
    return 1;
}


static int get_t5_frame_info(ErlNifEnv* env, ERL_NIF_TERM arg, T5_FrameInfo* info)
{
    const ERL_NIF_TERM* elem;
    const ERL_NIF_TERM* elem_vci;    
    int arity;    
    uint64_t leftTexHandle;
    uint64_t rightTexHandle;
    unsigned int texWidth_PIX;
    unsigned int texHeight_PIX;
    double startX_VCI;
    double startY_VCI;
    double width_VCI;
    double height_VCI;

    if (!enif_get_tuple(env, arg, &arity, &elem) || (arity != 12))
	return 0;
    if (elem[0] != ATOM(t5_frame_info))
	return 0;

    if (!enif_get_uint64(env, elem[1], &leftTexHandle))
	return 0;
    if (!enif_get_uint64(env, elem[2], &rightTexHandle))
	return 0;
    if (!enif_get_uint(env, elem[3], &texWidth_PIX))
	return 0;
    if (!enif_get_uint(env, elem[4], &texHeight_PIX))
	return 0;
    if (!get_boolean(env, elem[5], &info->isSrgb))
	return 0;
    if (!get_boolean(env, elem[6], &info->isUpsideDown))
	return 0;
    if (!enif_get_tuple(env, elem[7], &arity, &elem_vci) || (arity != 5))
	return 0;
    if (elem_vci[0] != ATOM(vci))
	return 0;
    else {
	if (!enif_get_double(env, elem_vci[1], &startX_VCI))
	    return 0;
	if (!enif_get_double(env, elem_vci[2], &startY_VCI))
	    return 0;
	if (!enif_get_double(env, elem_vci[3], &width_VCI))
	    return 0;
	if (!enif_get_double(env, elem_vci[4], &height_VCI))
	    return 0;
    }
    if (!get_t5_quat(env, elem[8], &info->rotToLVC_GBD))
	return 0;
    if (!get_t5_vec3(env, elem[9], &info->posLVC_GBD))
	return 0;
    if (!get_t5_quat(env, elem[10], &info->rotToRVC_GBD))
	return 0;
    if (!get_t5_vec3(env, elem[11], &info->posRVC_GBD))
	return 0;
    info->leftTexHandle = (void*) leftTexHandle;
    info->rightTexHandle = (void*) rightTexHandle;
    info->texWidth_PIX = texWidth_PIX;
    info->texHeight_PIX = texHeight_PIX;
    info->vci.startX_VCI = startX_VCI;
    info->vci.startY_VCI = startY_VCI;
    info->vci.width_VCI = width_VCI;
    info->vci.height_VCI = height_VCI;
    return 1;
}

// API
	
static ERL_NIF_TERM tiltfive_list_glasses(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);    
    T5_Result err;
    size_t bufferSize = GLASSES_BUFFER_SIZE;
    char glassesListBuffer[GLASSES_BUFFER_SIZE];
    const char* buffPtr;
    ERL_NIF_TERM list;

    err = t5ListGlasses(ctx->context, glassesListBuffer, &bufferSize);
    if (err != 	T5_SUCCESS)
	return make_error(env, err);

    list = enif_make_list(env, 0);
    buffPtr = glassesListBuffer;
    while ((buffPtr < glassesListBuffer + GLASSES_BUFFER_SIZE) &&
	   (*buffPtr != 0)) {
	size_t len = 0;
	const char* ptr = buffPtr;
	ERL_NIF_TERM elem;

	while(*ptr && (ptr < glassesListBuffer + GLASSES_BUFFER_SIZE))
	    ptr++;
	len = ptr - buffPtr;
	elem = enif_make_string_len(env, buffPtr, len, ERL_NIF_LATIN1);
	list = enif_make_list_cell(env, elem, list);
	
	buffPtr += (len+1);
    }
    return list;
}

static ERL_NIF_TERM tiltfive_create_glasses(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    tiltfive_glasses_t* gp;
    char id[GLASSES_BUFFER_SIZE];
    T5_Glasses glasses;
    T5_Result err;
    ERL_NIF_TERM term;
    
    if (!enif_get_string(env, argv[0], id, sizeof(id), ERL_NIF_LATIN1))
	return enif_raise_exception(env, ATOM(arg0));
    if ((err = t5CreateGlasses(ctx->context, id, &glasses)) != T5_SUCCESS)
	return make_error(env, err);
    if ((gp = enif_alloc_resource(glasses_r,
				  sizeof(tiltfive_glasses_t))) == NULL)
	return enif_make_badarg(env);
    gp->glasses = glasses;
    term = enif_make_resource(env, gp);
    enif_release_resource(gp);
    return term;
}

static ERL_NIF_TERM tiltfive_get_system_integer_param(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    int param_val;
    T5_ParamSys param;
    int64_t value;
    T5_Result err;

    if (!enif_get_int(env, argv[0], &param_val))
	return enif_raise_exception(env, ATOM(arg0));
    param = (T5_ParamSys) param_val;
    if ((err = t5GetSystemIntegerParam(ctx->context, param, &value)) != T5_SUCCESS)
	return make_error(env, err);
    return enif_make_int64(env, value);
}

static ERL_NIF_TERM tiltfive_get_system_float_param(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    int param_val;
    T5_ParamSys param;
    double value;
    T5_Result err;

    if (!enif_get_int(env, argv[0], &param_val))
	return enif_raise_exception(env, ATOM(arg0));
    param = (T5_ParamSys) param_val;
    if ((err = t5GetSystemFloatParam(ctx->context, param, &value)) != T5_SUCCESS)
	return make_error(env, err);
    return enif_make_double(env, value);    
}

static ERL_NIF_TERM tiltfive_get_system_utf8_param(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    int param_val;
    T5_ParamSys param;
    char value[PARAM_BUFFER_SIZE];
    size_t size = PARAM_BUFFER_SIZE;    
    T5_Result err;

    if (!enif_get_int(env, argv[0], &param_val))
	return enif_raise_exception(env, ATOM(arg0));
    param = (T5_ParamSys) param_val;
    
    if ((err = t5GetSystemUtf8Param(ctx->context, param, value, &size)) != T5_SUCCESS)
	return make_error(env, err);
    return enif_make_string_len(env, value, size-1, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM tiltfive_get_changed_system_params(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    uint16_t count;
    T5_ParamSys buffer[PARAM_SYS_NUMBER];
    T5_Result err;
    ERL_NIF_TERM list;
    int i;
    count = PARAM_SYS_NUMBER;
    if ((err = t5GetChangedSystemParams(ctx->context, buffer, &count))
	!= T5_SUCCESS)
	return make_error(env, err);
    list = enif_make_list(env, 0);
    for (i = 0; i < count; i++) {
	ERL_NIF_TERM elem = enif_make_int(env, (int) buffer[i]);
	list = enif_make_list_cell(env, enif_make_int(env, elem), list);
    }
    return list;
}

static ERL_NIF_TERM tiltfive_get_gameboard_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    int type;
    T5_GameboardType gmt;
    T5_GameboardSize gms;
    T5_Result err;
    
    if (!enif_get_int(env, argv[0], &type))
	return enif_raise_exception(env, ATOM(arg0));
    gmt = (T5_GameboardType) type;
    err = t5GetGameboardSize(ctx->context, gmt, &gms);
    if (err != T5_SUCCESS)
	return make_error(env, err);
    return enif_make_tuple6(env,
			    ATOM(t5_gameboard_size),
			    enif_make_double(env, gms.viewableExtentPositiveX),
			    enif_make_double(env, gms.viewableExtentNegativeX),
			    enif_make_double(env, gms.viewableExtentPositiveY),
			    enif_make_double(env, gms.viewableExtentNegativeY),
			    enif_make_double(env, gms.viewableExtentPositiveZ));
}

static ERL_NIF_TERM tiltfive_reserve_glasses(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    char value[PARAM_BUFFER_SIZE];
    T5_Result err;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_string(env, argv[1], value, sizeof(value), ERL_NIF_LATIN1))
	return enif_raise_exception(env, ATOM(arg1));
    if ((err = t5ReserveGlasses(gp->glasses, value)) != T5_SUCCESS)
	return make_error(env, err);
    return ATOM(ok);
}

static ERL_NIF_TERM tiltfive_set_glasses_display_name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    char value[PARAM_BUFFER_SIZE];
    T5_Result err;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_string(env, argv[1], value, sizeof(value), ERL_NIF_LATIN1))
	return enif_raise_exception(env, ATOM(arg1));  
    if ((err = t5SetGlassesDisplayName(gp->glasses, value)) != T5_SUCCESS)
	return make_error(env, err);
    return ATOM(ok);
}

static ERL_NIF_TERM tiltfive_ensure_glasses_ready(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if ((err = t5EnsureGlassesReady(gp->glasses)) != T5_SUCCESS)
	return make_error(env, err);
    return ATOM(ok);
}

static ERL_NIF_TERM tiltfive_get_glasses_connection_state(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;
    T5_ConnectionState connectionState;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if ((err = t5GetGlassesConnectionState(gp->glasses, &connectionState)) != T5_SUCCESS)
	return make_error(env, err);
    return enif_make_int(env, (int)connectionState);
}

static ERL_NIF_TERM tiltfive_get_glasses_identifier(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;
    char value[PARAM_BUFFER_SIZE];
    size_t size = PARAM_BUFFER_SIZE;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if ((err = t5GetGlassesIdentifier(gp->glasses, value, &size)) != T5_SUCCESS)
	return make_error(env, err);
    return enif_make_string_len(env, value, size-1, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM tiltfive_get_glasses_pose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;
    int usage;
    T5_GlassesPose pose;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_int(env, argv[1], &usage))
	return enif_raise_exception(env, ATOM(arg1));
    if ((err = t5GetGlassesPose(gp->glasses, (T5_GlassesPoseUsage) usage, &pose)) != T5_SUCCESS)
	return make_error(env, err);
    return make_t5_glasses_pose(env, &pose);
}

static ERL_NIF_TERM tiltfive_init_glasses_graphics_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;
    unsigned int graphicsApi;
    union {
	T5_GraphicsContextGL gl;
	T5_GraphicsContextVulkan vk;
    } graphicsContext;
    void* graphicsContext_ptr;
    uint64_t graphicsContext_u64;
    const ERL_NIF_TERM* elem;
    int arity;    
    int maybe_record;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_uint(env, argv[1], &graphicsApi))
	return enif_raise_exception(env, ATOM(arg1));
    maybe_record = enif_get_tuple(env, argv[2], &arity, &elem);
    if (maybe_record && (arity == 4) &&
	(elem[0] == ATOM(t5_graphics_context_gl))) {
	unsigned int texturemode;
	unsigned int left;
	unsigned int right;

	if (!enif_get_uint(env, elem[1], &texturemode))
	    return enif_raise_exception(env, ATOM(arg2));
	if (!enif_get_uint(env, elem[2], &left))
	    return enif_raise_exception(env, ATOM(arg2));
	if (!enif_get_uint(env, elem[3], &right))
	    return enif_raise_exception(env, ATOM(arg2));
	graphicsContext.gl.textureMode =
	    (T5_GraphicsApi_GL_TextureMode) texturemode;
	graphicsContext.gl.leftEyeArrayIndex = left;
	graphicsContext.gl.rightEyeArrayIndex = right;
	graphicsContext_ptr = (void*) &graphicsContext;
    }
    else if (argv[2] == ATOM(null))
	graphicsContext_ptr = NULL;
    else if (enif_get_uint64(env, argv[2], &graphicsContext_u64)) {
	if (graphicsContext_u64 == 0)
	    graphicsContext_ptr = NULL;
	else
	    return enif_raise_exception(env, ATOM(arg2));
    }
    else
	return enif_raise_exception(env, ATOM(arg2));
    
    if ((err = t5InitGlassesGraphicsContext(gp->glasses,
					    (T5_GraphicsApi) graphicsApi,
					    graphicsContext_ptr)) != T5_SUCCESS)
	return make_error(env, err);
    return ATOM(ok);
}

static ERL_NIF_TERM tiltfive_send_frame_to_glasses(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;
    T5_FrameInfo info;

    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!get_t5_frame_info(env, argv[1], &info))
	return enif_raise_exception(env, ATOM(arg1));
    err = t5SendFrameToGlasses(gp->glasses, &info);
    if (err != T5_SUCCESS)
	return make_error(env, err);
    return ATOM(ok);
}

static ERL_NIF_TERM tiltfive_configure_camera_stream_for_glasses(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;
    T5_CameraStreamConfig config;

    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!get_t5_camera_stream_config(env, argv[1], &config))
	return enif_raise_exception(env, ATOM(arg1));	
    err = t5ConfigureCameraStreamForGlasses(gp->glasses, config);
    if (err != T5_SUCCESS)
	return make_error(env, err);
    return ATOM(ok);
}

static ERL_NIF_TERM tiltfive_get_filled_cam_image_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;
    T5_CamImage image;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0)); 
    err = t5GetFilledCamImageBuffer(gp->glasses, &image);
    if (err != T5_SUCCESS)
	return make_error(env, err);
    // filled from where??? :-)
    return make_t5_cam_image(env, &image);
}

static ERL_NIF_TERM tiltfive_submit_empty_cam_image_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;
    T5_CamImage image;

    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!get_t5_cam_image(env, argv[1], &image))
	return enif_raise_exception(env, ATOM(arg1));
    err = t5SubmitEmptyCamImageBuffer(gp->glasses, &image);
    if (err != T5_SUCCESS)
	return make_error(env, err);
    return ATOM(ok);
}

static ERL_NIF_TERM tiltfive_cancel_cam_image_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;
    uintptr_t ptr;

    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_uint64(env, argv[1], &ptr))
	return enif_raise_exception(env, ATOM(arg1));
    err = t5CancelCamImageBuffer(gp->glasses, (uint8_t*) ptr);
    if (err != T5_SUCCESS)
	return make_error(env, err);
    return ATOM(ok);
}

static ERL_NIF_TERM tiltfive_validate_frameinfo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_Result err;
    T5_FrameInfo info;
    char detail[DETAIL_BUFfER_SIZE];
    size_t detailSize;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!get_t5_frame_info(env, argv[1], &info))
	return enif_raise_exception(env, ATOM(arg1));
    detailSize = DETAIL_BUFfER_SIZE;
    err = t5ValidateFrameInfo(gp->glasses, &info, detail, &detailSize);
    switch(err) {
    case T5_SUCCESS:  // no error
	return ATOM(ok);
    case T5_ERROR_NO_CONTEXT:  // invalid glasses
	return make_error(env, err);
    case T5_ERROR_DECODE_ERROR:  // invalid frame info
	return make_error(env, err);
    case T5_ERROR_INVALID_ARGS:  // null args...
	return make_error(env, err);	
    case T5_ERROR_OVERFLOW: // buffer too small (FIXME resize and retry)
	break;
    }
    return enif_make_tuple2(env, ATOM(error),
			    enif_make_string_len(env, detail, detailSize-1, ERL_NIF_LATIN1));
}

static ERL_NIF_TERM tiltfive_get_glasses_integer_param(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    int param_val;
    T5_ParamGlasses param;
    int iwand;
    T5_WandHandle wand;
    int64_t value;
    T5_Result err;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_int(env, argv[1], &iwand))
	return enif_raise_exception(env, ATOM(arg1));    
    if (!enif_get_int(env, argv[2], &param_val))
	return enif_raise_exception(env, ATOM(arg2));
    param = (T5_ParamGlasses) param_val;
    wand = (T5_WandHandle) iwand;
    if ((err = t5GetGlassesIntegerParam(gp->glasses, wand, param, &value)) != T5_SUCCESS)
	return make_error(env, err);
    return enif_make_int64(env, value);    
}

static ERL_NIF_TERM tiltfive_get_glasses_float_param(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    int param_val;
    T5_ParamGlasses param;
    int iwand;
    T5_WandHandle wand;
    double value;
    T5_Result err;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_int(env, argv[1], &iwand))
	return enif_raise_exception(env, ATOM(arg1));    
    if (!enif_get_int(env, argv[2], &param_val))
	return enif_raise_exception(env, ATOM(arg2));
    param = (T5_ParamGlasses) param_val;
    wand = (T5_WandHandle) iwand;
    if ((err = t5GetGlassesFloatParam(gp->glasses, wand, param, &value)) != T5_SUCCESS)
	return make_error(env, err);
    return enif_make_double(env, value);        
}

static ERL_NIF_TERM tiltfive_get_glasses_utf8_param(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    int param_val;
    T5_ParamGlasses param;
    int iwand;
    T5_WandHandle wand;
    char value[PARAM_BUFFER_SIZE];
    size_t size = PARAM_BUFFER_SIZE;
    T5_Result err;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_int(env, argv[1], &iwand))
	return enif_raise_exception(env, ATOM(arg1));    
    if (!enif_get_int(env, argv[2], &param_val))
	return enif_raise_exception(env, ATOM(arg2));
    param = (T5_ParamGlasses) param_val;
    wand = (T5_WandHandle) iwand;
    if ((err = t5GetGlassesUtf8Param(gp->glasses, wand, param, value, &size)) != T5_SUCCESS)
	return make_error(env, err);
    return enif_make_string_len(env, value, size-1, ERL_NIF_LATIN1);    
}

static ERL_NIF_TERM tiltfive_get_changed_glasses_params(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    uint16_t count;
    T5_ParamGlasses buffer[PARAM_GLASSES_NUMBER];
    T5_Result err;
    ERL_NIF_TERM list;
    int i;

    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    count = PARAM_GLASSES_NUMBER;
    if ((err = t5GetChangedGlassesParams(gp->glasses, buffer, &count)) != T5_SUCCESS)
	return make_error(env, err);
    list = enif_make_list(env, 0);
    for (i = 0; i < count; i++) {
	ERL_NIF_TERM elem = enif_make_int(env, (int) buffer[i]);
	list = enif_make_list_cell(env, enif_make_int(env, elem), list);
    }
    return list;    
}

static ERL_NIF_TERM tiltfive_get_projection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    unsigned int handedness;
    unsigned int depthRange;
    unsigned int matrixOrder;
    double nearPlane;
    double farPlane;
    double worldScale;
    T5_ProjectionInfo projectionInfo;
    T5_Result err;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_uint(env, argv[1], &handedness))
	return enif_raise_exception(env, ATOM(arg1));
    if (!enif_get_uint(env, argv[2], &depthRange))
	return enif_raise_exception(env, ATOM(arg2));
    if (!enif_get_uint(env, argv[3], &matrixOrder))
	return enif_raise_exception(env, ATOM(arg3));
    if (!enif_get_double(env, argv[4], &nearPlane))
	return enif_raise_exception(env, ATOM(arg4));
    if (!enif_get_double(env, argv[5], &farPlane))
	return enif_raise_exception(env, ATOM(arg5));
    if (!enif_get_double(env, argv[6], &worldScale))
	return enif_raise_exception(env, ATOM(arg6));            

    err = t5GetProjection(gp->glasses,
			  (T5_CartesianCoordinateHandedness) handedness,
			  (T5_DepthRange) depthRange,
			  (T5_MatrixOrder) matrixOrder,
			  nearPlane,
			  farPlane,
			  worldScale,
			  &projectionInfo);
    if (err != T5_SUCCESS)
	return make_error(env, err);
    return make_t5_projection_info(env, &projectionInfo);

}

static ERL_NIF_TERM tiltfive_list_wands_for_glasses(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;    
    T5_WandHandle buffer[WAND_BUFFER_SIZE];
    uint8_t count;
    ERL_NIF_TERM list;
    T5_Result err;
    int i;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));    
    count = WAND_BUFFER_SIZE;
    err = t5ListWandsForGlasses(gp->glasses, buffer, &count);
    if (err != T5_SUCCESS)
	return make_error(env, err);    

    list = enif_make_list(env, 0);
    for (i = 0; i < count; i++) {
	ERL_NIF_TERM elem = enif_make_int(env, (int) buffer[i]);
	list = enif_make_list_cell(env, elem, list);
    }
    return list;
}

static ERL_NIF_TERM tiltfive_send_impulse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    unsigned int wand;
    double amplitude;
    unsigned int duration;
    T5_Result err;
    
    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_uint(env, argv[1], &wand))
	return enif_raise_exception(env, ATOM(arg1));
    if (!enif_get_double(env, argv[2], &amplitude))
	return enif_raise_exception(env, ATOM(arg2));
    if (!enif_get_uint(env, argv[3], &duration))
	return enif_raise_exception(env, ATOM(arg3));    
    err = t5SendImpulse(gp->glasses, (T5_WandHandle) wand,
			(float) amplitude, (uint16_t) duration);
    if (err != T5_SUCCESS)
	return make_error(env, err);
    return ATOM(ok);
}

static ERL_NIF_TERM tiltfive_configure_wand_stream_for_glasses(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_WandStreamConfig config;
    T5_Result err;

    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!get_t5_wand_stream_config(env, argv[1], &config))
	return enif_raise_exception(env, ATOM(arg1));
    err = t5ConfigureWandStreamForGlasses(gp->glasses, &config);
    if (err != T5_SUCCESS)
	return make_error(env, err);    
    return ATOM(ok);
}

// FIXME: this is a blocking call, should be run in a separate thread. dirty?
static ERL_NIF_TERM tiltfive_read_wand_stream_for_glasses(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    tiltfive_glasses_t* gp;
    T5_WandStreamEvent event;
    T5_Result err;
    unsigned tmo;

    if (!enif_get_resource(env, argv[0], glasses_r, (void**)&gp))
	return enif_raise_exception(env, ATOM(arg0));
    if (!enif_get_uint(env, argv[1], &tmo))
	return enif_raise_exception(env, ATOM(arg1));    
    err = t5ReadWandStreamForGlasses(gp->glasses, &event, (uint32_t) tmo);
    if (err != T5_SUCCESS)
	return make_error(env, err);
    return make_t5_wand_stream_event(env, &event);
}

// create all tracing NIFs
#ifdef NIF_TRACE

#undef NIF

static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0])) {
	    // FIXME print object type if available
	    enif_fprintf(stdout, "%T", argv[0]);
	}
	else
	    enif_fprintf(stdout, "%T", argv[0]);
	for (i = 1; i < argc; i++)
	    enif_fprintf(stdout, ",%T", argv[i]);
    }
    enif_fprintf(stdout, ")");
}

#define NIF(name, arity, func)					\
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER tilefive:%s", (name));		\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST

#endif


// Declare all nif functions
#undef NIF
#ifdef NIF_TRACE
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST

#undef NIF
#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif

static ErlNifFunc nif_funcs[] =
{
    NIF_LIST
};

static int load_atoms(ErlNifEnv* env)
{
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(unknown);
    LOAD_ATOM(null);
    LOAD_ATOM(true);
    LOAD_ATOM(false);
    LOAD_ATOM(arg0);
    LOAD_ATOM(arg1);
    LOAD_ATOM(arg2);
    LOAD_ATOM(arg3);
    LOAD_ATOM(arg4);
    LOAD_ATOM(arg5);
    LOAD_ATOM(arg6);    
    
    // api names
    LOAD_ATOM(t5_vec2);
    LOAD_ATOM(t5_vec3);
    LOAD_ATOM(t5_quat);
    LOAD_ATOM(t5_glasses_pose);
    LOAD_ATOM(t5_gameboard_size);
    LOAD_ATOM(t5_wand_buttons);
    LOAD_ATOM(t5_wand_report);
    LOAD_ATOM(t5_wand_stream_event);
    LOAD_ATOM(t5_projection_info);
    LOAD_ATOM(t5_cam_image);
    LOAD_ATOM(t5_frame_info);
    LOAD_ATOM(t5_camera_stream_config);
    LOAD_ATOM(t5_wand_stream_config);
    LOAD_ATOM(t5_graphics_context_gl);
    LOAD_ATOM(vci);

    // error codes
    LOAD_ATOM(timeout);
    LOAD_ATOM(no_context);
    LOAD_ATOM(no_library);
    LOAD_ATOM(internal);
    LOAD_ATOM(no_service);
    LOAD_ATOM(io_failure);
    LOAD_ATOM(request_id_unknown);
    LOAD_ATOM(invalid_args);
    LOAD_ATOM(device_lost);
    LOAD_ATOM(target_not_found);
    LOAD_ATOM(invalid_state);
    LOAD_ATOM(setting_unknown);
    LOAD_ATOM(setting_wrong_type);
    LOAD_ATOM(misc_remote);
    LOAD_ATOM(overflow);
    LOAD_ATOM(graphics_api_unavailable);
    LOAD_ATOM(unsupported);
    LOAD_ATOM(decode_error);
    LOAD_ATOM(invalid_gfx_context);
    LOAD_ATOM(gfx_context_init_fail);
    LOAD_ATOM(try_again);
    LOAD_ATOM(unavailable);
    LOAD_ATOM(already_connected);
    LOAD_ATOM(not_connected);
    LOAD_ATOM(string_overflow);
    LOAD_ATOM(service_incompatible);
    LOAD_ATOM(permission_denied);
    LOAD_ATOM(invalid_buffer_size);
    LOAD_ATOM(invalid_geometry);
    // dyncall
    LOAD_ATOM(send_frame_to_glasses);
    LOAD_ATOM(init_glasses_graphics_context);
    return 0;
}

static void glasses_dtor(ErlNifEnv *env, tiltfive_glasses_t* obj)
{
    // nif_ctx_t* ctx = (nif_ctx_t*) enif_priv_data(env);
    t5DestroyGlasses(&obj->glasses);
}

// execute nif from other thread (wx)
static void glasses_dyncall(ErlNifEnv* caller_env, void* obj, void* call_data)
{
    dyncallarg_t* dap = (dyncallarg_t*) call_data;
    const ERL_NIF_TERM* argv2;
    int argc2;
    
    DEBUGF("glasses_dyncall %T %T", dap->argv[0], dap->argv[1]);

    if (!enif_get_tuple(caller_env, dap->argv[1], &argc2, &argv2)) {
	dap->result = ATOM(error);
	return;
    }
    
    if (dap->argv[0] == ATOM(send_frame_to_glasses)) {
	dap->result = tiltfive_send_frame_to_glasses(caller_env, argc2, argv2);
    }
    else if (dap->argv[0] == ATOM(init_glasses_graphics_context)) {
	dap->result = tiltfive_init_glasses_graphics_context(caller_env,
							     argc2, argv2);
    }
    DEBUGF("return %T = %T", dap->argv[0], dap->result);
}


static int tilefive_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    nif_ctx_t* ctx;
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit g_init;
    
    DEBUGF("load%s", "");

    g_init.dtor = (ErlNifResourceDtor*) glasses_dtor;
    g_init.stop = (ErlNifResourceStop*) NULL;
    g_init.down = (ErlNifResourceDown*) NULL;
    g_init.members = 4;
    g_init.dyncall = (ErlNifResourceDynCall*) glasses_dyncall;

    if ((glasses_r =
	 enif_init_resource_type(env,
				 "glasses",
				 &g_init,
				 ERL_NIF_RT_CREATE, // | ERL_NIF_RT_TAKEOVER,
				 &tried)) == NULL) {
	return -1;
    }

    if ((ctx = (nif_ctx_t*) enif_alloc(sizeof(nif_ctx_t))) == NULL)
	return -1;
    memset(ctx, 0, sizeof(nif_ctx_t));

    ctx->info.applicationId = "Erlang";
    ctx->info.applicationVersion = "27.0.1";
    ctx->info.sdkType = 0;
    ctx->info.reserved = 0;
    
    ctx->platformContext = NULL;
    
    if (t5CreateContext(&ctx->context, &ctx->info, ctx->platformContext) !=
	T5_SUCCESS)
	return -1;
    if (load_atoms(env) < 0)
	return -1;
    *priv_data = ctx;
    return 0;
}

static int tilefive_upgrade(ErlNifEnv* env, void** priv_data,
			void** old_priv_data,
			ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit g_init;
    // nif_ctx_t* ctx = (nif_ctx_t*) *old_priv_data;
    
    DEBUGF("upgrade%s", "");

    g_init.dtor = (ErlNifResourceDtor*) glasses_dtor;
    g_init.stop = (ErlNifResourceStop*) NULL;
    g_init.down = (ErlNifResourceDown*) NULL;
    g_init.members = 4;
    g_init.dyncall = (ErlNifResourceDynCall*) glasses_dyncall;

    if ((glasses_r =
	 enif_init_resource_type(env,
				 "glasses",
				 &g_init,
				 ERL_NIF_RT_CREATE, // | ERL_NIF_RT_TAKEOVER,
				 &tried)) == NULL) {
	return -1;
    }

    if (load_atoms(env) < 0)
	return -1;
    *priv_data = *old_priv_data;
    return 0;
}

static void tilefive_unload(ErlNifEnv* env, void* priv_data)
{
    nif_ctx_t* ctx = (nif_ctx_t*) priv_data;    
    UNUSED(env);
    UNUSED(priv_data);
    DEBUGF("unload%s", "");
    t5DestroyContext(&ctx->context);
    enif_free(ctx);
}

ERL_NIF_INIT(tiltfive,nif_funcs,tilefive_load,NULL,tilefive_upgrade,tilefive_unload)
