(*******************************************************************************
 * Copyright (c) 2008-2009 The Khronos Group Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and/or associated documentation files (the
 * "Materials"), to deal in the Materials without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Materials, and to
 * permit persons to whom the Materials are furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Materials.
 *
 * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
 ******************************************************************************)
(************************************************)
(*                                              *)
(*     OpenCL1.0 and Delphi and Windows         *)
(*                                              *)
(*      created by      : Maksim Tymkovich      *)
(*                           (niello)           *)
(*                                              *)
(*      headers versions: 0.04                  *)
(*      file name       : CL.pas                *)
(*      last modify     : 31.03.10              *)
(*      license         : BSD                   *)
(*                                              *)
(*      Site            : www.niello.org.ua     *)
(*      e-mail          : muxamed13@ukr.net     *)
(*      ICQ             : 446-769-253           *)
(*                                              *)
(*      Fixed by        : Dmitry Belkevich      *)
(*      Site            : www.makhaon.com       *)
(*      e-mail          : dmitry@makhaon.com    *)
(*      ICQ             : 207-381-695           *)
(*                    (c) 2009                  *)
(*                                              *)
(*      updated by      : Alexander Kiselev     *)
(*                          (Igroman)           *)
(*      Site : http://Igroman14.livejournal.com *)
(*      e-mail          : Igroman14@yandex.ru   *)
(*      ICQ             : 207-381-695           *)
(*                    (c) 2010                  *)
(*                                              *)
(***********Copyright (c) niello 2008-2010*******)

{
Comments:
  19.05.2010: ($IFNDEF VER170) include to block $REGION/$ENDREGION.
}

unit CL;

interface
  uses Windows, cl_platform;

{
#ifndef __OPENCL_CL_H
#define __OPENCL_CL_H

#ifdef __APPLE__
#include <OpenCL/cl_platform.h>
#else
#include <CL/cl_platform.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif
}

{$INCLUDE 'OpenCL.inc'}

{$IFDEF DEFINE_8087CW_NOT_IMPLEMENTED}
var
  Default8087CW: Word = $1332;
{$ENDIF}

{$IFNDEF VER170} //
{$REGION 'types'}
{$ENDIF}
type
  TSize_t = Longword;
  PSize_t = ^TSize_t;

  TSizet = array [0..2] of TSize_t;
  PSizet = ^TSizet;

  TCL_platform_id = record
  end;  //typedef struct _cl_platform_id *    cl_platform_id;
  PCL_platform_id = ^TCL_platform_id;
  PPCL_platform_id = ^PCL_platform_id;

  TCL_device_id = record
  end;  //typedef struct _cl_device_id *      cl_device_id;
  PCL_device_id = ^TCL_device_id;
  PPCL_device_id = ^PCL_device_id;

  TCL_context = record
  end;  //typedef struct _cl_context *        cl_context;
  PCL_context = ^TCL_context;

  TCL_command_queue = record
  end;  //typedef struct _cl_command_queue *  cl_command_queue;
  PCL_command_queue = ^TCL_command_queue;

  TCL_mem = record
  end;  //typedef struct _cl_mem *            cl_mem;
  PCL_mem = ^TCL_mem;
  PPCL_mem = ^PCL_mem;

  TCL_program = record
  end;  //typedef struct _cl_program *        cl_program;
  PCL_program = ^TCL_program;

  TCL_kernel = record
  end;  //typedef struct _cl_kernel *         cl_kernel;
  PCL_kernel = ^TCL_kernel;
  PPCL_kernel = ^PCL_kernel;

  TCL_event = record
  end;  //typedef struct _cl_event *          cl_event;
  PCL_event = ^TCL_event;
  PPCL_event = ^PCL_event;

  TCL_sampler = record
  end;  //typedef struct _cl_sampler *        cl_sampler;
  PCL_sampler = ^TCL_sampler;

  TCL_bool = TCL_uint;
  //typedef cl_uint             cl_bool;                     //* WARNING!  Unlike cl_ types in cl_platform.h, cl_bool is not guaranteed to be the same size as the bool in kernels. */
  PCL_bool = ^TCL_bool;

  TCL_bitfield = TCL_ulong;     //typedef cl_ulong            cl_bitfield;
  PCL_bitfield = ^TCL_bitfield;

  TCL_device_type = TCL_bitfield;  //typedef cl_bitfield         cl_device_type;
  PCL_device_type = ^TCL_device_type;

  TCL_platform_info = TCL_uint;      //typedef cl_uint             cl_platform_info;
  PCL_platform_info = ^TCL_platform_info;

  TCL_device_info = TCL_uint;      //typedef cl_uint             cl_device_info;
  PCL_device_info = ^TCL_device_info;

  TCL_device_address_info = TCL_bitfield;  //typedef cl_bitfield         cl_device_address_info;
  PCL_device_address_info = ^TCL_device_address_info;

  TCL_device_fp_config = TCL_bitfield;  //typedef cl_bitfield         cl_device_fp_config;
  PCL_device_fp_config = ^TCL_device_fp_config;

  TCL_device_mem_cache_type = TCL_uint;      //typedef cl_uint             cl_device_mem_cache_type;
  PCL_device_mem_cache_type = ^TCL_device_mem_cache_type;

  TCL_device_local_mem_type = TCL_uint;      //typedef cl_uint             cl_device_local_mem_type;
  PCL_device_local_mem_type = ^TCL_device_local_mem_type;

  TCL_device_exec_capabilities = TCL_bitfield;  //typedef cl_bitfield         cl_device_exec_capabilities;
  PCL_device_exec_capabilities = ^TCL_device_exec_capabilities;

  TCL_command_queue_properties = TCL_bitfield;  //typedef cl_bitfield         cl_command_queue_properties;
  PCL_command_queue_properties = ^TCL_command_queue_properties;

  PCL_context_properties = PInteger;//intptr_t;    //typedef intptr_t      cl_context_properties;
  PPCL_context_properties = ^PCL_context_properties;

  TCL_context_info = TCL_uint;    //typedef cl_uint             cl_context_info;
  PCL_context_info = ^TCL_context_info;

  TCL_command_queue_info = TCL_uint;    //typedef cl_uint             cl_command_queue_info;
  PCL_command_queue_info = ^TCL_command_queue_info;

  TCL_channel_order = TCL_uint;    //typedef cl_uint             cl_channel_order;
  PCL_channel_order = ^TCL_channel_order;

  TCL_channel_type = TCL_uint;    //typedef cl_uint             cl_channel_type;
  PCL_channel_type = ^TCL_channel_type;

  TCL_mem_flags = TCL_bitfield;//typedef cl_bitfield         cl_mem_flags;
  PCL_mem_flags = ^TCL_mem_flags;

  TCL_mem_object_type = TCL_uint;    //typedef cl_uint             cl_mem_object_type;
  PCL_mem_object_type = ^TCL_mem_object_type;

  TCL_mem_info = TCL_uint;    //typedef cl_uint             cl_mem_info;
  PCL_mem_info = ^TCL_mem_info;

  TCL_image_info = TCL_uint;    //typedef cl_uint             cl_image_info;
  PCL_image_info = ^TCL_image_info;

  TCL_addressing_mode = TCL_uint;    //typedef cl_uint             cl_addressing_mode;
  PCL_addressing_mode = ^TCL_addressing_mode;

  TCL_filter_mode = TCL_uint;    //typedef cl_uint             cl_filter_mode;
  PCL_filter_mode = ^TCL_filter_mode;

  TCL_sampler_info = TCL_uint;    //typedef cl_uint             cl_sampler_info;
  PCL_sampler_info = ^TCL_sampler_info;

  TCL_map_flags = TCL_bitfield;//typedef cl_bitfield         cl_map_flags;
  PCL_map_flags = ^TCL_map_flags;

  TCL_program_info = TCL_uint;    //typedef cl_uint             cl_program_info;
  PCL_program_info = ^TCL_program_info;

  TCL_program_build_info = TCL_uint;    //typedef cl_uint             cl_program_build_info;
  PCL_program_build_info = ^TCL_program_build_info;

  TCL_build_status = TCL_int;     //typedef cl_int              cl_build_status;
  PCL_build_status = ^TCL_build_status;

  TCL_kernel_info = TCL_uint;    //typedef cl_uint             cl_kernel_info;
  PCL_kernel_info = ^TCL_kernel_info;

  TCL_kernel_work_group_info = TCL_uint;    //typedef cl_uint             cl_kernel_work_group_info;
  PCL_kernel_work_group_info = ^TCL_kernel_work_group_info;

  TCL_event_info = TCL_uint;    //typedef cl_uint             cl_event_info;
  PCL_event_info = ^TCL_event_info;

  TCL_command_type = TCL_uint;    //typedef cl_uint             cl_command_type;
  PCL_command_type = ^TCL_command_type;

  TCL_profiling_info = TCL_uint;    //typedef cl_uint             cl_profiling_info;
  PCL_profiling_info = ^TCL_profiling_info;

  TCL_image_format = packed record
    Iage_channel_order: TCL_channel_order;
    Image_channel_data_type: TCl_channel_type;
  end;
  PCL_image_format = ^TCL_image_format;
{$IFNDEF VER170}
{$ENDREGION}
//types
{$ENDIF}

{$IFNDEF VER170}
{$REGION 'const'}
{$ENDIF}
const
  // Error Codes
  CL_SUCCESS = 0;
  CL_DEVICE_NOT_FOUND = -1;
  CL_DEVICE_NOT_AVAILABLE = -2;
  CL_COMPILER_NOT_AVAILABLE = -3;
  CL_MEM_OBJECT_ALLOCATION_FAILURE = -4;
  CL_OUT_OF_RESOURCES = -5;
  CL_OUT_OF_HOST_MEMORY = -6;
  CL_PROFILING_INFO_NOT_AVAILABLE = -7;
  CL_MEM_COPY_OVERLAP = -8;
  CL_IMAGE_FORMAT_MISMATCH = -9;
  CL_IMAGE_FORMAT_NOT_SUPPORTED = -10;
  CL_BUILD_PROGRAM_FAILURE = -11;
  CL_MAP_FAILURE = -12;

  CL_INVALID_VALUE = -30;
  CL_INVALID_DEVICE_TYPE = -31;
  CL_INVALID_PLATFORM = -32;
  CL_INVALID_DEVICE = -33;
  CL_INVALID_CONTEXT = -34;
  CL_INVALID_QUEUE_PROPERTIES = -35;
  CL_INVALID_COMMAND_QUEUE = -36;
  CL_INVALID_HOST_PTR = -37;
  CL_INVALID_MEM_OBJECT = -38;
  CL_INVALID_IMAGE_FORMAT_DESCRIPTOR = -39;
  CL_INVALID_IMAGE_SIZE = -40;
  CL_INVALID_SAMPLER = -41;
  CL_INVALID_BINARY = -42;
  CL_INVALID_BUILD_OPTIONS = -43;
  CL_INVALID_PROGRAM = -44;
  CL_INVALID_PROGRAM_EXECUTABLE = -45;
  CL_INVALID_KERNEL_NAME = -46;
  CL_INVALID_KERNEL_DEFINITION = -47;
  CL_INVALID_KERNEL = -48;
  CL_INVALID_ARG_INDEX = -49;
  CL_INVALID_ARG_VALUE = -50;
  CL_INVALID_ARG_SIZE = -51;
  CL_INVALID_KERNEL_ARGS = -52;
  CL_INVALID_WORK_DIMENSION = -53;
  CL_INVALID_WORK_GROUP_SIZE = -54;
  CL_INVALID_WORK_ITEM_SIZE = -55;
  CL_INVALID_GLOBAL_OFFSET = -56;
  CL_INVALID_EVENT_WAIT_LIST = -57;
  CL_INVALID_EVENT = -58;
  CL_INVALID_OPERATION = -59;
  CL_INVALID_GL_OBJECT = -60;
  CL_INVALID_BUFFER_SIZE = -61;
  CL_INVALID_MIP_LEVEL = -62;
  CL_INVALID_GLOBAL_WORK_SIZE = -63;

   // OpenCL Version
  CL_VERSION_1_0 = 1;

   //CL_bool
  CL_FALSE = 0;
  CL_TRUE = 1;

   //CL_platform_info
  CL_PLATFORM_PROFILE = $0900;
  CL_PLATFORM_VERSION = $0901;
  CL_PLATFORM_NAME = $0902;
  CL_PLATFORM_VENDOR = $0903;
  CL_PLATFORM_EXTENSIONS = $0904;

   //CL_device_type - bitfield
  CL_DEVICE_TYPE_DEFAULT = (1 shl 0);
  CL_DEVICE_TYPE_CPU = (1 shl 1);
  CL_DEVICE_TYPE_GPU = (1 shl 2);
  CL_DEVICE_TYPE_ACCELERATOR = (1 shl 3);
  CL_DEVICE_TYPE_ALL = $FFFFFFFF;

   //CL_device_info
  CL_DEVICE_TYPE = $1000;
  CL_DEVICE_VENDOR_ID = $1001;
  CL_DEVICE_MAX_COMPUTE_UNITS = $1002;
  CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS = $1003;
  CL_DEVICE_MAX_WORK_GROUP_SIZE = $1004;
  CL_DEVICE_MAX_WORK_ITEM_SIZES = $1005;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR = $1006;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT = $1007;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT = $1008;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG = $1009;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT = $100A;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE = $100B;
  CL_DEVICE_MAX_CLOCK_FREQUENCY = $100C;
  CL_DEVICE_ADDRESS_BITS = $100D;
  CL_DEVICE_MAX_READ_IMAGE_ARGS = $100E;
  CL_DEVICE_MAX_WRITE_IMAGE_ARGS = $100F;
  CL_DEVICE_MAX_MEM_ALLOC_SIZE = $1010;
  CL_DEVICE_IMAGE2D_MAX_WIDTH = $1011;
  CL_DEVICE_IMAGE2D_MAX_HEIGHT = $1012;
  CL_DEVICE_IMAGE3D_MAX_WIDTH = $1013;
  CL_DEVICE_IMAGE3D_MAX_HEIGHT = $1014;
  CL_DEVICE_IMAGE3D_MAX_DEPTH = $1015;
  CL_DEVICE_IMAGE_SUPPORT = $1016;
  CL_DEVICE_MAX_PARAMETER_SIZE = $1017;
  CL_DEVICE_MAX_SAMPLERS = $1018;
  CL_DEVICE_MEM_BASE_ADDR_ALIGN = $1019;
  CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE = $101A;
  CL_DEVICE_SINGLE_FP_CONFIG = $101B;
  CL_DEVICE_GLOBAL_MEM_CACHE_TYPE = $101C;
  CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE = $101D;
  CL_DEVICE_GLOBAL_MEM_CACHE_SIZE = $101E;
  CL_DEVICE_GLOBAL_MEM_SIZE = $101F;
  CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE = $1020;
  CL_DEVICE_MAX_CONSTANT_ARGS = $1021;
  CL_DEVICE_LOCAL_MEM_TYPE = $1022;
  CL_DEVICE_LOCAL_MEM_SIZE = $1023;
  CL_DEVICE_ERROR_CORRECTION_SUPPORT = $1024;
  CL_DEVICE_PROFILING_TIMER_RESOLUTION = $1025;
  CL_DEVICE_ENDIAN_LITTLE = $1026;
  CL_DEVICE_AVAILABLE = $1027;
  CL_DEVICE_COMPILER_AVAILABLE = $1028;
  CL_DEVICE_EXECUTION_CAPABILITIES = $1029;
  CL_DEVICE_QUEUE_PROPERTIES = $102A;
  CL_DEVICE_NAME = $102B;
  CL_DEVICE_VENDOR = $102C;
  CL_DRIVER_VERSION = $102D;
  CL_DEVICE_PROFILE = $102E;
  CL_DEVICE_VERSION = $102F;
  CL_DEVICE_EXTENSIONS = $1030;
  CL_DEVICE_PLATFORM = $1031;

   //CL_device_fp_config - bitfield
  CL_FP_DENORM = (1 shl 0);
  CL_FP_INF_NAN = (1 shl 1);
  CL_FP_ROUND_TO_NEAREST = (1 shl 2);
  CL_FP_ROUND_TO_ZERO = (1 shl 3);
  CL_FP_ROUND_TO_INF = (1 shl 4);
  CL_FP_FMA = (1 shl 5);

   //CL_device_mem_cache_type
  CL_NONE = $0;
  CL_READ_ONLY_CACHE = $1;
  CL_READ_WRITE_CACHE = $2;

   //CL_device_local_mem_type
  CL_LOCAL = $1;
  CL_GLOBAL = $2;

   //CL_device_exec_capabilities - bitfield
  CL_EXEC_KERNEL = (1 shl 0);
  CL_EXEC_NATIVE_KERNEL = (1 shl 1);

   //CL_command_queue_properties - bitfield
  CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE = (1 shl 0);
  CL_QUEUE_PROFILING_ENABLE = (1 shl 1);

   //CL_context_info
  CL_CONTEXT_REFERENCE_COUNT = $1080;
  CL_CONTEXT_DEVICES = $1081;
  CL_CONTEXT_PROPERTIES = $1082;

   //CL_context_properties
  CL_CONTEXT_PLATFORM = $1084;

   //CL_command_queue_info
  CL_QUEUE_CONTEXT = $1090;
  CL_QUEUE_DEVICE = $1091;
  CL_QUEUE_REFERENCE_COUNT = $1092;
  CL_QUEUE_PROPERTIES = $1093;

   //CL_mem_flags - bitfield
  CL_MEM_READ_WRITE = (1 shl 0);
  CL_MEM_WRITE_ONLY = (1 shl 1);
  CL_MEM_READ_ONLY = (1 shl 2);
  CL_MEM_USE_HOST_PTR = (1 shl 3);
  CL_MEM_ALLOC_HOST_PTR = (1 shl 4);
  CL_MEM_COPY_HOST_PTR = (1 shl 5);

   //CL_channel_order
  CL_R = $10B0;
  CL_A = $10B1;
  CL_RG = $10B2;
  CL_RA = $10B3;
  CL_RGB = $10B4;
  CL_RGBA = $10B5;
  CL_BGRA = $10B6;
  CL_ARGB = $10B7;
  CL_INTENSITY = $10B8;
  CL_LUMINANCE = $10B9;

   //CL_channel_type
  CL_SNORM_INT8 = $10D0;
  CL_SNORM_INT16 = $10D1;
  CL_UNORM_INT8 = $10D2;
  CL_UNORM_INT16 = $10D3;
  CL_UNORM_SHORT_565 = $10D4;
  CL_UNORM_SHORT_555 = $10D5;
  CL_UNORM_INT_101010 = $10D6;
  CL_SIGNED_INT8 = $10D7;
  CL_SIGNED_INT16 = $10D8;
  CL_SIGNED_INT32 = $10D9;
  CL_UNSIGNED_INT8 = $10DA;
  CL_UNSIGNED_INT16 = $10DB;
  CL_UNSIGNED_INT32 = $10DC;
  CL_HALF_FLOAT = $10DD;
  CL_FLOAT = $10DE;

   //CL_mem_object_type
  CL_MEM_OBJECT_BUFFER = $10F0;
  CL_MEM_OBJECT_IMAGE2D = $10F1;
  CL_MEM_OBJECT_IMAGE3D = $10F2;

   //CL_mem_info
  CL_MEM_TYPE = $1100;
  CL_MEM_FLAGS = $1101;
  CL_MEM_SIZE = $1102;
  CL_MEM_HOST_PTR = $1103;
  CL_MEM_MAP_COUNT = $1104;
  CL_MEM_REFERENCE_COUNT = $1105;
  CL_MEM_CONTEXT = $1106;

   //CL_image_info
  CL_IMAGE_FORMAT = $1110;
  CL_IMAGE_ELEMENT_SIZE = $1111;
  CL_IMAGE_ROW_PITCH = $1112;
  CL_IMAGE_SLICE_PITCH = $1113;
  CL_IMAGE_WIDTH = $1114;
  CL_IMAGE_HEIGHT = $1115;
  CL_IMAGE_DEPTH = $1116;

   //CL_addressing_mode
  CL_ADDRESS_NONE = $1130;
  CL_ADDRESS_CLAMP_TO_EDGE = $1131;
  CL_ADDRESS_CLAMP = $1132;
  CL_ADDRESS_REPEAT = $1133;

   //CL_filter_mode
  CL_FILTER_NEAREST = $1140;
  CL_FILTER_LINEAR = $1141;

   //CL_sampler_info
  CL_SAMPLER_REFERENCE_COUNT = $1150;
  CL_SAMPLER_CONTEXT = $1151;
  CL_SAMPLER_NORMALIZED_COORDS = $1152;
  CL_SAMPLER_ADDRESSING_MODE = $1153;
  CL_SAMPLER_FILTER_MODE = $1154;

   //CL_map_flags - bitfield
  CL_MAP_READ = (1 shl 0);
  CL_MAP_WRITE = (1 shl 1);

   //CL_program_info
  CL_PROGRAM_REFERENCE_COUNT = $1160;
  CL_PROGRAM_CONTEXT = $1161;
  CL_PROGRAM_NUM_DEVICES = $1162;
  CL_PROGRAM_DEVICES = $1163;
  CL_PROGRAM_SOURCE = $1164;
  CL_PROGRAM_BINARY_SIZES = $1165;
  CL_PROGRAM_BINARIES = $1166;

   //CL_program_build_info
  CL_PROGRAM_BUILD_STATUS = $1181;
  CL_PROGRAM_BUILD_OPTIONS = $1182;
  CL_PROGRAM_BUILD_LOG = $1183;

   //CL_build_status
  CL_BUILD_SUCCESS = 0;
  CL_BUILD_NONE = -1;
  CL_BUILD_ERROR = -2;
  CL_BUILD_IN_PROGRESS = -3;

   //CL_kernel_info
  CL_KERNEL_FUNCTION_NAME = $1190;
  CL_KERNEL_NUM_ARGS = $1191;
  CL_KERNEL_REFERENCE_COUNT = $1192;
  CL_KERNEL_CONTEXT = $1193;
  CL_KERNEL_PROGRAM = $1194;

   //CL_kernel_work_group_info
  CL_KERNEL_WORK_GROUP_SIZE = $11B0;
  CL_KERNEL_COMPILE_WORK_GROUP_SIZE = $11B1;
  CL_KERNEL_LOCAL_MEM_SIZE = $11B2;

   //CL_event_info
  CL_EVENT_COMMAND_QUEUE = $11D0;
  CL_EVENT_COMMAND_TYPE = $11D1;
  CL_EVENT_REFERENCE_COUNT = $11D2;
  CL_EVENT_COMMAND_EXECUTION_STATUS = $11D3;

   //CL_command_type
  CL_COMMAND_NDRANGE_KERNEL = $11F0;
  CL_COMMAND_TASK = $11F1;
  CL_COMMAND_NATIVE_KERNEL = $11F2;
  CL_COMMAND_READ_BUFFER = $11F3;
  CL_COMMAND_WRITE_BUFFER = $11F4;
  CL_COMMAND_COPY_BUFFER = $11F5;
  CL_COMMAND_READ_IMAGE = $11F6;
  CL_COMMAND_WRITE_IMAGE = $11F7;
  CL_COMMAND_COPY_IMAGE = $11F8;
  CL_COMMAND_COPY_IMAGE_TO_BUFFER = $11F9;
  CL_COMMAND_COPY_BUFFER_TO_IMAGE = $11FA;
  CL_COMMAND_MAP_BUFFER = $11FB;
  CL_COMMAND_MAP_IMAGE = $11FC;
  CL_COMMAND_UNMAP_MEM_OBJECT = $11FD;
  CL_COMMAND_MARKER = $11FE;
  CL_COMMAND_ACQUIRE_GL_OBJECTS = $11FF;
  CL_COMMAND_RELEASE_GL_OBJECTS = $1200;

   // command execution status
  CL_COMPLETE = $0;
  CL_RUNNING = $1;
  CL_SUBMITTED = $2;
  CL_QUEUED = $3;

   //CL_profiling_info
  CL_PROFILING_COMMAND_QUEUED = $1280;
  CL_PROFILING_COMMAND_SUBMIT = $1281;
  CL_PROFILING_COMMAND_START = $1282;
  CL_PROFILING_COMMAND_END = $1283;
{$IFNDEF VER170}
{$ENDREGION}
//const
{$ENDIF}


{$IFNDEF VER170}
{$REGION 'Types proceduress'}
{$ENDIF}
type
// Platform API
//extern CL_API_ENTRY cl_int CL_API_CALL
  TclGetPlatformIDs = function (num_entries: TCL_uint;          (* num_entries *)
                                 platforms: PPCL_platform_id;                             (* platforms *)
                                 num_platforms: PCL_uint                                  (* num_platforms *)
                                 ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


  TclGetPlatformInfo = function (platform: PCL_platform_id      (* platform *);
                                   param_name: TCL_platform_info                            (* param_name *);
                                   param_value_size: TSize_t                                (* param_value_size *);
                                   param_value: Pointer                                     (* param_value *);
                                   param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                   ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Device APIs
  TclGetDeviceIDs = function (platform: PCL_platform_id         (* platform *);
                                 device_type: TCL_device_type                             (* device_type *);
                                 num_entries: TCL_uint                                    (* num_entries *);
                                 devices: PPCL_device_id                                  (* devices *);
                                 num_devices: PCL_uint                                    (* num_devices *)
                                 ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetDeviceInfo = function (device: PCL_device_id            (* device *);
                                 param_name: TCL_device_info                              (* param_name *);
                                 param_value_size: TSize_t                                (* param_value_size *);
                                 param_value: Pointer                                     (* param_value *);
                                 param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                 ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Context APIs
 TContextNotify = procedure(const Name: PAnsiChar; const Data: Pointer; Size: TSize_t; Data2: Pointer); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  TclCreateContext = function (const properties: PPCL_context_properties(* properties *);
                                 num_devices: TCL_uint                                    (* num_devices *);
                                 const devices: PPCL_device_id                            (* devices *);
                                 pfn_notify: TContextNotify                               (*pfn_notify*);
                                 user_data: Pointer                                       (* user_data *);
                                 errcode_ret: PCL_int                                     (* errcode_ret *)
                                 ): PCL_context; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclCreateContextFromType = function (const properties: PPCL_context_properties(* properties *);
                                         device_type: TCL_device_type                             (* device_type *);
                                         pfn_notify: TContextNotify                               (*pfn_notify*);
                                         user_data: Pointer                                       (* user_data *);
                                         errcode_ret: PCL_int                                     (* errcode_ret *)
                                         ): PCL_context; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclRetainContext = function (context: PCL_context): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclReleaseContext = function (context: PCL_context): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetContextInfo = function (context: PCL_context            (* context *);
                                 param_name: TCL_context_info                             (* param_name *);
                                 param_value_size: TSize_t                                (* param_value_size *);
                                 param_value: Pointer                                     (* param_value *);
                                 param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                 ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Command Queue APIs

  TclCreateCommandQueue = function (context: PCL_context        (* context *);
                                     device: PCL_device_id                                    (* device *);
                                     properties: TCL_command_queue_properties                 (* properties *);
                                     errcode_ret: PCL_int                                     (* errcode_ret *)
                                     ): PCL_command_queue; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclRetainCommandQueue = function (command_queue: PCL_command_queue): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclReleaseCommandQueue = function (command_queue: PCL_command_queue): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetCommandQueueInfo = function (command_queue: PCL_command_queue(* command_queue *);
                                       param_name: TCL_command_queue_info                       (* param_name *);
                                       param_value_size: TSize_t                                (* param_value_size *);
                                       param_value: Pointer                                     (* param_value *);
                                       param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                       ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclSetCommandQueueProperty = function (command_queue: PCL_command_queue(* command_queue *);
                                           properties: TCL_command_queue_properties                 (* properties *);
                                           enable: TCL_bool                                         (* enable *);
                                           old_properties: PCL_command_queue_properties             (* old_properties *)
                                           ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Memory Object APIs
  TclCreateBuffer = function (context: PCL_context              (* context *);
                               flags: TCL_mem_flags                                     (* flags *);
                               size: TSize_t                                            (* size *);
                               host_ptr: Pointer                                        (* host_ptr *);
                               errcode_ret: PCL_int                                     (* errcode_ret *)
                               ): PCL_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclCreateImage2D = function (context: PCL_context             (* context *);
                                 flags: TCL_mem_flags                                     (* flags *);
                                 const image_format: PCL_image_format                     (* image_format *);
                                 image_width: TSize_t                                     (* image_width *);
                                 image_height: TSize_t                                    (* image_height *);
                                 image_row_pitch: TSize_t                                 (* image_row_pitch *);
                                 host_ptr: Pointer                                        (* host_ptr *);
                                 errcode_ret: PCL_int                                     (* errcode_ret *)
                                 ): PCL_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclCreateImage3D = function (context: PCL_context             (* context *);
                                 flags: TCL_mem_flags                                     (* flags *);
                                 const image_format: PCL_image_format                     (* image_format *);
                                 image_width: TSize_t                                     (* image_width *);
                                 image_height: TSize_t                                    (* image_height *);
                                 image_depth: TSize_t                                     (* image_depth *);
                                 image_row_pitch: TSize_t                                 (* image_row_pitch *);
                                 image_slice_pitch: TSize_t                               (* image_slice_pitch *);
                                 host_ptr: Pointer                                        (* host_ptr *);
                                 errcode_ret: PCL_int                                     (* errcode_ret *)
                                 ): PCL_mem; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclRetainMemObject = function (memobj: PCL_mem): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclReleaseMemObject = function (memobj: PCL_mem): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetSupportedImageFormats = function (context: PCL_context  (* context *);
                                           flags: TCL_mem_flags                                     (* flags *);
                                           image_type: TCL_mem_object_type                          (* image_type *);
                                           num_entries: TCL_uint                                    (* num_entries *);
                                           image_formats: PCL_image_format                          (* image_formats *);
                                           num_image_formats: PCL_uint                              (* num_image_formats *)
                                           ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetMemObjectInfo = function (memobj: PCL_mem               (* memobj *);
                                   param_name: TCL_mem_info                                 (* param_name *);
                                   param_value_size: TSize_t                                (* param_value_size *);
                                   param_value: Pointer                                     (* param_value *);
                                   param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                   ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetImageInfo = function (image: PCL_mem                    (* image *);
                                 param_name: TCL_image_info                               (* param_name *);
                                 param_value_size: TSize_t                                (* param_value_size *);
                                 param_value: Pointer                                     (* param_value *);
                                 param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                 ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Sampler APIs
  TclCreateSampler = function (context: PCL_context;
                                normalized_coords: TCL_bool;
                                addressing_mode: TCL_addressing_mode;
                                filter_mode: TCL_filter_mode;
                                errcode_ret: PCL_int): PCL_sampler; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclRetainSampler = function (sampler: PCL_sampler): TCL_sampler; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclReleaseSampler = function (sampler: PCL_sampler): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetSamplerInfo = function (sampler: PCL_sampler            (* sampler *);
                                 param_name: TCL_sampler_info                             (* param_name *);
                                 param_value_size: TSize_t                                (* param_value_size *);
                                 param_value: Pointer                                     (* param_value *);
                                 param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                 ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Program Object APIs
  TclCreateProgramWithSource = function (context: PCL_context   (* context *);
                                           Count: TCL_uint                                          (* count *);
                                           const strings: PPAnsiChar                                (* strings *);
                                           const lengths: PSize_t                                   (* lengths *);
                                           errcode_ret: PCL_int                                     (* errcode_ret *)
                                           ): PCL_program; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

type
 PPByte = ^PByte;

  TclCreateProgramWithBinary = function (context: PCL_context   (* context *);
                                           num_devices: TCL_uint                                    (* num_devices *);
                                           const device_list: PCL_device_id                         (* device_list *);
                                           const lengths: PSize_t                                   (* lengths *);
                                           const binaries: PPByte                                   {unsigned char **}  (* binaries *);
                                           binary_status: PCL_int                                   (* binary_status *);
                                           errcode_ret: PCL_int                                     (* errcode_ret *)
                                           ): PCL_program; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclRetainProgram = function (_program: PCL_program): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclReleaseProgram = function (_program: PCL_program): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

type
  TProgramNotify = procedure(_program: PCL_program; user_data: Pointer); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  TclBuildProgram = function (_program: PCL_program             (* program *);
                               num_devices: TCL_uint                                    (* num_devices *);
                               const device_list: PPCL_device_id                        (* device_list *);
                               const options: PAnsiChar                                 (* options *);
                               pfn_notify: TProgramNotify                               (* void (pfn_notify)*);
                               user_data: Pointer                                       (* user_data *)
                               ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclUnloadCompiler = function: TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetProgramInfo = function (_program: PCL_program           (* program *);
                                   param_name: TCL_program_info                             (* param_name *);
                                   param_value_size: TSize_t                                (* param_value_size *);
                                   param_value: Pointer                                     (* param_value *);
                                   param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                   ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


  TclGetProgramBuildInfo = function (_program: PCL_program      (* program *);
                                       device: PCL_device_id                                    (* device *);
                                       param_name: TCL_program_build_info                       (* param_name *);
                                       param_value_size: TSize_t                                (* param_value_size *);
                                       param_value: Pointer                                     (* param_value *);
                                       param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                       ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Kernel Object APIs
  TclCreateKernel = function (_program: PCL_program             (* program *);
                               const kernel_name: PAnsiChar                             (* kernel_name *);
                               errcode_ret: PCL_int                                     (* errcode_ret *)
                               ): PCL_kernel; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclCreateKernelsInProgram = function (_program: PCL_program   (* program *);
                                         num_kernels: TCL_uint                                    (* num_kernels *);
                                         kernels: PPCL_kernel                                     (* kernels *);
                                         num_kernels_ret: PCL_uint                                (* num_kernels_ret *)
                                         ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclRetainKernel = function (kernel: PCL_kernel): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclReleaseKernel = function (kernel: PCL_kernel): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclSetKernelArg = function (kernel: PCL_kernel                (* kernel *);
                               arg_index: TCL_uint                                      (* arg_index *);
                               arg_size: TSize_t                                        (* arg_size *);
                               const arg_value: Pointer                                 (* arg_value *)
                               ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetKernelInfo = function (kernel: PCL_kernel               (* kernel *);
                                 param_name: TCL_kernel_info                              (* param_name *);
                                 param_value_size: TSize_t                                (* param_value_size *);
                                 param_value: Pointer                                     (* param_value *);
                                 param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                 ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetKernelWorkGroupInfo = function (kernel: PCL_kernel      (* kernel *);
                                         device: PCL_device_id                                    (* device *);
                                         param_name: TCL_kernel_work_group_info                   (* param_name *);
                                         param_value_size: TSize_t                                (* param_value_size *);
                                         param_value: Pointer                                     (* param_value *);
                                         param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                         ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Event Object APIs
  TclWaitForEvents = function (num_events: TCL_uint             (* num_events *);
                                 const event_list: PCL_event                              (* event_list *)
                                 ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclGetEventInfo = function (event: PCL_event                  (* event *);
                               param_name: PCL_event_info                               (* param_name *);
                               param_value_size: TSize_t                                (* param_value_size *);
                               param_value: Pointer                                     (* param_value *);
                               param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                               ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclRetainEvent = function (event: PCL_event): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclReleaseEvent = function (event: PCL_event): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Profiling APIs
  TclGetEventProfilingInfo = function (event: PCL_event         (* event *);
                                         param_name: TCL_profiling_info                           (* param_name *);
                                         param_value_size: TSize_t                                (* param_value_size *);
                                         param_value: Pointer                                     (* param_value *);
                                         param_value_size_ret: PSize_t                            (* param_value_size_ret *)
                                         ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Flush and Finish APIs
  TclFlush = function (command_queue: PCL_command_queue): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclFinish = function (command_queue: PCL_command_queue): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Enqueued Commands APIs
  TclEnqueueReadBuffer = function (command_queue: PCL_command_queue(* command_queue *);
                                     buffer: PCL_mem                                          (* buffer *);
                                     blocking_read: TCL_bool                                  (* blocking_read *);
                                     offset: TSize_t                                          (* offset *);
                                     cb: TSize_t                                              (* cb *);
                                     ptr: Pointer                                             (* ptr *);
                                     num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                     const event_wait_list: PPCL_event                        (* event_wait_list *);
                                     event: PPCL_event                                        (* event *)
                                     ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueWriteBuffer = function (command_queue: PCL_command_queue(* command_queue *);
                                     buffer: PCL_mem                                          (* buffer *);
                                     blocking_write: TCL_bool                                 (* blocking_write *);
                                     offset: TSize_t                                          (* offset *);
                                     cb: TSize_t                                              (* cb *);
                                     const ptr: Pointer                                       (* ptr *);
                                     num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                     const event_wait_list: PPCL_event                        (* event_wait_list *);
                                     event: PPCL_event                                        (* event *)
                                     ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueCopyBuffer = function (command_queue: PCL_command_queue(* command_queue *);
                                     src_buffer: PCL_mem                                      (* src_buffer *);
                                     dst_buffer: PCL_mem                                      (* dst_buffer *);
                                     src_offset: TSize_t                                      (* src_offset *);
                                     dst_offset: TSize_t                                      (* dst_offset *);
                                     cb: TSize_t                                              (* cb *);
                                     num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                     const event_wait_list: PPCL_event                        (* event_wait_list *);
                                     event: PPCL_event                                        (* event *)
                                     ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueReadImage = function (command_queue: PCL_command_queue(* command_queue *);
                                     image: PCL_mem                                           (* image *);
                                     blocking_read: TCL_bool                                  (* blocking_read *);
                                     const origin: PSizet                                     (* origin[3] *);
                                     const region: PSizet                                     (* region[3] *);
                                     row_pitch: TSize_t                                       (* row_pitch *);
                                     slice_pitch: TSize_t                                     (* slice_pitch *);
                                     ptr: Pointer                                             (* ptr *);
                                     num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                     const event_wait_list: PPCL_event                        (* event_wait_list *);
                                     event: PPCL_event                                        (* event *)
                                     ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueWriteImage = function (command_queue: PCL_command_queue(* command_queue *);
                                     image: PCL_mem                                           (* image *);
                                     blocking_write: TCL_bool                                 (* blocking_write *);
                                     const origin: PSizet                                     (* [3] *);
                                     const region: PSizet                                     (* [3] *);
                                     input_row_pitch: TSize_t                                 (* input_row_pitch *);
                                     input_slice_pitch: TSize_t                               (* input_slice_pitch *);
                                     const ptr: Pointer                                       (* ptr *);
                                     num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                     const event_wait_list: PPCL_event                        (* event_wait_list *);
                                     event: PPCL_event                                        (* event *)
                                     ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueCopyImage = function (command_queue: PCL_command_queue(* command_queue *);
                                     src_image: PCL_mem                                       (* src_image *);
                                     dst_image: PCL_mem                                       (* dst_image *);
                                     const src_origin: PSizet                                 (* src_origin[3] *);
                                     const dst_origin: PSizet                                 (* dst_origin[3] *);
                                     const region: PSizet                                     (* region[3] *);
                                     num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                     const event_wait_list: PPCL_event                        (* event_wait_list *);
                                     event: PPCL_event                                        (* event *)
                                     ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueCopyImageToBuffer = function (command_queue: PCL_command_queue(* command_queue *);
                                           src_image: PCL_mem                                       (* src_image *);
                                           dst_buffer: PCL_mem                                      (* dst_buffer *);
                                           const src_origin: PSizet                                 (* [3] *);
                                           const region: PSizet                                     (* [3] *);
                                           dst_offset: TSize_t                                      (* dst_offset *);
                                           num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                           const event_wait_list: PPCL_event                        (* event_wait_list *);
                                           event: PPCL_event                                        (* event *)
                                           ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueCopyBufferToImage = function (command_queue: PCL_command_queue(* command_queue *);
                                           src_buffer: PCL_mem                                      (* src_buffer *);
                                           dst_image: PCL_mem                                       (* dst_image *);
                                           src_offset: TSize_t                                      (* src_offset *);
                                           const dst_origin: PSizet                                 (* [3] *);
                                           const region: PSizet                                     (* [3] *);
                                           num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                           const event_wait_list: PPCL_event                        (* event_wait_list *);
                                           event: PPCL_event                                        (* event *)
                                           ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueMapBuffer = function (command_queue: PCL_command_queue(* command_queue *);
                                     buffer: PCL_mem                                          (* buffer *);
                                     blocking_map: TCL_bool                                   (* blocking_map *);
                                     map_flags: TCL_map_flags                                 (* map_flags *);
                                     offset: TSize_t                                          (* offset *);
                                     cb: TSize_t                                              (* cb *);
                                     num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                     const event_wait_list: PPCL_event                        (* event_wait_list *);
                                     event: PPCL_event                                        (* event *);
                                     errcode_ret: PCL_int                                     (* errcode_ret *)
                                     ): Pointer; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueMapImage = function (command_queue: PCL_command_queue(* command_queue *);
                                   image: PCL_mem                                           (* image *);
                                   blocking_map: TCL_bool                                   (* blocking_map *);
                                   map_flags: TCL_map_flags                                 (* map_flags *);
                                   const origin: PSizet                                     (* [3] *);
                                   const region: PSizet                                     (* region[3] *);
                                   image_row_pitch: PSize_t                                 (* image_row_pitch *);
                                   image_slice_pitch: PSize_t                               (* image_slice_pitch *);
                                   num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                   const event_wait_list: PPCL_event                        (* event_wait_list *);
                                   event: PPCL_event                                        (* event *);
                                   errcode_ret: PCL_int                                     (* errcode_ret *)
                                   ): Pointer; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueUnmapMemObject = function (command_queue: PCL_command_queue(* command_queue *);
                                         memobj: PCL_mem                                          (* memobj *);
                                         mapped_ptr: Pointer                                      (* mapped_ptr *);
                                         num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                         const event_wait_list: PPCL_event                        (* event_wait_list *);
                                         event: PPCL_event                                        (* event *)
                                         ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueNDRangeKernel = function (command_queue: PCL_command_queue(* command_queue *);
                                       kernel: PCL_kernel                                       (* kernel *);
                                       work_dim: TCL_uint                                       (* work_dim *);
                                       const global_work_offset: PSize_t                        (* global_work_offset *);
                                       const global_work_size: PSize_t                          (* global_work_size *);
                                       const local_work_size: PSize_t                           (* local_work_size *);
                                       num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                       const event_wait_list: PPCL_event                        (* event_wait_list *);
                                       event: PPCL_event                                        (* event *)
                                       ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueTask = function (command_queue: PCL_command_queue   (* command_queue *);
                               kernel: PCL_kernel                                       (* kernel *);
                               num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                               const event_wait_list: PPCL_event                        (* event_wait_list *);
                               event: PPCL_event                                        (* event *)
                               ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

type
  TEnqueueUserProc = procedure(userdata: Pointer); {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};

  TclEnqueueNativeKernel = function (command_queue: PCL_command_queue;(* command_queue *)
                                       user_func: TEnqueueUserProc;                             (**)
                                       args: Pointer                                            (* args *);
                                       cb_args: TSize_t                                         (* cb_args *);
                                       num_mem_objects: TCL_uint                                (* num_mem_objects *);
                                       const mem_list: PPCL_mem                                 (* mem_list *);
                                       const args_mem_loc: PPointer                             (* args_mem_loc *);
                                       num_events_in_wait_list: TCL_uint                        (* num_events_in_wait_list *);
                                       const event_wait_list: PPCL_event                        (* event_wait_list *);
                                       event: PPCL_event                                        (* event *)
                                       ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueMarker = function (command_queue: PCL_command_queue (* command_queue *);
                                 event: PPCL_event                                        (* event *)
                                 ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueWaitForEvents = function (command_queue: PCL_command_queue(* command_queue *);
                                         num_events: TCL_uint                                     (* num_events *);
                                         const event_list: PPCL_event                             (* event_list *)
                                         ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

  TclEnqueueBarrier = function (command_queue: PCL_command_queue (* command_queue *) ): TCL_int; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

// Extension function access
//
// Returns the extension function address for the given function name;
// or NULL if a valid function can not be found.  The client must
// check to make sure the address is not NULL; before using or
// calling the returned function address.
//


//extern CL_API_ENTRY void * CL_API_CALL
  TclGetExtensionFunctionAddress = function (const func_name: PAnsiChar): Pointer; {$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

//#ifdef __cplusplus
//}
//#endif

//#endif  // __OPENCL_CL_H
{$IFNDEF VER170}
{$ENDREGION}
{$ENDIF}

{$IFNDEF VER170}
{$REGION 'Var'}
{$ENDIF}
var
//Platform API
  clGetPlatformIDs:   TclGetPlatformIDs;
  clGetPlatformInfo:  TclGetPlatformInfo;

//Device APIs
  clGetDeviceIDs:     TclGetDeviceIDs;
  clGetDeviceInfo:    TclGetDeviceInfo;

//Context APIs
  clCreateContext:          TclCreateContext;
  clCreateContextFromType:  TclCreateContextFromType;
  clRetainContext:          TclRetainContext;
  clReleaseContext:         TclReleaseContext;
  clGetContextInfo:         TclGetContextInfo;

//Command Queue APIs
  clCreateCommandQueue:       TclCreateCommandQueue;
  clRetainCommandQueue:       TclRetainCommandQueue;
  clReleaseCommandQueue:      TclReleaseCommandQueue;
  clGetCommandQueueInfo:      TclGetCommandQueueInfo;
  clSetCommandQueueProperty:  TclSetCommandQueueProperty;

//Memory Object APIs
  clCreateBuffer:             TclCreateBuffer;
  clCreateImage2D:            TclCreateImage2D;
  clCreateImage3D:            TclCreateImage3D;
  clRetainMemObject:          TclRetainMemObject;
  clReleaseMemObject:         TclReleaseMemObject;
  clGetSupportedImageFormats: TclGetSupportedImageFormats;
  clGetMemObjectInfo:         TclGetMemObjectInfo;
  clGetImageInfo:             TclGetImageInfo;

//Sampler APIs
  clCreateSampler:            TclCreateSampler;
  clRetainSampler:            TclRetainSampler;
  clReleaseSampler:           TclReleaseSampler;
  clGetSamplerInfo:           TclGetSamplerInfo;

//Program Object APIs
  clCreateProgramWithSource:  TclCreateProgramWithSource;
  clCreateProgramWithBinary:  TclCreateProgramWithBinary;
  clRetainProgram:            TclRetainProgram;
  clReleaseProgram:           TclReleaseProgram;

  clBuildProgram:         TclBuildProgram;

  clUnloadCompiler:       TclUnloadCompiler;

  clGetProgramInfo:       TclGetProgramInfo;


  clGetProgramBuildInfo:  TclGetProgramBuildInfo;

//Kernel Object APIs
  clCreateKernel:             TclCreateKernel;
  clCreateKernelsInProgram:   TclCreateKernelsInProgram;

  clRetainKernel:   TclRetainKernel;
  clReleaseKernel:  TclReleaseKernel;
  clSetKernelArg:   TclSetKernelArg;
  clGetKernelInfo:  TclGetKernelInfo;
  clGetKernelWorkGroupInfo: TclGetKernelWorkGroupInfo;

//Event Object APIs
  clWaitForEvents:  TclWaitForEvents;
  clGetEventInfo:   TclGetEventInfo;
  clRetainEvent:    TclRetainEvent;
  clReleaseEvent:   TclReleaseEvent;

//Profiling APIs
  clGetEventProfilingInfo:  TclGetEventProfilingInfo;

//Flush and Finish APIs
  clFlush:  TclFlush;
  clFinish: TclFinish;
//Enqueued Commands APIs
  clEnqueueReadBuffer:  TclEnqueueReadBuffer;
  clEnqueueWriteBuffer: TclEnqueueWriteBuffer;
  clEnqueueCopyBuffer:  TclEnqueueCopyBuffer;
  clEnqueueReadImage:   TclEnqueueReadImage;
  clEnqueueWriteImage:  TclEnqueueWriteImage;
  clEnqueueCopyImage:   TclEnqueueCopyImage;
  clEnqueueCopyImageToBuffer: TclEnqueueCopyImageToBuffer;
  clEnqueueCopyBufferToImage: TclEnqueueCopyBufferToImage;
  clEnqueueMapBuffer:   TclEnqueueMapBuffer;
  clEnqueueMapImage:    TclEnqueueMapImage;
  clEnqueueUnmapMemObject:    TclEnqueueUnmapMemObject;
  clEnqueueNDRangeKernel:     TclEnqueueNDRangeKernel;
  clEnqueueTask:          TclEnqueueTask;
  clEnqueueNativeKernel:  TclEnqueueNativeKernel;
  clEnqueueMarker:        TclEnqueueMarker;
  clEnqueueWaitForEvents: TclEnqueueWaitForEvents;
  clEnqueueBarrier:       TclEnqueueBarrier;

//Extension function access
  clGetExtensionFunctionAddress:  TclGetExtensionFunctionAddress;
{$IFNDEF VER170}
{$ENDREGION}
//var
{$ENDIF}

function oclGetProcAddress(ProcName: PAnsiChar; LibHandle: Pointer = nil): Pointer;
function InitOpenCL(LibName: AnsiString): Boolean;
function GetString(const Status: TCL_int): AnsiString;

var
  OCL_LibHandle: Pointer = nil;

implementation

function oclLoadLibrary(Name: PAnsiChar): Pointer;
begin
  Result := Pointer(LoadLibraryA(Name));
end;

function oclFreeLibrary(LibHandle: Pointer): Boolean;
begin
  if LibHandle = nil then
   Result := False
  else
   Result := FreeLibrary(HMODULE(LibHandle));
end;

function oclGetProcAddress(ProcName: PAnsiChar; LibHandle: Pointer = nil): Pointer;
begin
  if LibHandle = nil then
    LibHandle := OCL_LibHandle;

  Result := GetProcAddress(HMODULE(LibHandle), ProcName);

  if Result <> nil then
   Exit;
end;

{$IFNDEF VER170}
{$REGION 'InitOpenCL'}
{$ENDIF}
function InitOpenCL(LibName: AnsiString): Boolean;
begin
  Result := False;

  // free opened libraries
  if OCL_LibHandle <> nil then
    oclFreeLibrary(OCL_LibHandle);

  // load library
  OCL_LibHandle := oclLoadLibrary(PAnsiChar(LibName));

  // load GL functions
  if (OCL_LibHandle <> nil) then begin
    //Platform API
      clGetPlatformIDs :=   oclGetProcAddress('clGetPlatformIDs', OCL_LibHandle);
      clGetPlatformInfo :=  oclGetProcAddress('clGetPlatformInfo', OCL_LibHandle);

    //Device APIs
      clGetDeviceIDs :=     oclGetProcAddress('clGetDeviceIDs', OCL_LibHandle);
      clGetDeviceInfo :=    oclGetProcAddress('clGetDeviceInfo', OCL_LibHandle);

    //Context APIs
      clCreateContext :=          oclGetProcAddress('clCreateContext', OCL_LibHandle);
      clCreateContextFromType :=  oclGetProcAddress('clCreateContextFromType', OCL_LibHandle);
      clRetainContext :=          oclGetProcAddress('clRetainContext', OCL_LibHandle);
      clReleaseContext :=         oclGetProcAddress('clReleaseContext', OCL_LibHandle);
      clGetContextInfo :=         oclGetProcAddress('clGetContextInfo', OCL_LibHandle);

    //Command Queue APIs
      clCreateCommandQueue :=       oclGetProcAddress('clCreateCommandQueue', OCL_LibHandle);
      clRetainCommandQueue :=       oclGetProcAddress('clRetainCommandQueue', OCL_LibHandle);
      clReleaseCommandQueue :=      oclGetProcAddress('clReleaseCommandQueue', OCL_LibHandle);
      clGetCommandQueueInfo :=      oclGetProcAddress('clGetCommandQueueInfo', OCL_LibHandle);
      clSetCommandQueueProperty :=  oclGetProcAddress('clSetCommandQueueProperty', OCL_LibHandle);

    //Memory Object APIs
      clCreateBuffer :=             oclGetProcAddress('clCreateBuffer', OCL_LibHandle);
      clCreateImage2D :=            oclGetProcAddress('clCreateImage2D', OCL_LibHandle);
      clCreateImage3D :=            oclGetProcAddress('clCreateImage3D', OCL_LibHandle);
      clRetainMemObject :=          oclGetProcAddress('clRetainMemObject', OCL_LibHandle);
      clReleaseMemObject :=         oclGetProcAddress('clReleaseMemObject', OCL_LibHandle);
      clGetSupportedImageFormats := oclGetProcAddress('clGetSupportedImageFormats', OCL_LibHandle);
      clGetMemObjectInfo :=         oclGetProcAddress('clGetMemObjectInfo', OCL_LibHandle);
      clGetImageInfo :=             oclGetProcAddress('clGetImageInfo', OCL_LibHandle);

    //Sampler APIs
      clCreateSampler :=            oclGetProcAddress('clCreateSampler', OCL_LibHandle);
      clRetainSampler :=            oclGetProcAddress('clRetainSampler', OCL_LibHandle);
      clReleaseSampler :=           oclGetProcAddress('clReleaseSampler', OCL_LibHandle);
      clGetSamplerInfo :=           oclGetProcAddress('clGetSamplerInfo', OCL_LibHandle);

    //Program Object APIs
      clCreateProgramWithSource :=  oclGetProcAddress('clCreateProgramWithSource', OCL_LibHandle);
      clCreateProgramWithBinary :=  oclGetProcAddress('clCreateProgramWithBinary', OCL_LibHandle);
      clRetainProgram :=            oclGetProcAddress('clRetainProgram', OCL_LibHandle);
      clReleaseProgram :=           oclGetProcAddress('clReleaseProgram', OCL_LibHandle);

      clBuildProgram :=         oclGetProcAddress('clBuildProgram', OCL_LibHandle);
      clUnloadCompiler :=       oclGetProcAddress('clUnloadCompiler', OCL_LibHandle);
      clGetProgramInfo :=       oclGetProcAddress('clGetProgramInfo', OCL_LibHandle);
      clGetProgramBuildInfo :=  oclGetProcAddress('clGetProgramBuildInfo', OCL_LibHandle);

    //Kernel Object APIs
      clCreateKernel :=             oclGetProcAddress('clCreateKernel', OCL_LibHandle);
      clCreateKernelsInProgram :=   oclGetProcAddress('clCreateKernelsInProgram', OCL_LibHandle);

      clRetainKernel :=   oclGetProcAddress('clRetainKernel', OCL_LibHandle);
      clReleaseKernel :=  oclGetProcAddress('clReleaseKernel', OCL_LibHandle);
      clSetKernelArg :=   oclGetProcAddress('clSetKernelArg', OCL_LibHandle);
      clGetKernelInfo :=  oclGetProcAddress('clGetKernelInfo', OCL_LibHandle);
      clGetKernelWorkGroupInfo := oclGetProcAddress('clGetKernelWorkGroupInfo', OCL_LibHandle);

    //Event Object APIs
      clWaitForEvents :=  oclGetProcAddress('clWaitForEvents', OCL_LibHandle);
      clGetEventInfo :=   oclGetProcAddress('clGetEventInfo', OCL_LibHandle);
      clRetainEvent :=    oclGetProcAddress('clRetainEvent', OCL_LibHandle);
      clReleaseEvent :=   oclGetProcAddress('clReleaseEvent', OCL_LibHandle);

    //Profiling APIs
      clGetEventProfilingInfo :=  oclGetProcAddress('clGetEventProfilingInfo', OCL_LibHandle);

    //Flush and Finish APIs
      clFlush :=  oclGetProcAddress('clFlush', OCL_LibHandle);
      clFinish := oclGetProcAddress('clFinish', OCL_LibHandle);
    //Enqueued Commands APIs
      clEnqueueReadBuffer :=  oclGetProcAddress('clEnqueueReadBuffer', OCL_LibHandle);
      clEnqueueWriteBuffer := oclGetProcAddress('clEnqueueWriteBuffer', OCL_LibHandle);
      clEnqueueCopyBuffer :=  oclGetProcAddress('clEnqueueCopyBuffer', OCL_LibHandle);
      clEnqueueReadImage :=   oclGetProcAddress('clEnqueueReadImage', OCL_LibHandle);
      clEnqueueWriteImage :=  oclGetProcAddress('clEnqueueWriteImage', OCL_LibHandle);
      clEnqueueCopyImage :=   oclGetProcAddress('clEnqueueCopyImage', OCL_LibHandle);
      clEnqueueCopyImageToBuffer := oclGetProcAddress('clEnqueueCopyImageToBuffer', OCL_LibHandle);
      clEnqueueCopyBufferToImage := oclGetProcAddress('clEnqueueCopyBufferToImage', OCL_LibHandle);
      clEnqueueMapBuffer :=   oclGetProcAddress('clEnqueueMapBuffer', OCL_LibHandle);
      clEnqueueMapImage :=    oclGetProcAddress('clEnqueueMapImage', OCL_LibHandle);
      clEnqueueUnmapMemObject :=    oclGetProcAddress('clEnqueueUnmapMemObject', OCL_LibHandle);
      clEnqueueNDRangeKernel :=     oclGetProcAddress('clEnqueueNDRangeKernel', OCL_LibHandle);
      clEnqueueTask :=          oclGetProcAddress('clEnqueueTask', OCL_LibHandle);
      clEnqueueNativeKernel :=  oclGetProcAddress('clEnqueueNativeKernel', OCL_LibHandle);
      clEnqueueMarker :=        oclGetProcAddress('clEnqueueMarker', OCL_LibHandle);
      clEnqueueWaitForEvents := oclGetProcAddress('clEnqueueWaitForEvents', OCL_LibHandle);
      clEnqueueBarrier :=       oclGetProcAddress('clEnqueueBarrier', OCL_LibHandle);

    //Extension function access
      clGetExtensionFunctionAddress :=  oclGetProcAddress('clGetExtensionFunctionAddress', OCL_LibHandle);

      Result := True;
  end;
end;
{$IFNDEF VER170}
{$ENDREGION}
//InitOpenCL
{$ENDIF}

{$IFNDEF VER170}
{$REGION 'GetString'}
{$ENDIF}
function GetString(const Status: TCL_int): AnsiString;
begin
  Result := '';
  case Status of
    CL_SUCCESS: Result := 'Success';
    CL_DEVICE_NOT_FOUND: Result := 'device not found';
    CL_DEVICE_NOT_AVAILABLE: Result := 'device not available';
    CL_COMPILER_NOT_AVAILABLE: Result := 'compiler not available';
    CL_MEM_OBJECT_ALLOCATION_FAILURE: Result := 'mem object allocation failure';
    CL_OUT_OF_RESOURCES: Result := 'out of resources';
    CL_OUT_OF_HOST_MEMORY: Result := 'out of host memory';
    CL_PROFILING_INFO_NOT_AVAILABLE: Result := 'profiling info not available';
    CL_MEM_COPY_OVERLAP: Result := 'mem copy overlap';
    CL_IMAGE_FORMAT_MISMATCH: Result := 'image format mismatch';
    CL_IMAGE_FORMAT_NOT_SUPPORTED: Result := 'image format not support';
    CL_BUILD_PROGRAM_FAILURE: Result := 'build program failure';
    CL_MAP_FAILURE: Result := 'map failure';

    CL_INVALID_VALUE: Result := 'invalid value';
    CL_INVALID_DEVICE_TYPE: Result := 'invalid device type';
    CL_INVALID_PLATFORM: Result := 'invalid platform';
    CL_INVALID_DEVICE: Result := 'invalid device';
    CL_INVALID_CONTEXT: Result := 'invalid context';
    CL_INVALID_QUEUE_PROPERTIES: Result := 'invalid queue properties';
    CL_INVALID_COMMAND_QUEUE: Result := 'invalid command queue';
    CL_INVALID_HOST_PTR: Result := 'invalid host ptr';
    CL_INVALID_MEM_OBJECT: Result := 'invalid mem object';
    CL_INVALID_IMAGE_FORMAT_DESCRIPTOR: Result := 'invalid image format descriptor';
    CL_INVALID_IMAGE_SIZE: Result := 'invalid image size';
    CL_INVALID_SAMPLER: Result := 'invalid sampler';
    CL_INVALID_BINARY: Result := 'invalid binary';
    CL_INVALID_BUILD_OPTIONS: Result := 'invalid build options';
    CL_INVALID_PROGRAM: Result := 'invalid program';
    CL_INVALID_PROGRAM_EXECUTABLE: Result := 'invalid program executable';
    CL_INVALID_KERNEL_NAME: Result := 'invalid kernel name';
    CL_INVALID_KERNEL_DEFINITION: Result := 'invalid kernel definition';
    CL_INVALID_KERNEL: Result := 'invalid kernel';
    CL_INVALID_ARG_INDEX: Result := 'invalid arg index';
    CL_INVALID_ARG_VALUE: Result := 'invalid arg value';
    CL_INVALID_ARG_SIZE: Result := 'invalid arg size';
    CL_INVALID_KERNEL_ARGS: Result := 'invalid kernel args';
    CL_INVALID_WORK_DIMENSION: Result := 'invalid work dimension';
    CL_INVALID_WORK_GROUP_SIZE: Result := 'invalid work group size';
    CL_INVALID_WORK_ITEM_SIZE: Result := 'invalid work item size';
    CL_INVALID_GLOBAL_OFFSET: Result := 'invalid global offset';
    CL_INVALID_EVENT_WAIT_LIST: Result := 'invalid event wait list';
    CL_INVALID_EVENT: Result := 'invalid event';
    CL_INVALID_OPERATION: Result := 'invalid operation';
    CL_INVALID_GL_OBJECT: Result := 'invalid gl object';
    CL_INVALID_BUFFER_SIZE: Result := 'invalid buffer size';
    CL_INVALID_MIP_LEVEL: Result := 'invalid mip level';
    CL_INVALID_GLOBAL_WORK_SIZE: Result := 'invalid global work size';
  end;
end;
{$IFNDEF VER170}
{$ENDREGION}
//GetString
{$ENDIF}

initialization
{$IFDEF DEFINE_8087CW_NOT_IMPLEMENTED}
asm
  MOV     Default8087CW,AX
  FLDCW   Default8087CW
end;
{$ELSE}
  Set8087CW($133F);
{$ENDIF}
end.

