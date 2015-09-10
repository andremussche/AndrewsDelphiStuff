(**********************************************************************************
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
 **********************************************************************************)

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
(*      updated by      : Alexander Kiselev     *)
(*                          (Igroman)           *)
(*      Site : http://Igroman14.livejournal.com *)
(*      e-mail          : Igroman14@yandex.ru   *)
(*      ICQ             : 207-381-695           *)
(*                    (c) 2010                  *)
(*                                              *)
(***********Copyright (c) niello 2008-2010*******)

(* $Revision: 10424 $ on $Date: 2010-02-24 23:53:49 -0800 (Wed, 24 Feb 2010) $ *)

(*
 * cl_gl.h contains Khronos-approved (KHR) OpenCL extensions which have
 * OpenGL dependencies. The application is responsible for #including
 * OpenGL or OpenGL ES headers before #including cl_gl.h.
 *)
unit cl_gl;

interface



uses
  OpenGL,
  cl,
  cl_platform;

{$INCLUDE 'OpenCL.inc'}

type
  PCL_gl_object_type  = ^TCL_gl_object_type;
  TCL_gl_object_type  = TCL_uint;
  PCL_gl_texture_info = ^TCL_gl_texture_info;
  TCL_gl_texture_info = TCL_uint;
  PCL_gl_platform_info= ^TCL_gl_platform_info;
  TCL_gl_platform_info= TCL_uint;

const
// cl_gl_object_type
     CL_GL_OBJECT_BUFFER           =  $2000;
     CL_GL_OBJECT_TEXTURE2D        =  $2001;
     CL_GL_OBJECT_TEXTURE3D        =  $2002;
     CL_GL_OBJECT_RENDERBUFFER     =  $2003;

// cl_gl_texture_info
     CL_GL_TEXTURE_TARGET          =  $2004;
     CL_GL_MIPMAP_LEVEL            =  $2005;

// CL_KHR_gl_sharing
     CL_GL_CONTEXT_KHR             =  $2008;
     CL_EGL_DISPLAY_KHR            =  $2009;
     CL_GLX_DISPLAY_KHR            =  $200A;
     CL_WGL_HDC_KHR                =  $200B;
     CL_CGL_SHAREGROUP_KHR         =  $200C;


type
  TclCreateFromGLBuffer = function (context: Tcl_context;                  (* context *)
                                      flags: Tcl_mem_flags;                  (* flags *)
                                      bufobj:  GLuint;                       (* bufobj *)
                                      errcode_ret: PInteger                  (* errcode_ret *)
                                      ): TCL_mem;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


  TclCreateFromGLTexture2D = function (context: Tcl_context;               (* context *)
                                         flags: Tcl_mem_flags;               (* flags *)
                                         target: GLenum;                     (* target *)
                                         miplevel: GLint;                    (* miplevel *)
                                         texture: GLuint;                    (* texture *)
                                         errcode_ret: Pcl_int                (* errcode_ret *)
                                         ): TCL_mem;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


  TclCreateFromGLTexture3D = function (context: Tcl_context;               (* context *)
                                         flags: Tcl_mem_flags;               (* flags *)
                                         target: GLenum;                     (* target *)
                                         miplevel: GLint;                    (* miplevel *)
                                         texture: GLuint;                    (* texture *)
                                         errcode_ret: Pcl_int                (* errcode_ret *)
                                         ): TCL_mem;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


  TclCreateFromGLRenderbuffer = function (context: Tcl_context;            (* context *)
                                            flags: Tcl_mem_flags;            (* flags *)
                                            renderbuffer: GLuint;            (* renderbuffer *)
                                            errcode_ret: Pcl_int             (* errcode_ret *)
                                            ): Tcl_mem;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


  TclGetGLObjectInfo = function (memobj: Tcl_mem;                          (* memobj *)
                                   gl_object_type: Pcl_gl_object_type;       (* gl_object_type *)
                                   gl_object_name: PGLuint                   (* gl_object_name *)
                                   ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


  TclGetGLTextureInfo = function (memobj: Tcl_mem;                         (* memobj *)
                                    param_name: Tcl_gl_texture_info;         (* param_name *)
                                    param_value_size: Tsize_t;               (* param_value_size *)
                                    param_value: Pointer;                    (* param_value *)
                                    param_value_size_ret: Psize_t           (* param_value_size_ret *)
                                    ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


  TclEnqueueAcquireGLObjects = function (command_queue: Tcl_command_queue; (* command_queue *)
                                           num_objects: Tcl_uint;            (* num_objects *)
                                           const mem_objects: Pcl_mem;       (* mem_objects *)
                                           num_events_in_wait_list: Tcl_uint;(* num_events_in_wait_list *)
                                           const event_wait_list: Pcl_event; (* event_wait_list *)
                                           event: Pcl_event                  (* event *)
                                           ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


  TclEnqueueReleaseGLObjects = function (command_queue: Tcl_command_queue; (* command_queue *)
                                           num_objects: Tcl_uint;            (* num_objects *)
                                           const mem_objects: Pcl_mem;       (* mem_objects *)
                                           num_events_in_wait_list: Tcl_uint;(* num_events_in_wait_list *)
                                           const event_wait_list: Pcl_event; (* event_wait_list *)
                                           event: Pcl_event                  (* event *)
                                           ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

const
  CL_INVALID_GL_SHAREGROUP_REFERENCE_KHR  = -1000;
  CL_CURRENT_DEVICE_FOR_GL_CONTEXT_KHR    = $2006;
  CL_DEVICES_FOR_GL_CONTEXT_KHR           = $2007;

type
     Pcl_gl_context_info = ^Tcl_gl_context_info;
     Tcl_gl_context_info = Tcl_uint;


  TclGetGLContextInfoKHR = function (const properties: Pcl_context_properties; (* properties *)
                                       param_name: Tcl_gl_context_info;          (* param_name *)
                                       param_value_size: Tsize_t;                (* param_value_size *)
                                       param_value: Pointer;                     (* param_value *)
                                       param_value_size_ret : Psize_t            (* param_value_size_ret *)
                                       ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

var
  clCreateFromGLBuffer:     TclCreateFromGLBuffer;
  clCreateFromGLTexture2D:  TclCreateFromGLTexture2D;
  clCreateFromGLTexture3D:  TclCreateFromGLTexture3D;
  clCreateFromGLRenderbuffer: TclCreateFromGLRenderbuffer;
  clGetGLObjectInfo:        TclGetGLObjectInfo;
  clGetGLTextureInfo:       TclGetGLTextureInfo;
  clEnqueueAcquireGLObjects: TclEnqueueAcquireGLObjects;
  clEnqueueReleaseGLObjects: TclEnqueueReleaseGLObjects;
  clGetGLContextInfoKHR:    TclGetGLContextInfoKHR;

function InitCL_OGL: Boolean;

implementation

function InitCL_OGL: Boolean;
begin
  Result := False;
  if OCL_LibHandle <> nil then begin
    //OpenGL
    clCreateFromGLBuffer :=     oclGetProcAddress('clCreateFromGLBuffer', OCL_LibHandle);
    clCreateFromGLTexture2D :=  oclGetProcAddress('clCreateFromGLTexture2D', OCL_LibHandle);
    clCreateFromGLTexture3D :=  oclGetProcAddress('clCreateFromGLTexture3D', OCL_LibHandle);
    clCreateFromGLRenderbuffer := oclGetProcAddress('clCreateFromGLRenderbuffer', OCL_LibHandle);
    clGetGLObjectInfo :=        oclGetProcAddress('clGetGLObjectInfo', OCL_LibHandle);
    clGetGLTextureInfo :=       oclGetProcAddress('clGetGLTextureInfo', OCL_LibHandle);
    clEnqueueAcquireGLObjects := oclGetProcAddress('clEnqueueAcquireGLObjects', OCL_LibHandle);
    clEnqueueReleaseGLObjects := oclGetProcAddress('clEnqueueReleaseGLObjects', OCL_LibHandle);
    clGetGLContextInfoKHR :=    oclGetProcAddress('clGetGLContextInfoKHR', OCL_LibHandle);

    Result := True;
  end;
end;

end.