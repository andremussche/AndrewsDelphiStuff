(**********************************************************************************
 * Copyright (c) 2008 The Khronos Group Inc.
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
unit cl_d3d10;

interface

uses
  OpenGL,
  cl,
  D3D10,
  cl_platform;

{$INCLUDE 'OpenCL.inc'}


const CL_D3D10_DEVICE                            =  $1070;

type
  TclCreateFromD3D10Buffer = function (context: Tcl_context;                (* context *)
                                         flags: Tcl_mem_flags;                (* flags *)
                                         pD3DResource:  PID3D10Resource;      (* pD3DResource *)               //ID3D10Resource *
                                         errcode_ret: PInteger                (* errcode_ret *)
                                         ) : Tcl_mem;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


  TclCreateImageFromD3D10Resource = function (context: Tcl_context;         (* context *)
                                                flags: Tcl_mem_flags;         (* flags *)
                                                pD3DResource: PID3D10Resource;(* pD3DResource *)              //ID3D10Resource  *
                                                errcode_ret: PInteger         (* errcode_ret *)
                                                ) : Tcl_mem;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;

var
  clCreateFromD3D10Buffer:        TclCreateFromD3D10Buffer;
  clCreateImageFromD3D10Resource: TclCreateImageFromD3D10Resource;

function InitCL_D3D10: Boolean;

implementation

function InitCL_D3D10: Boolean;
begin
  Result := False;
  if OCL_LibHandle <> nil then begin
    //OpenGL
    clCreateFromD3D10Buffer :=        oclGetProcAddress('clCreateFromD3D10Buffer', OCL_LibHandle);
    clCreateImageFromD3D10Resource := oclGetProcAddress('clCreateImageFromD3D10Resource', OCL_LibHandle);

    Result := True;
  end;
end;

end.