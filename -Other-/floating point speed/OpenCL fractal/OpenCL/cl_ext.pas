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
(*      updated by      : Alexander Kiselev     *)
(*                          (Igroman)           *)
(*      Site : http://Igroman14.livejournal.com *)
(*      e-mail          : Igroman14@yandex.ru   *)
(*      ICQ             : 207-381-695           *)
(*                    (c) 2010                  *)
(*                                              *)
(***********Copyright (c) niello 2008-2010*******)

(* $Revision: 10424 $ on $Date: 2010-02-24 00:10:00  (Th, 25 Feb 2010) $ *)

unit cl_ext;

interface



uses
  cl,
  cl_platform;

{$INCLUDE 'OpenCL.inc'}

const
(* cl_khr_fp64 extension - no extension #define since it has no functions  *)
  CL_DEVICE_DOUBLE_FP_CONFIG                =  $1032;


(* cl_khr_fp16 extension - no extension #define since it has no functions  *)
  CL_DEVICE_HALF_FP_CONFIG                  =  $1033;


(* cl_khr_icd extension                                                    *)
  cl_khr_icd                                =  1;

(* cl_platform_info                                                        *)
  CL_PLATFORM_ICD_SUFFIX_KHR                =  $0920;

(* Additional Error Codes                                                  *)
  CL_PLATFORM_NOT_FOUND_KHR                 =  -1001;

type
  TclIcdGetPlatformIDsKHR = function (num_entries: Tcl_uint;          (* num_entries *)
                                platforms: Pcl_platform_id;     (* platforms *)
                                num_platforms: Pcl_uint         (* num_platforms *)
                                ): TCL_int;{$IFDEF CDECL}cdecl{$ELSE}stdcall{$ENDIF};// external OpenCL;


var
  clIcdGetPlatformIDsKHR: TclIcdGetPlatformIDsKHR;

function InitCL_EXT: Boolean;

implementation

function InitCL_EXT: Boolean;
begin
  Result := False;
  if OCL_LibHandle <> nil then begin
    //OpenGL
    clIcdGetPlatformIDsKHR := oclGetProcAddress('clIcdGetPlatformIDsKHR', OCL_LibHandle);

    Result := True;
  end;
end;

end.
