(********************************************)
(*                                          *)
(*     OpenCL1.0 and Delphi and Windows     *)
(*                                          *)
(*      created by      : Maksim Tymkovich  *)
(*                           (niello)       *)
(*                                          *)
(*      headers versions: 0.03              *)
(*      file name       : OpenCL.inc        *)
(*      last modify     : 13.02.10          *)
(*      license         : BSD               *)
(*                                          *)
(*      Site            : www.niello.org.ua *)
(*      e-mail          : muxamed13@ukr.net *)
(*      ICQ             : 446-769-253       *)
(*                                          *)
(*********Copyright (c) niello 2008-2010*****)


//{$DEFINE USE_LOG}           //Use default procedure Writeln()
//{$DEFINE DEFAULT_ATI}         //Perferences ATI   (find device ATI)
//{$DEFINE DEFAULT_NVIDIA}    //Perferences NVdia (find device NVdia)

{$DEFINE STDCALL}             //Now ATI (Windows) and NVdia (Windows)
//{$DEFINE CDECL}               //Old NVdia (Windows)


{$IFDEF VER120}//Delphi 4
  {$DEFINE DEFINE_8087CW_NOT_IMPLEMENTED}
  {$DEFINE DEFINE_UINT64_EQU_INT64}
{$ENDIF}
{$IFDEF VER110}//Builder 3
  {$DEFINE DEFINE_8087CW_NOT_IMPLEMENTED}
  {$DEFINE DEFINE_UINT64_EQU_INT64}
{$ENDIF}
{$IFDEF VER100}//Delphi3
  {$DEFINE DEFINE_8087CW_NOT_IMPLEMENTED}
  {$DEFINE DEFINE_UINT64_EQU_INT64}
{$ENDIF}
{$IFDEF VER140}//Delphi 6
  {$DEFINE DEFINE_UINT64_EQU_INT64}
{$ENDIF}
{$IFDEF VER130}//Delphi 5
  {$DEFINE DEFINE_UINT64_EQU_INT64}
{$ENDIF}


//{$IFDEF DEFAULT_ATI}
//const
// OpenCL = 'atiocl.dll';//'OpenCL.dll';
//{$ENDIF}
//{$IFDEF DEFAULT_NVIDIA}
//const
// OpenCL = 'OpenCL.dll';
//{$ENDIF}



