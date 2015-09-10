(*
 * Copyright 1993-2009 NVIDIA Corporation.  All rights reserved.
 *
 * NVIDIA Corporation and its licensors retain all intellectual property and 
 * proprietary rights in and to this software and related documentation. 
 * Any use, reproduction, disclosure, or distribution of this software 
 * and related documentation without an express license agreement from
 * NVIDIA Corporation is strictly prohibited.
 *
 * Please refer to the applicable NVIDIA end user license agreement (EULA) 
 * associated with this source code for terms and conditions that govern 
 * your use of this NVIDIA software.
 * 
 *)

(********************************************)
(*                                          *)
(*     OpenCL1.0 and Delphi and Windows     *)
(*                                          *)
(*      created by      : Maksim Tymkovich  *)
(*                           (niello)       *)
(*                                          *)
(*      headers versions: 0.03              *)
(*      file name       : oclUtils.pas      *)
(*      last modify     : 13.02.10          *)
(*      license         : BSD               *)
(*                                          *)
(*      Site            : www.niello.org.ua *)
(*      e-mail          : muxamed13@ukr.net *)
(*      ICQ             : 446-769-253       *)
(*                                          *)
(*********Copyright (c) niello 2008-2010*****)
unit oclUtils;

interface


  
uses
  CL,CL_platform,clExt;

{$INCLUDE 'OpenCL.inc'}

const
  PGMHeaderSize: TCL_int = $40;

  NVIDIA_PLATFORM = 'NVIDIA';
  ATI_PLATFORM    = 'ATI';


type
  LogWriteln = procedure(const message: AnsiString);


var
  Default_Platform: PAnsiChar = NVIDIA_PLATFORM;
  MainLog: LogWriteln;


function oclGetPlatformID(clSelectedPlatformID: PPCL_platform_id): TCL_int;
procedure oclPrintDevName(var device: PCL_device_id);//tested +fixed = complete
procedure oclPrintDevInfo(var device: Pcl_device_id);//tested +fixed = complete
function oclGetFirstDev(cxGPUContext: PCL_context): PPCL_device_id;//tested = complete
function oclGetMaxFlopsDev(cxGPUContext: Pcl_context): PPcl_device_id;//tested = complete
function oclLoadProgSource(const cFilename: string;
                           const cPreamble: PAnsiChar;
                           szFinalLength: Psize_t): PAnsiChar;//tested +fixed = complete
(*
procedure oclGetProgBinary(cpProgram: Pcl_program;
                           var cdDevice: Pcl_device_id;
                           var binary: PPAnsiChar;
                           length: Psize_t);//not yet
*)

function oclGetDev(cxGPUContext: Pcl_context; nr: TCL_uint): PPCL_device_id;//tested +fixed = complete

procedure oclLogBuildInfo(cpProgram: Pcl_program;
                            var cdDevice: Pcl_device_id);//tested ~ may be fix messages?
(*
procedure oclLogPtx(cpProgram: Pcl_program;
                    cdDevice: Pcl_device_id;
                    const cPtxFileName: AnsiString);//not yet
*)

procedure oclDeleteMemObjs(cmMemObjs: PPcl_mem;
                            iNumObjs: Integer);//tested + fixed = complete



procedure shrLog(const message: AnsiString);//OK

implementation

function IntToStr(const Value: Integer): AnsiString;
begin
  Str(Value,Result);
end;

function StrPos(const Str1, Str2: PAnsiChar): PAnsiChar; assembler;
asm
  PUSH    EDI
  PUSH    ESI
  PUSH    EBX
  OR      EAX,EAX
  JE      @@2
  OR      EDX,EDX
  JE      @@2
  MOV     EBX,EAX
  MOV     EDI,EDX
  XOR     AL,AL
  MOV     ECX,0FFFFFFFFH
  REPNE   SCASB
  NOT     ECX
  DEC     ECX
  JE      @@2
  MOV     ESI,ECX
  MOV     EDI,EBX
  MOV     ECX,0FFFFFFFFH
  REPNE   SCASB
  NOT     ECX
  SUB     ECX,ESI
  JBE     @@2
  MOV     EDI,EBX
  LEA     EBX,[ESI-1]
@@1:  MOV     ESI,EDX
  LODSB
  REPNE   SCASB
  JNE     @@2
  MOV     EAX,ECX
  PUSH    EDI
  MOV     ECX,EBX
  REPE    CMPSB
  POP     EDI
  MOV     ECX,EAX
  JNE     @@1
  LEA     EAX,[EDI-1]
  JMP     @@3
@@2:  XOR     EAX,EAX
@@3:  POP     EBX
  POP     ESI
  POP     EDI
end;

function pos2(SubStr: AnsiString; S: AnsiString; const StartPos: Integer): Integer;
var
 CopyStr: AnsiString;
begin
  CopyStr := Copy(S, StartPos, Length(S) - StartPos);
  Result := Pos(Substr, copyStr) + StartPos - 1;
end;

function oclGetPlatformID(clSelectedPlatformID: PPCL_platform_id): TCL_int;
var
  chBuffer: array [0..1023] of AnsiChar;
  num_platforms: TCL_uint;
  clPlatformIDs: array of PCL_platform_id;
  ciErrNum: TCL_int;
  i: integer;
begin
  //Get OpenCL platform count
  ciErrNum := clGetPlatformIDs(0, nil, @num_platforms);
  if ciErrNum <> CL_SUCCESS then begin
    {$IFDEF USE_LOG}
       //shrLog(LOGBOTH, 0, " Error %i in clGetPlatformIDs Call !!!\n\n", ciErrNum);
       shrLog('Error ' + IntToStr(ciErrNum) + ' in clGetPlatformIDs Call !!!');
    {$ENDIF}
    Result := -1000;
    Exit;
  end else begin
    if num_platforms = 0 then begin
      {$IFDEF USE_LOG}
        //shrLog(LOGBOTH, 0, "No OpenCL platform found!\n\n");
        shrLog('No OpenCL platform found!');
      {$ENDIF}
      Result := -2000;
      Exit;
    end else begin
      SetLength(clPlatformIDs,num_platforms*sizeOf(PCL_PLATFORM_ID));
      //GetMem(clPlatformIDs,num_platforms*sizeOf(PCL_PLATFORM_ID));
      if clPlatformIDs = nil then begin
        {$IFDEF USE_LOG}
          //shrLog(LOGBOTH, 0, "Failed to allocate memory for cl_platform ID's!\n\n");
          shrLog('Failed to allocate memory for cl_platform ID"s!');
        {$ENDIF}
        Result := -3000;
        Exit;
      end;
      {ciErrNum:=}clGetPlatformIDs(num_platforms, @clPlatformIDs, nil);
      for i := 0 to num_platforms - 1 do begin
        ciErrNum := clGetPlatformInfo(@clPlatformIDs[i], CL_PLATFORM_NAME, 1024, @Chbuffer, nil);
        if ciErrNum = CL_SUCCESS then begin
          if StrPos(chBuffer, Default_Platform) <> '' then begin
            clSelectedPlatformID := @clPlatformIDs[i];
            Break;
          end;
        end;
      end;

      if clSelectedPlatformID = nil then begin
        {$IFDEF USE_LOG}
          //shrLog(LOGBOTH, 0, "WARNING: NVIDIA OpenCL platform not found - defaulting to first platform!\n\n");
          shrLog('WARNING: NVIDIA OpenCL platform not found - defaulting to first platform!');
        {$ENDIF}
      end;

      //FreeMem(clPlatformIDs); //Delphi 6 Work Delph 2010 error
      SetLength(clPlatformIDs, 0);
    end;
  end;
  Result := CL_SUCCESS;
end;

procedure oclPrintDevName(var device: PCL_device_id);
var
  device_string: array[0..1023] of AnsiChar;
  indevice: integer;//promo value
begin
  indevice := Integer(device);
  clGetDeviceInfo(device, CL_DEVICE_NAME, sizeof(device_string), @device_string, nil);
  {$IFDEF USE_LOG}
    //shrLog(iLogMode, 0.0, " Device %s\n", device_string);
    shrLog({'Device '+}device_string);
  {$ENDIF}
  device := PCL_device_id(indevice);
end;

function oclGetFirstDev(cxGPUContext: PCL_context): PPCL_device_id;
var
  szParmDataBytes: TSize_t;
  cdDevices: array of PCL_device_id;
begin
  // get the list of GPU devices associated with context
  clGetContextInfo(cxGPUContext, CL_CONTEXT_DEVICES, 0, nil, @szParmDataBytes);
  SetLength(cdDevices, szParmDataBytes);
  //cdDevices = (cl_device_id*) malloc(szParmDataBytes);
  clGetContextInfo(cxGPUContext, CL_CONTEXT_DEVICES, szParmDataBytes, cdDevices, nil);

  Result := @cdDevices[0];
  SetLength(cdDevices, 0);
  //free(cdDevices);
end;

function oclGetDev(cxGPUContext: Pcl_context; nr: TCL_uint): PPCL_device_id;
var
  szParmDataBytes : TSize_t;
  cdDevices: array of PCL_device_id;
begin
  // get the list of GPU devices associated with context
  clGetContextInfo(cxGPUContext, CL_CONTEXT_DEVICES, 0,nil, @szParmDataBytes);

  if(szParmDataBytes / SizeOf(Pcl_device_id) < nr) then begin
    Result := PPCL_device_id(-1);
    Exit;
  end;

  //cdDevices = (cl_device_id*) malloc(szParmDataBytes);
  SetLength(cdDevices, szParmDataBytes);

  clGetContextInfo(cxGPUContext, CL_CONTEXT_DEVICES, szParmDataBytes, cdDevices, nil);

  //cl_device_id device = cdDevices[nr];
  Result := @cdDevices[nr];
  //free(cdDevices);
  SetLength(cdDevices, 0);
end;

procedure oclPrintDevInfo(var device: Pcl_device_id);
var
  nv_device_attibute_query: Boolean;
  _type: Tcl_device_type;
  compute_units: Tcl_uint;
  workitem_dims: Tsize_t;
  workitem_size: array [0..2] of Tsize_t;
  workgroup_size: Tsize_t;
  clock_frequency: Tcl_uint;
  addr_bits : Tcl_uint;
  max_mem_alloc_size: Tcl_ulong;
  mem_size: Tcl_ulong;
  error_correction_support: Tcl_bool;
  local_mem_type: Tcl_device_local_mem_type;
  queue_properties: Tcl_command_queue_properties;
  image_support: Tcl_bool;
  max_read_image_args: Tcl_uint;
  max_write_image_args: Tcl_uint;
  szMaxDims: array [0..4] of Tsize_t;
  compute_capability_major,
  compute_capability_minor: Tcl_uint;
  regs_per_block: Tcl_uint;
  warp_size: Tcl_uint;
  gpu_overlap: Tcl_bool;
  exec_timeout: Tcl_bool;
  integrated_memory: Tcl_bool;
  vec_width: array [0..5] of Tcl_uint;

  stdDevString: AnsiString;
  szOldPos: Tsize_t;
  szSpacePos: Tsize_t;

//  St: TCL_int;

  indevice: Integer;

  device_string: array [0..1023] of AnsiChar;
begin
  nv_device_attibute_query := false;

  // CL_DEVICE_NAME
  indevice := Integer(device); //this value promo<-device

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_NAME, SizeOf(device_string), @device_string, nil);
  //Writeln()
  //shrLog(iLogMode, 0.0, "  CL_DEVICE_NAME: \t\t\t%s\n", device_string);
  //Writeln('CL_DEVICE_NAME: '+device_string);

  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_NAME: ' + device_string);
  {$ENDIF}
  //Writeln(Integer(device));
  // CL_DEVICE_VENDOR
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_VENDOR, SizeOf(device_string), @device_string, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_VENDOR: \t\t\t%s\n", device_string);
  //Writeln('CL_DEVICE_VENDOR: '+device_string);
  //Writeln(integer(device^));
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_VENDOR: ' + device_string);
  {$ENDIF}

  // CL_DRIVER_VERSION
  clGetDeviceInfo(PCL_device_id(indevice), CL_DRIVER_VERSION, SizeOf(device_string), @device_string, nil);
  //shrLog(iLogMode, 0.0, "  CL_DRIVER_VERSION: \t\t\t%s\n", device_string);
  //Writeln('  CL_DRIVER_VERSION: ',device_string);
  {$IFDEF USE_LOG}
    shrLog('CL_DRIVER_VERSION: ' + device_string);
  {$ENDIF}

  // CL_DEVICE_INFO

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_TYPE, SizeOf(_type), @_type, nil);
  //Writeln(Integer(device));
  if (_type and CL_DEVICE_TYPE_CPU) <> 0 then begin
    //Writeln('CL_DEVICE_TYPE: CL_DEVICE_TYPE_CPU');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_TYPE: CL_DEVICE_TYPE_CPU');
    {$ENDIF}
  end;

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_TYPE:\t\t\t%s\n", "CL_DEVICE_TYPE_CPU");
  if (_type and CL_DEVICE_TYPE_GPU) <> 0 then begin
    //Writeln('CL_DEVICE_TYPE: CL_DEVICE_TYPE_GPU');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_TYPE: CL_DEVICE_TYPE_GPU');
    {$ENDIF}
  end;

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_TYPE:\t\t\t%s\n", "CL_DEVICE_TYPE_GPU");
  if (_type and CL_DEVICE_TYPE_ACCELERATOR) <> 0 then begin
    //Writeln('CL_DEVICE_TYPE: CL_DEVICE_TYPE_ACCELERATOR');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_TYPE: CL_DEVICE_TYPE_ACCELERATOR');
    {$ENDIF}
  end;
        //shrLog(iLogMode, 0.0, "  CL_DEVICE_TYPE:\t\t\t%s\n", "CL_DEVICE_TYPE_ACCELERATOR");
  if (_type and CL_DEVICE_TYPE_DEFAULT) <> 0 then begin
    //Writeln('CL_DEVICE_TYPE: CL_DEVICE_TYPE_DEFAULT');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_TYPE: CL_DEVICE_TYPE_DEFAULT');
    {$ENDIF}
  end;

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_TYPE:\t\t\t%s\n", "CL_DEVICE_TYPE_DEFAULT");

  // CL_DEVICE_MAX_COMPUTE_UNITS

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_MAX_COMPUTE_UNITS, SizeOf(compute_units), @compute_units, nil);
  //shrLog(iLogMode, 0.0, "  CL_DEVICE_MAX_COMPUTE_UNITS:\t\t%u\n", compute_units);
  //Writeln('CL_DEVICE_MAX_COMPUTE_UNITS: ',compute_units);
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_MAX_COMPUTE_UNITS: ' + IntToStr(compute_units));
  {$ENDIF}


  // CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, SizeOf(workitem_dims), @workitem_dims, nil);
  //shrLog(iLogMode, 0.0, "  CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS:\t%u\n", workitem_dims);
  //Writeln('CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS: ',workitem_dims);
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS: ' + IntToStr(workitem_dims));
  {$ENDIF}

  // CL_DEVICE_MAX_WORK_ITEM_SIZES

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_MAX_WORK_ITEM_SIZES, SizeOf(workitem_size), @workitem_size, nil);
  //shrLog(iLogMode, 0.0, "  CL_DEVICE_MAX_WORK_ITEM_SIZES:\t%u / %u / %u \n", workitem_size[0], workitem_size[1], workitem_size[2]);
  //Writeln('CL_DEVICE_MAX_WORK_ITEM_SIZES: ',workitem_size[0],' ',workitem_size[1],' ',workitem_size[2]);
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_MAX_WORK_ITEM_SIZES: ' +
            IntToStr(workitem_size[0]) + ' ' +
            IntToStr(workitem_size[1]) + ' ' +
            IntToStr(workitem_size[2]));
  {$ENDIF}

  // CL_DEVICE_MAX_WORK_GROUP_SIZE

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_MAX_WORK_GROUP_SIZE, SizeOf(workgroup_size), @workgroup_size, nil);
  //shrLog(iLogMode, 0.0, "  CL_DEVICE_MAX_WORK_GROUP_SIZE:\t%u\n", workgroup_size);
  //Writeln('CL_DEVICE_MAX_WORK_GROUP_SIZE: ',workitem_size[0],' ',workitem_size[1],' ',workgroup_size);
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_MAX_WORK_GROUP_SIZES: ' + IntToStr(workgroup_size));
  {$ENDIF}

  // CL_DEVICE_MAX_CLOCK_FREQUENCY
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_MAX_CLOCK_FREQUENCY, SizeOf(clock_frequency), @clock_frequency, nil);
  //shrLog(iLogMode, 0.0, "  CL_DEVICE_MAX_CLOCK_FREQUENCY:\t%u MHz\n", clock_frequency);
  //Writeln('CL_DEVICE_MAX_CLOCK_FREQUENCY: ',clock_frequency,' MHz');
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_MAX_CLOCK_FREQUENCY: '+IntToStr(clock_frequency)+' MHz');
  {$ENDIF}

  // CL_DEVICE_ADDRESS_BITS
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_ADDRESS_BITS, sizeof(addr_bits), @addr_bits, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_ADDRESS_BITS:\t\t%u\n", addr_bits);
  //Writeln('CL_DEVICE_ADDRESS_BITS: ',addr_bits);
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_ADDRESS_BITS: ' + IntToStr(addr_bits));
  {$ENDIF}

  // CL_DEVICE_MAX_MEM_ALLOC_SIZE
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_MAX_MEM_ALLOC_SIZE, SizeOf(max_mem_alloc_size), @max_mem_alloc_size, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_MAX_MEM_ALLOC_SIZE:\t\t%u MByte\n", (unsigned int)(max_mem_alloc_size / (1024 * 1024)));
  //Writeln('CL_DEVICE_MAX_MEM_ALLOC_SIZE: ',Round(max_mem_alloc_size/(1024 * 1024)),' MByte');
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_MAX_MEM_ALLOC_SIZE: ' +
            IntToStr(Round(max_mem_alloc_size / (1024 * 1024))) + ' MByte');
  {$ENDIF}

  // CL_DEVICE_GLOBAL_MEM_SIZE
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_GLOBAL_MEM_SIZE, SizeOf(mem_size), @mem_size, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_GLOBAL_MEM_SIZE:\t\t%u MByte\n", (unsigned int)(mem_size / (1024 * 1024)));
  //Writeln('CL_DEVICE_GLOBAL_MEM_SIZE: ',Round(mem_size/(1024 * 1024)),' MByte');
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_GLOBAL_MEM_SIZE: ' +
            IntToStr(Round(mem_size / (1024 * 1024))) + ' MByte');
  {$ENDIF}

  // CL_DEVICE_ERROR_CORRECTION_SUPPORT
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_ERROR_CORRECTION_SUPPORT, SizeOf(error_correction_support), @error_correction_support, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_ERROR_CORRECTION_SUPPORT:\t%s\n", error_correction_support == CL_TRUE ? "yes" : "no");
  if error_correction_support = CL_TRUE then begin
    //Writeln('CL_DEVICE_ERROR_CORRECTION_SUPPORT: yes');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_ERROR_CORRECTION_SUPPORT: yes');
    {$ENDIF}
  end else begin
    //Writeln('CL_DEVICE_ERROR_CORRECTION_SUPPORT: no');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_ERROR_CORRECTION_SUPPORT: no');
    {$ENDIF}
  end;

  // CL_DEVICE_LOCAL_MEM_TYPE
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_LOCAL_MEM_TYPE, SizeOf(local_mem_type), @local_mem_type, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_LOCAL_MEM_TYPE:\t\t%s\n", local_mem_type == 1 ? "local" : "global");
  if local_mem_type = 1 then begin
    //Writeln('CL_DEVICE_LOCAL_MEM_TYPE: local');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_LOCAL_MEM_TYPE: local');
    {$ENDIF}
  end else begin
    //Writeln('CL_DEVICE_LOCAL_MEM_TYPE: global');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_LOCAL_MEM_TYPE: global');
    {$ENDIF}
  end;

  // CL_DEVICE_LOCAL_MEM_SIZE
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_LOCAL_MEM_SIZE, SizeOf(mem_size), @mem_size, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_LOCAL_MEM_SIZE:\t\t%u KByte\n", (unsigned int)(mem_size / 1024));
  //Writeln('CL_DEVICE_LOCAL_MEM_SIZE: ',Round(mem_size/(1024)),' KByte');
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_LOCAL_MEM_SIZE: ' +
            IntToStr(Round(mem_size / (1024))) + ' KByte');
  {$ENDIF}

  // CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE, SizeOf(mem_size), @mem_size, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE:\t%u KByte\n", (unsigned int)(mem_size / 1024));
  //Writeln('CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE: ',Round(mem_size/(1024)),' KByte');
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE: '+IntToStr(Round(mem_size/(1024)))+' KByte');
  {$ENDIF}

  // CL_DEVICE_QUEUE_PROPERTIES

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_QUEUE_PROPERTIES, SizeOf(queue_properties), @queue_properties, nil);
  if (queue_properties and CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE) <> 0 then begin
      //Writeln('CL_DEVICE_QUEUE_PROPERTIES: CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_QUEUE_PROPERTIES: CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE');
    {$ENDIF}
  end;

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_QUEUE_PROPERTIES:\t\t%s\n", "CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE");

  if (queue_properties and CL_QUEUE_PROFILING_ENABLE) <> 0 then begin
    //Writeln('CL_DEVICE_QUEUE_PROPERTIES: CL_QUEUE_PROFILING_ENABLE');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_QUEUE_PROPERTIES: CL_QUEUE_PROFILING_ENABLE');
    {$ENDIF}
  end;

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_QUEUE_PROPERTIES:\t\t%s\n", "CL_QUEUE_PROFILING_ENABLE");


  // CL_DEVICE_IMAGE_SUPPORT
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_IMAGE_SUPPORT, SizeOf(image_support), @image_support, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_IMAGE_SUPPORT:\t\t%u\n", image_support);
  //Writeln('CL_DEVICE_IMAGE_SUPPORT: ',image_support);
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_IMAGE_SUPPORT: ' + IntToStr(image_support));
  {$ENDIF}

  // CL_DEVICE_MAX_READ_IMAGE_ARGS
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_MAX_READ_IMAGE_ARGS, SizeOf(max_read_image_args), @max_read_image_args, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_MAX_READ_IMAGE_ARGS:\t%u\n", max_read_image_args);
  //Writeln('CL_DEVICE_MAX_READ_IMAGE_ARGS: ',max_read_image_args);
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_MAX_READ_IMAGE_ARGS: ' + IntToStr(max_read_image_args));
  {$ENDIF}

  // CL_DEVICE_MAX_WRITE_IMAGE_ARGS
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_MAX_WRITE_IMAGE_ARGS, SizeOf(max_write_image_args), @max_write_image_args, nil);

  //shrLog(iLogMode, 0.0, "  CL_DEVICE_MAX_WRITE_IMAGE_ARGS:\t%u\n", max_write_image_args);
  //Writeln('CL_DEVICE_MAX_WRITE_IMAGE_ARGS: ',max_write_image_args);
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_MAX_WRITE_IMAGE_ARGS: ' + IntToStr(max_write_image_args));
  {$ENDIF}

  // CL_DEVICE_IMAGE2D_MAX_WIDTH, CL_DEVICE_IMAGE2D_MAX_HEIGHT, CL_DEVICE_IMAGE3D_MAX_WIDTH, CL_DEVICE_IMAGE3D_MAX_HEIGHT, CL_DEVICE_IMAGE3D_MAX_DEPTH

  //shrLog(iLogMode, 0.0, "\n  CL_DEVICE_IMAGE <dim>");
  //Writeln('CL_DEVICE_IMAGE');
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_IMAGE');
  {$ENDIF}

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_IMAGE2D_MAX_WIDTH, SizeOf(Tsize_t), @szMaxDims[0], nil);
  //shrLog(iLogMode, 0.0, "\t\t\t2D_MAX_WIDTH\t %u\n", szMaxDims[0]);
  //Writeln('t2D_MAX_WIDTH: ',szMaxDims[0]);
  {$IFDEF USE_LOG}
    shrLog('t2D_MAX_WIDTH: ' + IntToStr(szMaxDims[0]));
  {$ENDIF}

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_IMAGE2D_MAX_HEIGHT, SizeOf(Tsize_t), @szMaxDims[1], nil);
  //shrLog(iLogMode, 0.0, "\t\t\t\t\t2D_MAX_HEIGHT\t %u\n", szMaxDims[1]);
  //Writeln('t2D_MAX_HEIGHT: ',szMaxDims[1]);
  {$IFDEF USE_LOG}
    shrLog('t2D_MAX_HEIGHT: ' + IntToStr(szMaxDims[1]));
  {$ENDIF}

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_IMAGE3D_MAX_WIDTH, SizeOf(Tsize_t), @szMaxDims[2], nil);
  //shrLog(iLogMode, 0.0, "\t\t\t\t\t3D_MAX_WIDTH\t %u\n", szMaxDims[2]);
  //Writeln('t3D_MAX_WIDTH: ',szMaxDims[2]);
  {$IFDEF USE_LOG}
    shrLog('t3D_MAX_WIDTH: ' + IntToStr(szMaxDims[2]));
  {$ENDIF}
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_IMAGE3D_MAX_HEIGHT, SizeOf(Tsize_t), @szMaxDims[3], nil);

  //shrLog(iLogMode, 0.0, "\t\t\t\t\t3D_MAX_HEIGHT\t %u\n", szMaxDims[3]);
  //Writeln('t3D_MAX_HEIGHT: ',szMaxDims[3]);
  {$IFDEF USE_LOG}
    shrLog('t3D_MAX_HEIGHT: ' + IntToStr(szMaxDims[3]));
  {$ENDIF}

  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_IMAGE3D_MAX_DEPTH, SizeOf(Tsize_t), @szMaxDims[4], nil);
  //shrLog(iLogMode, 0.0, "\t\t\t\t\t3D_MAX_DEPTH\t %u\n", szMaxDims[4]);
  //Writeln('t3D_MAX_DEPTH: ',szMaxDims[4]);
  {$IFDEF USE_LOG}
    shrLog('t3D_MAX_DEPTH: ' + IntToStr(szMaxDims[4]));
  {$ENDIF}

  device_string:='';
  SetLength(stdDevString,1025);

  // CL_DEVICE_EXTENSIONS: get device extensions, and if any then parse & log the string onto separate lines
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_EXTENSIONS, SizeOf(device_string), @device_string, nil);
  //Writeln(Integer(device));
  //device_string:='NIELLO THE BEST hello world';
  if (device_string[0] <> #0) then begin
    //shrLog(iLogMode, 0.0, "\n  CL_DEVICE_EXTENSIONS:");
    //Writeln('CL_DEVICE_EXTENSIONS: ');
    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_EXTENSIONS: ');
    {$ENDIF}

    //stdDevString = std::string(device_string);
    //stdDevString:=stdDevString+'+';
    //stdDevString:=stdDevString+(device_string);
    szOldPos := 1;

    szSpacePos := Pos(' ', string(device_string));//stdDevString);
    //szSpacePos := stdDevString.find(' ', szOldPos); // extensions string is space delimited

    while (szSpacePos <> Cardinal(Length(AnsiString(device_string)))) and
          ((szSpacePos - szOldPos) > 1) and
          (szSpacePos < 1024) do begin
      {
        if( strcmp("cl_nv_device_attribute_query", stdDevString.substr(szOldPos, szSpacePos - szOldPos).c_str()) == 0 )
          nv_device_attibute_query = true;   }

      if 'cl_nv_device_attribute_query' = Copy(AnsiString(device_string), szOldPos, szSpacePos - szOldPos) then
       nv_device_attibute_query := true;

      if (szOldPos > 1) then begin
        //shrLog(iLogMode, 0.0, "\t\t");
        //Writeln(' ');
      end;

      //shrLog(iLogMode, 0.0, "\t\t\t%s\n", stdDevString.substr(szOldPos, szSpacePos - szOldPos).c_str());
      //Writeln(Copy(String(device_string),szOldPos,szSpacePos - szOldPos));
      {$IFDEF USE_LOG}
        shrLog(Copy(String(device_string), szOldPos, szSpacePos - szOldPos));
      {$ENDIF}

      szOldPos := szSpacePos + 1;
      //szSpacePos := stdDevString.find(' ', szOldPos);
      szSpacePos := pos2(' ', AnsiString(device_string), szOldPos);
      if (szOldPos - szSpacePos = 1) then
       Break;;
    end;

    //Writeln(Copy(String(device_string),szOldPos,Length(device_string)-szOldPos+1));
  end else begin
    //shrLog(iLogMode, 0.0, "  CL_DEVICE_EXTENSIONS: None\n");
    //Writeln('CL_DEVICE_MAX_WORK_GROUP_SIZE: None');

    {$IFDEF USE_LOG}
      shrLog('CL_DEVICE_MAX_WORK_GROUP_SIZE: None');
    {$ENDIF}
  end;


  //Writeln(Integer(device));
  if (nv_device_attibute_query) then begin
    clGetDeviceInfo(PCL_device_id(indevice), CL_NV_DEVICE_COMPUTE_CAPABILITY_MAJOR, SizeOf(Tcl_uint), @compute_capability_major, nil);
    clGetDeviceInfo(PCL_device_id(indevice), CL_NV_DEVICE_COMPUTE_CAPABILITY_MINOR, SizeOf(Tcl_uint), @compute_capability_minor, nil);

    //shrLog(iLogMode, 0.0, "\n  CL_NV_DEVICE_COMPUTE_CAPABILITY:\t%u.%u\n", compute_capability_major, compute_capability_minor);
    //Writeln('CL_NV_DEVICE_COMPUTE_CAPABILITY: ',compute_capability_major,' ' ,compute_capability_minor);
    {$IFDEF USE_LOG}
      shrLog('CL_NV_DEVICE_COMPUTE_CAPABILITY: ' +
              IntToStr(compute_capability_major) + ' ' +
              IntToStr(compute_capability_minor));
    {$ENDIF}

    clGetDeviceInfo(PCL_device_id(indevice), CL_NV_DEVICE_REGISTERS_PER_BLOCK, SizeOf(Tcl_uint), @regs_per_block, nil);
    //shrLog(iLogMode, 0.0, "  CL_NV_DEVICE_REGISTERS_PER_BLOCK:\t%u\n", regs_per_block);
    //Writeln('CL_NV_DEVICE_REGISTERS_PER_BLOCK: ',regs_per_block);

    clGetDeviceInfo(PCL_device_id(indevice), CL_NV_DEVICE_WARP_SIZE, SizeOf(Tcl_uint), @warp_size, nil);
    //shrLog(iLogMode, 0.0, "  CL_NV_DEVICE_WARP_SIZE:\t\t%u\n", warp_size);
    //Writeln('CL_NV_DEVICE_WARP_SIZE: ',warp_size);
    {$IFDEF USE_LOG}
      shrLog('CL_NV_DEVICE_WARP_SIZE: '+IntToStr(warp_size));
    {$ENDIF}

    clGetDeviceInfo(PCL_device_id(indevice), CL_NV_DEVICE_GPU_OVERLAP, SizeOf(Tcl_bool), @gpu_overlap, nil);
    //shrLog(iLogMode, 0.0, "  CL_NV_DEVICE_GPU_OVERLAP:\t\t%s\n", gpu_overlap == CL_TRUE ? "CL_TRUE" : "CL_FALSE");
    if gpu_overlap=CL_TRUE then begin
      //Writeln('CL_NV_DEVICE_GPU_OVERLAP: CL_TRUE');
      {$IFDEF USE_LOG}
        shrLog('CL_NV_DEVICE_GPU_OVERLAP: CL_TRUE');
      {$ENDIF}
    end else begin
      //Writeln('CL_NV_DEVICE_GPU_OVERLAP: CL_FALSE');
      {$IFDEF USE_LOG}
        shrLog('CL_NV_DEVICE_GPU_OVERLAP: CL_FALSE');
      {$ENDIF}
    end;

    clGetDeviceInfo(PCL_device_id(indevice), CL_NV_DEVICE_KERNEL_EXEC_TIMEOUT, SizeOf(Tcl_bool), @exec_timeout, nil);
    //shrLog(iLogMode, 0.0, "  CL_NV_DEVICE_KERNEL_EXEC_TIMEOUT:\t%s\n", exec_timeout == CL_TRUE ? "CL_TRUE" : "CL_FALSE");
    if exec_timeout = CL_TRUE then begin
      //Writeln('CL_NV_DEVICE_KERNEL_EXEC_TIMEOUT: CL_TRUE');
      {$IFDEF USE_LOG}
        shrLog('CL_NV_DEVICE_KERNEL_EXEC_TIMEOUT: CL_TRUE');
      {$ENDIF}
    end else begin
      //Writeln('CL_NV_DEVICE_KERNEL_EXEC_TIMEOUT: CL_FALSE');
      {$IFDEF USE_LOG}
        shrLog('CL_NV_DEVICE_KERNEL_EXEC_TIMEOUT: CL_FALSE');
      {$ENDIF}
    end;


    clGetDeviceInfo(PCL_device_id(indevice), CL_NV_DEVICE_INTEGRATED_MEMORY, SizeOf(Tcl_bool), @integrated_memory, nil);
    //shrLog(iLogMode, 0.0, "  CL_NV_DEVICE_INTEGRATED_MEMORY:\t%s\n", integrated_memory == CL_TRUE ? "CL_TRUE" : "CL_FALSE");
    if integrated_memory = CL_TRUE then begin
      //Writeln('CL_NV_DEVICE_INTEGRATED_MEMORY: CL_TRUE');
      {$IFDEF USE_LOG}
        shrLog('CL_NV_DEVICE_INTEGRATED_MEMORY: CL_TRUE');
      {$ENDIF}
    end else begin
      //Writeln('CL_NV_DEVICE_INTEGRATED_MEMORY: CL_FALSE');
      {$IFDEF USE_LOG}
        shrLog('CL_NV_DEVICE_INTEGRATED_MEMORY: CL_FALSE');
      {$ENDIF}
    end;
  end;

  // CL_DEVICE_PREFERRED_VECTOR_WIDTH_<type>
  //shrLog(iLogMode, 0.0, "  CL_DEVICE_PREFERRED_VECTOR_WIDTH_<t>\t");
  //Writeln('CL_DEVICE_PREFERRED_VECTOR_WIDTH_: ');
  {$IFDEF USE_LOG}
    shrLog('CL_DEVICE_PREFERRED_VECTOR_WIDTH_: ');
  {$ENDIF}

  //Writeln(Integer(device));
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR, SizeOf(Tcl_uint), @vec_width[0], nil);
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT, SizeOf(Tcl_uint), @vec_width[1], nil);
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT, SizeOf(Tcl_uint), @vec_width[2], nil);
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG, SizeOf(Tcl_uint), @vec_width[3], nil);
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT, SizeOf(Tcl_uint), @vec_width[4], nil);
  clGetDeviceInfo(PCL_device_id(indevice), CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE, SizeOf(Tcl_uint), @vec_width[5], nil);

  //shrLog(iLogMode, 0.0, "CHAR %u, SHORT %u, INT %u, FLOAT %u, DOUBLE %u\n\n\n",
  //       vec_width[0], vec_width[1], vec_width[2], vec_width[3], vec_width[4]);
  //Writeln('CHAR: ',vec_width[0],';',#13#10,'SHORT: ',vec_width[1],';',#13#10,'INT: ',vec_width[2],';',#13#10,'LONG: ',vec_width[3],';',#13#10,'FLOAT: ',vec_width[4],';',#13#10,'DOUBLE: ',vec_width[5]);
  {$IFDEF USE_LOG}
    shrLog('CHAR: ' + IntToStr(vec_width[0]) + ';' + #13#10 +
            'SHORT: ' + IntToStr(vec_width[1]) + ';' + #13#10 +
            'INT: '+IntToStr(vec_width[2]) + ';' + #13#10 +
            'LONG: '+IntToStr(vec_width[3]) + ';' + #13#10 +
            'FLOAT: ' + IntToStr(vec_width[4]) + ';' + #13#10 +
            'DOUBLE: ' + IntToStr(vec_width[5]));
  {$ENDIF}
    //Writeln(Integer(device));
    device := PCL_device_id(indevice);
end;

function oclGetMaxFlopsDev(cxGPUContext: Pcl_context): PPcl_device_id;
var
  szParmDataBytes: Tsize_t;
  cdDevices: array of Pcl_device_id;
  max_flops: TCL_int;
  max_flops_device: PPcl_device_id;
  current_device: TSize_t;
  compute_units: Tcl_uint;
  clock_frequency: Tcl_uint;
  device_count: TSize_t;
  flops: Tcl_int;
begin
  // get the list of GPU devices associated with context
  clGetContextInfo(cxGPUContext, CL_CONTEXT_DEVICES, 0, nil, @szParmDataBytes);
  //cdDevices := (cl_device_id*) malloc(szParmDataBytes);
  SetLength(cdDevices, szParmDataBytes);
  //GetMem(cdDevices,szParmDataBytes);
  device_count := Round(szParmDataBytes / sizeof(Pcl_device_id));

  clGetContextInfo(cxGPUContext, CL_CONTEXT_DEVICES, szParmDataBytes, cdDevices, nil);

  max_flops_device := @cdDevices[0];
	//max_flops := 0;

	current_device := 0;

  // CL_DEVICE_MAX_COMPUTE_UNITS
  clGetDeviceInfo(cdDevices[current_device], CL_DEVICE_MAX_COMPUTE_UNITS, SizeOf(compute_units), @compute_units, nil);

  // CL_DEVICE_MAX_CLOCK_FREQUENCY
  clGetDeviceInfo(cdDevices[current_device], CL_DEVICE_MAX_CLOCK_FREQUENCY, SizeOf(clock_frequency), @clock_frequency, nil);

	max_flops := compute_units * clock_frequency;
  inc(current_device);
	//++current_device;

	while (current_device < device_count) do begin
    // CL_DEVICE_MAX_COMPUTE_UNITS
    clGetDeviceInfo(cdDevices[current_device], CL_DEVICE_MAX_COMPUTE_UNITS, SizeOf(compute_units), @compute_units, nil);

    // CL_DEVICE_MAX_CLOCK_FREQUENCY
    clGetDeviceInfo(cdDevices[current_device], CL_DEVICE_MAX_CLOCK_FREQUENCY, SizeOf(clock_frequency), @clock_frequency, nil);

    flops := compute_units * clock_frequency;
		if (flops > max_flops) then begin
      max_flops        := flops;
			max_flops_device := @cdDevices[current_device];
		end;
		Inc(current_device);
	end;

  //free(cdDevices);
  SetLength(cdDevices, 0);
  //FreeMem(cdDevices);

	Result := max_flops_device;
end;

function oclLoadProgSource(const cFilename: string;
                            const cPreamble: PAnsiChar;
                             szFinalLength: Psize_t): PAnsiChar;
var
  pFileStream: TextFile;
  cSourceString: AnsiString;
  buf: AnsiString;
begin
  // locals
  //FILE* pFileStream = NULL;
  //size_t szSourceLength;
(*
    // open the OpenCL source code file
    #ifdef _WIN32   // Windows version
        if(fopen_s(&pFileStream, cFilename, "rb") != 0)
        {
            return NULL;
        }
    #else           // Linux version
        pFileStream = fopen(cFilename, "rb");
        if(pFileStream == 0)
        {
            return NULL;
        }
    #endif
*)

  Assign(pFileStream, cFilename);
  Reset(pFileStream);

  {
    // get the length of the source code
    fseek(pFileStream, 0, SEEK_END);
    szSourceLength = ftell(pFileStream);
    fseek(pFileStream, 0, SEEK_SET);
  }
  // allocate a buffer for the source code string and read it in
  //cSourceString := (char *)malloc(szSourceLength + szPreambleLength + 1);

  cSourceString := '' + AnsiString(cPreamble);
  //memcpy(cSourceString, cPreamble, szPreambleLength);

  (*
    if (fread((cSourceString) + szPreambleLength, szSourceLength, 1, pFileStream) != 1)
    {
        fclose(pFileStream);
        free(cSourceString);
        return 0;
    }
  *)
  while not EOF(pFileStream) do begin
    Readln(pFileStream, buf);
    cSourceString := cSourceString + buf + #10 + #13;
  end;
  CloseFile(pFileStream);

  // close the file and return the total length of the combined (preamble + source) string
  //fclose(pFileStream);

  if (szFinalLength <> nil) then begin
    szFinalLength^ := Length(cSourceString);
  end;

  //GetMemory(Result,Length(cSourceString));
  Result := PAnsiChar(cSourceString);
end;

(*
procedure oclGetProgBinary(cpProgram: Pcl_program;
                           var cdDevice: Pcl_device_id;
                           var binary: PPAnsiChar;
                           length: Psize_t);
type
 PAPAnsiChar = ^APAnsiChar;
 APAnsiChar= Array of PAnsiChar;
 PAPCL_device_id = ^APCL_device_id;
 APCL_device_id = Array of PCL_device_id;

 PASize_t = ^ASize_t;
 ASize_t = Array of TSize_t;
var
  num_devices: Tcl_uint;
  devices: PAPCL_device_id;
  binary_sizes: PASize_t;
  ptx_code: PAPAnsiChar;
  i: Cardinal;
  idx: Cardinal;
  Status: TCL_int;
begin
    // Grab the number of devices associated witht the program

    Status:=clGetProgramInfo(cpProgram, CL_PROGRAM_NUM_DEVICES, sizeof(Tcl_uint), @num_devices, nil);
    Writeln(Status);
    // Grab the device ids
    //cl_device_id* devices = (cl_device_id* ) malloc(num_devices * sizeof(cl_device_id));
    //SetLength(devices,num_devices*sizeof(PCL_device_id));
    GetMem(devices,num_devices*sizeof(PCL_device_id));
    Status:=clGetProgramInfo(cpProgram, CL_PROGRAM_DEVICES, num_devices * sizeof(Pcl_device_id), devices, nil);
    Writeln(Status);
    // Grab the sizes of the binaries
    //size_t* binary_sizes = (size_t* )malloc(num_devices * sizeof(size_t));
    //SetLength(binary_sizes,num_devices*sizeOf(TSize_t));
    GetMem(binary_sizes,num_devices*sizeOf(TSize_t));
    Status:=clGetProgramInfo(cpProgram, CL_PROGRAM_BINARY_SIZES, num_devices * sizeof(Tsize_t), binary_sizes^, nil);
    Writeln(Status);
    // Now get the binaries
    //char** ptx_code = (char** ) malloc(num_devices * sizeof(char* ));
    //SetLength(ptx_code,num_devices{*sizeOf(PAnsiChar)});
    GetMem(ptx_code,num_devices);
    //GetMem(ptx_code);
    {$R-}
    for i:=0 to num_devices-1 do
    begin
     //Writeln(Integer(binary_sizes^[i]));
     ptx_code^[i]:=PAnsichar(binary_sizes^[i]);
     //GetMem(ptx_code^[i],binary_sizes^[i]);
    end;
    {$R+}

    Status:=clGetProgramInfo(cpProgram, CL_PROGRAM_BINARIES, 4, ptx_code^, nil);
    Writeln(Status);

    // Find the index of the device of interest
    //unsigned int idx = 0;

    idx:=0;
    while( (idx<num_devices) and (devices^[idx] <> cdDevice) )do
    begin
      inc(idx);
    end;

    // If it is associated prepare the result

    //binary:='fdfdf';
    if( idx <= num_devices )then
    begin
        //*binary = ptx_code[idx];
        //GetMem(binary,10000);
        //binary:=Pointer(ptx_code^[idx]);
        Writeln(ptx_code^[idx-1]);
        //*length = binary_sizes[idx];
        //length:= @binary_sizes[idx];
    end;


    // Cleanup
    //free( devices );
    //SetLength(devices,0);
    FreeMemory(devices);
    //free( binary_sizes );
    //SetLength(binary_sizes,0);
    FreeMemory(binary_sizes);

    //for( unsigned int i=0; i<num_devices; ++i)
    //{
    //    if( i != idx ) free(ptx_code[i]);
    //}
    //

    for i:=0 to num_devices-1 do
    begin
      if i<>idx then FreeMemory(ptx_code);//SetLength(ptx_code[i]^,0);
    end;

    //free( ptx_code );
    FreeMemory(ptx_code);
end;
*)
(*
procedure oclLogPtx(cpProgram: Pcl_program;
                    cdDevice: Pcl_device_id;
                    const cPtxFileName: AnsiString);
var
  num_devices: TCL_uint;
  devices: Array of PCL_device_id;
  binary_sizes: Array of TSize_t;
  ptx_code: Array of PAnsiChar;
  i: cardinal;
  idx: cardinal;
  pFileStream: File;

  Status: TCL_int;
begin
    // Grab the number of devices associated with the program
    Status:=clGetProgramInfo(cpProgram, CL_PROGRAM_NUM_DEVICES, sizeof(Tcl_uint), @num_devices, nil);
    Writeln(Status);
    // Grab the device ids
    //cl_device_id* devices = (cl_device_id* ) malloc(num_devices * sizeof(cl_device_id));
    SetLength(devices,num_devices*SizeOf(Pcl_device_id));
    Status:=clGetProgramInfo(cpProgram, CL_PROGRAM_DEVICES, num_devices * sizeof(Pcl_device_id), devices, nil);
    Writeln(Status);
    // Grab the sizes of the binaries
    //size_t* binary_sizes = (size_t* )malloc(num_devices * sizeof(size_t));
    SetLength(binary_sizes,num_devices*SizeOf(TSize_t));
    Status:=clGetProgramInfo(cpProgram, CL_PROGRAM_BINARY_SIZES, num_devices * sizeof(Tsize_t), binary_sizes, nil);
    Writeln(Status);
    // Now get the binaries
    //char** ptx_code = (char** )malloc(num_devices * sizeof(char* ));
    SetLength(ptx_code,num_devices*SizeOf(PAnsiChar));
    for i:=0 to num_devices-1 do
    begin
      //SetLength(ptx_code[i],binary_sizes[i]);
      GetMem(ptx_code[i],binary_sizes[i]);
    end;
    //for( unsigned int i=0; i<num_devices; ++i)
    {
        ptx_code[i] = (char* )malloc(binary_sizes[i]);
    }
    Status:=clGetProgramInfo(cpProgram, CL_PROGRAM_BINARIES, binary_sizes[0], ptx_code, nil);
    Writeln(Status);
    // Find the index of the device of interest
    idx := 0;
    while((idx < num_devices) and (devices[idx] <> cdDevice))do
    begin
        inc(idx);
    end;
    
    // If the index is associated, log the result
    if(idx < num_devices)then
    begin
         
        // if a separate filename is supplied, dump ptx there 
        if (cPtxFileName<>'')then
        begin
            //shrLog(LOGBOTH, 0, "\nWriting ptx to separate file: %s ...\n\n", cPtxFileName);
            //Writeln('');
            //MainLog('');
            //Writeln('Writing ptx to separate file: ',cPtxFileName);
            //MainLog('Writing ptx to separate file: '+cPtxFileName);
            //Writeln('');
            //MainLog('');
            {$IFDEF USE_LOG}
              shrLog('');
              shrLog('Writing ptx to separate file: '+cPtxFileName);
              shrLog('');
            {$ENDIF}
            //FILE* pFileStream = NULL;
            //#ifdef _WIN32
                //fopen_s(&pFileStream, cPtxFileName, "wb");
                AssignFile(pFileStream,cPtxFileName);
                Rewrite(pFileStream,1);
            //#else
            //    pFileStream = fopen(cPtxFileName, "wb");
            //#endif

            //fwrite(ptx_code[idx], binary_sizes[idx], 1, pFileStream);
            BlockWrite(pFileStream,ptx_code[idx],binary_sizes[idx]);
            //fclose(pFileStream);
            CloseFile(pFileStream);
        end
        else // log to logfile and console if no ptx file specified
        begin
           //shrLog(LOGBOTH, 0, "\n%s\nProgram Binary:\n%s\n%s\n", HDASHLINE, ptx_code[idx], HDASHLINE);
           //Writeln('');
           //Writeln('____________________________');
           //Writeln('nProgram Binary: ',ptx_code[idx]);
           //Writeln('____________________________');
           //Writeln('');
            {$IFDEF USE_LOG}
              shrLog('');
              shrLog('____________________________');
              shrLog('nProgram Binary: '+ptx_code[idx]);
              shrLog('____________________________');
              shrLog('');
            {$ENDIF}
        end;
    end;

    // Cleanup
    //free(devices);
    SetLength(devices,0);
    //free(binary_sizes);
    SetLength(binary_sizes,0);
    //for(unsigned int i = 0; i < num_devices; ++i)
    {
        free(ptx_code[i]);
    }
    for i:=0 to num_devices-1 do
    begin
      //SetLength(ptx_code[i],0);
      FreeMemory(ptx_code[i]);
    end;
    //free( ptx_code );
    SetLength(ptx_code,0)
end;
*)

procedure oclLogBuildInfo(cpProgram: Pcl_program;
                            var cdDevice: Pcl_device_id);
var
  cBuildLog: array [0..1023] of AnsiChar;
//  Status: TCL_int;
  inDevice: Integer;
begin
  inDevice := Integer(cdDevice);
  // write out the build log and ptx, then exit
  clGetProgramBuildInfo(cpProgram, PCl_device_id(inDevice), CL_PROGRAM_BUILD_LOG,
                          SizeOf(cBuildLog), @cBuildLog, nil);
  //Writeln('__________________________________');
  //Writeln(cBuildLog);
  //Writeln('__________________________________');
  {$IFDEF USE_LOG}
    shrLog('__________________________________');
    shrLog(cBuildLog);
    shrLog('__________________________________');
  {$ENDIF}
  //shrLog(LOGBOTH, 0, "\n%s\nBuild Log:\n%s\n%s\n", HDASHLINE, cBuildLog, HDASHLINE);
  cdDevice := PCl_device_id(inDevice);
end;



procedure oclDeleteMemObjs(cmMemObjs: PPcl_mem;
                            iNumObjs: Integer);
type
  PAPcl_mem = ^APcl_mem;
  APcl_mem  = array of PCL_mem;
var
  i: Integer;
//  status: TCL_int;
begin
  for i := 0 to iNumObjs - 1 do begin
    if (@APcl_mem(cmMemobjs)[i] <> nil)then begin
      {status:=}clReleaseMemObject(PCL_mem(@APCL_mem(cmMemobjs^)[i]));
      //Writeln(status);
    end;
  end;
end;

procedure ___ocl_insite_log_writeln(const message: AnsiString);
begin
  Writeln(message);
end;

procedure shrLog(const message: AnsiString);
begin
  if (Pointer(@MainLog) <> nil) then
   MainLog(message);
end;

initialization
{$IFDEF USE_LOG}
  MainLog := ___ocl_insite_log_writeln;
{$ENDIF}
//{$IFDEF DEFAULT_NVIDIA}
//  Default_Platform := NVIDIA_PLATFORM;
//{$ELSE}
//  {$IFDEF DEFAULT_ATI}
//     Default_Platform := ATI_PLATFORM;
//  {$ENDIF}
//{$ENDIF}

end.
