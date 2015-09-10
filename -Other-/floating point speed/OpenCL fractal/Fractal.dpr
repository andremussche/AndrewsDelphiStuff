(* ********************************************************************** *)
(* *)
(* OpenCL1.0 and Delphi and Windows *)
(* *)
(* project site    : http://code.google.com/p/delphi-opencl/ *)
(* *)
(* file name       : Fractal.pas *)
(* last modify     : 27.01.11 *)
(* license         : BSD *)
(* *)
(* created by      : Maksim Tymkovich *)
(* (niello) *)
(* Site            : www.niello.org.ua *)
(* e-mail          : muxamed13@ukr.net *)
(* ICQ             : 446-769-253 *)
(* *)
(* and             : Alexander Kiselev *)
(* (Igroman) *)
(* Site : http://Igroman14.livejournal.com *)
(* e-mail          : Igroman14@yandex.ru *)
(* ICQ             : 207-381-695 *)
(* *)
(* ***********************delphi-opencl2010-2011************************* *)
program Fractal;

{$APPTYPE CONSOLE}

uses
  CL,
  CL_platform,
  ShellAPI, // ShellExecute
  oclutils, // oclGetFirstDevice...
  Math, SysUtils, Classes,
  Windows;

//{$define SINGLE_EXECUTE}      //uncomment this line for single threaded GPU

{$ifdef SINGLE_EXECUTE}
//note: execution can take up 12s! (500x500)
const
//   C_Width  = 200;
//   C_Height = 200;
   C_Width  = 1024;         //change this constant in fractal.cl too!
   C_Height = 1024;
{$else}
const
//  C_Width = 2048;
//  C_Height = 2048;
   C_Width  = 1024;
   C_Height = 1024;
{$endif}

var
  host_image: Array [0 .. C_Width * C_Height * 4] of Byte;

procedure SliceToFile(const FileName: AnsiString);
var
  F: File;
  BFH: TBitmapFileHeader;
  BIH: TBitmapInfoHeader;
begin
  Assign(F, FileName);
  Rewrite(F, 1);
  ZeroMemory(@BFH, SizeOf(BFH));
  ZeroMemory(@BIH, SizeOf(BIH));
  with BFH do
  begin
    bfType := $4D42;
    bfSize := SizeOf(BFH) + SizeOf(BIH) + C_Width * C_Height * 4;
    bfOffBits := SizeOf(BIH) + SizeOf(BFH);
  end;
  BlockWrite(F, BFH, SizeOf(BFH));
  with BIH do
  begin
    biSize := SizeOf(BIH);
    biWidth := C_Width;
    biHeight := C_Height;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biSizeImage := C_Width * C_Height * 4;
  end;
  BlockWrite(F, BIH, SizeOf(BIH));
  BlockWrite(F, host_image, C_Width * C_Height * 4 * SizeOf(Byte));
  Close(F);
end;

type
	// platform+device_type couples
	t_platform_device = record
		platform_id: Pcl_platform_id; // index for platform_id
		device_type: Tcl_device_type; // CL_DEVICE_TYPE_CPU; CL_DEVICE_TYPE_GPU; CL_DEVICE_TYPE_ACCELERATOR
	end;
var
  platform_devices: array of T_platform_device;
  GSelectedDeviceIndex: Integer = 0;

procedure ReadOpenCLCouples;
const
  c_device_types:array[1..3] of cardinal =
    (CL_DEVICE_TYPE_CPU, CL_DEVICE_TYPE_GPU, CL_DEVICE_TYPE_ACCELERATOR);
var
  i, j: integer;
  num_devices_returned,
	num_platforms_returned: TCL_uint;
  i_pl: integer;
  i_type: integer;
  device_vendor, device_name: ansistring;
  returned_size: Tsize_t;
  errcode_ret: TCL_Int;
  platform_id: array of Pcl_platform_id; // platform id
	device_ids: array of Pcl_device_id; // compute device IDs
  sPlatform: string;
begin
  // Number of platforms
	errcode_ret := clGetPlatformIDs(0, nil, @num_platforms_returned);
  //Writeln('clGetPlatformIDs - ' + GetString(Status));

  // Connect to a platform
  SetLength(platform_id, num_platforms_returned);
	errcode_ret := clGetPlatformIDs(num_platforms_returned, @platform_id[0], @num_platforms_returned);
  //Writeln('clGetPlatformIDs - ' + GetString(Status));

	// find platform+device_type
	SetLength(platform_devices, 0);
  for i_pl := 0 to num_platforms_returned - 1 do
    for i_type := 1 to 3 do
      if (CL_SUCCESS = clGetDeviceIDs(platform_id[i_pl], c_device_types[i_type], 0, nil, @num_devices_returned)) and
         (num_devices_returned >= 1) then
      begin
        SetLength(platform_devices, Length(platform_devices)+1);
        platform_devices[High(platform_devices)].platform_id := platform_id[i_pl];
        platform_devices[High(platform_devices)].device_type := c_device_types[i_type];
      end;

	// list platform+device_type couples
  //ListCouple.Clear;
	for i := 0 to High(platform_devices) do
  begin
		errcode_ret := clGetDeviceIDs(platform_devices[i].platform_id, platform_devices[i].device_type, 0, nil, @num_devices_returned);
    //Writeln('clGetDeviceIDs - ' + GetString(errcode_ret));

		SetLength(device_ids, num_devices_returned);
		errcode_ret := clGetDeviceIDs(platform_devices[i].platform_id, platform_devices[i].device_type, num_devices_returned, @device_ids[0], @num_devices_returned);
		for j := 0 to num_devices_returned-1 do
    begin
      SetLength(device_name, 1024);
			clGetDeviceInfo(device_ids[j], CL_DEVICE_NAME, Length(device_name), PAnsiChar(device_name), @returned_size);
      SetLength(device_name, Min(Pos(#0, device_name)-1, returned_size-1));
      SetLength(device_vendor, 1024);
			clGetDeviceInfo(device_ids[j], CL_DEVICE_VENDOR, Length(device_vendor), PAnsiChar(device_vendor), @returned_size);
      SetLength(device_vendor, Min(Pos(#0, device_vendor)-1, returned_size-1));
      //ListCouple.Items.Add('Vendor: '+device_vendor+'; Device: '+device_name);
      Writeln(IntToStr(i) + ' - Vendor: '+device_vendor+'; Device: '+device_name);
		end;
		SetLength(device_ids, 0);
	end;
  //if ListCouple.Items.Count>0 then ListCouple.ItemIndex:=0;

  Writeln('Select device:');
  ReadLn(sPlatform);
  GSelectedDeviceIndex := StrToIntDef(sPlatform, 0);
end;

procedure Init;
begin
  {
    Writeln('Select OpenCL library');
    Writeln('0: - atiocl.dll');
    Writeln('1: - OpenCL.dll');
    Readln(LibraryName);
    if LibraryName=0 then InitOpenCL('atiocl.dll')
    else InitOpenCL('OpenCL.dll'); }
  if not InitOpenCL('OpenCL.dll') then
  begin
    InitOpenCL('atiocl.dll');
  end;
end;

function ConvertToString(const Filename: string): ansistring;
var
  str: TStrings;
begin
  with TStringList.Create do
  begin
    LoadFromFile(Filename);
    Result := Text;
    Free;
  end;
  {
  with TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite) do
    try
      SetLength(Result, Size);
      if Size > 0 then
        Read(Result[1], Size);
    finally
      Free;
    end;
  }
end;

var
  properties: array [0 .. 2] of PCL_context_properties = (
    PCL_context_properties
    (
      CL_CONTEXT_PLATFORM
    ),
    nil,
    nil
  );

procedure MainProcedure;
var
  hContext: Pcl_context;
  Device: PPCL_device_id;
  hCmdQueue: Pcl_command_queue;
  hProgram: Pcl_program;
  hKernel: Pcl_kernel;
  Status: TCL_Int;
  sProgramSource: AnsiString;
  pProgramSource: PAnsiChar;

  Image: PCL_Mem;
  device_work_size, device_work_off: Array [0 .. 1] of TSize_t;

  // LibraryName: Integer;
  platforms: PCL_platform_id;
  num_devices_returned: TCL_uint;
	device_ids: array of Pcl_device_id; // compute device IDs

  returned_size: TSize_t;
  s: AnsiString;
  start, stop, freq: Int64;
begin
  QueryPerformanceFrequency(freq);

	Status := clGetDeviceIDs(platform_devices[GSelectedDeviceIndex].platform_id, platform_devices[GSelectedDeviceIndex].device_type, 0, nil, @num_devices_returned);
  SetLength(device_ids, num_devices_returned);
  Status := clGetDeviceIDs(platform_devices[GSelectedDeviceIndex].platform_id, platform_devices[GSelectedDeviceIndex].device_type, num_devices_returned, @device_ids[0], @num_devices_returned);
	hContext := clCreateContext(nil, num_devices_returned, @device_ids[0], nil, nil, @Status);

  {
  Status := clGetPlatformIDs(1, @platforms, nil);
  Writeln('clGetPlatformIDs - ' + GetString(Status));
  properties[1] := PCL_context_properties(platforms);
  hContext := clCreateContextFromType(@properties, CL_DEVICE_TYPE_ALL, nil,
    nil, @Status);
  Writeln('clCreateContextFromType - ' + GetString(Status));
  }

  Device := oclGetFirstDev(hContext);
  hCmdQueue := clCreateCommandQueue(hContext, Device^, 0, @Status);
  Writeln('clCreateCommandQueue - ' + GetString(Status));

  QueryPerformanceCounter(start);
  //sProgramSource := oclLoadProgSource('fractal.cl', '', nil);
  sProgramSource := ConvertToString('fractal.cl');
  pProgramSource := PAnsiChar(sProgramSource);
  hProgram := clCreateProgramWithSource(hContext, 1, @pProgramSource,
    nil, @Status);
  Writeln('clCreateProgramWithSource - ' + GetString(Status));
  Status := clBuildProgram(hProgram, 0, nil, nil, nil, nil);
  QueryPerformanceCounter(stop);
  Writeln('oclLoadProgSource + clCreateProgramWithSource + clBuildProgram - ' +
    GetString(Status) + Format(' in %.1f milliseconds',
    [(stop - start) / freq * 1000]));
  if Status <> CL_SUCCESS then
  // show compile warnings, errors etc
  begin
    Device := oclGetFirstDev(hContext);
    Status := clGetProgramBuildInfo(hProgram, Device^, CL_PROGRAM_BUILD_LOG, 0,
      nil, @returned_size);
    Writeln('clGetProgramBuildInfo - ' + GetString(Status));
    SetLength(s, returned_size + 2);
    Device := oclGetFirstDev(hContext);
    Status := clGetProgramBuildInfo(hProgram, Device^, CL_PROGRAM_BUILD_LOG,
      Length(s), PAnsiChar(s), @returned_size);
    SetLength(s, Min(Pos(#0, s) - 1, returned_size - 1));
    Writeln(s);
  end;

  Image := clCreateBuffer(hContext, CL_MEM_WRITE_ONLY, C_Width * C_Height * 4 *
    SizeOf(Byte), nil, @Status);
  Writeln('clCreateBuffer - ' + GetString(Status));

  {$ifdef SINGLE_EXECUTE}
  hKernel := clCreateKernel(hProgram, 'render_fixed', @Status);
  device_work_size[0] := 1;  //one execution
  device_work_size[1] := 1;
  {$else}
  hKernel := clCreateKernel(hProgram, 'render', @Status);
  //parallel execution
  device_work_size[0] := C_Width;
  device_work_size[1] := C_Height;
  {$endif}
  Writeln('clCreateKernel - ' + GetString(Status));

  Status := clSetKernelArg(hKernel, 0, SizeOf(PCL_Mem), @Image);
  Writeln('clSetKernelArg - ' + GetString(Status));

  device_work_off[0] := C_Height;
  device_work_off[1] := 0;

  QueryPerformanceCounter(start);
  //queue
  Status := clEnqueueNDRangeKernel(hCmdQueue, hKernel, 2, nil,
    @device_work_size, nil, 0, nil, nil);
  QueryPerformanceCounter(stop);
  Writeln('clEnqueueNDRangeKernel - ' + GetString(Status) +
    Format(' in %.1f milliseconds', [(stop - start) / freq * 1000]));

  QueryPerformanceCounter(start);
  //read memory
  Status := clEnqueueReadBuffer(hCmdQueue, Image, CL_FALSE, 0,
    SizeOf(Byte) * C_Width * C_Height * 4, @host_image, 0, nil, nil);
  QueryPerformanceCounter(stop);
  Writeln('clEnqueueReadBuffer - ' + GetString(Status) +
    Format(' in %.1f milliseconds', [(stop - start) / freq * 1000]));

  QueryPerformanceCounter(start);
  //wait till execution ready
  Status := clFinish(hCmdQueue);
  QueryPerformanceCounter(stop);
  Writeln('clFinish - ' + GetString(Status) +
    Format(' in %.1f milliseconds', [(stop - start) / freq * 1000]));

  Status := clReleaseCommandQueue(hCmdQueue);
  Writeln('clReleaseCommandQueue - ' + GetString(Status));

  Status := clReleaseContext(hContext);
  Writeln('clReleaseContext - ' + GetString(Status));

  SliceToFile('C:\OpenCL.bmp');
  ShellExecute(0, 'Open', 'C:\OpenCL.bmp', '', '', SW_SHOWMAXIMIZED);

  Writeln('Press enter...');
  ReadLn;
end;

begin
  try
    Init;
    ReadOpenCLCouples;
    MainProcedure;
  except
    on e:exception do
      Writeln('Error: ' + e.Message);
  end;
end.
