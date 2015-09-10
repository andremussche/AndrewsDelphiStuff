{***********************************************************************
Denomo 2.1.0
http://www.kbasm.com/

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
for the specific language governing rights and limitations under the
License.

The Original Code is DenomoUtils.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit DenomoUtils;

interface

{$include Denomo.inc}

uses
  Windows, Messages,
  MiscUtils,
  DenomoMemHook, DenomoHandleHook, DenomoConfig,
  CodeHookIntf;

const
  //if you change this value, LeakInspector must be re-compiled with the same value.
  SharedRecordDataSize = 1024 * 1024;

  //SOT -> String Output Type
  SOT_Print = 0;
  SOT_Error = 1;
  SOT_YesNo = 2;

  //SOC -> string output class
  SOC_DebugString = 0;
  SOC_ToInspector = 1;
  SOC_ToFile = 2;
  SOC_ToMessageBox = 3;

  SOC_Count = 4;

type
  TNotifyEvent = procedure(Sender: TObject) of object;

  TDebugSourceLocationInfo = packed record
    UnitName: TSafeString;
    ProcedureName: TSafeString;
    LineNumber: Integer;
  end;
  PDebugSourceLocationInfo = ^TDebugSourceLocationInfo;

  TDllServiceShutDown = procedure; stdcall;
  IDllService = interface
    procedure GetCodeHook(out Obj: ICodeHook); stdcall;

    function GetDebugSourceLocationInfo(AAddr: Pointer;
      AInfo: PDebugSourceLocationInfo): LongBool; stdcall;
    procedure GetProcAddressesFromNames(AModule: HMODULE;
      AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer); stdcall;
    function GetModuleImageSize(AModule: HMODULE): Cardinal; stdcall;
    procedure SetOnShutDown(AOnShutDown: TDllServiceShutDown); stdcall;
  end;

  THookProc = function(ATarget, AHook: Pointer; AOldFunc: PPointer): LongBool; stdcall;
  TUnhookProc = function(ATarget: Pointer): Pointer; stdcall;
  TGetDebugSourceLocationInfo = function(AAddr: Pointer; AInfo: PDebugSourceLocationInfo): LongBool; stdcall;
  TGetProcAddressesFromNames = procedure(AModule: HMODULE;
      AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer); stdcall;

  TInterObject = class;

  THookOption = packed record
    Flags: Cardinal;
  end;
  PHookOption = ^THookOption;

  TLeakEliminateInfo = packed record
    DerivedLeakCount: Integer;
    SameCallingPathLeakCount: Integer;
    LeakInfoRemoverCacheRootNode: Pointer;
    LeakInfoRemoverCacheNodeMax: array[0..1] of Pointer;
  end;
  PLeakEliminateInfo = ^TLeakEliminateInfo;

  TEnumHookIndexParamedData = packed record
    Data: TEnumHookIndexData;
    MatchMemTypes: Integer;
    MatchGroupID: Integer;
    //return Result here
    ResultValue: Integer;
    StringOutputType: Integer;
    //memory block count
    Count: Integer;
    HandleCount: Integer;

    GroupID: Integer;
    HookOption: THookOption;

    LeakEliminateInfo: TLeakEliminateInfo;

    GDISubTypeCount: array[0 .. HandleSubType_GDI_Count - 1] of Integer;
  end;
  PEnumHookIndexParamedData = ^TEnumHookIndexParamedData;

  TSharedRecord = packed record
    EnumData: TEnumHookIndexParamedData;
    ChunkData: array[0..SharedRecordDataSize - 1] of Byte;
  end;
  PSharedRecord = ^TSharedRecord;

  TStringOutput = class
  private
    FUpdateCount: Integer;
  protected
    procedure DoOutput(const S: string); virtual; abstract;
    procedure DoBeginOutput; virtual;
    procedure DoEndOutput; virtual;

    property UpdateCount: Integer read FUpdateCount;
  public
    procedure BeginOutput;
    procedure EndOutput;
    procedure Output(const S: string);
    procedure Flush; virtual;
  end;

  TStringOutputDebugString = class(TStringOutput)
  protected
    procedure DoOutput(const S: string); override;
  end;

  TStringOutputToInspector = class(TStringOutput)
  private
    FInterObject: TInterObject;
  protected
    procedure DoOutput(const S: string); override;

    property InterObject: TInterObject read FInterObject;
  public
    constructor Create(AInterObject: TInterObject);

    procedure Flush; override;
  end;

  TStringOutputToFile = class(TStringOutput)
  private
    FFileHandle: Cardinal;
    FHandleRequired: Boolean;
    FFileName: string;
  protected
    procedure DoOutput(const S: string); override;
    procedure RequireHandle;

    property FileHandle: Cardinal read FFileHandle;
    property HandleRequired: Boolean read FHandleRequired;
    property FileName: string read FFileName;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    procedure Flush; override;
  end;

  TStringOutputToMessageBox = class(TStringOutput)
  private
    FBufferLength: Integer;
    FCapacity: Integer;
    FBuffer: array of Char;
  protected
    procedure DoOutput(const S: string); override;

    property BufferLength: Integer read FBufferLength;
    property Capacity: Integer read FCapacity;
  public
    procedure Flush; override;
  end;

  TInterObject = class
  private
    FGuestWinHandle: HWND;
    FMyWinHandle: HWND;
    FShareMemoryPointer: PSharedRecord;
    FShareMemoryHandle: THandle;
    FStringBufferReader: TSimpleStringBufferReader;
    FStringBufferWriter: TSimpleStringBufferWriter;
    FOnDisconnected: TNotifyEvent;
    FOnConnected: TNotifyEvent;
    function GetConnected: Boolean;
  protected
    function GetMyWinClassName: string; virtual; abstract;
    function GetGuestWinClassName: string; virtual; abstract;
    function DoMoreLeakMsg(Msg: Integer; Param: Cardinal): Integer; virtual;
    procedure WndProc(var Message: TMessage);
    function DoLeakMsg(Msg: Integer; Param: Cardinal): Integer;

    procedure InitShareMemory;
    procedure DeInitShareMemory;
    function TryConnect: Boolean;
    procedure CreateCommunicateWindow;

    procedure DoOnConnected; virtual;
    procedure DoOnDisconnected; virtual;

    procedure DoInit; virtual;
    procedure DoDeInit; virtual;

    property MyWinHandle: HWND read FMyWinHandle;
    property GuestWinHandle: HWND read FGuestWinHandle;
    property ShareMemoryHandle: THandle read FShareMemoryHandle;
    property ShareMemoryPointer: PSharedRecord read FShareMemoryPointer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(AConnect: Boolean);
    procedure DeInit;

    function TalkToGuest(Msg: Integer; Param: Cardinal): Cardinal;
    function ResetShareMemory: PSharedRecord;
    function GetShareMemory: PSharedRecord;

    function GuestAvailable: Boolean;

    property StringBufferReader: TSimpleStringBufferReader read FStringBufferReader;
    property StringBufferWriter: TSimpleStringBufferWriter read FStringBufferWriter;

    property Connected: Boolean read GetConnected;

    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
  end;

  TDenomoPackedConfig = array[0..1023] of Char;
  PDenomoPackedConfig = ^TDenomoPackedConfig;

  TDenomoConfigTypeInfo = class
  private
    FAddr: Pointer;
    FName: string;
    FVarType: TVarType;
  protected
    procedure ErrorUnknownType;
  public
    function Pack(ABuffer: Pointer): Pointer;
    function Unpack(ABuffer: Pointer): Pointer;
    procedure LoadFromIni(AIni: TSimpleIniFile);
    procedure SaveToIni(AIni: TSimpleIniFile);

    property Name: string read FName write FName;
    property Addr: Pointer read FAddr write FAddr;
    property VarType: TVarType read FVarType write FVarType;
  end;

  TDenomoConfig = class
  private
    FFileName: string;
    FPreviousConfig: TDenomoPackedConfig;
    FTypeInfos: array of TDenomoConfigTypeInfo;
  protected
    {#@FIELD auto generated code start, don't touch }
    procedure InitTypeInfos;
    {#@FIELD auto generated code end }

  protected
    procedure LoadFromIni(AIni: TSimpleIniFile);
    procedure SaveToIni(AIni: TSimpleIniFile);

    procedure LoadFromFile;
    procedure SaveToFile;
    procedure SolveFileName;

    function IsDirty: Boolean;

    property FileName: string read FFileName;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure DeInit;

    procedure Pack(APackedConfig: PDenomoPackedConfig);
    procedure Unpack(APackedConfig: PDenomoPackedConfig);
  end;

  TSortedListKey = Cardinal;

  TSortedListItem = packed record
    Key: TSortedListKey;
    Value: Pointer;
  end;

  TSortedList = class
  private
    FItems: array of TSortedListItem;
    FCapacity: Integer;
    FCount: Integer;
    function GetValue(Index: Integer): Pointer;
  protected
    procedure Grow;

    function DoFindItem(const AKey: TSortedListKey; var ANearIndex: Integer): Integer;
    function CompareItem(const AKey1, AKey2: TSortedListKey): Integer; virtual;

    property Capacity: Integer read FCapacity;
  public
    constructor Create;
    destructor Destroy; override;

    function FindItem(const AKey: TSortedListKey): Integer;
    procedure InsertItem(const AKey: TSortedListKey; AItem: Pointer);
    procedure Clear(AFree: Boolean);

    property Values[Index: Integer]: Pointer read GetValue; default;
    property Count: Integer read FCount;
  end;


var
  Config: TDenomoConfig = nil;

const
  IniFileName = 'denomo.ini';
  HostClassName = 'DenomoHost';
  InspectorClassName = 'DenomoInspector';

  Denomo_Msg = WM_USER + 199;
  //SM -> Sub Message
  SM_Connect = 1;
  SM_Disconnect = 2;
  SM_MemCount = 3;
  SM_SessionLeakBegin = 4;
  SM_SessionLeakEnd = 5;
  SM_IncrementalSessionLeakBegin = 6;
  SM_IncrementalSessionLeakEnd = 7;
  SM_OutputStrings = 8;
  SM_ListMem = 9;
  SM_GetHookOption = 10;
  SM_SetHookOption = 11;
  SM_GetEnabledMemTypes = 12;

  //HOF -> Hook Option Flag
  HOF_OutputStackTrace = 1 shl 0;
  HOF_OutputSourceInfo = 1 shl 1;
  HOF_CheckLeakOnExit = 1 shl 2;
  HOF_PromptListTooManyBlock = 1 shl 3;
  HOF_OutputStringSummary = 1 shl 4;
  HOF_EliminateObjectFieldsLeak = 1 shl 5;
  HOF_EliminateLeaksOnSameCallingPath = 1 shl 6;

  HookOptionFlagCount = 7;

  HookOptionFlagCaptions: array[0..HookOptionFlagCount - 1] of string = (
    'Output stack trace',
    'Output source info',
    'Check leak on exit',
    'Prompt before listing too many blocks',
    'Output string summary',
    'Eliminate leaks allocated in object''s constructor.',
    'Eliminate leaks allocated in same calling path (same stack trace).'
  );

  MemTypeCaptions: array[0..MEMTYPE_COUNT - 1] of string = (
    'Memory blocks',
    'Objects',
    'String',
    'Dynamic array',
    'GDI objects'
  );

procedure PackHookOption(AOption: PHookOption);
procedure UnpackHookOption(AOption: PHookOption);

procedure ErrorOrExit(const AMsg, ATitle: string; AExit: Boolean = False);

function SimpleGetCurrentDirectory: string;

function IsDelphiRunning: Boolean;
function IsBeingDebugged: Boolean;

implementation

uses
  DenomoHost;

const
  ShareMemoryName = '_DenomoShareMem_';
  HookOptionIniSectionName = 'General';

const
  DefaultHookOptionValues: array[0 .. HookOptionFlagCount - 1] of PBoolean = (
    @DefaultOutputStackTrace,
    @DefaultOutputSourceInfo,
    @DefaultCheckLeakOnExit,
    @DefaultPromptListTooManyBlock,
    @DefaultOutputStringSummary,
    @DefaultEliminateObjectFieldsLeak,
    @DefaultEliminateLeaksOnSameCallingPath
  );

procedure PackHookOption(AOption: PHookOption);
var
  I: Integer;
  lFlags: Cardinal;
begin
  lFlags := 0;
  for I := 0 to HookOptionFlagCount - 1 do
  begin
    if DefaultHookOptionValues[I]^ then
      lFlags := lFlags or (1 shl I);
  end;

  AOption.Flags := lFlags;
end;

procedure UnpackHookOption(AOption: PHookOption);
var
  I: Integer;
  lFlags: Cardinal;
begin
  lFlags := AOption.Flags;
  for I := 0 to HookOptionFlagCount - 1 do
  begin
    DefaultHookOptionValues[I]^ := (lFlags and (1 shl I)) <> 0;;
  end;
end;

function IsSameHookOption(AOption1, AOption2: PHookOption): Boolean;
begin
  Result := AOption1^.Flags = AOption2^.Flags;
end;

procedure ErrorOrExit(const AMsg, ATitle: string; AExit: Boolean);
const
  MB_SERVICE_NOTIFICATION = $00200000;
  MB_SERVICE_NOTIFICATION_NT3X = $00040000;
begin
  // specify MB_SERVICE_NOTIFICATION so that the message box can be displayed
  // even when the dll is unloading.
  MessageBox(0, PChar(AMsg), PChar('Denomo: ' + ATitle), MB_OK or MB_ICONWARNING
    or MB_SERVICE_NOTIFICATION or MB_SERVICE_NOTIFICATION_NT3X);
  if AExit then
    ExitProcess(1);
end;

function SimpleGetCurrentDirectory: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetCurrentDirectory(Length(Result), @Result[1]));

  if Length(Result) = 0 then
    Result := HomePath;
end;

function IsDelphiRunning: Boolean;
begin
  Result := FindWindow('TAppBuilder', nil) <> 0;
end;

function IsBeingDebugged: Boolean;
type
  TIsDebuggerPresent = function: LongBool; stdcall;
var
  lIsDebuggerPresent: TIsDebuggerPresent;
begin
  lIsDebuggerPresent := GetProcAddress(GetModuleHandle('kernel32'), 'IsDebuggerPresent');
  if @lIsDebuggerPresent <> nil then
    Result := lIsDebuggerPresent
  else
    Result := False;
end;

{ TStringOutput }

procedure TStringOutput.DoBeginOutput;
begin

end;

procedure TStringOutput.DoEndOutput;
begin

end;

procedure TStringOutput.BeginOutput;
begin
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  Inc(FUpdateCount);
  if FUpdateCount = 1 then
    DoBeginOutput;
end;

procedure TStringOutput.EndOutput;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      DoEndOutput;
      Flush;
    end;
  end;
end;

procedure TStringOutput.Flush;
begin

end;

procedure TStringOutput.Output(const S: string);
begin
  DoOutput(S);

  if FUpdateCount = 0 then
    Flush;
end;

{ TStringOutputDebugString }

procedure TStringOutputDebugString.DoOutput(const S: string);
begin
  OutputDebugString(PChar(S));
end;

{ TStringOutputToSpy }

constructor TStringOutputToInspector.Create(AInterObject: TInterObject);
begin
  inherited Create;

  FInterObject := AInterObject;
end;

procedure TStringOutputToInspector.DoOutput(const S: string);
begin
  if not InterObject.StringBufferWriter.Write(S) then
  begin
    Flush;
    if not InterObject.StringBufferWriter.Write(S) then
      ErrorOrExit('String is too long to put into string writer.', '');
  end;
end;

procedure TStringOutputToInspector.Flush;
begin
  InterObject.TalkToGuest(SM_OutputStrings, 0);
  InterObject.StringBufferWriter.Reset;
end;

{ TStringOutputToFile }

constructor TStringOutputToFile.Create(const AFileName: string);
begin
  inherited Create;

  FFileHandle := INVALID_HANDLE_VALUE;
  FFileName := AFileName;
end;

destructor TStringOutputToFile.Destroy;
begin
  if FFileHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FFileHandle);

  inherited;
end;

procedure TStringOutputToFile.DoOutput(const S: string);
const
  NewLine = #13#10;
var
  C: Cardinal;
  sansi: ansistring;
begin
  RequireHandle;
  if FFileHandle = INVALID_HANDLE_VALUE then
    Exit;

  sansi := AnsiString(S);
//  if S <> '' then
//    WriteFile(FileHandle, S[1], Length(S) * StringElementSize(s), C, nil);
//  WriteFile(FileHandle, NewLine[1], Length(NewLine) * StringElementSize(s), C, nil);
  if sansi <> '' then
    WriteFile(FileHandle, sansi[1], Length(sansi), C, nil);

  sansi := NewLine;
  WriteFile(FileHandle, sansi[1], Length(sansi), C, nil);
end;

procedure TStringOutputToFile.Flush;
begin
end;

procedure TStringOutputToFile.RequireHandle;
var
  lSizeHigh: Cardinal;
begin
  if HandleRequired then
    Exit;

  FHandleRequired := True;
  FFileHandle := CreateFile(PChar(FFileName), GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_ARCHIVE, 0);

  if FFileHandle = INVALID_HANDLE_VALUE then
    ErrorOrExit('Can''t open log file ' + FileName + ' to write', 'Error');

  if GetFileSize(FileHandle, @lSizeHigh) >= Cardinal(LogFileMaxSize) * 1024 then
  begin
    SetFilePointer(FileHandle, 0, nil, FILE_BEGIN);
    SetEndOfFile(FileHandle);
  end;

  SetFilePointer(FileHandle, 0, nil, FILE_END);

  Output(#13#10#13#10);
end;

{ TStringOutputAnsiString }

procedure TStringOutputToMessageBox.DoOutput(const S: string);
var
  lLen: Integer;
begin
  lLen := Length(S);
  if lLen + BufferLength + 2 > Capacity then
  begin
    FCapacity := lLen + BufferLength + 30;
    SetLength(FBuffer, Capacity);
  end;
  if BufferLength > 0 then
  begin
    FBuffer[BufferLength] := #13;
    Inc(FBufferLength);
    FBuffer[BufferLength] := #10;
    Inc(FBufferLength);
  end;
  if lLen > 0 then
    Move(S[1], FBuffer[BufferLength], lLen);
  Inc(FBufferLength, lLen);
end;

procedure TStringOutputToMessageBox.Flush;
var
  S: string;
begin
  SetLength(S, BufferLength);
  Move(FBuffer[0], S[1], BufferLength);
  ErrorOrExit(S, 'Error');
  FBufferLength := 0;
end;

{ TInterObject }

constructor TInterObject.Create;
begin
  inherited;

end;

destructor TInterObject.Destroy;
var
  P: Pointer;
begin
  P := Pointer(GetWindowLong(FMyWinHandle, GWL_WNDPROC));
  DestroyWindow(FMyWinHandle);
  FMyWinHandle := 0;
  FreeMem(P);

  inherited;
end;

function WndProcPumper(hwnd: HWND; nMsg, WParam: Longint;
  LParam: Longint): Longint; stdcall;
asm
  push 0
  push LParam
  push WParam
  push nMsg
  mov edx, esp
  call ecx
  add esp, 12
  pop eax
end;

function SimpleMakeObjectInstance(ASelf, AProc: Pointer): Pointer;
var
  P: PByte;
begin
  GetMem(Result, 32);
  P := PByte(Result);

  P^ := $b8; //mov eax, ASelf
  Inc(P);
  PPointer(P)^ := ASelf;
  Inc(P, 4);

  P^ := $b9; //mov ecx, AProc
  Inc(P);
  PPointer(P)^ := AProc;
  Inc(P, 4);

  P^ := $e9; //jmp WndProcPumper
  Inc(P);
  PInteger(P)^ := Integer(@WndProcPumper) - (Integer(P) + 4);
end;

procedure TInterObject.CreateCommunicateWindow;
var
  lWinClass: TWndClass;
begin
  FillChar(lWinClass, SizeOf(TWndClass), 0);
  lWinClass.lpfnWndProc := SimpleMakeObjectInstance(Self, @TInterObject.WndProc);
  lWinClass.lpszClassName := PChar(GetMyWinClassName);
  lWinClass.hInstance := HInstance;
  Windows.RegisterClass(lWinClass);

  FMyWinHandle := CreateWindow(lWinClass.lpszClassName, '',
    WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
end;

procedure TInterObject.InitShareMemory;
const
  ShareSize = SizeOf(TSharedRecord);
var
  lInit: Boolean;
begin
  lInit := False;

  if FShareMemoryHandle = 0 then
  begin
    FShareMemoryHandle := OpenFileMapping(FILE_MAP_READ or FILE_MAP_WRITE, False, ShareMemoryName);
    if FShareMemoryHandle = 0 then
    begin
      FShareMemoryHandle := CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0,
        ShareSize, ShareMemoryName);
      lInit := True;
    end;
  end;

  FShareMemoryPointer := MapViewOfFile(FShareMemoryHandle,
    FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, ShareSize);
  if FShareMemoryPointer = nil then
    ErrorOrExit('Can not allocate share memory.', '', True);

  if lInit then
    FillChar(FShareMemoryPointer^, ShareSize, 0);
end;

procedure TInterObject.DeInitShareMemory;
begin
  if FShareMemoryPointer <> nil then
  begin
    UnmapViewOfFile(FShareMemoryPointer);
    FShareMemoryPointer := nil;
  end;
  if FShareMemoryHandle <> 0 then
  begin
    CloseHandle(FShareMemoryHandle);
    FShareMemoryHandle := 0;
  end;
end;

function TInterObject.TalkToGuest(Msg: Integer; Param: Cardinal): Cardinal;
begin
  Result := 0;
  if GuestWinHandle = 0 then
    Exit;
    
  Result := SendMessage(GuestWinHandle, Denomo_Msg, Msg, Param);
end;

function TInterObject.TryConnect: Boolean;
begin
  Result := False;
  FGuestWinHandle := FindWindow(PChar(GetGuestWinClassName), nil);

  if FGuestWinHandle <> 0 then
  begin
    TalkToGuest(SM_Connect, MyWinHandle);
    DoOnConnected;
    Result := True;
  end;
end;

procedure TInterObject.WndProc(var Message: TMessage);
begin
	Message.Result := 0;
	case Message.Msg of
  	WM_CREATE:
      Message.Result := 0;

  	WM_NCCREATE:
      Message.Result := 1;

    WM_DESTROY:
      Message.Result := DoLeakMsg(SM_Disconnect, 0);

    Denomo_Msg:
      Message.Result := DoLeakMsg(Message.wParam, Message.lParam);
  end;
end;

function TInterObject.DoLeakMsg(Msg: Integer; Param: Cardinal): Integer;
begin
  Result := 0;
  case Msg of
    SM_Connect:
    begin
      FGuestWinHandle := Param;

      DoOnConnected;
    end;
    SM_Disconnect:
    begin
      FGuestWinHandle := 0;

      DoOnDisconnected;
    end;
    else
      Result := DoMoreLeakMsg(Msg, Param);
  end;
end;

function TInterObject.DoMoreLeakMsg(Msg: Integer;
  Param: Cardinal): Integer;
begin
  Result := 0;
end;

procedure TInterObject.Init(AConnect: Boolean);
var
  H: HWND;
  lPID: Cardinal;
  lMsg: string;
begin
  H := FindWindow(PChar(GetMyWinClassName), nil);
  if H <> 0 then
  begin
    GetWindowThreadProcessId(H, lPID);
    lMsg := 'Denomo should only be run at one single instance.'#13#10
      + 'But now another same instance has been detected. PID: ' + SimpleIntToStr(lPID) + #13#10
      + 'This will mess up the communication between host and inspector.'#13#10
      + #13#10
      + 'Do you want to continue running?';
    if MessageBox(0, PChar(lMsg), 'Denomo Error', MB_ICONERROR or MB_YESNO) <> IDYES then
      ExitProcess(1);
  end;

  InitShareMemory;

  FStringBufferReader := TSimpleStringBufferReader.Create(
    @ShareMemoryPointer^.ChunkData, SharedRecordDataSize);
  FStringBufferWriter := TSimpleStringBufferWriter.Create(
    @ShareMemoryPointer^.ChunkData, SharedRecordDataSize);

  CreateCommunicateWindow;

  //must before TryConnect
  DoInit;

  if AConnect then
    TryConnect;
end;

procedure TInterObject.DeInit;
begin
  DoDeInit;

  DeInitShareMemory;
  
  TalkToGuest(SM_Disconnect, 0);

  SimpleFreeAndNil(FStringBufferReader);
  SimpleFreeAndNil(FStringBufferWriter);
end;

function TInterObject.ResetShareMemory: PSharedRecord;
begin
  Result := ShareMemoryPointer;
  FillChar(Result^, Integer(@Result^.ChunkData) - Integer(Result), 0);
end;

function TInterObject.GetShareMemory: PSharedRecord;
begin
  Result := ShareMemoryPointer;
end;

function TInterObject.GetConnected: Boolean;
begin
  Result := GuestWinHandle <> 0;
  if Result then
  begin
    if not IsWindow(GuestWinHandle) then
    begin
      FGuestWinHandle := 0;

      DoOnDisconnected;

      Result := False;
    end;
  end;
end;

procedure TInterObject.DoOnConnected;
begin
  if Assigned(OnConnected) then
    OnConnected(Self);
end;

procedure TInterObject.DoOnDisconnected;
begin
  if Assigned(OnDisconnected) then
    OnDisconnected(Self);
end;

procedure TInterObject.DoInit;
begin

end;

procedure TInterObject.DoDeInit;
begin

end;

function TInterObject.GuestAvailable: Boolean;
begin
  Result := False;
  if FGuestWinHandle <> 0 then
  begin
    Result := IsWindow(FGuestWinHandle);
    if not Result then
      FGuestWinHandle := 0;
  end;
end;

{ TDenomoConfigTypeInfo }

procedure TDenomoConfigTypeInfo.LoadFromIni(AIni: TSimpleIniFile);
begin
  case VarType of
    varInteger:
      PInteger(Addr)^ := AIni.ReadInteger(Name, PInteger(Addr)^);
    varBoolean:
      PBoolean(Addr)^ := AIni.ReadBoolean(Name, PBoolean(Addr)^);
    varString:
      PString(Addr)^ := AIni.ReadString(Name, PString(Addr)^);
    else
      ErrorUnknownType;
  end;
end;

procedure TDenomoConfigTypeInfo.SaveToIni(AIni: TSimpleIniFile);
begin
  case VarType of
    varInteger:
      AIni.WriteInteger(Name, PInteger(Addr)^);
    varBoolean:
      AIni.WriteBoolean(Name, PBoolean(Addr)^);
    varString:
      AIni.WriteString(Name, PString(Addr)^);
    else
      ErrorUnknownType;
  end;
end;

function TDenomoConfigTypeInfo.Pack(ABuffer: Pointer): Pointer;
var
  lSkip: Cardinal;
begin
  lSkip := 0;
  case VarType of
    varInteger:
    begin
      PInteger(ABuffer)^ := PInteger(Addr)^;
      lSkip := SizeOf(Integer);
    end;
    varBoolean:
    begin
      PBoolean(ABuffer)^ := PBoolean(Addr)^;
      lSkip := SizeOf(Boolean);
    end;
    varString:
    begin
      PWord(ABuffer)^ := Length(PString(Addr)^);
      if Length(PString(Addr)^) > 0 then
        Move(PString(Addr)^[1], PChar(Cardinal(ABuffer) + SizeOf(Word))^,
          Length(PString(Addr)^));
      lSkip := SizeOf(Word) + Length(PString(Addr)^);
    end;
    else
      ErrorUnknownType;
  end;
  Result := Pointer(Cardinal(ABuffer) + lSkip);
end;

function TDenomoConfigTypeInfo.Unpack(ABuffer: Pointer): Pointer;
var
  lSkip: Cardinal;
begin
  lSkip := 0;
  case VarType of
    varInteger:
    begin
      PInteger(Addr)^ := PInteger(ABuffer)^;
      lSkip := SizeOf(Integer);
    end;
    varBoolean:
    begin
      PBoolean(Addr)^ := PBoolean(ABuffer)^;
      lSkip := SizeOf(Boolean);
    end;
    varString:
    begin
      SetLength(PString(Addr)^, PWord(ABuffer)^);
      if Length(PString(Addr)^) > 0 then
        Move(PChar(Cardinal(ABuffer) + SizeOf(Word))^, PString(Addr)^[1],
          Length(PString(Addr)^));
      lSkip := SizeOf(Word) + Length(PString(Addr)^);
    end;
    else
      ErrorUnknownType;
  end;
  Result := Pointer(Cardinal(ABuffer) + lSkip);
end;

procedure TDenomoConfigTypeInfo.ErrorUnknownType;
begin
  Assert(False);
end;

{ TDenomoConfig }

constructor TDenomoConfig.Create;
begin
  inherited;

end;

destructor TDenomoConfig.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FTypeInfos) do
    FTypeInfos[I].Free;

  inherited;
end;

{#@CODE auto generated code start, don't touch }
procedure TDenomoConfig.InitTypeInfos;
var
  lTypeInfo: TDenomoConfigTypeInfo;
  lIndex: Integer;
  I: Integer;
begin
  SetLength(FTypeInfos, 28);
  for I := 0 to 28 - 1 do
    FTypeInfos[I] := nil;

  lIndex := 0;
  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'RuntimeDllDirectory';
  lTypeInfo.VarType := varString;
  lTypeInfo.Addr := @RuntimeDllDirectory;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'RuntimeDllName';
  lTypeInfo.VarType := varString;
  lTypeInfo.Addr := @RuntimeDllName;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'KeepTrackOnFreedBlock';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @KeepTrackOnFreedBlock;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'KeepDetailedTrackOnFreedBlock';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @KeepDetailedTrackOnFreedBlock;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'PromptNoTD32DebugInfo';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @PromptNoTD32DebugInfo;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'MaxStringSummaryLen';
  lTypeInfo.VarType := varInteger;
  lTypeInfo.Addr := @MaxStringSummaryLen;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'EnableMemTypeString';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @EnableMemTypeString;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'EnableMemTypeDynArray';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @EnableMemTypeDynArray;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'MaxSourceInfoCacheTreeNodeCount';
  lTypeInfo.VarType := varInteger;
  lTypeInfo.Addr := @MaxSourceInfoCacheTreeNodeCount;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'SkipFramesOfCallingPath';
  lTypeInfo.VarType := varInteger;
  lTypeInfo.Addr := @SkipFramesOfCallingPath;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'ReportToLogFileWhenCheckOnExit';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @ReportToLogFileWhenCheckOnExit;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'LogFileMaxSize';
  lTypeInfo.VarType := varInteger;
  lTypeInfo.Addr := @LogFileMaxSize;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'WarnAnyWay';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @WarnAnyWay;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'WarnIfDelphiNotRunning';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @WarnIfDelphiNotRunning;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'WarnIfNotBeingDebugged';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @WarnIfNotBeingDebugged;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'WarnIfComputerNameNotEqual';
  lTypeInfo.VarType := varString;
  lTypeInfo.Addr := @WarnIfComputerNameNotEqual;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'DefaultOutputStackTrace';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @DefaultOutputStackTrace;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'DefaultOutputSourceInfo';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @DefaultOutputSourceInfo;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'DefaultCheckLeakOnExit';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @DefaultCheckLeakOnExit;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'DefaultPromptListTooManyBlock';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @DefaultPromptListTooManyBlock;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'DefaultOutputStringSummary';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @DefaultOutputStringSummary;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'DefaultEliminateObjectFieldsLeak';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @DefaultEliminateObjectFieldsLeak;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'DefaultEliminateLeaksOnSameCallingPath';
  lTypeInfo.VarType := varBoolean;
  lTypeInfo.Addr := @DefaultEliminateLeaksOnSameCallingPath;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'StackTraceDepth';
  lTypeInfo.VarType := varInteger;
  lTypeInfo.Addr := @StackTraceDepth;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'FreedStackTraceDepth';
  lTypeInfo.VarType := varInteger;
  lTypeInfo.Addr := @FreedStackTraceDepth;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'HandleStackTraceDepth';
  lTypeInfo.VarType := varInteger;
  lTypeInfo.Addr := @HandleStackTraceDepth;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'MaxGroupID';
  lTypeInfo.VarType := varInteger;
  lTypeInfo.Addr := @MaxGroupID;
  Inc(lIndex);

  lTypeInfo := TDenomoConfigTypeInfo.Create;
  FTypeInfos[lIndex] := lTypeInfo;
  lTypeInfo.Name := 'DetectModuleName';
  lTypeInfo.VarType := varString;
  lTypeInfo.Addr := @DetectModuleName;

end;
{#@CODE auto generated code end }

procedure TDenomoConfig.Init;
begin
  InitTypeInfos;
  SolveFileName;
  LoadFromFile;

  Pack(@FPreviousConfig[0]);
end;

procedure TDenomoConfig.DeInit;
begin
  if IsDirty then
    SaveToFile;
end;

procedure TDenomoConfig.LoadFromFile;
var
  lIni: TSimpleIniFile;
begin
  if FileName = '' then
    Exit;
  if not SimpleFileExists(FileName) then
    Exit;

  lIni := TSimpleIniFile.Create(FileName);
  try
    LoadFromIni(lIni);
  finally
    lIni.Free;
  end;
end;

procedure TDenomoConfig.SaveToFile;
var
  lIni: TSimpleIniFile;
begin
  if FileName = '' then
    Exit;

  lIni := TSimpleIniFile.Create(FileName);
  try
    SaveToIni(lIni);
  finally
    lIni.Free;
  end;
end;

const
  CSIDL_APPDATA = $001A;
  CSIDL_COMMON_APPDATA = $0023;
  CSIDL_LOCAL_APPDATA = $001C;
  CSIDL_WINDOWS = $0024;

procedure TDenomoConfig.SolveFileName;
const
  ExtraPath = 'Denomo';

  function CheckPath(const APath: string; const AExtra: string = ''): Boolean;
  var
    lFN: string;
  begin
    lFN := APath;
    if (Length(lFN) > 0) and (lFN[Length(lFN)] <> '\') then
      lFN := lFN + '\';
    if AExtra <> '' then
      lFN := lFN + AExtra + '\';
    lFN := lFN + IniFileName;

    Result := SimpleFileExists(lFN);
    if Result then
      FFileName := lFN;
  end;
begin
  if CheckPath(HomePath) then
    Exit;

  if CheckPath(SimpleGetCurrentDirectory) then
    Exit;

  if (Length(HomePath) > 1) and (HomePath[2] = ':') then
  begin
    if CheckPath(HomePath[1] + ':\') then
      Exit;
  end;

  if CheckPath(GetShellFolder(CSIDL_LOCAL_APPDATA), ExtraPath) then
    Exit;

  if CheckPath(GetShellFolder(CSIDL_APPDATA), ExtraPath) then
    Exit;

  if CheckPath(GetShellFolder(CSIDL_COMMON_APPDATA), ExtraPath) then
    Exit;

  if CheckPath(GetShellFolder(CSIDL_WINDOWS)) then
    Exit;
end;

procedure TDenomoConfig.Pack(APackedConfig: PDenomoPackedConfig);
var
  I: Integer;
  P: Pointer;
begin
  SafeFillChar(APackedConfig^[0], Length(APackedConfig^), 0);
  P := APackedConfig;
  for I := 0 to High(FTypeInfos) do
    P := FTypeInfos[I].Pack(P);
end;

procedure TDenomoConfig.Unpack(APackedConfig: PDenomoPackedConfig);
var
  I: Integer;
  P: Pointer;
begin
  P := APackedConfig;
  for I := 0 to High(FTypeInfos) do
    P := FTypeInfos[I].Unpack(P);
end;

function TDenomoConfig.IsDirty: Boolean;
var
  lConfig: TDenomoPackedConfig;
  H: THandle;
  lSizeLow, lSizeHigh: Cardinal;
begin
  Pack(@lConfig[0]);

  Result := not SimpleCompareMem(@lConfig[0], @FPreviousConfig[0], SizeOf(TDenomoPackedConfig));

  if not Result then
  begin
    H := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ,
      nil, OPEN_EXISTING, FILE_ATTRIBUTE_ARCHIVE, 0);
    if H = INVALID_HANDLE_VALUE then
      Result := True
    else
    begin
      lSizeHigh := 0;
      lSizeLow := GetFileSize(H, @lSizeHigh);
      Result := (lSizeLow = 0) and (lSizeHigh = 0);
      CloseHandle(H);
    end;
  end;
end;

procedure TDenomoConfig.LoadFromIni(AIni: TSimpleIniFile);
var
  I: Integer;
begin
  for I := 0 to High(FTypeInfos) do
    FTypeInfos[I].LoadFromIni(AIni);
end;

procedure TDenomoConfig.SaveToIni(AIni: TSimpleIniFile);
var
  I: Integer;
begin
  for I := 0 to High(FTypeInfos) do
    FTypeInfos[I].SaveToIni(AIni);
end;

{ TSortedList }

constructor TSortedList.Create;
begin

end;

destructor TSortedList.Destroy;
begin
  Clear(False);

  inherited;
end;

function TSortedList.DoFindItem(const AKey: TSortedListKey;
  var ANearIndex: Integer): Integer;
var
  M, L, H: Integer;
  lCompare: Integer;
begin
  Result := -1;
  M := 0;
  L := 0;
  H := FCount - 1;
  lCompare := 0;
  
  while L <= H do
  begin
    M := (L + H) shr 1;
    
    lCompare := CompareItem(AKey, FItems[M].Key);
    if lCompare = 0 then
    begin
      Result := M;
      Break;
    end;
    
    if lCompare > 0 then
      L := M + 1
    else
      H := M - 1;
  end;
  ANearIndex := M;
  if lCompare > 0 then
    Inc(ANearIndex);
end;

function TSortedList.CompareItem(const AKey1, AKey2: TSortedListKey): Integer;
begin
  if AKey1 > AKey2 then
    Result := 1
  else
    if AKey1 < AKey2 then
      Result := -1
    else
      Result := 0;
end;

procedure TSortedList.Grow;
begin
  Inc(FCapacity, 10);
  SetLength(FItems, FCapacity);
end;

function TSortedList.FindItem(const AKey: TSortedListKey): Integer;
var
  lNearIndex: Integer;
begin
  Result := DoFindItem(AKey, lNearIndex);
end;

procedure TSortedList.InsertItem(const AKey: TSortedListKey; AItem: Pointer);
var
  lFoundIndex, lNearIndex: Integer;
begin
  if Count = Capacity then
    Grow;

  lFoundIndex := DoFindItem(AKey, lNearIndex);
  if lFoundIndex >= 0 then
    Exit;
  
  if Count > lNearIndex then
    Move(FItems[lNearIndex], FItems[lNearIndex + 1], (Count - lNearIndex) * SizeOf(TSortedListItem));
  FItems[lNearIndex].Key := AKey;
  FItems[lNearIndex].Value := AItem;
  Inc(FCount);
end;

procedure TSortedList.Clear(AFree: Boolean);
var
  I: Integer;
begin
  if AFree then
  begin
    for I := Count - 1 downto 0 do
      FreeMem(FItems[I].Value);
  end;
  
  FCount := 0;
end;

function TSortedList.GetValue(Index: Integer): Pointer;
begin
  Result := FItems[Index].Value;
end;

end.

