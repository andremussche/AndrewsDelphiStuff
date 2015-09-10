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

The Original Code is DenomoHost.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit DenomoHost;

interface

{$include Denomo.inc}

uses
  Windows, Messages,
  CodeHookIntf, MiscUtils,
  DenomoMemHook, DenomoHandleHook, DenomoUtils;

type
  TInterObjectHost = class(TInterObject)
  private
    FStringOutputList: array[0 .. SOC_Count - 1] of TStringOutput;
    FCurrentOutput: Integer;
    FCurrentOutputStack: array[0..9] of Integer;
    FCurrentOutputTop: Integer;

    procedure SessionLeakBegin(AGroupID: Integer);
    procedure SessionLeakEnd(AGroupID: Integer; AReset: Boolean);
    procedure SetCurrentOutput(const Value: Integer);
    function GetStringOutput: TStringOutput;
  protected
    function GetMyWinClassName: string; override;
    function GetGuestWinClassName: string; override;
    function DoMoreLeakMsg(Msg: Integer; Param: Cardinal): Integer; override;

    procedure BeginLeakEliminate(AInfo: PLeakEliminateInfo;
      AData: PEnumHookIndexParamedData);
    procedure EndLeakEliminate(AInfo: PLeakEliminateInfo;
      AData: PEnumHookIndexParamedData; ASilentIfNoFound: Boolean);

    procedure CheckLeakOnExit;

    property StringOutput: TStringOutput read GetStringOutput;
  public
    constructor Create;
    destructor Destroy; override;

    procedure PushCurrentOutput;
    procedure PopCurrentOutput;

    property CurrentOutput: Integer read FCurrentOutput write SetCurrentOutput;
  end;

procedure InitRuntimeDll;
procedure DeInitRuntimeDll;

procedure InitConfig;
procedure DeInitConfig;

procedure InitHost;
procedure DeInitHost;
procedure ConnectToInspector;

procedure BlockFreedTwice(P: Pointer; AFreedThumb: PMemFreedThumb);
procedure ReallocOnFreedBlock(P: Pointer; AFreedThumb: PMemFreedThumb);
procedure CallOnFreedObject(AObj: TObject; AClass: TClass; AFreedThumb: PMemFreedThumb);
procedure AccessFreedInterface(P: Pointer; AIntf: Pointer; AMethod: Cardinal; AFreedThumb: PMemFreedThumb);

procedure CheckLeakOnExit;

var
  DllService: IDllService = nil;
  HomePath: string;
  HostName: string;
  InterModuleDebug: Boolean = False;
  InterModuleHandle: Cardinal = 0;
  DestModuleAddrLow: Cardinal = 0;
  DestModuleAddrHigh: Cardinal = 0;

const
  MsgReallocateNotInPlaceWarning = 'You are using a memory manager that may not reallocate smaller size in place'#13#10
    + 'For example, Delphi 2007, 2006 and FastMM4 use more complex algorithm and often doesn''t reallocate smaller size in place.'#13#10
    + #13#10
    + 'This may affect the accuracy of detection of bad pointer access.'#13#10
    + 'Now Denomo will keep the memory blocks instead of reallcate them.'#13#10
    + #13#10
    + 'If you don''t want to see this message again, Set PromptReallocationNotInPlace in DenomoConfig to False.';
  TitleReallocateNotInPlaceWarning = 'Denomo: Can''t reallocate in place';

  MsgHookFailError = 'Hook procedure failed.'#13#10
    + 'Please send a bug report to the original author with the following information:'#13#10
    + '1, The Delphi version you use.'#13#10
    + '2, The Denomo version you use.'#13#10
    + 'The failed procedure name:'#13#10
    ;
  TitleHookFailError = 'Denomo: Hook procedure failed';

  MsgDllNotFound = 'Can''t load runtime dll.'#13#10
    + 'Please check the constant "RuntimeDllDirectory" in file DenomoConfig.pas.'#13#10
    + 'That constant must be the path contains the dll.'#13#10
    + 'The dll is in the path {Denomo path}\bin.'#13#10
    + ''#13#10
    + 'Now the program will exit.'#13#10
    + 'You will not lose any data because this program hasn''t run yet.'#13#10
    + #13#10;
  TitleDllNotFound = 'Fatal error - can''t load DLL';

  MsgProcNotFound = 'Function not found in DLL'#13#10
    + 'Please download the newest version.'#13#10
    + ''#13#10
    + 'Now the program will exit.'#13#10
    + 'You will not lose any data because this program hasn''t run yet.'#13#10
    + #13#10;
  TitleProcNotFound = 'Fatal error - function not found';

  MsgDebugInfoNotFound = 'Debug information is not available.'#13#10
    + 'This will cause no source code information output.'#13#10
    + 'To take the great convenience of source information,'#13#10
    + 'Please enable debug information (TD32, map file or JCL debug info).'#13#10
    + #13#10
    + 'To enable Map file,'#13#10
    + 'In Delphi IDE: go to menu Project->Options,'#13#10
    + '    in the "liner" tab, in "Map file" section, check the box "Detailed"'#13#10
    + #13#10
    + 'To enable TD32 debug information,'#13#10
    + 'In Delphi IDE: go to menu Project->Options,'#13#10
    + '    in the "liner" tab, check the box "Include TD32 debug info"'#13#10
    ;
  TitleDebugInfoNotFound = 'Debug information is highly recommended';

  MsgWarningToEndUser = 'Denomo is only for debug purpose.'#13#10
    + 'You may disable this warning by disable the configs'#13#10
    + 'which names are start with "WarnIf" in DenomoConfig.pas or Denomo.ini'#13#10
    + ''#13#10
    + 'Due to following reasons, this warning is showed.'#13#10
    + 'Reason:'#13#10
  ;
  TitleWarningToEndUser = 'Warning';

  MsgFinalLeakLogFile = 'There are some leaks.'#13#10
    + 'Please check the log file for details.'#13#10
    + 'The log file name is ';
  TitleFinalLeakLogFile = 'Final leak detected';

  MsgFinalLeak = 'There are some leaks on program exits.'#13#10
    + 'Please enable ReportToLogFileWhenCheckOnExit or run LeakInspector to view the detailed information.';
  TitleFinalLeak = 'Final leak detected';

  MsgConfigError = 'Following configs are error.'#13#10
    + 'Please check them'#13#10;
  TitleConfigError = 'Config error';

implementation

uses
  DenomoConfig, SysUtils;

const
  GroupID_Ignored = $80;
  LogFileExt = '.denomo.log';

var
  InterObject: TInterObjectHost;
  SourceInfoAvailable: Boolean = False;
  RuntimeDllHandle: Cardinal = 0;

type
  PSourceInfoCacheTreeNode = ^TSourceInfoCacheTreeNode;
  TSourceInfoCacheTreeNode = packed record
    Left: PSourceInfoCacheTreeNode;
    Right: PSourceInfoCacheTreeNode;
    Addr: Cardinal;
    UnitName: string;
    ProcedureName: string;
    LineNumber: Integer;
  end;

var
  SourceInfoCacheTreeNodeCount: Integer = 0;
  SourceInfoCacheTreeRootNode: TSourceInfoCacheTreeNode = (
    Left: nil; Right: nil;
    UnitName: ''; ProcedureName: ''
  );
  TempSourceInfoCacheTreeRootNode: TSourceInfoCacheTreeNode = (
    Left: nil; Right: nil;
    UnitName: ''; ProcedureName: ''
  );

function LoadSourceInfoToTreeNode(AAddr: Cardinal;
  ANode: PSourceInfoCacheTreeNode): Boolean;
var
  lLocationInfo: TDebugSourceLocationInfo;
begin
  Result := DllService.GetDebugSourceLocationInfo(Pointer(AAddr),
    @lLocationInfo);
  if Result then
  begin
    ANode^.Addr := AAddr;
    ANode^.LineNumber := lLocationInfo.LineNumber;
    SafeStrToPasStr(lLocationInfo.UnitName, ANode^.UnitName);
    SafeStrToPasStr(lLocationInfo.ProcedureName, ANode^.ProcedureName);
  end;
end;

function GetSourceInfoTreeNode(AAddr: Cardinal): PSourceInfoCacheTreeNode;
type
  TFoundResult = ( frNone, frInsertLeft, frInsertRight );
var
  lNode: PSourceInfoCacheTreeNode;
  lFoundResult: TFoundResult;
begin
  if SourceInfoCacheTreeNodeCount = 0 then
  begin
    if LoadSourceInfoToTreeNode(AAddr, @SourceInfoCacheTreeRootNode) then
    begin
      SourceInfoCacheTreeNodeCount := 1;
      Result := @SourceInfoCacheTreeRootNode;
    end
    else
      Result := nil;
    Exit;
  end;

  lFoundResult := frNone;
  lNode := @SourceInfoCacheTreeRootNode;
  while True do
  begin
    if lNode^.Addr = AAddr then
    begin
      Result := lNode;
      Exit;
    end;

    if AAddr < lNode^.Addr then
    begin
      if lNode^.Left <> nil then
      begin
        if lNode^.Left^.Addr < AAddr then
        begin
          lFoundResult := frInsertLeft;
          Break;
        end
        else
          lNode := lNode^.Left;
      end
      else
      begin
        lFoundResult := frInsertLeft;
        Break;
      end;
    end
    else
    begin
      if lNode^.Right <> nil then
      begin
        if lNode^.Right^.Addr > AAddr then
        begin
          lFoundResult := frInsertRight;
          Break;
        end
        else
          lNode := lNode^.Right;
      end
      else
      begin
        lFoundResult := frInsertRight;
        Break;
      end;
    end;
  end;

  Assert(lFoundResult <> frNone);
  if SourceInfoCacheTreeNodeCount < MaxSourceInfoCacheTreeNodeCount then
  begin
    New(Result);
    if LoadSourceInfoToTreeNode(AAddr, Result) then
    begin
      Inc(SourceInfoCacheTreeNodeCount);

      if lFoundResult = frInsertLeft then
      begin
        Result^.Right := nil;
        Result^.Left := lNode^.Left;
        lNode^.Left := Result;
      end
      else
      begin
        Result^.Left := nil;
        Result^.Right := lNode^.Right;
        lNode^.Right := Result;
      end;
    end
    else
    begin
      Dispose(Result);
      Result := nil;
    end;
  end
  else
  begin
    if LoadSourceInfoToTreeNode(AAddr, @TempSourceInfoCacheTreeRootNode) then
    begin
      Result := @TempSourceInfoCacheTreeRootNode;
    end
    else
      Result := nil;
  end;
end;

procedure FreeSourceInfoCacheTreeNode(ANode: PSourceInfoCacheTreeNode);
begin
  if ANode^.Left <> nil then
    FreeSourceInfoCacheTreeNode(ANode^.Left);
  if ANode^.Right <> nil then
    FreeSourceInfoCacheTreeNode(ANode^.Right);
  ANode^.UnitName := '';
  ANode^.ProcedureName := '';
  Dispose(ANode);
end;

type
  PLeakInfoRemoverCacheTreeNode = ^TLeakInfoRemoverCacheTreeNode;
  TLeakInfoRemoverCacheTreeNode = packed record
    Left: PLeakInfoRemoverCacheTreeNode;
    Right: PLeakInfoRemoverCacheTreeNode;
    SameCount: Cardinal;
    StackTrace: array[0 .. 0] of Cardinal;
  end;

function GetLeakInfoRemoverTreeNode(AThumb: PMemThumb;
  var ARootNode: PLeakInfoRemoverCacheTreeNode;
  out AFoundNode: PLeakInfoRemoverCacheTreeNode): Boolean;
type
  TFoundResult = ( frNone, frInsertLeft, frInsertRight );
var
  lNode: PLeakInfoRemoverCacheTreeNode;
  lFoundResult: TFoundResult;
  lCompare: Integer;

  procedure FillInfo(ANode: PLeakInfoRemoverCacheTreeNode);
  begin
    Move(AThumb^.StackTrace[0], ANode^.StackTrace[0],
      StackTraceDepth * SizeOf(Cardinal));
  end;

  function CompareInfo(ANode: PLeakInfoRemoverCacheTreeNode): Integer;
  var
    I: Cardinal;
    lStart: Cardinal;
    lST1, lST2: Cardinal;
  begin
    if SkipFramesOfCallingPath > 0 then
    begin
      lStart := SkipFramesOfCallingPath;
      if PCardinal(Cardinal(@ANode^.StackTrace[0]) + Cardinal(StackTraceDepth - 1) * SizeOf(Cardinal))^ = 0 then
        lStart := 0;
    end
    else
      lStart := 0;

    Result := 0;
    for I := lStart to StackTraceDepth - 1 do
    begin
      lST1 := PCardinal(Cardinal(@ANode^.StackTrace[0]) + I * SizeOf(Cardinal))^;
      lST2 := PCardinal(Cardinal(@AThumb^.StackTrace[0]) + I * SizeOf(Cardinal))^;
      if lST1 <> lST2 then
      begin
        if lST1 < lST2 then
          Result := -1
        else
          Result := 1;
      end;
    end;
  end;
begin
  if ARootNode = nil then
  begin
    ARootNode := SimpleAllocMem(SizeOf(TLeakInfoRemoverCacheTreeNode) + (StackTraceDepth - 1) * SizeOf(Cardinal));
    FillInfo(ARootNode);
    AFoundNode := ARootNode;
    Result := False;
    Exit;
  end;

  lFoundResult := frNone;
  lNode := ARootNode;
  while True do
  begin
    lCompare := CompareInfo(lNode);

    if lCompare = 0 then
    begin
      AFoundNode := lNode;
      Result := True;
      Exit;
    end;

    if lCompare < 0 then
    begin
      if lNode^.Left <> nil then
      begin
        lNode := lNode^.Left;
      end
      else
      begin
        lFoundResult := frInsertLeft;
        Break;
      end;
    end
    else
    begin
      if lNode^.Right <> nil then
      begin
        lNode := lNode^.Right;
      end
      else
      begin
        lFoundResult := frInsertRight;
        Break;
      end;
    end;
  end;

  AFoundNode := SimpleAllocMem(SizeOf(TLeakInfoRemoverCacheTreeNode) + (StackTraceDepth - 1) * SizeOf(Cardinal));
  FillInfo(AFoundNode);
  if lFoundResult = frInsertLeft then
  begin
    AFoundNode^.Right := nil;
    AFoundNode^.Left := lNode^.Left;
    lNode^.Left := AFoundNode;
  end
  else
  begin
    AFoundNode^.Left := nil;
    AFoundNode^.Right := lNode^.Right;
    lNode^.Right := AFoundNode;
  end;
  Result := False;
end;

procedure FreeLeakInfoRemoverCacheTreeNode(ANode: PLeakInfoRemoverCacheTreeNode);
begin
  if ANode^.Left <> nil then
    FreeLeakInfoRemoverCacheTreeNode(ANode^.Left);
  if ANode^.Right <> nil then
    FreeLeakInfoRemoverCacheTreeNode(ANode^.Right);
  FreeMem(ANode);
end;

procedure InitRuntimeDll;
type
  TGetDllService = function: IDllService; stdcall;
var
  lDllName: string;
  lGetDllService: TGetDllService;

  function GetProcAddr(const AProcName: string): FARPROC;
  begin
    Result := GetProcAddress(RuntimeDllHandle, PChar(AProcName));
    if Result = nil then
    begin
      ErrorOrExit(MsgProcNotFound
        + 'DLL name: ' + lDllName
        + #13#10
        + 'Function name: ' + AProcName,
        TitleProcNotFound, True);
    end;
  end;
begin
  if InterModuleDebug then
    Exit;

  if DllService <> nil then
    Exit;

  lDllName := RuntimeDllDirectory;
  if (lDllName <> '') and (lDllName[Length(lDllName)] <> '\') then
    lDllName := lDllName + '\';
  lDllName := lDllName + RuntimeDllName;

  RuntimeDllHandle := LoadLibrary(PChar(lDllName));
  if RuntimeDllHandle = 0 then
  begin
    ErrorOrExit(MsgDllNotFound + 'DLL name: ' + lDllName,
      TitleDllNotFound, True);
  end;

  lGetDllService := GetProcAddr('GetDllService');
  DllService := lGetDllService;
end;

procedure DeInitRuntimeDll;
var
  lCodeHook: ICodeHook;
  lCodeHookHelper: ICodeHookHelper;
begin
  DllService.GetCodeHook(lCodeHook);
  lCodeHook.GetCodeHookHelper(lCodeHookHelper);
  lCodeHookHelper.UnhookAll;
  // release the interfaces explicitly or AV error after the DLL is unloaded.
  lCodeHookHelper := nil;
  lCodeHook := nil;

  DllService := nil;

  if RuntimeDllHandle <> 0 then
    FreeLibrary(RuntimeDllHandle);
end;

procedure VerfiyConfig;
var
  lMsg: string;
  lMsgPrefix: string;
begin
  lMsg := '';
  lMsgPrefix := 'Stack trace depth must be between 1 and ' + SimpleIntToStr(MaxStackTraceDepth) + ': ';

  if (StackTraceDepth < 1) or (StackTraceDepth >= MaxStackTraceDepth) then
    lMsg := lMsg + #13#10 + lMsgPrefix + 'StackTraceDepth';

  if (FreedStackTraceDepth < 1) or (FreedStackTraceDepth >= MaxStackTraceDepth) then
    lMsg := lMsg + #13#10 + lMsgPrefix + 'FreedStackTraceDepth';

  if (HandleStackTraceDepth < 1) or (HandleStackTraceDepth >= MaxStackTraceDepth) then
    lMsg := lMsg + #13#10 + lMsgPrefix + 'HandleStackTraceDepth';

  if not KeepTrackOnFreedBlock then
  begin
    lMsgPrefix := 'KeepTrackOnFreedBlock is False but this is True: ';
    if KeepDetailedTrackOnFreedBlock then
      lMsg := lMsg + #13#10 + lMsgPrefix + 'KeepDetailedTrackOnFreedBlock';
  end;

  if not KeepDetailedTrackOnFreedBlock then
  begin
    lMsgPrefix := 'KeepDetailedTrackOnFreedBlock is False but this is True: ';
    if DetectVirtualMethodAccssOnFreedObject then
      lMsg := lMsg + #13#10 + lMsgPrefix + 'DetectVirtualMethodAccssOnFreedObject';
    if DetectInterfaceMethodAccssOnFreedObject then
      lMsg := lMsg + #13#10 + lMsgPrefix + 'DetectInterfaceMethodAccssOnFreedObject';
  end;

  if lMsg <> '' then
    ErrorOrExit(MsgConfigError + lMsg, TitleConfigError, True);
end;

procedure InitConfig;
var
  I: Integer;
begin
  if Config <> nil then
    Exit;

  HomePath := ParamStr(0);
  HostName := '';
  for I := Length(HomePath) downto 1 do
  begin
    if HomePath[I] = '\' then
    begin
      HostName := Copy(HomePath, I + 1, Length(HomePath));
      HomePath := Copy(HomePath, 1, I);
      Break;
    end;
  end;

  Config := TDenomoConfig.Create;
  Config.Init;

  VerfiyConfig;
end;

procedure DeInitConfig;
begin
  if Config <> nil then
  begin
    Config.DeInit;
    SimpleFreeAndNil(Config);
  end;
end;

function NeedWarnToEndUser(var AMsg: string): Boolean;
var
  lNeed: Boolean;
  C: Cardinal;
  lComputerName: array[0 .. MAX_COMPUTERNAME_LENGTH] of Char;
begin
  Result := False;
  AMsg := '';

  if WarnAnyWay then
  begin
    lNeed := True;
    Result := Result or lNeed;
    if lNeed then
      AMsg := '    Whenever runs.'#13#10;
  end;

  if WarnIfDelphiNotRunning and (not InterModuleDebug) then
  begin
    lNeed := not IsDelphiRunning;
    Result := Result or lNeed;
    if lNeed then
      AMsg := '    Delphi is not running.'#13#10;
  end;

  if WarnIfNotBeingDebugged then
  begin
    lNeed := not IsBeingDebugged;
    Result := Result or lNeed;
    if lNeed then
      AMsg := AMsg + '    This program is not being debugged.'#13#10;
  end;

  if WarnIfComputerNameNotEqual <> '' then
  begin
    C := Length(lComputerName);
    if GetComputerName(@lComputerName[0], C) then
    begin
      lNeed := lstrcmpi(@lComputerName[0], PChar(WarnIfComputerNameNotEqual)) <> 0;
      Result := Result or lNeed;
      if lNeed then
        AMsg := AMsg + '    The computer name should be '
          + WarnIfComputerNameNotEqual + ' but now is '
          //+ StrPas(@lComputerName[0]) + '.'#13#10;
          + lComputerName + '.'#13#10;
    end;
  end;
end;

procedure InitHost;
var
  lLocationInfo: TDebugSourceLocationInfo;
  lMsg: string;
begin
  if InterModuleDebug then
    SourceInfoAvailable := True
  else
    SourceInfoAvailable := DllService.GetDebugSourceLocationInfo(@InitHost, @lLocationInfo);

  if PromptNoTD32DebugInfo and (not SourceInfoAvailable) then
    ErrorOrExit(MsgDebugInfoNotFound, TitleDebugInfoNotFound, False)
  else
  begin
    if NeedWarnToEndUser(lMsg) then
      ErrorOrExit(MsgWarningToEndUser + lMsg, TitleWarningToEndUser);
  end;

  InterObject := TInterObjectHost.Create;
  InterObject.Init(False);
end;

procedure DeInitHost;
begin
  if SourceInfoCacheTreeRootNode.Left <> nil then
    FreeSourceInfoCacheTreeNode(SourceInfoCacheTreeRootNode.Left);
  if SourceInfoCacheTreeRootNode.Right <> nil then
    FreeSourceInfoCacheTreeNode(SourceInfoCacheTreeRootNode.Right);

  if InterObject <> nil then
  begin
    InterObject.DeInit;
    SimpleFreeAndNil(InterObject);
  end;
end;

procedure ConnectToInspector;
begin
  InterObject.TryConnect;
end;

function GetReportLogFileName: string;
begin
  Result := ParamStr(0) + LogFileExt;
end;

procedure BeginOutputString(AType: Integer);
var
  lSR: PSharedRecord;
  lData: PEnumHookIndexParamedData;
begin
  lSR := InterObject.GetShareMemory;
  lData := @lSR^.EnumData;
  lData^.StringOutputType := AType;

  InterObject.StringOutput.BeginOutput;
end;

function EndOutputString: Integer;
var
  lSR: PSharedRecord;
  lData: PEnumHookIndexParamedData;
begin
  InterObject.StringOutput.EndOutput;

  if InterObject.CurrentOutput = SOC_ToInspector then
  begin
    lSR := InterObject.GetShareMemory;
    lData := @lSR^.EnumData;

    Result := lData^.ResultValue;

    //always reset the output type.
    lData^.StringOutputType := SOT_Print;
  end
  else
    Result := 0;
end;

procedure OutputString(const S: string);
begin
  InterObject.StringOutput.Output(S);
end;

procedure OutputSectionDelimiter;
begin
  OutputString('');
  OutputString('');
end;

function ThumbMatch(AThumb: PMemThumb;
  AData: PEnumHookIndexParamedData; ACheckIgnored: Boolean = True; ACheckModule: Boolean = True): Boolean;
var
  lAddr: Cardinal;
begin
  if ACheckModule and OnlyReportLeakFromCurrentModule and (DestModuleAddrLow <> 0) then
  begin
    lAddr := AData^.Data.ThumbPtr^.StackTrace[0];
    if (lAddr < DestModuleAddrLow) or (lAddr > DestModuleAddrHigh) then
    begin
      Result := False;
      Exit;
    end;
  end;


  Result := ((1 shl AThumb^.MemType) and AData^.MatchMemTypes) <> 0;

  if ACheckIgnored then
    Result := Result and (AThumb^.GroupID <> GroupID_Ignored);
  if Result then
  begin
    if AData^.MatchGroupID = GroupID_Ignored then
      Result := (AThumb^.GroupID and GroupID_Ignored) <> 0
    else
    begin
      Result := (AData^.MatchGroupID = 0)
        or ((AData^.MatchGroupID > 0) and (AThumb^.GroupID = AData^.MatchGroupID))
        or ((AData^.MatchGroupID < 0) and (AThumb^.GroupID <> AData^.GroupID))
        ;
    end;
  end;
end;

function HandleNodeMatch(AData: PEnumHookIndexData;
  ANode: PHandleBinaryTreeNode; ACheckIgnored: Boolean = True): Boolean;
var
  lThumb: TMemThumb;
  lAddr: Cardinal;
begin
  if OnlyReportLeakFromCurrentModule and (DestModuleAddrLow <> 0) then
  begin
    lAddr := ANode^.StackTrace[0];
    if (lAddr < DestModuleAddrLow) or (lAddr > DestModuleAddrHigh) then
    begin
      Result := False;
      Exit;
    end;
  end;


  lThumb.MemType := ANode^.HandleType;
  lThumb.GroupID := ANode^.GroupID;

  Result := ThumbMatch(@lThumb,
    PEnumHookIndexParamedData(AData), ACheckIgnored, False);
end;

procedure EnumHooks(ACallback: THookEnumCallback;
  AData: PEnumHookIndexData; AHandleHookCalleback: THandleHookEnumCallback);
begin
  EnumHookIndex(@ACallback, AData);
  HandleHookManager.EnumHookIndex(AHandleHookCalleback, AData);
end;

procedure ResetGDISubTypeCount(AData: PEnumHookIndexParamedData);
var
  I: Integer;
begin
  for I := Low(AData^.GDISubTypeCount) to High(AData^.GDISubTypeCount) do
    AData^.GDISubTypeCount[I] := 0;
end;

procedure IncGDISubTypeCount(AData: PEnumHookIndexParamedData;
  ANode: PHandleBinaryTreeNode);
begin
  if ANode^.HandleType = RESTYPE_GDI then
    Inc(AData^.GDISubTypeCount[ANode^.HandleSubType]);
end;

procedure OutputGDISubTypeCount(AData: PEnumHookIndexParamedData);
var
  I: Integer;
  S: string;
begin
  S := 'GDI summary: ';
  for I := Low(AData^.GDISubTypeCount) to High(AData^.GDISubTypeCount) do
  begin
    if I > Low(AData^.GDISubTypeCount) then
      S := S + ', ';
    S := S + HandleSubTypeNames_GDI[I] + ': ' + SimpleIntToStr(AData^.GDISubTypeCount[I]);
  end;
  OutputString(S);
end;

function GetMemBlockCountCallback(AData: PEnumHookIndexData): Boolean;
var
  lData: PEnumHookIndexParamedData;
begin
  Result := True;

  lData := PEnumHookIndexParamedData(ADAta);
  if ThumbMatch(AData^.ThumbPtr, lData) then
  begin
  	Inc(lData^.Count);
  end;
end;

function GetMemBlockCountCallback_Handle(AData: PEnumHookIndexData;
  ANode: PHandleBinaryTreeNode): Boolean;
var
  lData: PEnumHookIndexParamedData;
begin
  Result := True;

  lData := PEnumHookIndexParamedData(AData);
  if HandleNodeMatch(AData, ANode) then
  begin
  	Inc(lData^.HandleCount);
    IncGDISubTypeCount(lData, ANode);
  end;
end;

function GetMemBlockCount(AData: PEnumHookIndexParamedData): Integer;
begin
  AData^.Count := 0;
  AData^.HandleCount := 0;
  ResetGDISubTypeCount(PEnumHookIndexParamedData(AData));
  EnumHooks(@GetMemBlockCountCallback, @AData^.Data,
    @GetMemBlockCountCallback_Handle);
  Result := AData^.Count + AData^.HandleCount;
end;

function SetGroupIDCallback(AData: PEnumHookIndexData): Boolean;
var
  lData: PEnumHookIndexParamedData;
begin
  Result := True;

  lData := PEnumHookIndexParamedData(ADAta);
  if ThumbMatch(AData^.ThumbPtr, lData, False) then
  begin
    if lData^.GroupID = GroupID_Ignored then
      AData^.ThumbPtr.GroupID := AData^.ThumbPtr.GroupID and (not GroupID_Ignored)
    else
    	AData^.ThumbPtr.GroupID := lData^.GroupID;
  end;
end;

function SetGroupIDCallback_Handle(AData: PEnumHookIndexData;
  ANode: PHandleBinaryTreeNode): Boolean;
var
  lData: PEnumHookIndexParamedData;
begin
  Result := True;

  lData := PEnumHookIndexParamedData(ADAta);
  if HandleNodeMatch(AData, ANode, False) then
  begin
    if lData^.GroupID = GroupID_Ignored then
      ANode^.GroupID := ANode^.GroupID and (not GroupID_Ignored)
    else
    	ANode^.GroupID := lData^.GroupID;
  end;
end;

procedure SetGroupID(AData: PEnumHookIndexParamedData);
begin
  EnumHooks(@SetGroupIDCallback, Pointer(AData), @SetGroupIDCallback_Handle);
end;

procedure OutputStackTrace(AStackTrace: PCardinal; AMaxDepth: Integer);
var
  I: Integer;
  lAddr: Cardinal;
  S: string;
  lSourceNode: PSourceInfoCacheTreeNode;
  lMoreInfo: Boolean;

  procedure AppendString(const Append: string);
  begin
    S := S + ' [' + Append + ']';
  end;
begin
  if not DefaultOutputStackTrace then
    Exit;

  for I := 0 to AMaxDepth - 1 do
  begin
    lAddr := PCardinal(Integer(AStackTrace) + I * SizeOf(Cardinal))^;
    if lAddr = 0 then
      Break;
    S := '    ST: ' + SimpleIntToHex(lAddr);

    //get more source information
    lSourceNode := nil;
    if SourceInfoAvailable
      and DefaultOutputSourceInfo then
    begin
      lSourceNode := GetSourceInfoTreeNode(lAddr);
      lMoreInfo := lSourceNode <> nil;
    end
    else
      lMoreInfo := False;
    if lMoreInfo then
    begin
      AppendString(lSourceNode^.UnitName);
      AppendString(lSourceNode^.ProcedureName);
      if lSourceNode^.LineNumber <> 0 then
        AppendString(SimpleIntToStr(lSourceNode^.LineNumber))
      else
        AppendString('');
    end;

    OutputString(S);
  end;
end;

function OutputMemStatusCallback(AData: PEnumHookIndexData): Boolean;
var
  lData: PEnumHookIndexParamedData;
  S: string;

  function ExtractStringSummary(AString: PChar): string;
  var
    I: Integer;
  begin
    try
      I := lstrlen(AString);
      SetLength(Result, I);
      if I > 0 then
        Move(AString^, Result[1], I);
      I := Pos(#13, Result);
      if I > 0 then
        Result := Copy(Result, 1, I - 1);
      if Length(Result) > MaxStringSummaryLen then
        SetLength(Result, MaxStringSummaryLen);
    except
      Result := 'Exception occurred during extract string summary.';
    end;
  end;

  function GetRefLengthInfo: string;
  begin
    Result := 'Ref: ' + SimpleIntToStr(PRefLengthRecord(AData^.MemPtr).RefCount)
      + ' Len: ' + SimpleIntToStr(PRefLengthRecord(AData^.MemPtr).Length);
  end;
begin
  Result := True;

  lData := PEnumHookIndexParamedData(ADAta);
  if ThumbMatch(AData^.ThumbPtr, lData) then
  begin
    S := SimpleIntToHex(Integer(AData^.MemPtr));
    S := S + '  BlockSize: ' + SimpleIntToStr(AData^.MemSize);
    try
      case AData^.ThumbPtr^.MemType of
        MEMTYPE_RAWGETMEM:
          S := S + ' GETMEM';
        MEMTYPE_OBJECT:
          S := S + ' Class: ' + AData^.ThumbPtr^.ClassInfo.ClassName;
        MEMTYPE_STRING:
        begin
          S := S + ' String';

          if DefaultOutputStringSummary then
            S := S + ': ' + GetRefLengthInfo
              + ' "' + ExtractStringSummary(PChar(Integer(AData^.MemPtr) + SizeOf(TRefLengthRecordString))) + '"';
        end;
        MEMTYPE_DYNARRAY:
        begin
          S := S + ' DynArray: ' + GetRefLengthInfo;
        end;
      end;
    except
      S := S + ' error occurred';
    end;
    OutputString(S);
    OutputStackTrace(@AData^.ThumbPtr^.StackTrace, StackTraceDepth);
    OutputString('');
    Inc(lData^.Count);
  end;
end;

function OutputMemStatusCallback_Handle(AData: PEnumHookIndexData;
  ANode: PHandleBinaryTreeNode): Boolean;
var
  lData: PEnumHookIndexParamedData;
  S: string;
begin
  Result := True;

  lData := PEnumHookIndexParamedData(ADAta);
  if HandleNodeMatch(AData, ANode) then
  begin
    //S := SimpleIntToHex(ANode^.Handle);
    S := IntToHex(ANode^.Handle, 8);
    case ANode^.HandleType of
      RESTYPE_GDI:
      begin
        S := #13 + S + ' GDI object -- ' + HandleSubTypeNames_GDI[ANode^.HandleSubType];
      end;
    end;
    OutputString(S);
    S := 'Allocator function: ' + ANode^.AllocatorProcName;
    OutputString(S);
    OutputStackTrace(@ANode^.StackTrace, HandleStackTraceDepth);
    Inc(lData^.HandleCount);
  end;
end;

procedure OutputMemStatus(AData: PEnumHookIndexParamedData;
  ACheckCount: Boolean; ASilentIfNoFound: Boolean);
var
  lCount, lMaxCount: Integer;
  lExit: Boolean;
  lStartTime: Cardinal;
begin
  ACheckCount := ACheckCount
    and DefaultPromptListTooManyBlock;

  if ACheckCount then
  begin
    lMaxCount := 300000;
    if DefaultOutputStackTrace then
    begin
      if DefaultOutputSourceInfo then
        lMaxCount := 30000
      else
        lMaxCount := 80000;
    end;
    lCount := GetMemBlockCount(AData);
    if lCount >= lMaxCount then
    begin
      BeginOutputString(SOT_YesNo);
      try
        OutputString('Too many (' + SimpleIntToStr(lCount) + ') memory blocks to list.');
        OutputString('The suggested max count is ' + SimpleIntToStr(lMaxCount));
        OutputString('The suggested max count of blocks may take 10-30 seconds to output.');
        OutputString('It may take about '
          + IntToStr(lCount * 10 div lMaxCount)
          + ' to '
          + IntToStr(lCount * 30 div lMaxCount)
          + ' seconds to complete.');
        OutputString('');
        OutputString('Do you want to continue listing?');
      finally
        lExit := EndOutputString = 0;
      end;
      if lExit then
        Exit;
    end;
  end;

  AData^.Count := 0;
  AData^.HandleCount := 0;
  BeginOutputString(SOT_Print);
  try
    lStartTime := GetTickCount;

    EnumHooks(@OutputMemStatusCallback, Pointer(AData), @OutputMemStatusCallback_Handle);

    if (not ASilentIfNoFound) or (AData^.Count > 0) or (AData^.HandleCount > 0) then
    begin
      OutputString(#13'Totally output '
        + IntToStr(AData^.Count) + ' blocks '
        + IntToStr(AData^.HandleCount) + ' handles'
        + ' in '
        + IntToStr((GetTickCount - lStartTime + 500) div 1000)
        + ' seconds.');
    end;
  finally
    EndOutputString;
  end;
end;

procedure CheckLeakOnExit;
begin
  if InterObject <> nil then
    InterObject.CheckLeakOnExit;
end;

function EliminateDerivedLeakCallback(AData: PEnumHookIndexData): Boolean;
var
  lData: PEnumHookIndexParamedData;
  lThumb, lThumbParent: PMemThumb;
  lIndex: Pointer;
  P: Pointer;
begin
  Result := True;

  lData := PEnumHookIndexParamedData(ADAta);
  if ThumbMatch(AData^.ThumbPtr, lData) then
  begin
    lThumb := AData^.ThumbPtr;
    P := lThumb^.ObjectParent;
    if P <> nil then
    begin
      if GetIndexAndThumb(P, @lIndex, @lThumbParent) then
      begin
        if IsValidIndex(lIndex) and (lThumbParent <> nil) then
        begin
          if lThumbParent^.GroupID = lThumb^.GroupID then
          begin
            if lData^.GroupID = GroupID_Ignored then
              lThumb^.GroupID := lThumb^.GroupID or GroupID_Ignored
            else
              lThumb^.GroupID := lData^.GroupID;

            Inc(lData^.LeakEliminateInfo.DerivedLeakCount);
          end;
        end;
      end;
    end;
  end;
end;

function EliminateDerivedLeak(AData: PEnumHookIndexParamedData): Integer;
begin
  AData^.LeakEliminateInfo.DerivedLeakCount := 0;

  EnumHookIndex(@EliminateDerivedLeakCallback, Pointer(AData));

  Result := AData^.LeakEliminateInfo.DerivedLeakCount;
end;

function EliminateLeakOnSameCallingPathCallback(AData: PEnumHookIndexData): Boolean;
var
  lData: PEnumHookIndexParamedData;
  lThumb: PMemThumb;
  lRootNode, lFoundNode: PLeakInfoRemoverCacheTreeNode;
  lFound: Boolean;
  I, K: Integer;
begin
  Result := True;

  lData := PEnumHookIndexParamedData(AData);

  if ThumbMatch(AData^.ThumbPtr, lData) then
  begin
    lThumb := AData^.ThumbPtr;

    lRootNode := PLeakInfoRemoverCacheTreeNode(lData^.LeakEliminateInfo.LeakInfoRemoverCacheRootNode);
    lFound := GetLeakInfoRemoverTreeNode(lThumb, lRootNode, lFoundNode);
    lData^.LeakEliminateInfo.LeakInfoRemoverCacheRootNode := lRootNode;

    if lFound then
    begin
      Inc(lFoundNode^.SameCount);

      if lData^.GroupID = GroupID_Ignored then
        lThumb^.GroupID := lThumb^.GroupID or GroupID_Ignored
      else
        lThumb^.GroupID := lData^.GroupID;

      Inc(lData^.LeakEliminateInfo.SameCallingPathLeakCount);

      for I := 0 to High(lData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax) do
      begin
        if lData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax[I] = nil then
        begin
          lData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax[I] := lFoundNode;
          Break;
        end;

        if lData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax[I] = lFoundNode then
          Break;
          
        if lFoundNode^.SameCount
          >= PLeakInfoRemoverCacheTreeNode(lData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax[I])^.SameCount then
        begin
          for K := I to High(lData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax) - 1 do
            lData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax[K + 1] := lData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax[K];
          lData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax[I] := lFoundNode;
          Break;
        end;
      end;
    end;
  end;
end;

function EliminateLeakOnSameCallingPath(AData: PEnumHookIndexParamedData): Integer;
begin
  AData^.LeakEliminateInfo.SameCallingPathLeakCount := 0;

  AData^.LeakEliminateInfo.LeakInfoRemoverCacheRootNode := nil;

  EnumHookIndex(@EliminateLeakOnSameCallingPathCallback, Pointer(AData));

  Result := AData^.LeakEliminateInfo.SameCallingPathLeakCount;
end;

procedure GetHookOption(AData: PEnumHookIndexParamedData);
begin
  PackHookOption(@AData^.HookOption);
end;

procedure SetHookOption(AData: PEnumHookIndexParamedData);
begin
  UnpackHookOption(@AData^.HookOption);
end;

function GetFileterString(AData: PEnumHookIndexParamedData): string;
var
  I: Integer;
  S: string;
begin
  Result := '';
  S := '';
  for I := 0 to MEMTYPE_COUNT - 1 do
  begin
    if ((1 shl I) and (AData^.MatchMemTypes)) <> 0 then
    begin
      if S <> '' then
        S := S + ', ';
      S := S + '[' + MemTypeCaptions[I] + ']';
    end;
  end;
  if S <> '' then
    Result := 'MemTyps: ' + S + '    ';
  Result := Result + 'Session ID: ' + SimpleIntToStr(AData^.MatchGroupID);
end;

procedure OutputFileterString(AData: PEnumHookIndexParamedData);
begin
  OutputString('Filters: ' + GetFileterString(AData));
end;


//********************Pointer error detection start****************************

procedure PointerAccessError(P: Pointer; AFreedThumb: PMemFreedThumb;
  ASkipFrames: Cardinal = 4);
var
  lStackTrace: array[0..MaxStackTraceDepth - 1] of Cardinal;
  S: string;
begin
  if KeepDetailedTrackOnFreedBlock then
  begin
    S := '';
    if AFreedThumb <> nil then
    begin
      try
        case PInteger(P)^ of
          MEMTYPE_RAWGETMEM:
            S := S + ' GETMEM';
          MEMTYPE_STRING:
            S := S + ' String';
          MEMTYPE_DYNARRAY:
            S := S + ' DynArray: '
          else
          begin
            if AFreedThumb^.RealClassInfo <> nil then
              S := S + '  Class: ' + AFreedThumb^.RealClassInfo.ClassName
            else
              S := S + '  Class: ' + TClass(PInteger(P)^).ClassName;
          end;
        end;
      except
        S := S + '  error occurred';
      end;
    end;

    if S <> '' then
      OutputString(S);
    OutputString('=========Listing the first free stack trace=========');
    if AFreedThumb <> nil then
      OutputStackTrace(@AFreedThumb^.StackTrace, FreedStackTraceDepth)
    else
      OutputString('    Not available.');
    GetFrameBasedStackTrace(@lStackTrace, FreedStackTraceDepth, ASkipFrames);
    OutputString('');
    OutputString('=========Listing current stack trace=========');
    OutputStackTrace(@lStackTrace, FreedStackTraceDepth);
  end;
end;

procedure EnterPointerError;
begin
  EnterHookLocks(AllHookLocks);
  InterObject.PushCurrentOutput;
  if InterObject.GuestAvailable then
    InterObject.SetCurrentOutput(SOC_ToInspector)
  else
    InterObject.SetCurrentOutput(SOC_ToMessageBox);
  BeginOutputString(SOT_Error);
end;

procedure LeavePointerError;
begin
  try
    try
      EndOutputString;
    finally
      InterObject.PopCurrentOutput;
    end;
  finally
    LeaveHookLocks(AllHookLocks);
  end;
end;

procedure BlockFreedTwice(P: Pointer; AFreedThumb: PMemFreedThumb);
begin
  EnterPointerError;
  try
    OutputString(SimpleIntToHex(Integer(P)) + ' is being freed twice.');

    PointerAccessError(P, AFreedThumb);

    OutputSectionDelimiter;
  finally
    LeavePointerError;
  end;
end;

procedure ReallocOnFreedBlock(P: Pointer; AFreedThumb: PMemFreedThumb);
begin
  EnterPointerError;
  try
    OutputString(SimpleIntToHex(Integer(P)) + ' is freed but being reallocated.');

    PointerAccessError(P, AFreedThumb);

    OutputSectionDelimiter;
  finally
    LeavePointerError;
  end;
end;

procedure AccessFreedInterface(P: Pointer; AIntf: Pointer; AMethod: Cardinal; AFreedThumb: PMemFreedThumb);
var
  lClass: TClass;
  lIntfTable: PInterfaceTable;
  lIntfEntry: PInterfaceEntry;
  I: Integer;
  lGUID: string;
  lSourceNode: PSourceInfoCacheTreeNode;
  S: string;
begin
  EnterPointerError;
  try
    OutputString('Interface was freed but is being accessed');
    if (P = nil) or (AIntf = nil) or (AFreedThumb = nil) then
    begin
      OutputString('The object and interface can''t be identified.');
    end
    else
    begin
      if AFreedThumb^.RealClassInfo <> nil then
        lClass := AFreedThumb^.RealClassInfo
      else
        lClass := TObject(P).ClassType;

      OutputString('Object: ' + SimpleIntToHex(Integer(P)) + '  Class: ' + lClass.ClassName);

      lGUID := '';
      while lClass <> nil do
      begin
        lIntfTable := lClass.GetInterfaceTable;
        if lIntfTable <> nil then
        begin
          for I := 0 to lIntfTable.EntryCount-1 do
          begin
            lIntfEntry := @lIntfTable.Entries[I];
            if Pointer(Cardinal(P) + Cardinal(lIntfEntry^.IOffset)) = AIntf then
            begin
              lGUID := GUIDToString(lIntfEntry^.IID);
              Break;
            end;
          end;
        end;
        lClass := lClass.ClassParent;
      end;

      if lGUID <> '' then
        OutputString('GUID: ' + lGUID)
      else
        OutputString('Interface can''t be found from the object. Maybe wrong.');
    end;
    S := '  Accessing addr: ';
    if AMethod = 0 then
    begin
      S := S +  'Unknown';
    end
    else
    begin
      lSourceNode := GetSourceInfoTreeNode(Cardinal(AMethod));
      S := S + SimpleIntToHex(Cardinal(AMethod));
      if lSourceNode <> nil then
      begin
        S := S + ' [' + lSourceNode^.UnitName + '] [' + lSourceNode^.ProcedureName + '] [';
        if lSourceNode^.LineNumber <> 0 then
          S := S + SimpleIntToStr(lSourceNode^.LineNumber);
        S := S + ']';
      end;
    end;
    OutputString(S);

    PointerAccessError(P, AFreedThumb, 3);

    OutputString('');
    OutputString('Now an "int 3" instruction will be executed and the program stop there.');

    OutputSectionDelimiter;
  finally
    LeavePointerError;

    asm int 3 end;
  end;
end;

procedure CallOnFreedObject(AObj: TObject; AClass: TClass; AFreedThumb: PMemFreedThumb);
var
  S: string;
begin
  EnterPointerError;
  try
    OutputString('Call on virtual method of freed object.');
    S := '';
    try
      if AObj <> nil then
        S := S + '  Object: ' + SimpleIntToHex(Cardinal(AObj));
      if AClass <> nil then
        S := S + '  Class: ' + SimpleIntToHex(Cardinal(AClass)) + ' ' + AClass.ClassName;
      OutputString(S);
    except
      S := S + ' error occurred';
      OutputString(S);
    end;
    PointerAccessError(AObj, AFreedThumb, 3);

    OutputString('');
    OutputString('Now an "int 3" instruction will be executed and the program stop there.');

    OutputSectionDelimiter;
  finally
    LeavePointerError;

    asm int 3 end;
  end;
end;

//********************Pointer error detection end****************************

{ TInterObjectHost }

function TInterObjectHost.DoMoreLeakMsg(Msg: Integer;
  Param: Cardinal): Integer;
var
  lData: TEnumHookIndexParamedData;
  lGroupID: Integer;
begin
  Result := 0;

  EnterMemHookCritical;
  EnterHookLocks(AllHookLocks);
  try
    if not (Msg in [ SM_GetHookOption, SM_SetHookOption, SM_GetEnabledMemTypes ]) then
      OutputFileterString(@ShareMemoryPointer^.EnumData);

    case Msg of
      SM_MemCount:
      begin
        GetMemBlockCount(@InterObject.GetShareMemory.EnumData);
        OutputString('Memory block count: '
          + SimpleIntToStr(InterObject.GetShareMemory.EnumData.Count));
        OutputString('Resource handle count: '
          + SimpleIntToStr(InterObject.GetShareMemory.EnumData.HandleCount));
        OutputGDISubTypeCount(PEnumHookIndexParamedData(@InterObject.GetShareMemory.EnumData));
        OutputSectionDelimiter;
      end;

      SM_SessionLeakBegin:
      begin
        lData := ShareMemoryPointer^.EnumData;
        lGroupID := lData.GroupID;
        SessionLeakBegin(lGroupID);

        OutputString('Begin session leak dectection at group '
           + SimpleIntToStr(ShareMemoryPointer^.EnumData.GroupID));

        ShareMemoryPointer^.EnumData.ResultValue := GetCurGroupID;
      end;

      SM_SessionLeakEnd:
      begin
        lData := ShareMemoryPointer^.EnumData;
        lGroupID := lData.GroupID;
        SessionLeakEnd(lGroupID, True);

        OutputString('End session leak dectection at group '
          + SimpleIntToStr(ShareMemoryPointer^.EnumData.GroupID));

        OutputSectionDelimiter;

        ShareMemoryPointer^.EnumData.ResultValue := GetCurGroupID;
      end;

      SM_IncrementalSessionLeakBegin:
      begin
        if IncCurGroupID then
        begin
          lGroupID := GetCurGroupID;
          SessionLeakBegin(lGroupID);

          OutputString('Begin incremental session leak dectection at group '
            + SimpleIntToStr(lGroupID));
        end
        else
        begin
          OutputString('Begin incremental session leak detection failed: No more available group index.');
        end;

        ShareMemoryPointer^.EnumData.ResultValue := GetCurGroupID;
      end;

      SM_IncrementalSessionLeakEnd:
      begin
        lGroupID := GetCurGroupID;
        if DecCurGroupID then
        begin
          SessionLeakEnd(lGroupID, True);

          OutputString('End incremental session leak dectection at group '
            + SimpleIntToStr(lGroupID));
        end
        else
        begin
          OutputString('End incremental session leak detection failed: No more available group index.');
        end;

        OutputSectionDelimiter;

        ShareMemoryPointer^.EnumData.ResultValue := GetCurGroupID;
      end;

      SM_ListMem:
      begin
        SessionLeakEnd(ShareMemoryPointer^.EnumData.MatchGroupID, False);

        BeginOutputString(SOT_Print);
        try
          OutputString('End list memory');
          OutputSectionDelimiter;
        finally
          EndOutputString;
        end;
      end;

      SM_GetHookOption:
      begin
        GetHookOption(@ShareMemoryPointer^.EnumData);
      end;

      SM_SetHookOption:
      begin
        SetHookOption(@ShareMemoryPointer^.EnumData);
      end;

      SM_GetEnabledMemTypes:
      begin
        Result := GetEnabledMemTypes;
        ShareMemoryPointer^.EnumData.ResultValue := Result;
      end;
    end;
  finally
    LeaveHookLocks(AllHookLocks);
    LeaveMemHookCritical;
  end;
end;

procedure TInterObjectHost.SessionLeakBegin(AGroupID: Integer);
var
  lData: TEnumHookIndexParamedData;
begin
  //reset all blocks has AGroupID to group 0
  lData := ShareMemoryPointer^.EnumData;
  lData.MatchGroupID := AGroupID;
  lData.GroupID := 0;
  SetGroupID(@lData);

  SetCurGroupID(AGroupID);
end;

procedure TInterObjectHost.SessionLeakEnd(AGroupID: Integer; AReset: Boolean);
var
  lEliminateInfo: TLeakEliminateInfo;
begin
  ShareMemoryPointer^.EnumData.MatchGroupID := AGroupID;
  if AReset then
    ShareMemoryPointer^.EnumData.GroupID := 0
  else
    ShareMemoryPointer^.EnumData.GroupID := GroupID_Ignored;

  BeginLeakEliminate(@lEliminateInfo, @ShareMemoryPointer^.EnumData);
  try
    OutputMemStatus(@ShareMemoryPointer^.EnumData, True, False);
  finally
    EndLeakEliminate(@lEliminateInfo, @ShareMemoryPointer^.EnumData, False);
  end;

  if not AReset then
  begin
    ShareMemoryPointer^.EnumData.MatchGroupID := GroupID_Ignored;
    ShareMemoryPointer^.EnumData.GroupID := GroupID_Ignored;
    SetGroupID(@ShareMemoryPointer^.EnumData);
  end;
end;

function TInterObjectHost.GetGuestWinClassName: string;
begin
  Result := InspectorClassName;
end;

function TInterObjectHost.GetMyWinClassName: string;
begin
  Result := HostClassName;
end;

constructor TInterObjectHost.Create;
begin
  inherited;

  //initialize all output object
  CurrentOutput := SOC_ToFile;
  CurrentOutput := SOC_DebugString;
  CurrentOutput := SOC_ToInspector;
end;

destructor TInterObjectHost.Destroy;
var
  I: Integer;
begin
  for I := Low(FStringOutputList) to High(FStringOutputList) do
  begin
    FStringOutputList[I].Free;
    FStringOutputList[I] := nil;
  end;

  inherited;
end;

procedure TInterObjectHost.BeginLeakEliminate(AInfo: PLeakEliminateInfo;
  AData: PEnumHookIndexParamedData);
begin
  FillChar(AInfo^, SizeOf(TLeakEliminateInfo), 0);

  if DefaultEliminateObjectFieldsLeak then
    AInfo^.DerivedLeakCount := EliminateDerivedLeak(AData);

  if DefaultEliminateLeaksOnSameCallingPath then
    AInfo^.SameCallingPathLeakCount := EliminateLeakOnSameCallingPath(AData);
end;

procedure TInterObjectHost.EndLeakEliminate(AInfo: PLeakEliminateInfo;
  AData: PEnumHookIndexParamedData; ASilentIfNoFound: Boolean);
var
  lNode: PLeakInfoRemoverCacheTreeNode;
  I: Integer;
begin
  BeginOutputString(SOT_Print);
  try
    if (not ASilentIfNoFound) or (AInfo^.DerivedLeakCount > 0) then
    begin
      if DefaultEliminateObjectFieldsLeak then
        OutputString('Eliminated derived object''s fields leak of '
          + SimpleIntToStr(AInfo^.DerivedLeakCount) + ' blocks.')
      else
        OutputString('Elimination of object''s fields leak is disabled.');
    end;

    if (not ASilentIfNoFound) or (AInfo^.SameCallingPathLeakCount > 0) then
    begin
      if DefaultEliminateLeaksOnSameCallingPath then
      begin
        OutputString('Eliminated leaks on same calling path of '
          + SimpleIntToStr(AInfo^.SameCallingPathLeakCount) + ' blocks.');

        if AInfo^.SameCallingPathLeakCount > 0 then
        begin
          OutputString('The most places blocks on same calling path occurs in:');
          for I := 0 to High(AData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax) do
          begin
            lNode := PLeakInfoRemoverCacheTreeNode(AData^.LeakEliminateInfo.LeakInfoRemoverCacheNodeMax[I]);
            if lNode <> nil then
            begin
              OutputString('Same count: ' + SimpleIntToStr(lNode^.SameCount));
              OutputStackTrace(@lNode^.StackTrace, StackTraceDepth);
            end;
          end;
        end;
      end
      else
        OutputString('Elimination leaks on same calling path is disabled.');
    end;
  finally
    EndOutputString;
  end;

  if AData^.LeakEliminateInfo.LeakInfoRemoverCacheRootNode <> nil then
    FreeLeakInfoRemoverCacheTreeNode(AData^.LeakEliminateInfo.LeakInfoRemoverCacheRootNode);
end;

procedure TInterObjectHost.CheckLeakOnExit;
var
  lData: TEnumHookIndexParamedData;
  lEliminateInfo: TLeakEliminateInfo;
  lReport: Boolean;
  lTime: TSystemTime;
  lBuf: array[0..1023] of Char;
  S: string;
begin
  if not DefaultCheckLeakOnExit then
    Exit;

  if ReportToLogFileWhenCheckOnExit then
    InterObject.CurrentOutput := SOC_ToFile;

  FillChar(lData, SizeOf(lData), 0);
  EnterHookLocks(AllHookLocks);
  try
    BeginOutputString(SOT_Print);
    try
      lData.MatchMemTypes := $ffffff;
      lData.MatchGroupID := 0;
      lData.GroupID := 1;
      SetGroupID(@lData);

      lData.MatchMemTypes := $ffffff;
      lData.MatchGroupID := 1;
      lData.GroupID := 0;
      BeginLeakEliminate(@lEliminateInfo, @lData);
      try
        lData.MatchMemTypes := $ffffff;
        lData.MatchGroupID := -1;
        lReport := GetMemBlockCount(@lData) > 0;
        if lReport then
        begin
          GetLocalTime(lTime);
          GetDateFormat(LOCALE_SYSTEM_DEFAULT, DATE_LONGDATE, @lTime, nil, @lBuf[0], Length(lBuf));
          //S := StrPas(@lBuf[0]);
          S := lBuf;
          GetTimeFormat(LOCALE_SYSTEM_DEFAULT, 0, @lTime, nil, @lBuf[0], Length(lBuf));
          //S := S + ' ' + StrPas(@lBuf[0]);
          S := S + ' ' + lBuf;
          OutputString('Checking final leak at ' + S);
        end;
        OutputMemStatus(@lData, False, True);

        if lReport then
        begin
          if ReportToLogFileWhenCheckOnExit then
          begin
            ErrorOrExit(MsgFinalLeakLogFile + GetReportLogFileName, TitleFinalLeakLogFile);
          end
          else
          begin
            if not InterObject.GuestAvailable then
            begin
              ErrorOrExit(MsgFinalLeak, TitleFinalLeak);
            end;
          end;
        end;
      finally
        EndLeakEliminate(@lEliminateInfo, @lData, True);
      end;
    finally
      EndOutputString;
    end;

    if lData.Count + lData.HandleCount > 0 then
    begin
      BeginOutputString(SOT_Error);
      try
        OutputString(#13'Leaks found on program exit. Totallly '
          + IntToStr(lData.Count) + ' blocks '
          + IntToStr(lData.HandleCount) + ' handles.');
        OutputSectionDelimiter;
      finally
        EndOutputString;
      end;
    end;
  finally
    LeaveHookLocks(AllHookLocks);
  end;
end;

procedure TInterObjectHost.SetCurrentOutput(const Value: Integer);
begin
  Assert((Value >= Low(FStringOutputList)) and (Value <= High(FStringOutputList)));

  FCurrentOutput := Value;

  if FStringOutputList[Value] = nil then
  begin
    case Value of
      SOC_DebugString:
        FStringOutputList[Value] := TStringOutputDebugString.Create;

      SOC_ToInspector:
        FStringOutputList[Value] := TStringOutputToInspector.Create(Self);

      SOC_ToFile:
        FStringOutputList[Value] := TStringOutputToFile.Create(GetReportLogFileName);

      SOC_ToMessageBox:
        FStringOutputList[Value] := TStringOutputToMessageBox.Create;
    end;
  end;
end;

function TInterObjectHost.GetStringOutput: TStringOutput;
begin
  Result := FStringOutputList[CurrentOutput];
end;

procedure TInterObjectHost.PopCurrentOutput;
begin
  Assert(FCurrentOutputTop > 0);
  Dec(FCurrentOutputTop);
  CurrentOutput := FCurrentOutputStack[FCurrentOutputTop];
end;

procedure TInterObjectHost.PushCurrentOutput;
begin
  Assert(FCurrentOutputTop < Length(FCurrentOutputStack) - 1);
  FCurrentOutputStack[FCurrentOutputTop] := FCurrentOutput;
  Inc(FCurrentOutputTop);
end;

end.

