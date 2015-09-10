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

The Original Code is DenomoCPPMain.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit DenomoCPPMain;

interface

uses
  SysUtils, Classes, Windows,
  JclDebug, JclTD32, JCLWin32, JclPeImage, Disasm32,
  MiscUtils, DenomoUtils, DenomoMemHook, DenomoHost, DenomoConfig,
  CodeHookIntf, CodeHook;

type
  ICPPService = interface
    function GetInstructionLength(ACode: PByte): Integer; stdcall;
    procedure InitDenomoCPP(AInfo: PCPPInfo); stdcall;
    procedure DeInitDenomoCPP; stdcall;
  end;

  TDebugInfoEngine = class
  protected
    function DoInitEngine(AModule: HMODULE): Boolean; virtual;
    procedure DoDeInitEngine; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function InitEngine(AModule: HMODULE): Boolean;
    procedure DeInitEngine;

    function GetDebugSourceLocationInfo(AAddr: Pointer;
      AInfo: PDebugSourceLocationInfo): LongBool; virtual;
    function GetProcAddressesFromNames(AModule: HMODULE;
      AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer): Integer; virtual;
  end;
  TDebugInfoEngineClass = class of TDebugInfoEngine;

  TDebugInfoEngineJCL = class(TDebugInfoEngine)
  protected
    function DoInitEngine(AModule: HMODULE): Boolean; override;
    procedure DoDeInitEngine; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetDebugSourceLocationInfo(AAddr: Pointer;
      AInfo: PDebugSourceLocationInfo): LongBool; override;
    function GetProcAddressesFromNames(AModule: HMODULE;
      AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer): Integer; override;
  end;

  TDebugInfoEngineDbgHelp = class(TDebugInfoEngine)
  protected
    function DoInitEngine(AModule: HMODULE): Boolean; override;
    procedure DoDeInitEngine; override;
  public
    function GetDebugSourceLocationInfo(AAddr: Pointer;
      AInfo: PDebugSourceLocationInfo): LongBool; override;
    function GetProcAddressesFromNames(AModule: HMODULE;
      AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer): Integer; override;
    class function _GetProcAddressesFromNames(AModule: HMODULE;
      AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer): Integer;
  end;

  TDebugInfoEngineManager = class
  private
    FDebugInfoEngine: TDebugInfoEngine;
  public
    constructor Create;
    destructor Destroy; override;

    function Init(AModule: HMODULE): Boolean;
    procedure DeInit;

    property DebugInfoEngine: TDebugInfoEngine read FDebugInfoEngine;
  end;

  TDenomoDLLService = class(TInterfacedObject, IDllService, ICPPService)
  private
    FDisassembler: TDisAsm;
    FOnShutDown: TDllServiceShutDown;
  protected
    { IDllService }
    procedure GetCodeHook(out Obj); stdcall;

    function GetDebugSourceLocationInfo(AAddr: Pointer;
      AInfo: PDebugSourceLocationInfo): LongBool; stdcall;
    procedure GetProcAddressesFromNames(AModule: HMODULE;
      AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer); stdcall;
    function GetModuleImageSize(AModule: HMODULE): Cardinal; stdcall;
    procedure SetOnShutDown(AOnShutDown: TDllServiceShutDown); stdcall;

    { ICPPService }
    function GetInstructionLength(ACode: PByte): Integer; stdcall;
    procedure InitDenomoCPP(AInfo: PCPPInfo); stdcall;
    procedure DeInitDenomoCPP; stdcall;

    property Disassembler: TDisAsm read FDisassembler;
  public
    constructor Create;
    destructor Destroy; override;

    procedure NotifyShutDown;
  end;

  PImagehlpSymbol = ^TImagehlpSymbol;
  _IMAGEHLP_SYMBOL = packed record
    SizeOfStruct: DWORD;                                { set to sizeof(IMAGEHLP_SYMBOL) }
    Address: DWORD;                                     { virtual address including dll base address }
    Size: DWORD;                                        { estimated size of symbol, can be zero }
    Flags: DWORD;                                       { info about the symbols, see the SYMF defines }
    MaxNameLength: DWORD;                               { maximum size of symbol name in 'Name' }
    Name: packed array[0..0] of Char;                   { symbol name (null terminated string) }
  end;
  IMAGEHLP_SYMBOL = _IMAGEHLP_SYMBOL;
  PIMAGEHLP_SYMBOL = ^IMAGEHLP_SYMBOL;
  TImagehlpSymbol = _IMAGEHLP_SYMBOL;

  _SYMBOL_INFO = packed record
    SizeOfStruct: Cardinal;
    TypeIndex: Cardinal;
    Reserved: array[0..1] of Int64;
    Reserved2: Cardinal;
    Size: Cardinal;
    ModBase: Int64;
    Flags: Cardinal;
    Value: Int64;
    Address: Int64;
    Register: Cardinal;
    Scope: Cardinal;
    Tag: Cardinal;
    NameLen: Cardinal;
    MaxNameLen: Cardinal;
    Name: packed array[0..0] of Char;
  end;
  SYMBOL_INFO = _SYMBOL_INFO;
  PSYMBOL_INFO = ^_SYMBOL_INFO;

var
  _SymInitialize: function(hProcess: Cardinal; UserSearchPath: PChar;
    fInvadeProcess: LongBool): LongBool; stdcall = nil;
  _SymLoadModule: function(hProcess: Cardinal; hFile: Cardinal; ImageName: PChar; ModuleName: PChar;
    BaseOfDll: Cardinal; SizeOfDll: Cardinal): Cardinal; stdcall = nil;
  _SymGetSymFromAddr: function(hProcess: Cardinal; Address: Cardinal;
    Displacement: PCardinal; Symbol: PImagehlpSymbol): LongBool; stdcall = nil;
  _SymGetLineFromAddr: function(hProcess: Cardinal; dwAddr: Cardinal;
    pdwDisplacement: PCardinal; var Line: TImageHlpLine): LongBool; stdcall = nil;
  _SymGetSymFromName: function(hProcess: Cardinal; Name: PChar; Symbol: PImagehlpSymbol): LongBool; stdcall = nil;
  _SymFromName: function(hProcess: Cardinal; Name: PChar; Symbol: PSYMBOL_INFO): LongBool; stdcall = nil;
  _UnDecorateSymbolName: function (DecoratedName: PChar; UnDecoratedName: PChar; UndecoratedLength: Cardinal; Flags: Cardinal): DWORD; stdcall = nil;

procedure InitStartDll;
procedure InitDll;
procedure DeInitDll;

procedure GetCPPService(out Obj); stdcall;

implementation

const
  MsgModuleNotFound = 'Can not find the module to detect memory leak.'#13#10
    + 'You should modify the value ''DetectModuleName'' in DenomoConfig.pas or Denomo.ini.'#13#10
    + ''#13#10
    + 'Now the program will exit.'#13#10
    + 'You will not lose any data because this program hasn''t run yet.'#13#10
    + #13#10
    + 'The module name is ';

  TitleModuleNotFound = 'Module not found';

var
  DebugInfoEngineManager: TDebugInfoEngineManager;
  DenomoDLLService: TDenomoDLLService = nil;
  HostFileName: string = '';
  HostPath: string = '';
  DbgHelpAvailable: Boolean = False;
  MainProc: Pointer = nil;

procedure GetCPPService(out Obj);
begin
  if DenomoDLLService = nil then
    DenomoDLLService := TDenomoDLLService.Create;
  Pointer(Obj) := Pointer(ICPPService(DenomoDLLService));
end;

function GetProcAddressesFromName(AModule: HMODULE; const AProcName: string): Pointer;
var
  lSafeString: TSafeString;
begin
  Result := nil;
  PasStrToSafeStr(AProcName, @lSafeString);
  DenomoDLLService.GetProcAddressesFromNames(AModule, @lSafeString, 1, @Result);
end;

function InitDbgHelp: Boolean;
var
  H: Cardinal;
begin
  H := LoadLibrary('dbghelp.dll');
  Result := H <> 0;
  if not Result then
    Exit;

  _SymInitialize := GetProcAddress(H, 'SymInitialize');
  Result := @_SymInitialize <> nil;
  if not Result then
    Exit;

  _SymLoadModule := GetProcAddress(H, 'SymLoadModule');
  Result := @_SymLoadModule <> nil;
  if not Result then
    Exit;

  _SymGetSymFromAddr := GetProcAddress(H, 'SymGetSymFromAddr');
  Result := @_SymGetSymFromAddr <> nil;
  if not Result then
    Exit;

  _SymGetLineFromAddr := GetProcAddress(H, 'SymGetLineFromAddr');
  Result := @_SymGetLineFromAddr <> nil;
  if not Result then
    Exit;

  _SymGetSymFromName := GetProcAddress(H, 'SymGetSymFromName');
  Result := @_SymGetSymFromName <> nil;
  if not Result then
    Exit;

  _SymFromName := GetProcAddress(H, 'SymFromName');
  Result := @_SymFromName <> nil;
  if not Result then
    Exit;

  H := LoadLibrary('imagehlp.dll');
  if H <> 0 then
  begin
    _UnDecorateSymbolName := GetProcAddress(H, 'UnDecorateSymbolName');
  end;
end;

function UnDecorateSymbolName(const ASymbolName: string): string;
const
  SymbolNameLength = 1000;
  SymbolSize = SizeOf(TImagehlpSymbol) + SymbolNameLength;
  UndecoratedLength = 255;
var
  UndecoratedName: array [0..UndecoratedLength] of Char;
  lLen: Integer;
begin
  if @_UnDecorateSymbolName = nil then
    Result := ASymbolName
  else
  begin
    lLen := _UnDecorateSymbolName(PChar(ASymbolName), UndecoratedName, UndecoratedLength, UNDNAME_NAME_ONLY or UNDNAME_NO_ARGUMENTS);
    if lLen = 0 then
      Result := ASymbolName
    else
      SetString(Result, UndecoratedName, lLen);
  end;
end;

function LoadModuleSymInfo(AModule: HMODULE): Boolean;
var
  lFileName: string;
begin
  SetLength(lFileName, MAX_PATH);
  GetModuleFileName(AModule, @lFileName[1], Length(lFileName));
  Result := _SymLoadModule(GetCurrentProcess, 0, PChar(lFileName), 'abc', 0, 0) <> 0;
end;

procedure InitStartDll;
var
  P: Pointer;
  lTempCPPInfo: PCPPInfo;
  lModuleName: PChar;
begin
  DbgHelpAvailable := InitDbgHelp;

  InterModuleDebug := True;
  if DetectModuleName = '' then
    lModuleName := nil
  else
    lModuleName := PChar(DetectModuleName);
  InterModuleHandle := GetModuleHandle(lModuleName);
  if InterModuleHandle = 0 then
  begin
    ErrorOrExit(MsgModuleNotFound + DetectModuleName, TitleModuleNotFound, True);
    Exit;
  end;

  SetLength(HostFileName, MAX_PATH);
  GetModuleFileName(InterModuleHandle, @HostFileName[1], Length(HostFileName));
  SetLength(HostFileName, lstrlen(@HostFileName[1]));

  HostPath := ExtractFilePath(HostFileName);
  HostFileName := ExtractFileName(HostFileName);

  if DenomoDLLService = nil then
    DenomoDLLService := TDenomoDLLService.Create;

  DLLService := DenomoDLLService;

  DebugInfoEngineManager := TDebugInfoEngineManager.Create;
  if not DebugInfoEngineManager.Init(InterModuleHandle) then
  begin
    FreeAndNil(DebugInfoEngineManager);
    Exit;
  end;

  lTempCPPInfo := CPPInfo;
  try
    CPPInfo := nil;
    P := GetProcAddressesFromName(InterModuleHandle, 'InitDenomo');
    if P = nil then
      Exit;
    asm
      push -1
      call P
    end;
  finally
    if lTempCPPInfo <> nil then
      CPPInfo := lTempCPPInfo;
  end;
end;

procedure DoInitDll;
begin
  InitLeakMemHook;
end;

procedure InitDll;
begin
  if MainProc <> nil then
    Exit;

  DoInitDll;
end;

procedure DoDeInitDll;
begin
  DeInitLeakMemHook;

  if DebugInfoEngineManager <> nil then
  begin
    DebugInfoEngineManager.DeInit;
    FreeAndNil(DebugInfoEngineManager);
  end;
  //FreeAndNil(DenomoDLLService);
end;

procedure DeInitDll;
begin
  if MainProc <> nil then
    Exit;

  DoDeInitDll;
end;

{ TDebugInfoEngine }

constructor TDebugInfoEngine.Create;
begin
  inherited;

end;

destructor TDebugInfoEngine.Destroy;
begin

  inherited;
end;

function TDebugInfoEngine.DoInitEngine(AModule: HMODULE): Boolean;
begin
  Result := False;
end;

function TDebugInfoEngine.GetDebugSourceLocationInfo(AAddr: Pointer;
  AInfo: PDebugSourceLocationInfo): LongBool;
begin
  Result := False;
end;

function TDebugInfoEngine.GetProcAddressesFromNames(AModule: HMODULE;
  AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer): Integer;
begin
  Result := 0;
end;

procedure TDebugInfoEngine.DoDeInitEngine;
begin

end;

function TDebugInfoEngine.InitEngine(AModule: HMODULE): Boolean;
begin
  Result := DoInitEngine(AModule);
end;

procedure TDebugInfoEngine.DeInitEngine;
begin

end;

{ TDebugInfoEngineJCL }

constructor TDebugInfoEngineJCL.Create;
begin
  inherited;

end;

destructor TDebugInfoEngineJCL.Destroy;
begin

  inherited;
end;

function TDebugInfoEngineJCL.DoInitEngine(AModule: HMODULE): Boolean;
begin
  Result := DebugInfoAvailable(AModule);

  if DbgHelpAvailable then
  begin
    _SymInitialize(GetCurrentProcess, nil, not True);
    LoadModuleSymInfo(AModule);
  end;
end;

function TDebugInfoEngineJCL.GetDebugSourceLocationInfo(AAddr: Pointer;
  AInfo: PDebugSourceLocationInfo): LongBool;
var
  lLocationInfo: TJclLocationInfo;
begin
  FillChar(AInfo^, SizeOf(TDebugSourceLocationInfo), 0);
  Result := GetLocationInfo(AAddr, lLocationInfo);

  if Result then
  begin
    AInfo^.UnitName := PasStrToSafeStr(lLocationInfo.UnitName);
    AInfo^.ProcedureName := PasStrToSafeStr(UnDecorateSymbolName(lLocationInfo.ProcedureName));
    AInfo^.LineNumber := lLocationInfo.LineNumber;
  end;

  lLocationInfo.UnitName := '';
  lLocationInfo.ProcedureName := '';
  lLocationInfo.SourceName := '';
  lLocationInfo.BinaryFileName := '';
end;

function TDebugInfoEngineJCL.GetProcAddressesFromNames(AModule: HMODULE;
  AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer): Integer;
var
  lTD32: TJclPeBorTD32Image;
  lScaner: TJclTD32InfoScanner;
  lName: string;
  lProcNames: array of string;
  I, K: Integer;
  lCountToFind: Integer;
  lTempP: PPointer;
begin
  Result := 0;

  SafeFillChar(AAddrs^, ANameCount * SizeOf(Pointer), 0);
  SetLength(lProcNames, ANameCount);

  for I := 0 to ANameCount - 1 do
  begin
    lProcNames[I] := LowerCase(
      SafeStrToPasStr(PSafeString(Integer(AProcNames)
        + I * SizeOf(TSafeString))^)
      );
  end;

  lCountToFind := ANameCount;

  lTD32 := TJclPeBorTD32Image.Create(True);
  try
    lTD32.AttachLoadedModule(AModule);
    if lTD32.IsTD32DebugPresent then
    begin
      lScaner := lTD32.TD32Scanner;
      for I := lScaner.ProcSymbolCount - 1 downto 0 do
      begin
        if lScaner.ProcSymbols[I].NameIndex > 0 then
        begin
          lName := LowerCase(lScaner.Names[lScaner.ProcSymbols[I].NameIndex]);
          for K := 0 to ANameCount - 1 do
          begin
            if CompareStr(lName, lProcNames[K]) = 0 then
            begin
              lTempP := PPointer(Integer(AAddrs) + K * SizeOf(Pointer));
              lTempP^ := Pointer(lScaner.ProcSymbols[I].Offset
                + AModule + $1000);

              Inc(Result);
              Dec(lCountToFind);
              if lCountToFind = 0 then
                Exit;

              Break;
            end;
          end;
        end;
      end;
    end;
  finally
    lTD32.Free;
  end;

  if Result = 0 then
    Result := TDebugInfoEngineDbgHelp._GetProcAddressesFromNames(AModule, AProcNames, ANameCount, AAddrs);
end;

procedure TDebugInfoEngineJCL.DoDeInitEngine;
begin
  inherited;

end;

{ TDebugInfoEngineDbgHelp }

function TDebugInfoEngineDbgHelp.DoInitEngine(AModule: HMODULE): Boolean;
begin
  Result := DbgHelpAvailable;
  if not Result then
    Exit;

  Result := _SymInitialize(GetCurrentProcess, nil, True);
//  if not Result then
//    Exit;
//Exit;
//  Result := LoadModuleSymInfo(AModule);
end;

procedure TDebugInfoEngineDbgHelp.DoDeInitEngine;
begin
  inherited;

end;

function TDebugInfoEngineDbgHelp.GetDebugSourceLocationInfo(AAddr: Pointer;
  AInfo: PDebugSourceLocationInfo): LongBool;
const
  SymbolNameLength = 1000;
  SymbolSize = SizeOf(TImagehlpSymbol) + SymbolNameLength;
var
  Displacement: DWORD;
  Symbol: PImagehlpSymbol;
  SymbolName: PChar;
  ProcessHandle: THandle;
  Line: TImageHlpLine;
  S: string;
begin
  GetMem(Symbol, SymbolSize);
  try
    ProcessHandle := GetCurrentProcess;

    ZeroMemory(Symbol, SymbolSize);
    Symbol^.SizeOfStruct := SizeOf(TImageHlpSymbol);
    Symbol^.MaxNameLength := SymbolNameLength;
    Displacement := 0;

    Result := _SymGetSymFromAddr(ProcessHandle, DWORD(AAddr), @Displacement, Symbol);

    if Result then
    begin
      SymbolName := Symbol^.Name;
      S := UnDecorateSymbolName(SymbolName);
//      SetString(S, SymbolName, lstrlen(SymbolName));
      PasStrToSafeStr(S, @AInfo^.ProcedureName);
    end;
  finally
    FreeMem(Symbol);
  end;

  // line number is optional
  if Result and Assigned(_SymGetLineFromAddr) then
  begin
    ZeroMemory(@Line, SizeOf(Line));
    Line.SizeOfStruct := SizeOf(Line);
    Displacement := 0;

    if _SymGetLineFromAddr(ProcessHandle, DWORD(AAddr), @Displacement, Line) then
    begin
      AInfo^.LineNumber := Line.LineNumber;
    end;
    GetLastError;
  end;
end;

function TDebugInfoEngineDbgHelp.GetProcAddressesFromNames(AModule: HMODULE;
  AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer): Integer;
begin
  Result := _GetProcAddressesFromNames(AModule, AProcNames, ANameCount, AAddrs);
end;

class function TDebugInfoEngineDbgHelp._GetProcAddressesFromNames(
  AModule: HMODULE; AProcNames: PSafeString; ANameCount: Integer;
  AAddrs: PPointer): Integer;
const
  SymbolNameLength = 1000;
  SymbolSize = SizeOf(TImagehlpSymbol) + SymbolNameLength;
var
  I: Integer;
  Symbol: PImagehlpSymbol;
  ProcessHandle: THandle;
  lName: PSafeString;
begin
  Result := 0;

  GetMem(Symbol, SymbolSize);
  try
    ProcessHandle := GetCurrentProcess;

    lName := AProcNames;
    for I := 0 to ANameCount - 1 do
    begin
      ZeroMemory(Symbol, SymbolSize);
      Symbol^.SizeOfStruct := SizeOf(TImageHlpSymbol);
      Symbol^.MaxNameLength := SymbolNameLength;

      if _SymGetSymFromName(ProcessHandle, @lName^.Chars, Symbol) then
      begin
        PCardinal(Cardinal(AAddrs) + Cardinal(I) * SizeOf(Pointer))^ := Symbol^.Address;
        Inc(Result);
      end;

      Inc(lName);
    end;
  finally
    FreeMem(Symbol);
  end;
end;

{ TDebugInfoEngineManager }

constructor TDebugInfoEngineManager.Create;
begin
  inherited;

end;

destructor TDebugInfoEngineManager.Destroy;
begin
  FreeAndNil(FDebugInfoEngine);

  inherited;
end;

function TDebugInfoEngineManager.Init(AModule: HMODULE): Boolean;
  function CreateEngine(AClass: TDebugInfoEngineClass): Boolean;
  begin
    FDebugInfoEngine := AClass.Create;
    Result := FDebugInfoEngine.InitEngine(AModule);
    if not Result then
      FreeAndNil(FDebugInfoEngine);
  end;

  function CreateFromEngines(AClasses: array of TDebugInfoEngineClass): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(AClasses) to High(AClasses) do
    begin
      Result := CreateEngine(AClasses[I]);
      if Result then
        Exit;
    end;
  end;
begin
  FreeAndNil(FDebugInfoEngine);

  Result := CreateFromEngines([TDebugInfoEngineJCL, TDebugInfoEngineDbgHelp])
end;

procedure TDebugInfoEngineManager.DeInit;
begin

end;

{ TDenomoDLLService }

constructor TDenomoDLLService.Create;
begin
  inherited;

  FDisassembler := TDisAsm.Create;
end;

destructor TDenomoDLLService.Destroy;
begin
  FreeAndNil(FDisassembler);

  inherited;
end;

procedure TDenomoDLLService.GetCodeHook(out Obj);
begin
  Pointer(Obj) := Pointer(CodeHook.GetCodeHookIntf);
  IInterface(Obj)._AddRef;
end;

function TDenomoDLLService.GetDebugSourceLocationInfo(AAddr: Pointer;
  AInfo: PDebugSourceLocationInfo): LongBool;
begin
  FillChar(AInfo^, SizeOf(TDebugSourceLocationInfo), 0);

  if DebugInfoEngineManager = nil then
  begin
    Result := False;
    Exit;
  end;

  Result := DebugInfoEngineManager.DebugInfoEngine.GetDebugSourceLocationInfo(AAddr, AInfo);
end;

procedure TDenomoDLLService.GetProcAddressesFromNames(AModule: HMODULE;
  AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer);
var
  I: Integer;
begin
  for I := 0 to ANameCount - 1 do
  begin
    PPointer(Cardinal(AAddrs) + Cardinal(I) * SizeOf(Pointer))^ := nil;
  end;

  if DebugInfoEngineManager = nil then
    Exit;

  DebugInfoEngineManager.DebugInfoEngine.GetProcAddressesFromNames(AModule, AProcNames,
    ANameCount, AAddrs);
end;

function TDenomoDLLService.GetInstructionLength(ACode: PByte): Integer;
begin
  Disassembler.Disassemble(ACode, 1);

  Assert(Disassembler.Count = 1);

  Result := Disassembler.Instructions[0].Code.size;
end;

function TDenomoDLLService.GetModuleImageSize(AModule: HMODULE): Cardinal;
begin
  Result := PeMapImgSize(Pointer(AModule));
end;

procedure TDenomoDLLService.NotifyShutDown;
begin
  if Assigned(FOnShutDown) then
    FOnShutDown;
end;

procedure TDenomoDLLService.SetOnShutDown(AOnShutDown: TDllServiceShutDown);
begin
  FOnShutDown := AOnShutDown;
end;

procedure TDenomoDLLService.InitDenomoCPP(AInfo: PCPPInfo);
begin
  CPPInfo := AInfo;
  InitDll;
end;

procedure TDenomoDLLService.DeInitDenomoCPP;
begin
  // only do DeInitDll when the module is a DLL.
  if CPPInfo^.ModuleHandle <> GetModuleHandle(nil) then
    DeInitDll;
end;

initialization

finalization
  DenomoDLLService.NotifyShutDown;

end.

