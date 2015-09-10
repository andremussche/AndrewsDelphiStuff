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

The Original Code is CodeHookIntf.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit CodeHookIntf;

interface

uses
  SysUtils, Windows;

type
  TCodeHookHandle = Pointer;

const
  MAX_EXTRA_PARAM_COUNT = 32;
  INVALID_CODEHOOK_HANDLE = TCodeHookHandle(0);

  //Hook Calling Convention
  HCC_REGISTER = 0; //used in Delphi
  HCC_STDCALL = 1;
  HCC_CDECL = 2;

  HCC_COUNT = 3;
  
  //Hook Error Code
  HEC_NONE = 0;
  HEC_FUNCTION_TOO_SMALL = 1;
  HEC_UNKNOWN_INSTRUCTION = 2;
  HEC_ADDRESS_CANNOT_WRITE = 3;
  HEC_INVALID_HANDLE = 4;
  HEC_INVALID_CC = 5;
  HEC_TOO_MANY_EXTRA_PARAMS = 6;
  HEC_EXTEND_INFO_UNAVAILABLE = 7;

  HEC_COUNT = 8;

type
  IErrorCodeHook = interface
    function GetErrorCode: Cardinal; stdcall;
    function GetErrorMessage(AMessage: PChar; AMessageLen: Integer): Integer; stdcall;
  end;

  IDirectCodeHook = interface(IErrorCodeHook)
    // error code: HEC_FUNCTION_TOO_SMALL, HEC_UNKNOWN_INSTRUCTION, HEC_ADDRESS_CANNOT_WRITE
    function Hook(ATarget, AHook: Pointer; APreviousMethod: Pointer): LongBool; stdcall;
    // error code: HEC_ADDRESS_CANNOT_WRITE
    function Unhook(ATarget: Pointer; APreviousMethod: Pointer): LongBool; stdcall;
    function AllocatePreviousMethodMemory: Pointer; stdcall;
    procedure FreePreviousMethodMemory(APreviousMethod: Pointer); stdcall;
  end;

  TCodeHookInfo = packed record
    Target: Pointer;
  	Hook: Pointer;
    PreviousMethod: Pointer;
    UserData: Pointer;

    //Extend info
    ExtendInfoAvailable: LongBool;
    TargetObject: Pointer;
	  ParamCount: Integer;
    TargetCallingConvention: Integer;
    HookCallingConvention: Integer;
  end;
  PCodeHookInfo = ^TCodeHookInfo;

  ICodeHookHelper = interface;

  TCodeHookParamAccessor = array[0 .. 65535] of Cardinal;
  PCodeHookParamAccessor = ^TCodeHookParamAccessor;

  ICodeHook = interface(IErrorCodeHook)
    procedure GetDirectCodeHook(out Obj); stdcall;
    procedure GetCodeHookHelper(out Obj); stdcall;
    procedure CreateCodeHook(out Obj); stdcall;

    procedure SetUserDataSize(ASize: Integer); stdcall;

    // error code: same as IDirectCodeHook.Hook
    function Hook(ATarget, AHook: Pointer): TCodeHookHandle; stdcall;
    // error code: same as IDirectCodeHook.Unhook
    function Unhook(AHandle: TCodeHookHandle): LongBool; stdcall;

    function GetHookInfo(AHandle: TCodeHookHandle; AInfo: PCodeHookInfo): LongBool; stdcall;
    function GetPreviousMethod(AHandle: TCodeHookHandle): Pointer; stdcall;
    function GetUserData(AHandle: TCodeHookHandle): Pointer; stdcall;

    // error code: same as IDirectCodeHook.Hook, and HEC_TOO_MANY_EXTRA_PARAMS
    function AdvancedHook(ATargetObject: Pointer;
      ATarget: Pointer; ATargetCallingConvention: Integer;
      AHook: Pointer; AHookCallingConvention: Integer;
      AParamCount: Integer;
      AExtraParams: PCardinal; AExtraParamCount: Integer;
      AFlags: Cardinal): TCodeHookHandle; stdcall;
    function AdvancedUnhook(AHandle: TCodeHookHandle): LongBool; stdcall;
    function AdvancedGetRealReturnAddress(AAddr: Pointer): Pointer; stdcall;

    function FindHookHandleFromTarget(ATarget: Pointer): TCodeHookHandle; stdcall;
    // error code: HEC_EXTEND_INFO_UNAVAILABLE
    function CallPreviousMethod(AHandle: TCodeHookHandle; AParams: Pointer): Cardinal; stdcall;
    function CallMethod(AObject: Pointer; AMethod: Pointer;
      AParams: PCardinal; AParamCount: Integer; ACallingConvention: Integer): Cardinal; stdcall;
  end;

  ICodeHookHelper = interface(IErrorCodeHook)
    procedure SetCallingConvention(ATargetCC: Integer; AHookCC: Integer); stdcall;

    function HookWithGlobalMethod(ATargetObject: Pointer; ATarget: Pointer; AHook: Pointer;
      AParamCount: Integer; AFlags: Cardinal): TCodeHookHandle; stdcall;
    function HookWithObjectMethod(ATargetObject: Pointer; AHookObject: Pointer;
      ATarget: Pointer; AHook: Pointer;
      AParamCount: Integer; AFlags: Cardinal): TCodeHookHandle; stdcall;
    function HookWithGlobalMethodExtra(ATargetObject: Pointer; ATarget: Pointer; AHook: Pointer;
      AParamCount: Integer;
      AExtraParams: PCardinal; AExtraParamCount: Integer;
      AFlags: Cardinal): TCodeHookHandle; stdcall;
    function HookWithObjectMethodExtra(ATargetObject: Pointer; AHookObject: Pointer;
      ATarget: Pointer; AHook: Pointer;
      AParamCount: Integer;
      AExtraParams: PCardinal; AExtraParamCount: Integer;
      AFlags: Cardinal): TCodeHookHandle; stdcall;

    function UnhookTarget(ATarget: Pointer): LongBool; stdcall;
    procedure UnhookAll; stdcall;
  end;

procedure InitCodeHookDLL(const ADllName: string = '');
procedure DeInitCodeHookDLL;
procedure GetCodeHook(out Obj); stdcall;
procedure _GetCodeHook(Obj: PPointer); stdcall;

function CodeHookCheckHandle(AHandle: TCodeHookHandle; ACodeHook: ICodeHook): TCodeHookHandle;

const
  HookCallInstSignature0 = $01ff0eeb;
  HookCallInstSignature1 = $BadeFece;
  HookCallInstSignature2 = $1999Face;

implementation

const DynDllName = 'CHook.dll';

type
  TGetCodeHookProc = procedure(out Obj); stdcall;

var
  DllInited: Boolean = False;
  AddrGetCodeHook: TGetCodeHookProc = nil;
  DllHandle: HMODULE = 0;

procedure InitCodeHookDLL(const ADllName: string);
var
  lDllName: string;

  procedure Error(const AMsg: string);
  begin
    raise Exception.Create(AMsg);
  end;
begin
  if not DllInited then
  begin
    DllInited := True;
    if ADllName <> '' then
      lDllName := ADllName
    else
      lDllName := DynDllName;
    DllHandle := LoadLibrary(PChar(lDllName));
    if DllHandle <> 0 then
    begin
      AddrGetCodeHook := FARPROC(GetProcAddress(DllHandle, 'GetCodeHook'));
      if @AddrGetCodeHook = nil then
        Error('Can''t find procedure ' + 'GetCodeHook' + ' in DLL ' + lDllName);
    end
    else
      Error('Can''t load code hook DLL ' + lDllName);
  end;
end;

procedure DeInitCodeHookDLL;
begin
  if DllInited then
  begin
    DllInited := False;
    if DllHandle <> 0 then
      FreeLibrary(DllHandle);
    DllHandle := 0;
  end;
end;

procedure GetCodeHook(out Obj); stdcall;
begin
  InitCodeHookDLL;

  if @AddrGetCodeHook <> nil then
    AddrGetCodeHook(Obj)
  else
    Pointer(Obj) := nil;
end;

procedure _GetCodeHook(Obj: PPointer); stdcall;
begin
  GetCodeHook(Obj^);
end;

function CodeHookCheckHandle(AHandle: TCodeHookHandle; ACodeHook: ICodeHook): TCodeHookHandle;
var
  lMsg: string;
begin
  Result := AHandle;

  if ACodeHook.GetErrorCode <> HEC_NONE then
  begin
    SetLength(lMsg, 1024);
    SetLength(lMsg, ACodeHook.GetErrorMessage(@lMsg[1], Length(lMsg)));
    raise Exception.Create('CodeHook failure:'#13#10
      + 'Message: ' + lMsg);
  end;
end;

end.

