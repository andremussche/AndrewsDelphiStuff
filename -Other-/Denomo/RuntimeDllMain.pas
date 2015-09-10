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

The Original Code is RuntimeDllMain.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit RuntimeDllMain;

interface

uses
  SysUtils, Classes, Windows,
  JclDebug, JclTD32, JclPeImage,
  MiscUtils, DenomoUtils, DenomoMemHook,
  CodeHookIntf, CodeHook;

type
  TRuntimeDLLService = class(TInterfacedObject,
                             IDllService)
  private
    FOnShutDown: TDllServiceShutDown;
  public
    constructor Create;
    destructor Destroy; override;

    procedure NotifyShutDown;

    { IDllService }
    procedure GetCodeHook(out Obj: ICodeHook); stdcall;

    function GetDebugSourceLocationInfo(AAddr: Pointer;
      AInfo: PDebugSourceLocationInfo): LongBool; stdcall;
    procedure GetProcAddressesFromNames(AModule: HMODULE;
      AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer); stdcall;
    function GetModuleImageSize(AModule: HMODULE): Cardinal; stdcall;
    procedure SetOnShutDown(AOnShutDown: TDllServiceShutDown); stdcall;
  end;

function GetDllService: IDllService; stdcall;

implementation

var
  RuntimeDLLServiceObj: TRuntimeDLLService = nil;
  RuntimeDLLService: IDllService = nil;

function GetDllService: IDllService; stdcall;
begin
  Result := RuntimeDLLService;
end;

{ TRuntimeDLLService }

constructor TRuntimeDLLService.Create;
begin
  inherited;

end;

destructor TRuntimeDLLService.Destroy;
begin

  inherited;
end;

function TRuntimeDLLService.GetDebugSourceLocationInfo(AAddr: Pointer;
  AInfo: PDebugSourceLocationInfo): LongBool;
var
  lLocationInfo: TJclLocationInfo;
begin
  FillChar(AInfo^, SizeOf(TDebugSourceLocationInfo), 0);
  Result := GetLocationInfo(AAddr, lLocationInfo);

  if Result then
  begin
    AInfo^.UnitName := PasStrToSafeStr(lLocationInfo.UnitName);
    AInfo^.ProcedureName := PasStrToSafeStr(lLocationInfo.ProcedureName);
    AInfo^.LineNumber := lLocationInfo.LineNumber;
  end;

  lLocationInfo.UnitName := '';
  lLocationInfo.ProcedureName := '';
  lLocationInfo.SourceName := '';
  lLocationInfo.BinaryFileName := '';
end;

function TRuntimeDLLService.GetModuleImageSize(AModule: HMODULE): Cardinal;
begin
  Result := PeMapImgSize(Pointer(AModule));
end;

procedure TRuntimeDLLService.GetProcAddressesFromNames(AModule: HMODULE;
  AProcNames: PSafeString; ANameCount: Integer; AAddrs: PPointer); stdcall;
var
  lTD32: TJclPeBorTD32Image;
  lScaner: TJclTD32InfoScanner;
  lName: string;
  lProcNames: array of string;
  I, K: Integer;
  lCountToFind: Integer;
  lTempP: PPointer;
begin
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
end;

procedure TRuntimeDLLService.NotifyShutDown;
begin
  if Assigned(FOnShutDown) then
    FOnShutDown;
end;

procedure TRuntimeDLLService.SetOnShutDown(AOnShutDown: TDllServiceShutDown);
begin
  FOnShutDown := AOnShutDown;
end;

procedure TRuntimeDLLService.GetCodeHook(out Obj: ICodeHook); stdcall;
begin
  obj := CodeHook.GetCodeHookIntf;
//  Pointer(Obj) := Pointer(CodeHook.GetCodeHookIntf);
//  IInterface(Obj)._AddRef;
end;

initialization
  RuntimeDLLServiceObj := TRuntimeDLLService.Create;
  RuntimeDLLService := RuntimeDLLServiceObj;

finalization
  RuntimeDLLServiceObj.NotifyShutDown;
  RuntimeDLLService := nil;

end.

