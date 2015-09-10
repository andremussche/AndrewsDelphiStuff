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

The Original Code is LeakInspect.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit LeakInspect;

interface

uses
  SysUtils, Windows, Messages, Forms,
  DenomoUtils;

type
  IInterObjectInspectorService = interface
    function GetSessionID: Integer;
    function GetMemoryTypes: Cardinal;
    function OutputStrings(AType: Integer): Integer;
    function EditHookOption(AOption: PHookOption): Boolean;
    procedure BeforeExecuteCommand(ACommand: Integer);
    procedure AfterExecuteCommand(ACommand: Integer);

    procedure OnConnected(Sender: TObject);
  end;

  TInterObjectInspector = class(TInterObject)
  private
    FInspectorService: IInterObjectInspectorService;
  protected
    function GetMyWinClassName: string; override;
    function GetGuestWinClassName: string; override;
    function DoMoreLeakMsg(Msg: Integer; Param: Cardinal): Integer; override;
    procedure DoOnConnected; override;
    procedure DoInit; override;

    procedure DoEditHookOption;

    procedure ExecuteNonMsgCommand(ACommand: Integer);
  public
    procedure LoadHookOption(AOption: PHookOption);
    procedure SaveHookOption(AOption: PHookOption);

    procedure ExecuteCommand(ACommand: Integer);
    function CommandEnabled(ACommand: Integer): Boolean;
    
    property InspectorService: IInterObjectInspectorService read FInspectorService write FInspectorService;
  end;

const
  //IC -> Inspector Command
  IC_MIN = 1;
  IC_MemCount = IC_MIN;
  IC_IncrementalSessionLeakBegin = IC_MIN + 1;
  IC_IncrementalSessionLeakEnd = IC_MIN + 2;
  IC_EditHookOption = IC_MIN + 3;
  IC_ListMem = IC_MIN + 4;
  IC_MAX = IC_ListMem;
  IC_COUNT = IC_MAX - IC_MIN + 1;

  CommandMsgMap: array[0..IC_COUNT - 1] of array[0..1] of Integer = (
    ( IC_MemCount, SM_MemCount ),
    ( IC_IncrementalSessionLeakBegin, SM_IncrementalSessionLeakBegin ),
    ( IC_IncrementalSessionLeakEnd, SM_IncrementalSessionLeakEnd ),
    ( IC_EditHookOption, -1 ),
    ( IC_ListMem, SM_ListMem )
  );

var
  InterObject: TInterObjectInspector;

procedure InitInspector(AService: IInterObjectInspectorService);
procedure DeInitInspector;

implementation

procedure InitInspector(AService: IInterObjectInspectorService);
begin
  InterObject := TInterObjectInspector.Create;
  InterObject.InspectorService := AService;
  InterObject.Init(True);
end;

procedure DeInitInspector;
begin
  InterObject.DeInit;
  FreeAndNil(InterObject);
end;

{ TInterObjectInspector }

function TInterObjectInspector.DoMoreLeakMsg(Msg: Integer;
  Param: Cardinal): Integer;
begin
  Result := 0;
  case Msg of
    SM_OutputStrings:
    begin
      if InspectorService <> nil then
      begin
        Result := InspectorService.OutputStrings(GetShareMemory^.EnumData.StringOutputType);
        GetShareMemory^.EnumData.ResultValue := Result;
      end;
    end;
  end;
end;

procedure TInterObjectInspector.DoOnConnected;
begin
  inherited;

  InspectorService.OnConnected(Self);
end;

function TInterObjectInspector.GetGuestWinClassName: string;
begin
  Result := HostClassName;
end;

function TInterObjectInspector.GetMyWinClassName: string;
begin
  Result := InspectorClassName;
end;

procedure TInterObjectInspector.DoInit;
begin
  inherited;

end;

procedure TInterObjectInspector.SaveHookOption(AOption: PHookOption);
var
  lSR: PSharedRecord;
  lData: PEnumHookIndexParamedData;
begin
  lSR := ResetShareMemory;
  lData := @lSR^.EnumData;
  lData^.HookOption := AOption^;

  TalkToGuest(SM_SetHookOption, 0);
end;

procedure TInterObjectInspector.LoadHookOption(AOption: PHookOption);
var
  lSR: PSharedRecord;
  lData: PEnumHookIndexParamedData;
begin
  lSR := ResetShareMemory;
  lData := @lSR^.EnumData;

  TalkToGuest(SM_GetHookOption, 0);

  AOption^ := lData^.HookOption;
end;

function TInterObjectInspector.CommandEnabled(ACommand: Integer): Boolean;
begin
  Result := True;
end;

procedure TInterObjectInspector.ExecuteCommand(ACommand: Integer);
var
  lSR: PSharedRecord;
  lData: PEnumHookIndexParamedData;
  lMsg: Integer;
  I: Integer;
begin
  lMsg := -1;
  for I := 0 to IC_COUNT - 1 do
  begin
    if CommandMsgMap[I][0] = ACommand then
    begin
      lMsg := CommandMsgMap[I][1];
      Break;
    end;
  end;

  if lMsg < 0 then
  begin
    ExecuteNonMsgCommand(ACommand);
    Exit;
  end;

  lSR := ResetShareMemory;
  lData := @lSR^.EnumData;

  lData^.MatchMemTypes := InspectorService.GetMemoryTypes;
  lData.MatchGroupID := InspectorService.GetSessionID;
  lData^.GroupID := 1;

  InspectorService.BeforeExecuteCommand(ACommand);

  TalkToGuest(lMsg, 0);

  InspectorService.AfterExecuteCommand(ACommand);
end;

procedure TInterObjectInspector.DoEditHookOption;
var
  lOption: THookOption;
begin
  LoadHookOption(@lOption);
  if InspectorService.EditHookOption(@lOption) then
    SaveHookOption(@lOption);
end;

procedure TInterObjectInspector.ExecuteNonMsgCommand(ACommand: Integer);
begin
  InspectorService.BeforeExecuteCommand(ACommand);

  case ACommand of
    IC_EditHookOption:
      DoEditHookOption;
    else
      Assert(False);
  end;

  InspectorService.AfterExecuteCommand(ACommand);
end;

end.

