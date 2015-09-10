unit RemoteControlMethods;

interface

uses
  SysUtils, Classes, Messages,
  Rtti, AppEvnts, SyncObjs,
  CommunicationTypes;

type
{$METHODINFO ON}
  TRemoteControlMethods = class(TComponent)
  private
    function Sync_ExecuteRemoteFunction(
      const aRemoteObjectTree: TObjectStructList;
      const aRemoteFunction: UnicodeString;
      const aArguments: TVariantList): TVariantList;
    function SearchComponent(aParent: TComponent;
      aRemoteObject: TObjectStruct): TComponent;
    function Sync_RemoteControlExists(
      const aRemoteObjectTree: TObjectStructList): Boolean;
    procedure Sync_PostRemoteMessage(const aRemoteObjectTree: TObjectStructList;
      const aMessage, aWParam, aLParam: Int64);
  protected
    FAppEvents: TApplicationEvents;
    FAppIsIdle: TEvent;
    procedure AppIdle(Sender: TObject; var Done: Boolean);
  public
    procedure  AfterConstruction;override;
    destructor Destroy;override;

    function  ExecuteRemoteFunction(const aRemoteObjectTree: TObjectStructList; const aRemoteFunction: UnicodeString; const aArguments: TVariantList; const aTimeout: integer = 0): TVariantList;
    procedure ExecuteRemoteFunction_Async(const aRemoteObjectTree: TObjectStructList; const aRemoteFunction: UnicodeString; const aArguments: TVariantList; const aTimeout: integer = 0);
    procedure PostRemoteMessage(const aRemoteObjectTree: TObjectStructList; const aMessage: Integer; const aWParam: Integer; const aLParam: Integer; const aTimeout: integer = 0);
    function  RemoteControlExists(const aRemoteObjectTree: TObjectStructList; const aTimeout: integer = 0): Boolean;
  end;
{$METHODINFO OFF}

  TSimpleMainformSync = class
  protected
    type
      PSynchronizeRecord = ^TSynchronizeRecord;
      TSynchronizeRecord = record
        FMethod   : TThreadMethod;
        FProcedure: TThreadProcedure;
      end;
  protected
    FAsyncMessageHandle: THandle;
    procedure WndProc(var aMsg: TMessage);
  public
    procedure Queue(aProc: TThreadProcedure);

    procedure AfterConstruction;override;
  end;

implementation

uses
  Windows, Controls, Forms;

const
  WM_ASYNC_EXECUTE = WM_USER + 1;

var
  _SimpleMainformSync: TSimpleMainformSync;
 RttiCache: TRttiContext;

function SimpleMainformSync: TSimpleMainformSync;
begin
  if _SimpleMainformSync = nil then
    _SimpleMainformSync := TSimpleMainformSync.Create; //todo: not threadsafe
  Result := _SimpleMainformSync;
end;

{ TRemoteControlMethods }

procedure TRemoteControlMethods.AfterConstruction;
begin
  inherited;

  FAppIsIdle := TEvent.Create;
  FAppIsIdle.ResetEvent;

  FAppEvents := TApplicationEvents.Create(nil);
  FAppEvents.OnIdle := Self.AppIdle;
end;

procedure TRemoteControlMethods.AppIdle(Sender: TObject; var Done: Boolean);
begin
  FAppIsIdle.SetEvent;
end;

destructor TRemoteControlMethods.Destroy;
begin
  FAppEvents.Free;
  FAppIsIdle.Free;
  inherited;
end;

function TRemoteControlMethods.ExecuteRemoteFunction(
  const aRemoteObjectTree: TObjectStructList; const aRemoteFunction: UnicodeString;
  const aArguments: TVariantList; const aTimeout: integer = 0): TVariantList;
var
  vResult: TVariantList;
begin
  if (aTimeout > 0) and
     (FAppIsIdle.WaitFor(aTimeout) = wrTimeout)
  then
    raise Exception.CreateFmt('GUI not idle after %d msec',[aTimeout]);

  TThread.Synchronize( TThread.CurrentThread,
    procedure
    begin
      vResult := Sync_ExecuteRemoteFunction(aRemoteObjectTree, aRemoteFunction, aArguments);
    end);
  Result := vResult;

  FAppIsIdle.ResetEvent;
end;

procedure TRemoteControlMethods.ExecuteRemoteFunction_Async(
  const aRemoteObjectTree: TObjectStructList; const aRemoteFunction: UnicodeString;
  const aArguments: TVariantList; const aTimeout: integer = 0);
begin
  if (aTimeout > 0) and
     (FAppIsIdle.WaitFor(aTimeout) = wrTimeout)
  then
    raise Exception.CreateFmt('GUI not idle after %d msec',[aTimeout]);

  //TThread.Queue( TThread.CurrentThread,   -> sometimes lock if you do a .queue direct after a .synchronize, so we made our own message based queue...
  SimpleMainformSync.Queue(
    procedure
    begin
//      try
        Sync_ExecuteRemoteFunction(aRemoteObjectTree, aRemoteFunction, aArguments);
//      finally
//        remoteObjectTree.Free;
//        arguments.Free;
//      end;
    end);

  FAppIsIdle.ResetEvent;
end;

procedure TRemoteControlMethods.PostRemoteMessage(
  const aRemoteObjectTree: TObjectStructList; const aMessage, aWParam,
  aLParam: Integer; const aTimeout: integer = 0);
begin
  if (aTimeout > 0) and
     (FAppIsIdle.WaitFor(aTimeout) = wrTimeout)
  then
    raise Exception.CreateFmt('GUI not idle after %d msec',[aTimeout]);

  TThread.Synchronize( TThread.CurrentThread,
    procedure
    begin
      Sync_PostRemoteMessage(aRemoteObjectTree, aMessage, aWParam, aLParam);
    end);

  FAppIsIdle.ResetEvent;
end;

function TRemoteControlMethods.RemoteControlExists(
  const aRemoteObjectTree: TObjectStructList; const aTimeout: integer = 0): Boolean;
var
  bResult: Boolean;
begin
  if (aTimeout > 0) and
     (FAppIsIdle.WaitFor(aTimeout) = wrTimeout)
  then
    raise Exception.CreateFmt('GUI not idle after %d msec',[aTimeout]);

  TThread.Synchronize( TThread.CurrentThread,
    procedure
    begin
       bResult := Sync_RemoteControlExists(aRemoteObjectTree);
    end);

  Result := bResult;
end;

function TRemoteControlMethods.SearchComponent(aParent: TComponent; aRemoteObject: TObjectStruct): TComponent;
var
  tempcomp: TComponent;
begin
  Result := aParent.FindComponent(aRemoteObject.ObjectName);

  //check topmost form (of same type)
  if (Result <> nil) then
  begin
    if (Result is TCustomForm) then
    begin
      if Screen.ActiveCustomForm is Result.ClassType then
        Result := Screen.ActiveCustomForm;
    end;
  end
  else //if Result = nil then
  begin
    for tempcomp in aParent do
    begin
      if tempcomp.ClassName = aRemoteObject.ObjectClass then
      begin
        if Result <> nil then
          Assert(False, '2 or more components found of same class!');
        Result := tempcomp;
      end;
      if Result = nil then
        Result := SearchComponent(tempcomp, aRemoteObject);
      if Result <> nil then
        Break;
    end;

    if (Result = nil) and
       (Screen.ActiveCustomForm <> nil) and
       ( (Screen.ActiveCustomForm.Name      = aRemoteObject.ObjectName) or
         (Screen.ActiveCustomForm.ClassName = aRemoteObject.ObjectClass) )
    then
      Result := Screen.ActiveCustomForm;
  end;
end;

function TRemoteControlMethods.Sync_ExecuteRemoteFunction(const aRemoteObjectTree: TObjectStructList;
                                                          const aRemoteFunction: UnicodeString;
                                                          const aArguments: TVariantList): TVariantList;
var
  childcomponent: TComponent;
  rttimethod: TRttiMethod;
  rttitype: TRttiType;
  rttiprop: TRttiProperty;
  i: Integer;
  remoteobject: TObjectStruct;
  args: array of TValue;
  tvResult, tvCast: TValue;
  sFullPath: string;
begin
  Result         := TVariantList.Create;
  childcomponent := Forms.Application;

  sFullPath := childcomponent.Name;
  for i := aRemoteObjectTree.Count - 1 downto 0 do
  begin
    remoteobject := aRemoteObjectTree.Objects[i];
    sFullPath    := sFullPath + '.' + remoteobject.ObjectName;
  end;

  for i := aRemoteObjectTree.Count - 1 downto 0 do
  //for remoteobject in aRemoteObjectTree.InnerArray do
  begin
    remoteobject := aRemoteObjectTree.Objects[i];
    if remoteobject = nil then Continue;
    if remoteobject.ObjectName = '' then Continue;   //= Forms.Application

    Assert(childcomponent <> nil);
    childcomponent := SearchComponent(childcomponent, remoteobject);
    if childcomponent = nil then
      raise Exception.CreateFmt('Remote error: cannot find object "%s" (full path = "%s")', [remoteobject.ObjectName, sFullPath]);
  end;

  Assert(childcomponent <> nil);
  rttitype := RttiCache.GetType( childcomponent.ClassType );
  Assert(rttitype <> nil);
  rttimethod := rttitype.GetMethod(aRemoteFunction);
  if rttimethod <> nil then
  begin
    SetLength(args, aArguments.Count);
    for i := 0 to aArguments.Count - 1 do
      args[i] := TValue.FromVariant( aArguments.Variants[i].Value );
    tvResult := rttimethod.Invoke(childcomponent, args);
  end
  else
  begin
    Assert( aArguments.Count <= 1);
    rttiprop := rttitype.GetProperty(aRemoteFunction);
    Assert(rttiprop <> nil);
    if aArguments.Count = 1 then
      rttiprop.SetValue(childcomponent, TValue.FromVariant( aArguments.Variants[0].Value ));
    tvResult := rttiprop.GetValue(childcomponent);
  end;

  if not tvResult.IsEmpty then
  begin
    if tvResult.TryCast(TypeInfo(Variant), tvCast) then
      Result.Add( tvCast.AsVariant )
    else if tvResult.TypeInfo = TypeInfo(Boolean) then
      Result.Add( tvResult.AsBoolean )
    else
      raise Exception.CreateFmt('Unsupported type: %s', [tvResult.TypeInfo.Name]);
//        Result := tvCast.AsOrdinal
//        Result := tvCast.AsInteger
//        Result := tvCast.AsExtended
//        Result := tvCast.AsInt64
//        Result := tvCast.AsString
//        Result := tvCast.AsCurrency
  end
end;

function TRemoteControlMethods.Sync_RemoteControlExists(
  const aRemoteObjectTree: TObjectStructList): Boolean;
var
  childcomponent: TComponent;
  i: Integer;
  remoteobject: TObjectStruct;
  sFullPath: string;
begin
  childcomponent := Forms.Application;
  sFullPath      := childcomponent.Name;
  for i := aRemoteObjectTree.Count - 1 downto 0 do
  begin
    remoteobject := aRemoteObjectTree.Objects[i];
    sFullPath    := sFullPath + '.' + remoteobject.ObjectName;
  end;

  for i := aRemoteObjectTree.Count - 1 downto 0 do
  begin
    remoteobject := aRemoteObjectTree.Objects[i];
    if remoteobject = nil then Continue;
    if remoteobject.ObjectName = '' then Continue;   //= Forms.Application
    if childcomponent = nil then Break;

    childcomponent := SearchComponent(childcomponent, remoteobject);
  end;

  Result := (childcomponent <> nil);
end;

procedure TRemoteControlMethods.Sync_PostRemoteMessage(
  const aRemoteObjectTree: TObjectStructList; const aMessage, aWParam, aLParam: Int64);
var
  childcomponent: TComponent;
  i: Integer;
  remoteobject: TObjectStruct;
  sFullPath: string;
begin
  remoteobject   := nil;
  childcomponent := Forms.Application;
  Assert(aRemoteObjectTree.Count > 0, 'Empty object list!');

  sFullPath      := childcomponent.Name;
  for i := aRemoteObjectTree.Count - 1 downto 0 do
  begin
    remoteobject := aRemoteObjectTree.Objects[i];
    sFullPath    := sFullPath + '.' + remoteobject.ObjectName;
  end;

  for i := aRemoteObjectTree.Count - 1 downto 0 do
  begin
    remoteobject := aRemoteObjectTree.Objects[i];
    if remoteobject = nil then Continue;
    if remoteobject.ObjectName = '' then Continue;   //= Forms.Application

    Assert(childcomponent <> nil);
    childcomponent := SearchComponent(childcomponent, remoteobject);
    if childcomponent = nil then
      raise Exception.CreateFmt('Remote error: cannot find object "%s" (full path = "%s")', [remoteobject.ObjectName, sFullPath]);
  end;

  if childcomponent.InheritsFrom(TWinControl) then
    PostMessage( (childcomponent as TWinControl).Handle, aMessage, aWParam, aLParam)
  else
    raise Exception.CreateFmt('Remote error: cannot send message to object "%s": is not a TWinControl (full path = "%s")', [remoteobject.ObjectName, sFullPath]);
end;

{ TSimpleMainformSync }

procedure TSimpleMainformSync.AfterConstruction;
begin
  inherited;
  TThread.Synchronize(nil,
    procedure
    begin
      //do this in mainthread!
      FAsyncMessageHandle  := Classes.AllocateHWnd(WndProc);
    end);
end;

procedure TSimpleMainformSync.Queue(aProc: TThreadProcedure);
var
  LSynchronize: PSynchronizeRecord;
begin
  New(LSynchronize);
  LSynchronize.FMethod := nil;
  LSynchronize.FProcedure := aProc;
  PostMessage(Self.FAsyncMessageHandle, WM_ASYNC_EXECUTE, WPARAM(LSynchronize), 0);
end;

procedure TSimpleMainformSync.WndProc(var aMsg: TMessage);
var
  LSynchronize: PSynchronizeRecord;
begin
  if FAsyncMessageHandle = 0 then Exit;

  if aMsg.Msg >= WM_USER then
  begin
    LSynchronize := PSynchronizeRecord(aMsg.WParam);
    if LSynchronize <> nil then
      try
        if Assigned(LSynchronize.FProcedure) then
          LSynchronize.FProcedure()
        else if Assigned(LSynchronize.FMethod) then
          LSynchronize.FMethod();
      finally
        Dispose(LSynchronize);
      end;
  end
  else
    aMsg.Result := DefWindowProc(FAsyncMessageHandle, aMsg.Msg, aMsg.WParam, aMsg.LParam);
end;

initialization
  RttiCache := TRttiContext.Create;

finalization
  RttiCache.Free;

end.

