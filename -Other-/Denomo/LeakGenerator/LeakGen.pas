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

The Original Code is LeakGen.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit LeakGen;

interface

uses
  SysUtils, Classes, Windows, Contnrs, SyncObjs,
  Controls, Forms, Dialogs, ActiveX;

const
  MaxChildObjectCount = 5;

  //ST -> Strategy Type
  ST_Memory = 0;
  ST_Object = 1;
  ST_MulThread = 2;
  ST_FreeOnNil = 3;
  ST_StringAndDynArray = 4;
  ST_GDI = 5;

type
  TLeakStrategy = class;
  TLeakStrategyManager = class;
  TLeakObject = class;
  TLeakObjectClass = class of TLeakObject;

  TLeakThreadSafeList = class(TThreadList)
  private
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount;
  end;

  TLeakGeneratorSession = class
  private
    FLeakedMemList: TLeakThreadSafeList;
    FLeakedObjectList: TLeakThreadSafeList;
  protected
    procedure Reset;

    property LeakedMemList: TLeakThreadSafeList read FLeakedMemList;
    property LeakedObjectList: TLeakThreadSafeList read FLeakedObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginSession;
    procedure EndSession;
  end;

  TLeakOutputString = procedure(const S: string) of object;

  TLeakGenerator = class
  private
    FLeakedMemList: TLeakThreadSafeList;
    FLeakedObjectList: TLeakThreadSafeList;
    FSession: TLeakGeneratorSession;
    FStrategyManager: TLeakStrategyManager;
    FOnOutputString: TLeakOutputString;
    FThreadedCriticalSection: TCriticalSection;
  protected
    function MemoryLeaked(AMemory: Pointer): Pointer;
    function ObjectLeaked(AObject: TObject): TObject;

    procedure MemoryFreed(AMemory: Pointer);
    procedure ObjectFreed(AObject: TObject);

    procedure BeforeLeak;
    procedure AfterLeak;

    procedure LeakMemory(ASize: Integer);
    procedure LeakObject(AClass: TLeakObjectClass);

    procedure PerformCustomizeStrategy(AStrategy: Integer);

    procedure OutputString(const S: string);

    property LeakedMemList: TLeakThreadSafeList read FLeakedMemList;
    property LeakedObjectList: TLeakThreadSafeList read FLeakedObjectList;
    property Session: TLeakGeneratorSession read FSession;
  public
    constructor Create;
    destructor Destroy; override;

    procedure PerformLeak(AStrategy: Integer);

    procedure EnterThreadLock;
    procedure LeaveThreadLock;

    property StrategyManager: TLeakStrategyManager read FStrategyManager;
    property OnOutputString: TLeakOutputString read FOnOutputString write FOnOutputString;
  end;

  TRecordLeakStrategyItem = packed record
    StrategyType: Integer;
    MinCount: Integer;
    MaxCount: Integer;
    Flags: Integer;
    //used by ST_MulThread
    DestStrategyName: string;

    case Integer of
      ST_Memory: (
        MinSize: Integer;
        MaxSize: Integer;
      );
      ST_Object: (
        MinDepth: Integer;
        MaxDepth: Integer;
      );
      ST_MulThread: (
        MinExecute: ShortInt;
        MaxExecute: ShortInt;
        MinThreads: ShortInt;
        MaxThreads: ShortInt;
      );
  end;
  PRecordLeakStrategyItem = ^TRecordLeakStrategyItem;

  TRecordLeakStrategy = packed record
    Name: string;
    Desc: string;
    Items: PRecordLeakStrategyItem;
    ItemCount: Integer;
  end;
  PRecordLeakStrategy = ^TRecordLeakStrategy;
  TRecordLeakStrategyArray = array of TRecordLeakStrategy;
  PRecordLeakStrategyArray = ^TRecordLeakStrategyArray;

  TLeakStrategyItem = class
  private
    FMinCount: Integer;
    FMaxCount: Integer;
    FStrategy: TLeakStrategy;
    FExecutedCount: Integer;
    procedure SetMaxCount(const Value: Integer);
    procedure SetMinCount(const Value: Integer);
  protected
    procedure DoExecuteOnce; virtual; abstract;
    procedure ExecuteCertainCount(ACount: Integer);

    property ExecutedCount: Integer read FExecutedCount;
  public
    constructor Create(AStrategy: TLeakStrategy); virtual;
    destructor Destroy; override;

    procedure Execute;

    procedure LoadFromRecord(ARecord: PRecordLeakStrategyItem); virtual;

    property MinCount: Integer read FMinCount write SetMinCount;
    property MaxCount: Integer read FMaxCount write SetMaxCount;

    property Strategy: TLeakStrategy read FStrategy;
  end;
  TLeakStrategyItemClass = class of TLeakStrategyItem;

  TLeakStrategyItemMemory = class(TLeakStrategyItem)
  private
    FMaxSize: Integer;
    FMinSize: Integer;
  protected
    procedure DoExecuteOnce; override;
  public
    constructor Create(AStrategy: TLeakStrategy); override;
    destructor Destroy; override;

    procedure LoadFromRecord(ARecord: PRecordLeakStrategyItem); override;

    property MinSize: Integer read FMinSize write FMinSize;
    property MaxSize: Integer read FMaxSize write FMaxSize;
  end;

  TLeakStrategyItemObject = class(TLeakStrategyItem)
  private
    FMinDepth: Integer;
    FMaxDepth: Integer;
  protected
    procedure DoExecuteOnce; override;
  public
    constructor Create(AStrategy: TLeakStrategy); override;
    destructor Destroy; override;

    procedure LoadFromRecord(ARecord: PRecordLeakStrategyItem); override;

    property MinDepth: Integer read FMinDepth write FMinDepth;
    property MaxDepth: Integer read FMaxDepth write FMaxDepth;
  end;

  TLeakStrategyItemMulThread = class;
  TLeakThread = class(TThread)
  private
    FOwnerStrategy: TLeakStrategyItemMulThread;
  protected
    procedure Execute; override;
  public
    property OwnerStrategy: TLeakStrategyItemMulThread read FOwnerStrategy write FOwnerStrategy;
  end;

  TLeakStrategyItemMulThread = class(TLeakStrategyItem)
  private
    FDestStrategyName: string;
    FDestStrategy: TLeakStrategy;
    FThreadCount: Integer;
    FMaxExecute: Integer;
    FMinExecute: Integer;
    FMinThreads: Integer;
    FMaxThreads: Integer;
  protected
    procedure DoExecuteOnce; override;

    procedure ThreadFinished(AThread: TLeakThread);

    property DestStrategy: TLeakStrategy read FDestStrategy;
  public
    constructor Create(AStrategy: TLeakStrategy); override;
    destructor Destroy; override;

    procedure LoadFromRecord(ARecord: PRecordLeakStrategyItem); override;

    //the name of strategy to really execute
    property DestStrategyName: string read FDestStrategyName write FDestStrategyName;
    property MinExecute: Integer read FMinExecute write FMinExecute;
    property MaxExecute: Integer read FMaxExecute write FMaxExecute;
    property MinThreads: Integer read FMinThreads write FMinThreads;
    property MaxThreads: Integer read FMaxThreads write FMaxThreads;
  end;

  ITestInterface = interface
    procedure StdcallNonVir(A, B, C, D: Integer); stdcall;
    procedure StdcallVir(A, B, C, D: Integer); stdcall;
    procedure CdeclNonVir(A, B, C, D: Integer); cdecl;
    procedure CdeclVir(A, B, C, D: Integer); cdecl;
    procedure RegisterNonVir(A, B, C, D: Integer);
    procedure RegisterVir(A, B, C, D: Integer);
  end;

  TTestIntfObject = class(TInterfacedObject, ITestInterface)
  protected
    procedure StdcallNonVir(A, B, C, D: Integer); stdcall;
    procedure StdcallVir(A, B, C, D: Integer); virtual; stdcall;
    procedure CdeclNonVir(A, B, C, D: Integer); cdecl;
    procedure CdeclVir(A, B, C, D: Integer); virtual; cdecl;
    procedure RegisterNonVir(A, B, C, D: Integer);
    procedure RegisterVir(A, B, C, D: Integer); virtual;
  end;

  TLeakStrategyItemFreeOnNil = class(TLeakStrategyItem)
  protected
    procedure DoExecuteOnce; override;
    procedure FirstFree(P: Pointer);
    procedure SecondFree(P: Pointer);
  public
    constructor Create(AStrategy: TLeakStrategy); override;
    destructor Destroy; override;
  end;

  TLeakStrategyItemStringAndDynArray = class(TLeakStrategyItem)
  protected
    procedure DoExecuteOnce; override;
  public
    constructor Create(AStrategy: TLeakStrategy); override;
    destructor Destroy; override;

    procedure LoadFromRecord(ARecord: PRecordLeakStrategyItem); override;
  end;

  TLeakStrategyItemGDI = class(TLeakStrategyItem)
  protected
    procedure DoExecuteLeak(AExecuteCount: Integer);
    procedure DoExecuteOnce; override;
  public
    constructor Create(AStrategy: TLeakStrategy); override;
    destructor Destroy; override;

    procedure LoadFromRecord(ARecord: PRecordLeakStrategyItem); override;
  end;

  TLeakStrategyItemList = class(TObjectList)
  private
    function GetItem(Index: Integer): TLeakStrategyItem;
  public
    constructor Create;
    destructor Destroy; override;

    property Items[Index: Integer]: TLeakStrategyItem read GetItem; default;
  end;

  TLeakStrategy = class
  private
    FStrategyItemList: TLeakStrategyItemList;
    FDesc: string;
    FName: string;
  protected
    property StrategyItemList: TLeakStrategyItemList read FStrategyItemList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute;

    procedure LoadFromRecord(ARecord: PRecordLeakStrategy);

    property Name: string read FName write FName;
    property Desc: string read FDesc write FDesc;
  end;

  TLeakStrategyList = class(TObjectList)
  private
    function GetItem(Index: Integer): TLeakStrategy;
  public
    constructor Create;
    destructor Destroy; override;

    function FindStrategyByName(const AName: string): TLeakStrategy;

    property Items[Index: Integer]: TLeakStrategy read GetItem; default;
  end;

  TLeakStrategyManager = class
  private
    FStrategyList: TLeakStrategyList;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromItemArray(const AArray: array of TRecordLeakStrategy;
      ACount: Integer);

    property StrategyList: TLeakStrategyList read FStrategyList;
  end;

  TLeakObject = class
  private
    FChildCount: Integer;
    FKeepChildLeaked: Boolean;
  protected
    FChildObjects: array[0..MaxChildObjectCount - 1] of TLeakObject;

    procedure ClearChildObject(AFree: Boolean);

    property ChildCount: Integer read FChildCount;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function AddChild(AClass: TLeakObjectClass): TLeakObject;

    property KeepChildLeaked: Boolean read FKeepChildLeaked write FKeepChildLeaked;
  end;

var
  LeakGenerator: TLeakGenerator = nil;

implementation

procedure InitRand;
begin
  Randomize;
end;

function GetRand(AMin, AMax: Integer): Integer;
begin
  if AMin = AMax then
    Result := AMin
  else
  begin
    Assert(AMin < AMax);
    Result := Random(MaxInt) mod (AMax - AMin + 1) + AMin;
    Assert((Result >= AMin) and (Result <= AMax));
  end;
end;

{ TLeakThreadSafeList }

constructor TLeakThreadSafeList.Create;
begin
  inherited;

  //for better perfomance
  Duplicates := dupAccept;
end;

destructor TLeakThreadSafeList.Destroy;
begin

  inherited;
end;

function TLeakThreadSafeList.GetCount: Integer;
var
  lList: TList;
begin
  lList := LockList;
  try
    Result := lList.Count;
  finally
    UnlockList;
  end;
end;

{ TLeakGeneratorSession }

constructor TLeakGeneratorSession.Create;
begin
  inherited;

  FLeakedMemList := TLeakThreadSafeList.Create;
  FLeakedObjectList := TLeakThreadSafeList.Create;
end;

destructor TLeakGeneratorSession.Destroy;
begin
  FreeAndNil(FLeakedObjectList);
  FreeAndNil(FLeakedMemList);

  inherited;
end;

procedure TLeakGeneratorSession.BeginSession;
begin
  Reset;
end;

procedure TLeakGeneratorSession.EndSession;
begin

end;

procedure TLeakGeneratorSession.Reset;
begin
  LeakedMemList.Clear;
  LeakedObjectList.Clear;
end;

{ TLeakGenerator }

constructor TLeakGenerator.Create;
begin
  inherited;

  FLeakedMemList := TLeakThreadSafeList.Create;
  FLeakedObjectList := TLeakThreadSafeList.Create;
  FSession := TLeakGeneratorSession.Create;
  FStrategyManager := TLeakStrategyManager.Create;
  FThreadedCriticalSection := TCriticalSection.Create;
end;

destructor TLeakGenerator.Destroy;
begin
  FreeAndNil(FThreadedCriticalSection);
  FreeAndNil(FStrategyManager);
  FreeAndNil(FSession);
  FreeAndNil(FLeakedObjectList);
  FreeAndNil(FLeakedMemList);

  inherited;
end;

procedure TLeakGenerator.BeforeLeak;
begin

end;

procedure TLeakGenerator.AfterLeak;
begin

end;

function TLeakGenerator.MemoryLeaked(AMemory: Pointer): Pointer;
begin
  LeakedMemList.Add(AMemory);
  Session.LeakedMemList.Add(AMemory);

  Result := AMemory;
end;

function TLeakGenerator.ObjectLeaked(AObject: TObject): TObject;
begin
  LeakedObjectList.Add(AObject);
  Session.LeakedObjectList.Add(AObject);

  Result := AObject;
end;

procedure TLeakGenerator.MemoryFreed(AMemory: Pointer);
begin
  LeakedMemList.Remove(AMemory);
  Session.LeakedMemList.Remove(AMemory);
end;

procedure TLeakGenerator.ObjectFreed(AObject: TObject);
begin
  LeakedObjectList.Remove(AObject);
  Session.LeakedObjectList.Remove(AObject);
end;

procedure TLeakGenerator.PerformLeak(AStrategy: Integer);
begin
  BeforeLeak;
  try
    Session.BeginSession;
    try
      if AStrategy < 0 then
        PerformCustomizeStrategy(AStrategy)
      else
      begin
        if AStrategy < StrategyManager.StrategyList.Count then
          StrategyManager.StrategyList[AStrategy].Execute
        else
          raise Exception.CreateFmt('Strategy %d is out of bound.', [AStrategy]);
      end;
      OutputString(Format('Totally %d objects and %d blocks are leaked.',
        [ Session.LeakedObjectList.Count, Session.LeakedMemList.Count ]));
    finally
      Session.EndSession;
    end;
  finally
    AfterLeak;
  end;
end;

procedure TLeakGenerator.LeakMemory(ASize: Integer);
var
  P: Pointer;
begin
  GetMem(P, ASize);
  MemoryLeaked(P);
end;

procedure TLeakGenerator.LeakObject(AClass: TLeakObjectClass);
begin
  AClass.Create;
end;

procedure TLeakGenerator.PerformCustomizeStrategy(AStrategy: Integer);
begin
  raise Exception.CreateFmt('Customize strategy of ID %d is not implemented yet.', [AStrategy]);
end;

procedure TLeakGenerator.OutputString(const S: string);
begin
  if Assigned(OnOutputString) then
  begin
    EnterThreadLock;
    try
      OnOutputString(S);
    finally
      LeaveThreadLock;
    end;
  end;
end;

procedure TLeakGenerator.EnterThreadLock;
begin
  FThreadedCriticalSection.Enter;
end;

procedure TLeakGenerator.LeaveThreadLock;
begin
  FThreadedCriticalSection.Leave;
end;

{ TLeakStrategyItem }

constructor TLeakStrategyItem.Create(AStrategy: TLeakStrategy);
begin
  inherited Create;

  FMinCount := 1;
  FMaxCount := 5;
end;

destructor TLeakStrategyItem.Destroy;
begin

  inherited;
end;

procedure TLeakStrategyItem.ExecuteCertainCount(ACount: Integer);
var
  I: Integer;
begin
  for I := ACount downto 1 do
    DoExecuteOnce;
end;

procedure TLeakStrategyItem.Execute;
var
  lCount: Integer;
begin
  lCount := GetRand(MinCount, MaxCount);
  ExecuteCertainCount(lCount shr 1);
  ExecuteCertainCount((lCount + 1) shr 1);

  Inc(FExecutedCount);
end;

procedure TLeakStrategyItem.LoadFromRecord(
  ARecord: PRecordLeakStrategyItem);
begin
  MinCount := ARecord^.MinCount;
  MaxCount := ARecord^.MaxCount;
end;

procedure TLeakStrategyItem.SetMaxCount(const Value: Integer);
begin
  FMaxCount := Value;
  if FMaxCount <= 0 then
    FMaxCount := 1;
end;

procedure TLeakStrategyItem.SetMinCount(const Value: Integer);
begin
  FMinCount := Value;
  if FMinCount <= 0 then
    FMinCount := 1;
end;

{ TLeakStrategyItemMemory }

constructor TLeakStrategyItemMemory.Create(AStrategy: TLeakStrategy);
begin
  inherited;

  FMinSize := 0;
  FMaxSize := 1024;
end;

destructor TLeakStrategyItemMemory.Destroy;
begin

  inherited;
end;

procedure TLeakStrategyItemMemory.DoExecuteOnce;
begin
  inherited;

  LeakGenerator.LeakMemory(GetRand(MinSize, MaxSize));
end;

procedure TLeakStrategyItemMemory.LoadFromRecord(
  ARecord: PRecordLeakStrategyItem);
begin
  inherited;

  MinSize := ARecord^.MinSize;
  MaxSize := ARecord^.MaxSize;
end;

{ TLeakStrategyItemObject }

constructor TLeakStrategyItemObject.Create(AStrategy: TLeakStrategy);
begin
  inherited;

  FMinDepth := 0;
  FMaxDepth := 2;
end;

destructor TLeakStrategyItemObject.Destroy;
begin

  inherited;
end;

procedure TLeakStrategyItemObject.DoExecuteOnce;
var
  lObj: TLeakObject;
  lDepth: Integer;

  procedure GoDeeper(AObj: TLeakObject; ACurDepth: Integer);
  var
    I: Integer;
    lCount: Integer;
  begin
    if ACurDepth <= 0 then
      Exit;
    lCount := GetRand(1, MaxChildObjectCount);
    for I := 0 to lCount - 1 do
      GoDeeper(AObj.AddChild(TLeakObject), ACurDepth - 1);
  end;
begin
  inherited;

  lObj := TLeakObject.Create;
  lDepth := GetRand(MinDepth, MaxDepth);
  GoDeeper(lObj, lDepth);
end;

procedure TLeakStrategyItemObject.LoadFromRecord(
  ARecord: PRecordLeakStrategyItem);
begin
  inherited;

  MinDepth := ARecord^.MinDepth;
  MaxDepth := ARecord^.MaxDepth;
end;

{ TLeakThread }

procedure TLeakThread.Execute;
var
  I: Integer;
  lCount: Integer;
begin
  lCount := GetRand(OwnerStrategy.MinExecute, OwnerStrategy.MaxExecute);

  for I := 1 to lCount do
  begin
    OwnerStrategy.DestStrategy.Execute;

    Sleep(GetRand(1, 30));
  end;

  OwnerStrategy.ThreadFinished(Self);
end;

{ TLeakStrategyItemMulThread }

constructor TLeakStrategyItemMulThread.Create(AStrategy: TLeakStrategy);
begin
  inherited;

end;

destructor TLeakStrategyItemMulThread.Destroy;
begin

  inherited;
end;

procedure TLeakStrategyItemMulThread.DoExecuteOnce;
var
  I: Integer;
  lThread: TLeakThread;
  lExit: Boolean;
begin
  inherited;

  Assert(FThreadCount = 0);
  Assert(DestStrategy <> nil);

  FThreadCount := GetRand(MinThreads, MaxThreads);

  LeakGenerator.OutputString(Format('%d threads are created.', [ FThreadCount ]));
  for I := 0 to FThreadCount - 1 do
  begin
    lThread := TLeakThread.Create(True);
    lThread.FreeOnTerminate := True;
    lThread.OwnerStrategy := Self;
    lThread.Resume;
  end;

  while True do
  begin
    LeakGenerator.EnterThreadLock;
    try
      lExit := FThreadCount = 0;
    finally
      LeakGenerator.LeaveThreadLock;
    end;
    if lExit then
      Break;
    Sleep(10);
  end;
end;

procedure TLeakStrategyItemMulThread.ThreadFinished(AThread: TLeakThread);
begin
  LeakGenerator.EnterThreadLock;
  try
    Assert(FThreadCount > 0);
    Dec(FThreadCount);
  finally
    LeakGenerator.LeaveThreadLock;
  end;
end;

procedure TLeakStrategyItemMulThread.LoadFromRecord(
  ARecord: PRecordLeakStrategyItem);
begin
  inherited;

  DestStrategyName := ARecord^.DestStrategyName;
  FDestStrategy := LeakGenerator.StrategyManager.StrategyList.FindStrategyByName(DestStrategyName);
  if FDestStrategy = nil then
    raise Exception.CreateFmt('Can not find dest strategy "%s".',
      [ FDestStrategyName ]);
  FMinExecute := ARecord^.MinExecute;
  FMaxExecute := ARecord^.MaxExecute;
  FMinThreads := ARecord^.MinThreads;
  FMaxThreads := ARecord^.MaxThreads;
end;

{ TLeakStrategyItemFreeOnNil }

constructor TLeakStrategyItemFreeOnNil.Create(AStrategy: TLeakStrategy);
begin
  inherited;

end;

destructor TLeakStrategyItemFreeOnNil.Destroy;
begin

  inherited;
end;

procedure TLeakStrategyItemFreeOnNil.DoExecuteOnce;
var
  P: Pointer;
  lObj: TTestIntfObject;
  lIntf: ITestInterface;
begin
  inherited;

  lObj := TTestIntfObject.Create;
  lIntf := lObj;
  lIntf.StdcallNonVir(1, 2, 3, 4);
lIntf._Release;
{  lIntf.StdcallVir(1, 2, 3, 4);
  lIntf.CdeclNonVir(1, 2, 3, 4);
  lIntf.CdeclVir(1, 2, 3, 4);
  lIntf.RegisterNonVir(1, 2, 3, 4);
  lIntf.RegisterVir(1, 2, 3, 4);
  lIntf._Release;}
  lObj.Free;

  GetMem(P, 10);
  FirstFree(P);
  ReallocMem(P, 5);
end;

procedure TLeakStrategyItemFreeOnNil.FirstFree(P: Pointer);
begin
  FreeMem(P);
end;

procedure TLeakStrategyItemFreeOnNil.SecondFree(P: Pointer);
begin
  FreeMem(P);
end;

{ TLeakStrategyItemString }

constructor TLeakStrategyItemStringAndDynArray.Create(AStrategy: TLeakStrategy);
begin
  inherited;

end;

destructor TLeakStrategyItemStringAndDynArray.Destroy;
begin

  inherited;
end;

type
  TStringRecord = record
    S1: string;
    S2: string;
    S3: string;
  end;
var
  AStringRecord: TStringRecord;
  ADynArray: array of array of Integer;
procedure TLeakStrategyItemStringAndDynArray.DoExecuteOnce;
var
  I: Integer;
begin
  inherited;

  ADynArray := nil;
  SetLength(ADynArray, 30);
//  for I := 0 to High(ADynArray) do
//    SetLength(ADynArray[I], I + 1);

  FillChar(AStringRecord, SizeOf(AStringRecord), 0);

  AStringRecord.S1 := 'abc';

  AStringRecord.S2 := '';
  for I := 1 to 10 do
    AStringRecord.S2 := AStringRecord.S2 + ' repeated ';
  AStringRecord.S2 := AStringRecord.S2 + AStringRecord.S2;

  AStringRecord.S3 := AStringRecord.S2;
  for I := 1 to 10 do
    SetLength(AStringRecord.S3, Length(AStringRecord.S3) - 1);

  AStringRecord.S3 := '';

  LeakGenerator.OutputString('Some strings and dynamic arrays are leaked');
  LeakGenerator.OutputString('Note the leak is fake and will only appear in a session.');
  LeakGenerator.OutputString('Such fake leak will be auto cleaned when program exits.');
end;

procedure TLeakStrategyItemStringAndDynArray.LoadFromRecord(
  ARecord: PRecordLeakStrategyItem);
begin
  inherited;

end;

{ TLeakStrategyItemGDI }

constructor TLeakStrategyItemGDI.Create(AStrategy: TLeakStrategy);
begin
  inherited;

end;

destructor TLeakStrategyItemGDI.Destroy;
begin

  inherited;
end;

procedure TLeakStrategyItemGDI.DoExecuteLeak(AExecuteCount: Integer);
var
  lBrush: LOGBRUSH;
begin
  case AExecuteCount mod 3 of
    0:
    begin
      CreatePen(PS_SOLID, 1, 0);
      LeakGenerator.OutputString('A pen object leaked.');
    end;

    1:
    begin
      lBrush.lbStyle := BS_SOLID;
      lBrush.lbColor := 0;
      lBrush.lbHatch := HS_VERTICAL;
      CreateBrushIndirect(lBrush);
      LeakGenerator.OutputString('A brush object leaked.');
    end;

    2:
    begin
      CreateRectRgn(1, 2, 3, 4);
      LeakGenerator.OutputString('A region object leaked.');
    end;
  end;
end;

procedure TLeakStrategyItemGDI.DoExecuteOnce;
begin
  inherited;

  DoExecuteLeak(0);
  DoExecuteLeak(1);
  DoExecuteLeak(2);
//  DoExecuteLeak(ExecutedCount);
end;

procedure TLeakStrategyItemGDI.LoadFromRecord(
  ARecord: PRecordLeakStrategyItem);
begin
  inherited;

end;

{ TLeakStrategyItemList }

constructor TLeakStrategyItemList.Create;
begin
  inherited Create(True);

end;

destructor TLeakStrategyItemList.Destroy;
begin

  inherited;
end;

function TLeakStrategyItemList.GetItem(Index: Integer): TLeakStrategyItem;
begin
  Result := TLeakStrategyItem(inherited Items[Index]);
end;

{ TLeakStrategy }

constructor TLeakStrategy.Create;
begin
  inherited;

  FStrategyItemList := TLeakStrategyItemList.Create;
end;

destructor TLeakStrategy.Destroy;
begin
  FreeAndNil(FStrategyItemList);

  inherited;
end;

procedure TLeakStrategy.Execute;
var
  I: Integer;
  lCount: Integer;
begin
  lCount := StrategyItemList.Count;
  for I := 0 to lCount - 1 do
    StrategyItemList[I].Execute;
end;

procedure TLeakStrategy.LoadFromRecord(
  ARecord: PRecordLeakStrategy);
var
  I: Integer;
  lItemObj: TLeakStrategyItem;
  lItemPtr: PRecordLeakStrategyItem;

  function SizeToDisplay(ASize: Integer): string;
  begin
    if ASize mod (1024 * 1024) = 0 then
      Result := IntToStr(ASize div (1024 * 1024)) + 'M'
    else
    begin
      if ASize mod 1024 = 0 then
        Result := IntToStr(ASize div 1024) + 'K'
      else
      begin
        Result := IntToStr(ASize);
      end;
    end;
  end;

  function DoFormatStr(const S: string): string;
  begin
    Result := S;
    if ARecord^.ItemCount <> 1 then
      Exit;

    Result := StringReplace(Result,
      '$minsize', SizeToDisplay(ARecord^.Items^.MinSize),
      [ rfReplaceAll, rfIgnoreCase ]);
    Result := StringReplace(Result,
      '$maxsize', SizeToDisplay(ARecord^.Items^.MaxSize),
      [ rfReplaceAll, rfIgnoreCase ]);
    Result := StringReplace(Result,
      '$mindepth', IntToStr(ARecord^.Items^.MinDepth),
      [ rfReplaceAll, rfIgnoreCase ]);
    Result := StringReplace(Result,
      '$maxdepth', IntToStr(ARecord^.Items^.MaxDepth),
      [ rfReplaceAll, rfIgnoreCase ]);
  end;
begin
  Name := DoFormatStr(ARecord^.Name);
  Desc := DoFormatStr(ARecord^.Desc);
  for I := 0 to ARecord^.ItemCount - 1 do
  begin
    lItemPtr := ARecord^.Items;
    Inc(lItemPtr, I);
    case lItemPtr^.StrategyType of
      ST_Memory:
        lItemObj := TLeakStrategyItemMemory.Create(Self);
      ST_Object:
        lItemObj := TLeakStrategyItemObject.Create(Self);
      ST_MulThread:
        lItemObj := TLeakStrategyItemMulThread.Create(Self);
      ST_FreeOnNil:
        lItemObj := TLeakStrategyItemFreeOnNil.Create(Self);
      ST_StringAndDynArray:
        lItemObj := TLeakStrategyItemStringAndDynArray.Create(Self);
      ST_GDI:
        lItemObj := TLeakStrategyItemGDI.Create(Self);
      else
        raise Exception.CreateFmt('Unknown strategy type %d', [lItemPtr^.StrategyType]);
    end;
    lItemObj.LoadFromRecord(lItemPtr);
    StrategyItemList.Add(lItemObj);
  end;
end;

{ TLeakStrategyList }

constructor TLeakStrategyList.Create;
begin
  inherited Create(True);

end;

destructor TLeakStrategyList.Destroy;
begin

  inherited;
end;

function TLeakStrategyList.FindStrategyByName(
  const AName: string): TLeakStrategy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Result := Items[I];
    if SameText(Result.Name, AName) then
      Exit;
  end;
  Result := nil;
end;

function TLeakStrategyList.GetItem(Index: Integer): TLeakStrategy;
begin
  Result := TLeakStrategy(inherited Items[Index]);
end;

{ TLeakStrategyManager }

constructor TLeakStrategyManager.Create;
begin
  inherited;

  FStrategyList := TLeakStrategyList.Create;
end;

destructor TLeakStrategyManager.Destroy;
begin
  FreeAndNil(FStrategyList);

  inherited;
end;

procedure TLeakStrategyManager.LoadFromItemArray(
  const AArray: array of TRecordLeakStrategy; ACount: Integer);
var
  I: Integer;
  lStrategy: TLeakStrategy;
  lStrategyPtr: PRecordLeakStrategy;
begin
  for I := 0 to ACount - 1 do
  begin
    lStrategyPtr := @AArray[I];
    lStrategy := TLeakStrategy.Create;
    lStrategy.LoadFromRecord(lStrategyPtr);
    StrategyList.Add(lStrategy);
  end;
end;

{ TLeakObject }

constructor TLeakObject.Create;
begin
  inherited;

  LeakGenerator.ObjectLeaked(Self);
end;

destructor TLeakObject.Destroy;
begin
  ClearChildObject(not KeepChildLeaked);
  LeakGenerator.ObjectFreed(Self);

  inherited;
end;

procedure TLeakObject.ClearChildObject(AFree: Boolean);
var
  I: Integer;
begin
  for I := 0 to ChildCount - 1 do
  begin
    if FChildObjects[I] <> nil then
    begin
      if AFree then
        FChildObjects[I].Free;
      FChildObjects[I] := nil;
    end;
  end;
  FChildCount := 0;
end;

function TLeakObject.AddChild(AClass: TLeakObjectClass): TLeakObject;
begin
  Assert(ChildCount < MaxChildObjectCount);
  Result := AClass.Create;
  FChildObjects[ChildCount] := Result;
  Inc(FChildCount);
end;

{ TTestIntfObject }

procedure TTestIntfObject.CdeclVir(A, B, C, D: Integer);
begin

end;

procedure TTestIntfObject.CdeclNonVir(A, B, C, D: Integer);
begin

end;

procedure TTestIntfObject.RegisterNonVir(A, B, C, D: Integer);
begin

end;

procedure TTestIntfObject.RegisterVir(A, B, C, D: Integer);
begin

end;

procedure TTestIntfObject.StdcallNonVir(A, B, C, D: Integer);
begin

end;

procedure TTestIntfObject.StdcallVir(A, B, C, D: Integer);
begin

end;

initialization
  LeakGenerator := TLeakGenerator.Create;

finalization
  FreeAndNil(LeakGenerator);

end.

