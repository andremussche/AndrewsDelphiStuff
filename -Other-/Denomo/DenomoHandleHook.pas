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

The Original Code is DenomoHandleHook.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit DenomoHandleHook;

//force stack frame on
{$W+}

interface

uses
  Windows,
  MiscUtils,
  DenomoConfig, DenomoMemHook, CodeHookIntf;

const
  HandleSubType_GDI_Bitmap = 0;
  HandleSubType_GDI_Brush = 1;
  HandleSubType_GDI_Font = 2;
  HandleSubType_GDI_Pen = 3;
  HandleSubType_GDI_Region = 4;
  HandleSubType_GDI_DC = 5;
  HandleSubType_GDI_Palette = 6;
  HandleSubType_GDI_MetaFile = 7;

  HandleSubType_GDI_Count = 8;

  HandleSubTypeNames_GDI: array[0 .. HandleSubType_GDI_Count - 1] of string = (
    'Bitmap', 'Brush', 'Font', 'Pen', 'Region', 'DC', 'Palette', 'Meta file'
  );

  GDITypeMap: array[OBJ_PEN .. OBJ_ENHMETAFILE] of Integer = (
    HandleSubType_GDI_Pen, HandleSubType_GDI_Brush,
    HandleSubType_GDI_DC, HandleSubType_GDI_DC,
    HandleSubType_GDI_Palette, HandleSubType_GDI_Font,
    HandleSubType_GDI_Bitmap, HandleSubType_GDI_Region,
    HandleSubType_GDI_MetaFile, HandleSubType_GDI_DC,
    HandleSubType_GDI_Pen, HandleSubType_GDI_DC,
    HandleSubType_GDI_MetaFile
  );

type
  TFreeHandleMethodType = ( fhmtDeleteObject, fhmtDestroyCursor, fhmtDestroyIcon,
    fhmtDC, fhmtMetaFile );
  TFreeHandleMethodTypes = set of TFreeHandleMethodType;

  TProcHookRecord = packed record
    Target: Pointer;
    Hook: Pointer;
    OldFunc: Pointer;
  end;

  TProcHookInfo = packed record
    ParamCount: Integer;
    OldFunc: Pointer;
    ProcName: TSafeString;
    Target: Pointer;
    Code: array[0..128 - 1] of Byte;
  end;
  PProcHookInfo = ^TProcHookInfo;

  THandleProcHookInfo = packed record
    ProcName: TSafeString;
    HandleParamIndex: Integer;
    FreeHandleMethodTypes: TFreeHandleMethodTypes;
  end;
  PHandleProcHookInfo = ^THandleProcHookInfo;

  THandleBinaryTreeNode = packed record
    Handle: Cardinal;

    FreeHandleMethodTypes: TFreeHandleMethodTypes;

    HandleType: Byte;
    HandleSubType: Byte;
    GroupID: Byte;
    Freed: Byte;

    RefCount: Cardinal;

    AllocatorProcName: array[0 .. 63] of Char;

    StackTrace: array[0 .. 0] of Cardinal;
  end;
  PHandleBinaryTreeNode = ^THandleBinaryTreeNode;

  THandleHookEnumCallback = function(AData: PEnumHookIndexData;
    ANode: PHandleBinaryTreeNode): Boolean;

  THandleTypeGroup = class;
  THandleSubTypeGroup = class
  private
    FHandleSubType: Integer;
    FTypeGroup: THandleTypeGroup;
    FHandleTree: TBalancedBinaryTree;
  protected
    function CreateHandleHook(AHandle: TCodeHookHandle;
      AParams: PCardinal): Cardinal;

    property HandleTree: TBalancedBinaryTree read FHandleTree;
    property TypeGroup: THandleTypeGroup read FTypeGroup;
  public
    constructor Create(AHandleSubType: Integer);
    destructor Destroy; override;

    procedure HandleAllocated(AHandle: Cardinal;
      AHookInfo: PHandleProcHookInfo);

    procedure AddCreateHandleHookProc(AProc: Pointer;
      AParamCount: Integer; AFreeMethodTypes: TFreeHandleMethodTypes;
      const AProcName: string = ''); overload;
    procedure AddCreateHandleHookProc(const ADllName, AProcName: string;
      AParamCount: Integer; AFreeMethodTypes: TFreeHandleMethodTypes); overload;

    property HandleSubType: Integer read FHandleSubType;
  end;

  THandleTypeGroup = class
  private
    FHandleType: Integer;
    FSubTypes: array of THandleSubTypeGroup;
    FSubTypeCount: Integer;
    FHandleTree: TBalancedBinaryTree;
  protected
    procedure FreeHandleHook(AHandle: TCodeHookHandle;
      AParams: PCardinal);

    function NewSHGetFileInfoA(AHandle: TCodeHookHandle;
      AParams: PCardinal): Cardinal;
    function NewSHGetFileInfoW(AHandle: TCodeHookHandle;
      AParams: PCardinal): Cardinal;

    property HandleTree: TBalancedBinaryTree read FHandleTree;
    property SubTypeCount: Integer read FSubTypeCount;
  public
    constructor Create(AHandleType: Integer);
    destructor Destroy; override;

    function RequireSubTypeGroup(ASubType: Integer): THandleSubTypeGroup;

    procedure HandleFreed(AHandle: Cardinal; AHookInfo: PHandleProcHookInfo);

    procedure AddFreeHandleHookProc(AProc: Pointer;
      AMethodType: TFreeHandleMethodType;
      AParamCount: Integer = 1; AHandleParamIndex: Integer = 0;
      const AProcName: string = ''); overload;
    procedure AddFreeHandleHookProc(const ADllName, AProcName: string;
      AMethodType: TFreeHandleMethodType;
      AParamCount: Integer = 1; AHandleParamIndex: Integer = 0); overload;

    property HandleType: Integer read FHandleType;
  end;

  THandleHookManager = class
  private
    FTypeGroups: array of THandleTypeGroup;
    FTypeGroupCount: Integer;
    FHandleTrees: array of TBalancedBinaryTree;
    FCodeHook: ICodeHook;
    FCodeHookHelper: ICodeHookHelper;
  protected
    procedure RegisterHandleTypes;

    function RequireHandleTree(AType: Integer): TBalancedBinaryTree;

    function CompareBlock(Block1, Block2: Pointer): Integer;

    procedure IncreaseTypeGroups(ADelta: Integer);
    function Enumerator(AData: Pointer; AParam: Pointer): Boolean;

    property TypeGroupCount: Integer read FTypeGroupCount;
    property CodeHook: ICodeHook read FCodeHook;
    property CodeHookHelper: ICodeHookHelper read FCodeHookHelper;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure DeInit;

    function RequireTypeGroup(AType: Integer): THandleTypeGroup;

    procedure EnumHookIndex(ACallback: THandleHookEnumCallback;
      AData: PEnumHookIndexData);
  end;

procedure InitHandleHook;
procedure DeInitHandleHook;

var
  HandleHookManager: THandleHookManager = nil;

implementation

uses DenomoUtils, DenomoHost;

var
  HandleBinaryTreeNodeSize: Integer = 0;

var
  FakeWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TDenomoFakeWindow'
  );

{ Create and destroy a window before hooking to make the environment clean.
  Otherwise, seems the very first window (TApplication.CreateHandle)
  will cause a fake GDI (brush) leak which can't be freed in the
  application lifetime.
}
procedure CreateFakeWindow;
var
  H: HWND;
  lTempClass: TWndClass;
begin
  FakeWindowClass.lpfnWndProc := @DefWindowProc;
  if not GetClassInfo(HInstance, FakeWindowClass.lpszClassName, lTempClass) then
  begin
    FakeWindowClass.hInstance := HInstance;
    if Windows.RegisterClass(FakeWindowClass) = 0 then
      Exit;
  end;
  H := CreateWindow(FakeWindowClass.lpszClassName, 'abc',
    WS_POPUP or WS_CAPTION or WS_CLIPSIBLINGS or WS_SYSMENU
    or WS_MINIMIZEBOX,
    GetSystemMetrics(SM_CXSCREEN) div 2,
    GetSystemMetrics(SM_CYSCREEN) div 2,
    0, 0, 0, 0, HInstance, nil);
  if H <> 0 then
    DestroyWindow(H);
  UnregisterClass(FakeWindowClass.lpszClassName, HInstance);
end;

procedure MakeHandlesClean;
begin
  CreateFakeWindow;
end;

procedure InitHandleHook;
begin
  HandleBinaryTreeNodeSize := SizeOf(THandleBinaryTreeNode) + (HandleStackTraceDepth - 1) * SizeOf(Cardinal);

  MakeHandlesClean;

  HandleHookManager := THandleHookManager.Create;
  HandleHookManager.Init;
end;

procedure DeInitHandleHook;
begin
  if HandleHookManager = nil then
    Exit;

  HandleHookManager.DeInit;

  HandleHookManager.Free;
  HandleHookManager := nil;
end;

function GetDllProc(const ADllName,
  AProcName: string): Pointer;
var
  lModule: THandle;
begin
  Result := nil;
  lModule := GetModuleHandle(PChar(ADllName));
  if lModule = 0 then
    lModule := LoadLibrary(PChar(ADllName));
  if lModule <> 0 then
    Result := GetProcAddress(lModule, PChar(AProcName));

  if Result = nil then
  begin
    MessageBox(0, PChar('Can''t find function ' + AProcName + ' in dll ' + ADllName), '', MB_OK);
    Exit;
  end;
end;

{ THandleSubTypeGroup }

constructor THandleSubTypeGroup.Create(AHandleSubType: Integer);
begin
  inherited Create;

  FHandleSubType := AHandleSubType;
end;

destructor THandleSubTypeGroup.Destroy;
begin

  inherited;
end;

procedure THandleSubTypeGroup.HandleAllocated(AHandle: Cardinal;
  AHookInfo: PHandleProcHookInfo);
var
  lNode: PHandleBinaryTreeNode;
  lTempNode: THandleBinaryTreeNode;
  lType: Integer;
begin
  if AHandle = 0 then
    Exit;
  if AHandle = Cardinal(-1) then
    Exit;

  lTempNode.Handle := AHandle;

  lNode := HandleTree.Find(@lTempNode);
  if lNode <> nil then
  begin
    Assert(lNode^.RefCount > 0);
    Inc(lNode^.RefCount);
    Exit;
  end;

  BeginOriginalMM;
  try
    lNode := HandleTree.Add(@lTempNode);
  finally
    EndOriginalMM;
  end;
  Assert(lNode <> nil);

  lNode^.Handle := AHandle;
  lNode^.HandleType := TypeGroup.HandleType;
  lNode^.HandleSubType := HandleSubType;
  lNode^.RefCount := 1;

  if TypeGroup.HandleType = RESTYPE_GDI then
  begin
    lType := GetObjectType(AHandle);
    if (lType >= Low(GDITypeMap)) and (lType <= High(GDITypeMap)) then
      lNode^.HandleSubType := GDITypeMap[lType];
  end;

  lNode^.FreeHandleMethodTypes := AHookInfo^.FreeHandleMethodTypes;
  lNode^.Freed := 0;
  lNode^.GroupID := GetCurGroupID;
  lstrcpyn(@lNode^.AllocatorProcName[0], @AHookInfo^.ProcName.Chars[0],
    Length(lNode^.AllocatorProcName));

  GetFrameBasedStackTrace(@lNode^.StackTrace[0],
    HandleStackTraceDepth, 3);
end;

procedure THandleSubTypeGroup.AddCreateHandleHookProc(AProc: Pointer;
  AParamCount: Integer; AFreeMethodTypes: TFreeHandleMethodTypes;
  const AProcName: string);
var
  lInfo: PHandleProcHookInfo;
  lExtraParam: Cardinal;
begin
  lExtraParam := Cardinal(Self);
  lInfo :=
    PHandleProcHookInfo(
      HandleHookManager.CodeHook.GetUserData(
        CodeHookCheckHandle(
          HandleHookManager.CodeHook.AdvancedHook(
    nil, AProc, HCC_STDCALL,
    @THandleSubTypeGroup.CreateHandleHook, HCC_REGISTER,
    AParamCount, @lExtraParam, 1, 0
              ),
          HandleHookManager.CodeHook)));
  if lInfo <> nil then
  begin
    lInfo^.FreeHandleMethodTypes := AFreeMethodTypes;
    PasStrToSafeStr(AProcName, @lInfo^.ProcName);
  end;
end;

procedure THandleSubTypeGroup.AddCreateHandleHookProc(const ADllName,
  AProcName: string; AParamCount: Integer; AFreeMethodTypes: TFreeHandleMethodTypes);
var
  lProc: Pointer;
begin
  lProc := GetDllProc(ADllName, AProcName);
  if lProc = nil then
    Exit;
  AddCreateHandleHookProc(lProc, AParamCount, AFreeMethodTypes, AProcName);
end;

function THandleSubTypeGroup.CreateHandleHook(
  AHandle: TCodeHookHandle; AParams: PCardinal): Cardinal;
var
  lInfo: PHandleProcHookInfo;
  lLocked: Boolean;
begin
  EnterMemHookCritical;
  try
    lLocked := IsHookLocked(hlAllocateHandle);
    EnterHookLocks([ hlAllocateHandle ]);
    try
      lInfo := PHandleProcHookInfo(HandleHookManager.CodeHook.GetUserData(AHandle));
      Result := HandleHookManager.CodeHook.CallPreviousMethod(AHandle, AParams);

      if not lLocked then
        HandleAllocated(Result, lInfo);
    finally
      LeaveHookLocks([ hlAllocateHandle ]);
    end;
  finally
    LeaveMemHookCritical;
  end;
end;

{ THandleTypeGroup }

constructor THandleTypeGroup.Create(AHandleType: Integer);
begin
  inherited Create;

  FHandleTree := HandleHookManager.RequireHandleTree(AHandleType);
  FHandleType := AHandleType;

  SetLength(FSubTypes, 10);
  FSubTypeCount := 0;
end;

destructor THandleTypeGroup.Destroy;
var
  I: Integer;
begin
  for I := 0 to FSubTypeCount - 1 do
    FSubTypes[I].Free;
  SetLength(FSubTypes, 0);

  FHandleTree := nil;

  inherited;
end;

procedure THandleTypeGroup.HandleFreed(AHandle: Cardinal;
  AHookInfo: PHandleProcHookInfo);
var
  lNode: PHandleBinaryTreeNode;
  lTempNode: THandleBinaryTreeNode;
begin
  lTempNode.Handle := AHandle;
  lNode := HandleTree.Find(@lTempNode);
  if lNode <> nil then
  begin
    Dec(lNode^.RefCount);
    if lNode^.RefCount > 0 then
      Exit;

    if lNode^.Freed <> 0 then
    begin
      Exit;
    end;

    // to check whether the handle is freed by the corresponding function
    // not is not implemented yet
    {
    if AHookInfo^.FreeHandleMethodTypes * lNode^.FreeHandleMethodTypes
      <> AHookInfo^.FreeHandleMethodTypes then
    begin
    end;
    }

    BeginOriginalMM;
    try
      HandleTree.Remove(lNode);
    finally
      EndOriginalMM;
    end;

{    lNodeBuf^.Freed := 1;
    GetFrameBasedStackTrace(@lNodeBuf^.StackTrace[0],
      HandleStackTraceDepth, 2);
}
  end
  else
  begin

  end;
end;

function THandleTypeGroup.RequireSubTypeGroup(
  ASubType: Integer): THandleSubTypeGroup;
var
  I: Integer;
begin
  for I := 0 to FSubTypeCount - 1 do
  begin
    if FSubTypes[I].HandleSubType = ASubType then
    begin
      Result := FSubTypes[I];
      Exit;
    end;
  end;
  if FSubTypeCount = Length(FSubTypes) then
    SetLength(FSubTypes, Length(FSubTypes) + 10);
  Result := THandleSubTypeGroup.Create(ASubType);
  Result.FTypeGroup := Self;
  Result.FHandleTree := HandleTree;
  FSubTypes[FSubTypeCount] := Result;
  Inc(FSubTypeCount);
end;

procedure THandleTypeGroup.AddFreeHandleHookProc(AProc: Pointer;
  AMethodType: TFreeHandleMethodType;
  AParamCount: Integer; AHandleParamIndex: Integer;
  const AProcName: string);
var
  lInfo: PHandleProcHookInfo;
  lExtraParam: Cardinal;
begin
  lExtraParam := Cardinal(Self);
  lInfo := PHandleProcHookInfo(HandleHookManager.CodeHook.GetUserData(
    CodeHookCheckHandle(HandleHookManager.CodeHook.AdvancedHook(
    nil, AProc, HCC_STDCALL,
    @THandleTypeGroup.FreeHandleHook, HCC_REGISTER,
    AParamCount, @lExtraParam, 1, 0
  ), HandleHookManager.CodeHook)));
  if lInfo <> nil then
  begin
    lInfo^.HandleParamIndex := AHandleParamIndex;
    PasStrToSafeStr(AProcName, @lInfo^.ProcName);
  end;
end;

procedure THandleTypeGroup.AddFreeHandleHookProc(const ADllName, AProcName: string;
  AMethodType: TFreeHandleMethodType;
  AParamCount, AHandleParamIndex: Integer);
var
  lProc: Pointer;
begin
  lProc := GetDllProc(ADllName, AProcName);
  if lProc = nil then
    Exit;
  AddFreeHandleHookProc(lProc, AMethodType,
    AParamCount, AHandleParamIndex, AProcName);
end;

procedure THandleTypeGroup.FreeHandleHook(
  AHandle: TCodeHookHandle; AParams: PCardinal);
var
  lInfo: PHandleProcHookInfo;
  lHandle: Cardinal;
  lLocked: Boolean;
begin
  EnterMemHookCritical;
  try
    lLocked := IsHookLocked(hlFreeHandle);
    EnterHookLocks([ hlFreeHandle ]);
    try
      lInfo := PHandleProcHookInfo(HandleHookManager.CodeHook.GetUserData(AHandle));
      lHandle := PCardinal(Integer(AParams) + SizeOf(Integer) * lInfo^.HandleParamIndex)^;

      if not lLocked then //check to avoid free in a nested free handle function
        HandleFreed(lHandle, lInfo);

      HandleHookManager.CodeHook.CallPreviousMethod(AHandle, AParams);
    finally
      LeaveHookLocks([ hlFreeHandle ]);
    end;
  finally
    LeaveMemHookCritical;
  end;
end;

function THandleTypeGroup.NewSHGetFileInfoA(
  AHandle: TCodeHookHandle; AParams: PCardinal): Cardinal;
begin
  EnterMemHookCritical;
  try
    EnterHookLocks([ hlAllocateHandle ]);
    try
      Result := HandleHookManager.CodeHook.CallPreviousMethod(AHandle, AParams);
    finally
      LeaveHookLocks([ hlAllocateHandle ]);
    end;
  finally
    LeaveMemHookCritical;
  end;
end;

function THandleTypeGroup.NewSHGetFileInfoW(
  AHandle: TCodeHookHandle; AParams: PCardinal): Cardinal;
begin
  EnterMemHookCritical;
  try
    EnterHookLocks([ hlAllocateHandle ]);
    try
      Result := HandleHookManager.CodeHook.CallPreviousMethod(AHandle, AParams);
    finally
      LeaveHookLocks([ hlAllocateHandle ]);
    end;
  finally
    LeaveMemHookCritical;
  end;
end;

{ THandleHookManager }

constructor THandleHookManager.Create;
begin
  inherited;

  DllService.GetCodeHook(FCodeHook);
  FCodeHook.GetCodeHookHelper(FCodeHookHelper);
  CodeHook.SetUserDataSize(SizeOf(THandleProcHookInfo));

  IncreaseTypeGroups(10);
  FTypeGroupCount := 0;

  SetLength(FHandleTrees, 10);
  FillChar(FHandleTrees[0], SizeOf(Pointer) * Length(FHandleTrees), 0);
end;

destructor THandleHookManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FHandleTrees) - 1 do
  begin
    FHandleTrees[I].Free;
    FHandleTrees[I] := nil;
  end;
  SetLength(FHandleTrees, 0);

  for I := 0 to FTypeGroupCount - 1 do
    FTypeGroups[I].Free;
  SetLength(FTypeGroups, 0);
  FTypeGroupCount := 0;

  FCodeHookHelper := nil;
  FCodeHook := nil;

  inherited;
end;

function THandleHookManager.RequireTypeGroup(
  AType: Integer): THandleTypeGroup;
var
  I: Integer;
begin
  for I := 0 to FTypeGroupCount - 1 do
  begin
    if FTypeGroups[I].HandleType = AType then
    begin
      Result := FTypeGroups[I];
      Exit;
    end;
  end;
  if FTypeGroupCount = Length(FTypeGroups) then
    IncreaseTypeGroups(10);
  Result := THandleTypeGroup.Create(AType);
  FTypeGroups[FTypeGroupCount] := Result;
  Inc(FTypeGroupCount);
end;

procedure THandleHookManager.Init;
begin
  RegisterHandleTypes;
end;

procedure THandleHookManager.DeInit;
begin
  CodeHookHelper.UnhookAll;
end;

type
  TCallbackRecord = record
    TypeGroup: THandleTypeGroup;
    Callback: THandleHookEnumCallback;
    Data: PEnumHookIndexData;
  end;
  PCallbackRecord = ^TCallbackRecord;

function THandleHookManager.Enumerator(AData, AParam: Pointer): Boolean;
var
  lNode: PHandleBinaryTreeNode;
begin
  Result := True;
  lNode := PHandleBinaryTreeNode(AData);
  if lNode^.HandleType = RESTYPE_GDI then
  begin
    if GDIObjectIsStockObject(lNode^.Handle)
      or (not IsValidGDIObject(lNode^.Handle))
      or GDIObjectIsDeleted(lNode^.Handle)
      then
    begin
      Exit;
    end;
  end;
  if lNode^.Freed = 0 then
    PCallbackRecord(AParam).Callback(PCallbackRecord(AParam).Data, lNode);
end;

procedure THandleHookManager.EnumHookIndex(ACallback: THandleHookEnumCallback;
  AData: PEnumHookIndexData);
var
  I: Integer;
  lTypeGroup: THandleTypeGroup;
  lRec: TCallbackRecord;
begin
  lRec.Callback := ACallback;
  lRec.Data := AData;
  for I := 0 to FTypeGroupCount - 1 do
  begin
    lTypeGroup := FTypeGroups[I];
    lRec.TypeGroup := lTypeGroup;
    lTypeGroup.HandleTree.Enumerate(Enumerator, @lRec);
  end;
end;

function THandleHookManager.RequireHandleTree(
  AType: Integer): TBalancedBinaryTree;
var
  I: Integer;

  function CreateHandleTree: TBalancedBinaryTree;
  begin
    Result := TBalancedBinaryTree.Create(HandleBinaryTreeNodeSize);
    Result.Comparer := CompareBlock;
  end;
begin
  case AType of
    RESTYPE_GDI:
      I := 0;
    else
      I := Length(FHandleTrees) - 1;
  end;
  if FHandleTrees[I] = nil then
    FHandleTrees[I] := CreateHandleTree;
  Result := FHandleTrees[I];
end;

function THandleHookManager.CompareBlock(Block1, Block2: Pointer): Integer;
begin
  Result := 0;
  if PHandleBinaryTreeNode(Block1)^.Handle
    < PHandleBinaryTreeNode(Block2)^.Handle then
    Result := -1
  else
    if PHandleBinaryTreeNode(Block1)^.Handle
      > PHandleBinaryTreeNode(Block2)^.Handle then
      Result := 1;
end;

procedure THandleHookManager.IncreaseTypeGroups(ADelta: Integer);
var
  I: Integer;
  lLen: Integer;
begin
  lLen := Length(FTypeGroups);
  SetLength(FTypeGroups, lLen + ADelta);
  for I := lLen to lLen + ADelta - 1 do
    FTypeGroups[I] := nil;
end;

procedure THandleHookManager.RegisterHandleTypes;
const
  GDI32 = 'gdi32.dll';
  User32 = 'user32.dll';
  Shell32 = 'shell32.dll';
var
  lGroup: THandleTypeGroup;
  lSubGroup: THandleSubTypeGroup;
begin
  lGroup := RequireTypeGroup(RESTYPE_GDI);

  CodeHookHelper.SetCallingConvention(HCC_STDCALL, HCC_REGISTER);
  CodeHookCheckHandle(CodeHookHelper.HookWithObjectMethod(nil, lGroup, GetDllProc(Shell32, 'SHGetFileInfoA'),
    @THandleTypeGroup.NewSHGetFileInfoA, 5, 0), CodeHook);
  CodeHookCheckHandle(CodeHookHelper.HookWithObjectMethod(nil, lGroup, GetDllProc(Shell32, 'SHGetFileInfoW'),
    @THandleTypeGroup.NewSHGetFileInfoW, 5, 0), CodeHook);

  lSubGroup := lGroup.RequireSubTypeGroup(HandleSubType_GDI_Bitmap);
  lSubGroup.AddCreateHandleHookProc(User32, 'LoadBitmapA',            2,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(User32, 'LoadBitmapW',            2,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateBitmap',           5,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateBitmapIndirect',   1,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateCompatibleBitmap', 3,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateDIBitmap',         6,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateDIBSection',       6,  [ fhmtDeleteObject ]);

  lSubGroup := lGroup.RequireSubTypeGroup(HandleSubType_GDI_Brush);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateBrushIndirect',    1,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateSolidBrush',       1,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreatePatternBrush',     1,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateDIBPatternBrush',  2,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateDIBPatternBrushPt',2,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateHatchBrush',       2,  [ fhmtDeleteObject ]);

  lSubGroup := lGroup.RequireSubTypeGroup(HandleSubType_GDI_Font);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateFontA',             14, [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateFontW',             14, [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateFontIndirectA',     1,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateFontIndirectW',     1,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateFontIndirectExA',   1,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateFontIndirectExW',   1,  [ fhmtDeleteObject ]);

  lSubGroup := lGroup.RequireSubTypeGroup(HandleSubType_GDI_Pen);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreatePen',              3,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreatePenIndirect',      1,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'ExtCreatePen',           5,  [ fhmtDeleteObject ]);

  lSubGroup := lGroup.RequireSubTypeGroup(HandleSubType_GDI_Region);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateEllipticRgn',          4,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateEllipticRgnIndirect',  1,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreatePolygonRgn',           3,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreatePolyPolygonRgn',       4,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateRectRgn',              4,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateRectRgnIndirect',      1,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateRoundRectRgn',         6,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'ExtCreateRegion',            3,  [ fhmtDeleteObject ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'PathToRegion',  1,  [ fhmtDeleteObject ]);

  lSubGroup := lGroup.RequireSubTypeGroup(HandleSubType_GDI_DC);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateCompatibleDC',        1,  [ fhmtDC ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateDCA',                 4,  [ fhmtDC ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateDCW',                 4,  [ fhmtDC ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateICA',                 4,  [ fhmtDC ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateICW',                 4,  [ fhmtDC ]);
  lSubGroup.AddCreateHandleHookProc(User32,  'GetDC',                     1,  [ fhmtDC ]);
  lSubGroup.AddCreateHandleHookProc(User32,  'GetDCEx',                   3,  [ fhmtDC ]);
  lSubGroup.AddCreateHandleHookProc(User32,  'GetWindowDC',               1,  [ fhmtDC ]);

  lSubGroup := lGroup.RequireSubTypeGroup(HandleSubType_GDI_Palette);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreatePalette',      1,  [ fhmtDC ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateHalftonePalette',      1,  [ fhmtDC ]);

  lSubGroup := lGroup.RequireSubTypeGroup(HandleSubType_GDI_MetaFile);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateMetaFileA',      1,  [ fhmtMetaFile ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateMetaFileW',      1,  [ fhmtMetaFile ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateEnhMetaFileA',   4,  [ fhmtMetaFile ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'CreateEnhMetaFileW',   4,  [ fhmtMetaFile ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'GetMetaFileA',         1,  [ fhmtMetaFile ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'GetMetaFileW',         1,  [ fhmtMetaFile ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'GetEnhMetaFileA',      1,  [ fhmtMetaFile ]);
  lSubGroup.AddCreateHandleHookProc(GDI32,  'GetEnhMetaFileW',      1,  [ fhmtMetaFile ]);

  lGroup.AddFreeHandleHookProc(GDI32, 'DeleteObject', fhmtDeleteObject);
  lGroup.AddFreeHandleHookProc(GDI32, 'DeleteDC', fhmtDeleteObject);
  lGroup.AddFreeHandleHookProc(GDI32, 'DeleteMetaFile', fhmtDeleteObject);
  lGroup.AddFreeHandleHookProc(GDI32, 'DeleteEnhMetaFile', fhmtDeleteObject);
  lGroup.AddFreeHandleHookProc(GDI32, 'CloseMetaFile', fhmtDeleteObject);
  lGroup.AddFreeHandleHookProc(GDI32, 'CloseEnhMetaFile', fhmtDeleteObject);
  lGroup.AddFreeHandleHookProc(User32, 'ReleaseDC', fhmtDeleteObject, 2, 1);
end;

end.

