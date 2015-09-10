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

The Original Code is DenomoMemHook.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit DenomoMemHook;

{***********************************************************************
  WARNING:
  Don't include this unit directly.
  You only need to include Denomo.pas
***********************************************************************}

{$WARN SYMBOL_DEPRECATED OFF}

interface

{$include Denomo.inc}

uses
  Windows,
  MiscUtils, DenomoConfig, CodeHookIntf;

const
{***********************************************************************
  Any code in this file should not be modified unless
  you know exactly how this program works.
***********************************************************************}


  MEMTYPE_RAWGETMEM = 0;
  MEMTYPE_OBJECT = 1;
  MEMTYPE_STRING = 2;
  MEMTYPE_DYNARRAY = 3;
  RESTYPE_GDI = 4;

  RESTYPE_MASK = 1 shl RESTYPE_GDI;
  MEMTYPE_COUNT = 5;

  //Never change following two values.
  MemoryAlign = 8;
  MemoryAlignMask = MemoryAlign - 1;

type
  TMemThumb = packed record
    MemType: Byte;
    GroupID: Byte;
    NotUsed: Word;
    RawSize: Cardinal;
    //which object allocates this block?
    ObjectParent: Pointer;
    ClassInfo: TClass;
    StackTrace: array[0 .. 0] of Cardinal;
  end;
  PMemThumb = ^TMemThumb;

  //Used when KeepDetailedTrackOnFreedBlock is True
  TMemFreedThumb = packed record
    RealClassInfo: TClass;
    StackTrace: array[0 .. 0] of Cardinal;
  end;
  PMemFreedThumb = ^TMemFreedThumb;

  TEnumHookIndexData = packed record
    IndexPtr: Pointer;
    MemPtr: Pointer;
    MemSize: Cardinal;
    ThumbPtr: PMemThumb;
    EnumCounter: Integer;
    Freed: Boolean;
    Padding1: Byte;
    Padding2: Byte;
    Padding3: Byte;
  end;
  PEnumHookIndexData = ^TEnumHookIndexData;

  THookEnumCallback = function(AData: PEnumHookIndexData): Boolean;

  THookLock = ( hlGetMem, hlFreeMem, hlReallocMem,
    hlAllocateHandle, hlFreeHandle );
  THookLocks = set of THookLock;

  TEnumHookOption = ( ehoAllocated, ehoFreed );
  TEnumHookOptions = set of TEnumHookOption;

  TRefLengthRecord = packed record
    RefCount: Cardinal;
    Length: Cardinal;
  end;
  PRefLengthRecord = ^TRefLengthRecord;
  TRefLengthRecordString = TRefLengthRecord;
  PRefLengthRecordString = ^TRefLengthRecordString;
  TRefLengthRecordDynArray = TRefLengthRecord;
  PRefLengthRecordDynArray = ^TRefLengthRecordDynArray;

  TCPPInfo = packed record
    ModuleHandle: HMODULE;
    AddrGetMemBlockSize: Pointer;
    _new: Pointer;
    _delete: Pointer;
    _new_array: Pointer;
    _delete_array: Pointer;
    malloc: Pointer;
    calloc: Pointer;
    realloc: Pointer;
    free: Pointer;
    _expand: Pointer;
    _malloc_dbg: Pointer;
    _calloc_dbg: Pointer;
    _realloc_dbg: Pointer;
    _free_dbg: Pointer;
    _expand_dbg: Pointer;
    TObject_InitInstance: Pointer;
    GetMem: Pointer;
    FreeMem: Pointer;
    ReallocMem: Pointer;
  end;
  PCPPInfo = ^TCPPInfo;

procedure EnumHookIndex(ACallback: THookEnumCallback;
  AData: PEnumHookIndexData; AOptions: TEnumHookOptions = [ ehoAllocated ]);
function GetIndexAndThumb(P: Pointer;
  Index: PPointer; Thumb: PPointer): Boolean;

//return old group id
function SetCurGroupID(groupid: Integer): Integer;
//return current group id
function GetCurGroupID: Integer;
//return true if success
function IncCurGroupID: Boolean;
//return true if success
function DecCurGroupID: Boolean;

procedure InitLeakMemHook;
procedure DeInitLeakMemHook;

procedure EnterMemHookCritical;
procedure LeaveMemHookCritical;

procedure EnterHookLocks(ALocks: THookLocks);
procedure LeaveHookLocks(ALocks: THookLocks);
function IsHookLocked(AHookLock: THookLock): Boolean;

function IsValidIndex(AIndex: Pointer): Boolean;

procedure GetFrameBasedStackTrace(AStackTrace: PCardinal;
  AMaxDepth, ASkipFrames: Cardinal);
function GetEnabledMemTypes: Cardinal;

procedure BeginOriginalMM;
procedure EndOriginalMM;

const
  AllHookLocks = [ Low(THookLock)..High(THookLock) ];

var
  CPPInfo: PCPPInfo = nil;

implementation

uses
  DenomoUtils, DenomoHost, DenomoHandleHook;

const
  Level1Bits = 10;
  Level2Bits = 10;
  Level3Bits = 9;

  Level1Max = (1 shl (Level1Bits)) - 1;
  Level2Max = (1 shl (Level2Bits)) - 1;
  Level3Max = (1 shl (Level3Bits)) - 1;

  //used when KeepTrackOnFreedBlock is True
  MemFreedMask = Cardinal(1 shl 31);

  MemBlockSizeMask = $ffffffff and (not MemFreedMask) and (not MemoryAlignMask);

  FreedInterfaceMethodAddrOffsetMask = $80000000;

type
  THookProc = function(ATarget, AHook: Pointer): Pointer; stdcall;

  TInitInstance = function(AClass: TClass; Instance: Pointer): TObject;
  //hook the global _AfterConstruction function in system.pas
  //not TObject.AfterConstruction
  T_AfterConstruction = function(Instance: TObject): TObject;
  T_NewAnsiString = function(length: Longint): Pointer;
  TDynArraySetLength = procedure(var a: Pointer; typeInfo: Pointer; dimCnt: Longint; lengthVec: PLongint);

  TIndexType = packed record
    BlockSize: Cardinal;
  end;
  PIndexType = ^TIndexType;

  //must be thread safe
  TCallerEnvironment = record
    HookLockRef: array[THookLock] of Integer;
    ObjectParent: Pointer;
    ObjectParentDepth: Integer;
    InRealloc: Boolean;
    StackFramesToSkip: Integer;
  end;

  TObjectTrap = packed record
    MethodTable: array[vmtSelfPtr div SizeOf(Pointer) .. vmtDestroy div SizeOf(Pointer)] of Pointer;
    VMT: array[0 .. MaxVMTEntryCount - 1] of Pointer;
  end;

var
  LevelIndex: PPointer;
  DenomoInited: Boolean = False;

  OldGetMem: TGetMem;
  OldFreeMem: TFreeMem;
  OldReallocMem: TReallocMem;
  OldInitInstance: TInitInstance;

  Old_AfterConstruction: T_AfterConstruction;
  Old_NewAnsiString: T_NewAnsiString;
  OldDynArraySetLength: TDynArraySetLength;

  Real_AfterConstruction: Pointer = nil;
  Real_NewAnsiString: Pointer = nil;

  CriticalSection: TRTLCriticalSection;

  CurGroupID: Integer;

  //must be thread safe
  CallerEnvironment: TCallerEnvironment;

  MemFreedThumbSize: Integer;

  ReallocationNotInPlaceWarningHasShown: Boolean = False;
  DontFreeToKeepTrackOnFreedBlock: Boolean = False;

  MemThumbSize: Integer = 0;

  ObjectTrap: TObjectTrap;
  ObjectTrapClassName: ShortString = 'TrappedObject';
  ObjectTrapParent: TObject = nil;
  FreedInterfaceVMTList: TSortedList = nil;

procedure EnterMemHookCritical;
begin
  EnterCriticalSection(CriticalSection);
end;

procedure LeaveMemHookCritical;
begin
  LeaveCriticalSection(CriticalSection);
end;

procedure EnterHookLocks(ALocks: THookLocks);
var
  T: THookLock;
begin
  for T := Low(THookLock) to High(THookLock) do
  begin
    if T in ALocks then
    begin
      if CallerEnvironment.HookLockRef[T] < 0 then
        CallerEnvironment.HookLockRef[T] := 0;
      Inc(CallerEnvironment.HookLockRef[T]);
    end;
  end;
end;

procedure LeaveHookLocks(ALocks: THookLocks);
var
  T: THookLock;
begin
  for T := Low(THookLock) to High(THookLock) do
  begin
    if T in ALocks then
    begin
      Dec(CallerEnvironment.HookLockRef[T]);
      if CallerEnvironment.HookLockRef[T] < 0 then
        CallerEnvironment.HookLockRef[T] := 0;
    end;
  end;
end;

function IsHookLocked(AHookLock: THookLock): Boolean;
begin
  Result := CallerEnvironment.HookLockRef[AHookLock] > 0;
end;

function AllocLevelMem(ABits: Integer; ALevel3: Boolean): Pointer;
const
  RecordSize: array[Boolean] of Integer = (
    SizeOf(Pointer), SizeOf(TIndexType)
  );
var
  sz: Integer;
begin
  sz := (1 shl ABits) * RecordSize[ALevel3];
  Result := Pointer(VirtualAlloc(nil, sz, MEM_COMMIT, PAGE_READWRITE));
  if Result = nil then
  begin
    //not safe because following code will also allocate new memory.
    ErrorOrExit('Out of memory when allocating Denomo index memory.', 'Out of memory');
  end;

  SafeFillChar(Result^, sz, 0);
end;

procedure AllocLevel2;
asm
  push ecx
  push edx

  mov eax, Level2Bits
  mov edx, False
  call AllocLevelMem

  pop edx
  pop ecx
end;

procedure AllocLevel3;
asm
  push ecx
  push edx

  mov eax, Level3Bits
  mov edx, True
  call AllocLevelMem

  pop edx
  pop ecx
end;

//param: eax - the address to check
//       edx - 0 for not allocate (check only), others for allocation.
//return: The pointer to the index
function CheckAndAlloc(Addr: Pointer; AllocateIt: Boolean): Pointer;
asm
  push esi
  push ebx
  push edx
  push ecx
  mov ebx, eax
  mov esi, edx

  mov edx, ebx
  shr edx, Level2Bits + Level3Bits + 2
  shl edx, 2
  mov ecx, LevelIndex
  cmp dword ptr [ecx + edx], 0
  jnz @@L1
  or esi, esi
  jz @@NotAlloc
  call AllocLevel2
  mov dword ptr [ecx + edx], eax
@@L1:
  mov ecx, dword ptr [ecx + edx]

  mov edx, ebx
  shr edx, Level3Bits + 2
  and edx, Level2Max
  shl edx, 2
  cmp dword ptr [ecx + edx], 0
  jnz @@L2
  or esi, esi
  jz @@NotAlloc
  call AllocLevel3
  mov dword ptr [ecx + edx], eax
@@L2:
  mov eax, dword ptr [ecx + edx]

  mov edx, ebx
  shr edx, 2
  and edx, Level3Max
  shl edx, 2
  add eax, edx

  jmp @@Finish
@@NotAlloc:
  xor eax, eax
@@Finish:
  pop ecx
  pop edx
  pop ebx
  pop esi
end;

function GetIndexAndThumb(P: Pointer;
  Index: PPointer; Thumb: PPointer): Boolean;
var
  lIndexPtr: Pointer;
begin
  if P = nil then
  begin
    Result := False;
    Exit;
  end;

  lIndexPtr := CheckAndAlloc(P, False);

  Result := lIndexPtr <> nil;

  if Result then
  begin
    if Index <> nil then
      Index^ := lIndexPtr;
    if Thumb <> nil then
    begin
      if PInteger(lIndexPtr)^ <> 0 then
        Thumb^ := Pointer(Cardinal(P)
          + PIndexType(lIndexPtr)^.BlockSize and MemBlockSizeMask)
      else
        Thumb^ := nil;
    end;
  end
  else
  begin
    if Index <> nil then
      Index^ := nil;
    if Thumb <> nil then
      Thumb^ := nil;
  end;
end;

function SetCurGroupID(groupid: Integer): Integer;
begin
  Assert((groupid >= 0) and (groupid <= MaxGroupID));

  Result := CurGroupID;
  CurGroupID := groupid;
end;

function GetCurGroupID: Integer;
begin
  Result := CurGroupID;
end;

function IncCurGroupID: Boolean;
begin
  if CurGroupID < MaxGroupID then
  begin
    Inc(CurGroupID);
    Result := True;
  end
  else
    Result := False;
end;

function DecCurGroupID: Boolean;
begin
  if CurGroupID > 0 then
  begin
    Dec(CurGroupID);
    Result := True;
  end
  else
    Result := False;
end;

//eax -- pointer to stack trace array
//edx -- max depth of stack trace (the length of stack trace array)
//ecx -- frames to skip
procedure GetFrameBasedStackTrace(AStackTrace: PCardinal;
  AMaxDepth, ASkipFrames: Cardinal);
const
  HookCallInstSignature0 = $01ff0eeb;
  HookCallInstSignature1 = $BadeFece;
  HookCallInstSignature2 = $1999Face;
asm
  or edx, edx
  jz @@ret

  push ebx
  push esi
  push edi

  mov esi, fs:[4]
  sub esi, 3
  mov edi, ebp
  { now
    esi -- stack top
    edi -- current stack frame
    ebp -- stack bottom
  }

@@mainloop:
  push [edi + 4]
  pop [eax]
  mov edi, [edi]
  
  cmp edi, esi
  jnb @@over
  
  cmp edi, ebp
  jna @@over

  dec ecx
  jg @@mainloop

  push edx
  mov edx, [eax]
  or edx, edx
  jz @@checkhookover
  cmp [edx], HookCallInstSignature0
  jnz @@checkhookover
  cmp [edx + 4], HookCallInstSignature1
  jnz @@checkhookover
  cmp [edx + 8], HookCallInstSignature2
  jnz @@checkhookover
  mov edx, [edx + $c]
  mov [eax], edx
@@checkhookover:
  pop edx

  dec edx
  jle @@over
  add eax, 4
  jmp @@mainloop
@@over:

  dec edx
  jl @@nomore
  mov [eax], 0
  add eax, 4
  jmp @@over
@@nomore:
  
  pop edi
  pop esi
  pop ebx
@@ret:
end;

function GetEnabledMemTypes: Cardinal;
begin
  Result := (1 shl MEMTYPE_COUNT) - 1;

  if (not EnableMemTypeString) or (Real_NewAnsiString = nil) then
    Result := Result and (not (1 shl MEMTYPE_STRING));

  if not EnableMemTypeDynArray then
    Result := Result and (not (1 shl MEMTYPE_DYNARRAY));

  if InterModuleDebug then
  begin
    Result := Result and (not (1 shl MEMTYPE_STRING)) and (not (1 shl MEMTYPE_DYNARRAY));
    if CPPInfo^.TObject_InitInstance = nil then
      Result := Result and (not (1 shl MEMTYPE_OBJECT));
  end;
end;

procedure BeginOriginalMM;
begin
  EnterHookLocks([ hlGetMem, hlFreeMem, hlReallocMem ]);
end;

procedure EndOriginalMM;
begin
  LeaveHookLocks([ hlGetMem, hlFreeMem, hlReallocMem ]);
end;

function CalcClassInterfacePointerSize(AClass: TClass): Integer;
var
  lIntfTable: PInterfaceTable;
  lIntfEntry: PInterfaceEntry;
  I: Integer;
begin
  Result := 0;
  while AClass <> nil do
  begin
    lIntfTable := AClass.GetInterfaceTable;
    if lIntfTable <> nil then
    begin
      for I := 0 to lIntfTable.EntryCount-1 do
      begin
        lIntfEntry := @lIntfTable.Entries[I];
        if lIntfEntry^.VTable <> nil then
        begin
          if lIntfEntry^.IOffset > Result then
            Result := lIntfEntry^.IOffset;
        end;
      end;
    end;
    AClass := AClass.ClassParent;
  end;
end;

procedure FreedInterfaceMethod(AIntf: Pointer; AOffset: Integer;
  AMethod: Cardinal; AStdcallIntf: Pointer);
var
  lIntf: Pointer;
  lSelf: TObject;
  lFreedThumb: PMemFreedThumb;
  lIndex: PIndexType;
  P: PCardinal;
begin
  if (AOffset and 1) <> 0 then
    lIntf := AStdcallIntf
  else
    lIntf := AIntf;
  lSelf := TObject(Integer(lIntf) + (AOffset and not 1));
  lIndex := CheckAndAlloc(lSelf, False);
  if lIndex <> nil then
  begin
    lFreedThumb := PMemFreedThumb(Cardinal(lSelf) + (lIndex^.BlockSize and not MemFreedMask) - Cardinal(MemFreedThumbSize));
  end
  else
  begin
    lSelf := nil;
    lIntf := nil;
    lFreedThumb := nil;
  end;
  if ((AMethod and FreedInterfaceMethodAddrOffsetMask) <> 0) and (lFreedThumb <> nil) then
  begin
    if not IsBadReadPtr(lSelf, 4) then
    begin
      P := PCardinal(Integer(lFreedThumb^.RealClassInfo) + Integer(AMethod and not FreedInterfaceMethodAddrOffsetMask));
      if not IsBadReadPtr(P, 4) then
        AMethod := P^;
    end;
  end;
  AccessFreedInterface(lSelf, lIntf, AMethod, lFreedThumb);
end;

type
  TFreedInterfaceVMT = packed record
    VMT: array[0 .. MaxInterfaceVMTEntryCount - 1] of Pointer;
    ThunkCode: array[0 .. MaxInterfaceVMTEntryCount * 16 - 1] of Byte;
  end;
  PFreedInterfaceVMT = ^TFreedInterfaceVMT;

function BuildNewInterfaceVMT(AOldVMT: PPointer; ANewHandler: Pointer): PFreedInterfaceVMT;
var
  I: Integer;
  P: PByte;
  lOldThunk: PByte;

  function GetDestAddr(ARegisterCall: Boolean): Cardinal;
  begin
    Result := 0;

    // non virtual method
    if lOldThunk^ = $e9 then
    begin
      Inc(lOldThunk);
      Result := PInteger(lOldThunk)^ + (Integer(lOldThunk) + 4);
      Exit;
    end;

    // virtual method

    if ARegisterCall then
    begin
      // push eax
      if lOldThunk^ = $50 then
      begin
        Inc(lOldThunk);
        // mov eax, [eax]
        if PWord(lOldThunk)^ = $008b then
        begin
          Inc(lOldThunk, 2);
          if lOldThunk^ = $8b then
          begin
            Inc(lOldThunk);
            case lOldThunk^ of
              $00: // mov eax, [eax]
                Result := FreedInterfaceMethodAddrOffsetMask;
              $40: // mov eax, [eax + imm8]
                Result := FreedInterfaceMethodAddrOffsetMask or (PByte(Cardinal(lOldThunk) + 1)^);
              $80: // mov eax, [eax + imm32]
                Result := FreedInterfaceMethodAddrOffsetMask or (PCardinal(Cardinal(lOldThunk) + 1)^);
            end;
          end;
        end;
      end;
    end
    else
    begin
      // non-register call

      // mov eax, [esp + 4]
      if PCardinal(lOldThunk)^ = $0424448b then
      begin
        Inc(lOldThunk, 4);
        // mov eax, [eax]
        if PWord(lOldThunk)^ = $008b then
        begin
          Inc(lOldThunk, 2);

          if lOldThunk^ = $ff then
          begin
            Inc(lOldThunk);
            case lOldThunk^ of
              $20: // jmp [eax]
                Result := FreedInterfaceMethodAddrOffsetMask;
              $60: // jmp [eax + imm8]
                Result := FreedInterfaceMethodAddrOffsetMask or (PByte(Cardinal(lOldThunk) + 1)^);
              $a0: // jmp [eax + imm32]
                Result := FreedInterfaceMethodAddrOffsetMask or (PCardinal(Cardinal(lOldThunk) + 1)^);
            end;
          end;
        end;
      end;
    end;
  end;

  function BuildEntry: Boolean;
  var
    lOffset: Integer;
    lDestAddr: Cardinal;
  begin
    Result := False;
    lOffset := 0;

    // add [esp + 4], imm32
    if PCardinal(lOldThunk)^ = $04244481 then
    begin
      Inc(lOldThunk, 4);
      lOffset := PInteger(lOldThunk)^ + 1;
      Inc(lOldThunk, 4);
    end;

    if lOffset = 0 then
    begin
      // add [esp + 4], imm8
      if PCardinal(lOldThunk)^ = $04244483 then
      begin
        Inc(lOldThunk, 4);
        lOffset := PShortInt(lOldThunk)^ + 1;
        Inc(lOldThunk);
      end
    end;

    if lOffset = 0 then
    begin
      // add eax, imm32
      if PByte(lOldThunk)^ = $05 then
      begin
        Inc(lOldThunk);
        lOffset := PInteger(lOldThunk)^;
        Inc(lOldThunk, 4);
      end
    end;

    if lOffset = 0 then
    begin
      // add eax, imm8
      if PWord(lOldThunk)^ = $c083 then
      begin
        Inc(lOldThunk, 2);
        lOffset := PShortInt(lOldThunk)^;
        Inc(lOldThunk);
      end
    end;

    if lOffset = 0 then
      Exit;

    lDestAddr := GetDestAddr((lOffset and 1) = 0);

    // mov edx, imm32
    P^ := $ba;
    Inc(P);
    PInteger(P)^ := lOffset;
    Inc(P, 4);

    // mov ecx, imm32
    P^ := $b9;
    Inc(P);
    PCardinal(P)^ := lDestAddr;
    Inc(P, 4);

    // jmp imm32
    P^ := $e9;
    Inc(P);
    PInteger(P)^ := Integer(ANewHandler) - (Integer(P) + 4);
    Inc(P, 4);

    Result := True;
  end;
begin
  Result := SimpleAllocMem(SizeOf(TFreedInterfaceVMT));
  P := PByte(@Result^.ThunkCode);
  for I := 0 to MaxInterfaceVMTEntryCount - 1 do
  begin
    Result^.VMT[I] := P;
    lOldThunk := PByte(AOldVMT^);
    Inc(AOldVMT);
    if IsBadReadPtr(lOldThunk, 15) then
      Break;

    if not BuildEntry then
      Exit;
  end;
end;

procedure ResetFreedInterfaceVMT(AClass: TClass; AObj: Pointer);
var
  lIntfTable: PInterfaceTable;
  lIntfEntry: PInterfaceEntry;
  I: Integer;
  lNewVMT: Pointer;
  lIndex: Integer;
begin
  while AClass <> nil do
  begin
    lIntfTable := AClass.GetInterfaceTable;
    if lIntfTable <> nil then
    begin
      for I := 0 to lIntfTable.EntryCount-1 do
      begin
        lIntfEntry := @lIntfTable.Entries[I];
        if lIntfEntry^.VTable <> nil then
        begin
          lIndex := FreedInterfaceVMTList.FindItem(Cardinal(lIntfEntry^.VTable));
          if lIndex < 0 then
            lNewVMT := nil
          else
            lNewVMT := FreedInterfaceVMTList.Values[lIndex];
          if lNewVMT = nil then
          begin
            lNewVMT := BuildNewInterfaceVMT(lIntfEntry^.VTable, @FreedInterfaceMethod);
            FreedInterfaceVMTList.InsertItem(Cardinal(lIntfEntry^.VTable), lNewVMT);
          end;
          PInteger(@PChar(AObj)[lIntfEntry^.IOffset])^ := Integer(lNewVMT);
        end;
      end;
    end;
    AClass := AClass.ClassParent;
  end;
end;

function IsValidIndex(AIndex: Pointer): Boolean;
begin
  Result := (PIndexType(AIndex) <> nil) and (PIndexType(AIndex)^.BlockSize <> 0)
    and (
      (not KeepTrackOnFreedBlock)
      or ((PIndexType(AIndex)^.BlockSize and MemFreedMask) = 0)
    )
    ;
end;

function NewGetMem(Size: Integer): Pointer;
var
  lIndex: PIndexType;
  lThumb: PMemThumb;
  lHookLocked: Boolean;
  lRawSize: Integer;
begin
  EnterMemHookCritical;
  lHookLocked := IsHookLocked(hlGetMem);
  EnterHookLocks([ hlGetMem ]);
  try
    lRawSize := Size;

    if lHookLocked and (not CallerEnvironment.InRealloc) then
    begin
      Result := OldGetMem(Size);

      Exit;
    end;

    if KeepTrackOnFreedBlock then
    begin
      if DetectInterfaceMethodAccssOnFreedObject then
      begin
        if MemFreedThumbSize > MemThumbSize then
          Size := Size + MemFreedThumbSize - MemThumbSize;
      end
      else
        if Size < MemFreedThumbSize - MemThumbSize then
          Size := MemFreedThumbSize - MemThumbSize;
    end;

    if Size = 0 then
      Size := 1;

    Size := (Size + MemoryAlignMask) and (not MemoryAlignMask);

    Result := OldGetMem(Size + MemThumbSize);

    if Result = nil then
      Exit;

    lIndex := CheckAndAlloc(Result, True);
    Assert(lIndex <> nil);

    lIndex^.BlockSize := Size;
    lThumb := PMemThumb(Integer(Result) + Size);

    SafeFillChar(lThumb^, MemThumbSize, 0);

    lThumb^.MemType := MEMTYPE_RAWGETMEM;
    lThumb^.GroupID := CurGroupID;
    lThumb^.RawSize := lRawSize;

    if Real_AfterConstruction <> nil then
    begin
      if CallerEnvironment.ObjectParentDepth > 0 then
        lThumb^.ObjectParent := CallerEnvironment.ObjectParent;
    end;

    GetFrameBasedStackTrace(@lThumb^.StackTrace[0], StackTraceDepth,
      2 + CallerEnvironment.StackFramesToSkip);
  finally
    LeaveHookLocks([ hlGetMem ]);
    LeaveMemHookCritical;
  end;
end;

function NewFreeMem(P: Pointer): Integer;
var
  lIndex: PIndexType;
  lThumb: PMemThumb;
  lHookLocked: Boolean;

  //used when KeepTrackOnFreedBlock is True
  lNewPtr: Pointer;
  lNewSize: Cardinal;
  lIntfPointerSize: Integer;

  //used when KeepDetailedTrackOnFreedBlock is True
  lFreedThumb: PMemFreedThumb;

  lClassInfo: TClass;
  lMemType: Byte;
begin
  EnterMemHookCritical;
  lHookLocked := IsHookLocked(hlFreeMem);
  EnterHookLocks([ hlFreeMem ]);
  try
    if lHookLocked and (not CallerEnvironment.InRealloc) then
    begin
      Result := OldFreeMem(P);
      Exit;
    end;

    lIndex := CheckAndAlloc(P, False);
    if (lIndex <> nil) and (lIndex^.BlockSize <> 0) then
    begin
      if KeepTrackOnFreedBlock then
      begin
        if (lIndex^.BlockSize and MemFreedMask) <> 0 then
        begin
          EnterHookLocks(AllHookLocks);
          try
            lFreedThumb := PMemFreedThumb(Cardinal(P) + (lIndex^.BlockSize and not MemFreedMask) - Cardinal(MemFreedThumbSize));
            BlockFreedTwice(P, lFreedThumb);
          finally
            LeaveHookLocks(AllHookLocks);
          end;
          Result := 0;
          Exit;
        end
        else
        begin
          if GetIndexAndThumb(P, @lIndex, @lThumb) then
          begin
            lMemType := lThumb^.MemType;
            lClassInfo := lThumb^.ClassInfo;
          end
          else
          begin
            lMemType := MEMTYPE_RAWGETMEM;
            lClassInfo := nil;
          end;

          lNewSize := MemFreedThumbSize;
          if DetectInterfaceMethodAccssOnFreedObject and (lMemType = MEMTYPE_OBJECT) then
          begin
            lIntfPointerSize := CalcClassInterfacePointerSize(TClass(PInteger(P)^));
            lNewSize := lIntfPointerSize + MemFreedThumbSize + 4;
          end;

          Assert(lIndex^.BlockSize + Cardinal(MemThumbSize) >= lNewSize);

          if DontFreeToKeepTrackOnFreedBlock then
            lNewPtr := P
          else
            lNewPtr := OldReallocMem(P, lNewSize);

          if lNewPtr = P then
          begin
            // must update the BlockSize because OldReallocMem won't affect it.
            lIndex^.BlockSize := lNewSize;

            if KeepDetailedTrackOnFreedBlock then
            begin
              lFreedThumb := PMemFreedThumb(Cardinal(lNewPtr) + Cardinal(lNewSize) - Cardinal(MemFreedThumbSize));
              GetFrameBasedStackTrace(@lFreedThumb^.StackTrace,
                FreedStackTraceDepth, 2);
              lFreedThumb^.RealClassInfo := nil;
              if lMemType = MEMTYPE_OBJECT then
              begin
                if DetectInterfaceMethodAccssOnFreedObject then
                begin
                  ResetFreedInterfaceVMT(lClassInfo, P);
                end;

                if DetectVirtualMethodAccssOnFreedObject then
                begin
                  TClass(PInteger(lNewPtr)^) := TClass(@ObjectTrap.VMT[0]);
                  if KeepDetailedTrackOnFreedBlock then
                  begin
                    lFreedThumb^.RealClassInfo := lClassInfo;
                  end;
                end;
              end
              else
              begin
                TClass(PInteger(lNewPtr)^) := TClass(lMemType);
              end;
            end;
          end
          else
          begin
            //Fails to reallocate inplace, this mostly like happen in FastMM
            lIndex^.BlockSize := 0;
            lIndex := CheckAndAlloc(lNewPtr, False);

  {***********************************************************************
    If you stop here, means you are using a memory manager that may not
    reallocate smaller size in place, e.g, FastMM4 uses more complex
    algorithm and often doesn't reallocate smaller size in place.
    This may affect the accuracy of detection of wild pointer access.
  ***********************************************************************}
            if not PromptReallocationNotInPlace then
              ReallocationNotInPlaceWarningHasShown := True;

            if not ReallocationNotInPlaceWarningHasShown then
            begin
              EnterHookLocks(AllHookLocks);
              try
                MessageBox(0, MsgReallocateNotInPlaceWarning,
                  TitleReallocateNotInPlaceWarning, MB_OK);
              finally
                LeaveHookLocks(AllHookLocks);
              end;
              ReallocationNotInPlaceWarningHasShown := True;
            end;

            DontFreeToKeepTrackOnFreedBlock := True;
          end;

          lIndex^.BlockSize := lIndex^.BlockSize or MemFreedMask;

          Result := 0;
          Exit;
        end;
      end
      else
      begin
        lIndex^.BlockSize := 0;
      end;
    end;
    Result := OldFreeMem(P);
  finally
    LeaveHookLocks([ hlFreeMem ]);
    LeaveMemHookCritical;
  end;
end;

function NewReallocMem(P: Pointer; Size: Integer): Pointer;
var
  lIndex: PIndexType;
  lThumb, lThumbNew: PMemThumb;
  lFreedThumb: PMemFreedThumb;
  lSize: Integer;
  lSuccess: Boolean;
  lHookLocked: Boolean;
begin
  EnterMemHookCritical;
  lHookLocked := IsHookLocked(hlReallocMem);
  EnterHookLocks([ hlGetMem, hlFreeMem, hlReallocMem ]);
  try
    if lHookLocked then
      lSuccess := False
    else
      lSuccess := GetIndexAndThumb(P, @lIndex, @lThumb);

    if KeepTrackOnFreedBlock then
    begin
      if lSuccess and (lIndex <> nil) and ((lIndex^.BlockSize and MemFreedMask) <> 0) then
      begin
        EnterHookLocks(AllHookLocks);
        try
          lFreedThumb := PMemFreedThumb(Cardinal(P) + (lIndex^.BlockSize and not MemFreedMask) - Cardinal(MemFreedThumbSize));
          ReallocOnFreedBlock(P, lFreedThumb);
          Result := OldGetMem(Size);
        finally
          LeaveHookLocks(AllHookLocks);
        end;
        Exit;
      end;
    end;

    if lSuccess and IsValidIndex(lIndex) then
    begin
      lSize := lIndex^.BlockSize;
      lIndex^.BlockSize := 0;
      CallerEnvironment.InRealloc := True;
      try
        GetMem(Result, Size);
      finally
        CallerEnvironment.InRealloc := False;
      end;
      if lSize > Size then
        lSize := Size;
      Move(P^, Result^, lSize);

      GetIndexAndThumb(Result, @lIndex, @lThumbNew);
      Assert(IsValidIndex(lIndex));

      lThumbNew^.MemType := lThumb^.MemType;
      lThumbNew^.GroupID := lThumb^.GroupID;

      FreeMem(P);
    end
    else
      Result := OldReallocMem(P, Size);
  finally
    LeaveHookLocks([ hlGetMem, hlFreeMem, hlReallocMem ]);
    LeaveMemHookCritical;
  end;
end;

function NewInitInstance(AClass: TClass; Instance: Pointer): TObject;
var
  lThumb: PMemThumb;
  lSuccess: Boolean;
begin
  EnterMemHookCritical;
  try
    Result := OldInitInstance(AClass, Instance);

    lSuccess := GetIndexAndThumb(Instance, nil, @lThumb);

    //Is this object allocated without thumb information?
    //If so, do no more actions.
    if (not lSuccess) or (lThumb = nil) then
      Exit;

    lThumb^.MemType := MEMTYPE_OBJECT;
    lThumb^.ClassInfo := AClass;

    if Real_AfterConstruction <> nil then
    begin
      if CallerEnvironment.ObjectParentDepth = 0 then
      begin
        CallerEnvironment.ObjectParent := Result;
        lThumb^.ObjectParent := nil;
      end
      else
        lThumb^.ObjectParent := CallerEnvironment.ObjectParent;
      Inc(CallerEnvironment.ObjectParentDepth);
    end;
  finally
    LeaveMemHookCritical;
  end;
end;

function New_AfterConstruction(Instance: TObject): TObject;
var
  lThumb: PMemThumb;
  lSuccess: Boolean;
begin
  //Don't call the old function in the critical section
  //it may cause dead lock
  Result := Old_AfterConstruction(Instance);

  EnterMemHookCritical;
  try
    lSuccess := GetIndexAndThumb(Instance, nil, @lThumb);

    //Is this object allocated without thumb information?
    //If so, do no more actions.
    if (not lSuccess) or (lThumb = nil) then
      Exit;

    Assert(CallerEnvironment.ObjectParentDepth > 0);

    Dec(CallerEnvironment.ObjectParentDepth);
    if CallerEnvironment.ObjectParentDepth = 0 then
      CallerEnvironment.ObjectParent := nil;
  finally
    LeaveMemHookCritical;
  end;
end;

function New_NewAnsiString(length: Longint): Pointer;
var
  lThumb: PMemThumb;
  lSuccess: Boolean;
  lRawMem: Pointer;
begin
  EnterMemHookCritical;
  try
    CallerEnvironment.StackFramesToSkip := 1;
    Result := Old_NewAnsiString(length);

    if Result = nil then
      Exit;

    lRawMem := Pointer(Cardinal(Result) - SizeOf(TRefLengthRecordString));

    lSuccess := GetIndexAndThumb(lRawMem, nil, @lThumb);

    //Is this string allocated without thumb information?
    //If so, do no more actions.
    if (not lSuccess) or (lThumb = nil) then
      Exit;

    lThumb^.MemType := MEMTYPE_STRING;
  finally
    CallerEnvironment.StackFramesToSkip := 0;
    LeaveMemHookCritical;
  end;
end;

procedure NewDynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: Longint; lengthVec: PLongint);
var
  lThumb: PMemThumb;
  lSuccess: Boolean;
  lRawMem: Pointer;
begin
  EnterMemHookCritical;
  try
    CallerEnvironment.StackFramesToSkip := 2;

    OldDynArraySetLength(a, typeInfo, dimCnt, lengthVec);

    if a = nil then
      Exit;

    lRawMem := Pointer(Integer(a) - SizeOf(TRefLengthRecordDynArray));

    lSuccess := GetIndexAndThumb(lRawMem, nil, @lThumb);

    //Is this dynamic array allocated without thumb information?
    //If so, do no more actions.
    if (not lSuccess) or (lThumb = nil) then
      Exit;

    lThumb^.MemType := MEMTYPE_DYNARRAY;
  finally
    CallerEnvironment.StackFramesToSkip := 0;
    LeaveMemHookCritical;
  end;
end;

procedure EnumHookIndex(ACallback: THookEnumCallback;
  AData: PEnumHookIndexData; AOptions: TEnumHookOptions);
var
	I, K, H: Integer;
  P, Q: Pointer;
  lData: PEnumHookIndexData;
  lSize: Cardinal;
begin
  EnterMemHookCritical;
  try
    lData := AData;
    lData^.EnumCounter := 0;
    for I := 0 to Level1Max do
    begin
      P := PPointer(Integer(LevelIndex) + (I shl 2))^;
      if P <> nil then
      begin
        for K := 0 to Level2Max do
        begin
          Q := PPointer(Integer(P) + (K shl 2))^;
          if Q <> nil then
          begin
            for H := 0 to Level3Max do
            begin
              lData^.IndexPtr := Pointer(Integer(Q) + (H shl 2));
              lSize := PIndexType(lData.IndexPtr)^.BlockSize;
              if (lSize <> 0)
                and (
                  (not KeepTrackOnFreedBlock)
                    or ((ehoAllocated in AOptions) and ((lSize and MemFreedMask) = 0)
                      or (ehoFreed in AOptions) and ((lSize and MemFreedMask) <> 0))
                )
                then
              begin
                lData^.MemPtr := Pointer((I shl (Level2Bits + Level3Bits)) + (K shl (Level3Bits)) + H);
                lData^.MemPtr := Pointer(Integer(lData.MemPtr) shl 2);

                lData^.ThumbPtr := PMemThumb(Cardinal(lData^.MemPtr)
                  + (lSize and MemBlockSizeMask));

                lData^.MemSize := lData^.ThumbPtr^.RawSize;

                if KeepTrackOnFreedBlock then
                  lData^.Freed := (lSize and MemFreedMask) <> 0
                else
                  lData^.Freed := False;

                if not ACallback(lData) then
                  Exit;
                Inc(lData^.EnumCounter);
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    LeaveMemHookCritical;
  end;
end;

function HookProc(ATarget, AHook: Pointer; out AOldFunc: Pointer;
  const AProcName: string): Boolean;
var
  lMsg: array[0 .. 2047] of Char;
  lHandle: TCodeHookHandle;
  lCodeHook: ICodeHook;
begin
  if ATarget = nil then
  begin
    Result := False;
    Exit;
  end;

  DllService.GetCodeHook(lCodeHook);
  if lCodeHook.FindHookHandleFromTarget(ATarget) <> INVALID_CODEHOOK_HANDLE then
  begin
    Result := False;
    Exit;
  end;

  lHandle := lCodeHook.Hook(ATarget, AHook);
  Result := lHandle <> INVALID_CODEHOOK_HANDLE;
  if Result then
    AOldFunc := lCodeHook.GetPreviousMethod(lHandle);

  if not Result then
  begin
{***********************************************************************
  If you stop here, means a function can't be hooked.
  Please send a bug report to the original author with the following
  information:
  1, The Delphi version you use
  2, The Denomo version you use.
***********************************************************************}
    EnterHookLocks(AllHookLocks);
    try
      lstrcpy(@lMsg[0], MsgHookFailError);
      lstrcat(@lMsg[0], PChar(AProcName));
      MessageBox(0, @lMsg[0], TitleHookFailError, MB_OK);
    finally
      LeaveHookLocks(AllHookLocks);
    end;
  end;
end;

function UnhookProc(ATarget: Pointer): Pointer;
var
  lCodeHook: ICodeHook;
begin
  Result := ATarget;
  if ATarget = nil then
    Exit;

  DllService.GetCodeHook(lCodeHook);
  lCodeHook.Unhook(lCodeHook.FindHookHandleFromTarget(ATarget));
end;

procedure GetProcAddressesFromNames;
const
  NameCount = 2;
  lNameStrings: array[0 .. NameCount - 1] of string = (
    '@System@@AfterConstruction',
    '@System@@NewAnsiString'
  );
var
  lNames: array[0 .. NameCount - 1] of TSafeString;
  lAddrs: array[0 .. NameCount - 1] of Pointer;
  I: Integer;
begin
  for I := 0 to NameCount - 1 do
    lNames[I] := PasStrToSafeStr(lNameStrings[I]);
  DllService.GetProcAddressesFromNames(GetModuleHandle(nil), @lNames[0], NameCount, @lAddrs[0]);

  Assert(lAddrs[0] <> nil);
  Assert(lAddrs[1] <> nil);

  Real_AfterConstruction := lAddrs[0];
  Real_NewAnsiString := lAddrs[1];
end;

var
  VirtualMethodThunk: Pointer = nil;

procedure SetObjectTrapMethod(AOffset: Integer; AValue: Pointer);
begin
  ObjectTrap.MethodTable[AOffset div SizeOf(Pointer)] := AValue;
end;

function IsFreedThumbObject(AFreedThumb: PMemFreedThumb): Boolean;
begin
  Result := Cardinal(AFreedThumb) > 65535;
end;

function SimpleIsValidClass(AClass: TClass): Boolean;
var
  lVEntryMatchCount: Integer;
  P: PByte;
  I: Integer;
  lCount: Integer;

  procedure CheckVirtualMethod(Offset: Integer; Addr: Pointer);
  begin
    if PPointer(Integer(AClass) + Offset)^ = Addr then
      Inc(lVEntryMatchCount);
  end;
begin
  Result := False;

  if (Integer(AClass) <= $ffff) or ((Integer(AClass) and 3) <> 0) then
    Exit;

  if IsBadReadPtr(Pointer(Integer(AClass) + vmtSelfPtr), -vmtSelfPtr + 8) then
    Exit;

  if PPointer(Integer(AClass) + vmtSelfPtr)^ <> AClass then
    Exit;

  lVEntryMatchCount := 0;
  asm
    mov eax, VMTOFFSET TObject.SafeCallException
    mov edx, offset TObject.SafeCallException
    push ebp
    call CheckVirtualMethod
    pop eax

    mov eax, VMTOFFSET TObject.AfterConstruction
    mov edx, offset TObject.AfterConstruction
    push ebp
    call CheckVirtualMethod
    pop eax

    mov eax, VMTOFFSET TObject.BeforeDestruction
    mov edx, offset TObject.BeforeDestruction
    push ebp
    call CheckVirtualMethod
    pop eax

    mov eax, VMTOFFSET TObject.Dispatch
    mov edx, offset TObject.Dispatch
    push ebp
    call CheckVirtualMethod
    pop eax

    mov eax, VMTOFFSET TObject.DefaultHandler
    mov edx, offset TObject.DefaultHandler
    push ebp
    call CheckVirtualMethod
    pop eax

    mov eax, VMTOFFSET TObject.NewInstance
    mov edx, offset TObject.NewInstance
    push ebp
    call CheckVirtualMethod
    pop eax

    mov eax, VMTOFFSET TObject.FreeInstance
    mov edx, offset TObject.FreeInstance
    push ebp
    call CheckVirtualMethod
    pop eax
  end;
  
  if lVEntryMatchCount = 0 then
    Exit;
    
  if lVEntryMatchCount >= 3 then
  begin
    Result := True;
    Exit;
  end;
  
  P := PPointer(Integer(AClass) + vmtClassName)^;
  if IsBadReadPtr(P, 256) then
  begin
    if IsBadReadPtr(P, 1) then
      Exit;
    lCount := P^;
    if IsBadReadPtr(P, 1 + lCount) then
      Exit;
  end;
  lCount := P^;
  Inc(P);
  
  for I := 0 to lCount - 1 do
  begin
    if not (P^ in [ $20, $7f ]) then
      Exit;
    Inc(P);
  end;
  
  Result := True;
end;

function IsValidClass(AClass: TClass): Boolean;
begin
  Result := SimpleIsValidClass(AClass);
  if not Result then
    Exit;
    
  AClass := AClass.ClassParent;
  if AClass = nil then
  begin
    Result := True;
    Exit;
  end;
  Result := SimpleIsValidClass(AClass);
  if not Result then
    Exit;
end;

function IsValidObject(AObj: TObject): Boolean;
begin
  Result := False;

  if (Cardinal(AObj) <= $ffff) or ((Integer(AObj) and 3) <> 0) then
    Exit;

  if IsBadReadPtr(AObj, 4) then
    Exit;

  Result := IsValidClass(PPointer(AObj)^);
end;

procedure GuessObjectType(ASelf: Pointer; var AObject: TObject; var AClass: TClass);
var
  lFreedThumb: PMemFreedThumb;
  lIndex: PIndexType;
begin
  AObject := nil;
  AClass := nil;

  if ASelf = @ObjectTrap.VMT[0] then
    AClass := TClass(@ObjectTrap.VMT[0])
  else
  begin
    lIndex := CheckAndAlloc(ASelf, False);
    if lIndex <> nil then
    begin
      if IsValidObject(TObject(ASelf)) then
      begin
        lFreedThumb := PMemFreedThumb(Cardinal(ASelf) + (lIndex^.BlockSize and not MemFreedMask) - Cardinal(MemFreedThumbSize));
        if IsFreedThumbObject(lFreedThumb) then
        begin
          AObject := TObject(ASelf);
          if KeepDetailedTrackOnFreedBlock then
            AClass := lFreedThumb^.RealClassInfo;
        end;
        Exit;
      end;
    end;

    // maybe a virtual method
    if IsValidClass(TClass(ASelf)) then
    begin
      AClass := TClass(ASelf);
    end;
  end;
end;

procedure FreedVirtualMethod(ASelf: Pointer; AIndex: Integer);
var
  lStdcallSelf: Pointer;
  lObject, lObjectStdcall: TObject;
  lClass, lClassStdcall: TClass;
  lFreedThumb: PMemFreedThumb;
  lIndex: PIndexType;
begin
  asm
    push [esp + 8]
    pop lStdcallSelf;
  end;
  lFreedThumb := nil;
  GuessObjectType(ASelf, lObject, lClass);
  if (lObject = nil) or (lClass = nil) then
  begin
    GuessObjectType(lStdcallSelf, lObjectStdcall, lClassStdcall);

    if ((lObject = nil) and (lObjectStdcall <> nil))
      or ((lClass = nil) and (lClassStdcall <> nil)) then
    begin
      lObject := lObjectStdcall;
      lClass := lClassStdcall;
    end;
  end;

  if lObject = nil then
  begin
  end
  else
  begin
    lIndex := CheckAndAlloc(lObject, False);
    if lIndex <> nil then
      lFreedThumb := PMemFreedThumb(Cardinal(lObject) + (lIndex^.BlockSize and not MemFreedMask) - Cardinal(MemFreedThumbSize))
    else
    begin
      lObject := nil;
      lClass := nil;
      lFreedThumb := nil;
    end;
  end;
  CallOnFreedObject(lObject, lClass, lFreedThumb);
end;

function InitVirtualMethodThunk(AHandler: Pointer; AVMT: PPointer; AStart: Integer): Pointer;
var
  P: PByte;
  I: Integer;
begin
  GetMem(Result, 5 + (10 * MaxVMTEntryCount));
  P := Result;

  Inc(AVMT, AStart);

  for I := AStart to MaxVMTEntryCount - 1 + AStart do
  begin
    if AVMT <> nil then
    begin
      AVMT^ := P;
      Inc(AVMT);
    end;

    // mov edx, index
    P^ := $ba;
    Inc(P);
    PInteger(P)^ := I;
    Inc(P, SizeOf(Integer));

    // jmp AHandler
    P^ := $e9;
    Inc(P);
    PInteger(P)^ := Integer(AHandler) - Integer(P) - 4;
    Inc(P, SizeOf(Integer));
  end;
end;

procedure InitFreedObjectVMT;
begin
  FillChar(ObjectTrap, SizeOf(ObjectTrap), 0);

  VirtualMethodThunk := InitVirtualMethodThunk(@FreedVirtualMethod, @ObjectTrap.VMT[0], vmtSafeCallException div 4);

  ObjectTrapParent := TObject.Create;
  SetObjectTrapMethod(vmtSelfPtr, @ObjectTrap.VMT);
  SetObjectTrapMethod(vmtClassName, @ObjectTrapClassName);
  SetObjectTrapMethod(vmtInstanceSize, Pointer(4));
  SetObjectTrapMethod(vmtParent, ObjectTrapParent);

  asm
    mov eax, VMTOFFSET TObject.SafeCallException
    mov edx, offset TObject.SafeCallException
    call SetObjectTrapMethod

    mov eax, VMTOFFSET TObject.AfterConstruction
    mov edx, offset TObject.AfterConstruction
    call SetObjectTrapMethod

    mov eax, VMTOFFSET TObject.BeforeDestruction
    mov edx, offset TObject.BeforeDestruction
    call SetObjectTrapMethod

    mov eax, VMTOFFSET TObject.Dispatch
    mov edx, offset TObject.Dispatch
    call SetObjectTrapMethod

    mov eax, VMTOFFSET TObject.DefaultHandler
    mov edx, offset TObject.DefaultHandler
    call SetObjectTrapMethod

    mov eax, VMTOFFSET TObject.NewInstance
    mov edx, offset TObject.NewInstance
    call SetObjectTrapMethod

    mov eax, VMTOFFSET TObject.FreeInstance
    mov edx, offset TObject.FreeInstance
    call SetObjectTrapMethod
  end;

  if DetectInterfaceMethodAccssOnFreedObject then
    FreedInterfaceVMTList := TSortedList.Create;
end;

procedure DeInitFreedObjectVMT;
begin
  FreeMem(VirtualMethodThunk);

  if DetectInterfaceMethodAccssOnFreedObject then
  begin
    FreedInterfaceVMTList.Clear(True);
    SimpleFreeAndNil(FreedInterfaceVMTList);
  end;
  SimpleFreeAndNil(ObjectTrapParent);
end;

procedure InitDelphi;
var
  lMM: TMemoryManager;
begin
  GetProcAddressesFromNames;

  GetMemoryManager(lMM);

  //start hooking
  EnterHookLocks(AllHookLocks);
  try
    HookProc(@lMM.GetMem, @NewGetMem, @OldGetMem, 'GetMem');
    HookProc(@lMM.FreeMem, @NewFreeMem, @OldFreeMem, 'FreeMem');
    HookProc(@lMM.ReallocMem, @NewReallocMem, @OldReallocMem, 'ReallocMem');

    HookProc(@TObject.InitInstance, @NewInitInstance, @OldInitInstance, 'TObject.InitInstance');

    if Real_AfterConstruction <> nil then
      HookProc(Real_AfterConstruction, @New_AfterConstruction, @Old_AfterConstruction, '_AfterConstruction');

    if not EnableMemTypeString then
      Real_NewAnsiString := nil;

    if Real_NewAnsiString <> nil then
      HookProc(Real_NewAnsiString, @New_NewAnsiString, @Old_NewAnsiString, 'NewAnsiString');

    if EnableMemTypeDynArray then
      HookProc(@DynArraySetLength, @NewDynArraySetLength, @OldDynArraySetLength, 'DynArraySetLength');
  finally
    LeaveHookLocks(AllHookLocks);
  end;
  //end hooking
end;

procedure DeInitDelphi;
var
  lMM: TMemoryManager;
begin
  GetMemoryManager(lMM);
  UnhookProc(@lMM.ReallocMem);
  UnhookProc(@lMM.GetMem);
  UnhookProc(@lMM.FreeMem);

  UnhookProc(@TObject.InitInstance);

  if Real_AfterConstruction <> nil then
    UnhookProc(Real_AfterConstruction);

  if EnableMemTypeString and (Real_NewAnsiString <> nil) then
    UnhookProc(Real_NewAnsiString);

  if EnableMemTypeDynArray then
    UnhookProc(@DynArraySetLength);
end;


//**************C++ related functions start************************************

function GetAllocateSize(ASize: Cardinal): Cardinal;
begin
  if ASize = 0 then
    ASize := 1;

  ASize := (ASize + MemoryAlignMask) and (not MemoryAlignMask);

  Result := ASize + Cardinal(MemThumbSize);
end;

procedure MemAllocated(AMem: Pointer; ARawSize, ANewSize: Integer);
var
  lIndex: PIndexType;
  lThumb: PMemThumb;
  lHookLocked: Boolean;
begin
  if AMem = nil then
    Exit;

  EnterMemHookCritical;
  lHookLocked := IsHookLocked(hlGetMem);
  EnterHookLocks([ hlGetMem ]);
  try
    if lHookLocked and (not CallerEnvironment.InRealloc) then
      Exit;

    Dec(ANewSize, MemThumbSize);
    lIndex := CheckAndAlloc(AMem, True);
    Assert(lIndex <> nil);

    lIndex^.BlockSize := ANewSize;
    lThumb := PMemThumb(Integer(AMem) + ANewSize);

    SafeFillChar(lThumb^, MemThumbSize, 0);

    lThumb^.MemType := MEMTYPE_RAWGETMEM;
    lThumb^.GroupID := CurGroupID;
    lThumb^.RawSize := ARawSize;

    GetFrameBasedStackTrace(@lThumb^.StackTrace[0], StackTraceDepth,
      2 + CallerEnvironment.StackFramesToSkip);
  finally
    LeaveHookLocks([ hlGetMem ]);
    LeaveMemHookCritical;
  end;
end;

procedure MemFreed(P: Pointer);
var
  lIndex: PIndexType;
  lHookLocked: Boolean;
begin
  EnterMemHookCritical;
  lHookLocked := IsHookLocked(hlFreeMem);
  EnterHookLocks([ hlFreeMem ]);
  try
    if lHookLocked and (not CallerEnvironment.InRealloc) then
      Exit;

    lIndex := CheckAndAlloc(P, False);
    if lIndex <> nil then
    begin
      if lIndex^.BlockSize <> 0 then
      begin
        lIndex^.BlockSize := 0;
      end;
    end;
  finally
    LeaveHookLocks([ hlFreeMem ]);
    LeaveMemHookCritical;
  end;
end;

var
  CPP_Old_new: function(Size: Cardinal): Pointer; cdecl = nil;
  CPP_Old_delete: function(P: Pointer): Cardinal; cdecl;
  CPP_Old_new_array: function(Size: Cardinal): Pointer; cdecl = nil;
  CPP_Old_delete_array: function(P: Pointer): Cardinal; cdecl;
  CPP_Old_malloc: function(Size: Cardinal): Pointer; cdecl = nil;
  CPP_Old_calloc: function(ANum: Cardinal; Size: Cardinal): Pointer; cdecl = nil;
  CPP_Old_realloc: function(P: Pointer; Size: Cardinal): Pointer; cdecl = nil;
  CPP_Old_free: function(P: Pointer): Cardinal; cdecl = nil;
  CPP_Old_expand: function(P: Pointer; Size: Cardinal): Pointer; cdecl = nil;
  CPP_Old_malloc_dbg: function(ASize: Cardinal; ABlockType: Integer; AFileName: PChar; ALineNum: Integer): Pointer; cdecl;
  CPP_Old_calloc_dbg: function(ANum: Cardinal; Size: Cardinal; ABlockType: Integer; AFileName: PChar; ALineNum: Integer): Pointer; cdecl;
  CPP_Old_realloc_dbg: function(P: Pointer; Size: Cardinal; ABlockType: Integer; AFileName: PChar; ALineNum: Integer): Pointer; cdecl;
  CPP_Old_free_dbg: function(P: Pointer; ABlockType: Integer): Cardinal; cdecl;
  CPP_Old_expand_dbg: function(P: Pointer; Size: Cardinal; ABlockType: Integer; AFileName: PChar; ALineNum: Integer): Pointer; cdecl;
  CPP_GetMemBlockSize: function(P: Pointer): Cardinal; cdecl = nil;

function CPP_new(Size: Cardinal): Pointer; cdecl;
var
  lNewSize: Cardinal;
begin
  lNewSize := GetAllocateSize(Size);
  Result := CPP_Old_new(lNewSize);
  MemAllocated(Result, Size, lNewSize);
end;

function CPP_delete(P: Pointer): Cardinal; cdecl;
begin
  MemFreed(P);
  Result := CPP_Old_delete(P);
end;

function CPP_new_array(Size: Cardinal): Pointer; cdecl;
var
  lNewSize: Cardinal;
begin
  lNewSize := GetAllocateSize(Size);
  Result := CPP_Old_new_array(lNewSize);
  MemAllocated(Result, Size, lNewSize);
end;

function CPP_delete_array(P: Pointer): Cardinal; cdecl;
begin
  MemFreed(P);
  Result := CPP_Old_delete_array(P);
end;

function CPP_malloc(ASize: Cardinal): Pointer; cdecl;
var
  lNewSize: Cardinal;
begin
  lNewSize := GetAllocateSize(ASize);
  Result := CPP_Old_malloc(lNewSize);
  MemAllocated(Result, ASize, lNewSize);
end;

function CPP_calloc(ANum: Cardinal; Size: Cardinal): Pointer; cdecl;
var
  lNewSize: Cardinal;
begin
  lNewSize := GetAllocateSize(ANum * Size);
  if ANum = 0 then
    ANum := lNewSize
  else
    ANum := (lNewSize + Size - 1) div Size;
  Result := CPP_Old_calloc(ANum, Size);
  MemAllocated(Result, ANum * Size, lNewSize);
end;

function CPP_realloc(P: Pointer; Size: Cardinal): Pointer; cdecl;
var
  lIndex: PIndexType;
  lThumb, lThumbNew: PMemThumb;
  lSize: Cardinal;
  lSuccess: Boolean;
  lHookLocked: Boolean;
begin
  EnterMemHookCritical;
  lHookLocked := IsHookLocked(hlReallocMem);
  EnterHookLocks([ hlGetMem, hlFreeMem, hlReallocMem ]);
  try
    if lHookLocked then
      lSuccess := False
    else
      lSuccess := GetIndexAndThumb(P, @lIndex, @lThumb);

    if lSuccess and IsValidIndex(lIndex) then
    begin
      lSize := lIndex^.BlockSize;
      lIndex^.BlockSize := 0;
      CallerEnvironment.InRealloc := True;
      try
        Result := CPP_malloc(Size);
      finally
        CallerEnvironment.InRealloc := False;
      end;
      if lSize > Size then
        lSize := Size;
      Move(P^, Result^, lSize);

      GetIndexAndThumb(Result, @lIndex, @lThumbNew);
      Assert(IsValidIndex(lIndex));

      lThumbNew^.MemType := lThumb^.MemType;
      lThumbNew^.GroupID := lThumb^.GroupID;

      FreeMem(P);
    end
    else
      Result := CPP_Old_realloc(P, Size);
  finally
    LeaveHookLocks([ hlGetMem, hlFreeMem, hlReallocMem ]);
    LeaveMemHookCritical;
  end;
end;

function CPP_expand(P: Pointer; Size: Cardinal): Pointer; cdecl;
var
  lNewSize: Cardinal;
begin
  lNewSize := GetAllocateSize(Size);
  Result := CPP_Old_expand(P, lNewSize);
  if (Result <> nil) and ((lNewSize - Size) >= CPP_GetMemBlockSize(Result)) then
    MemAllocated(Result, Size, lNewSize);
end;

function CPP_free(P: Pointer): Cardinal; cdecl;
begin
  MemFreed(P);
  Result := CPP_Old_free(P);
end;

function CPP_malloc_dbg(ASize: Cardinal; ABlockType: Integer; AFileName: PChar; ALineNum: Integer): Pointer; cdecl;
var
  lNewSize: Cardinal;
begin
  lNewSize := GetAllocateSize(ASize);
  Result := CPP_Old_malloc_dbg(lNewSize, ABlockType, AFileName, ALineNum);
  MemAllocated(Result, ASize, lNewSize);
end;

function CPP_calloc_dbg(ANum: Cardinal; Size: Cardinal; ABlockType: Integer; AFileName: PChar; ALineNum: Integer): Pointer; cdecl;
var
  lNewSize: Cardinal;
begin
  lNewSize := GetAllocateSize(ANum * Size);
  if ANum = 0 then
    ANum := lNewSize
  else
    ANum := (lNewSize + Size - 1) div Size;
  Result := CPP_Old_calloc_dbg(ANum, Size, ABlockType, AFileName, ALineNum);
  MemAllocated(Result, ANum * Size, lNewSize);
end;

function CPP_realloc_dbg(P: Pointer; Size: Cardinal; ABlockType: Integer; AFileName: PChar; ALineNum: Integer): Pointer; cdecl;
var
  lIndex: PIndexType;
  lThumb, lThumbNew: PMemThumb;
  lSize: Cardinal;
  lSuccess: Boolean;
  lHookLocked: Boolean;
begin
  EnterMemHookCritical;
  lHookLocked := IsHookLocked(hlReallocMem);
  EnterHookLocks([ hlGetMem, hlFreeMem, hlReallocMem ]);
  try
    if lHookLocked then
      lSuccess := False
    else
      lSuccess := GetIndexAndThumb(P, @lIndex, @lThumb);

    if lSuccess and IsValidIndex(lIndex) then
    begin
      lSize := lIndex^.BlockSize;
      lIndex^.BlockSize := 0;
      CallerEnvironment.InRealloc := True;
      try
        Result := CPP_malloc_dbg(Size, ABlockType, AFileName, ALineNum);
      finally
        CallerEnvironment.InRealloc := False;
      end;
      if lSize > Size then
        lSize := Size;
      Move(P^, Result^, lSize);

      GetIndexAndThumb(Result, @lIndex, @lThumbNew);
      Assert(IsValidIndex(lIndex));

      lThumbNew^.MemType := lThumb^.MemType;
      lThumbNew^.GroupID := lThumb^.GroupID;

      FreeMem(P);
    end
    else
      Result := CPP_Old_realloc_dbg(P, Size, ABlockType, AFileName, ALineNum);
  finally
    LeaveHookLocks([ hlGetMem, hlFreeMem, hlReallocMem ]);
    LeaveMemHookCritical;
  end;
end;

function CPP_expand_dbg(P: Pointer; Size: Cardinal; ABlockType: Integer; AFileName: PChar; ALineNum: Integer): Pointer; cdecl;
var
  lNewSize: Cardinal;
begin
  lNewSize := GetAllocateSize(Size);
  Result := CPP_Old_expand_dbg(P, lNewSize, ABlockType, AFileName, ALineNum);
  if (Result <> nil) and ((lNewSize - Size) >= CPP_GetMemBlockSize(Result)) then
    MemAllocated(Result, Size, lNewSize);
end;

function CPP_free_dbg(P: Pointer; ABlockType: Integer): Cardinal; cdecl;
begin
  MemFreed(P);
  Result := CPP_Old_free_dbg(P, ABlockType);
end;

procedure Dump(P: PByte);
var
  S: string;
  I: Integer;
begin
  S := '';
  for I := 0 to 10 - 1 do
  begin
    S := S + SimpleIntToHex(P^) + ' ';
    Inc(P);
  end;
  OutputDebugString(PChar(S));
end;

procedure InitCPPHooks;
begin
  @CPP_GetMemBlockSize := CPPInfo^.AddrGetMemBlockSize;

  HookProc(CPPInfo^._new, @CPP_new, @CPP_Old_new, '');
  HookProc(CPPInfo^._delete, @CPP_delete, @CPP_Old_delete, '');
  HookProc(CPPInfo^._new_array, @CPP_new_array, @CPP_Old_new_array, '');
  HookProc(CPPInfo^._delete_array, @CPP_delete_array, @CPP_Old_delete_array, '');
  HookProc(CPPInfo^.malloc, @CPP_malloc, @CPP_Old_malloc, '');
  HookProc(CPPInfo^.calloc, @CPP_calloc, @CPP_Old_calloc, '');
  HookProc(CPPInfo^.realloc, @CPP_realloc, @CPP_Old_realloc, '');
  HookProc(CPPInfo^.free, @CPP_free, @CPP_Old_free, '');
  HookProc(CPPInfo^._expand, @CPP_expand, @CPP_Old_expand, '');
  HookProc(CPPInfo^._malloc_dbg, @CPP_malloc_dbg, @CPP_Old_malloc_dbg, '');
  HookProc(CPPInfo^._calloc_dbg, @CPP_calloc_dbg, @CPP_Old_calloc_dbg, '');
  HookProc(CPPInfo^._realloc_dbg, @CPP_realloc_dbg, @CPP_Old_realloc_dbg, '');
  HookProc(CPPInfo^._free_dbg, @CPP_free_dbg, @CPP_Old_free_dbg, '');
  HookProc(CPPInfo^._expand_dbg, @CPP_expand_dbg, @CPP_Old_expand_dbg, '');
  HookProc(CPPInfo^.TObject_InitInstance, @NewInitInstance, @OldInitInstance, 'TObject.InitInstance');

  if (CPPInfo^.GetMem <> nil) and (CPPInfo^.FreeMem <> nil) and (CPPInfo^.ReallocMem <> nil) then
  begin
    HookProc(CPPInfo^.GetMem, @NewGetMem, @OldGetMem, 'GetMem');
    HookProc(CPPInfo^.FreeMem, @NewFreeMem, @OldFreeMem, 'FreeMem');
    HookProc(CPPInfo^.ReallocMem, @NewReallocMem, @OldReallocMem, 'ReallocMem');
  end;
end;

procedure DeInitCPPHooks;
begin
  UnhookProc(CPPInfo^._new);
  UnhookProc(CPPInfo^._delete);
  UnhookProc(CPPInfo^._new_array);
  UnhookProc(CPPInfo^._delete_array);
  UnhookProc(CPPInfo^.malloc);
  UnhookProc(CPPInfo^.calloc);
  UnhookProc(CPPInfo^.realloc);
  UnhookProc(CPPInfo^.free);
  UnhookProc(CPPInfo^._expand);
  UnhookProc(CPPInfo^._malloc_dbg);
  UnhookProc(CPPInfo^._calloc_dbg);
  UnhookProc(CPPInfo^._realloc_dbg);
  UnhookProc(CPPInfo^._free_dbg);
  UnhookProc(CPPInfo^._expand_dbg);
  UnhookProc(CPPInfo^.TObject_InitInstance);

  if (CPPInfo^.GetMem <> nil) and (CPPInfo^.FreeMem <> nil) and (CPPInfo^.ReallocMem <> nil) then
  begin
    UnhookProc(CPPInfo^.GetMem);
    UnhookProc(CPPInfo^.FreeMem);
    UnhookProc(CPPInfo^.ReallocMem);
  end;
end;

procedure InitCPP;
begin
  InitCPPHooks;
end;

procedure DeInitCPP;
begin
  DeInitCPPHooks;
end;

//**************C++ related functions end************************************

procedure OnDllServiceShutDown; stdcall;
begin
  DeInitLeakMemHook;;
end;

procedure InitModuleArrange;
var
  lModule: Cardinal;
  MBI: TMemoryBasicInformation;
begin
  lModule := 0;
  if InterModuleDebug then
    lModule := InterModuleHandle;
  if lModule = 0 then
    lModule := GetModuleHandle(nil);

  DestModuleAddrLow := 0;
  VirtualQuery(Pointer(lModule), MBI, SizeOf(MBI));
  if MBI.State = MEM_COMMIT then
  begin
    DestModuleAddrHigh := DllService.GetModuleImageSize(lModule);
    if DestModuleAddrHigh <> 0 then
      DestModuleAddrLow := Cardinal(MBI.AllocationBase);
    DestModuleAddrHigh := DestModuleAddrLow + DestModuleAddrHigh;
  end;
end;

procedure InitLeakMemHook;
var
  lCodeHook: ICodeHook;
begin
  if DenomoInited then
    Exit;

  MemThumbSize := (SizeOf(TMemThumb) + (StackTraceDepth - 1) * SizeOf(Cardinal) + MemoryAlignMask) and (not MemoryAlignMask);

  if KeepDetailedTrackOnFreedBlock then
    MemFreedThumbSize := (SizeOf(TMemFreedThumb) + (FreedStackTraceDepth - 1) * SizeOf(Cardinal) + MemoryAlignMask) and (not MemoryAlignMask)
  else
  begin
    if DetectVirtualMethodAccssOnFreedObject then
      MemFreedThumbSize := 4
    else
      MemFreedThumbSize := 1;
  end;

  if DetectVirtualMethodAccssOnFreedObject or DetectInterfaceMethodAccssOnFreedObject then
    InitFreedObjectVMT;

  InitConfig;

  InitRuntimeDll;

  //must before InitHost because InitHost may use CriticalSection
  InitializeCriticalSection(CriticalSection);

  DllService.GetCodeHook(lCodeHook);

  CurGroupID := 0;

  SafeFillChar(CallerEnvironment, SizeOf(TCallerEnvironment), 0);
  //explicit initializing is clear
  CallerEnvironment.InRealloc := False;

  InitHost;

  LevelIndex := AllocLevelMem(Level1Bits, True);

  InitHandleHook;

  InitModuleArrange;

  if InterModuleDebug then
    InitCPP
  else
    InitDelphi;

  ConnectToInspector;

  DllService.SetOnShutDown(OnDllServiceShutDown);

  DenomoInited := True;
end;

procedure DeInitLeakMemHook;
begin
  if not DenomoInited then
    Exit;

  DenomoInited := False;

  DllService.SetOnShutDown(nil);

  DeInitFreedObjectVMT;

  CheckLeakOnExit;

  DeInitHandleHook;

  if InterModuleDebug then
    DeInitCPP
  else
    DeInitDelphi;

  DeInitHost;

  DeInitConfig;

  DeleteCriticalSection(CriticalSection);

  DeInitRuntimeDll;
end;

end.

