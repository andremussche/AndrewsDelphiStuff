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

The Original Code is MiscUtils.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit MiscUtils;

// this unit is independent of Denomo

interface

uses
  Windows, ShellAPI, ShlObj, DenomoALAVLBinaryTree;

const
  SafeStringMaxLen = 298;

type
  TGetMem = function(Size: Integer): Pointer;
  TFreeMem = function(P: Pointer): Integer;
  TReallocMem = function(P: Pointer; Size: Integer): Pointer;

  TSafeString = packed record
    Len: Word;
    Chars: array[0..SafeStringMaxLen - 1] of Char;
  end;
  PSafeString = ^TSafeString;

  TWindowsVersion = ( wvUnsolved, wvUnknown,
    wv95, wv98, wvMe,
    wvNT351, wvNT4,
    wv2000, wvXP,
    wv2003,
    wvVista );

  TSimpleIniFile = class
  private
    FSectionName: string;
    FFileName: string;
  protected
    property FileName: string read FFileName;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    function ReadInteger(const AKey: string; ADefault: Integer): Integer;
    function ReadString(const AKey: string; const ADefault: string): string;
    function ReadBoolean(const AKey: string; ADefault: Boolean): Boolean;
    procedure WriteInteger(const AKey: string; AValue: Integer);
    procedure WriteString(const AKey: string; const AValue: string);
    procedure WriteBoolean(const AKey: string; AValue: Boolean);

    property SectionName: string read FSectionName write FSectionName;
  end;

  TSimpleStringBuffer = class
  private
    FBufferSize: Integer;
    FCurPtr: PChar;
    FBuffer: PChar;
    function GetStringCount: Integer;
  protected
    procedure SetStringCount(const Value: Integer); virtual;
    function SkipCurPtr(ASkip: Integer): PChar;

    property CurPtr: PChar read FCurPtr;
    property Buffer: PChar read FBuffer;
    property BufferSize: Integer read FBufferSize;
  public
    constructor Create(ABuffer: Pointer; ABufferSize: Integer);
    destructor Destroy; override;

    procedure Reset; virtual;

    property StringCount: Integer read GetStringCount write SetStringCount;
  end;

  TSimpleStringBufferReader = class(TSimpleStringBuffer)
  protected
    procedure SetStringCount(const Value: Integer); override;
  public
    function ReadNext: string;
    procedure ReadAll(var AString: string; const ADelimiter: string = #13#10);
  end;

  TSimpleStringBufferWriter = class(TSimpleStringBuffer)
  public
    procedure Reset; override;
    function Write(const S: string): Boolean;
  end;

  TBalancedBinaryTreeCompare = function(AData1, AData2: Pointer): Integer of object;
  TBalancedBinaryTreeEnumerator = function(AData: Pointer; AParam: Pointer): Boolean of object;

  // a wrapper class for any third party balanced-binary-tree implementation
  TBalancedBinaryTree = class
  private
    FComparer: TBalancedBinaryTreeCompare;
    FEnumerator: TBalancedBinaryTreeEnumerator;
    FDataSize: Integer;
    FBinTree: TALBaseAVLBinaryTree;
  protected
    function AllocateData: Pointer;
    procedure FreeData(AData: Pointer);

    property DataSize: Integer read FDataSize;
    property BinTree: TALBaseAVLBinaryTree read FBinTree;
    property Enumerator: TBalancedBinaryTreeEnumerator read FEnumerator write FEnumerator;
  public
    constructor Create(ADataSize: Integer);
    destructor Destroy; override;

    function Add(AData: Pointer): Pointer;
    function Remove(AData: Pointer): Boolean;
    function Find(AData: Pointer): Pointer;
    procedure Clear;
    procedure Enumerate(AEnumerator: TBalancedBinaryTreeEnumerator; AParam: Pointer);

    property Comparer: TBalancedBinaryTreeCompare read FComparer write FComparer;
  end;

  TGDITableEntry = packed record
    pKernelInfo: DWORD;
    ProcessID: WORD;
    nCount: WORD;
    nUpper: WORD;
    nType: WORD;
    pUserInfo: DWORD;
  end;
  PGDITableEntry =^TGDITableEntry;

function SafeStrToPasStr(const ASafeStr: TSafeString): string; overload;
procedure SafeStrToPasStr(const ASafeStr: TSafeString; out AString: string); overload;
function PasStrToSafeStr(APasStr: string): TSafeString; overload;
procedure PasStrToSafeStr(APasStr: string; ASafeStr: PSafeString); overload;

procedure SafeFillChar(var Dest; Count: Integer; Value: Integer);

procedure SimpleFreeAndNil(var obj);
function SimpleIntToStr(Num: Integer): string;
function SimpleIntToHex(Num: Cardinal): string;
function SimpleStrToInt(const S: string): Integer;
function SimpleAllocMem(Size: Cardinal): Pointer;
function SimpleFileExists(const AFileName: string): Boolean;
function SimpleCompareMem(P1, P2: Pointer; ALength: Integer): Boolean;
function GetShellFolder(CSIDL: Integer): string;

function DebugTrace(const AMsg: string): Boolean;

function GetWindowsVersion: TWindowsVersion;

function IsValidGDIObject(AHandle: Cardinal): Boolean;
function LoadGDIHandleTable(ABuffer: PGDITableEntry; AProcID: Integer): Integer;
function GDIEntryGetPID(AEntry: PGDITableEntry): Integer;
function GDIEntryIsDeleted(AEntry: PGDITableEntry): Boolean;
function GDIEntryGetEntryFromIndex(AIndex: Integer): PGDITableEntry;
function GDIEntryGetEntryFromHandle(AHandle: HGDIOBJ): PGDITableEntry;
function GDIObjectIsDeleted(AHandle: HGDIOBJ): Boolean;
function GDIObjectIsStockObject(AHandle: HGDIOBJ): Boolean;
function GDIEntryGetIndexFromHandle(AHandle: HGDIOBJ): Word;

implementation

const
  MaxGDIHandle = $4000;

var
  BalancedBinaryTreeGetMem: TGetMem = nil;
  BalancedBinaryTreeFreeMem: TFreeMem = nil;

function SafeStrToPasStr(const ASafeStr: TSafeString): string;
begin
  SetLength(Result, ASafeStr.Len);
  if ASafeStr.Len > 0 then
    Move(ASafeStr.Chars[0], Result[1], ASafeStr.Len);
end;

procedure SafeStrToPasStr(const ASafeStr: TSafeString; out AString: string);
begin
  SetLength(AString, ASafeStr.Len);
  if ASafeStr.Len > 0 then
    Move(ASafeStr.Chars[0], AString[1], ASafeStr.Len);
end;

function PasStrToSafeStr(APasStr: string): TSafeString;
begin
  PasStrToSafeStr(APasStr, @Result);
end;

procedure PasStrToSafeStr(APasStr: string; ASafeStr: PSafeString); overload;
var
  lLen: Integer;
begin
  lLen := Length(APasStr);

  if lLen >= SafeStringMaxLen then
    lLen := SafeStringMaxLen - 1;

  if lLen < 0 then
    lLen := 0;
  ASafeStr^.Len := lLen;
  if lLen > 0 then
    Move(APasStr[1], ASafeStr^.Chars[0], lLen * StringElementSize(APasStr) );
  ASafeStr^.Chars[lLen] := #0;
end;

{ Use this method instead of Delphi's FillChar.
  FillChar in Delphi 2007 starts using float point instructions
  that may cause exception raised if the FPU stack is already full.

params:
  eax - the address to fill in
  edx - the count of bytes to fill
  ecx - the byte value to fill with
}
procedure SafeFillChar(var Dest; Count: Integer; Value: Integer);
asm
  push edi

  mov edi, eax

  mov ch, cl
  movzx eax, cx
  shl ecx, 16
  or eax, ecx

  mov ecx, edx
  shr ecx, 2
  js @@end

  rep stosd

  mov ecx, edx
  and ecx, 3
  rep stosb

@@end:
  pop edi
end;

procedure SimpleFreeAndNil(var obj);
var
  lTemp: TObject;
begin
  lTemp := TObject(Obj);
  Pointer(Obj) := nil;
  lTemp.Free;
end;

function SimpleIntToStr(Num: Integer): string;
var
  lNeg: Boolean;
  K: Integer;
  C: Char;
  lBuf: array[0..31] of Char;
  I: Integer;
begin
  Result := '';
  if Num = 0 then
  begin
    Result := '0';
    Exit;
  end;

  lNeg := Num < 0;
  if lNeg then
    Num := -Num;
  I := High(lBuf);
  while Num > 0 do
  begin
    K := Num mod 10;
    Num := (Num - K) div 10;
    if K < 10 then
      C := Chr(K + Ord('0'))
    else
      C := Chr(K - 10 + Ord('A'));
    lBuf[I] := C;
    Dec(I);
  end;
  if lNeg then
  begin
    lBuf[I] := '-';
    Dec(I);
  end;
  K := High(lBuf) - I;
  if K > 0 then
  begin
    SetLength(Result, K);
    Move(lBuf[I + 1], Result[1], K);
  end
end;

function SimpleIntToHex(Num: Cardinal): string;
var
  K: Cardinal;
  C: Char;
  lBuf: array[0..31] of Char;
  I: Integer;
begin
  Result := '';
  if Num = 0 then
  begin
    Result := '0';
    Exit;
  end;

  I := High(lBuf);
  while Num > 0 do
  begin
    K := Num and 15;
    Num := (Num - K) shr 4;
    if K < 10 then
      C := Chr(K + Ord('0'))
    else
      C := Chr(K - 10 + Ord('A'));
    lBuf[I] := C;
    Dec(I);
  end;
  K := High(lBuf) - I;
  if K > 0 then
  begin
    if K < 8 then
    begin
      while K < 8 do
      begin
        lBuf[I] := '0';
        Dec(I);
        Inc(K);
      end;
    end;
    SetLength(Result, K);
    Move(lBuf[I + 1], Result[1], K);
  end
end;

function SimpleStrToInt(const S: string): Integer;
var
  lCode: Integer;
begin
  Val(S, Result, lCode);
end;

function SimpleAllocMem(Size: Cardinal): Pointer;
begin
  GetMem(Result, Size);
  FillChar(Result^, Size, 0);
end;

function SimpleFileExists(const AFileName: string): Boolean;
var
  H: THandle;
  lFindData: TWin32FindData;
begin
  H := FindFirstFile(PChar(AFilename), lFindData);
  if H = INVALID_HANDLE_VALUE then
    Result := False
  else
  begin
    Result := True;
    FindClose(H);
  end;
end;

function SimpleCompareMem(P1, P2: Pointer; ALength: Integer): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to ALength - 1 do
  begin
    if PByte(Integer(P1) + I)^ <> PByte(Integer(P2) + I)^ then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function GetShellFolder(CSIDL: Integer): string;
var
  pidl         : PItemIdList;
  FolderPath   : string;
  SystemFolder : Integer;
begin
  FolderPath := '';
  SystemFolder := CSIDL;
  if Succeeded(SHGetSpecialFolderLocation(0, SystemFolder, pidl)) then
  begin
    SetLength(FolderPath, MAX_PATH);
    if SHGetPathFromIDList(pidl, PChar(FolderPath)) then
    begin
      SetLength(FolderPath, lstrlen(PChar(FolderPath)));
    end;
  end;
  Result := FolderPath;
end;

function DebugTrace(const AMsg: string): Boolean;
begin
  Result := True;
  OutputDebugString(PChar(AMsg));
end;

var
  GWindowsVersion: TWindowsVersion = wvUnsolved;

function GetWindowsVersion: TWindowsVersion;
var
  lVersion: OSVERSIONINFO;
begin
  if GWindowsVersion = wvUnsolved then
  begin
    GWindowsVersion := wvUnknown;
    lVersion.dwOSVersionInfoSize := SizeOf(lVersion);
    if GetVersionEx(lVersion) then
    begin
      case lVersion.dwMajorVersion of
        6:
        begin
          GWindowsVersion := wvVista;
        end;

        5:
        begin
          case lVersion.dwMinorVersion of
            2:
              GWindowsVersion := wv2003;
            1:
              GWindowsVersion := wvXP;
            0:
              GWindowsVersion := wv2000;
          end;
        end;

        4:
        begin
          if lVersion.dwPlatformId = VER_PLATFORM_WIN32_NT then
          begin
            case lVersion.dwMinorVersion of
              0:
                GWindowsVersion := wvNT4;
              10:
                GWindowsVersion := wv98;
              90:
                GWindowsVersion := wvMe;
            end;
          end
          else
            GWindowsVersion := wv95;
        end;

        3:
        begin
          GWindowsVersion := wvNT351;
        end;
      end;
    end;
  end;

  Result := GWindowsVersion;
end;

function IsValidGDIObject(AHandle: Cardinal): Boolean;
var
  lObjType: Integer;
  lBuf: array[0..1023] of Byte;
  lRect: TRect;

  function CheckSize(ASize: Integer): Boolean;
  begin
    Result := GetObject(AHandle, ASize, @lBuf[0]) = ASize;
  end;
begin
  lObjType := GetObjectType(AHandle);
  Result := False;
  if lObjType = 0 then
    Exit;

  case lObjType of
    OBJ_BITMAP:
      Result := CheckSize(SizeOf(BITMAP));

    OBJ_BRUSH:
      Result := CheckSize(SizeOf(LOGBRUSH));

    OBJ_FONT:
      Result := CheckSize(SizeOf(LOGFONT));

    OBJ_PEN:
      Result := CheckSize(SizeOf(LOGPEN));

    OBJ_PAL:
      Result := CheckSize(SizeOf(Word));

    OBJ_EXTPEN:
      Result := CheckSize(SizeOf(EXTLOGPEN));

    OBJ_REGION:
      Result := GetRgnBox(AHandle, lRect) <> 0;

    OBJ_DC, OBJ_MEMDC, OBJ_METAFILE,
      OBJ_METADC, OBJ_ENHMETAFILE, OBJ_ENHMETADC:
      Result := True;
  end;
end;

type
  TGdiQueryTable = function: Cardinal; stdcall;
var
  GdiQueryTable: TGdiQueryTable = nil;

function GetGDISharedHandleTable: PGDITableEntry;
var
  lOffset: Cardinal;
begin
  if @GdiQueryTable <> nil then
  begin
    Result := PGDITableEntry(GdiQueryTable);
    Exit;
  end;

  lOffset := 0;
  case GetWindowsVersion of
    wv2000, wvNT4:
      lOffset := $7ffdf000 + $94;
    wvXP:
      lOffset := $7ffde000 + $94;
  end;

  if lOffset = 0 then
    Result := nil
  else
  begin
    try
      if not IsBadReadPtr(Pointer(lOffset), SizeOf(Pointer)) then
      begin
        Result := PGDITableEntry(PPointer(lOffset)^);
        if IsBadReadPtr(Result, MaxGDIHandle * SizeOf(TGDITableEntry)) then
          Result := nil;
      end
      else
        Result := nil;
    except
      Result := nil;
    end;
  end;
end;

function AllocateGDIHandleTable: PGDITableEntry;
begin
  GetMem(Result, SizeOf(TGDITableEntry) * MaxGDIHandle);
end;

procedure FreeGDIHandleTable(P: PGDITableEntry);
begin
  FreeMem(P);
end;

function LoadGDIHandleTable(ABuffer: PGDITableEntry; AProcID: Integer): Integer;
var
  lSharedTable: PGDITableEntry;
  P, lDest: PGDITableEntry;
  I: Integer;
  lProcID: Integer;
  lIsNT: Boolean;
begin
  lSharedTable := GetGDISharedHandleTable;

  if lSharedTable = nil then
  begin
    Result := -1;
    Exit;
  end;

  Result := 0;

  try
    lIsNT := GetWindowsVersion in [ wvNT351, wvNT4 ];
    P := PGDITableEntry(Cardinal(lSharedTable) - SizeOf(TGDITableEntry));
    lDest := PGDITableEntry(Cardinal(ABuffer) - SizeOf(TGDITableEntry));
    for I := 0 to MaxGDIHandle - 1 do
    begin
      P := PGDITableEntry(Cardinal(P) + SizeOf(TGDITableEntry));

      if lIsNT then
        lProcID := P^.nCount
      else
        lProcID := P^.ProcessID;

      if (AProcID < 0) or (lProcID = AProcID) then
      begin
        lDest := PGDITableEntry(Cardinal(lDest) + SizeOf(TGDITableEntry));
        Move(P^, lDest^, SizeOf(TGDITableEntry));
        if lIsNT then
        begin
          lProcID := lDest^.ProcessID;
          lDest^.ProcessID := lDest^.nCount;
          lDest^.nCount := lProcID;
        end;
        Inc(Result);
      end;
    end;
  except
  
  end;
end;

function GDIEntryGetPID(AEntry: PGDITableEntry): Integer;
begin
  if GetWindowsVersion in [ wvNT351, wvNT4 ] then
    Result := AEntry^.nCount
  else
    Result := AEntry^.ProcessID
end;

function GDIEntryIsDeleted(AEntry: PGDITableEntry): Boolean;
begin
  Result := (AEntry <> nil) and ((AEntry^.pKernelInfo and $80000000) = 0);
end;

function GDIEntryGetIndexFromHandle(AHandle: HGDIOBJ): Word;
begin
  Result := AHandle and $3fff;
end;

function GDIEntryGetEntryFromIndex(AIndex: Integer): PGDITableEntry;
begin
  Result := GetGDISharedHandleTable;
  if Result <> nil then
    Result := PGDITableEntry(Cardinal(Result) + Cardinal(SizeOf(TGDITableEntry) * AIndex));
end;

function GDIEntryGetEntryFromHandle(AHandle: HGDIOBJ): PGDITableEntry;
begin
  Result := GDIEntryGetEntryFromIndex(GDIEntryGetIndexFromHandle(AHandle));
end;

function GDIObjectIsDeleted(AHandle: HGDIOBJ): Boolean;
begin
  Result := GDIEntryIsDeleted(GDIEntryGetEntryFromHandle(AHandle));
end;

function GDIObjectIsStockObject(AHandle: HGDIOBJ): Boolean;
var
  lEntry: PGDITableEntry;
begin
  lEntry := GDIEntryGetEntryFromHandle(AHandle);

  if lEntry <> nil then
    Result := GDIEntryGetPID(lEntry) = 0
  else
    Result := (AHandle and $00800000) <> 0; //undocument flag
end;

{ TSimpleIniFile }

constructor TSimpleIniFile.Create(const AFileName: string);
begin
  inherited Create;

  FFileName := AFileName;
  SectionName := 'main';  
end;

destructor TSimpleIniFile.Destroy;
begin

  inherited;
end;

function TSimpleIniFile.ReadBoolean(const AKey: string;
  ADefault: Boolean): Boolean;
begin
  Result := ReadInteger(AKey, Ord(ADefault)) <> 0;
end;

function TSimpleIniFile.ReadInteger(const AKey: string;
  ADefault: Integer): Integer;
var
  S: string;
begin
  S := ReadString(AKey, '');
  if S = '' then
    Result := ADefault
  else
    Result := SimpleStrToInt(S);
end;

function TSimpleIniFile.ReadString(const AKey, ADefault: string): string;
var
  lBuf: array[0..2047] of Char;
  lLen: Integer;
begin
  lLen := GetPrivateProfileString(PChar(SectionName),
    PChar(AKey), PChar(ADefault), lBuf, Length(lBuf), PChar(FileName));
  SetLength(Result, lLen);
  if lLen > 0 then
    Move(lBuf[0], Result[1], lLen);
end;

procedure TSimpleIniFile.WriteBoolean(const AKey: string; AValue: Boolean);
const
  BooleanValues: array[Boolean] of string = ('0', '1');
begin
  WriteString(AKey, BooleanValues[AValue]);
end;

procedure TSimpleIniFile.WriteInteger(const AKey: string; AValue: Integer);
begin
  WriteString(AKey, SimpleIntToStr(AValue));
end;

procedure TSimpleIniFile.WriteString(const AKey, AValue: string);
begin
  WritePrivateProfileString(PChar(SectionName),
    PChar(AKey), PChar(AValue), PChar(FileName));
end;

{ TSimpleStringBuffer }

constructor TSimpleStringBuffer.Create(ABuffer: Pointer;
  ABufferSize: Integer);
begin
  FBuffer := ABuffer;
  FBufferSize := ABufferSize;

  Reset;
end;

destructor TSimpleStringBuffer.Destroy;
begin

  inherited;
end;

procedure TSimpleStringBuffer.Reset;
begin
  FCurPtr := FBuffer + SizeOf(Integer);
end;

function TSimpleStringBuffer.GetStringCount: Integer;
begin
  Result := PInteger(FBuffer)^;
end;

procedure TSimpleStringBuffer.SetStringCount(const Value: Integer);
begin
  PInteger(FBuffer)^ := Value;
end;

function TSimpleStringBuffer.SkipCurPtr(ASkip: Integer): PChar;
begin
  FCurPtr := FCurPtr + ASkip;
  Result := FCurPtr;
end;

{ TSimpleStringBufferReader }

function TSimpleStringBufferReader.ReadNext: string;
var
  lLen: Integer;
begin
  lLen := PInteger(CurPtr)^;
  SkipCurPtr(SizeOf(Integer));
  SetLength(Result, lLen);
  if lLen > 0 then
    Move(CurPtr^, Result[1], lLen);
  SkipCurPtr(lLen);
end;

procedure TSimpleStringBufferReader.ReadAll(var AString: string;
  const ADelimiter: string);
var
  I: Integer;
  lTotalLen: Integer;
  lDelimiterLen: Integer;
  lLen: Integer;
  lCount: Integer;
  P, PDest: PChar;
begin
  Reset;

  lDelimiterLen := Length(ADelimiter);
  lTotalLen := 0;
  lCount := StringCount;
  P := CurPtr;
  for I := lCount - 1 downto 0 do
  begin
    lLen := PInteger(P)^;
    Inc(lTotalLen, lLen + lDelimiterLen);
    Inc(P, lLen + SizeOf(Integer));
  end;

  SetLength(AString, lTotalLen);

  if lTotalLen > 0 then
  begin
    P := CurPtr;
    PDest := @AString[1];
    for I := lCount - 1 downto 0 do
    begin
      lLen := PInteger(P)^;
      if lLen > 0 then
      begin
        Move((P + SizeOf(Integer))^, PDest^, lLen);
        Inc(PDest, lLen);
      end;

      if lDelimiterLen > 0 then
      begin
        case lDelimiterLen of
          2:
          begin
            PWord(PDest)^ := PWord(@ADelimiter[1])^;
            Inc(PDest, SizeOf(Word));
          end;
          1:
          begin
            PDest^ := ADelimiter[1];
            Inc(PDest);
          end;
          else
          begin
            Move(ADelimiter[1], PDest^, lDelimiterLen);
            Inc(PDest, lDelimiterLen);
          end;
        end;
      end;
      Inc(P, PInteger(P)^ + SizeOf(Integer));
    end;
  end;
end;

procedure TSimpleStringBufferReader.SetStringCount(const Value: Integer);
begin
  //raise Exception.Create('String buffer is readonly, can not set count.');
end;

{ TSimpleStringBufferWriter }

procedure TSimpleStringBufferWriter.Reset;
begin
  inherited;

  StringCount := 0;
end;

function TSimpleStringBufferWriter.Write(const S: string): Boolean;
var
  lLen: Integer;
begin
  lLen := Length(S) * StringElementSize(s);
  Result := CurPtr + lLen + SizeOf(Integer) < Buffer + BufferSize;

  if Result then
  begin
    PInteger(CurPtr)^ := lLen;
    SkipCurPtr(SizeOf(Integer));

    //Check it to avoid Range Checking error
    if lLen > 0 then
      Move(S[1], CurPtr^, lLen);

    SkipCurPtr(lLen);

    StringCount := StringCount + 1;
  end;
end;

type
  TAVLBalancedBinaryTreeNode = class(TALBaseAVLBinaryTreeNode)
  private
    FData: Pointer;
    FOwnerTree: TBalancedBinaryTree;
  protected
    property Data: Pointer read FData;
    property OwnerTree: TBalancedBinaryTree read FOwnerTree;
  public
    constructor Create(ATree: TBalancedBinaryTree); reintroduce;
    destructor Destroy; override;

    class function CreateFromData(ATree: TBalancedBinaryTree; AData: Pointer): TAVLBalancedBinaryTreeNode;
  end;

  TAVLBalancedBinaryTree = class(TALBaseAVLBinaryTree)
  private
    FOwnerTree: TBalancedBinaryTree;
  protected
    Function  CompareNode(IdVal: pointer; ANode: TALBaseAVLBinaryTreeNode): Integer; overload; override;
    Function  CompareNode(ANode1, ANode2: TALBaseAVLBinaryTreeNode): Integer; overload; override;
    function  CreateNode: TALBaseAVLBinaryTreeNode; override;

    property OwnerTree: TBalancedBinaryTree read FOwnerTree;
  public
    constructor Create(ATree: TBalancedBinaryTree); reintroduce;
    destructor Destroy; override;
  end;

{ TAVLBalancedBinaryTreeNode }

constructor TAVLBalancedBinaryTreeNode.Create(ATree: TBalancedBinaryTree);
begin
  inherited Create;

  FOwnerTree := ATree;
  FData := OwnerTree.AllocateData;
end;

destructor TAVLBalancedBinaryTreeNode.Destroy;
begin
  OwnerTree.FreeData(FData);

  inherited;
end;

class function TAVLBalancedBinaryTreeNode.CreateFromData(
  ATree: TBalancedBinaryTree; AData: Pointer): TAVLBalancedBinaryTreeNode;
begin
  Result := Create(ATree);
  Move(AData^, Result.Data^, ATree.DataSize);
end;

{ TAVLBalancedBinaryTree }

constructor TAVLBalancedBinaryTree.Create(ATree: TBalancedBinaryTree);
begin
  inherited Create;

  FOwnerTree := ATree;
end;

destructor TAVLBalancedBinaryTree.Destroy;
begin

  inherited;
end;

function TAVLBalancedBinaryTree.CompareNode(ANode1,
  ANode2: TALBaseAVLBinaryTreeNode): Integer;
begin
  Result := OwnerTree.Comparer(TAVLBalancedBinaryTreeNode(ANode1).Data,
    TAVLBalancedBinaryTreeNode(ANode2).Data);
end;

function TAVLBalancedBinaryTree.CompareNode(IdVal: pointer;
  ANode: TALBaseAVLBinaryTreeNode): Integer;
begin
  Result := OwnerTree.Comparer(IdVal,
    TAVLBalancedBinaryTreeNode(ANode).Data);
end;

function TAVLBalancedBinaryTree.CreateNode: TALBaseAVLBinaryTreeNode;
begin
  Result := TAVLBalancedBinaryTreeNode.Create(OwnerTree);
end;

{ TBalancedBinaryTree }

constructor TBalancedBinaryTree.Create(ADataSize: Integer);
begin
  inherited Create;

  Assert(ADataSize > 0, 'TBalancedBinaryTree: DataSize must be great than zero');

  FDataSize := ADataSize;

  FBinTree := TAVLBalancedBinaryTree.Create(Self);
end;

destructor TBalancedBinaryTree.Destroy;
begin
  Clear;
  SimpleFreeAndNil(FBinTree);

  inherited;
end;

function TBalancedBinaryTree.Add(AData: Pointer): Pointer;
var
  lNode: TAVLBalancedBinaryTreeNode;
begin
  Assert(Assigned(Comparer), 'TBalancedBinaryTree: Comparer must not be nil');

  lNode := TAVLBalancedBinaryTreeNode.CreateFromData(Self, AData);
  if BinTree.AddNode(lNode) then
  begin
    Result := lNode.Data;
  end
  else
  begin
    lNode.Free;
    Result := nil;
  end;
end;

function TBalancedBinaryTree.Remove(AData: Pointer): Boolean;
begin
  Assert(Assigned(Comparer), 'TBalancedBinaryTree: Comparer must not be nil');

  Result := BinTree.DeleteNode(AData);
end;

function TBalancedBinaryTree.Find(AData: Pointer): Pointer;
var
  lNode: TAVLBalancedBinaryTreeNode;
begin
  Assert(Assigned(Comparer), 'TBalancedBinaryTree: Comparer must not be nil');

  lNode := TAVLBalancedBinaryTreeNode(BinTree.FindNode(AData));
  if lNode = nil then
    Result := nil
  else
    Result := lNode.Data;
end;

procedure TBalancedBinaryTree.Clear;
begin
  BinTree.Clear;
end;

function TBalancedBinaryTree.AllocateData: Pointer;
begin
  if @BalancedBinaryTreeGetMem <> nil then
    Result := BalancedBinaryTreeGetMem(DataSize)
  else
    GetMem(Result, DataSize);
end;

procedure TBalancedBinaryTree.FreeData(AData: Pointer);
begin
  if @BalancedBinaryTreeFreeMem <> nil then
    BalancedBinaryTreeFreeMem(AData)
  else
    FreeMem(AData);
end;

type
  TEnumerateRec = record
    Enumerator: TBalancedBinaryTreeEnumerator;
    Param: Pointer;
  end;
  PEnumerateRec = ^TEnumerateRec;

procedure EnumerateCallback(ATree: TALBaseAVLBinaryTree; ANode: TALBaseAVLBinaryTreeNode;
  AExtData: Pointer; var AContinue: Boolean);
begin
  AContinue := PEnumerateRec(AExtData)^.Enumerator(TAVLBalancedBinaryTreeNode(ANode).Data,
    PEnumerateRec(AExtData)^.Param);
end;

procedure TBalancedBinaryTree.Enumerate(
  AEnumerator: TBalancedBinaryTreeEnumerator; AParam: Pointer);
var
  lRec: TEnumerateRec;
begin
  lRec.Enumerator := AEnumerator;
  lRec.Param := AParam;
  BinTree.Iterate(EnumerateCallback, False, @lRec);
end;

initialization
  GdiQueryTable := GetProcAddress(GetModuleHandle('gdi32'), 'GdiQueryTable');

end.

