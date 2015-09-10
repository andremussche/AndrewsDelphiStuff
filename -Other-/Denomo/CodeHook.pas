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

The Original Code is CodeHook.pas

The Initial Developer of the Original Code is Wang Qi.
***********************************************************************}


unit CodeHook;

interface

uses
  SysUtils, Classes, Windows,
  DisAsm32,
  CodeHookIntf;

const  
  MIN_HOOK_PREVIOUSMETHOD_LEN = 128;
  MIN_HOOK_THUMB_LEN = 1024;

type
  //DisAsm32 redefined PCardinal which conflicts with System one...
  PCardinal = System.PCardinal;

  TExtendCodeHookInfo = packed record
    Target: Pointer;
  	Hook: Pointer;
    Flags: Cardinal;
    TargetObject: Pointer;
	  ParamCount: Integer;
    TargetCallingConvention: Integer;
    HookCallingConvention: Integer;
    ExtraParams: array[0 .. MAX_EXTRA_PARAM_COUNT - 1] of Cardinal;
    PreviousMethod: array[0 .. MIN_HOOK_PREVIOUSMETHOD_LEN - 1] of Byte;
    HookThumbCode: array[0 .. MIN_HOOK_THUMB_LEN - 1] of Byte;
    UserDataPlaceHolder: Byte;
  end;
  PExtendCodeHookInfo = ^TExtendCodeHookInfo;
  
  TByteWriter = class
  private
    FBuffer: PByte;
  public
    procedure WriteByte(V: Byte);
    procedure WriteWord(V: Word);
    procedure WriteCardinal(V: Cardinal);
    procedure WriteInteger(V: Integer);
    procedure WriteBuffer(V: PByte; ALen: Integer);
    procedure WriteByteArray(V: array of Byte);

    property Buffer: PByte read FBuffer write FBuffer;
  end;

  TErrorCodeHook = class(TInterfacedObject, IErrorCodeHook)
  private
    FErrorCode: Cardinal;
    FErrorMsg: string;
  protected
    procedure SetErrorCode(AErrorCode: Cardinal; const AParams: array of const);
    procedure ClearErrorCode;

    { IErrorCodeHook}
    function GetErrorCode: Cardinal; stdcall;
    function GetErrorMessage(AMessage: PChar; AMessageLen: Integer): Integer; stdcall;
  end;

  TErrorCodeHookRedirect = class(TInterfacedObject, IErrorCodeHook)
  protected
    function GetDelegator: TErrorCodeHook; virtual; abstract;
    procedure SetErrorCode(AErrorCode: Cardinal; const AParams: array of const);

    { IErrorCodeHook}
    function GetErrorCode: Cardinal; stdcall;
    function GetErrorMessage(AMessage: PChar; AMessageLen: Integer): Integer; stdcall;
  end;

  TDirectCodeHookInstructionType = ( itBad, itCopy, itJCC, itJmp, itCall, itJECXZ );
  TDirectCodeHookInstruction = packed record
    InstType: TDirectCodeHookInstructionType;
    RawCodeLen: Byte;
    RawCode: array[0 .. 15] of Byte;
    JumpDestAddr: Cardinal; //absolute address
    DestOpCode: array[0 .. 15] of Byte;
    DestOpCodeLen: Byte;
  end;
  PDirectCodeHookInstruction = ^TDirectCodeHookInstruction;

  TDirectCodeHook = class(TErrorCodeHook, IDirectCodeHook)
  private
    FDisassembler: TDisAsm;

    FInsts: array of TDirectCodeHookInstruction;
    FInstCount: Integer;
    FCodeLen: Cardinal;
    FMinCodeLen: Cardinal;
    FTarget: PByte;
  protected
    function DisassembleInst(ACode: PByte; AInst: PDirectCodeHookInstruction): PByte;
    function GetInstructionLength(ACode: PByte): Integer;

    function Disassemble: Boolean;
    function Fixup: Boolean;
    function OutputByteCode(ADest: PByte): Boolean;
    function RedirectCode(AHook: Pointer): Boolean;

    property Disassembler: TDisAsm read FDisassembler;
  public
    constructor Create;
    destructor Destroy; override;

    { IDirectCodeHook }
    function Hook(ATarget, AHook: Pointer; APreviousMethod: Pointer): LongBool; stdcall;
    function Unhook(ATarget: Pointer; APreviousMethod: Pointer): LongBool; stdcall;
    function AllocatePreviousMethodMemory: Pointer; stdcall;
    procedure FreePreviousMethodMemory(APreviousMethod: Pointer); stdcall;
  end;

  TCodeHook = class(TErrorCodeHookRedirect, ICodeHook)
  private
    FDirectCodeHook: TDirectCodeHook;
    FHandleList: TList;
    FUserDataSize: Integer;
    FCodeHookHelper: ICodeHookHelper;
  protected
    function MakeThumbCode(ATargetObject: Pointer; AHandle: TCodeHookHandle;
      ATarget: Pointer; ATargetCallingConvention: Integer;
      AHook: Pointer; AHookCallingConvention: Integer;
      AParamCount: Integer;
      AExtraParams: PCardinal; AExtraParamCount: Integer): Boolean;

    procedure ClearHooks;

    function AllocateHookInfo(var AHandle: TCodeHookHandle): PExtendCodeHookInfo;
    procedure FreeHookInfo(AHandle: TCodeHookHandle);
    function GetHookInfoFromHandle(AHandle: TCodeHookHandle): PExtendCodeHookInfo;
    function FindHookHandleFromTargetAndHook(ATarget: Pointer; AHook: Pointer): TCodeHookHandle;

    function GetDelegator: TErrorCodeHook; override;

    { ICodeHook }
    procedure GetDirectCodeHook(out Obj); stdcall;
    procedure GetCodeHookHelper(out Obj); stdcall;
    procedure CreateCodeHook(out Obj); stdcall;
    procedure SetUserDataSize(ASize: Integer); stdcall;
    function Hook(ATarget, AHook: Pointer): TCodeHookHandle; stdcall;
    function Unhook(AHandle: TCodeHookHandle): LongBool; stdcall;
    function GetHookInfo(AHandle: TCodeHookHandle; AInfo: PCodeHookInfo): LongBool; stdcall;
    function GetPreviousMethod(AHandle: TCodeHookHandle): Pointer; stdcall;
    function GetUserData(AHandle: TCodeHookHandle): Pointer; stdcall;
    function AdvancedHook(ATargetObject: Pointer;
      ATarget: Pointer; ATargetCallingConvention: Integer;
      AHook: Pointer; AHookCallingConvention: Integer;
      AParamCount: Integer;
      AExtraParams: PCardinal; AExtraParamCount: Integer;
      AFlags: Cardinal): TCodeHookHandle; stdcall;
    function AdvancedUnhook(AHandle: TCodeHookHandle): LongBool; stdcall;
    function AdvancedGetRealReturnAddress(AAddr: Pointer): Pointer; stdcall;
    function FindHookHandleFromTarget(ATarget: Pointer): TCodeHookHandle; stdcall;
    function CallPreviousMethod(AHandle: TCodeHookHandle; AParams: Pointer): Cardinal; stdcall;
    function CallMethod(AObject: Pointer; AMethod: Pointer;
      AParams: PCardinal; AParamCount: Integer;
      ACallingConvention: Integer): Cardinal; stdcall;

    property DirectCodeHook: TDirectCodeHook read FDirectCodeHook;
    property HandleList: TList read FHandleList;
    property UserDataSize: Integer read FUserDataSize;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TCodeHookHelper = class(TErrorCodeHookRedirect, ICodeHookHelper)
  private
    FTargetCC: Integer;
    FHookCC: Integer;
    FCodeHook: TCodeHook;
  protected
    function CheckCallingConvention: Boolean;

    function GetDelegator: TErrorCodeHook; override;

    { ICodeHookHelper }
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

    property TargetCC: Integer read FTargetCC;
    property HookCC: Integer read FHookCC;
  public
    constructor Create(ACodeHook: TCodeHook);
    destructor Destroy; override;
  end;

function GetCodeHookIntf: ICodeHook;

implementation

const
  ErrorMessages: array[0 .. HEC_COUNT - 1] of string = (
    'No error.', //HEC_NONE
    'Target function is too small.', //HEC_FUNCTION_TOO_SMALL
    'Unknown instruction.', //HEC_UNKNOWN_INSTRUCTION
    'Address %p is read only.', //HEC_ADDRESS_CANNOT_WRITE
    'Invalid code hook handle', //HEC_INVALID_HANDLE
    'Invalid calling convention', //HEC_INVALID_CC
    'Too many extra parameters. The maximum count is %d.', //HEC_TOO_MANY_EXTRA_PARAMS
    'Extend information is unavailable.' //HEC_EXTEND_INFO_UNAVAILABLE
  );

  //code hook info flags
  CHIF_HAS_EXTEND_INFO = 1 shl 0;
  CHIF_HAS_USER_DATA = 1 shl 1;

var
  GCodeHook: TCodeHook = nil;

function CreateCodeHookIntf: TCodeHook;
begin
  Result := TCodeHook.Create;
end;

function GetCodeHookIntf: ICodeHook;
begin
  if GCodeHook = nil then
    GCodeHook := CreateCodeHookIntf;
  Result := GCodeHook;
  Result._AddRef;
end;

function _AllocateMemory(Size: Integer): Pointer;
begin
  GetMem(Result, Size)
end;

procedure _FreeMemory(P: Pointer);
begin
  FreeMem(P)
end;

function GetAbsoluteAddr(ACode: PByte; AInstOffset,
  AAddrSize: Integer): Cardinal;
begin
  Inc(ACode, AInstOffset);
  case AAddrSize of
    1:
      Result := Cardinal(ACode) + 1 + Cardinal(PByte(ACode)^);
    4:
      Result := Cardinal(ACode) + 4 + Cardinal(PCardinal(ACode)^);
    else
    begin
      Assert(False);
      Result := 0;
    end;
  end;
end;

function GetRelativeAddr(ACode: PByte; AInstOffset: Integer;
  AAddr: Cardinal): Integer;
begin
  Inc(ACode, AInstOffset);
  Result := Integer(AAddr) - (Integer(ACode) + 4);
end;

function VerifyCallingConvention(ACC: Integer): Integer;
begin
  Result := HEC_NONE;

  if (ACC < 0) or (ACC >= HCC_COUNT) then
    Result := HEC_INVALID_CC;
end;

{ TByteWriter }

procedure TByteWriter.WriteByte(V: Byte);
begin
  PByte(FBuffer)^ := V;
  Inc(FBuffer);
end;

procedure TByteWriter.WriteWord(V: Word);
begin
  PWord(FBuffer)^ := V;
  Inc(FBuffer, SizeOf(Word));
end;

procedure TByteWriter.WriteCardinal(V: Cardinal);
begin
  PCardinal(FBuffer)^ := V;
  Inc(FBuffer, SizeOf(Cardinal));
end;

procedure TByteWriter.WriteInteger(V: Integer);
begin
  PInteger(FBuffer)^ := V;
  Inc(FBuffer, SizeOf(Integer));
end;

procedure TByteWriter.WriteBuffer(V: PByte; ALen: Integer);
begin
  Move(V^, FBuffer^, ALen);
  Inc(FBuffer, ALen);
end;

procedure TByteWriter.WriteByteArray(V: array of Byte);
var
  I: Integer;
begin
  for I := Low(V) to High(V) do
  begin
    PByte(FBuffer)^ := V[I];
    Inc(FBuffer);
  end;
end;

{ TErrorCodeHook }

procedure TErrorCodeHook.ClearErrorCode;
begin
  SetErrorCode(HEC_NONE, []);
end;

function TErrorCodeHook.GetErrorCode: Cardinal;
begin
  Result := FErrorCode;
end;

function TErrorCodeHook.GetErrorMessage(AMessage: PChar;
  AMessageLen: Integer): Integer;
begin
  Result := Length(FErrorMsg);
  if Result >= AMessageLen then
    Result := AMessageLen - 1;
  if Result < 0 then
    Result := 0;
  if Result > 0 then
  begin
    Move(FErrorMsg[1], AMessage^, Result);
    Inc(AMessage, Result);
    AMessage^ := #0;
    Inc(Result);
  end;
end;

procedure TErrorCodeHook.SetErrorCode(AErrorCode: Cardinal;
  const AParams: array of const);
begin
  FErrorCode := AErrorCode;

  if FErrorCode < HEC_COUNT then
    FErrorMsg := ErrorMessages[FErrorCode]
  else
    FErrorMsg := Format('Unknow error code %d.', [ FErrorCode ]);
end;

{ TErrorCodeHookRedirect }

function TErrorCodeHookRedirect.GetErrorCode: Cardinal;
begin
  Result := GetDelegator.GetErrorCode;
end;

function TErrorCodeHookRedirect.GetErrorMessage(AMessage: PChar;
  AMessageLen: Integer): Integer;
begin
  Result := GetDelegator.GetErrorMessage(AMessage, AMessageLen);
end;

procedure TErrorCodeHookRedirect.SetErrorCode(AErrorCode: Cardinal;
  const AParams: array of const);
begin
  GetDelegator.SetErrorCode(AErrorCode, AParams);
end;

{ TDirectCodeHook }

constructor TDirectCodeHook.Create;
begin
  inherited;

  FDisassembler := TDisAsm.Create;
end;

destructor TDirectCodeHook.Destroy;
begin
  FreeAndNil(FDisassembler);

  inherited;
end;

function TDirectCodeHook.DisassembleInst(ACode: PByte;
  AInst: PDirectCodeHookInstruction): PByte;
var
  P: PByte;

  procedure CopyRawCode;
  begin
    Move(ACode^, AInst^.RawCode[0], AInst^.RawCodeLen);
  end;

  procedure GetCodeOp1Addr1;
  begin
    AInst^.RawCodeLen := 2;
    CopyRawCode;
    AInst^.JumpDestAddr := GetAbsoluteAddr(P, 1, 1);
  end;

  procedure GetCodeOp1Addr4;
  begin
    AInst^.RawCodeLen := 5;
    CopyRawCode;
    AInst^.JumpDestAddr := GetAbsoluteAddr(P, 1, 4);
  end;
begin
  FillChar(AInst^, SizeOf(TDirectCodeHookInstruction), 0);

  P := ACode;

  if (P^ and $f0) = $70 then //jcc rel8
  begin
    AInst^.InstType := itJCC;
    GetCodeOp1Addr1;
    AInst^.DestOpCodeLen := 2;
    AInst^.DestOpCode[0] := $0f;
    AInst^.DestOpCode[1] := $80 or (P^ and $0f);
  end
  else
  begin
    case P^ of
      $cf, //iret
      $c3, $cb, $c2, $ca: //ret
      begin
        AInst^.InstType := itBad;
      end;

      //jmp rel8
      //rewrite: jmp rel32
      $eb:
      begin
        AInst^.InstType := itJmp;
        GetCodeOp1Addr1;
        AInst^.DestOpCodeLen := 1;
        AInst^.DestOpCode[0] := $e9;
      end;

      //jmp rel32
  	  //don't rewrite
	    $e9:
      begin
        AInst^.InstType := itJmp;
        GetCodeOp1Addr4;
        AInst^.DestOpCodeLen := 1;
        AInst^.DestOpCode[0] := $e9;
      end;

      //jecxz rel8
      //rewrite
      //  jecxz rel8
      //  jmp rel8
      //  jmp rel32
      $e3:
      begin
        AInst^.InstType := itJECXZ;
        GetCodeOp1Addr1;
        AInst^.DestOpCodeLen := 2 + 2 + 1;

        //jecxz rel8
        AInst^.DestOpCode[0] := $e3;
        AInst^.DestOpCode[1] := 2;

        //jmp rel8
        AInst^.DestOpCode[2] := $eb;
        AInst^.DestOpCode[3] := 5;

        //jmp rel32
        AInst^.DestOpCode[4] := $e9;
      end;

      //call rel32
      //don't rewrite
      $e8:
      begin
        AInst^.InstType := itCall;
        GetCodeOp1Addr4;
        AInst^.DestOpCodeLen := 1;
        AInst^.DestOpCode[0] := $e8;
      end;

      else
      begin
        AInst^.InstType := itCopy;
        AInst^.RawCodeLen := GetInstructionLength(P);
        AInst^.DestOpCodeLen := AInst^.RawCodeLen;
        CopyRawCode;
      end;
    end;
  end;

  Result := P;
  Inc(Result, AInst^.RawCodeLen);
end;

function TDirectCodeHook.GetInstructionLength(ACode: PByte): Integer;
begin
  Disassembler.Disassemble(ACode, 1);

  Assert(Disassembler.Count = 1);

  Result := Disassembler.Instructions[0].Code.size;
end;

function TDirectCodeHook.Hook(ATarget, AHook: Pointer; APreviousMethod: Pointer): LongBool;
begin
  ClearErrorCode;

  FTarget := ATarget;

  FMinCodeLen := 5;
  FCodeLen := 0;
  FInstCount := 0;
  SetLength(FInsts, FMinCodeLen);

  Result := Disassemble;
  if not Result then
    Exit;

  Result := Fixup;
  if not Result then
    Exit;

  Result := OutputByteCode(APreviousMethod);
  if not Result then
    Exit;

  Result := RedirectCode(AHook);
  if not Result then
    Exit;
end;

function TDirectCodeHook.Unhook(ATarget, APreviousMethod: Pointer): LongBool;
var
  P: PByte;
  lCodeLen: Integer;
  lOldProtect: Cardinal;
begin
  ClearErrorCode;

  Result := False;

  P := APreviousMethod;
  if P^ <> $eb then
  begin
    SetErrorCode(HEC_UNKNOWN_INSTRUCTION, []);
    Exit;
  end;
  Inc(P);

  lCodeLen := P^;
  Inc(P);

  VirtualProtect(ATarget, lCodeLen, PAGE_READWRITE, @lOldProtect);

  Result := not IsBadWritePtr(ATarget, lCodeLen);
  if not Result then
  begin
    SetErrorCode(HEC_ADDRESS_CANNOT_WRITE, [ FTarget ]);
    Exit;
  end;

  Move(P^, ATarget^, lCodeLen);

  Result := True;
end;

function TDirectCodeHook.Disassemble: Boolean;
var
  P: PByte;
begin
  Result := True;

  P := FTarget;
  while True do
  begin
    P := DisassembleInst(P, @FInsts[FInstCount]);

    if (FInsts[FInstCount].RawCodeLen = 0) or (FInsts[FInstCount].InstType = itBad) then
    begin
      if FInsts[FInstCount].InstType = itBad then
        SetErrorCode(HEC_FUNCTION_TOO_SMALL, [])
      else
        SetErrorCode(HEC_UNKNOWN_INSTRUCTION, []);
      Result := False;
      Exit;
    end;

    Inc(FCodeLen, FInsts[FInstCount].RawCodeLen);
    Inc(FInstCount);

    if FCodeLen >= FMinCodeLen then
      Break;
  end;
end;

function TDirectCodeHook.Fixup: Boolean;
var
  I, K, H: Integer;
  lAddr: Cardinal;
  lInst: PDirectCodeHookInstruction;
begin
  Result := True;

  for I := 0 to FInstCount - 1 do
  begin
    lInst := @FInsts[I];
    case lInst^.InstType of
      itCopy:
      begin
      end;

      itJCC, itJmp, itJECXZ, itCall:
      begin
        if (lInst^.JumpDestAddr > Cardinal(FTarget))
          and (lInst^.JumpDestAddr <= Cardinal(FTarget) + FCodeLen) then
        begin
          lAddr := Cardinal(FTarget);
          for K := 0 to FInstCount - 1 do
          begin
            if lAddr = lInst^.JumpDestAddr then
            begin
              lInst^.JumpDestAddr := Cardinal(FTarget);
              for H := 0 to K do
              begin
                Inc(lInst^.JumpDestAddr, FInsts[H].DestOpCodeLen);
              end;

              Break;
            end;

            Inc(lAddr, FInsts[K].RawCodeLen);
          end;
        end;
      end;
    end;
  end;
end;

function TDirectCodeHook.OutputByteCode(ADest: PByte): Boolean;
var
  I: Integer;
  lInst: PDirectCodeHookInstruction;
  lWriter: TByteWriter;
begin
  Result := True;

  lWriter := TByteWriter.Create;
  try
    lWriter.Buffer := ADest;

    lWriter.WriteByte($eb); //jmp rel8
    lWriter.WriteByte(FCodeLen);
    lWriter.WriteBuffer(FTarget, FCodeLen);

    for I := 0 to FInstCount - 1 do
    begin
      lInst := @FInsts[I];
      case lInst^.InstType of
        itCopy:
        begin
          lWriter.WriteBuffer(@lInst^.RawCode[0], lInst^.RawCodeLen);
        end;

        itJCC, itJmp, itJECXZ, itCall:
        begin
          lWriter.WriteBuffer(@lInst^.DestOpCode[0], lInst^.DestOpCodeLen);
          lWriter.WriteInteger(GetRelativeAddr(lWriter.Buffer, 0, lInst^.JumpDestAddr));
        end;
      end;
    end;
    lWriter.WriteByte($e9);
    lWriter.WriteInteger(GetRelativeAddr(lWriter.Buffer, 0, Cardinal(FTarget) + FCodeLen));
  finally
    lWriter.Free;
  end;
end;

function TDirectCodeHook.RedirectCode(AHook: Pointer): Boolean;
var
  lOldProtect: Cardinal;
begin
  VirtualProtect(FTarget, FMinCodeLen, PAGE_READWRITE, @lOldProtect);

  Result := not IsBadWritePtr(FTarget, FMinCodeLen);
  if not Result then
  begin
    SetErrorCode(HEC_ADDRESS_CANNOT_WRITE, [ FTarget ]);
    Exit;
  end;

  FTarget^ := $e9;
  PInteger(Cardinal(FTarget) + 1)^ := GetRelativeAddr(FTarget, 1, Cardinal(AHook));
end;

function TDirectCodeHook.AllocatePreviousMethodMemory: Pointer;
begin
  Result := _AllocateMemory(MIN_HOOK_PREVIOUSMETHOD_LEN);
end;

procedure TDirectCodeHook.FreePreviousMethodMemory(APreviousMethod: Pointer);
begin
  _FreeMemory(APreviousMethod);
end;

{ TCodeHook }

constructor TCodeHook.Create;
begin
  inherited;

  FHandleList := TList.Create;
  FDirectCodeHook := TDirectCodeHook.Create;
  SetUserDataSize(0);
end;

destructor TCodeHook.Destroy;
begin
  if Self = GCodeHook then
    GCodeHook := nil;

  ClearHooks;

  FCodeHookHelper := nil;
  FreeAndNil(FDirectCodeHook);
  FreeAndNil(FHandleList);

  inherited;
end;

function TCodeHook.FindHookHandleFromTarget(
  ATarget: Pointer): TCodeHookHandle;
var
  I: Integer;
  lInfo: PExtendCodeHookInfo;
begin
  DirectCodeHook.ClearErrorCode;

  for I := HandleList.Count - 1 downto 0 do
  begin
    Result := TCodeHookHandle(HandleList[I]);
    lInfo := GetHookInfoFromHandle(Result);
    if lInfo^.Target = ATarget then
      Exit;
  end;

  DirectCodeHook.SetErrorCode(HEC_INVALID_HANDLE, []);
  Result := INVALID_CODEHOOK_HANDLE;
end;

procedure TCodeHook.GetDirectCodeHook(out Obj); stdcall;
begin
  DirectCodeHook.ClearErrorCode;

  Pointer(Obj) := DirectCodeHook;
  IInterface(Obj)._AddRef;
end;

procedure TCodeHook.GetCodeHookHelper(out Obj); stdcall;
begin
  DirectCodeHook.ClearErrorCode;

  if FCodeHookHelper = nil then
    FCodeHookHelper := TCodeHookHelper.Create(Self);

  Pointer(Obj) := Pointer(FCodeHookHelper);
  IInterface(Obj)._AddRef;
end;

procedure TCodeHook.CreateCodeHook(out Obj);
begin
  Pointer(Obj) := Pointer(CreateCodeHookIntf);
end;

function TCodeHook.Hook(ATarget, AHook: Pointer): TCodeHookHandle;
var
  lInfo: PExtendCodeHookInfo;
begin
  DirectCodeHook.ClearErrorCode;

  Result := FindHookHandleFromTargetAndHook(ATarget, AHook);
  if Result <> INVALID_CODEHOOK_HANDLE then
    Unhook(Result);

  lInfo := AllocateHookInfo(Result);
  lInfo^.Flags := lInfo^.Flags and (not CHIF_HAS_EXTEND_INFO);
  lInfo^.Target := ATarget;
  lInfo^.Hook := AHook;
  if not DirectCodeHook.Hook(ATarget, AHook, @lInfo^.PreviousMethod[0]) then
  begin
    FreeHookInfo(Result);
    Result := INVALID_CODEHOOK_HANDLE;
  end
  else
    HandleList.Add(Pointer(Result));
end;

function TCodeHook.Unhook(AHandle: TCodeHookHandle): LongBool;
var
  lInfo: PExtendCodeHookInfo;
begin
  DirectCodeHook.ClearErrorCode;

  Result := False;
  if AHandle = INVALID_CODEHOOK_HANDLE then
  begin
    SetErrorCode(HEC_INVALID_HANDLE, []);
    Exit;
  end;

  lInfo := GetHookInfoFromHandle(AHandle);
  if lInfo = nil then
  begin
    SetErrorCode(HEC_INVALID_HANDLE, []);
    Exit;
  end;

  Result := DirectCodeHook.Unhook(lInfo^.Target, @lInfo^.PreviousMethod[0]);
  FreeHookInfo(AHandle);
end;

function TCodeHook.GetHookInfo(AHandle: TCodeHookHandle; AInfo: PCodeHookInfo): LongBool; stdcall;
var
  lInfo: PExtendCodeHookInfo;
begin
  DirectCodeHook.ClearErrorCode;

  lInfo := GetHookInfoFromHandle(AHandle);
  if lInfo = nil then
  begin
    DirectCodeHook.SetErrorCode(HEC_INVALID_HANDLE, []);

    Result := False;
    Exit;
  end;
  AInfo^.Target := lInfo^.Target;
  AInfo^.Hook := lInfo^.Hook;
  AInfo^.ExtendInfoAvailable := (lInfo^.Flags and CHIF_HAS_EXTEND_INFO) <> 0;
  AInfo^.TargetObject := lInfo^.TargetObject;
  AInfo^.ParamCount := lInfo^.ParamCount;
  AInfo^.TargetCallingConvention := lInfo^.TargetCallingConvention;
  AInfo^.HookCallingConvention := lInfo^.HookCallingConvention;
  AInfo^.PreviousMethod := @lInfo^.PreviousMethod[0];
  AInfo^.UserData := @lInfo^.UserDataPlaceHolder;
  if (lInfo^.Flags and CHIF_HAS_USER_DATA) = 0 then
    AInfo^.UserData := nil;

  Result := True;
end;

function TCodeHook.GetPreviousMethod(AHandle: TCodeHookHandle): Pointer;
var
  lInfo: PExtendCodeHookInfo;
begin
  DirectCodeHook.ClearErrorCode;

  lInfo := GetHookInfoFromHandle(AHandle);
  if lInfo = nil then
  begin
    DirectCodeHook.SetErrorCode(HEC_INVALID_HANDLE, []);

    Result := nil;
    Exit;
  end;

  Result := @lInfo^.PreviousMethod;
end;

function TCodeHook.GetUserData(AHandle: TCodeHookHandle): Pointer;
var
  lInfo: PExtendCodeHookInfo;
begin
  DirectCodeHook.ClearErrorCode;

  lInfo := GetHookInfoFromHandle(AHandle);
  if lInfo = nil then
  begin
    DirectCodeHook.SetErrorCode(HEC_INVALID_HANDLE, []);

    Result := nil;
    Exit;
  end;

  if (lInfo^.Flags and CHIF_HAS_USER_DATA) = 0 then
    Result := nil
  else
    Result := @lInfo^.UserDataPlaceHolder;
end;

{ Hook Callback prototype,
function HookCallback(AExtraParam1: Cardinal; AExtraParam2: Cardinal .. AExtraParamN: Cardinal, AHandle: TCodeHookHandle; AParams: PCodeHookParamAccessor): Cardinal; CallingConvertion;
if no extra parameters, the prototype is,
function HookCallback(AHandle: TCodeHookHandle; AParams: PCodeHookParamAccessor): Cardinal; CallingConvertion;
}
function TCodeHook.AdvancedHook(ATargetObject: Pointer;
  ATarget: Pointer; ATargetCallingConvention: Integer;
  AHook: Pointer; AHookCallingConvention: Integer;
  AParamCount: Integer;
  AExtraParams: PCardinal; AExtraParamCount: Integer;
  AFlags: Cardinal): TCodeHookHandle;
var
  lInfo: PExtendCodeHookInfo;
  lSuccess: Boolean;
  lParamCount: Integer;
begin
  DirectCodeHook.ClearErrorCode;

  if AExtraParamCount > MAX_EXTRA_PARAM_COUNT then
  begin
    SetErrorCode(HEC_TOO_MANY_EXTRA_PARAMS, [ MAX_EXTRA_PARAM_COUNT ]);
    Result := INVALID_CODEHOOK_HANDLE;
    Exit;
  end;

  DirectCodeHook.ClearErrorCode;
  
  Result := FindHookHandleFromTargetAndHook(ATarget, AHook);
  if Result <> INVALID_CODEHOOK_HANDLE then
    Unhook(Result);

  lInfo := AllocateHookInfo(Result);
  lInfo^.Flags := lInfo^.Flags or CHIF_HAS_EXTEND_INFO;
  lInfo^.TargetObject := ATargetObject;
  lInfo^.Target := ATarget;
  lInfo^.Hook := AHook;
  lInfo^.ParamCount := AParamCount;
  lInfo^.TargetCallingConvention := ATargetCallingConvention;
  lInfo^.HookCallingConvention := AHookCallingConvention;
  if AExtraParamCount > 0 then
    Move(AExtraParams^, lInfo^.ExtraParams[0], AExtraParamCount * SizeOf(Cardinal));

  lParamCount := AParamCount;

  lSuccess := MakeThumbCode(ATargetObject, Result,
    ATarget, ATargetCallingConvention, AHook, AHookCallingConvention,
    lParamCount, @lInfo^.ExtraParams[0], AExtraParamCount);

  if lSuccess then
  begin
    lSuccess := DirectCodeHook.Hook(ATarget, @lInfo^.HookThumbCode[0],
      @lInfo^.PreviousMethod[0]);
  end;
  if not lSuccess then
  begin
    FreeHookInfo(Result);
//    Result := INVALID_CODEHOOK_HANDLE;
  end
  else
    HandleList.Add(Pointer(Result));
end;

function TCodeHook.AdvancedUnhook(AHandle: TCodeHookHandle): LongBool;
var
  lInfo: PExtendCodeHookInfo;
  I: Integer;
begin
  DirectCodeHook.ClearErrorCode;

  if AHandle = INVALID_CODEHOOK_HANDLE then
  begin
    Result := False;
    DirectCodeHook.SetErrorCode(HEC_INVALID_HANDLE, []);
    Exit;
  end;

  I := HandleList.Remove(Pointer(AHandle));
  if I >= 0 then
  begin
    lInfo := GetHookInfoFromHandle(AHandle);
    Result := DirectCodeHook.Unhook(lInfo^.Target, @lInfo^.PreviousMethod[0]);
  end
  else
  begin
    Result := False;
    DirectCodeHook.SetErrorCode(HEC_INVALID_HANDLE, []);
  end;
end;

function TCodeHook.AdvancedGetRealReturnAddress(AAddr: Pointer): Pointer;
asm
  mov eax, AAddr
  cmp [eax], HookCallInstSignature0
  jnz @@NotHooked
  cmp [eax + 4], HookCallInstSignature1
  jnz @@NotHooked
  cmp [eax + 8], HookCallInstSignature2
  jnz @@NotHooked
  mov eax, [eax + $c]
@@NotHooked:
end;

function TCodeHook.CallPreviousMethod(AHandle: TCodeHookHandle;
  AParams: Pointer): Cardinal;
var
  lInfo: PExtendCodeHookInfo;
begin
  DirectCodeHook.ClearErrorCode;

  lInfo := GetHookInfoFromHandle(AHandle);

  if (lInfo^.Flags and CHIF_HAS_EXTEND_INFO) <> 0 then
    Result := CallMethod(lInfo^.TargetObject, @lInfo^.PreviousMethod,
      AParams, lInfo^.ParamCount, lInfo^.TargetCallingConvention)
  else
  begin
    SetErrorCode(HEC_EXTEND_INFO_UNAVAILABLE, []);
    Result := 0;
  end;
end;

function TCodeHook.CallMethod(AObject: Pointer; AMethod: Pointer;
  AParams: PCardinal; AParamCount: Integer; ACallingConvention: Integer): Cardinal;
var
  lParamCount: Integer;
  I: Integer;
  lStack, lStackTop: Cardinal;
  lRegisters: array[0..2] of Cardinal;
  lRegisterCount: Integer;

  procedure PushD(AData: Cardinal);
  begin
    if (ACallingConvention = HCC_REGISTER) and (lRegisterCount < 3) then
    begin
      lRegisters[lRegisterCount] := AData;
      Inc(lRegisterCount);
    end
    else
    begin
      Dec(lStackTop, 4);
      PCardinal(lStackTop)^ := AData;
    end;
  end;

  procedure PushTargetObject;
  begin
    if AObject <> nil then
      PushD(Cardinal(AObject));
  end;
begin
  DirectCodeHook.ClearErrorCode;

  Result := 0;

  lParamCount := AParamCount;
  I := (lParamCount + 1) * 4;
  asm
    mov lStack, esp
    mov lStackTop, esp
    sub esp, I
  end;
  lRegisterCount := 0;

  if ACallingConvention = HCC_REGISTER then
  begin
    PushTargetObject;
    for I := 0 to lParamCount - 1 do
      PushD(PCardinal(Cardinal(AParams) + Cardinal(I) * SizeOf(Cardinal))^);
  end
  else
  begin
    for I := lParamCount - 1 downto 0 do
      PushD(PCardinal(Cardinal(AParams) + Cardinal(I) * SizeOf(Cardinal))^);
    PushTargetObject;
  end;

  asm
    mov esp, lStackTop
    mov eax, dword ptr lRegisters[0]
    mov edx, dword ptr lRegisters[4]
    mov ecx, dword ptr lRegisters[8]
    call AMethod
    mov esp, lStack
    Mov Result, eax
  end;
end;

function TCodeHook.MakeThumbCode(ATargetObject: Pointer; AHandle: TCodeHookHandle;
  ATarget: Pointer; ATargetCallingConvention: Integer;
  AHook: Pointer; AHookCallingConvention: Integer;
  AParamCount: Integer; AExtraParams: PCardinal;
  AExtraParamCount: Integer): Boolean;
const
  FrameESPSize = 4;
var
  lInfo: PExtendCodeHookInfo;
  lWriter: TByteWriter;
  lRetN: Integer;
  lParamOffset: Integer;
  lRegCallReverseCount: Integer;
  I: Integer;

  procedure PushD(V: Cardinal);
  begin
    lWriter.WriteByte($68);
    lWriter.WriteInteger(V);
  end;

  procedure WriteParamOffset;
  begin
    if ATargetCallingConvention = HCC_REGISTER then
      lWriter.WriteInteger(4 + lParamOffset * 4 + 0)
    else
      lWriter.WriteInteger(4 + lParamOffset * 4 + FrameESPSize);
  end;

  procedure MovEAX(V: Cardinal);
  begin
    lWriter.WriteByte($b8);
    lWriter.WriteInteger(V);
  end;

  procedure MovEDX(V: Cardinal);
  begin
    lWriter.WriteByte($ba);
    lWriter.WriteInteger(V);
  end;

  procedure MovECX(V: Cardinal);
  begin
    lWriter.WriteByte($b9);
    lWriter.WriteInteger(V);
  end;
begin
  Result := True;

  lParamOffset := 0;
  if ATargetObject <> nil then
    Inc(lParamOffset);

  lInfo := GetHookInfoFromHandle(AHandle);
  lWriter := TByteWriter.Create;
  try
    lWriter.Buffer := @lInfo^.HookThumbCode[0];

    // push ebp
    lWriter.WriteByte($55);
    // mov ebp, esp
    lWriter.WriteByteArray([ $8b, $ec ]);

    lRegCallReverseCount := 0;
    if ATargetCallingConvention = HCC_REGISTER then
    begin
      lRegCallReverseCount := AParamCount - 3 + lParamOffset;
      if lRegCallReverseCount > 0 then
      begin
        //reverse the stack
// Don't use following commented code, it may cause lWriter buffer overflow
// if there are too many parameters.
//        for I := 0 to lRegCallReverseCount - 1 do
//        begin
//          lWriter.WriteByteArray([ $ff, $B4, $24 ]);
//          lWriter.WriteInteger(4 + (I + I) * 4);
//        end;

        // sub esp, lRegCallReverseCount * 4
        lWriter.WriteByteArray([ $81, $ec ]);
        lWriter.WriteInteger(lRegCallReverseCount * 4);

        // push esi
        lWriter.WriteByte($56);
        // push edi
        lWriter.WriteByte($57);
        // push ecx
        lWriter.WriteByte($51);
        // push eax
        lWriter.WriteByte($50);

        // mov ecx, lRegCallReverseCount
        lWriter.WriteByte($b9);
        lWriter.WriteInteger(lRegCallReverseCount);

        // lea esi, [esp + ]
        lWriter.WriteByteArray([ $8d, $b4, $24 ]);
        lWriter.WriteInteger((4 + lRegCallReverseCount + 1 + lRegCallReverseCount - 1) * 4 + FrameESPSize);

        // lea edi, [esp + ]
        lWriter.WriteByteArray([ $8d, $bc, $24 ]);
        lWriter.WriteInteger(4 * 4);
        
        // loop
        // mov eax, [esi]
        lWriter.WriteByteArray([ $8b, $06 ]);
        // mov [edi], eax
        lWriter.WriteByteArray([ $89, $07 ]);
        // sub esi, 4
        lWriter.WriteByteArray([ $83, $ee, $04 ]);
        // add edi, 4
        lWriter.WriteByteArray([ $83, $c7, $04 ]);
        // dec ecx
        lWriter.WriteByte($49);
        // jnz back
        lWriter.WriteByte($75);
        lWriter.WriteByte(Byte(-13));

        // pop eax
        lWriter.WriteByte($58);
        // pop ecx
        lWriter.WriteByte($59);
        // pop edi
        lWriter.WriteByte($5f);
        // pop esi
        lWriter.WriteByte($5e);
      end;

      //push ecx
      lWriter.WriteByte($51);

      //push edx
      lWriter.WriteByte($52);

      //push eax
      lWriter.WriteByte($50);

      //fake return address
      lWriter.WriteByte($68);
      lWriter.WriteInteger(0);
    end;

    if AHookCallingConvention = HCC_REGISTER then
    begin
      case AExtraParamCount of
        0:
        begin
          // mov eax, AHandle
          MovEAX(Integer(AHandle));

          // lea edx, [esp + 4 + lParamOffset * 4]
          lWriter.WriteByteArray([ $8d, $94, $24 ]);
          WriteParamOffset;
        end;

        1:
        begin
          // mov eax, AExtraParams^
          MovEAX(AExtraParams^);

          // mov edx, AHandle
          MovEDX(Integer(AHandle));

          // lea ecx, [esp + 4 + lParamOffset * 4]
          lWriter.WriteByteArray([ $8d, $8c, $24 ]);
          WriteParamOffset;
        end;

        2:
        begin
          // lea eax, [esp + 4 + lParamOffset * 4]; get the params
          lWriter.WriteByteArray([ $8d, $84, $24 ]);
          WriteParamOffset;

          // push eax
          lWriter.WriteByte($50);

          // mov eax, AExtraParams^
          MovEAX(AExtraParams^);

          // mov edx, (AExtraParams + 4)^
          MovEDX(PCardinal(Cardinal(AExtraParams) + 4)^);

          // mov ecx, AHandle
          MovECX(Integer(AHandle));
        end;

        else
        begin
          for I := 3 to AExtraParamCount - 1 do
            PushD(PCardinal(Cardinal(AExtraParams) + Cardinal(I) * SizeOf(Cardinal))^);

          PushD(Integer(AHandle));

          // lea eax, [esp + 4 + lParamOffset * 4]; get the params
          lWriter.WriteByteArray([ $8d, $84, $24 ]);
          lParamOffset := AExtraParamCount - 3 + 1;
          if ATargetObject <> nil then
            Inc(lParamOffset);
          WriteParamOffset;

          // push eax
          lWriter.WriteByte($50);

          // mov eax, AExtraParams^
          MovEAX(AExtraParams^);

          // mov edx, (AExtraParams + 4)^
          MovEDX(PCardinal(Cardinal(AExtraParams) + 4)^);

          // mov ecx, (AExtraParams + 8)^
          MovECX(PCardinal(Cardinal(AExtraParams) + 8)^);
        end;
      end;
    end
    else
    begin
      // lea eax, [esp + 4 + AExtraParamCount * 4]; get the params
      lWriter.WriteByteArray([ $8d, $84, $24 ]);
      WriteParamOffset;

      // push eax
      lWriter.WriteByte($50);

      PushD(Integer(AHandle));

      for I := AExtraParamCount - 1 downto 0 do
        PushD(PCardinal(Cardinal(AExtraParams) + Cardinal(I) * SizeOf(Cardinal))^);
    end;

    //call hook
    lWriter.WriteByte($e8);
    lWriter.WriteInteger(GetRelativeAddr(lWriter.Buffer, 0, Cardinal(AHook)));

    lWriter.WriteCardinal(HookCallInstSignature0);
    lWriter.WriteCardinal(HookCallInstSignature1);
    lWriter.WriteCardinal(HookCallInstSignature2);
    lWriter.WriteCardinal(Cardinal(ATarget));

    if AHookCallingConvention = HCC_CDECL then
    begin
      // add esp, params; clear stack
      lWriter.WriteByteArray([ $81, $c4 ]);
      lWriter.WriteInteger((AExtraParamCount + 2) * 4);
    end;

    if ATargetCallingConvention = HCC_REGISTER then
    begin
      lWriter.WriteByteArray([ $81, $c4 ]);
      if lRegCallReverseCount < 0 then
        lRegCallReverseCount := 0;
      lWriter.WriteInteger(12 + lRegCallReverseCount * 4 + 4);
    end;

    // pop ebp
    lWriter.WriteByte($5d);

    lRetN := AParamCount;
    if ATargetObject <> nil then
      Inc(lRetN);
    if ATargetCallingConvention = HCC_REGISTER then
    begin
      if lRetN < 3 then
      begin
        // ret
        lWriter.WriteByte($c3);
      end
      else
      begin
        // ret n
        lWriter.WriteByte($c2);
        lWriter.WriteWord((lRetN - 3) * 4);
      end;
    end
    else
    begin
      if ATargetCallingConvention = HCC_CDECL then
      begin
        // ret
        lWriter.WriteByte($c3);
      end
      else
      begin
        // ret n
        lWriter.WriteByte($c2);
        lWriter.WriteWord(lRetN * 4);
      end;
    end;
  finally
    lWriter.Free;
  end;
end;

procedure TCodeHook.ClearHooks;
var
  I: Integer;
begin
  for I := HandleList.Count - 1 downto 0 do
  begin
    AdvancedUnhook(TCodeHookHandle(HandleList[I]));
  end;
  HandleList.Clear;
end;

function TCodeHook.AllocateHookInfo(
  var AHandle: TCodeHookHandle): PExtendCodeHookInfo;
var
  lLen: Integer;
begin
  lLen := SizeOf(TExtendCodeHookInfo) + UserDataSize;
  Result := _AllocateMemory(lLen);
  AHandle := Result;
  ZeroMemory(Result, lLen);
  if UserDataSize > 0 then
    Result^.Flags := CHIF_HAS_USER_DATA;
end;

procedure TCodeHook.FreeHookInfo(AHandle: TCodeHookHandle);
begin
  HandleList.Remove(Pointer(AHandle));
  _FreeMemory(AHandle);
end;

function TCodeHook.GetHookInfoFromHandle(
  AHandle: TCodeHookHandle): PExtendCodeHookInfo;
begin
  Result := AHandle;
end;

procedure TCodeHook.SetUserDataSize(ASize: Integer);
begin
  DirectCodeHook.ClearErrorCode;

  FUserDataSize := ASize;
  if FUserDataSize < 0 then
    FUserDataSize := 0;
end;

function TCodeHook.GetDelegator: TErrorCodeHook;
begin
  Result := DirectCodeHook;
end;

function TCodeHook.FindHookHandleFromTargetAndHook(ATarget,
  AHook: Pointer): TCodeHookHandle;
var
  I: Integer;
  lInfo: PExtendCodeHookInfo;
begin
  for I := HandleList.Count - 1 downto 0 do
  begin
    Result := TCodeHookHandle(HandleList[I]);
    lInfo := GetHookInfoFromHandle(Result);
    if (lInfo^.Target = ATarget) and (lInfo^.Hook = AHook) then
      Exit;
  end;
  Result := INVALID_CODEHOOK_HANDLE;
end;

{ TCodeHookHelper }

const HCC_INVALID = -1;

constructor TCodeHookHelper.Create(ACodeHook: TCodeHook);
begin
  inherited Create;

  FCodeHook := ACodeHook;

  FTargetCC := HCC_INVALID;
  FHookCC := HCC_INVALID;
end;

destructor TCodeHookHelper.Destroy;
begin
  FCodeHook := nil;

  inherited;
end;

function TCodeHookHelper.CheckCallingConvention: Boolean;
var
  lErrorCode: Integer;
begin
  Result := True;

  lErrorCode := VerifyCallingConvention(FTargetCC);
  if lErrorCode <> HEC_NONE then
  begin
    Result := False;
    SetErrorCode(lErrorCode, []);
    Exit;
  end;

  lErrorCode := VerifyCallingConvention(FTargetCC);
  if lErrorCode <> HEC_NONE then
  begin
    Result := False;
    SetErrorCode(lErrorCode, []);
  end;
end;

function TCodeHookHelper.GetDelegator: TErrorCodeHook;
begin
  Result := FCodeHook.DirectCodeHook;
end;

function TCodeHookHelper.HookWithGlobalMethod(ATargetObject: Pointer;
  ATarget, AHook: Pointer; AParamCount: Integer; AFlags: Cardinal): TCodeHookHandle;
begin
  Result := INVALID_CODEHOOK_HANDLE;
  if not CheckCallingConvention then
    Exit;

  Result := FCodeHook.AdvancedHook(ATargetObject, ATarget, FTargetCC,
    AHook, FHookCC, AParamCount, nil, 0, AFlags);
end;

function TCodeHookHelper.HookWithObjectMethod(ATargetObject, AHookObject, ATarget,
  AHook: Pointer; AParamCount: Integer; AFlags: Cardinal): TCodeHookHandle;
var
  lExtraParam: Cardinal;
begin
  Result := INVALID_CODEHOOK_HANDLE;
  if not CheckCallingConvention then
    Exit;

  lExtraParam := Cardinal(AHookObject);
  Result := FCodeHook.AdvancedHook(ATargetObject, ATarget, FTargetCC,
    AHook, FHookCC, AParamCount, @lExtraParam, 1, AFlags);
end;

function TCodeHookHelper.HookWithGlobalMethodExtra(ATargetObject, ATarget,
  AHook: Pointer; AParamCount: Integer; AExtraParams: PCardinal;
  AExtraParamCount: Integer; AFlags: Cardinal): TCodeHookHandle;
begin
  Result := INVALID_CODEHOOK_HANDLE;
  if not CheckCallingConvention then
    Exit;

  Result := FCodeHook.AdvancedHook(ATargetObject, ATarget, FTargetCC,
    AHook, FHookCC, AParamCount, AExtraParams, AExtraParamCount, AFlags);
end;

function TCodeHookHelper.HookWithObjectMethodExtra(ATargetObject,
  AHookObject, ATarget, AHook: Pointer; AParamCount: Integer;
  AExtraParams: PCardinal; AExtraParamCount: Integer; AFlags: Cardinal): TCodeHookHandle;
var
  lExtraParam: array of Cardinal;
begin
  Result := INVALID_CODEHOOK_HANDLE;
  if not CheckCallingConvention then
    Exit;

  SetLength(lExtraParam, AExtraParamCount + 1);
  lExtraParam[0] := Cardinal(AHookObject);
  if AExtraParamCount > 0 then
    Move(AExtraParams^, lExtraParam[1], AExtraParamCount * SizeOf(Cardinal));
  Result := FCodeHook.AdvancedHook(ATargetObject, ATarget, FTargetCC,
    AHook, FHookCC, AParamCount, @lExtraParam[0], AExtraParamCount + 1, AFlags);
end;

procedure TCodeHookHelper.SetCallingConvention(ATargetCC,
  AHookCC: Integer);
begin
  FTargetCC := ATargetCC;
  FHookCC := AHookCC;
end;

function TCodeHookHelper.UnhookTarget(ATarget: Pointer): LongBool;
begin
  Result := FCodeHook.Unhook(FCodeHook.FindHookHandleFromTarget(ATarget));
end;

procedure TCodeHookHelper.UnhookAll;
begin
  FCodeHook.ClearHooks;
end;

initialization
finalization
  if GCodeHook <> nil then
    GCodeHook.ClearHooks;
  GCodeHook := nil;

end.

