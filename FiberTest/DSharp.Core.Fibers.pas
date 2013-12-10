(*
  Copyright (c) 2011, Stefan Glienke
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  - Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  - Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  - Neither the name of this library nor the names of its contributors may be
    used to endorse or promote products derived from this software without
    specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
*)

unit DSharp.Core.Fibers;

{$IFNDEF MSWINDOWS}
{$MESSAGE WARN 'Unit only supports Windows'}
{$ENDIF}

interface

uses
  Classes,
  SysUtils;

type
  TFiber = class(TInterfacedObject)
  strict private
    FBaseHandle: Pointer;
    FException: Exception;
    FFinished: Boolean;
    FFreeOnTerminate: Boolean;
    FHandle: Pointer;
    FThread: TThread;
  private
    procedure Run;
  strict protected
    procedure Execute; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    class function CurrentFiber: TFiber;
    class procedure Initialize;

    procedure HandleException;

    procedure Resume;
    procedure Yield;

    procedure Synchronize(AThreadProc: TThreadProcedure);
    procedure SwitchToMainThread;
    procedure SwitchToWorkerThread;

    property FreeOnTerminate: Boolean
      read FFreeOnTerminate write FFreeOnTerminate;
    property Finished: Boolean read FFinished;
  end;

  TActionFiber = class(TFiber)
  strict private
    FAction: TProc;
    FThreaded: Boolean;
  strict protected
    procedure Execute; override;
    property Action: TProc read FAction write FAction;
    property Threaded: Boolean read FThreaded write FThreaded;
  public
    constructor Create(const AAction: TProc; const AThreaded: Boolean = True);
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;

{$REGION 'Fiber Functions'}
function ConvertFiberToThread: Pointer; stdcall; external kernel32;
function ConvertThreadToFiber(lpParameter: Pointer): Pointer; stdcall; external kernel32;
function CreateFiber(dwStackSize: Cardinal; lpStartAddress: Pointer;
  lpParameter: Pointer): Pointer; stdcall; external kernel32;
function DeleteFiber(lpFiber: Pointer): Boolean; stdcall; external kernel32;
function SwitchToFiber(lpFiber: Pointer): Boolean; stdcall; external kernel32;
{$ENDIF}

function GetCurrentFiber: Pointer;
asm
{$IFDEF CPUX64}
//  MOV RAX, GS[$20] // BASM64 generates wrong code => $20 becomes image00000000_00400000+0x25af9
  DB $65, $48, $8B, $04, $25, $20, $00, $00, $00
{$ELSE}
  MOV EAX, FS:[$10]
{$ENDIF}
end;

function GetFiberData: Pointer; inline;
begin
{$IFDEF MSWINDOWS}
  Result := PPointer(GetCurrentFiber()^);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

procedure GlobalStartFiber;
var
  LFiber: TFiber;
begin
  LFiber := TFiber.CurrentFiber;
  LFiber.Run;
end;
{$ENDREGION}

type
  TFiberThread = class(TThread)
  strict private
    FFiber: TFiber;
  strict protected
    procedure Execute; override;
  public
    constructor Create(const AFiber: TFiber);
  end;

threadvar
  FiberCount: Integer;
  ThreadFiber: Pointer;

var
  MainFiber: Pointer;

{ TFiber }

constructor TFiber.Create;
begin
  Initialize;
  FHandle := nil;
{$IFDEF MSWINDOWS}
  FHandle := CreateFiber(0, @GlobalStartFiber, Self);
{$ENDIF}
  FBaseHandle := GetCurrentFiber;
end;

destructor TFiber.Destroy;
begin
  Dec(FiberCount);
  if not FFreeOnTerminate then
  begin
{$IFDEF MSWINDOWS}
    DeleteFiber(FHandle);
    if Assigned(ThreadFiber) and (FiberCount = 0) then
    begin
      ConvertFiberToThread;
      ThreadFiber := nil;
    end;
{$ENDIF}
  end;
  inherited;
end;

class function TFiber.CurrentFiber: TFiber;
begin
  if FiberCount > 0 then
  begin
    Result := TFiber(GetFiberData);
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TFiber.HandleException;
var
  E: Exception;
begin
  if FException <> nil then
  begin
    E := FException;
    FException := nil;
    raise E;
  end;
end;

class procedure TFiber.Initialize;
begin
  if not Assigned(ThreadFiber) then
  begin
{$IFDEF MSWINDOWS}
    ThreadFiber := ConvertThreadToFiber(nil);
{$ENDIF}
    if not Assigned(MainFiber) then
    begin
      MainFiber := ThreadFiber;
    end;
  end;
  Inc(FiberCount);
end;

procedure TFiber.Resume;
begin
  if not FFinished then
  begin
{$IFDEF MSWINDOWS}
    SwitchToFiber(FHandle);
{$ENDIF}
  end;
end;

procedure TFiber.Run;
begin
  try
    Execute;
  except
    on EAbort do;
    on Exception do
      FException := AcquireExceptionObject;
  end;
  FFinished := True;
  if FFreeOnTerminate then
  begin
    Free;
  end;
  SwitchToMainThread;
  Yield;
end;

procedure TFiber.SwitchToMainThread;
begin
{$IFDEF MSWINDOWS}
  if Assigned(FThread) and (GetCurrentThreadId = FThread.ThreadID) then
  begin
    FThread.Terminate;
    FThread := nil;
    SwitchToFiber(ThreadFiber);
  end;
{$ENDIF}
end;

procedure TFiber.SwitchToWorkerThread;
begin
{$IFDEF MSWINDOWS}
  if GetCurrentThreadID = MainThreadID then
  begin
    FThread := TFiberThread.Create(Self);
    FThread.FreeOnTerminate := True;
    Yield;
  end;
{$ENDIF}
end;

procedure TFiber.Synchronize(AThreadProc: TThreadProcedure);
begin
  TThread.Synchronize(FThread, AThreadProc);
end;

procedure TFiber.Yield;
begin
{$IFDEF MSWINDOWS}
  SwitchToFiber(FBaseHandle);
{$ENDIF}
end;

{ TFiberThread }

constructor TFiberThread.Create(const AFiber: TFiber);
begin
  inherited Create(False);
  FFiber := AFiber;
end;

procedure TFiberThread.Execute;
begin
  TFiber.Initialize;
  while not Terminated do
  begin
    FFiber.Resume;
  end;
  TThread.Synchronize(nil, FFiber.Resume);
end;

{ TActionFiber }

procedure TActionFiber.Execute;
begin
  if FThreaded then
  begin
    SwitchToWorkerThread;
  end;
  FAction();
  if FThreaded then
  begin
    SwitchToMainThread;
  end;
end;

constructor TActionFiber.Create(const AAction: TProc; const AThreaded: Boolean);
begin
  inherited Create();
  Action := AAction;
  Threaded := AThreaded;
  FreeOnTerminate := True;
end;

end.
