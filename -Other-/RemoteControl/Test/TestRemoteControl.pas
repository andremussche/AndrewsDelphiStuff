unit TestRemoteControl;

interface

uses
  TestFramework, SysUtils, Windows, Forms, Dialogs, Controls, Classes,
  Messages, Variants, Graphics, StdCtrls,
  JvCreateProcess;

type
  TTestRemoteControl = class(TTestCase)
  private
    class var FRemoteableApp: TJvCreateProcess;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure StartRemoteApp;
    procedure ConnectRemoteApp;

    procedure DoSomeTest;

    procedure StopRemoteApp;
  end;

implementation

uses
  DateUtils, mcRemoteApp, RemoteConnector;

procedure TTestRemoteControl.ConnectRemoteApp;
begin
  dmRemoteConnector.IdTCPClient1.Disconnect;
  dmRemoteConnector.IdTCPClient1.Host := 'localhost';
  dmRemoteConnector.IdTCPClient1.Port := 12345;
  dmRemoteConnector.IdTCPClient1.Connect;
end;

procedure TTestRemoteControl.DoSomeTest;
begin
  RemoteApp.frmMain.btnShowModal.Click;
  Sleep(1000); //visible delay for demo

  RemoteApp.frmTest.framTest1.CheckExists(15 * 1000);   //client has "message idle lock" for 10s, we wait 15s to test the "wait for idle" code
  RemoteApp.frmTest.framTest1.Edit1.CheckCanFocus;
  RemoteApp.frmTest.actOk.CheckEnabled;

    Check( RemoteApp.frmTest.framTest1.Exists );
    Check( RemoteApp.frmTest.framTest1.Edit1.CanFocus );
    RemoteApp.frmTest.framTest1.Edit1.Text := 'test';
    CheckEqualsString( RemoteApp.frmTest.framTest1.Edit1.Text, 'test' );
    Sleep(1000);  //visible delay
    RemoteApp.frmTest.framTest1.Button1.Click;
    CheckEqualsString( RemoteApp.frmTest.framTest1.Button1.Caption, 'Button clicked!' );
    Sleep(1000);  //visible delay
    RemoteApp.frmTest.btnOk.Click;
    Check( not RemoteApp.frmTest.Exists );

  RemoteApp.frmMain.btnShowFloating.Click;
  Sleep(1000);   //visible delay
  RemoteApp.frmMain.btnShowFloating.Click;
  Sleep(1000);
    Check( RemoteApp.frmTest.framTest1.Exists );
    RemoteApp.frmTest.framTest1.Edit1.Text := 'test';
    Sleep(1000);  //visible delay
    RemoteApp.frmTest.framTest1.Button1.Click;
    Sleep(1000);  //visible delay
    RemoteApp.frmTest.btnOk.Click;
    Sleep(1000);  //visible delay
    RemoteApp.frmTest.btnOk.Click;
end;

procedure TTestRemoteControl.SetUp;
begin
  inherited;
end;

procedure TTestRemoteControl.StartRemoteApp;
var
  tstart: TDateTime;
begin
  FreeAndNil(FRemoteableApp);
  FRemoteableApp := TJvCreateProcess.Create(nil);
  FRemoteableApp.ApplicationName  := '..\RemoteApp\RemoteableApp.exe';
  FRemoteableApp.CommandLine      := '"..\RemoteApp\RemoteableApp.exe" -remotecontrol=12345';
  FRemoteableApp.WaitForTerminate := True;
  FRemoteableApp.Run;

  tstart := Now;
  while SecondsBetween(Now, tstart) < 2 * 1000 do
  begin
    if FRemoteableApp.State in [psWaiting, psRunning] then
    begin
      Windows.WaitForInputIdle(FRemoteableApp.ProcessInfo.hProcess, 1 * 1000);
      Break;
    end;
    Sleep(100);
  end;
end;

procedure TTestRemoteControl.StopRemoteApp;
begin
  Assert(FRemoteableApp <> nil);
  try
    RemoteApp.frmMain.Close;
    dmRemoteConnector.IdTCPClient1.Disconnect;

    if Windows.WaitForSingleObject(FRemoteableApp.ProcessInfo.hProcess, 5 * 1000) <> 0 then
      FRemoteableApp.CloseApplication(True);  //force stop
  finally
    FreeAndNil(FRemoteableApp);
  end;
end;

procedure TTestRemoteControl.TearDown;
begin
  inherited;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TTestRemoteControl.Suite);
end.

