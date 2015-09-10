unit mfRemoteMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvCreateProcess;

type
  TForm7 = class(TForm)
    btnTestModal: TButton;
    btnTestFloating: TButton;
    btnSetText: TButton;
    btnPushButton: TButton;
    btnPressOk: TButton;
    btnStartRemoteApp: TButton;
    btnStopRemoteApp: TButton;
    Button1: TButton;
    btnConnect: TButton;
    procedure btnStartRemoteAppClick(Sender: TObject);
    procedure btnStopRemoteAppClick(Sender: TObject);
    procedure btnTestModalClick(Sender: TObject);
    procedure btnTestFloatingClick(Sender: TObject);
    procedure btnSetTextClick(Sender: TObject);
    procedure btnPushButtonClick(Sender: TObject);
    procedure btnPressOkClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
  private
    { Private declarations }
   FRemoteableApp: TJvCreateProcess;
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

uses
  DateUtils,
  //mdRemoteControlConnector,
  mcRemoteApp, RemoteConnector;

{$R *.dfm}

procedure TForm7.btnConnectClick(Sender: TObject);
begin
  dmRemoteConnector.IdTCPClient1.Disconnect;
  dmRemoteConnector.IdTCPClient1.Host := 'localhost';
  dmRemoteConnector.IdTCPClient1.Port := 12345;
  dmRemoteConnector.IdTCPClient1.Connect;
end;

procedure TForm7.btnPressOkClick(Sender: TObject);
begin
  RemoteApp.frmTest.btnOk.Click;
end;

procedure TForm7.btnPushButtonClick(Sender: TObject);
begin
  RemoteApp.frmTest.framTest1.Button1.Click;
end;

procedure TForm7.btnSetTextClick(Sender: TObject);
begin
  RemoteApp.frmTest.framTest1.Exists;
  RemoteApp.frmTest.framTest1.Edit1.Text := 'test';
end;

procedure TForm7.btnStartRemoteAppClick(Sender: TObject);
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

  dmRemoteConnector.IdTCPClient1.Disconnect;
  dmRemoteConnector.IdTCPClient1.Host := 'localhost';
  dmRemoteConnector.IdTCPClient1.Port := 12345;
  dmRemoteConnector.IdTCPClient1.Connect;
end;

procedure TForm7.btnStopRemoteAppClick(Sender: TObject);
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

procedure TForm7.btnTestFloatingClick(Sender: TObject);
begin
  RemoteApp.frmMain.btnShowFloating.Click;
end;

procedure TForm7.btnTestModalClick(Sender: TObject);
begin
  RemoteApp.frmMain.btnShowModal.Click;
end;

procedure TForm7.Button1Click(Sender: TObject);
begin
  btnStartRemoteApp.Click;

  RemoteApp.frmMain.btnShowModal.Click;
  Sleep(1000);

    RemoteApp.frmTest.framTest1.Exists;
    RemoteApp.frmTest.framTest1.Edit1.Text := 'test';
    Sleep(1000);
    RemoteApp.frmTest.framTest1.Button1.Click;
    Sleep(1000);
    RemoteApp.frmTest.btnOk.Click;

  RemoteApp.frmMain.btnShowFloating.Click;
  Sleep(1000);
  RemoteApp.frmMain.btnShowFloating.Click;
  Sleep(1000);
    RemoteApp.frmTest.framTest1.Exists;
    RemoteApp.frmTest.framTest1.Edit1.Text := 'test';
    Sleep(1000);
    RemoteApp.frmTest.framTest1.Button1.Click;
    Sleep(1000);
    RemoteApp.frmTest.btnOk.Click;
    Sleep(1000);
    RemoteApp.frmTest.btnOk.Click;

  btnStopRemoteApp.Click;
end;

end.
