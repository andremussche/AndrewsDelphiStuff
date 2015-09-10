unit mfTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, mfTestBase, StdCtrls, mfrBaseFrame, mfrTestFrame, ActnList, ExtCtrls,
  AppEvnts;

type
  TfrmTest = class(TfrmTestBase)
    framTest1: TframTest;
    ApplicationEvents1: TApplicationEvents;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
  private
    { Private declarations }
    FStart: TDateTime;
    procedure Dummy(var aMsg: TMessage); message WM_USER;
  public
    { Public declarations }
  end;

implementation

uses
  DateUtils;

{$R *.dfm}

procedure TfrmTest.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  inherited;
  actOk.Enabled := (MilliSecondsBetween(Now, FStart) > 10*1000);
end;

procedure TfrmTest.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  inherited;
  //
end;

procedure TfrmTest.Dummy(var aMsg: TMessage);
begin
  //Make message loop for 10s: messages are processed but app is not idle,
  //so actionlist is not updated for 10s.
  //This simulates the need to wait till app is idle before a remote "Action1.Enabled" etc can be read.
  //Because sometimes it can take a while till a full form is painted after create etc.
  //In the mean time, a remote call can be processed (TThread.Synchronize/Queue message),
  //but the GUI is not fully ready yet -> strange unit test results.
  if MilliSecondsBetween(Now, FStart) < 10*1000 then
    PostMessage(Self.Handle, WM_USER, 0, 0);
end;

procedure TfrmTest.FormActivate(Sender: TObject);
begin
  inherited;
//  Sleep(2 * 1000);
end;

procedure TfrmTest.FormCreate(Sender: TObject);
begin
  inherited;
  FStart := Now;
  actOk.Enabled := False;

  Sleep(2 * 1000);        //give unit test time to "inject" remote call message into message queue
  PostMessage(Self.Handle, WM_USER, 0, 0);  //start our message "idle lock"
end;

procedure TfrmTest.FormResize(Sender: TObject);
begin
  inherited;
//  Sleep(2 * 1000);
end;

procedure TfrmTest.FormShow(Sender: TObject);
begin
  inherited;
//  Sleep(2 * 1000);
end;

end.
