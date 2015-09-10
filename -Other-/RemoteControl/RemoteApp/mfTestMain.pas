unit mfTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmMain = class(TForm)
    btnShowModal: TButton;
    btnShowFloating: TButton;
    procedure btnShowModalClick(Sender: TObject);
    procedure btnShowFloatingClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  mfTest;

{$R *.dfm}

procedure TfrmMain.btnShowFloatingClick(Sender: TObject);
begin
  with TfrmTest.Create(Application) do
    Show;
end;

procedure TfrmMain.btnShowModalClick(Sender: TObject);
begin
  with TfrmTest.Create(nil) do
  begin
    ShowModal;
    Release;
  end;
end;

end.
