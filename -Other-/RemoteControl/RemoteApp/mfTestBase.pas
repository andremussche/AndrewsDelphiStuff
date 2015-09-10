unit mfTestBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls, ExtCtrls;

type
  TfrmTestBase = class(TForm)
    ActionList1: TActionList;
    Panel1: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    actOk: TAction;
    actCancel: TAction;
    procedure actOkExecute(Sender: TObject);
    procedure actCancelExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TfrmTestBase.actCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
  Close;
end;

procedure TfrmTestBase.actOkExecute(Sender: TObject);
begin
  ModalResult := mrOk;
  Close;
end;

end.
