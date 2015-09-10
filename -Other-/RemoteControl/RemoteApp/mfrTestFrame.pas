unit mfrTestFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, mfrBaseFrame, StdCtrls, ExtCtrls;

type
  TframTest = class(TframBase)
    Panel1: TPanel;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TframTest.Button1Click(Sender: TObject);
begin
  //MessageDlg('Button clicked!', mtInformation, [mbOK], 0); dit kunnen we niet afvangen?
  Button1.Caption := 'Button clicked!';
end;

end.
