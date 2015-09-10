unit fMainExt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvOfficePager;

type
  TfrmMainExt = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainExt: TfrmMainExt;

implementation

{$R *.dfm}

procedure TfrmMainExt.Button1Click(Sender: TObject);
begin
  with tbitmap.Create do
  begin
    LoadFromFile('..\..\..\RES\close.bmp');
//    Free;
  end;
end;

end.
