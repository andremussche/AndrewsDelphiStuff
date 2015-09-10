unit fImagePopup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, jpeg, pngimage;

type
  TfrmImagePopup = class(TForm)
    Image1: TImage;
    procedure Image1DblClick(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    class procedure ShowImage(const aFile: string);
  end;

var
  frmImagePopup: TfrmImagePopup;

implementation

uses
  Math;

{$R *.dfm}

procedure TfrmImagePopup.Image1Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmImagePopup.Image1DblClick(Sender: TObject);
begin
  Self.Close;
end;

class procedure TfrmImagePopup.ShowImage(const aFile: string);
var sfile: string;
begin
  with TfrmImagePopup.Create(nil) do
  try
    if FileExists(aFile) then
      Image1.Picture.LoadFromFile(aFile)
    else
    begin
      sfile := ExtractFilePath(Application.ExeName) + ExtractFileName(aFile);
      if FileExists(sFile) then
        Image1.Picture.LoadFromFile(sfile)
      else
      begin
        MessageDlg('Kon plaatje niet vinden: ' + ExtractFileName(aFile), mtError, [mbOK], 0);
        Exit;
      end;
    end;

    Width  := Min(Image1.Width,  Trunc(Screen.Width  * 0.85) );
    Height := Min(Image1.Height, Trunc(Screen.Height * 0.85) );
    Image1.Align := alClient;
    Image1.Proportional := True;

    Position := poMainFormCenter;
    ShowModal;
  finally
    Free;
  end;
end;

end.
