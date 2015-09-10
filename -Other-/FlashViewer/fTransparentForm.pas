unit fTransparentForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, AppEvnts;

type
  TfrmTransparent = class(TForm)
    Panel1: TPanel;
    ApplicationEvents1: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;

    procedure RestoreCursor;
  public
    procedure UpdateCursor;
  end;

var
  frmTransparent: TfrmTransparent;

implementation

uses
  fMainWithBmp;

{$R *.dfm}

var
  iconInfo: TIconInfo;
  horgcursor: HCURSOR;
  hmycursor: HCURSOR;
const
  crMyCursor = 1;

procedure TfrmTransparent.FormCreate(Sender: TObject);
begin
  { Next five properties can also be set at design-time }
  Borderstyle     := bsNone;
//  AlphaBlend      := true;
//  AlphaBlendvalue := 1;
//  Color           := clBlack;
  Formstyle       := fsStayOnTop;
  Boundsrect      := Screen.DesktopRect;

  horgcursor := Screen.Cursors[crDefault];
  UpdateCursor;
end;

procedure TfrmTransparent.FormDestroy(Sender: TObject);
begin
  RestoreCursor;
end;

procedure TfrmTransparent.RestoreCursor;
begin
  Screen.Cursors[crDefault] := horgcursor;
  DestroyIcon(hmycursor);
  Screen.Cursor             := crDefault;
end;

procedure TfrmTransparent.UpdateCursor;
var
  bmpMask : TBitmap;
  bmpColor: TBitmap;
  icentre: Integer;
  iminus, iplus: integer;
begin
  if hmycursor <> 0 then
  begin
    Screen.Cursors[crDefault] := horgcursor;
    DestroyIcon(hmycursor);
  end;

  bmpMask  := TBitmap.Create;
  bmpColor := TBitmap.Create;
  try
    icentre := 30 div 2;
    iminus  := frmMainBmp.btnPenSize.Tag div 2;
    iplus   := frmMainBmp.btnPenSize.Tag - iminus + 1;

    bmpMask.Monochrome := True;
    bmpMask.Height     := 30;
    bmpMask.Width      := 30;
    bmpMask.Canvas.Brush.Color := clWhite;  //white = 1
    bmpMask.Canvas.FillRect( Rect(0, 0, bmpMask.Width, bmpMask.Height) );
    bmpMask.Canvas.Brush.Color := clBlack;  //black = 0
    bmpMask.Canvas.Ellipse(icentre - iminus,
                           icentre - iminus,
                           icentre + iplus,
                           icentre + iplus);
    //bmpMask.SaveToFile('cursormask.bmp');

    bmpColor.Height := 30;
    bmpColor.Width  := 30;
    bmpColor.Canvas.Brush.Color := clBlack;  //black = 0
    bmpColor.Canvas.FillRect( Rect(0, 0, bmpColor.Width, bmpColor.Height) );
    bmpColor.Canvas.Pen.Color   := frmMainBmp.cbColorBox.Selected;
    bmpColor.Canvas.Brush.Color := frmMainBmp.cbColorBox.Selected;
    bmpColor.Canvas.Ellipse(icentre - iminus,
                            icentre - iminus,
                            icentre + iplus,
                            icentre + iplus);
    //bmpColor.SaveToFile('cursorcolor.bmp');

    {
    AND bitmask XOR bitmask Display
    0           0 Black
    0           1 White
    1           0 Screen
    1           1 Reverse screen
    }
    with iconInfo do
    begin
      fIcon    := false;
      xHotspot := 15;
      yHotspot := 15;
      hbmMask  := bmpMask.Handle;    //AND
      hbmColor := bmpColor.Handle;   //XOR
    end;

    hmycursor                  := CreateIconIndirect(iconInfo);
    Screen.Cursors[crMyCursor] := hmycursor;
    Screen.Cursor              := crMyCursor;
  finally
    bmpMask.Free;
    bmpColor.Free;
  end;
end;

var
  down: Boolean;
  FPreviousPoint: TPoint;

procedure TfrmTransparent.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  Handled := False;

  if Msg.message = WM_LBUTTONDOWN then
  begin
    down := True;
    FPreviousPoint := Msg.pt;
  end
  else if Msg.message = WM_LBUTTONUP then
    down := False;

  if (Msg.message = WM_MOUSEMOVE) and down then
     //(Abs(FPreviousPoint.X - Msg.pt.X) > 5) and
     //(Abs(FPreviousPoint.Y - Msg.pt.Y) > 5) then
  begin
    Handled := True;

    { TODO : kleur + grootte doorgeven via properties }
    Self.Canvas.Brush.Color := frmMainBmp.cbColorBox.Selected;
    Self.Canvas.Brush.Style := bsClear;
    Self.Canvas.Pen.Color   := frmMainBmp.cbColorBox.Selected;
    Self.Canvas.Pen.Style   := psSolid;
    Self.Canvas.Pen.Mode    := pmCopy;
    Self.Canvas.Pen.Width   := frmMainBmp.btnPenSize.Tag;
    {
    Self.Canvas.Brush.Color := clRed;
    Self.Canvas.Brush.Style := bsClear;
    Self.Canvas.Pen.Color   := clRed;
    Self.Canvas.Pen.Style   := psSolid;
    Self.Canvas.Pen.Mode    := pmCopy;
    Self.Canvas.Pen.Width   := 4;
    }

    Self.Canvas.MoveTo( FPreviousPoint.X, FPreviousPoint.Y);

    if (Abs(FPreviousPoint.X - Msg.pt.X) > 1) or
       (Abs(FPreviousPoint.Y - Msg.pt.Y) > 1) then
      Self.Canvas.LineTo( Msg.pt.X,
                          Msg.pt.Y );

    FPreviousPoint := Msg.pt;
  end;
end;

procedure TfrmTransparent.CreateParams(var Params: TCreateParams); // override;
begin
  inherited;
  //params.exstyle := params.ExStyle or WS_EX_TRANSPARENT;
end;

end.
