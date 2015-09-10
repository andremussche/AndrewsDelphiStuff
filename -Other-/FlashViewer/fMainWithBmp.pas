unit fMainWithBmp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ImgList, ActnList, Buttons, pngimage,
  uComponentMover, uFormData, uDragDrop, uFlashMemo,
  Generics.Collections, uFlashRect, uFlashQuestion, Menus;

type
  TfrmMainBmp = class(TForm,
                      IDragDrop)
    ScrollBox1: TScrollBox;
    imgFlash: TImage;
    Panel1: TPanel;
    lblPageMax: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    btnTestLoad: TButton;
    btnSave: TButton;
    btnPrevious: TButton;
    btnNext: TButton;
    edtPage: TEdit;
    edtZoom: TEdit;
    chkEditMode: TCheckBox;
    btnTekstvak: TButton;
    ActionList1: TActionList;
    actNext: TAction;
    actPrevious: TAction;
    actZoomIn: TAction;
    actZoomOut: TAction;
    actTekstVak: TAction;
    actOpslaan: TAction;
    actEditMode: TAction;
    ImageList1: TImageList;
    OpenDialog1: TOpenDialog;
    Shape1: TShape;
    btnVraag: TButton;
    actVraag: TAction;
    imgPaint: TImage;
    cbColorBox: TColorBox;
    chkTekenen: TCheckBox;
    ImageList2: TImageList;
    PopupMenu1: TPopupMenu;
    actSize11: TMenuItem;
    actSize31: TMenuItem;
    actSize51: TMenuItem;
    btnPenSize: TButton;
    ActionList2: TActionList;
    acPen1: TAction;
    acPen3: TAction;
    acPen5: TAction;
    acPen7: TAction;
    acPen9: TAction;
    acPen11: TAction;
    N71: TMenuItem;
    N91: TMenuItem;
    N111: TMenuItem;
    btnErase: TButton;
    pmuErase: TPopupMenu;
    actAllesWissen: TAction;
    Alleswissen1: TMenuItem;
    actErase: TAction;
    lblMain: TLabel;
    Action1: TAction;
    btnToonAntwoorden: TButton;
    actToonAnswers: TAction;
    procedure btnPenSizeClick(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actPreviousExecute(Sender: TObject);
    procedure actZoomInExecute(Sender: TObject);
    procedure actZoomOutExecute(Sender: TObject);
    procedure actOpslaanExecute(Sender: TObject);
    procedure edtPageChange(Sender: TObject);
    procedure edtPageKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtZoomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actTekstVakExecute(Sender: TObject);
    procedure actEditModeExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure imgFlashMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgFlashMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgFlashMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure actVraagExecute(Sender: TObject);
    procedure imgPaintMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgPaintMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgPaintMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgPaintMouseLeave(Sender: TObject);
    procedure chkTekenenClick(Sender: TObject);
    procedure actSize1Execute(Sender: TObject);
    procedure btnTestLoadClick(Sender: TObject);
    procedure actAllesWissenExecute(Sender: TObject);
    procedure actEraseExecute(Sender: TObject);
    procedure lblMainClick(Sender: TObject);
    procedure imgFlashDblClick(Sender: TObject);
    procedure cbColorBoxChange(Sender: TObject);
    procedure actToonAnswersExecute(Sender: TObject);
  protected
    FMover: TMover;
    FFlashData: TFlashData;
    FTextLabels : TObjectList<TFlashMemo>;
    FQuestions  : TObjectList<TFlashQuestion>;

    procedure LoadFlash(const aFlashFile: TFileName);
    procedure RefreshTextButtons;
    procedure RefreshFlashImage;
    procedure ReScaleTextMemos;
    procedure DrawMemoBackground(aMemo: TFlashMemo);
    procedure ControlMoved(Sender: TObject);
  protected
    FDropTarget: TDropTarget;
    {IDragDrop}
    function  DropAllowed(const FileNames: array of string): Boolean;
    procedure Drop(const FileNames: array of string);
    //procedure WMDROPFILES(var msg: TWMDropFiles); message WM_DROPFILES;
    //procedure WMDROPFILES(var msg: TWMDropFiles);
  protected
    FMouseDown: Boolean;
    FMouseDownRect: TPoint;
    FFlashPos1,
    FFlashPos2: TFlashRect;
  protected
    FPaintMouseDown: Boolean;
    FPreviousPoint : TPoint;
    //FPagePng: TObjectList<TPngImage>;
    FPageBmp: TObjectList<TBitmap>;
    //function InitPngCanvas: TPngImage;
    function  InitBmpCanvas: TBitmap;
    procedure PaintScreenToBitmap;
  public
    procedure  AfterConstruction;override;
    destructor Destroy;override;
  end;

var
  frmMainBmp: TfrmMainBmp;

implementation

uses
  fFlashOffscreen, Math, fTransparentForm;

{$R *.dfm}

procedure TfrmMainBmp.FormCreate(Sender: TObject);
begin
  //pnlFlash.BorderStyle      := bsNone;
  ScrollBox1.BorderStyle := bsNone;
  ScrollBox1.AutoScroll  := False;
  ScrollBox1.Align       := alClient;
  ScrollBox1.Visible     := False;

  lblPageMax.Caption := '0';

  {$WARN SYMBOL_PLATFORM OFF}
  btnTestLoad.Visible := DebugHook <> 0;

//  png := TPngImage.CreateBlank(COLOR_RGB, 16, Image2.Width, Image2.Height);
//  png.Transparent := True;
//  png.TransparentColor := clBlack;
//  //png.TransparencyMode := ptmBit;
//  Image2.Picture.Assign(png);
//  Image2.Picture.Assign( TBitmap.Create );
//  Image2.Picture.g
//  Image2.Picture.Bitmap.Width  := Image2.Width;
//  Image2.Picture.Bitmap.Height := Image2.Height;
//  Image2.Picture.Bitmap.PixelFormat      := pf32bit;
//  Image2.Picture.Bitmap.Transparent      := True;
//  Image2.Picture.Bitmap.TransparentMode  := tmFixed;
//  Image2.Picture.Bitmap.TransparentColor := clWhite;
  imgPaint.Transparent := True;
  imgPaint.Align       := alClient;
end;

procedure TfrmMainBmp.FormResize(Sender: TObject);
begin
  edtZoomChange(nil);
end;

procedure TfrmMainBmp.imgFlashDblClick(Sender: TObject);
//var
//  pt: TPoint;
begin
  //with frmFlashOffscreen do
  //  FMouseDown  := (ShockwaveFlash1.ReadyState > 0);
  if Screen.Cursor <> crCross then
  begin
//    pt := Mouse.CursorPos;
//    pt := imgFlash.ScreenToClient(pt);
//    imgFlashMouseDown(Sender, mbLeft, [], pt.X, pt.Y);
    Screen.Cursor := crCross;
    Windows.SetCursor(Screen.Cursors[crCross]);
  end
  else
  begin
    Screen.Cursor := crDefault;
    if FMouseDown then
      imgFlashMouseUp(Sender, mbLeft, [], 0, 0);
  end;
end;

procedure TfrmMainBmp.imgFlashMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Screen.Cursor = crCross then
    Windows.SetCursor(Screen.Cursors[crCross]);
  //if Screen.Cursor = crCross then Exit;
  if Screen.Cursor <> crCross then Exit;

  with frmFlashOffscreen do
    FMouseDown     := (ShockwaveFlash1.ReadyState > 0);
  FMouseDownRect.X := X - ScrollBox1.HorzScrollBar.Position;
  FMouseDownRect.Y := Y - ScrollBox1.VertScrollBar.Position;

  Shape1.Left   := FMouseDownRect.X;
  Shape1.Top    := FMouseDownRect.Y;
  Shape1.Width  := 0;
  Shape1.Height := 0;
end;

procedure TfrmMainBmp.imgFlashMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Screen.Cursor = crCross then
    Windows.SetCursor(Screen.Cursors[crCross]);
//  if Screen.Cursor <> crCross then Exit;

  if FMouseDown then
  begin
    Shape1.Width  := Abs(FMouseDownRect.X - X + ScrollBox1.HorzScrollBar.Position);
    Shape1.Height := Abs(FMouseDownRect.Y - Y + ScrollBox1.VertScrollBar.Position);

    if (FMouseDownRect.X > X - ScrollBox1.HorzScrollBar.Position)
       //(FMouseDownRect.Y > Y - ScrollBox1.VertScrollBar.Position)
    then
      Shape1.Brush.Color := clRed   //unzoom
    else
      Shape1.Brush.Color := clBlue;

    Shape1.Left := Min(FMouseDownRect.X, X - ScrollBox1.HorzScrollBar.Position);
    Shape1.Top  := Min(FMouseDownRect.Y, Y - ScrollBox1.VertScrollBar.Position);

    Shape1.Visible := True;
  end;
end;

procedure TfrmMainBmp.imgFlashMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  iXdiff, iYdiff: Integer;
  fZoomX, fZoomY: Double;
begin
  //if Screen.Cursor = crCross then Exit;
  FMouseDown := False;

  if //(Screen.Cursor = crCross) and
     (Shape1.Visible = True) then
  begin
    Screen.Cursor := crDefault
  end
  else
    Exit;

  FMouseDown     := False;
  Shape1.Visible := False;

  if Shape1.Width  < 30 then Exit;
  if Shape1.Height < 30 then Exit;

  //unzoom?
  if Shape1.Brush.Color = clRed then
  begin
    edtZoom.Text := '100';
    Exit;
  end;

  FFlashPos1.ScreenTop    := Shape1.Top  + ScrollBox1.VertScrollBar.Position;
  FFlashPos1.ScreenLeft   := Shape1.Left + ScrollBox1.HorzScrollBar.Position;
  FFlashPos1.ScreenWidth  := Shape1.Width;
  FFlashPos1.ScreenHeigth := Shape1.Height;

  iXdiff := Shape1.Width  + 25{scrollbar};
  iYdiff := Shape1.Height + 25{scrollbar};
  fZoomX := (imgFlash.Width  * 100) / iXdiff;
  fZoomY := (imgFlash.Height * 100) / iYdiff;

  //zoom
  edtZoom.Text := IntToStr(Trunc(Min(fZoomX, fZoomY)));

  //scroll to pos
  FFlashPos1.RecalcScreenPositions;
  ScrollBox1.VertScrollBar.Position := FFlashPos1.ScreenTop;
  ScrollBox1.HorzScrollBar.Position := FFlashPos1.ScreenLeft;
end;

procedure TfrmMainBmp.imgPaintMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{  FPaintMouseDown  := True;
  FPreviousPoint.X := X;
  FPreviousPoint.Y := Y;

  //draw point
  imgPaintMouseMove(Sender, Shift, X, Y);}
end;

procedure TfrmMainBmp.imgPaintMouseLeave(Sender: TObject);
begin
{  FPaintMouseDown := False;

  Self.DoubleBuffered := False;   //enable normal drawing: so first textbox, then picture/lines
  Self.Invalidate;            }
  //ReScaleTextMemos;
  //imgPaint.Picture.Assign(png);
end;

procedure TfrmMainBmp.imgPaintMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
{var
  r: TRect;
  //png: TPngImage;
  xscale, yscale: Single;
  bmp: TBitmap;}
begin
{
  if FPaintMouseDown then
  begin
    r.Top    := y;
    r.Left   := x;
    r.Right  := x + 3;
    r.Bottom := y + 3;

    //png := image2.Picture.Graphic as TPngImage;
    //png := InitPngCanvas;
    bmp    := InitBmpCanvas;
    xscale := bmp.Width  / imgPaint.Width;
    yscale := bmp.Height / imgPaint.Height;

    //paint in background (loaded when page is changed)
    bmp.Canvas.MoveTo( Round(FPreviousPoint.X * xscale),
                       Round(FPreviousPoint.Y * yscale) );
    bmp.Canvas.LineTo( Round(X * xscale),
                       Round(Y * yscale) );

    //paint directly on screen
    imgPaint.Canvas.Brush.Color := cbColorBox.Selected;
    imgPaint.Canvas.Brush.Style := bsClear;
    imgPaint.Canvas.Pen.Color   := cbColorBox.Selected;
    imgPaint.Canvas.Pen.Style   := psSolid;
    imgPaint.Canvas.Pen.Mode    := pmCopy;
    imgPaint.Canvas.Pen.Width   := btnPenSize.Tag;

    imgPaint.Canvas.MoveTo( Round(FPreviousPoint.X * xscale),
                            Round(FPreviousPoint.Y * yscale) );
    imgPaint.Canvas.LineTo( Round(X * xscale),
                            Round(Y * yscale) );
    imgPaint.BringToFront;

    Self.DoubleBuffered := True;   //no flickering!
//    imgPaint.Picture.Assign(png);  slooooow
//    ScrollBox1.Invalidate;

    FPreviousPoint.X := X;
    FPreviousPoint.Y := Y;
  end;                           }
end;

procedure TfrmMainBmp.imgPaintMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{  FPaintMouseDown := False;
  RefreshFlashImage;}
end;

function TfrmMainBmp.InitBmpCanvas: TBitmap;
begin
  Result := FPageBmp.Items[frmFlashOffscreen.ShockwaveFlash1.CurrentFrame-1];
  Result.Canvas.Brush.Color := cbColorBox.Selected;
  Result.Canvas.Brush.Style := bsClear;
  Result.Canvas.Pen.Color   := cbColorBox.Selected;
  Result.Canvas.Pen.Style   := psSolid;
  Result.Canvas.Pen.Mode    := pmCopy;
  Result.Canvas.Pen.Width   := btnPenSize.Tag;
end;

{
function TfrmMainBmp.InitPngCanvas: TPngImage;
begin
  Result := FPagePng.Items[frmFlashOffscreen.ShockwaveFlash1.CurrentFrame-1];
  Result.Canvas.Brush.Color := cbColorBox.Selected;
  Result.Canvas.Brush.Style := bsClear;
  Result.Canvas.Pen.Color   := cbColorBox.Selected;
//    begin
//      png.Canvas.Pen.Color   := clBlack;  //transparent
    //png.Canvas.Pen.Mode   := pmNop;
//    end;
  Result.Canvas.Pen.Style   := psSolid;
  Result.Canvas.Pen.Mode    := pmCopy;
  //png.Canvas.Pen.Width   := 4;
  Result.Canvas.Pen.Width   := btnPenSize.Tag;
end;
}

procedure TfrmMainBmp.lblMainClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadFlash(OpenDialog1.FileName);
end;

procedure TfrmMainBmp.LoadFlash(const aFlashFile: TFileName);
var
  //png: TPngImage;
  bmp: TBitmap;
  i, y, iScanlineWidth: integer;
begin
  Screen.Cursor := crHourGlass;
  try
    frmFlashOffscreen.LoadFlash(aFlashFile);
    Self.Caption := Format('School Flash Viewer - %s', [ExtractFileName(aFlashFile)]);

    imgFlash.Left   := 0;
    imgFlash.Top    := 0;
    imgFlash.Width  := ScrollBox1.Width  - 5;
    imgFlash.Height := ScrollBox1.Height - 5;

    FFlashData.LoadFromFile(aFlashFile + '.xml');
    with frmFlashOffscreen do
    begin
      FFlashData.PageCount := ShockwaveFlash1.TotalFrames;
      edtPage.Text         := IntToStr(ShockwaveFlash1.CurrentFrame);
      lblPageMax.Caption   := IntToStr(ShockwaveFlash1.TotalFrames - 1);

      FPageBmp.Clear;
      for i := 0 to ShockwaveFlash1.TotalFrames - 1 do
      begin
  //      png := TPngImage.CreateBlank(COLOR_RGB, 16, FlashWidth, FlashHeight);
  //      png.Transparent      := True;
  //      png.TransparentColor := clBlack;
        bmp := TBitmap.Create;
        bmp.PixelFormat := pf32bit;
        bmp.PixelFormat := pf32bit;
        with frmFlashOffscreen do
          bmp.SetSize(FlashWidth, FlashHeight);
        bmp.Transparent      := True;
        bmp.TransparentColor := clBtnFace;  //clblack

        // ensure all pixels are black with opacity 0 (= fully transparent)
        iScanlineWidth := bmp.Width * SizeOf(TRGBQuad);
        for y := 0 to bmp.Height - 1 do
          ZeroMemory(bmp.ScanLine[y], iScanlineWidth);

        //png.TransparentColor := clWhite - 1;
        //png.TransparencyMode := ptmBit;
        //Image2.Picture.Assign(png);

        FPageBmp.Add(bmp);
      end;
    end;

    RefreshFlashImage;
    RefreshTextButtons;
    ReScaleTextMemos;
    edtZoomChange(nil);

    ScrollBox1.Visible := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMainBmp.PaintScreenToBitmap;
var
  Result : TBitmap;
begin
  Result := TBitmap.Create;
  try
    Result.Width  := ScrollBox1.ClientWidth;
    Result.Height := ScrollBox1.ClientHeight;
    Result.Canvas.Brush := ScrollBox1.Brush;
    Result.Canvas.FillRect(ScrollBox1.ClientRect);
    Result.Canvas.Lock;
    try
      PaintTo(Result.Canvas.Handle, 0, 0);
    finally
      Result.Canvas.Unlock;
    end;
  except
    Result.Free;
    raise;
  end;

  imgPaint.Canvas.Lock;
  try
    imgPaint.Transparent := False;
//    ScrollBox1.PaintTo(imgPaint.Canvas.Handle, 0, 0);
    imgPaint.Picture.Assign(Result);
  finally
    imgPaint.Canvas.Unlock;
  end;
end;

procedure TfrmMainBmp.AfterConstruction;
begin
  inherited;
  FFlashData   := TFlashData.Create;
  FMover       := TMover.Create(self);
  //FTextButtons := TObjectList<TFlashButton>.Create(True{owns!});
  //FTextLabels  := TObjectList<TFlashText>.Create(True{owns!});
  FTextLabels  := TObjectList<TFlashMemo>.Create(True{owns!});
  FQuestions   := TObjectList<TFlashQuestion>.Create(True{owns!});

  //ready to accept files
  //DragAcceptFiles(Handle, True) ;
  FDropTarget := TDropTarget.Create(Self.Handle, Self);

  //FPagePng := TObjectList<TPngImage>.Create(True);
  FPageBmp := TObjectList<TBitmap>.Create(True);

  chkTekenen.Checked  := False;
  actEditMode.Checked := False;
  actEditModeExecute(nil);
  chkTekenenClick(nil);
  actSize1Execute(acPen5);
end;

procedure TfrmMainBmp.btnTestLoadClick(Sender: TObject);
begin
  LoadFlash(ExtractFilePath(Application.ExeName) + 'hoofdstuk1B.swf');
end;

destructor TfrmMainBmp.Destroy;
begin
  FPageBmp.Free;

  FDropTarget := nil;
  //FTextButtons.Free;
  FTextLabels.Free;
  FQuestions.Free;
  FFlashData.Free;
  FMover.Free;

  inherited;
end;

procedure TfrmMainBmp.DrawMemoBackground(aMemo: TFlashMemo);
var
  p: TPoint;
  r: TRect;
var
  AStyle: Integer;
begin
  if aMemo.Lines.Count > 1 then
  begin
    //RefreshFlashImage;
    aMemo.ControlStyle := aMemo.ControlStyle - [csOpaque];
    AStyle := GetWindowLong(aMemo.Handle, GWL_EXSTYLE);
    SetWindowLong(aMemo.Handle, GWL_EXSTYLE, AStyle and not WS_EX_TRANSPARENT);

    aMemo.Font.Style   := aMemo.Font.Style + [fsUnderline];
    //p := Self.ClientToParent(Self.ClientOrigin);
    //p := Self.ClientOrigin;

    r := aMemo.ClientRect;
    r.TopLeft     := aMemo.ClientToParent(r.TopLeft);
    r.BottomRight := aMemo.ClientToParent(r.BottomRight);

    p := r.TopLeft;
    aMemo.Color := imgFlash.Canvas.Pixels[p.X, p.Y];
    imgFlash.Canvas.Brush.Color := aMemo.Color;
    //imgFlash.Canvas.FillRect(r);

    //Self.DoubleBuffered := True;
    //Self.Update;
    //Self.DoubleBuffered := False;
  end;
end;

procedure TfrmMainBmp.Drop(const FileNames: array of string);
var s: string;
begin
  for s in FileNames do
  begin
    if LowerCase(ExtractFileExt(s)) = '.swf' then
    begin
      LoadFlash(s);
      Exit;  //load the first one
    end;
    if ( (LowerCase(ExtractFileExt(s)) = '.xml') and
         FileExists(ChangeFileExt(s, '')) ) then
    begin
      LoadFlash(ChangeFileExt(s, ''));
      Exit;  //load the first one
    end;

  end;
end;

function TfrmMainBmp.DropAllowed(const FileNames: array of string): Boolean;
var s: string;
begin
  Result := False;
  for s in FileNames do
  begin
    if (LowerCase(ExtractFileExt(s)) = '.swf') or
       ( (LowerCase(ExtractFileExt(s)) = '.xml') and
         FileExists(ChangeFileExt(s, '')) )
    then
      Exit(True)
  end;
end;

procedure TfrmMainBmp.actAllesWissenExecute(Sender: TObject);
var
  bmp: TBitmap;
begin
  bmp := FPageBmp.Items[frmFlashOffscreen.ShockwaveFlash1.CurrentFrame-1];
  bmp.Canvas.Brush.Color := bmp.TransparentColor;
  bmp.Canvas.Pen.Color   := bmp.TransparentColor;
  bmp.Canvas.FillRect(bmp.Canvas.ClipRect);
  imgPaint.Picture.Assign(bmp);
end;

procedure TfrmMainBmp.actEditModeExecute(Sender: TObject);
begin
  FMover.Enabled := chkEditMode.Checked;

  if chkEditMode.Checked then
  begin
    chkTekenen.Checked  := False;

    actTekstVak.Visible := True;
    actVraag.Visible    := True;
    actOpslaan.Visible  := True;

    btnSave.Left        := Self.Width;
    btnTekstvak.Left    := btnSave.Left     - btnSave.Width;
    btnVraag.Left       := btnTekstvak.Left - btnTekstvak.Width;
    chkEditMode.Left    := btnVraag.Left    - btnVraag.Width;
  end
  else
  begin
    actTekstVak.Visible := False;
    actVraag.Visible    := False;
    actOpslaan.Visible  := False;
  end;

  TFlashQuestion.EditMode := chkEditMode.Checked;

  RefreshTextButtons;
  ReScaleTextMemos;
end;

procedure TfrmMainBmp.actEraseExecute(Sender: TObject);
var
  bmp: TBitmap;
begin
  bmp := FPageBmp.Items[frmFlashOffscreen.ShockwaveFlash1.CurrentFrame-1];
  cbColorBox.Selected := bmp.TransparentColor;
  acPen11.Execute;

  if frmTransparent <> nil then
    frmTransparent.UpdateCursor;
end;

procedure TfrmMainBmp.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  with frmFlashOffscreen do
  begin
    actPrevious.Enabled := (ShockwaveFlash1.ReadyState > 0) and
                           (ShockwaveFlash1.CurrentFrame > 1);
    actNext.Enabled     := (ShockwaveFlash1.ReadyState > 0) and
                           (ShockwaveFlash1.CurrentFrame < ShockwaveFlash1.TotalFrames);
    edtPage.Enabled     := (ShockwaveFlash1.ReadyState > 0);

    actZoomIn.Enabled   := (ShockwaveFlash1.ReadyState > 0);
    actZoomOut.Enabled  := (ShockwaveFlash1.ReadyState > 0);
    edtZoom.Enabled     := (ShockwaveFlash1.ReadyState > 0);

    chkTekenen.Enabled  := (ShockwaveFlash1.ReadyState > 0);

    actEditMode.Enabled := (ShockwaveFlash1.ReadyState > 0);
    actOpslaan.Enabled  := (ShockwaveFlash1.ReadyState > 0);
    actTekstVak.Enabled := (ShockwaveFlash1.ReadyState > 0);
    actVraag.Enabled    := (ShockwaveFlash1.ReadyState > 0);
    cbColorBox.Enabled  := (ShockwaveFlash1.ReadyState > 0);

    actToonAnswers.Enabled  := (ShockwaveFlash1.ReadyState > 0);
  end;
end;

procedure TfrmMainBmp.actNextExecute(Sender: TObject);
begin
  with frmFlashOffscreen do
  begin
    //ShockwaveFlash1.GotoFrame( ShockwaveFlash1.CurrentFrame + 1);
    edtPage.Text := IntToStr(ShockwaveFlash1.CurrentFrame + 1);
  end;
end;

procedure TfrmMainBmp.actPreviousExecute(Sender: TObject);
begin
  with frmFlashOffscreen do
  begin
    //ShockwaveFlash1.GotoFrame( ShockwaveFlash1.CurrentFrame - 1);
    edtPage.Text := IntToStr(ShockwaveFlash1.CurrentFrame - 1);
  end;
end;

procedure TfrmMainBmp.actSize1Execute(Sender: TObject);
begin
  btnPenSize.Tag        := (Sender as TAction).Tag;
  btnPenSize.Caption    := IntToStr(btnPenSize.Tag);
  btnPenSize.ImageIndex := (Sender as TAction).ImageIndex;

  if frmTransparent <> nil then
    frmTransparent.UpdateCursor;
end;

procedure TfrmMainBmp.actTekstVakExecute(Sender: TObject);
var
  //btn: TFlashButton2;
  lbl: TFlashMemo;
  s: string;
begin
  //todo: Button1 -> Click -> memo, 28pt
  //if not InputQuery('Knop tekst', 'Tekst van de knop: ', s) then Exit;
  //btn := TFlashButton2.Create(nil);
  //FTextButtons.Add(btn);

  lbl := TFlashMemo.Create(nil);
  FTextLabels.Add(lbl);

  lbl.Parent := ScrollBox1;
  with frmFlashOffscreen do
    lbl.Data   := FFlashData.Pages[ShockwaveFlash1.CurrentFrame].AddData;
  lbl.Top    := ScrollBox1.Height div 4;
  lbl.Left   := ScrollBox1.Width  div 4;

  lbl.ShowNormalText := True;
  s := 'Nieuw tekstvak';
  lbl.Text   := s;
//  lbl.OnDblClick := TextLabelDblClick;
//  btn.Height := pnlFlash.Height div 3;
//  btn.Width  := pnlFlash.Width div 3;
//  FFlashData.SaveToFile(FFlashData.XMLFileName);

  FMover.Add(lbl);
  chkEditMode.Checked := True;

  //update and select memo
  ReScaleTextMemos();
  lbl.SetFocus;
  lbl.SelectAll;
  FMover.ControlMouseDown(lbl, mbLeft, [], 0, 0);
  FMover.ControlMouseUp(lbl, mbLeft, [], 0, 0);
end;

procedure TfrmMainBmp.actToonAnswersExecute(Sender: TObject);
var
  i: Integer;
  memo: TFlashMemo;
begin
  for i := 0 to ScrollBox1.ControlCount-1 do
  begin
    if //(ScrollBox1.Controls[i] is TFlashButton2) or
       (ScrollBox1.Controls[i] is TFlashMemo) then
    begin
      memo := ScrollBox1.Controls[i] as TFlashMemo;
      memo.ShowNormalText := not memo.ShowNormalText;
    end;
  end;
end;

procedure TfrmMainBmp.actVraagExecute(Sender: TObject);
var
  lbl: TFlashQuestion;
begin
  lbl := TFlashQuestion.Create(nil);
  FQuestions.Add(lbl);

  lbl.Parent := ScrollBox1;
  with frmFlashOffscreen do
    lbl.Data   := FFlashData.Pages[ShockwaveFlash1.CurrentFrame].AddQuestion;
  lbl.Top    := ScrollBox1.Height div 3;
  lbl.Left   := ScrollBox1.Width  div 3;

  FMover.Add(lbl);
  chkEditMode.Checked := True;
end;

procedure TfrmMainBmp.edtPageChange(Sender: TObject);
var
  bmp: TBitmap;
begin
//  disable paint
//  + - font size
//  move items -> background paint

  if edtPage.Text = '' then Exit;

  with frmFlashOffscreen do
  begin
    ShockwaveFlash1.GotoFrame( StrToInt(edtPage.Text) );
    edtPage.Text := IntToStr(ShockwaveFlash1.CurrentFrame);

    bmp := FPageBmp.Items[ShockwaveFlash1.CurrentFrame-1];
    if chkTekenen.Checked then
      imgPaint.Picture.Assign(bmp);
  end;

  RefreshFlashImage;
  RefreshTextButtons;
  ReScaleTextMemos;
end;

procedure TfrmMainBmp.edtZoomChange(Sender: TObject);
var
  iZoom: Integer;
  fPercentage: Double;
begin
  Screen.Cursor := crHourGlass;
  try
    iZoom       := StrToInt(edtZoom.Text);
    if iZoom > 700 then
    begin
      iZoom := 700;  //max +- 200mb?
      edtZoom.Text := IntToStr(iZoom);
    end;
    fPercentage := iZoom / 100;
    ScrollBox1.AutoScroll := (fPercentage > 1);

    imgFlash.Width  := Round(ScrollBox1.Width  * fPercentage);
    imgFlash.Height := Round(ScrollBox1.Height * fPercentage);

    RefreshFlashImage;
    ReScaleTextMemos;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMainBmp.actZoomInExecute(Sender: TObject);
begin
  edtZoom.Text := IntToStr(StrToInt(edtZoom.Text) + 10);
end;

procedure TfrmMainBmp.actZoomOutExecute(Sender: TObject);
begin
  edtZoom.Text := IntToStr(StrToInt(edtZoom.Text) - 10);
end;

procedure TfrmMainBmp.btnPenSizeClick(Sender: TObject);
var
  p: TPoint;
begin
  p.X := 0;
  p.Y := 0;
  //p := Button1.ClientOrigin;
  p := btnPenSize.ClientToScreen(p);
  PopupMenu1.Popup(p.X, p.Y);
end;

procedure TfrmMainBmp.cbColorBoxChange(Sender: TObject);
begin
  if frmTransparent <> nil then
    frmTransparent.UpdateCursor;
end;

procedure TfrmMainBmp.chkTekenenClick(Sender: TObject);
begin
  if chkTekenen.Checked then
  begin
    chkEditMode.Checked := False;

    //imgPaint.Visible     := True;
    if frmTransparent = nil then
      frmTransparent := TfrmTransparent.Create(Self);
    frmTransparent.Show;

    cbColorBox.Visible := True;
    btnPenSize.Visible := True;
    actErase.Visible   := True;
    cbColorBox.Left    := chkTekenen.Left + chkTekenen.Width;
    btnPenSize.Left    := cbColorBox.Left + cbColorBox.Width;
    btnErase.Left      := btnPenSize.Left + btnPenSize.Width;

    //load png
    //edtPageChange(nil);
    //PaintScreenToBitmap;
  end
  else
  begin
    imgPaint.Visible   := False;
    cbColorBox.Visible := False;
    btnPenSize.Visible := False;
    actErase.Visible   := False;

    FreeAndNil(frmTransparent);
  end;
end;

procedure TfrmMainBmp.ControlMoved(Sender: TObject);
begin
  if FMover.Enabled then
    FMover.PositionNodes(Sender as TControl);
  if Sender is TFlashMemo then
    DrawMemoBackground(Sender as TFlashMemo);
end;

procedure TfrmMainBmp.actOpslaanExecute(Sender: TObject);
begin
  FFlashData.SaveToFile(FFlashData.XMLFileName);
end;

procedure TfrmMainBmp.edtPageKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_UP then
    actPrevious.Execute
  else if Key = VK_PRIOR then
    actPrevious.Execute
  else if Key = VK_DOWN then
    actNext.Execute
  else if Key = VK_NEXT then
    actNext.Execute
end;

procedure TfrmMainBmp.RefreshFlashImage;
var
  lbl: TFlashMemo;
begin
  ScrollBox1.Update;
  //Self.DoubleBuffered := True;
  frmFlashOffscreen.PaintToImage(imgFlash);
  for lbl in FTextLabels do
    //lbl.SpecialInvalidate;
    lbl.Update;
  //Self.DoubleBuffered := False;
end;

procedure TfrmMainBmp.RefreshTextButtons;
var
  page: TFlashPage;
  //btn: TFlashButton2;
  lbl: TFlashMemo;
  question: TFlashQuestion;
  i: Integer;
begin
  //FTextButtons.Clear;
  FTextLabels.Clear;
  FQuestions.Clear;
  FMover.MovableControls.Clear;

  if frmFlashOffscreen = nil then Exit;

  page := FFlashData.Pages[frmFlashOffscreen.ShockwaveFlash1.CurrentFrame];
  for i := 0 to page.DataCount - 1 do
  begin
    //btn := TFlashButton2.Create(nil);
    //FTextButtons.Add(btn);
    lbl := TFlashMemo.Create(nil);
    FTextLabels.Add(lbl);

    lbl.ReadOnly := not chkEditMode.Checked;
    lbl.Parent   := ScrollBox1;
    lbl.Data     := page.Data[i];
    lbl.OnMoved  := Self.ControlMoved;

    FMover.Add(lbl);
  end;

  for i := 0 to page.QuestionCount - 1 do
  begin
    question := TFlashQuestion.Create(nil);
    FQuestions.Add(question);

    question.Parent := ScrollBox1;
    question.Data   := page.Questions[i];

    FMover.Add(question);
  end;

  FMover.Enabled := False;
  FMover.Enabled := chkEditMode.Checked;
end;

procedure TfrmMainBmp.ReScaleTextMemos;
var
  i: Integer;
  memo: TFlashMemo;
  question: TFlashQuestion;
  //fPercentage: Double;
  iHorzScrol, iVertScrol: Integer;
begin
  iVertScrol := ScrollBox1.VertScrollBar.Position;
  iHorzScrol := ScrollBox1.HorzScrollBar.Position;
  ScrollBox1.VertScrollBar.Position := 0;
  ScrollBox1.HorzScrollBar.Position := 0;
  try
    for i := 0 to ScrollBox1.ControlCount-1 do
    begin
      if //(ScrollBox1.Controls[i] is TFlashButton2) or
         (ScrollBox1.Controls[i] is TFlashMemo) then
      begin
        (*
        if (pnlFlash.Controls[i] is TFlashButton2) then
        begin
          btn := pnlFlash.Controls[i] as TFlashButton2;
          btn.FFlashPos.RecalcScreenPositions;

          memo := btn.FMemo;
          memo.FFlashPos.RecalcScreenPositions;

  //        btn.FZoomBusy := True;
  //        try
            btn.SetBounds(btn.FFlashPos.ScreenLeft, btn.FFlashPos.ScreenTop,
                          btn.Width, btn.Height);   //note: also sets offset of memo below button
  //        finally
  //          btn.FZoomBusy := False;
  //        end;
            memo.SetBounds(memo.FFlashPos.ScreenLeft, memo.FFlashPos.ScreenTop,
                           memo.FFlashPos.ScreenWidth, memo.FFlashPos.ScreenHeigth);
            memo.Font.Size := memo.FFlashPos.ScreenFontSize;
        end
        else
        *)
        if (ScrollBox1.Controls[i] is TFlashMemo) then
        begin
          memo := ScrollBox1.Controls[i] as TFlashMemo;
          memo.FFlashPos.RecalcScreenPositions;
          memo.FZoomBusy := True;
          try
            memo.SetBounds(memo.FFlashPos.ScreenLeft,  memo.FFlashPos.ScreenTop,
                           memo.FFlashPos.ScreenWidth, memo.FFlashPos.ScreenHeigth);
          finally
            memo.FZoomBusy := False;
          end;

          //fPercentage    := StrToInt(edtZoom.Text) / 100;
          //scale font (depending on zoom + window size)
          if memo.Data.FontSize <= 0 then
            memo.Data.FontSize := 20;
          memo.Font.Size := Round(memo.Data.FontSize
                                  * memo.FFlashPos.Flash2ScreenRatio);

          //memo.FParentImage   := imgFlash;
          DrawMemoBackground(memo);

          //if memo.Lines.Count > 1 then
          if memo.Height < memo.HeightNeeded then
            memo.Height := memo.HeightNeeded;
          if memo.Width < memo.WidthNeeded then
            memo.Width := memo.WidthNeeded;
        end;
      end;

      if (ScrollBox1.Controls[i] is TFlashQuestion) then
      begin
        question := ScrollBox1.Controls[i] as TFlashQuestion;
        question.FFlashPos.RecalcScreenPositions;
        question.FZoomBusy := True;
        try
          question.SetBounds(question.FFlashPos.ScreenLeft,  question.FFlashPos.ScreenTop,
                             question.FFlashPos.ScreenWidth, question.FFlashPos.ScreenHeigth);
        finally
          question.FZoomBusy := False;
        end;

        //fPercentage    := StrToInt(edtZoom.Text) / 100;
        //scale font (depending on zoom + window size)
        //question.Font.Size := Round(20 * fPercentage * question.FFlashPos.Flash2ScreenRatio);
      end;
    end;

  finally
    ScrollBox1.VertScrollBar.Position := iVertScrol;
    ScrollBox1.HorzScrollBar.Position := iHorzScrol;
  end;
  FMover.Enabled := False;
  FMover.Enabled := chkEditMode.Checked;
end;

end.
