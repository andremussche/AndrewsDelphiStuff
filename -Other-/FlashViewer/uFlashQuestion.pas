unit uFlashQuestion;

interface

uses
  ExtCtrls, Controls,
  uFormData, uFlashRect, Messages, Dialogs;

type
  //TFlashQuestion = class(TPanel)
  TFlashQuestion = class(TShape)
  private
    FData: TQuestionData;
    procedure SetData(const Value: TQuestionData);
  protected
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure Paint; override;
  public
    procedure AfterConstruction;override;

    property  Data: TQuestionData read FData write SetData;

    //positioning
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    var FFlashPos: TFlashRect;
        FZoomBusy: Boolean;

    class var EditMode: Boolean;
  end;

implementation

uses
  SysUtils, Forms, IOUtils, fImagePopup, Graphics;

{ TFlashQuestion }

procedure TFlashQuestion.AfterConstruction;
begin
  inherited;

//  Self.ShowCaption := False;
//  Self.BevelOuter  := bvNone;

//exit;
//  with TShape.Create(Self) do
//  begin
//    Align  := alClient;
  Shape  := stEllipse;

  Brush.Style := bsSolid;
  Brush.Color := clWhite;

  Pen.Color := clGreen;
  Pen.Width := 2;

//    Parent := Self;
//  end;
end;

procedure TFlashQuestion.Paint;
var x, y: Integer;
begin
  inherited Paint;

  Canvas.Font.Size := Self.Height div 2;
  x := (Self.Width div 2)  - (Canvas.TextWidth('?')  div 2);
  y := (Self.Height div 2) - (Canvas.TextHeight('?') div 2);
  Canvas.TextOut(x, y, '?');
end;

procedure TFlashQuestion.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  scrollb: TScrollBox;
begin
  if not FZoomBusy then
  begin
    scrollb := Parent as TScrollBox;
    if scrollb <> nil then
    begin
      FFlashPos.ScreenLeft   := ALeft + scrollb.HorzScrollBar.Position;
      FFlashPos.ScreenTop    := ATop  + scrollb.VertScrollBar.Position;
    end
    else
    begin
      FFlashPos.ScreenLeft   := ALeft;
      FFlashPos.ScreenTop    := ATop;
    end;
    FFlashPos.ScreenWidth  := AWidth;
    FFlashPos.ScreenHeigth := AHeight;
  end;

  if FData <> nil then
  begin
    FData.QuestionPos.Top    := FFlashPos.FlashTop;
    FData.QuestionPos.Left   := FFlashPos.FlashLeft;
    FData.QuestionPos.Width  := FFlashPos.FlashWidth;
    FData.QuestionPos.Height := FFlashPos.FlashHeigth;
  end;

  inherited;
  Self.Invalidate;
end;

procedure TFlashQuestion.SetData(const Value: TQuestionData);
var
  scrollb: TScrollBox;
begin
  FData := Value;
  if FData = nil then Exit;

  FFlashPos.FlashTop    := FData.QuestionPos.Top;
  FFlashPos.FlashLeft   := FData.QuestionPos.Left;
  FFlashPos.FlashWidth  := FData.QuestionPos.Width;
  FFlashPos.FlashHeigth := FData.QuestionPos.Height;

  scrollb := Parent as TScrollBox;
  if scrollb <> nil then
  begin
    Self.SetBounds(Self.FFlashPos.ScreenLeft - scrollb.HorzScrollBar.Position,
                   Self.FFlashPos.ScreenTop  - scrollb.VertScrollBar.Position,
                   Self.FFlashPos.ScreenWidth, Self.FFlashPos.ScreenHeigth)
  end
  else
    Self.SetBounds(Self.FFlashPos.ScreenLeft,  Self.FFlashPos.ScreenTop,
                   Self.FFlashPos.ScreenWidth, Self.FFlashPos.ScreenHeigth);
end;

procedure TFlashQuestion.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  sDir,
  sFile, sRel: string;
begin
  //if frmMainBmp.chkEditMode.Checked then
  if EditMode then  
  begin
    with TOpenDialog.Create(nil) do
    try
      DefaultExt := '*.jpg';
      if Execute then
      begin
        sFile := FileName;
        sDir  := ExtractFilePath(Application.ExeName);
        sRel  := ExtractRelativePath(sDir, sFile);
        Self.Data.ImageFile := sRel;
      end;
    finally
      Free;
    end;
  end
  else
  begin
    TfrmImagePopup.ShowImage(Self.Data.ImageFile);
  end;
end;

end.
