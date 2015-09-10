unit uFlashMemo;

interface

uses
  cTranspMemo, uFormData, uFlashRect, Classes,
  ExtCtrls, Messages, Controls;

type
  TFlashMemo = class(TTransparentMemo)
  private
    FData: TPageData;
    FShowNormalText: Boolean;
    FOnMoved: TNotifyEvent;
    procedure SetData(const Value: TPageData);
    procedure SetShowNormalText(const Value: Boolean);
  protected
    procedure TextChanged(Sender: TObject);
    procedure KeyPress(var Key: Char); override;
    procedure Change; override;

    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMKeyDown(var aMsg: TWMKeyDown); message WM_KEYDOWN;
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;

    function  HideText(const aText: string): string;
  public
    procedure AfterConstruction;override;

    procedure SpecialInvalidate;
    function  HeightNeeded: integer;
    function  WidthNeeded: integer;

    property  Data: TPageData read FData write SetData;
    property  ShowNormalText: Boolean read FShowNormalText write SetShowNormalText;

    //positioning
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    var FFlashPos: TFlashRect;
        FZoomBusy: Boolean;
    property  OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
  end;

implementation

uses
  StrUtils, SysUtils, Character, Forms, Windows, Graphics, Math;

{ TFlashMemo }

procedure TFlashMemo.AfterConstruction;
begin
  inherited;
  Self.BorderStyle := bsNone;
  Self.WordWrap    := False;
//  (Self.Lines as TMemoStrings).OnChange := Self.TextChanged;
end;

procedure TFlashMemo.Change;
begin
  inherited;
  if FData = nil then Exit;

  TextChanged(nil);
end;

procedure TFlashMemo.CMMouseEnter(var Message: TMessage);
begin
  if ReadOnly then
    HideCaret(Self.Handle)
  else
    inherited
end;

procedure TFlashMemo.CMTextChanged(var Message: TMessage);
begin
  inherited;
  if FData = nil then Exit;

  TextChanged(nil);
end;

function TFlashMemo.HideText(const aText: string): string;
var
  i: Integer;
begin
  Result := aText;
  for i := 1 to Length(aText) do
  begin
    if not CharInSet(aText[i], [' ',#10,#13]) then
      Result[i] := '#';//'*';
  end;
end;

function TFlashMemo.WidthNeeded: integer;
Var
  OldFont : HFont;
  Hand : THandle;
  TM : TTextMetric;
  tempint : integer;
  i: Integer;
begin
  Hand := GetDC(Self.Handle);
  try
    OldFont := SelectObject(Hand, Self.Font.Handle);
    try
      GetTextMetrics(Hand, TM);
      tempint := 0;
      for i := 0 to Lines.Count - 1 do
        tempint := Max(tempint,
                       (TM.tmAveCharWidth) * Length(Lines[i]));
    finally
      SelectObject(Hand, OldFont);
    end;
  finally
    ReleaseDC(Self.Handle, Hand);
  end;
  Result := tempint;
end;

procedure TFlashMemo.WMContextMenu(var Message: TWMContextMenu);
begin
  if ReadOnly then
    Exit
  else
    inherited;
end;

procedure TFlashMemo.WMKeyDown(var aMsg: TWMKeyDown);
var
  ks: TKeyboardState;
begin
  if ReadOnly then
    HideCaret(Self.Handle); 

  if not ReadOnly and
     (aMsg.CharCode in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
                        VK_ADD, VK_SUBTRACT]) then
  begin
    if GetKeyboardState(ks) and
       (ssCtrl in KeyboardStateToShiftState(ks)) then
    begin
      case aMsg.CharCode of
        VK_LEFT  : Left := Left - 1;
        VK_RIGHT : Left := Left + 1;
        VK_UP    : Top  := Top  - 1;
        VK_DOWN  : Top  := Top  + 1;

        VK_ADD   :
        begin
          Data.FontSize := Data.FontSize + 1;
          Font.Size := Round(Data.FontSize
                             * FFlashPos.Flash2ScreenRatio);
        end;
        VK_SUBTRACT   :
        begin
          Data.FontSize := Data.FontSize - 1;
          Font.Size := Round(Data.FontSize
                             * FFlashPos.Flash2ScreenRatio);
        end;

      end;

      if (ssCtrl  in KeyboardStateToShiftState(ks)) and
         (ssShift in KeyboardStateToShiftState(ks)) then
      case aMsg.CharCode of
        VK_LEFT  : Left := Left - 4;
        VK_RIGHT : Left := Left + 4;
        VK_UP    : Top  := Top  - 4;
        VK_DOWN  : Top  := Top  + 4;
      end;

      //update pos: redraw background
      SpecialInvalidate;
      Exit;
    end;
  end;

  inherited;
end;

procedure TFlashMemo.KeyPress(var Key: Char);
begin
  if not ShowNormalText then
    Key := #0;

  inherited KeyPress(Key);
  if FData = nil then Exit;

  TextChanged(nil);
end;

procedure TFlashMemo.TextChanged(Sender: TObject);
begin
  if FData = nil then Exit;

  if ShowNormalText then
    FData.AnswerText := Lines.Text;
end;

procedure TFlashMemo.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Self.ShowNormalText := not Self.ShowNormalText;
end;

procedure TFlashMemo.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if ReadOnly then
    HideCaret(Self.Handle)
  else
    inherited
end;

procedure TFlashMemo.WMNCHitTest(var Message: TWMNCHitTest);
begin
//  if ReadOnly then
//    HideCaret(Self.Handle);
//    Message.Result := HTBORDER //HTTRANSPARENT
//  else
  inherited;

//  Cursor := crDefault;
end;

procedure TFlashMemo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
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
    FData.AnswerPos.Top    := FFlashPos.FlashTop;
    FData.AnswerPos.Left   := FFlashPos.FlashLeft;
    FData.AnswerPos.Width  := FFlashPos.FlashWidth;
    FData.AnswerPos.Height := FFlashPos.FlashHeigth;
  end;

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  SpecialInvalidate;

  if Assigned(OnMoved) then
    OnMoved(Self);
end;

procedure TFlashMemo.SetData(const Value: TPageData);
var
  scrollb: TScrollBox;
begin
  FData := Value;
  if FData = nil then Exit;

  ShowNormalText := False; //default no normal text but hidden text
  //Lines.Text := FData.AnswerText;

  FFlashPos.FlashTop    := FData.AnswerPos.Top;
  FFlashPos.FlashLeft   := FData.AnswerPos.Left;
  FFlashPos.FlashWidth  := FData.AnswerPos.Width;
  FFlashPos.FlashHeigth := FData.AnswerPos.Height;

  scrollb := Parent as TScrollBox;
  if scrollb <> nil then
  begin
    Self.SetBounds(Self.FFlashPos.ScreenLeft - scrollb.HorzScrollBar.Position,
                   Self.FFlashPos.ScreenTop  - scrollb.VertScrollBar.Position,
                   Self.FFlashPos.ScreenWidth, Self.FFlashPos.ScreenHeigth)
  end
  else
    Self.SetBounds(Self.FFlashPos.ScreenLeft,  Self.FFlashPos.ScreenTop,
                   Self.FFlashPos.ScreenWidth, Self.FFlashPos.ScreenHeigth)
end;

procedure TFlashMemo.SetShowNormalText(const Value: Boolean);
begin
  FShowNormalText := Value;
  if ShowNormalText then
  begin
    Self.Lines.Text := FData.AnswerText;
  end
  else
    Self.Lines.Text := HideText(FData.AnswerText)
end;

procedure TFlashMemo.SpecialInvalidate;
begin
  if Parent <> nil then
    PostMessage(Handle,TMWM__SpecialInvalidate,0,0);
  Self.Invalidate;
end;

function TFlashMemo.HeightNeeded: integer;
Var
  OldFont : HFont;
  Hand : THandle;
  TM : TTextMetric;
//  Rect  : TRect;
  tempint : integer;
begin
  Hand := GetDC(Self.Handle);
  try
    OldFont := SelectObject(Hand, Self.Font.Handle);
    try
      GetTextMetrics(Hand, TM);
      //Self.Perform(EM_GETRECT, 0, longint(@Rect));
      tempint := //(Rect.Bottom - Rect.Top) div
                 (TM.tmHeight + TM.tmExternalLeading) *
                 Lines.Count;
    finally
      SelectObject(Hand, OldFont);
    end;
  finally
    ReleaseDC(Self.Handle, Hand);
  end;
  Result := tempint;
end;


end.
