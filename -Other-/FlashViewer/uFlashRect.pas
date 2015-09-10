unit uFlashRect;

interface

type
  TFlashRect = record
  private
    FFlashTop: Integer;
    FFlashHeigth: Integer;
    FFlashLeft: Integer;
    FFlashWidth: Integer;
    FFlashFontSize: Integer;
    procedure SetFlashHeigth(const Value: Integer);
    procedure SetFlashLeft(const Value: Integer);
    procedure SetFlashTop(const Value: Integer);
    procedure SetFlashWidth(const Value: Integer);
    procedure SetFlashFontSize(const Value: Integer);
  private
    FScreenTop: Integer;
    FScreenHeigth: Integer;
    FScreenLeft: Integer;
    FScreenWidth: Integer;
    FScreenFontSize: Integer;
    procedure SetScreenHeigth(const Value: Integer);
    procedure SetScreenLeft(const Value: Integer);
    procedure SetScreenTop(const Value: Integer);
    procedure SetScreenWidth(const Value: Integer);
    procedure SetScreenFontSize(const Value: Integer);

    function GetFlash2ScreenRatio: Double;
  public
    property ScreenTop   : Integer read FScreenTop write SetScreenTop;
    property ScreenLeft  : Integer read FScreenLeft write SetScreenLeft;
    property ScreenWidth : Integer read FScreenWidth write SetScreenWidth;
    property ScreenHeigth: Integer read FScreenHeigth write SetScreenHeigth;
    property ScreenFontSize: Integer read FScreenFontSize write SetScreenFontSize;

    procedure RecalcScreenPositions;
    property  Flash2ScreenRatio: Double read GetFlash2ScreenRatio;

    property FlashTop   : Integer read FFlashTop write SetFlashTop;
    property FlashLeft  : Integer read FFlashLeft write SetFlashLeft;
    property FlashWidth : Integer read FFlashWidth write SetFlashWidth;
    property FlashHeigth: Integer read FFlashHeigth write SetFlashHeigth;
    property FlashFontSize: Integer read FFlashFontSize write SetFlashFontSize;
  end;

implementation

uses
  fFlashOffscreen, Math;

{ TFlashRect }

function TFlashRect.GetFlash2ScreenRatio: Double;
var fFlashWidth2PanelRatio, fFlashHeight2PanelRatio: Double;
begin
  with frmFlashOffscreen do
  begin
    fFlashWidth2PanelRatio  := ShockwaveFlash1.Width  / Max(frmFlashOffscreen.FlashWidth, 1);
    fFlashHeight2PanelRatio := ShockwaveFlash1.Height / Max(frmFlashOffscreen.FlashHeight, 1);
  end;
  //proportional fix
  Result := Min(fFlashWidth2PanelRatio, fFlashHeight2PanelRatio);
end;

procedure TFlashRect.RecalcScreenPositions;
begin
  FScreenHeigth   := Round(FFlashHeigth   * GetFlash2ScreenRatio);
  FScreenLeft     := Round(FFlashLeft     * GetFlash2ScreenRatio);
  FScreenTop      := Round(FFlashTop      * GetFlash2ScreenRatio);
  FScreenWidth    := Round(FFlashWidth    * GetFlash2ScreenRatio);
  FScreenFontSize := Round(FFlashFontSize * GetFlash2ScreenRatio);
end;

procedure TFlashRect.SetFlashFontSize(const Value: Integer);
begin
  if FFlashFontSize = Value then Exit;
  FFlashFontSize  := Value;
  FScreenFontSize := Round(FFlashFontSize * GetFlash2ScreenRatio);
end;

procedure TFlashRect.SetFlashHeigth(const Value: Integer);
begin
  if FFlashHeigth = Value then Exit;
  FFlashHeigth  := Value;
  FScreenHeigth := Round(FFlashHeigth * GetFlash2ScreenRatio);
end;

procedure TFlashRect.SetFlashLeft(const Value: Integer);
begin
  if FFlashLeft = Value then Exit;
  FFlashLeft  := Value;
  FScreenLeft := Round(FFlashLeft * GetFlash2ScreenRatio);
end;

procedure TFlashRect.SetFlashTop(const Value: Integer);
begin
  if FFlashTop = Value then Exit;
  FFlashTop  := Value;
  FScreenTop := Round(FFlashTop * GetFlash2ScreenRatio);
end;

procedure TFlashRect.SetFlashWidth(const Value: Integer);
begin
  if FFlashWidth = Value then Exit;
  FFlashWidth  := Value;
  FScreenWidth := Round(FFlashWidth * GetFlash2ScreenRatio);
end;

procedure TFlashRect.SetScreenFontSize(const Value: Integer);
begin
  if FScreenFontSize = Value then Exit;
  FScreenFontSize := Value;
  FFlashFontSize  := Round(FScreenFontSize / GetFlash2ScreenRatio);
end;

procedure TFlashRect.SetScreenHeigth(const Value: Integer);
begin
  if FScreenHeigth = Value then Exit;
  FScreenHeigth := Value;
  FFlashHeigth  := Round(FScreenHeigth / GetFlash2ScreenRatio);
end;

procedure TFlashRect.SetScreenLeft(const Value: Integer);
begin
  if FScreenLeft = Value then Exit;
  FScreenLeft := Value;
  FFlashLeft  := Round(FScreenLeft / GetFlash2ScreenRatio);
end;

procedure TFlashRect.SetScreenTop(const Value: Integer);
begin
  if FScreenTop = Value then Exit;
  FScreenTop := Value;
  FFlashTop  := Round(FScreenTop / GetFlash2ScreenRatio);
end;

procedure TFlashRect.SetScreenWidth(const Value: Integer);
begin
  if FScreenWidth = Value then Exit;
  FScreenWidth := Value;
  FFlashWidth  := Round(FScreenWidth / GetFlash2ScreenRatio);
end;

end.
