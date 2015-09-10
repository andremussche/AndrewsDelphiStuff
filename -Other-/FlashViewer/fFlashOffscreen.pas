unit fFlashOffscreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, ShockwaveFlashObjects_TLB, ExtCtrls;

type
  TfrmFlashOffscreen = class(TForm)
    ShockwaveFlash1: TShockwaveFlash;
    procedure FormCreate(Sender: TObject);
  private
    FBitmap: TBitmap;
    FRatio_XY: Double;
    FFlashWidth : Integer;
    FFlashHeight: Integer;
    //FFlashZoom: Double;
  public
    destructor Destroy; override;

    procedure LoadFlash(const aFile: string);
    procedure PaintToImage(const aImage: TImage);

    property FlashWidth : Integer read FFlashWidth;
    property FlashHeight: Integer read FFlashHeight;
    //property FlashZoom  : Integer read FFlashZoom;
  end;

var
  frmFlashOffscreen: TfrmFlashOffscreen;

implementation

{$R *.dfm}

{ TfrmFlashOffscreen }

destructor TfrmFlashOffscreen.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TfrmFlashOffscreen.FormCreate(Sender: TObject);
begin
  FBitmap := TBitmap.Create;
end;

procedure TfrmFlashOffscreen.LoadFlash(const aFile: string);
var
  swidth, sheight: string;
//  iwidth, iheight: Integer;
begin
//  pnlFlash.Width  := Self.Width;
//  pnlFlash.Height := Self.Height;
//  pnlFlash.Top    := 0;
//  pnlFlash.Left   := 0;

  ShockwaveFlash1.Align   := alClient;
  ShockwaveFlash1.SAlign  := 'TL';       //top left
  ShockwaveFlash1.BackgroundColor := -1; //default
  //ShockwaveFlash1.Movie   := 'c:\-Andre-\-Projects-\FlashViewer\hoofdstuk1B.swf';
  ShockwaveFlash1.Movie   := aFile;
  ShockwaveFlash1.Visible := True;
  //ShockwaveFlash1.Play;

//  FFlashData.LoadFromFile(aFlashFile + '.xml');
//  FFlashData.PageCount := ShockwaveFlash1.TotalFrames;

  swidth  := ShockwaveFlash1.TGetProperty('/', 8);
  sheight := ShockwaveFlash1.TGetProperty('/', 9);
  DecimalSeparator := '.';
  ThousandSeparator := ',';
  FFlashWidth  := Round(StrToFloatDef(swidth , Self.Width));
  FFlashHeight := Round(StrToFloatDef(sheight, Self.Height));
  FRatio_XY    := FFlashWidth / FFlashHeight;

//  iwidth  := _FlashWidth;
//  iheight := _FlashHeight;
//  if iheight > AdvScrollBox1.Height then
//  begin
//    iheight := AdvScrollBox1.Height - 5;
//    iwidth  := AdvScrollBox1.Width - 5;
//  end;
//  ShockwaveFlash1.Width  := iwidth  ;
//  ShockwaveFlash1.Height := iheight ;

  Self.BorderStyle := bsNone;
  Self.Width   := 1;
  Self.Height  := 1;
  Self.Visible := True;
  Self.Visible := False;
  Application.ProcessMessages;
  Self.Width   := 100;
  Self.Height  := 100;

 //  edtPage.Text       := IntToStr(ShockwaveFlash1.CurrentFrame);
//  lblPageMax.Caption := IntToStr(ShockwaveFlash1.TotalFrames - 1);
//  RefreshTextButtons;
end;

procedure TfrmFlashOffscreen.PaintToImage(const aImage: TImage);
begin
  ShockwaveFlash1.Width  := aImage.Width;
  ShockwaveFlash1.Height := aImage.Height;

  FBitmap.Pixelformat := pf24bit; // if you want it device independent
  FBitmap.Width  := aImage.Width;
  FBitmap.Height := aImage.Height;

  ShockwaveFlash1.PaintTo(FBitmap.Canvas.Handle, 0, 0);

  //FBitmap.SaveToFile('Test1.bmp'); // *
  aImage.Picture.Assign(FBitmap);
end;

end.
