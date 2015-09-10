unit FMandelTest;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Math, ExtCtrls;

type
  Double = Single;  //GPU OpenCL only supports Single, so we use single to get good comparison

  {$IF CompilerVersion >= 23.0}
   {$EXCESSPRECISION OFF}
  {$IFEND}

  TForm20 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Shape1: TShape;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    FBitmap : TBitmap;
    FScanLines : array of PIntegerArray;

    iterLimit : Integer;
    qmin, qmax, pmin, pmax : Double;
    controlColors : array of TColor;
    colors : array of TColor;
    mbX, mbY : Integer;

    procedure ResetMandel;

    procedure ResetControlColors;
    procedure ComputeColors;

    procedure ComputeMandel;
    procedure ComputeMandelDelphi;
//    procedure ComputeMandelSSE;

    procedure DrawPixel(x, y, c : Integer);
  end;

var
  Form20: TForm20;

implementation

uses
  ShellAPI;

{$R *.dfm}

const
   MAX_COLORS = 512;

procedure TForm20.FormCreate(Sender: TObject);
var
   i : Integer;
begin
   FBitmap:=TBitmap.Create;
   FBitmap.PixelFormat:=pf32bit;
   FBitmap.SetSize(Image1.Width, Image1.Height);
   Image1.Picture.Bitmap:=FBitmap;

   SetLength(FScanLines, FBitmap.Height);
   for i:=0 to FBitmap.Height-1 do
      FScanLines[i]:=FBitmap.ScanLine[i];

   ResetMandel;
   ResetControlColors;
   ComputeColors;

   ComputeMandel;
end;

procedure TForm20.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   mbX:=X;
   mbY:=Y;
end;

procedure TForm20.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
   s : Integer;
begin
   if ssLeft in Shift then begin
      s := Max(X-mbX, Y-mbY);
      if s>0 then begin
         Shape1.SetBounds(mbX+Image1.Left, mbY+Image1.Top, s, s);
         Shape1.Visible:=True;
      end;
   end;
end;

procedure TForm20.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   s : Integer;
   pw, qw : Double;
begin
   Shape1.Visible:=False;

   s:=Max(X-mbX, Y-mbY);
   if (s>3) then begin
      X := mbX + s;
      Y := mbY + s;
      pw := pmax - pmin;
      pmin := pmin + mbX * pw / FBitmap.Width;
      pmax := pmax - (FBitmap.Width - X) * pw / FBitmap.Width;
      qw := qmax - qmin;
      qmin := qmin + (FBitmap.Height - Y) * qw / FBitmap.Height;
      qmax := qmax - mbY * qw / FBitmap.Height;

      ComputeMandel;
   end;
end;

// ResetMandel
//
procedure TForm20.ResetMandel;
begin
   iterLimit := 100;
   qmin := -1.5;
   qmax := 1.5;
   pmin := -2.25;
   pmax := 0.75;
end;

// ResetControlColors
//
procedure TForm20.ResetControlColors;
begin
   SetLength(controlColors, 5);

   controlColors[0] := RGB($20, $00, $00);
   controlColors[1] := RGB($ff, $ff, $ff);
   controlColors[2] := RGB($A0, $00, $00);
   controlColors[3] := RGB($FF, $ff, $40);
   controlColors[4] := RGB($FF, $20, $20);
end;

const
//  C_Width  = 2048;
//  C_Heigth = 2048;
  C_Width  = 1024;
  C_Heigth = 1024;
var
  host_image: Array [0..C_Width*C_Heigth*4]of Byte;
  host_image4: Array [0..C_Width*C_Heigth]of TColor absolute host_image;

procedure SliceToFile(const FileName: AnsiString);

var
  F: File;
  BFH: TBitmapFileHeader;
  BIH: TBitmapInfoHeader;
begin
  Assign(F, FileName);
  Rewrite(F,1);
  ZeroMemory(@BFH,SizeOf(BFH));
  ZeroMemory(@BIH,SizeOf(BIH));
  with BFH do
  begin
    bfType:=$4D42;
    bfSize:=SizeOf(BFH)+SizeOf(BIH)+C_Width*C_Heigth*4;
    bfOffBits:=SizeOf(BIH)+SizeOf(BFH);
  end;
  BlockWrite(F,BFH,SizeOf(BFH));
  with BIH do
  begin
    biSize:=SizeOf(BIH);
    biWidth:=C_Width;
    biHeight:=C_Heigth;
    biPlanes:=1;
    biBitCount:=32;
    biCompression:=BI_RGB;
    biSizeImage:=C_Width*C_Heigth*4;
  end;
  BlockWrite(F,BIH,SizeOf(BIH));
  BlockWrite(F,host_image,C_Width*C_Heigth*4*SizeOf(Byte));
  Close(F);
end;

// ComputeMandel
//
procedure TForm20.ComputeMandel;
var
   start, stop, freq : Int64;
begin
   QueryPerformanceCounter(start);

//   if CheckBox1.Checked then
//      ComputeMandelSSE
//   else
   ComputeMandelDelphi;

   QueryPerformanceCounter(stop);
   QueryPerformanceFrequency(freq);

   Image1.Picture.Bitmap:=FBitmap;

   Caption:=Format('%.1f milliseconds', [(stop-start)/freq*1000]);

  ForceDirectories('C:\temp\');
  SliceToFile('C:\temp\OpenCL.bmp');
  ShellExecute(0,'Open','C:\temp\OpenCL.bmp','','',SW_SHOWMAXIMIZED);
end;


// ComputeMandelDelphi
//
procedure TForm20.ComputeMandelDelphi;
const
   kmax = 256;
var
   xstep, ystep : Double;
   x, y, r : Double;
   sx, sy, k : Integer;
   p, q, x0, y0 : Double;

   color: TRGBTriple;

  function __mynormalize(v: byte): byte;inline;
  begin
     if (v>255) then
       Exit(0);
     Result := v;
  end;
var idx: Integer;

begin
//   xstep := (pmax - pmin) / FBitmap.Width;
//   ystep := (qmax - qmin) / FBitmap.Height;
   xstep := (pmax - pmin) / c_width;
   ystep := (qmax - qmin) / c_width;

   for sx := 0 to c_width-1 do
   begin
      for sy := 0 to C_Heigth-1 do
      begin

         p := pmin + xstep * sx;
         q := qmax - ystep * sy;
         k := 0;
         x0 := 0;
         y0 := 0;

         repeat
            x := x0 * x0 - y0 * y0 + p;
            y := 2 * x0 * y0 + q;
            x0 := x;
            y0 := y;
            r := x * x + y * y;
            Inc(k);
         until ((r > iterLimit) or (k >= kmax));

         (*
         idx := sx*4 + 4*c_width*sy;
         if k >= kmax then
         begin
            host_image[idx]     := 0;
            host_image[idx + 1] := 0;
            host_image[idx + 2] := 0;
            host_image[idx + 3] := 255;
         end
         else
         begin
            host_image[idx]     := __mynormalize(trunc(k*5*x*y/100));
            host_image[idx + 1] := __mynormalize(trunc(k*x/10));
            host_image[idx + 2] := __mynormalize(trunc(k*y/10));
            host_image[idx + 3] := 255;
         end;
         *)

         idx := sx + c_width*sy;
         if k >= kmax then
         begin
           host_image4[idx] := 0
         end
         else
           host_image4[idx] := colors[k];

//         if k >= kmax then
//            k := 0;
//         DrawPixel(sx, sy, k);
      end;
   end;
end;

// ComputeMandelSSE
//
(*
procedure TForm20.ComputeMandelSSE;
const
   kmax = 256;
   c2 : Double = 2;
var
   xstep, ystep : Double;
   r : Double;
   sx, sy, k : Integer;
   p, q, x0, y0 : Double;
begin
   xstep := (pmax - pmin) / FBitmap.Width;
   ystep := (qmax - qmin) / FBitmap.Height;

   for sx := 0 to FBitmap.Width-1 do begin
      for sy := 0 to FBitmap.Height-1 do begin

         p := pmin + xstep * sx;
         q := qmax - ystep * sy;
         k := 0;
         x0 := 0;
         y0 := 0;
         asm
            movsd xmm0, x0;
            movsd xmm1, y0;
         end;

         repeat
            asm
               // x := x0 * x0 - y0 * y0 + p;
               movsd xmm2, xmm0
               mulsd xmm2, xmm2
               movsd xmm3, xmm1
               mulsd xmm3, xmm3
               subsd xmm2, xmm3
               addsd xmm2, p
               // y := 2 * x0 * y0 + q;
               // y0 :=y
               mulsd xmm1, xmm0
               mulsd xmm1, c2
               addsd xmm1, q
               // x0 := x
               movsd xmm0, xmm2
               // r := x * x + y * y;
               mulsd xmm2, xmm2
               movsd xmm3, xmm1
               mulsd xmm3, xmm1
               addsd xmm2, xmm3
               movsd r, xmm2
            end;
            Inc(k);
         until ((r > iterLimit) or (k >= kmax));

         if k >= kmax then
            k := 0;

         DrawPixel(sx, sy, k);
      end;
   end;
end;
*)

// ComputeColors
//
procedure TForm20.Button1Click(Sender: TObject);
begin
   ResetMandel;
   ComputeMandel;
end;

procedure TForm20.CheckBox1Click(Sender: TObject);
begin
   ComputeMandel;
end;

procedure TForm20.ComputeColors;
var
   i, k : Integer;
   rstep, bstep, gstep : Double;
begin
   SetLength(colors, MAX_COLORS);

   colors[0] := RGB(0, 0, 0);

   for i:=0 to High(controlColors) do begin
      rstep := (GetRValue(controlColors[i + 1]) - GetRValue(controlColors[i])) / 63;
      gstep := (GetGValue(controlColors[i + 1]) - GetGValue(controlColors[i])) / 63;
      bstep := (GetBValue(controlColors[i + 1]) - GetBValue(controlColors[i])) / 63;

      for k:=0 to 63 do
         colors[k + (i * 64) + 1] := RGB(Round(GetRValue(controlColors[i]) + rstep * k),
                                         Round(GetGValue(controlColors[i]) + gstep * k),
                                         Round(GetBValue(controlColors[i]) + bstep * k));
   end;

    for i := 257 to MAX_COLORS-1 do
       colors[i] := colors[i - 256];
end;

// DrawPixel
//
procedure TForm20.DrawPixel(x, y, c : Integer);
begin
   FScanLines[y][x] := colors[c];
end;

end.
