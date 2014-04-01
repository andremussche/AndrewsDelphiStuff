unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frxClass;

type
  TForm2 = class(TForm)
    frxReport1: TfrxReport;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  WinSpool, frxPrinter;

{$R *.dfm}

procedure WriteRawStringToPrinter(const aPrinterName, aDocument, aData: string);
var
  h: THandle;
  N: DWORD;
  docInfo: TDocInfo1;
begin
  if not OpenPrinter(PChar(aPrinterName), h, nil) then Assert(false);
  try
    with docInfo do
    begin
      pDocName    := PChar(aDocument);
      pOutputFile := nil;
      pDataType   := 'RAW';
    end;
    if StartDocPrinter(h, 1, @docInfo) = 0 then Assert(false);
    if not StartPagePrinter(h) then Assert(false);
    if not WritePrinter(h, PAnsiChar(AnsiString(aData)), Length(AnsiString(aData)), N) then Assert(false);
    if not EndPagePrinter(h) then Assert(false);
    if not EndDocPrinter(h) then Assert(false);
  finally
    ClosePrinter(h);
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  i,j: Integer;
  page: TfrxPage;
  c: TfrxComponent;
  fm: TfrxMemoView;
  x,y,w,h: Double;
begin
  frxReport1.PrepareReport();
  if frxPrinters.Printers.Count <= 0 then
    frxPrinters.FillPrinters;
  //frxPrinters.PrinterIndex := frxPrinters.IndexOf('FinePrint');

  Memo1.Clear;
  Memo1.Lines.Add('^XA');

  for i := 0 to frxReport1.PagesCount-1 do
  begin
    page := frxReport1.Pages[i];
    if page.BaseName <> 'Page' then Continue;

    for j := 0 to page.Objects.Count - 1 do
    begin
      c := TObject(page.Objects[j]) as TfrxComponent;
      if c is TfrxMemoView then
      begin
        fm := (c as TfrxMemoView);
        x := fm.Left;  //is in pixels
        y := fm.Top;

        //recalc from FR dpi (96dpi) to e.g. 200dpi of Zebra printer
        x := (x * frxPrinters.Printer.DPI.X) / 96;
        y := (y * frxPrinters.Printer.DPI.Y) / 96;
        //12 points = 12/72 logical inch = 1/6 logical inch = 96/6 pixels = 16 pixels
        //http://msdn.microsoft.com/en-us/library/windows/desktop/ff684173(v=vs.85).aspx
        h := Round( Abs(fm.Font.Height) * frxPrinters.Printer.DPI.Y / 72 );
        w := h * (12/15);  //default scale of font width vs height?

        //http://www.zebra.com/id/zebra/na/en/documentlibrary/manuals/en/zpl_ii_programming2.File.tmp/45541L-002_RA.pdf
        Memo1.Lines.Add(
          Format('^FO%0.0f,%0.0f^A0N,%0.0f,%0.0f^FD%s^FS',
                 [x, y, w, h, fm.Text]) );
      end
    end;
  end;

  Memo1.Lines.Add('^XZ');
  frxReport1.Print;

  //preview generated code with zplviewer
  //  http://sourceforge.net/projects/zplviewer/
  //or preview on printer:
  //  https://km.zebra.com/kb/index?page=content&id=SO8374
end;

end.
