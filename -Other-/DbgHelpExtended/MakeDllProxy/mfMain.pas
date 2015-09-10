unit mfMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    mmoGen: TMemo;
    Button2: TButton;
    mmoExports: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  JclPeImage, JclDebug, JclWin32;

{$R *.dfm}

procedure TfrmMain.Button1Click(Sender: TObject);
var
  smb: TJclDebugInfoSymbols;
  ne: TNotifyEvent;
  m: TMethod;
  info: TJclLocationInfo;
  //symbol: JclWin32.TImagehlpSymbolA;
  psymbol: PImagehlpSymbolA;
begin
  psymbol := GetMemory(SizeOf(TImagehlpSymbolA) + 20);
  ZeroMemory(psymbol, SizeOf(TImagehlpSymbolA) + 20);
  Move('Test', psymbol.Name[0], Length('Test'));

  smb := TJclDebugInfoSymbols.Create(HInstance); //load own exe
  smb.InitializeSource;

  ne := Button1Click;
  ZeroMemory(@info, SizeOf(info));
  smb.GetLocationInfo(TMethod(ne).Code, info);

end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  s: string;
begin
  mmoGen.Lines.Clear;

  mmoGen.Lines.Add('unit Unit2;');
  mmoGen.Lines.Add('');
  mmoGen.Lines.Add('interface');
  mmoGen.Lines.Add('');
  mmoGen.Lines.Add('  procedure LoadFunctions;');
  mmoGen.Lines.Add('');
  for s in Memo1.Lines do
    mmoGen.Lines.Add(Format('  procedure %s;', [s]));

  mmoGen.Lines.Add('');
  mmoGen.Lines.Add('implementation');
  mmoGen.Lines.Add('');
  mmoGen.Lines.Add('uses Windows;');
  mmoGen.Lines.Add('');
  mmoGen.Lines.Add('var');
  for s in Memo1.Lines do
    mmoGen.Lines.Add(Format('  p%s: Pointer;', [s]));
  mmoGen.Lines.Add('');
  mmoGen.Lines.Add('var');
  mmoGen.Lines.Add('  hDLL: THandle;');
  mmoGen.Lines.Add('');
  mmoGen.Lines.Add('procedure LoadFunctions;');
  mmoGen.Lines.Add('begin');
  mmoGen.Lines.Add('  hDLL := LoadLibrary(''dbghelp.dll'');');
  mmoGen.Lines.Add('');
  for s in Memo1.Lines do
    mmoGen.Lines.Add(Format('  p%s := GetProcAddress(hDll, ''%s'');', [s, s]));
  mmoGen.Lines.Add('end;');
  mmoGen.Lines.Add('');

  for s in Memo1.Lines do
  begin
    mmoGen.Lines.Add(Format('procedure %s;', [s]));
    mmoGen.Lines.Add('asm');
    mmoGen.Lines.Add(Format('  jmp p%s;', [s]));
    mmoGen.Lines.Add('end;');
    mmoGen.Lines.Add('');
  end;

  mmoGen.Lines.Add('end.');

  mmoExports.Lines.Clear;
  mmoExports.Lines.Add('exports');
  for s in Memo1.Lines do
    mmoExports.Lines.Add(Format('  %s,', [s]));
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  FImage: TJclPeBorImage;
  i: Integer;
  itm: TJclPeExportFuncItem;
begin
  FImage := TJclPeBorImage.Create;
  //FImage.AttachLoadedModule( LoadLibrary('dbghelp.dll') );
  if FImage.StatusOK and (FImage.ExportList.Count > 0) then
  begin
    Memo1.Lines.Clear;
    for i := 0 to FImage.ExportList.Count - 1 do
    begin
      itm := FImage.ExportList.Items[i];
      Memo1.Lines.Add(itm.Name);
    end;
  end;
end;

end.
