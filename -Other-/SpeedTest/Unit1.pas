unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    btnFill1: TButton;
    Memo1: TMemo;
    btnFill2: TButton;
    btnFill3: TButton;
    btnSearch1: TButton;
    btnSearch2: TButton;
    btnSearch3: TButton;
    btnSearch4: TButton;
    btnWait1: TButton;
    btnWait2: TButton;
    btnJpegTest1: TButton;
    btnJpegTest2: TButton;
    procedure btnFill1Click(Sender: TObject);
    procedure btnFill2Click(Sender: TObject);
    procedure btnFill3Click(Sender: TObject);
    procedure btnSearch1Click(Sender: TObject);
    procedure btnSearch2Click(Sender: TObject);
    procedure btnSearch3Click(Sender: TObject);
    procedure btnSearch4Click(Sender: TObject);
    procedure btnWait1Click(Sender: TObject);
    procedure btnWait2Click(Sender: TObject);
    procedure btnJpegTest1Click(Sender: TObject);
    procedure btnJpegTest2Click(Sender: TObject);
  private
    { Private declarations }
    procedure SearchStringlist(const aStrings: TStrings);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Inifiles, DateUtils, Jpeg,
     _uAsmProfDllLoader;

{$R *.dfm}

procedure TForm1.btnFill1Click(Sender: TObject);
var
  i:integer;
begin
  Memo1.Clear;
  for i := 1 to 1000 do
    Memo1.Lines.Add( 'Item ' +
                     IntToStr( Random(1000) )
                   );
end;

procedure TForm1.btnFill2Click(Sender: TObject);
var
  i:integer;
begin
  Memo1.Lines.BeginUpdate;      //lock updates
  try
    Memo1.Clear;
    for i := 1 to 1000 do
      Memo1.Lines.Add( 'Item ' +
                       IntToStr( Random(1000) )
                     );
  finally
    Memo1.Lines.EndUpdate;      //unlock
  end;
end;

procedure TForm1.btnFill3Click(Sender: TObject);
var
  i:integer;
begin
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Clear;
    Memo1.Lines.Capacity := 1000;       //pre allocate space
    for i := 1 to 1000 do
      Memo1.Lines.Add( 'Item ' +
                       IntToStr( Random(1000) )
                     );
  finally
    Memo1.Lines.EndUpdate;
  end;
end;

procedure TForm1.btnJpegTest1Click(Sender: TObject);
var
  jpg: TJPEGImage;
  fs: TFileStream;
begin
  jpg := TJPEGImage.Create;
  fs  := TFileStream.Create('test.jpg', fmOpenRead or fmShareCompat);
  jpg.LoadFromStream(fs);
  fs.free;
  jpg.DIBNeeded;
  jpg.free;
end;

procedure TForm1.btnJpegTest2Click(Sender: TObject);
var
  jpg: TJPEGImage;
begin
  jpg := TJPEGImage.Create;
  jpg.LoadFromFile('test.jpg');
  jpg.DIBNeeded;
  jpg.free;
end;

procedure TForm1.btnSearch1Click(Sender: TObject);
begin
  SearchStringlist(Memo1.Lines);
end;

procedure TForm1.btnSearch2Click(Sender: TObject);
var
  str: TStringList;
begin
  str := TStringList.Create;
  str.Text := Memo1.Lines.Text;
  str.Sort;

  SearchStringlist(str);

  str.Free;
end;

procedure TForm1.btnSearch3Click(Sender: TObject);
var
  hl: THashedStringList;
begin
  hl := THashedStringList.Create;
  hl.Text := Memo1.Lines.Text;

  SearchStringlist(hl);

  hl.Free;
end;

procedure TForm1.btnSearch4Click(Sender: TObject);
var
  hl: THashedStringList;
begin
  hl := THashedStringList.Create;
  hl.Text := Memo1.Lines.Text;

  hl.Sort;
  SearchStringlist(hl);
  hl.Free;
end;

procedure TForm1.btnWait1Click(Sender: TObject);
var
  tStart: TDatetime;
begin
  tStart := Now;
  while MilliSecondsBetween(Now,tStart) < 5000 do
  begin
    Application.ProcessMessages;
    Sleep(1);
  end;
end;

procedure TForm1.btnWait2Click(Sender: TObject);
var
  tStart: TDatetime;
begin
  tStart := Now;
  while MilliSecondsBetween(Now,tStart) < 5000 do
    Application.HandleMessage;
end;

procedure TForm1.SearchStringlist(const aStrings: TStrings);
begin
  aStrings.IndexOf( '123');
  aStrings.IndexOf( '345');
  aStrings.IndexOf( '567');
  aStrings.IndexOf( '789');
end;

initialization
  Randomize;

  if LoadProfilerDll then
    ShowProfileForm;

end.
