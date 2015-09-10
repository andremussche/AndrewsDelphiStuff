unit fMainform;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdTCPServer, StdCtrls,
  ShellAPI, Pipes, frPipePrinter, ExtCtrls, Generics.Collections;

type
  TfrmMain = class(TForm)
    framPipePrinter1: TframPipePrinter;
    Panel1: TPanel;
    btnPipe: TButton;
    btnLoad: TButton;
    btnSave: TButton;
    ScrollBox1: TScrollBox;
    Memo1: TMemo;
    Splitter1: TSplitter;
    procedure framPipePrinter1btnDeleteClick(Sender: TObject);
    procedure btnPipeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  protected
    FFrames: TObjectlist<TframPipePrinter>;
    function  AddPipe: TframPipePrinter;
    procedure LoadPipes;
    procedure SavePipes;
  public
    procedure  AfterConstruction;override;
    destructor Destroy; override;

    procedure UpdatePrintCount(const aPipe: string);
    procedure AddLog(const aMessage: string);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  DateUtils, IniFiles, dPipeServer;

{$R *.dfm}

procedure TfrmMain.AddLog(const aMessage: string);
begin
  Memo1.Lines.Add( Format('%s: %s',[FormatDatetime('hh:nn:ss:zzz', now),
                                    aMessage]) );
end;

function TfrmMain.AddPipe: TframPipePrinter;
begin
  Result := TframPipePrinter.Create(nil);
  FFrames.Add(Result);
  Result.Parent            := ScrollBox1;
  Result.Align             := alTop;
  Result.Top               := FFrames.Count * Result.Height + 1;
  Result.SectionName       := 'Pipe' + IntToStr(FFrames.Count);
  Result.btnDelete.OnClick := Self.framPipePrinter1btnDeleteClick;
end;

procedure TfrmMain.AfterConstruction;
begin
  FFrames := TObjectlist<TframPipePrinter>.Create(True);
  inherited;
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  LoadPipes;
end;

procedure TfrmMain.btnPipeClick(Sender: TObject);
begin
  AddPipe;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  SavePipes;
  dmPipeServer.LoadPipes;
end;

destructor TfrmMain.Destroy;
begin
  FFrames.Free;
  inherited;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Memo1.Clear;

  LoadPipes;
  dmPipeServer.LoadPipes;
end;

procedure TfrmMain.framPipePrinter1btnDeleteClick(Sender: TObject);
var
  frame: TframPipePrinter;
begin
  frame := (Sender as TButton).Owner as TframPipePrinter;
  FFrames.Remove(frame);
end;

procedure TfrmMain.LoadPipes;
var
  ini: TMemIniFile;
  pipes: TStrings;
  spipe: string;
  iCount: Integer;
  frame: TframPipePrinter;
begin
  FFrames.Clear;

  ini := TMemIniFile.Create( ExtractFilePath(Application.ExeName) + 'printerpipes.ini');
  try
    pipes := TStringList.Create;
    try
      ini.ReadSections(pipes);

      iCount := 0;
      for spipe in pipes do
      begin
        if iCount = 0 then
          frame := framPipePrinter1
        else
        begin
          frame := AddPipe;
        end;

        frame.SectionName   := spipe;
        frame.PipeName      := ini.ReadString(spipe, 'Pipe', '');
        frame.OutputPrinter := ini.ReadString(spipe, 'Printer', '');

        Inc(iCount);
      end;

    finally
      pipes.Free;
    end;
  finally
    ini.Free;
  end;
end;

procedure TfrmMain.SavePipes;
var
  ini: TMemIniFile;
  pipes: TStrings;
  spipe: string;
  iCount: Integer;
  frame: TframPipePrinter;
begin
  ini   := TMemIniFile.Create( ExtractFilePath(Application.ExeName) + 'printerpipes.ini');
  pipes := TStringList.Create;
  try
    ini.Clear;

    frame := framPipePrinter1;
    ini.WriteString(frame.SectionName, 'Pipe', frame.PipeName);
    ini.WriteString(frame.SectionName, 'Printer', frame.OutputPrinter);
    pipes.Add(frame.PipeName);

    for frame in FFrames do
    begin
      //check unique pipe
      iCount := 0;
      spipe  := frame.PipeName;
      while pipes.IndexOf(spipe) >= 0 do
      begin
        Inc(iCount);
        spipe := frame.PipeName + IntToStr(iCount);
      end;
      pipes.Add(spipe);
      frame.PipeName := spipe;

      ini.WriteString(frame.SectionName, 'Pipe', frame.PipeName);
      ini.WriteString(frame.SectionName, 'Printer', frame.OutputPrinter);
    end;

    ini.UpdateFile;
  finally
    pipes.Free;
    ini.Free;
  end;
end;

procedure TfrmMain.UpdatePrintCount(const aPipe: string);
var
  frame: TframPipePrinter;
begin
  if framPipePrinter1.PipeName = aPipe then
    framPipePrinter1.PrintCount := framPipePrinter1.PrintCount + 1
  else
  begin
    for frame in FFrames do
    begin
      if frame.PipeName = aPipe then
      begin
        frame.PrintCount := frame.PrintCount + 1;
        Exit;
      end;
    end;
  end;

end;

end.
