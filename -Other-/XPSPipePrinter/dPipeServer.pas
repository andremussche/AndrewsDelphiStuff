unit dPipeServer;

interface

uses
  Pipes, Windows,
  Generics.Collections,
  SysUtils, Classes;

type
  TPipeServerExt = class(TPipeServer)
  private
    FPrinterName: string;
    FData: TMemoryStream;
  public
    procedure  AfterConstruction;override;
    destructor Destroy; override;

    property PrinterName: string read FPrinterName write FPrinterName;
  end;

  TdmPipeServer = class(TDataModule)
  private
    FPipes: TObjectlist<TPipeServerExt>;

    procedure PipeConnect(Sender: TObject; Pipe: HPIPE);
    procedure PipeDisconnect(Sender: TObject; Pipe: HPIPE);
    procedure PipeMessage(Sender: TObject; Pipe: HPIPE; Stream: TStream);
    procedure PipeSent(Sender: TObject; Pipe: HPIPE; Size: DWORD);
    procedure PipeError(Sender: TObject; Pipe: HPIPE; PipeContext: TPipeContext; ErrorCode: Integer);

    procedure PrintData(aPipe: TPipeServerExt);
  public
    procedure  AfterConstruction;override;
    destructor Destroy; override;

    procedure LoadPipes;
  end;

var
  dmPipeServer: TdmPipeServer;

implementation

uses
  Forms, ShellAPI, IniFiles, Printers, fMainform;

{$R *.dfm}

procedure TdmPipeServer.PipeSent(Sender: TObject; Pipe: HPIPE; Size: DWORD);
begin
  TThread.Queue(nil,
    procedure
    begin
      frmMain.AddLog('Data sent to ' +
                     (Sender as TPipeServerExt).PipeName +
                     ', Size = ' +
                     IntToStr(Size) );
    end);
end;

procedure TdmPipeServer.PrintData(aPipe: TPipeServerExt);
var
  sprintxps,
  sPrinter, sFile: string;
begin
  //save received data to file
  sFile := Format('%s\%s_%s.xps',
                  [ExtractFilePath(Application.ExeName),
                   'Temp_' + aPipe.PipeName,
                   FormatDatetime('hhnnsszzz', now)]);
  aPipe.FData.SaveToFile(sFile);
  aPipe.FData.Clear;

  if FileExists('printxps.exe') then
    sprintxps := 'printxps.exe'
  else
    sprintxps := 'xpswin.exe';

  //print xps file to printer
  sPrinter := aPipe.PrinterName;
  if not FileExists(sprintxps) then
    ShellExecute(0, 'print',
                    PChar(Format('"%s"',
                                 [sFile])),
                    nil, nil, SW_SHOWNORMAL)
  else
    ShellExecute(0, 'open',
                    PChar(sprintxps),
                    PChar(Format('"%s" "%s"',
                                 [sPrinter, sFile])),
                    nil, SW_SHOWNORMAL);

  TThread.Queue(nil,
    procedure
    begin
      frmMain.AddLog('Printing: ' +
                     aPipe.PipeName);
      frmMain.UpdatePrintCount(aPipe.PipeName);
    end);
end;

procedure TdmPipeServer.AfterConstruction;
begin
  inherited;
  FPipes := TObjectlist<TPipeServerExt>.Create(True);
end;

destructor TdmPipeServer.Destroy;
begin
  FPipes.Free;
  inherited;
end;

procedure TdmPipeServer.LoadPipes;
var
  ini: TMemIniFile;
  pipes: TStrings;
  spipe: string;
  pipe: TPipeServerExt;
begin
  FPipes.Clear;

  ini := TMemIniFile.Create( ExtractFilePath(Application.ExeName) + 'printerpipes.ini');
  try
    pipes := TStringList.Create;
    try
      ini.ReadSections(pipes);

      for spipe in pipes do
      begin
        pipe := TPipeServerExt.Create(nil);
        pipe.PipeName      := ini.ReadString(spipe, 'Pipe', '');
        pipe.PrinterName   := ini.ReadString(spipe, 'Printer', '');
        pipe.OnPipeSent       := Self.PipeSent;
        pipe.OnPipeConnect    := Self.PipeConnect;
        pipe.OnPipeDisconnect := Self.PipeDisconnect;
        pipe.OnPipeMessage    := Self.PipeMessage;
        pipe.OnPipeError      := Self.PipeError;
        //printer exists? then make active
        pipe.Active        := Printer.Printers.IndexOf(pipe.PrinterName) >= 0;
      end;

    finally
      pipes.Free;
    end;
  finally
    ini.Free;
  end;
end;

procedure TdmPipeServer.PipeConnect(Sender: TObject; Pipe: HPIPE);
begin
  TThread.Queue(nil,
    procedure
    begin
      frmMain.AddLog('Connected: ' +
                     (Sender as TPipeServerExt).PipeName);
    end);

  (Sender as TPipeServerExt).FData.Clear;
end;

procedure TdmPipeServer.PipeDisconnect(Sender: TObject; Pipe: HPIPE);
begin
  TThread.Queue(nil,
    procedure
    begin
      frmMain.AddLog('Dicconnected: ' +
                     (Sender as TPipeServerExt).PipeName);
    end);

  PrintData( (Sender as TPipeServerExt) );
end;

procedure TdmPipeServer.PipeError(Sender: TObject; Pipe: HPIPE;
  PipeContext: TPipeContext; ErrorCode: Integer);
begin
  TThread.Queue(nil,
    procedure
    begin
      frmMain.AddLog('Error at ' +
                     (Sender as TPipeServerExt).PipeName +
                     ', errorcode = ' +
                     IntToStr(ErrorCode) );
    end);
end;

procedure TdmPipeServer.PipeMessage(Sender: TObject; Pipe: HPIPE; Stream: TStream);
begin
  TThread.Queue(nil,
    procedure
    begin
      frmMain.AddLog('Stream received at ' +
                     (Sender as TPipeServerExt).PipeName +
                     ', Size = ' +
                     IntToStr(Stream.Size) );
    end);

  Stream.Position := 0;
  with (Sender as TPipeServerExt) do
  begin
    FData.Position  := FData.Size;
    FData.CopyFrom(stream, Stream.Size);
  end;
end;

{ TPipeServerExt }

procedure TPipeServerExt.AfterConstruction;
begin
  inherited;
  FData := TMemoryStream.Create;
end;

destructor TPipeServerExt.Destroy;
begin
  FData.Free;
  inherited;
end;

end.
