program XPSPipePrinter;

uses
  Forms,
  fMainform in 'fMainform.pas' {frmMain},
  Pipes in 'Pipes.pas',
  frPipePrinter in 'frPipePrinter.pas' {framPipePrinter: TFrame},
  dPipeServer in 'dPipeServer.pas' {dmPipeServer: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TdmPipeServer, dmPipeServer);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
