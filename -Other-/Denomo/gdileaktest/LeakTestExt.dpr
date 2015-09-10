program LeakTestEmbedded;

uses
  Denomo,
  Forms,
  fMainExt in 'fMainExt.pas' {frmMainExt};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMainExt, frmMainExt);
  Application.Run;
end.
