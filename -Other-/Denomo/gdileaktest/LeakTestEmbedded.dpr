program LeakTestEmbedded;

uses
  Forms,
  Unit7 in 'Unit7.pas' {Form7},
  RuntimeDllMain in '..\RuntimeDllMain.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
