program FiberTest;

uses
  Forms,
  Unit2 in 'Unit2.pas' {Form2},
  DSharp.Core.Fibers in 'DSharp.Core.Fibers.pas',
  AsyncCalls in 'AsyncCalls.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
