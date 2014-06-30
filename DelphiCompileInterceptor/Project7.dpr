program Project7;

uses
  Forms,
  Unit7 in 'Unit7.pas' {Form7},
  InterceptIntf in 'InterceptIntf.pas',
  DbugIntf in 'C:\Projecten_IT\RBK Tools\DebuggingTools\RBKDelphiPlasticPlugin\DbugIntf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
