program HitXmlDemo;

uses
  Forms,
  XMLDemo in 'XMLDemo.pas' {Form1},
  XMLModel in 'XMLModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
