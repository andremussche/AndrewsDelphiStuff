program SpeedTest;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  _uAsmProfDllLoader in '..\DllVersion\_uAsmProfDllLoader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Speedtest';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
