program ProfilingDemos;

uses
  //TCmallocMM,
  //msvcrtMM,
  //ScaleMM2,
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  UHojaCalc in 'UHojaCalc.pas',
  _uAsmProfDllLoader in 'AsmProfiler Complete v1.1.2\Instrumenting\API\_uAsmProfDllLoader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
