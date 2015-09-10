program LeakGenerator;

uses
  Denomo in '..\Denomo.pas',
  Forms,
  LeakGeneratorMain in 'LeakGeneratorMain.pas' {FormMain},
  LeakGen in 'LeakGen.pas',
  LeakGenFrame in 'LeakGenFrame.pas' {FrameLeakGen: TFrame},
  MiscUtils in '..\MiscUtils.pas';

{$R *.res}

{$R denomores.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

