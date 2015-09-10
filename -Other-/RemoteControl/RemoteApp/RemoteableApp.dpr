program RemoteableApp;

uses
  Forms,
  mfTestMain in 'mfTestMain.pas' {frmMain},
  mfrBaseFrame in 'mfrBaseFrame.pas' {framBase: TFrame},
  mfrTestFrame in 'mfrTestFrame.pas' {framTest: TFrame},
  mfTestBase in 'mfTestBase.pas' {frmTestBase},
  mfTest in 'mfTest.pas' {frmTest},
  RemoteControlServer in 'RemoteControlServer.pas' {dmRemoteControlServer},
  RemoteControlMethods in 'RemoteControlMethods.pas',
  CommunicationTypes in '..\Shared\CommunicationTypes.pas',
  BaseObject in '..\Shared\BaseObject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdmRemoteControlServer, dmRemoteControlServer);
  Application.Run;
end.
