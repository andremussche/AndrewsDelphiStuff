program RemoteControl;

uses
  Forms,
  mfRemoteMain in 'mfRemoteMain.pas' {Form7},
  CommunicationTypes in '..\Shared\CommunicationTypes.pas',
  BaseObject in '..\Shared\BaseObject.pas',
  mcRemoteApp in '..\Shared\mcRemoteApp.pas',
  RemoteConnector in '..\Shared\RemoteConnector.pas' {dmRemoteConnector: TDataModule},
  mcRemoteControlTypes in '..\Shared\mcRemoteControlTypes.pas',
  mcRemoteControlExecuter in '..\Shared\mcRemoteControlExecuter.pas' {$R *.res},
  JvCreateProcess in '..\Shared\JvCreateProcess.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.CreateForm(TdmRemoteConnector, dmRemoteConnector);
  //Application.CreateForm(TdmRemoteConnector, dmRemoteConnector);
  Application.Run;
end.
