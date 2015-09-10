program ROWebSocketsServer;

{#ROGEN:NewLibrary.rodl} // RemObjects: Careful, do not remove!

uses
  uROComInit,
  Forms,
  fServerForm in 'fServerForm.pas' {ServerForm},
  NewLibrary_Intf in 'NewLibrary_Intf.pas',
  NewLibrary_Invk in 'NewLibrary_Invk.pas',
  NewService_Impl in 'NewService_Impl.pas' {NewService: TRORemoteDataModule},
  uROWebsocketServer in 'uROWebsocketServer.pas';

{$R *.res}
{$R RODLFile.res}

begin
  Application.Initialize;
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
