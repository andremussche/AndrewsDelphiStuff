program ROWebSocketsClient;

uses
  uROComInit,
  Forms,
  fClientForm in 'fClientForm.pas' {ClientForm},
  uROIndyHTTPWebsocketChannel in 'IndyWebsocket\uROIndyHTTPWebsocketChannel.pas',
  IdIOHandlerWebsocket in 'IndyWebsocket\IdIOHandlerWebsocket.pas',
  IdHTTPWebsocketClient in 'IndyWebsocket\IdHTTPWebsocketClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
