unit fServerForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  uROClient, uROPoweredByRemObjectsButton, uROClientIntf,
  uROJSONMessage, uROIpTCPServer, uROIndyHTTPServer, uROBaseHTTPServer,
  uROServer;

type
  TServerForm = class(TForm)
    RoPoweredByRemObjectsButton1: TRoPoweredByRemObjectsButton;
    ROMessage: TROJSONMessage;
    ROServer: TROIpTCPServer;
    ROIndyHTTPServer1: TROIndyHTTPServer;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ServerForm: TServerForm;

implementation

{$R *.dfm}

uses
  uROWebsocketServer;
var
  server: TROWebsocketServer;

procedure TServerForm.FormCreate(Sender: TObject);
begin
  ROServer.Active := true;

  server := TROWebsocketServer.Create(Self);
  server.Port   := 7000;
  server.Active := True;

  with (server.Dispatchers.Add as TROMessageDispatcher) do
  begin
    Name    := 'JSON';
    Message := Self.ROMessage;
    Enabled := True;
  end;
end;

end.
