unit uROWebsocketServer;

interface

uses
  Classes,
  //copied from: http://code.google.com/p/bauglir-websocket/downloads/detail?name=BauglirWebSocket2_pascal_library.2.0.4.zip
  WebSocket2, CustomServer2,
  uROServer, uROBaseHTTPServer, SysUtils;

type
  TROWebsocketServer = class(TROServer)
  protected
    FServer: TWebSocketServer;
    procedure ServerAfterAddConnection(Server: TCustomServer; aConnection: TCustomConnection);
    procedure WebSocketConnectionDataFull(aSender: TWebSocketCustomConnection; aCode: integer; aData: TMemoryStream);
  protected
    FActive: Boolean;
    FPort: Integer;

    procedure IntSetActive(const Value: boolean); override;
    function  IntGetActive : boolean; override;

    function  GetPort: Integer;override;
    procedure SetPort(const Value: Integer);override;
    function  GetServerType: TROServerType; override;
  public
    constructor Create(aComponent: TComponent); override;
    destructor  Destroy; override;

    property Port;
  end;

implementation

uses
  uROClientIntf;

{ TROWebsocketServer }

constructor TROWebsocketServer.Create(aComponent: TComponent);
begin
  inherited;
end;

destructor TROWebsocketServer.Destroy;
begin
  //
  inherited;
end;

function TROWebsocketServer.GetPort: Integer;
begin
  Result := FPort;
end;

function TROWebsocketServer.GetServerType: TROServerType;
begin
  Result := rstTCP;
end;

function TROWebsocketServer.IntGetActive: boolean;
begin
  Result := FActive;
end;

procedure TROWebsocketServer.IntSetActive(const Value: boolean);
begin
  inherited;
  FActive := Value;

  if FActive then
  begin
    FServer := TWebSocketServer.Create('0.0.0.0', IntToStr(Self.Port) );
    FServer.OnAfterAddConnection := ServerAfterAddConnection;
    FServer.Start;
  end
  else
  begin
    //FServer.Stop;
    FServer.FreeOnTerminate := True;
    FServer.CloseAllConnections(0, 'Server stop');
    FServer.TerminateThread;
    //FServer.WaitFor();
    //FreeAndNil(FServer);
  end;
end;

procedure TROWebsocketServer.ServerAfterAddConnection(Server: TCustomServer;
  aConnection: TCustomConnection);
begin
  if aConnection is TWebSocketCustomConnection then
  begin
    (aConnection as TWebSocketCustomConnection).OnReadFull      := WebSocketConnectionDataFull;
    (aConnection as TWebSocketCustomConnection).FullDataProcess := True;
  end;
end;

procedure TROWebsocketServer.SetPort(const Value: Integer);
begin
  inherited;
  FPort := Value;
end;

type
  TTempTransport = class(TInterfacedObject,
                         IROTransport)
  protected
    FROServer: TROServer;
    {IROTransport}
    function GetTransportObject: TObject;
  public
    constructor Create(aROServer: TROServer);
  end;

procedure TROWebsocketServer.WebSocketConnectionDataFull(
  aSender: TWebSocketCustomConnection; aCode: integer; aData: TMemoryStream);
var
  strmResponse: TMemoryStream;
begin
  strmResponse := TMemoryStream.Create;
  try
    Self.DispatchMessage( TTempTransport.Create(Self), aData,  strmResponse);

    with TStreamReader.Create(strmResponse) do
    begin
      aSender.SendText(ReadToEnd);
      Free;
    end;
    //aSender.SendBinary(strmResponse);
  finally
    strmResponse.Free;
  end;
end;

{ TTempTransport }

constructor TTempTransport.Create(aROServer: TROServer);
begin
  FROServer := aROServer;
end;

function TTempTransport.GetTransportObject: TObject;
begin
  Result := FROServer;
end;

end.
