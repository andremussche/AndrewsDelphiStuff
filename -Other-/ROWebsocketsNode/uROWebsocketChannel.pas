unit uROWebsocketChannel;

interface

uses
  Classes, SysUtils,
  uROClient, uROClientIntf,
  //copied from: http://code.google.com/p/bauglir-websocket/downloads/detail?name=BauglirWebSocket2_pascal_library.2.0.4.zip
  WebSocket2;

type
  TROWebsocketChannel = class(TROTransportChannel,
                              IROTransport, IROTCPTransport, IROTCPTransportProperties)
  private
    FClient: TWebSocketClientConnection;
    fKeepAlive: boolean;
    fDisableNagle: boolean;
    FHost: string;
    FPort: Integer;
    FTimeOut: Integer;
    FStreamRead: TMemoryStream;
    procedure WebSocketConnectionDataFull(aSender: TWebSocketCustomConnection; aCode: integer; aData: TMemoryStream);
  protected
    procedure IntDispatch(aRequest, aResponse : TStream); override;

    { IROTransport }
    function GetTransportObject : TObject; override;

    { IROTCPTransport }
    function GetClientAddress : string;

    {IROTCPTransportProperties}
    function  GetHost: string;
    function  GetPort: integer;
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: integer);
    function  GetTimeout: Integer;
    procedure SetTimeout(const Value: Integer);
  public
    constructor Create(aOwner : TComponent); override;
    procedure   Assign(aSource: TPersistent); override;
    destructor  Destroy; override;

    property Port : integer read GetPort write SetPort;
    property Host : string read GetHost write SetHost;
    //property DisableNagle : boolean read fDisableNagle write fDisableNagle default FALSE;
    //property KeepAlive : boolean read fKeepAlive write fKeepAlive default false;
  published
    // from TROTransportChannel
    property DispatchOptions;
    property OnAfterProbingServer;
    property OnAfterProbingServers;
    property OnBeforeProbingServer;
    property OnBeforeProbingServers;
    property OnLoginNeeded;
    property OnProgress;
    property OnReceiveStream;
    property OnSendStream;
    property OnServerLocatorAssignment;
    property ProbeFrequency;
    property ProbeServers;
    property ServerLocators;
    property SynchronizedProbing;
  end;

implementation

{ TROWebsocketChannel }

procedure TROWebsocketChannel.Assign(aSource: TPersistent);
begin
  inherited;
  //
end;

constructor TROWebsocketChannel.Create(aOwner: TComponent);
begin
  inherited;
  //
end;

destructor TROWebsocketChannel.Destroy;
begin
  //
  inherited;
end;

function TROWebsocketChannel.GetClientAddress: string;
begin
  if FClient <> nil then
    Result := FClient.Origin
  else
    Result := '';
end;

function TROWebsocketChannel.GetHost: string;
begin
  Result := FHost;
end;

function TROWebsocketChannel.GetPort: integer;
begin
  Result := FPort;
end;

function TROWebsocketChannel.GetTimeout: Integer;
begin
  Result := FTimeOut;
end;

function TROWebsocketChannel.GetTransportObject: TObject;
begin
  Result := Self;
end;

procedure TROWebsocketChannel.IntDispatch(aRequest, aResponse: TStream);
begin
  if FClient = nil then
    FClient := TWebSocketClientConnection.Create(FHost, IntToStr(FPort),
                                                 Format('ws://%s:%d/', [FHost, FPort]) );

  FClient.Start;
  while not FClient.CanReceiveOrSend do
    Sleep(10);

  FClient.FullDataProcess := True;
  FClient.OnReadFull      := WebSocketConnectionDataFull;
  FStreamRead := nil;

  //FClient.SendBinary(aRequest);
  with TStreamReader.Create(aRequest) do
  begin
    FClient.SendText(ReadToEnd);
    Free;
  end;

  while FStreamRead = nil do
    CheckSynchronize(10);

  aResponse.CopyFrom(FStreamRead, FStreamRead.Size);
end;

procedure TROWebsocketChannel.SetHost(const Value: string);
begin
  FHost := Value;
end;

procedure TROWebsocketChannel.SetPort(const Value: integer);
begin
  FPort := Value;
end;

procedure TROWebsocketChannel.SetTimeout(const Value: Integer);
begin
  FTimeOut := Value;
end;

procedure TROWebsocketChannel.WebSocketConnectionDataFull(
  aSender: TWebSocketCustomConnection; aCode: integer; aData: TMemoryStream);
begin
  FStreamRead := aData;
end;

end.
