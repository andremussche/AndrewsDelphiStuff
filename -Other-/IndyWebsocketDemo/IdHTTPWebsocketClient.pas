unit IdHTTPWebsocketClient;

interface

uses
  IdHTTP, IdHashSHA1;

type
  TIdHTTPWebsocketClient = class(TIdHTTP)
  private
    FWSResourceName: string;
    FHash: TIdHashSHA1;
  protected
    procedure InternalUpgradeToWebsocket(aRaiseException: Boolean; out aFailedReason: string);
  public
    procedure  AfterConstruction;override;
    destructor Destroy; override;

    function  TryUpgradeToWebsocket: Boolean;
    procedure UpgradeToWebsocket;
  published
    property  Host;
    property  Port;
    property  WSResourceName: string read FWSResourceName write FWSResourceName;
  end;

implementation

uses
  Classes, IdCoderMIME, SysUtils, IdIOHandlerWebsocket;

{ TIdHTTPWebsocketClient }

procedure TIdHTTPWebsocketClient.AfterConstruction;
begin
  inherited;
  FHash := TIdHashSHA1.Create;
  IOHandler := TIdIOHandlerWebsocket.Create(nil);
end;

destructor TIdHTTPWebsocketClient.Destroy;
begin
  FHash.Free;
  inherited;
end;

function TIdHTTPWebsocketClient.TryUpgradeToWebsocket: Boolean;
var
  sError: string;
begin
  InternalUpgradeToWebsocket(False{no raise}, sError);
  Result := (sError = '');
end;

procedure TIdHTTPWebsocketClient.UpgradeToWebsocket;
var
  sError: string;
begin
  InternalUpgradeToWebsocket(True{raise}, sError);
end;

procedure TIdHTTPWebsocketClient.InternalUpgradeToWebsocket(aRaiseException: Boolean; out aFailedReason: string);
var
  sURL: string;
  strmResponse: TMemoryStream;
  i: Integer;
  sKey, sResponseKey: string;
begin
  Assert(not (IOHandler as TIdIOHandlerWebsocket).IsWebsocket);

  Request.Clear;

  //http://www.websocket.org/aboutwebsocket.html
  (* GET ws://echo.websocket.org/?encoding=text HTTP/1.1
     Origin: http://websocket.org
     Cookie: __utma=99as
     Connection: Upgrade
     Host: echo.websocket.org
     Sec-WebSocket-Key: uRovscZjNol/umbTt5uKmw==
     Upgrade: websocket
     Sec-WebSocket-Version: 13 *)

  //Connection: Upgrade
  Request.Connection := 'upgrade';
  //Upgrade: websocket
  Request.CustomHeaders.Add('Upgrade: websocket');

  //Sec-WebSocket-Key
  sKey := '';
  for i := 1 to 16 do
    sKey := sKey + Char(Random(127-32) + 32);
  //base64 encoded
  sKey := TIdEncoderMIME.EncodeString(sKey);
  Request.CustomHeaders.AddValue('Sec-WebSocket-Key', sKey);
  //Sec-WebSocket-Version: 13
  Request.CustomHeaders.AddValue('Sec-WebSocket-Version', '13');

  strmResponse := TMemoryStream.Create;
  try
    Request.Host := Format('Host: %s:%d',[Host,Port]);
    //ws://host:port/<resourcename>
    //about resourcename, see: http://dev.w3.org/html5/websockets/ "Parsing WebSocket URLs"
    sURL := Format('ws://%s:%d/%s', [Host, Port, WSResourceName]);
    Get(sURL, strmResponse, [101]);

    //http://www.websocket.org/aboutwebsocket.html
    (* HTTP/1.1 101 WebSocket Protocol Handshake
       Date: Fri, 10 Feb 2012 17:38:18 GMT
       Connection: Upgrade
       Server: Kaazing Gateway
       Upgrade: WebSocket
       Access-Control-Allow-Origin: http://websocket.org
       Access-Control-Allow-Credentials: true
       Sec-WebSocket-Accept: rLHCkw/SKsO9GAH/ZSFhBATDKrU=
       Access-Control-Allow-Headers: content-type *)

    //'HTTP/1.1 101 Switching Protocols'
    if ResponseCode <> 101 then
    begin
      aFailedReason := Format('Error while upgrading: "%d: %s"',[ResponseCode, ResponseText]);
      if aRaiseException then
        raise EIdWebSocketHandleError.Create(aFailedReason);
    end;
    //connection: upgrade
    if not SameText(Response.Connection, 'upgrade') then
    begin
      aFailedReason := Format('Connection not upgraded: "%s"',[Response.Connection]);
      if aRaiseException then
        raise EIdWebSocketHandleError.Create(aFailedReason);
    end;
    //upgrade: websocket
    if not SameText(Response.RawHeaders.Values['upgrade'], 'websocket') then
    begin
      aFailedReason := Format('Not upgraded to websocket: "%s"',[Response.RawHeaders.Values['upgrade']]);
      if aRaiseException then
        raise EIdWebSocketHandleError.Create(aFailedReason);
    end;
    //check handshake key
    sResponseKey := Trim(sKey) +                                         //... "minus any leading and trailing whitespace"
                    '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';              //special GUID
    sResponseKey := TIdEncoderMIME.EncodeBytes(                          //Base64
                         FHash.HashString(sResponseKey) );               //SHA1
    if not SameText(Response.RawHeaders.Values['sec-websocket-accept'], sResponseKey) then
    begin
      aFailedReason := 'Invalid key handshake';
      if aRaiseException then
        raise EIdWebSocketHandleError.Create(aFailedReason);
    end;

    //upgrade succesful
    (IOHandler as TIdIOHandlerWebsocket).IsWebsocket := True;
    aFailedReason := '';
  finally
    Request.Clear;
    strmResponse.Free;
  end;
end;

end.
