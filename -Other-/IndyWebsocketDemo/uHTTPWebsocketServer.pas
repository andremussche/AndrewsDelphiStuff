unit uHTTPWebsocketServer;

interface

uses
  Classes, IdServerIOHandlerWebsocket, IdIOHandlerWebsocket,
  IdThread,
  IdHTTPServer, IdContext, IdCustomHTTPServer, IdCustomTCPServer, IdHashSHA1;

type
  TWSMessageNotify = procedure(const aDataCode: TWSDataCode; const aRequest: TMemoryStream; out aResponse: TStream) of object;

  TIndyHTTPWebsocketServer = class(TIdHTTPServer)
  private
    FOnWSMessage: TWSMessageNotify;
  protected
    FHash: TIdHashSHA1;
    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
     AResponseInfo: TIdHTTPResponseInfo); override;

    procedure DoWSExecute(AContext: TIdContext);
  public
    procedure  AfterConstruction; override;
    destructor Destroy; override;
    procedure  Loaded; override;
  published
    property OnWSMessage: TWSMessageNotify read FOnWSMessage write FOnWSMessage;
  end;

  TIdServerWSContext = class(TIdServerContext)
  private
    FWebSocketKey: string;
    FWebSocketVersion: Integer;
    FPath: string;
    FWebSocketProtocol: string;
    FResourceName: string;
    FOrigin: string;
    FQuery: string;
    FHost: string;
    FWebSocketExtensions: string;
    FCookie: string;
  public
    function IOHandler: TIdIOHandlerWebsocket;
  public
    property Path        : string read FPath write FPath;
    property Query       : string read FQuery write FQuery;
    property ResourceName: string read FResourceName write FResourceName;
    property Host        : string read FHost write FHost;
    property Origin      : string read FOrigin write FOrigin;
    property Cookie      : string read FCookie write FCookie;

    property WebSocketKey       : string  read FWebSocketKey write FWebSocketKey;
    property WebSocketProtocol  : string  read FWebSocketProtocol write FWebSocketProtocol;
    property WebSocketVersion   : Integer read FWebSocketVersion write FWebSocketVersion;
    property WebSocketExtensions: string  read FWebSocketExtensions write FWebSocketExtensions;
  end;

implementation

uses
  SysUtils, IdCoderMIME, Windows;

procedure TIndyHTTPWebsocketServer.AfterConstruction;
begin
  inherited;
  //set special WebSockets stuff
  Self.ContextClass := TIdServerWSContext;
  if Self.IOHandler = nil then
    Self.IOHandler  := TIdServerIOHandlerWebsocket.Create(Self);

  FHash := TIdHashSHA1.Create;
end;

destructor TIndyHTTPWebsocketServer.Destroy;
begin
  FHash.Free;
  inherited;
end;

procedure TIndyHTTPWebsocketServer.DoWSExecute(AContext: TIdContext);
var
  strmRequest: TMemoryStream;
  strmResponse: TStream;
  wscode: TWSDataCode;
  wsIO: TIdIOHandlerWebsocket;
begin
  try
    while AContext.Connection.Connected do
    begin
      if (AContext.Connection.IOHandler.InputBuffer.Size > 0) or    //already some data?
         AContext.Connection.IOHandler.Readable(5 * 1000) then      //else wait some time (5s)
      begin
        strmResponse := TMemoryStream.Create;
        strmRequest  := TMemoryStream.Create;
        try
          wsIO := AContext.Connection.IOHandler as TIdIOHandlerWebsocket;

          strmRequest.Position := 0;
          //first is the type: text or bin
          wscode := TWSDataCode(wsIO.ReadLongWord);
          //then the length + data = stream
          wsIO.ReadStream(strmRequest);
          //ignore ping/pong messages
          if wscode in [wdcPing, wdcPong] then Continue;

          //EXECUTE FUNCTION
          if Assigned(OnWSMessage) then
          begin
            strmRequest.Position := 0;
            OnWSMessage(wscode, strmRequest, strmResponse);
            if strmResponse <> nil then
            begin
              strmResponse.Position := 0;

              //write result back (of the same type: text or bin)
              if wscode = wdcText then
                wsIO.Write(strmResponse, wdtText)
              else
                wsIO.Write(strmResponse, wdtBinary)
            end;
          end;
        finally
          strmRequest.Free;
          strmResponse.Free;
        end;
      end
      else
      //keep alive: send a ping each 5s
      begin
        wsIO := AContext.Connection.IOHandler as TIdIOHandlerWebsocket;
        //ping
        wsIO.WriteData(nil, wdcPing);
      end;

    end;
  finally
    AContext.Data := nil;
  end;
end;

procedure TIndyHTTPWebsocketServer.DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  sValue: string;
  context: TIdServerWSContext;
begin
  (* GET /chat HTTP/1.1
     Host: server.example.com
     Upgrade: websocket
     Connection: Upgrade
     Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
     Origin: http://example.com
     Sec-WebSocket-Protocol: chat, superchat
     Sec-WebSocket-Version: 13 *)

  (* GET ws://echo.websocket.org/?encoding=text HTTP/1.1
     Origin: http://websocket.org
     Cookie: __utma=99as
     Connection: Upgrade
     Host: echo.websocket.org
     Sec-WebSocket-Key: uRovscZjNol/umbTt5uKmw==
     Upgrade: websocket
     Sec-WebSocket-Version: 13 *)

  //Connection: Upgrade
  //Connection: keep-alive, Upgrade (firefox etc))
  if Pos('upgrade', LowerCase(ARequestInfo.Connection)) <= 0 then
  //if not SameText('Upgrade', ARequestInfo.Connection) then
    inherited DoCommandGet(AContext, ARequestInfo, AResponseInfo)
  else
  begin
    context := AContext as TIdServerWSContext;

    //Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
    sValue := ARequestInfo.RawHeaders.Values['sec-websocket-key'];
    //"The value of this header field MUST be a nonce consisting of a randomly
    // selected 16-byte value that has been base64-encoded"
    if (sValue <> '') then
    begin
      if (Length(TIdDecoderMIME.DecodeString(sValue)) = 16) then
        context.WebSocketKey := sValue
      else
        Abort; //invalid length
    end
    else
      //important: key must exists, otherwise stop!
      Abort;

    (*
     ws-URI = "ws:" "//" host [ ":" port ] path [ "?" query ]
     wss-URI = "wss:" "//" host [ ":" port ] path [ "?" query ]
     2.   The method of the request MUST be GET, and the HTTP version MUST be at least 1.1.
          For example, if the WebSocket URI is "ws://example.com/chat",
          the first line sent should be "GET /chat HTTP/1.1".
     3.   The "Request-URI" part of the request MUST match the /resource
          name/ defined in Section 3 (a relative URI) or be an absolute
          http/https URI that, when parsed, has a /resource name/, /host/,
          and /port/ that match the corresponding ws/wss URI.
    *)
    context.ResourceName := ARequestInfo.Document;
    if ARequestInfo.UnparsedParams <> '' then
      context.ResourceName := context.ResourceName + '?' +
                              ARequestInfo.UnparsedParams;
    //seperate parts
    context.Path         := ARequestInfo.Document;
    context.Query        := ARequestInfo.UnparsedParams;

    //Host: server.example.com
    context.Host   := ARequestInfo.RawHeaders.Values['host'];
    //Origin: http://example.com
    context.Origin := ARequestInfo.RawHeaders.Values['origin'];
    //Cookie: __utma=99as
    context.Cookie := ARequestInfo.RawHeaders.Values['cookie'];

    //Sec-WebSocket-Version: 13
    //"The value of this header field MUST be 13"
    sValue := ARequestInfo.RawHeaders.Values['sec-websocket-version'];
    if (sValue <> '') then
    begin
      context.WebSocketVersion := StrToIntDef(sValue, 0);
      if context.WebSocketVersion < 13 then
        Abort;  //must be at least 13
    end
    else
      Abort; //must exist

    context.WebSocketProtocol   := ARequestInfo.RawHeaders.Values['sec-websocket-protocol'];
    context.WebSocketExtensions := ARequestInfo.RawHeaders.Values['sec-websocket-extensions'];

    //Response
    (* HTTP/1.1 101 Switching Protocols
       Upgrade: websocket
       Connection: Upgrade
       Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo= *)
    AResponseInfo.ResponseNo         := 101;
    AResponseInfo.ResponseText       := 'Switching Protocols';
    AResponseInfo.CloseConnection    := False;
    //Connection: Upgrade
    AResponseInfo.Connection         := 'Upgrade';
    //Upgrade: websocket
    AResponseInfo.CustomHeaders.Values['Upgrade'] := 'websocket';

    //Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
    sValue := Trim(context.WebSocketKey) +                     //... "minus any leading and trailing whitespace"
              '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';          //special GUID
    sValue := TIdEncoderMIME.EncodeBytes(                      //Base64
                         FHash.HashString(sValue) );           //SHA1
    //sValue := string(FHash.CalcString(AnsiString(sValue)));  //SHA1 + Base64
    AResponseInfo.CustomHeaders.Values['Sec-WebSocket-Accept'] := sValue;

    //send same protocol back?
    AResponseInfo.CustomHeaders.Values['Sec-WebSocket-Protocol']   := context.WebSocketProtocol;
    //we do not support extensions yet (gzip deflate compression etc)
    //AResponseInfo.CustomHeaders.Values['Sec-WebSocket-Extensions'] := context.WebSocketExtensions;
    //http://www.lenholgate.com/blog/2011/07/websockets---the-deflate-stream-extension-is-broken-and-badly-designed.html
    //but is could be done using idZlib.pas and DecompressGZipStream etc

    //send response back
    AContext.Connection.IOHandler.InputBuffer.Clear;
    (AContext.Connection.IOHandler as TIdIOHandlerWebsocket).BusyUpgrading := True;
    AResponseInfo.WriteHeader;

    //handle all WS communication in seperate loop
    DoWSExecute(AContext);
  end;
end;

procedure TIndyHTTPWebsocketServer.Loaded;
begin
  inherited;
  if Self.IOHandler = nil then
    Self.IOHandler := TIdServerIOHandlerWebsocket.Create(Self);
end;

{ TIdServerWSContext }

function TIdServerWSContext.IOHandler: TIdIOHandlerWebsocket;
begin
  Result := Self.Connection.IOHandler as TIdIOHandlerWebsocket;
end;

end.
