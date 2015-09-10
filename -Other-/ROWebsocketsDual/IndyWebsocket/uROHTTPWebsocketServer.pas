unit uROHTTPWebsocketServer;

interface

uses
  Classes, IdServerIOHandlerWebsocket, IdIOHandlerWebsocket,
  uROIndyHTTPServer, uROClientIntf, uROServer, uROHTTPDispatch,
  IdContext, IdCustomHTTPServer, IdCustomTCPServer, uROHash, uROServerIntf;

type
  TROIndyHTTPWebsocketServer = class(TROIndyHTTPServer)
  protected
    FHash: TROHash;
    procedure InternalServerCommandGet(AThread: TIdThreadClass;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); override;
    function GetDispatchersClass: TROMessageDispatchersClass; override;

    procedure DoWSExecute(AThread: TIdThreadClass);
  public
    procedure  AfterConstruction; override;
    destructor Destroy; override;
    procedure  Loaded; override;
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

  TROHTTPDispatcher_Websocket = class(TROHTTPDispatcher)
  public
    function CanHandleMessage(const aTransport: IROTransport; aRequeststream : TStream): boolean; override;
  end;

  TROHTTPMessageDispatchers_WebSocket = class(TROHTTPMessageDispatchers)
  protected
    function GetDispatcherClass : TROMessageDispatcherClass; override;
  end;

  TROTransportContext = class(TInterfacedObject,
                       IROTransport, IROTCPTransport,
                       IROActiveEventServer)
  private
    FROServer: TROIndyHTTPServer;
    FIdContext: TIdServerWSContext;
    FEventCount: Integer;
    FClientId: TGUID;
  private
    class var FGlobalEventCount: Integer;
  protected
    {IROTransport}
    function GetTransportObject: TObject;
    {IROTCPTransport}
    function GetClientAddress : string;
    {IROActiveEventServer}
    procedure EventsRegistered(aSender : TObject; aClient: TGUID);
    procedure DispatchEvent(anEventDataItem : TROEventData; aSessionReference : TGUID; aSender: TObject); // asender is TROEventRepository
  public
    //constructor Create(aROServer: TROIndyHTTPServer; aIOHandler: TIdIOHandlerWebsocket);
    constructor Create(aROServer: TROIndyHTTPServer; aIdContext: TIdServerWSContext);

    property ClientId: TGUID read FClientId write FClientId;
  end;


implementation

uses
  SysUtils, IdCoderMIME, Windows, uROEventRepository, uROSessions, uROClient;

procedure TROIndyHTTPWebsocketServer.AfterConstruction;
begin
  inherited;
  FHash := TROHash_SHA1.Create(nil);
  IndyServer.ContextClass := TIdServerWSContext;
end;

destructor TROIndyHTTPWebsocketServer.Destroy;
begin
  FHash.Free;
  inherited;
end;

procedure TROIndyHTTPWebsocketServer.DoWSExecute(AThread: TIdThreadClass);
var
  transport : TROTransportContext;
  strmRequest, strmResponse: TMemoryStream;
  wscode: TWSDataCode;
  wsIO: TIdIOHandlerWebsocket;
  cWSNR: array[0..3] of AnsiChar;
  iMsgNr: Integer;
  msg: TROMessageDispatcher;
  imsg: IROMessage;
begin
  transport := nil;
  try
    while AThread.Connection.Connected do
    begin
      if (AThread.Connection.IOHandler.InputBuffer.Size > 0) or
         AThread.Connection.IOHandler.Readable(5 * 1000) then
      begin
        strmResponse := TMemoryStream.Create;
        strmRequest  := TMemoryStream.Create;
        try
          wsIO := AThread.Connection.IOHandler as TIdIOHandlerWebsocket;

          strmRequest.Position := 0;
          //first is the type: text or bin
          wscode := TWSDataCode(wsIO.ReadLongWord);
          //then the length + data = stream
          wsIO.ReadStream(strmRequest);
          //ignore ping/pong messages
          if wscode in [wdcPing, wdcPong] then Continue;
          if strmRequest.Size < Length('WSNR') + SizeOf(iMsgNr) then Continue;

          //read messagenr from the end
          strmRequest.Position := strmRequest.Size - Length('WSNR') - SizeOf(iMsgNr);
          strmRequest.Read(cWSNR[0], Length(cWSNR));
          if (cWSNR <> 'WSNR') then Continue;
          strmRequest.Read(iMsgNr, SizeOf(iMsgNr));
          strmRequest.Position := 0;
          //trunc extra data
          strmRequest.Size := strmRequest.Size - Length('WSNR') - SizeOf(iMsgNr);

          transport := AThread.Data as TROTransportContext;
          //no RO transport object already made?
          if transport = nil then
          begin
            //create IROTransport object
            transport    := TROTransportContext.Create(Self, AThread as TIdServerWSContext);
            (transport as IROTransport)._AddRef;
            //attach RO transport to indy context
            AThread.Data := transport;

            //read client GUID the first time (needed to be able to send RO events)
            msg  := Self.Dispatchers.FindDispatcher(transport, strmRequest);
            imsg := (msg.MessageIntf as IROMessageCloneable).Clone();
            imsg.InitializeRead(transport);
            imsg.ReadFromStream(strmRequest);
            transport.ClientId := imsg.ClientID;
            imsg := nil;
            Assert(not IsEqualGUID(transport.ClientID, EmptyGUID));
          end;

          //EXECUTE FUNCTION
          Self.DispatchMessage(transport, strmRequest, strmResponse);

          //write number at end
          strmResponse.Position := strmResponse.Size;
          strmResponse.Write(AnsiString('WSNR'), Length('WSNR'));
          strmResponse.Write(iMsgNr, SizeOf(iMsgNr));
          strmResponse.Position := 0;

          //write result back (of the same type: text or bin)
          if wscode = wdcText then
            wsIO.Write(strmResponse, wdtText)
          else
            wsIO.Write(strmResponse, wdtBinary)
        finally
          strmRequest.Free;
          strmResponse.Free;
        end;
      end
      else
      begin
        wsIO := AThread.Connection.IOHandler as TIdIOHandlerWebsocket;
        //ping
        wsIO.WriteData(nil, wdcPing);
      end;

    end;
  finally
    //detach RO transport
    if transport <> nil then
      (transport as IROTransport)._Release;
    AThread.Data := nil;
  end;
end;

function TROIndyHTTPWebsocketServer.GetDispatchersClass: TROMessageDispatchersClass;
begin
  Result := TROHTTPMessageDispatchers_Websocket;
end;

procedure TROIndyHTTPWebsocketServer.InternalServerCommandGet(AThread: TIdThreadClass;
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
  if not SameText('Upgrade', ARequestInfo.Connection) then
    inherited InternalServerCommandGet(AThread, ARequestInfo, AResponseInfo)
  else
  begin
    context := AThread as TIdServerWSContext;

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
    sValue := Trim(context.WebSocketKey) +                   //... "minus any leading and trailing whitespace"
              '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';        //special GUID
    sValue := string(FHash.CalcString(AnsiString(sValue)));  //SHA1 + Base64
    AResponseInfo.CustomHeaders.Values['Sec-WebSocket-Accept'] := sValue;

    //send same protocol back?
    AResponseInfo.CustomHeaders.Values['Sec-WebSocket-Protocol']   := context.WebSocketProtocol;
    //we do not support extensions yet (gzip deflate compression etc)
    //AResponseInfo.CustomHeaders.Values['Sec-WebSocket-Extensions'] := context.WebSocketExtensions;
    //http://www.lenholgate.com/blog/2011/07/websockets---the-deflate-stream-extension-is-broken-and-badly-designed.html
    //but is could be done using idZlib.pas and DecompressGZipStream etc

    //send response back
    AThread.Connection.IOHandler.InputBuffer.Clear;
    (AThread.Connection.IOHandler as TIdIOHandlerWebsocket).BusyUpgrading := True;
    AResponseInfo.WriteHeader;

    //handle all WS communication in seperate loop
    DoWSExecute(AThread);
  end;
end;

procedure TROIndyHTTPWebsocketServer.Loaded;
begin
  inherited;
  if Self.IndyServer.IOHandler = nil then
    IndyServer.IOHandler := TIdServerIOHandlerWebsocket.Create(Self);
end;

{ TROTransport }

constructor TROTransportContext.Create(aROServer: TROIndyHTTPServer;
  aIdContext: TIdServerWSContext);
begin
  FROServer  := aROServer;
  FIdContext := aIdContext;
end;

procedure TROTransportContext.EventsRegistered(aSender: TObject; aClient: TGUID);
begin
  //
end;

procedure TROTransportContext.DispatchEvent(anEventDataItem: TROEventData;
  aSessionReference: TGUID; aSender: TObject);
var
  i: Integer;
  LContext: TIdContext;
  transport: TROTransportContext;
  l: TList;
  ws: TIdIOHandlerWebsocket;
  cWSNR: array[0..3] of AnsiChar;
begin
  l := FROServer.IndyServer.Contexts.LockList;
  try
    if l.Count <= 0 then Exit;

    anEventDataItem.Data.Position := anEventDataItem.Data.Size - Length('WSNR') - SizeOf(FEventCount);
    anEventDataItem.Data.Read(cWSNR[0], Length(cWSNR));
    //event number not written already?
    if cWSNR <> 'WSNR' then
    begin
      //new event nr
      FEventCount   := -1 * InterlockedIncrement(FGlobalEventCount); //negative = event, positive is normal RO message
      //overflow? then start again from 0
      if FEventCount > 0 then
      begin
        InterlockedExchange(FGlobalEventCount, 0);
        FEventCount := -1 * InterlockedIncrement(FGlobalEventCount); //negative = event, positive is normal RO message
      end;
      Assert(FEventCount < 0);
      //write nr at end of message
      anEventDataItem.Data.Position := anEventDataItem.Data.Size;
      anEventDataItem.Data.Write(AnsiString('WSNR'), Length('WSNR'));
      anEventDataItem.Data.Write(FEventCount, SizeOf(FEventCount));
      anEventDataItem.Data.Position := 0;
    end;

    //search specific client
    for i := 0 to l.Count - 1 do
    begin
      LContext  := TIdContext(l.Items[i]);
      transport := LContext.Data as TROTransportContext;
      if transport = nil then Continue;
      if not IsEqualGUID(transport.ClientId, aSessionReference) then Continue;

      //direct write event data
      ws       := (LContext.Connection.IOHandler as TIdIOHandlerWebsocket);
      if not ws.IsWebsocket then Exit;
      ws.Lock;
      try
        ws.Write(anEventDataItem.Data, wdtBinary);
      finally
        ws.Unlock;
      end;
    end;
  finally
    FROServer.IndyServer.Contexts.UnlockList;
  end;
end;

function TROTransportContext.GetClientAddress: string;
begin
  Result := FIdContext.Binding.PeerIP;
end;

function TROTransportContext.GetTransportObject: TObject;
begin
  Result := FROServer;
end;

{ TROHTTPMessageDispatchers_WebSocket }

function TROHTTPMessageDispatchers_WebSocket.GetDispatcherClass: TROMessageDispatcherClass;
begin
  result := TROHTTPDispatcher_Websocket;
end;

{ TROHTTPDispatcher_Websocket }

function TROHTTPDispatcher_Websocket.CanHandleMessage(
  const aTransport: IROTransport; aRequeststream: TStream): boolean;
var
  tcp: IROTCPTransport;
begin
  if aRequeststream = nil then result := FALSE else // for preventing warning in FPC
  result := FALSE;

  if not Enabled or
     not Supports(aTransport, IROTCPTransport, tcp)
  then
    Exit;
  if (tcp as TROTransportContext).FIdContext.IOHandler.IsWebsocket then
  begin
    //we can handle all kind of messages, independent on the path, so check which kind of message we have
    Result := Self.Message.IsValidMessage((aRequeststream as TMemoryStream).Memory, aRequeststream.Size);
  end
  else
    Result := inherited CanHandleMessage(aTransport, aRequeststream);
end;

{ TIdServerWSContext }

function TIdServerWSContext.IOHandler: TIdIOHandlerWebsocket;
begin
  Result := Self.Connection.IOHandler as TIdIOHandlerWebsocket;
end;

end.
