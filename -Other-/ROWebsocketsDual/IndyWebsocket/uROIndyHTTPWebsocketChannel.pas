unit uROIndyHTTPWebsocketChannel;

interface

uses
  Classes, SyncObjs,
  uROIndyHTTPChannel, uROClientIntf,
  IdHTTPWebsocketClient, IdHTTP, IdWinsock2;

type
  TROIndyHTTPWebsocketChannel = class(TROIndyHTTPChannel,
                                       IROActiveEventChannel)
  private
    function  GetHost: string;
    function  GetPort: integer;
    procedure SetHost(const Value: string);
    procedure SetPort(const Value: integer);
    function  GetIndyClient: TIdHTTPWebsocketClient;
    procedure SetWSResourceName(const Value: string);
    function  GetWSResourceName: string;
  protected
    FTriedUpgrade: Boolean;
    FEventReceivers: TInterfaceList;
    FMessageNr: Integer;
    procedure IntDispatchEvent(aEvent: TStream);
    procedure SocketConnected(Sender: TObject);
    procedure ResetChannel;

    function  TryUpgradeToWebsocket: Boolean;
  protected
    procedure IntDispatch(aRequest, aResponse: TStream); override;
    function  CreateIndyClient: TIdHTTP; override;
  protected
    {IROActiveEventChannel}
    procedure RegisterEventReceiver  (aReceiver: IROEventReceiver);
    procedure UnregisterEventReceiver(aReceiver: IROEventReceiver);
  public
    procedure  AfterConstruction;override;
    destructor Destroy; override;
  published
    property  IndyClient: TIdHTTPWebsocketClient read GetIndyClient;
    property  Port: integer read GetPort write SetPort;
    property  Host: string  read GetHost write SetHost;
    property  WSResourceName: string read GetWSResourceName write SetWSResourceName;
  end;

  TROIndyWSMultiChannelReadThread = class(TThread)
  private
    class var FInstance: TROIndyWSMultiChannelReadThread;
  protected
    FTempHandle: THandle;
    FPendingBreak: Boolean;
    Freadset, Fexceptionset: TFDSet;
    Finterval: TTimeVal;
    procedure InitSpecialEventSocket;
    procedure ResetSpecialEventSocket;
    procedure BreakSelectWait;
  protected
    FChannels: TThreadList;
    procedure ReadFromAllChannels;

    procedure Execute; override;
  public
    procedure  AfterConstruction;override;
    destructor Destroy; override;

    procedure Terminate;

    procedure AddChannel(aChannel: TROIndyHTTPWebsocketChannel);
    procedure RemoveChannel(aChannel: TROIndyHTTPWebsocketChannel);

    class function Instance: TROIndyWSMultiChannelReadThread;
  end;

implementation

uses
  SysUtils, Windows,
  IdStack, IdStackConsts, IdGlobal, IdStackBSDBase,
  uRORes, uROIndySupport, IdIOHandlerWebsocket;

{ TROIndyHTTPChannel_Websocket }

procedure TROIndyHTTPWebsocketChannel.AfterConstruction;
begin
  inherited;
  FEventReceivers := TInterfaceList.Create;
  //not needed, is ignored at server now, but who knows later? :) e.g. support multiple sub protocols
  WSResourceName  := 'RemObjects';
end;

destructor TROIndyHTTPWebsocketChannel.Destroy;
begin
  TROIndyWSMultiChannelReadThread.Instance.RemoveChannel(Self);

  FEventReceivers.Free;
  inherited;
end;

function TROIndyHTTPWebsocketChannel.GetIndyClient: TIdHTTPWebsocketClient;
begin
  Result := inherited IndyClient as TIdHTTPWebsocketClient;
end;

procedure TROIndyHTTPWebsocketChannel.SetHost(const Value: string);
begin
  IndyClient.Host := Value;
  TargetURL := Format('ws://%s:%d/%s', [Host, Port, WSResourceName]);
end;

procedure TROIndyHTTPWebsocketChannel.SetPort(const Value: integer);
begin
  IndyClient.Port := Value;
  TargetURL := Format('ws://%s:%d/%s', [Host, Port, WSResourceName]);
end;

procedure TROIndyHTTPWebsocketChannel.SetWSResourceName(const Value: string);
begin
  IndyClient.WSResourceName := Value;
  TargetURL := Format('ws://%s:%d/%s', [Host, Port, WSResourceName]);
end;

function TROIndyHTTPWebsocketChannel.GetHost: string;
begin
  Result := IndyClient.Host;
end;

function TROIndyHTTPWebsocketChannel.GetPort: integer;
begin
  Result := IndyClient.Port;
end;

function TROIndyHTTPWebsocketChannel.GetWSResourceName: string;
begin
  Result := IndyClient.WSResourceName;
end;

function TROIndyHTTPWebsocketChannel.CreateIndyClient: TIdHTTP;
var
  wsclient: TIdHTTPWebsocketClient;
begin
  //Result := inherited CreateIndyClient;
  wsclient := TIdHTTPWebsocketClient.Create(Self);
  wsclient.Request.UserAgent := uRORes.str_ProductName;
  wsclient.Port := 80;
  wsclient.Host := '127.0.0.1';
  wsclient.OnConnected := SocketConnected;
  TargetURL := '';

  Result := wsclient;
end;

procedure TROIndyHTTPWebsocketChannel.SocketConnected(Sender: TObject);
begin
  if DisableNagle then
    uROIndySupport.Indy_DisableNagle(IndyClient);
end;

function TROIndyHTTPWebsocketChannel.TryUpgradeToWebsocket: Boolean;
begin
  try
    Result := (IndyClient as TIdHTTPWebsocketClient).TryUpgradeToWebsocket;
    if Result then
    begin
      Self.IndyClient.IOHandler.InputBuffer.Clear;
      //background wait for data in single thread
      TROIndyWSMultiChannelReadThread.Instance.AddChannel(Self);
    end;
  except
    ResetChannel;
    raise;
  end;
end;

procedure TROIndyHTTPWebsocketChannel.IntDispatch(aRequest, aResponse: TStream);
var
  cWSNR: array[0..3] of AnsiChar;
  iMsgNr, iMsgNr2: Integer;
  ws: TIdIOHandlerWebsocket;
begin
  //http server supports websockets?
  if not FTriedUpgrade then
  begin
    TryUpgradeToWebsocket;
    FTriedUpgrade := True; //one shot
  end;

  ws := IndyClient.IOHandler as TIdIOHandlerWebsocket;
  if not ws.IsWebsocket then
    //normal http dispatch
    inherited IntDispatch(aRequest, aResponse)
  else
  //websocket dispatch
  begin
    ws.Lock;
    try
      //write messagenr at end
      aRequest.Position := aRequest.Size;
      Inc(FMessageNr);
      iMsgNr := FMessageNr;
      aRequest.Write(AnsiString('WSNR'), Length('WSNR'));
      aRequest.Write(iMsgNr, SizeOf(iMsgNr));
      aRequest.Position := 0;

      //write
      IndyClient.IOHandler.Write(aRequest);

      iMsgNr2 := 0;
      while iMsgNr2 <= 0 do
      begin
        //first is the data type TWSDataType(text or bin), but is ignore/not needed
        IndyClient.IOHandler.ReadLongInt;
        //next the size + data = stream
        IndyClient.IOHandler.ReadStream(aResponse);

        //get event or message nr
        aResponse.Position   := aResponse.Size - Length('WSNR') - SizeOf(iMsgNr2);
        aResponse.Read(cWSNR[0], Length(cWSNR));
        Assert(cWSNR = 'WSNR');
        aResponse.Read(iMsgNr2, SizeOf(iMsgNr2));
        aResponse.Size       := aResponse.Size - Length('WSNR') - SizeOf(iMsgNr2); //trunc
        aResponse.Position   := 0;

        //event?
        if iMsgNr2 < 0 then
        begin
          ws.Unlock;
          try
            IntDispatchEvent(aResponse);
            aResponse.Size := 0;
          finally
            ws.Lock;
          end;
        end;
      end;
    except
      ws.Unlock; //always unlock
      ResetChannel;
      Raise;
    end;
    ws.Unlock;  //normal unlock (no extra try finally needed)

    if iMsgNr2 <> iMsgNr then
      Assert(iMsgNr2 = iMsgNr, 'Message number mismatch between send and received!');
  end;
end;

procedure TROIndyHTTPWebsocketChannel.IntDispatchEvent(aEvent: TStream);
var
  i: Integer;
  eventrecv: IROEventReceiver;
begin
  for i := 0 to FEventReceivers.Count - 1 do
  begin
    aEvent.Position := 0;
    eventrecv := FEventReceivers.Items[i] as IROEventReceiver;
    try
      eventrecv.Dispatch(aEvent, TThread.CurrentThread);
    except
      //ignore errors within events, so normal communication is preserved
    end;
  end;
end;

procedure TROIndyHTTPWebsocketChannel.RegisterEventReceiver(
  aReceiver: IROEventReceiver);
begin
  FEventReceivers.Add(aReceiver);
end;

procedure TROIndyHTTPWebsocketChannel.ResetChannel;
var
  ws: TIdIOHandlerWebsocket;
begin
  FTriedUpgrade := False; //reset
  TROIndyWSMultiChannelReadThread.Instance.RemoveChannel(Self);

  IndyClient.IOHandler.InputBuffer.Clear;
  //close/disconnect internal socket
  ws := IndyClient.IOHandler as TIdIOHandlerWebsocket;
  ws.Close;
  IndyClient.Disconnect(False);
end;

procedure TROIndyHTTPWebsocketChannel.UnregisterEventReceiver(
  aReceiver: IROEventReceiver);
begin
  FEventReceivers.Remove(aReceiver);
end;

{ TMultiChannelReadThread }

procedure TROIndyWSMultiChannelReadThread.AfterConstruction;
begin
  inherited;
  FChannels := TThreadList.Create;
  FillChar(Freadset, SizeOf(Freadset), 0);
  FillChar(Fexceptionset, SizeOf(Fexceptionset), 0);

  InitSpecialEventSocket;
end;

destructor TROIndyWSMultiChannelReadThread.Destroy;
begin
  IdWinsock2.closesocket(FTempHandle);
  FChannels.Free;
  inherited;
end;

procedure TROIndyWSMultiChannelReadThread.Execute;
begin
  Self.NameThreadForDebugging(AnsiString(Self.ClassName));

  while not Terminated do
  begin
    try
      while not Terminated do
        ReadFromAllChannels;
    except
      //continue
    end;
  end;
end;

procedure TROIndyWSMultiChannelReadThread.ReadFromAllChannels;
var
  l: TList;
  chn: TROIndyHTTPWebsocketChannel;
  iCount,
  i, iEventNr: Integer;
  cWSNR: array[0..3] of AnsiChar;
  iResult: NativeInt;
  strmEvent: TMemoryStream;
  ws: TIdIOHandlerWebsocket;
  wscode: TWSDataCode;
begin
  l := FChannels.LockList;
  try
    iCount := 0;
    Freadset.fd_count := iCount;

    for i := 0 to l.Count - 1 do
    begin
      chn := TROIndyHTTPWebsocketChannel(l.Items[i]);
      //valid?
      if //not chn.Busy and    also take busy channels (will be ignored later), otherwise we have to break/reset for each RO function execution
         (chn.IndyClient.Socket.Binding.Handle > 0) and
         (chn.IndyClient.Socket.Binding.Handle <> INVALID_SOCKET) then
      begin
        Freadset.fd_count         := iCount+1;
        Freadset.fd_array[iCount] := chn.IndyClient.Socket.Binding.Handle;
        Inc(iCount);
      end;
    end;

    if FPendingBreak then
      ResetSpecialEventSocket;
  finally
    FChannels.UnlockList;
  end;

  //special helper socket to be able to stop "select" from waiting
  Fexceptionset.fd_count    := 1;
  Fexceptionset.fd_array[0] := FTempHandle;

  //wait 15s till some data
  Finterval.tv_sec  := 15; //15s
  Finterval.tv_usec := 0;

  //nothing to wait for? then sleep some time to prevent 100% CPU
  if iCount = 0 then
  begin
    iResult := IdWinsock2.select(0, nil, nil, @Fexceptionset, @Finterval);
    if iResult = SOCKET_ERROR then
      iResult := 1;  //ignore errors
  end
  //wait till a socket has some data (or a signal via exceptionset is fired)
  else
    iResult := IdWinsock2.select(0, @Freadset, nil, @Fexceptionset, @Finterval);

  if iResult = SOCKET_ERROR then
    //raise EIdWinsockStubError.Build(WSAGetLastError, '', []);
    //ignore error during wait: socket disconnected etc
    Exit;

  //some data?
  if (iResult > 0) then
  begin
    strmEvent := nil;

    l := FChannels.LockList;
    try
      //check for data for all channels
      for i := 0 to l.Count - 1 do
      begin
        chn := TROIndyHTTPWebsocketChannel(l.Items[i]);
        try
          //try to process all events
          while not chn.Busy and  //no client call pending
                chn.IndyClient.IOHandler.Readable(0) do //has some data
          begin
            ws  := chn.IndyClient.IOHandler as TIdIOHandlerWebsocket;
            //no pending dispatch active? (so actually we only read events here?)
            if ws.TryLock then
            begin
              try
                if strmEvent = nil then
                  strmEvent := TMemoryStream.Create;
                strmEvent.Clear;

                //first is the data type TWSDataType(text or bin), but is ignore/not needed
                wscode := TWSDataCode(chn.IndyClient.IOHandler.ReadLongWord);
                //next the size + data = stream
                chn.IndyClient.IOHandler.ReadStream(strmEvent);

                //ignore ping/pong messages
                if wscode in [wdcPing, wdcPong] then Continue;
                if strmEvent.Size < Length('WSNR') + SizeOf(iEventNr) then Continue;

                //get event nr
                strmEvent.Position := strmEvent.Size - Length('WSNR') - SizeOf(iEventNr);
                strmEvent.Read(cWSNR[0], Length(cWSNR));
                Assert(cWSNR = 'WSNR');
                strmEvent.Read(iEventNr, SizeOf(iEventNr));
                Assert(iEventNr < 0);
                //trunc
                strmEvent.Size := strmEvent.Size - Length('WSNR') - SizeOf(iEventNr);
              finally
                ws.Unlock;
              end;
              //fire event
              chn.IntDispatchEvent(strmEvent);
            end;
          end;
        except
          chn.ResetChannel;
          raise;
        end;
      end;

      if FPendingBreak then
        ResetSpecialEventSocket;
    finally
      FChannels.UnlockList;
      strmEvent.Free;
    end;
  end;
end;

procedure TROIndyWSMultiChannelReadThread.RemoveChannel(
  aChannel: TROIndyHTTPWebsocketChannel);
begin
  FChannels.Remove(aChannel);
end;

procedure TROIndyWSMultiChannelReadThread.AddChannel(
  aChannel: TROIndyHTTPWebsocketChannel);
var l: TList;
begin
  Assert( (aChannel.IndyClient.IOHandler as TIdIOHandlerWebsocket).IsWebsocket, 'Channel is not a websocket');

  l := FChannels.LockList;
  try
    Assert(l.Count < 64, 'Max 64 connections can be handled by one read thread!');  //due to restrictions of the "select" API
    l.Add(aChannel);

    //trigger the "select" wait
    BreakSelectWait;
  finally
    FChannels.UnlockList;
  end;
end;

class function TROIndyWSMultiChannelReadThread.Instance: TROIndyWSMultiChannelReadThread;
begin
  if FInstance = nil then
  begin
    FInstance := TROIndyWSMultiChannelReadThread.Create(True);
    FInstance.Start;
  end;
  Result := FInstance;
end;

procedure TROIndyWSMultiChannelReadThread.InitSpecialEventSocket;
var
  param: Cardinal;
  iResult: Integer;
begin
  //alloc socket
  FTempHandle := GStack.NewSocketHandle(Id_SOCK_STREAM, Id_IPPROTO_IP, Id_IPv4, False);
  Assert(FTempHandle <> Id_INVALID_SOCKET);
  //non block mode
  param   := 1; // enable NON blocking mode
  iResult := ioctlsocket(FTempHandle, FIONBIO, param);
  GStack.CheckForSocketError(iResult);
end;

procedure TROIndyWSMultiChannelReadThread.BreakSelectWait;
var
  iResult: Integer;
  LAddr: TSockAddrIn6;
begin
  FillChar(LAddr, SizeOf(LAddr), 0);
  //Id_IPv4
  with PSOCKADDR(@LAddr)^ do
  begin
    sin_family := Id_PF_INET4;
    //dummy address and port
    (GStack as TIdStackBSDBase).TranslateStringToTInAddr('0.0.0.0', sin_addr, Id_IPv4);
    sin_port := htons(1);
  end;

  FPendingBreak := True;

  //connect to non-existing address to stop "select" from waiting
  //Note: this is some kind of "hack" because there is no nice way to stop it
  //The only(?) other possibility is to make a "socket pair" and send a byte to it,
  //but this requires a dynamic server socket (which can trigger a firewall
  //exception/question popup in WindowsXP+)
  iResult := IdWinsock2.connect(FTempHandle, PSOCKADDR(@LAddr), SIZE_TSOCKADDRIN);
  //non blocking socket, so will always result in "would block"!
  if (iResult <> Id_SOCKET_ERROR) or
     (GStack.WSGetLastError <> WSAEWOULDBLOCK)
  then
    GStack.CheckForSocketError(iResult);
end;

procedure TROIndyWSMultiChannelReadThread.ResetSpecialEventSocket;
begin
  Assert(FPendingBreak);
  FPendingBreak := False;

  IdWinsock2.closesocket(FTempHandle);
  InitSpecialEventSocket;
end;

procedure TROIndyWSMultiChannelReadThread.Terminate;
begin
  inherited Terminate;

  FChannels.LockList;
  try
    //fire a signal, so the "select" wait will quit and thread can stop
    BreakSelectWait;
  finally
    FChannels.UnlockList;
  end;
end;

end.
