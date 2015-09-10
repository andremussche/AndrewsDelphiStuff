unit mfMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  uHTTPWebsocketServer,
  IdContext, IdCustomHTTPServer, IdIOHandlerWebsocket;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FHttpWsServer: TIndyHTTPWebsocketServer;
  protected
    procedure HTTPCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure WSMessageRecieved(const aDataCode: TWSDataCode; const aRequest: TMemoryStream; out aResponse: TStream);
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FHttpWsServer := TIndyHTTPWebsocketServer.Create(Self);
  FHttpWsServer.DefaultPort  := 80;
  FHttpWsServer.Active       := True;

  FHttpWsServer.OnCommandGet := HTTPCommandGet;
  FHttpWsServer.OnWSMessage  := WSMessageRecieved;
end;

procedure TfrmMain.HTTPCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  ms: TMemoryStream;
  sfile: string;
begin
  ms    := TMemoryStream.Create;
  sfile := ExtractFileDir(Application.ExeName) +
           StringReplace(ARequestInfo.Document, '/', '\', [rfReplaceAll]);
  if FileExists(sfile) then
    ms.LoadFromFile(sfile);
  AResponseInfo.ContentStream := ms;
end;

procedure TfrmMain.WSMessageRecieved(const aDataCode: TWSDataCode; const aRequest: TMemoryStream;
  out aResponse: TStream);
begin
  aResponse := TMemoryStream.Create;
  if aDataCode = wdcText then
  begin
    with TStreamWriter.Create(aResponse) do
    try
      with TStreamReader.Create(aRequest) do
      try
        WriteLine('Text data received: ' + ReadToEnd);
      finally
        Free
      end;
    finally
      Free;
    end;
  end
  else
    //binary data? then just return same data
    aResponse.CopyFrom(aRequest, aRequest.Size);
end;

end.
