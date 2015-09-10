unit fServerForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  uROClient, uROPoweredByRemObjectsButton, uROClientIntf,
  uROJSONMessage, uROIpTCPServer, uROIndyHTTPServer, uROBaseHTTPServer,
  uROServer, uROBinMessage,
  uROHTTPDispatch, uROHTTPFileDispatcher,
  IdBaseComponent, IdIntercept, IdServerInterceptLogBase,
  IdServerInterceptLogEvent, IdComponent, IdServerIOHandler, IdSSL, IdSSLOpenSSL,
  IdContext, IdCustomHTTPServer, IdGlobal, IdServerIOHandlerStack,
  IdIOHandlerStack,
  uROHTTPWebsocketServer, uROIndyTCPServer, uROSessions, uROEventRepository;

type
  TROIndyHTTPServer = class(TROIndyHTTPWebsocketServer);

  TServerForm = class(TForm)
    RoPoweredByRemObjectsButton1: TRoPoweredByRemObjectsButton;
    ROMessage: TROJSONMessage;
    ROIndyHTTPServer1: TROIndyHTTPServer;
    ROBinMessage1: TROBinMessage;
    ROHTTPFileDispatcher1: TROHTTPFileDispatcher;
    Button2: TButton;
    ROInMemorySessionManager1: TROInMemorySessionManager;
    ROInMemoryEventRepository1: TROInMemoryEventRepository;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ROInMemorySessionManager1CustomCreateSession(
      const aSessionID: TGUID; var Session: TROSession);
  private
  public
  end;

var
  ServerForm: TServerForm;

implementation

uses
  uROSimpleEventRepository, NewLibrary_Intf, NewService_Impl;

{$R *.dfm}

var
  FSimpleEventRepository: TROSimpleWebsocketEventRepository;
var
  ievent: INewEventSink_Writer;

procedure TServerForm.Button2Click(Sender: TObject);
begin
  if FSimpleEventRepository = nil then
  begin
    FSimpleEventRepository := TROSimpleWebsocketEventRepository.Create;
    FSimpleEventRepository.Message  := Self.ROBinMessage1;
    FSimpleEventRepository.ROServer := Self.ROIndyHTTPServer1;
    (FSimpleEventRepository as IInterface)._AddRef;
  end;

  if ievent = nil then
    ievent := FSimpleEventRepository.GetEventWriter(INewEventSink_Writer) as INewEventSink_Writer;
  ievent.NewMethod(ROBinMessage1.ClientID, 'test');
end;

procedure TServerForm.FormCreate(Sender: TObject);
begin
  ROIndyHTTPServer1.Active := True;
end;

procedure TServerForm.ROInMemorySessionManager1CustomCreateSession(
  const aSessionID: TGUID; var Session: TROSession);
begin
  Session := TROSessionContext.Create(aSessionID);
end;

end.
