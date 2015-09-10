unit fServerForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  uROClient, uROPoweredByRemObjectsButton, uROClientIntf, uROServer,
  uROJSONMessage, uROIndyHTTPServer, uROBaseHTTPServer, uROSOAPMessage, uRORemoteService, uROBaseHTTPClient,
  uROIndyHTTPChannel, IdBaseComponent, IdScheduler, IdSchedulerOfThread, IdSchedulerOfThreadPool,
  uROIpHttpServer, uROBPDXTCPServer, uROBPDXHTTPServer;

type
  TServerForm = class(TForm)
    RoPoweredByRemObjectsButton1: TRoPoweredByRemObjectsButton;
    ROMessage: TROJSONMessage;
    ROServer: TROIndyHTTPServer;
    ROSOAPMessage1: TROSOAPMessage;
    ROIndyHTTPChannel1: TROIndyHTTPChannel;
    ROJSONMessage1: TROJSONMessage;
    RORemoteService1: TRORemoteService;
    Button1: TButton;
    IdSchedulerOfThreadPool1: TIdSchedulerOfThreadPool;
    ROBPDXHTTPServer1: TROBPDXHTTPServer;
    ROIpHTTPServer1: TROIpHTTPServer;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ServerForm: TServerForm;

implementation

uses
  NewLibrary_Intf, uROThreadPool;


{$R *.dfm}

procedure TServerForm.Button1Click(Sender: TObject);
var
  s: string;
begin
  s := (RORemoteService1 as NewLibrary_Intf.INewService).HelloWorld;
end;

procedure TServerForm.FormCreate(Sender: TObject);
begin
  ROServer.Active := true;

//  ROBPDXHTTPServer1.Active := True;

  {
  ROIpHTTPServer1.ThreadPool := uROThreadPool.TROThreadPool.Create(nil);
  ROIpHTTPServer1.ThreadPool.MaxThreads  := 100;
  ROIpHTTPServer1.ThreadPool.PoolThreads := 50;
  ROIpHTTPServer1.ThreadPool.MaxQueue    := 50;
  ROIpHTTPServer1.Active := True;
  }
end;

end.
