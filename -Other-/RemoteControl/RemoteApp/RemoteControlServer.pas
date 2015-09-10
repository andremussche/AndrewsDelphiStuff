unit RemoteControlServer;

interface

uses
  SysUtils, Classes, IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer,
  IdTCPServer;

type
  TdmRemoteControlServer = class(TDataModule)
    IdTCPServer1: TIdTCPServer;
    procedure DataModuleCreate(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
  private
  public
  end;

var
  dmRemoteControlServer: TdmRemoteControlServer;

implementation

uses Windows, RemoteControlMethods, XMLFile, CommunicationTypes;

var
  _Methods: TRemoteControlMethods;

{$R *.dfm}

procedure TdmRemoteControlServer.DataModuleCreate(Sender: TObject);
var i, iPort: integer;
    sParam: string;
begin
  iPort := 221;
  for i := 1 to ParamCount do
  begin
    sParam := Lowercase( ParamStr(i) );
    if Pos('-remotecontrol=', sParam) = 1 then
      iPort := StrToIntDef(Copy(sParam, Length('-remotecontrol=')+1, Length(sParam)),221);
  end;
  IdTCPServer1.DefaultPort := iPort;
  IdTCPServer1.Active := True;
end;

procedure TdmRemoteControlServer.IdTCPServer1Execute(AContext: TIdContext);
var
  iSize: Integer;
  sXML: string;
  call: TRemoteCall;
begin
  //receive
  iSize := AContext.Connection.IOHandler.ReadLongInt;
  sXML  := AContext.Connection.IOHandler.ReadString(iSize);
  //load
  call := TRemoteCall.Create;
  XMLFile.LoadFromString(sXML, call);

  try
    //execute
    if call.FunctionName = 'ExecuteRemoteFunction' then
      call.Results := _Methods.ExecuteRemoteFunction(call.ObjectList, call.RemoteFunction, call.Arguments, call.Timeout)
    else if call.FunctionName = 'ExecuteRemoteFunction_Async' then
      _Methods.ExecuteRemoteFunction_Async(call.ObjectList, call.RemoteFunction, call.Arguments, call.Timeout)
    else if call.FunctionName = 'PostRemoteMessage' then
      _Methods.PostRemoteMessage(call.ObjectList, call.WndMessage, call.MsgWParam, call.MsgLParam, call.Timeout)
    else if call.FunctionName = 'RemoteControlExists' then
      call.Exists := _Methods.RemoteControlExists(call.ObjectList, call.Timeout);
  except
    on E:Exception do
    begin
      call.ExceptionClass := e.ClassName;
      call.ExceptionMsg   := e.Message;
    end;
  end;

  //save
  sXML := XMLFile.SaveToString(call);
  //send
  AContext.Connection.IOHandler.Write(Length(sXML));
  AContext.Connection.IOHandler.Write(sXML);
end;

initialization
  _Methods := TRemoteControlMethods.Create(nil);
finalization
  _Methods.Free;
end.

