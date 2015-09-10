unit RemoteConnector;

interface

uses
  Forms, SysUtils, Classes, WideStrings,
  CommunicationTypes, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  TdmRemoteConnector = class(TDataModule)
    IdTCPClient1: TIdTCPClient;
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    function  ExecuteRemoteFunction(aRemoteObjects: TObjectStructList; const aRemoteFunction: string; aArgs: TVariantList; const aTimeout: integer = 0): TVariantList;
    procedure ExecuteRemoteFunction_Async(const aRemoteObjectTree: TObjectStructList; const aRemoteFunction: UnicodeString; const aArguments: TVariantList);
    procedure PostRemoteMessage(const aRemoteObjectTree: TObjectStructList; const aMessage: Integer; const aWParam: Integer; const aLParam: Integer);
    function  RemoteControlExists(const aRemoteObjectTree: TObjectStructList; const aTimeout: integer = 0): Boolean;
  end;

  ERemoteException = class(Exception);

var
  dmRemoteConnector: TdmRemoteConnector;

implementation

uses
  XMLFile;

{$R *.dfm}

constructor TdmRemoteConnector.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TdmRemoteConnector.Destroy;
begin
  inherited;
end;

function TdmRemoteConnector.ExecuteRemoteFunction(
  aRemoteObjects: TObjectStructList; const aRemoteFunction: string;
  aArgs: TVariantList; const aTimeout: integer = 0): TVariantList;
var
  call: TRemoteCall;
  iSize: Integer;
  sXML: string;
begin
  call := TRemoteCall.Create;
  try
    call.FunctionName   := 'ExecuteRemoteFunction';
    call.RemoteFunction := aRemoteFunction;
    call.ObjectList     := aRemoteObjects;
    call.Arguments      := aArgs;
    call.Timeout        := aTimeout;

    sXML := XMLFile.SaveToString(call);
    //send
    IdTCPClient1.IOHandler.Write(Length(sXML));
    IdTCPClient1.IOHandler.Write(sXML);
    //receive
    iSize := IdTCPClient1.IOHandler.ReadLongInt;
    sXML  := IdTCPClient1.IOHandler.ReadString(iSize);

    XMLFile.LoadFromString(sXML, call);
    if call.ExceptionMsg <> '' then
      raise ERemoteException.CreateFmt('Remote exception occured:'#13 + '%s: %s',
                                       [call.ExceptionClass, call.ExceptionMsg]);

    Result := TVariantList.Create;
    XMLFile.LoadFromString(XMLFile.SaveToString(call.Results), Result );
  finally
    call.Free;
  end;
end;

procedure TdmRemoteConnector.ExecuteRemoteFunction_Async(
  const aRemoteObjectTree: TObjectStructList;
  const aRemoteFunction: UnicodeString; const aArguments: TVariantList);
var
  call: TRemoteCall;
  iSize: Integer;
  sXML: string;
begin
  call := TRemoteCall.Create;
  try
    call.FunctionName   := 'ExecuteRemoteFunction_Async';
    call.RemoteFunction := aRemoteFunction;
    call.ObjectList     := aRemoteObjectTree;
    call.Arguments      := aArguments;

    sXML := XMLFile.SaveToString(call);
    //send
    IdTCPClient1.IOHandler.Write(Length(sXML));
    IdTCPClient1.IOHandler.Write(sXML);
    //receive
    iSize := IdTCPClient1.IOHandler.ReadLongInt;
    sXML  := IdTCPClient1.IOHandler.ReadString(iSize);

    XMLFile.LoadFromString(sXML, call);
    if call.ExceptionMsg <> '' then
      raise ERemoteException.CreateFmt('Remote exception occured:'#13 + '%s: %s',
                                       [call.ExceptionClass, call.ExceptionMsg]);
  finally
    call.Free;
  end;
end;

procedure TdmRemoteConnector.PostRemoteMessage(
  const aRemoteObjectTree: TObjectStructList; const aMessage, aWParam,
  aLParam: Integer);
var
  call: TRemoteCall;
  iSize: Integer;
  sXML: string;
begin
  call := TRemoteCall.Create;
  try
    call.FunctionName   := 'PostRemoteMessage';
    //call.RemoteFunction := aRemoteFunction;
    call.ObjectList     := aRemoteObjectTree;
    //call.Arguments      := aArguments;
    call.WndMessage     := aMessage;
    call.MsgWParam      := aWParam;
    call.MsgLParam      := aLParam;

    sXML := XMLFile.SaveToString(call);
    //send
    IdTCPClient1.IOHandler.Write(Length(sXML));
    IdTCPClient1.IOHandler.Write(sXML);
    //receive
    iSize := IdTCPClient1.IOHandler.ReadLongInt;
    sXML  := IdTCPClient1.IOHandler.ReadString(iSize);

    XMLFile.LoadFromString(sXML, call);
    if call.ExceptionMsg <> '' then
      raise ERemoteException.CreateFmt('Remote exception occured:'#13 + '%s: %s',
                                       [call.ExceptionClass, call.ExceptionMsg]);
  finally
    call.Free;
  end;
end;

function TdmRemoteConnector.RemoteControlExists(
  const aRemoteObjectTree: TObjectStructList; const aTimeout: integer = 0): Boolean;
var
  call: TRemoteCall;
  iSize: Integer;
  sXML: string;
begin
  call := TRemoteCall.Create;
  try
    call.FunctionName   := 'RemoteControlExists';
    //call.RemoteFunction := aRemoteFunction;
    call.ObjectList     := aRemoteObjectTree;
    //call.Arguments      := aArguments;
    call.Timeout        := aTimeout;

    sXML := XMLFile.SaveToString(call);
    //send
    IdTCPClient1.IOHandler.Write(Length(sXML));
    IdTCPClient1.IOHandler.Write(sXML);
    //receive
    iSize := IdTCPClient1.IOHandler.ReadLongInt;
    sXML  := IdTCPClient1.IOHandler.ReadString(iSize);

    XMLFile.LoadFromString(sXML, call);
    if call.ExceptionMsg <> '' then
      raise ERemoteException.CreateFmt('Remote exception occured:'#13 + '%s: %s',
                                       [call.ExceptionClass, call.ExceptionMsg]);

    Result := call.Exists;
  finally
    call.Free;
  end;
end;

initialization
  dmRemoteConnector := TdmRemoteConnector.Create(Application);
//finalization
//  FreeAndNil(dmRemoteConnector);
end.
