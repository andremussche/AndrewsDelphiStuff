unit mcRemoteControlExecuter;

interface

uses
  Classes, Rtti,
  Generics.Collections,
  //AsyncCalls,
  mcRemoteControlTypes;

type
  TRemoteControlExecutor = class
  public
    class function  Exists(const aRemoteObject: TBaseRemoteObject; const aTimeout: integer = 0): Boolean;
    class function  Execute(const aRemoteObject: TBaseRemoteObject; const aRemoteFunction: string; const aArgs: array of TValue; const aTimeout: integer = 0): TValue;
    class procedure Execute_Async(const aRemoteObject: TBaseRemoteObject; const aRemoteFunction: string; const aArgs: array of TValue);
    class procedure PostMessage(const aRemoteObject: TBaseRemoteObject; aMessage, aWParam, aLParam: Integer);
    //class function  ExecuteIAsync(const aRemoteObject: TBaseRemoteObject; const aRemoteFunction: string; const aArgs: array of TValue): IAsyncCall;overload;
  end;

implementation

uses CommunicationTypes, RemoteConnector, XMLFile, Variants;

function RemoteObject2Array(aRemoteObject: TBaseRemoteObject): TObjectStructList;
var
  //sFullObjectPath: string;
  parent: TBaseRemoteObject;
  remoteobject : TObjectStruct;
begin
  Result := TObjectStructList.Create;

  remoteobject             := TObjectStruct.Create;
  Result.Add(remoteobject);
  remoteobject.ObjectName  := aRemoteObject.Name;
  remoteobject.ObjectClass := aRemoteObject.ClassName;

  //sFullObjectPath := aRemoteObject.Name + '.' + aRemoteFunction;
  parent          := aRemoteObject.Owner as TBaseRemoteObject;
  while parent <> nil do
  begin
    remoteobject             := TObjectStruct.Create;
    Result.Add(remoteobject);
    remoteobject.ObjectName  := parent.Name;
    remoteobject.ObjectClass := parent.ClassName;

    //sFullObjectPath := parent.Name + '.' + sFullObjectPath;
    parent          := parent.Owner as TBaseRemoteObject;
  end;
end;

function ValueArray2VariantArray(const aArray: array of TValue): TVariantList;
var i: Integer;
begin
  Result := TVariantList.Create;
  for i := 0 to High(aArray) do
    Result.Add(aArray[i].AsVariant);
end;

{ TRemoteControlExecutor }

class function TRemoteControlExecutor.Execute(
  const aRemoteObject: TBaseRemoteObject; const aRemoteFunction: string;
  const aArgs: array of TValue; const aTimeout: integer = 0): TValue;
var
  remoteobjects: TObjectStructList;
  arguments: TVariantList;
  results: TVariantList;
  //sXML: string;
begin
  remoteobjects := RemoteObject2Array(aRemoteObject);
  arguments     := ValueArray2VariantArray(aArgs);

  results := dmRemoteConnector.ExecuteRemoteFunction(remoteobjects, aRemoteFunction, arguments, aTimeout);

  if (results <> nil) and (results.Count > 0) then
  begin
    Result := TValue.FromVariant(
                  VarAsType(results.Variants[0].Value,
                            results.Variants[0].VarType) );
    //Result := TValue.FromVariant(results.Variants[0].Value)
  end
  else
    Result := TValue.Empty;
end;

class procedure TRemoteControlExecutor.Execute_Async(
  const aRemoteObject: TBaseRemoteObject; const aRemoteFunction: string;
  const aArgs: array of TValue);
var
  remoteobjects: TObjectStructList;
  arguments: TVariantList;
begin
  remoteobjects := RemoteObject2Array(aRemoteObject);
  arguments     := ValueArray2VariantArray(aArgs);

  dmRemoteConnector.ExecuteRemoteFunction_Async(remoteobjects,
                                                aRemoteFunction,
                                                arguments);
end;

{
class function TRemoteControlExecutor.ExecuteIAsync(
  const aRemoteObject: TBaseRemoteObject; const aRemoteFunction: string;
  const aArgs: array of TValue): IAsyncCall;
var
  remoteobjects: TObjectStructList;
  arguments: TVariantList;
begin
  remoteobjects := RemoteObject2Array(aRemoteObject);
  arguments     := ValueArray2VariantArray(aArgs);

  if AsyncCalls.GetMaxAsyncCallThreads < 5 then
    AsyncCalls.SetMaxAsyncCallThreads(5);

  Result := TAsyncCalls.Invoke(
    function: Integer
    begin
      Result := 0;
      dmRemoteConnector.RemoteControlMethodsClient.ExecuteRemoteFunction(remoteobjects,
                                                   aRemoteFunction,
                                                   arguments);
    end);
end;
}

class function TRemoteControlExecutor.Exists(
  const aRemoteObject: TBaseRemoteObject; const aTimeout: integer = 0): Boolean;
var
  remoteobjects: TObjectStructList;
begin
  remoteobjects := RemoteObject2Array(aRemoteObject);
  Result        := dmRemoteConnector.RemoteControlExists(remoteobjects, aTimeout);
end;

class procedure TRemoteControlExecutor.PostMessage(
  const aRemoteObject: TBaseRemoteObject; aMessage, aWParam, aLParam: Integer);
var
  remoteobjects: TObjectStructList;
begin
  remoteobjects := RemoteObject2Array(aRemoteObject);
  dmRemoteConnector.PostRemoteMessage(remoteobjects,
                                      aMessage, aWParam, aLParam);
end;

end.
