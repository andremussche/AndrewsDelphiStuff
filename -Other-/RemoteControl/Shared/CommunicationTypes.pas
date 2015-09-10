unit CommunicationTypes;

interface

uses
  Generics.Collections, Classes,
  Rtti,
  BaseObject;

type
  {$METHODINFO ON}
  TObjectStruct = class(TBaseObject)
  private
    fObjectClass: String;
    fObjectName: String;
  published
    property ObjectName :String read fObjectName  write fObjectName;
    property ObjectClass:String read fObjectClass write fObjectClass;
  end;

  TObjectArray      = array of TObjectStruct;
  TObjectStructList = class(TBaseObject)
  private
    FObjects: TObjectArray;
  public
    procedure Add(aObject: TObjectStruct);
    function  Count: Integer;
  published
    property Objects: TObjectArray read FObjects write FObjects;
  end;

  TStringValue = class(TBaseObject)
  private
    FValue: string;
    FVarType: TVarType;
  published
    property Value  : string read FValue write FValue;
    property VarType: TVarType read FVarType write FVarType;
  end;

  //TVariantArray = array of Variant;     not supported
  //TVariantArray = array of string;      gives errors?
  TVariantArray = array of TStringValue;
  TVariantList  = class(TBaseObject)
  private
    FVariants: TVariantArray;
  public
    procedure Add(aValue: Variant);
    function  Count: Integer;
  published
    property Variants: TVariantArray read FVariants write FVariants;
  end;

  TRemoteCall = class(TObjectStruct)
  private
    FArguments: TVariantList;
    FObjectList: TObjectStructList;
    FFunctionName: string;
    FResults: TVariantList;
    FRemoteFunction: string;
    FMsgWParam: Integer;
    FWndMessage: Integer;
    FMsgLParam: Integer;
    FExists: boolean;
    FTimeout: Integer;
    FExceptionClass: string;
    FExceptionMsg: string;
  published
    property ExceptionClass: string        read FExceptionClass write FExceptionClass;
    property ExceptionMsg  : string        read FExceptionMsg   write FExceptionMsg;

    property FunctionName: string          read FFunctionName   write FFunctionName;
    property RemoteFunction: string        read FRemoteFunction write FRemoteFunction;
    property ObjectList: TObjectStructList read FObjectList     write FObjectList;
    property Arguments : TVariantList      read FArguments      write FArguments;
    property Results   : TVariantList      read FResults        write FResults;

    property Timeout: Integer read FTimeout write FTimeout;

    property WndMessage: Integer read FWndMessage write FWndMessage;
    property MsgWParam : Integer read FMsgWParam  write FMsgWParam;
    property MsgLParam : Integer read FMsgLParam  write FMsgLParam;

    property Exists: boolean read FExists write FExists;
  end;

implementation

uses
  Variants, SysUtils;

{ TObjectStructList }

procedure TObjectStructList.Add(aObject: TObjectStruct);
begin
  SetLength(FObjects, Length(FObjects)+1 );
  FObjects[Length(FObjects)-1] := aObject;
end;

function TObjectStructList.Count: Integer;
begin
  Result := Length(FObjects);
end;

{ TVariantList }

procedure TVariantList.Add(aValue: Variant);
var
  v: TStringValue;
begin
  SetLength(FVariants, Length(FVariants)+1 );
  v := TStringValue.Create;
  v.Value   := aValue;
  v.VarType := VarType(aValue);
  FVariants[Length(FVariants)-1] := v;
end;

function TVariantList.Count: Integer;
begin
  Result := Length(FVariants);
end;

end.
