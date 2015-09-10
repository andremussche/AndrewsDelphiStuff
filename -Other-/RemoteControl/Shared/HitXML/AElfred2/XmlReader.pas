// Filename : XmlReader.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit XmlReader;

// "!!"
// "(*..*)"

interface

{$I SAX.INC}

uses
{$IFDEF DELPHI6_UP}
  Variants,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, Contnrs, SAX, SAXDriver, EventFilter, EventConsumer,
  ValidationConsumer;

type

  TXmlReader = class(TComponent, IUnknown, IXMLReader)
  private
    aelfred2Impl : TSAXDriver;
    aelfred2 : IXMLReader;
    filterImpl : TEventFilter;
    filter : IEventConsumer;
    isValidating : Boolean;
    active : Boolean;
  protected  
    refCount : Integer;
    ownerIsComponent : Boolean;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function getContentHandler() : IContentHandler;
    procedure setContentHandler(const handler : IContentHandler);
    function getDTDHandler() : IDTDHandler;
    procedure setDTDHandler(const handler : IDTDHandler);
    function getEntityResolver() : IEntityResolver;
    procedure setEntityResolver(const resolver : IEntityResolver);
    function getErrorHandler() : IErrorHandler;
    procedure setErrorHandler(const handler : IErrorHandler);
    function getProperty(const propertyId : SAXString) : IProperty;
    procedure forceValidating();
    procedure setFeature(const featureId : SAXString; state : Boolean);
    function getFeature(const featureId : SAXString) : Boolean;
    //!! setLocale omitted
    procedure parse(const systemId : SAXString); overload;
    procedure parse(const source : IInputSource); overload;
  end;

implementation

{ TXmlReader }

constructor TXmlReader.Create(AOwner: TComponent);
var consumer : IEventConsumer;
begin
  inherited Create(AOwner);
  aelfred2Impl:= TSAXDriver.Create(nil);
  aelfred2:= aelfred2Impl as IXMLReader;
  consumer:= nil;
  filterImpl:= TEventFilter.Create(nil, consumer);
  filter:= filterImpl as IEventConsumer;
end;

destructor TXmlReader.Destroy;
begin
  aelfred2Impl:= nil;
  aelfred2:= nil;
  filterImpl:= nil;
  filter:= nil;
  inherited Destroy();
end;

procedure TXmlReader.AfterConstruction;
begin
  inherited;
  ownerIsComponent:= (Owner <> nil) and (Owner is TComponent);
  InterlockedDecrement(refCount);
end;

procedure TXmlReader.BeforeDestruction;
begin
  inherited;
end;

class function TXmlReader.NewInstance: TObject; 
begin
  Result := inherited NewInstance;
  TXMLReader(Result).refCount:= 1;
end;

function TXmlReader._AddRef: Integer;
begin
  Result:= InterlockedIncrement(refCount);
end;

function TXmlReader._Release: Integer;
begin
  Result:= InterlockedDecrement(refCount);
  if (Result = 0) and (not ownerIsComponent) then
    Destroy;
end;

function TXmlReader.getContentHandler: IContentHandler;
begin
  Result:= filter.getContentHandler();
  Exit;
end;

procedure TXmlReader.setContentHandler(const handler: IContentHandler);
begin
//  if (active) then
//    raise EIllegalStateException.Create('already parsing');
  filterImpl.setContentHandler(handler);
  aelfred2Impl.setContentHandler(filter.getContentHandler());
end;

function TXmlReader.getDTDHandler: IDTDHandler;
begin
  Result:= filter.getDTDHandler();
  Exit;
end;

procedure TXmlReader.setDTDHandler(const handler: IDTDHandler);
begin
//  if (active) then
//    raise EIllegalStateException.Create('already parsing');
  filterImpl.setDTDHandler(handler);
end;

function TXmlReader.getEntityResolver: IEntityResolver;
begin
  Result:= aelfred2.getEntityResolver();
  Exit;
end;

procedure TXmlReader.setEntityResolver(const resolver: IEntityResolver);
begin
  aelfred2.setEntityResolver(resolver);
end;

function TXmlReader.getErrorHandler: IErrorHandler;
begin
  Result:= aelfred2.getErrorHandler();
  Exit;
end;

procedure TXmlReader.setErrorHandler(const handler: IErrorHandler);
begin
//  if (active) then
//    raise EIllegalStateException.Create('already parsing');
  aelfred2.setErrorHandler(handler);
end;

function TXmlReader.getProperty(const propertyId: SAXString): IProperty;
begin
  if ((DeclHandlerProperty = propertyId) or
      (LexicalHandlerProperty = propertyId)) then
  begin
    Result:= filter.getProperty(propertyId);
    Exit;
  end;
  raise ESAXNotRecognizedException.Create(propertyId);
end;

procedure TXmlReader.forceValidating;
begin
  aelfred2.setFeature(NamespacePrefixesFeature, true);
  aelfred2.setFeature(ExternalGeneralFeature, true);
  aelfred2.setFeature(ExternalParameterFeature, true);
end;

procedure TXmlReader.setFeature(const featureId: SAXString;
  state: Boolean);
var value : Boolean;
begin
  value:= getFeature(featureId);

  if (state = value) then
    Exit;

  if (ValidationFeature = featureId) then
  begin
//    if (active) then
//      raise ESAXNotSupportedException.Create('already parsing');
    if (state) then
      forceValidating();
    isValidating:= state;
  end else
    aelfred2.setFeature(featureId, state);
end;

function TXmlReader.getFeature(const featureId: SAXString): Boolean;
begin
  if (ValidationFeature = featureId) then
  begin
    Result:= isValidating;
    Exit;
  end;

  Result:= aelfred2.getFeature(featureId);
  Exit;
end;

procedure TXmlReader.parse(const systemId: SAXString);
var source : IInputSource;
begin
  source:= TInputSource.Create(systemId) as IInputSource;
  try
    parse(source);
  finally
    source:= nil;
  end;
end;

procedure TXmlReader.parse(const source: IInputSource);
var next : IEventConsumer;
begin
  //!!synchronize(aelfred2) {
//  if (active) then
//    raise EIllegalStateException.Create('already parsing');
  active:= true;
  //!!} synchronized

  next:= nil;
  // set up the output pipeline
  if (isValidating) then
  begin
    forceValidating();
    next:= TValidationConsumer.Create(nil, filter);
  end else
    next:= filter;

  TEventFilter.bind(aelfred2, next);

  // parse, clean up
  try
    aelfred2.parse(source);
  finally
    active:= false;
    TEventFilter.bind(aelfred2, nil);
    if (isValidating) then
    begin
      //!! TValidationConsumer(next).Kill;
    end;
  end;
end;

end.

