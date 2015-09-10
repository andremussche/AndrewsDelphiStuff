// Filename : EventFilter.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit EventFilter;

// "!!"

interface

{$I SAX.INC}

uses
{$IFDEF DELPHI6_UP}
  Variants,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, Contnrs, SAX, SAXExt, Dialogs, EventConsumer, HashTable,
  SAXHelpers, SAXDriver;

type

  TEventFilter = class(TComponent, IUnknown, IEventConsumer, IContentHandler,
    IDTDHandler, ILexicalHandler, IDeclHandler, IAElfredDeclHandlerProperty,
    IAElfredLexicalHandlerProperty)
  private
    docHandler : IContentHandler;
    docNext : IContentHandler;
    dtdHandler : IDTDHandler;
    dtdNext : IDTDHandler;
    lexHandler : ILexicalHandler;
    lexNext : ILexicalHandler;
    declHandler : IDeclHandler;
    declNext : IDeclHandler;
    next : IEventConsumer;
    errHandler : IErrorHandler;
    locator : ILocator;
    procedure setDeclHandler(const handler: IDeclHandler);
    procedure setLexicalHandler(const handler: ILexicalHandler);
  protected
    refCount : Integer;
    ownerIsComponent : Boolean;
    // Property interfaces
    function QueryInterfaceDeclHandler(const IID: TGUID;
      out Obj): HResult; stdcall;
    function getDeclHandlerPropertyName : SAXString;
    function getDeclHandlerPropertyValue : IUnknown;
    procedure setDeclHandlerPropertyValue(const value : IUnknown);
    function QueryInterfaceLexicalHandler(const IID: TGUID;
      out Obj): HResult; stdcall;
    function getLexicalHandlerPropertyName : SAXString;
    function getLexicalHandlerPropertyValue : IUnknown;
    procedure setLexicalHandlerPropertyValue(const value : IUnknown);
    // Property Interfaces
    function IAElfredDeclHandlerProperty.QueryInterface = QueryInterfaceDeclHandler;
    function IAElfredDeclHandlerProperty.getName = getDeclHandlerPropertyName;
    function IAElfredDeclHandlerProperty.getValue = getDeclHandlerPropertyValue;
    procedure IAElfredDeclHandlerProperty.setValue = setDeclHandlerPropertyValue;
    function IAElfredLexicalHandlerProperty.QueryInterface = QueryInterfaceLexicalHandler;
    function IAElfredLexicalHandlerProperty.getName = getLexicalHandlerPropertyName;
    function IAElfredLexicalHandlerProperty.getValue = getLexicalHandlerPropertyValue;
    procedure IAElfredLexicalHandlerProperty.setValue = setLexicalHandlerPropertyValue;
  public
    constructor Create(AOwner : TComponent); overload; override;
    constructor Create(AOwner : TComponent; const consumer : IEventConsumer);
      reintroduce; overload; virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    class procedure bind(const producer : IXMLReader; consumer : IEventConsumer);
    procedure chainTo(const next : IXMLFilter);
    procedure setErrorHandler(const handler : IErrorHandler);
    function getErrorHandler() : IErrorHandler;
    function getNext() : IEventConsumer;
    procedure setContentHandler(const handler : IContentHandler);
    function getContentHandler() : IContentHandler;
    procedure setDTDHandler(const handler : IDTDHandler);
    function getDTDHandler() : IDTDHandler;
    function getProperty(const propertyId: SAXString): IProperty;
    function getDocumentLocator() : ILocator;
    procedure setDocumentLocator(const locator: ILocator); virtual;
    procedure startDocument(); virtual;
    procedure skippedEntity(const name : SAXString); virtual;
    procedure processingInstruction(const target, data : SAXString); virtual;
    procedure characters(const ch : SAXString); virtual;
    procedure ignorableWhitespace(const ch : SAXString); virtual;
    procedure startPrefixMapping(const prefix, uri : SAXString); virtual;
    procedure startElement(const uri, localName, qName: SAXString; const atts: IAttributes); virtual;
    procedure endElement(const uri, localName, qName: SAXString); virtual;
    procedure endPrefixMapping(const prefix : SAXString); virtual;
    procedure endDocument(); virtual;
    procedure unparsedEntityDecl(const name, publicId, systemId, notationName : SAXString); virtual;
    procedure notationDecl(const name, publicId, systemId : SAXString); virtual;
    procedure startDTD(const name, publicId, systemId : SAXString); virtual;
    procedure endDTD(); virtual;
    procedure comment(const ch : SAXString); virtual;
    procedure startCDATA(); virtual;
    procedure endCDATA(); virtual;
    procedure startEntity(const name : SAXString); virtual;
    procedure endEntity(const name : SAXString); virtual;
    procedure elementDecl(const name, model : SAXString); virtual;
    procedure attributeDecl(const eName, aName, attrType, mode, value: SAXString); virtual;
    procedure externalEntityDecl(const name, publicId, systemId : SAXString); virtual;
    procedure internalEntityDecl(const name, value : SAXString); virtual;
  end;

implementation

uses NSFilter, WellFormednessFilter;

{ TEventFilter }

constructor TEventFilter.Create(AOwner : TComponent);
var consumer : IEventConsumer;
begin
  consumer:= nil;
  create(AOwner, consumer);
end;

constructor TEventFilter.Create(AOwner : TComponent;
  const consumer: IEventConsumer);
begin
  inherited Create(AOwner);
  if (consumer = nil) then
    Exit;

  next:= consumer;

  docHandler:= consumer.getContentHandler();
  docNext:= consumer.getContentHandler();
  dtdHandler:= consumer.getDTDHandler();
  dtdNext:= consumer.getDTDHandler();
  try
    declHandler:= IInterfaceProperty(consumer.getProperty(
      DeclHandlerProperty)).getValue() as IDeclHandler;
    declNext:= declHandler;
  except
    on e : ESAXException do
    begin
      // leave value null
    end;
  end;

  try
    lexHandler:= IInterfaceProperty(consumer.getProperty(
      LexicalHandlerProperty)).getValue() as ILexicalHandler;
    lexNext:= lexHandler;
  except
    on e : ESAXException do
    begin
      // leave value null
    end;
  end;
end;

procedure TEventFilter.AfterConstruction;
begin
  inherited;
  ownerIsComponent:= (Owner <> nil) and (Owner is TComponent);
  InterlockedDecrement(refCount);
end;

procedure TEventFilter.BeforeDestruction;
begin
  inherited;
end;

class function TEventFilter.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TEventFilter(Result).refCount:= 1;
end;

function TEventFilter._AddRef: Integer;
begin
  Result:= InterlockedIncrement(refCount);
end;

function TEventFilter._Release: Integer;
begin
  Result:= InterlockedDecrement(refCount);
  if (Result = 0) and (not ownerIsComponent) then
    Destroy;
end;

class procedure TEventFilter.bind(const producer: IXMLReader;
  consumer: IEventConsumer);
var // !! prefixes : Boolean;
    h : TDefaultHandler2;
    content : IContentHandler;
    dtd : IDTDHandler;
    error : IErrorHandler;
    lh : ILexicalHandler;
    dh : IDeclHandler;
    cprop : IInterfaceProperty;
    pprop : IInterfaceProperty;
begin
  // DOM building, printing, layered validation, and other
  // things don't work well when prefix info is discarded.
  // Include it by default, whenever possible.
  try
    producer.setFeature(NamespacePrefixesFeature, true);
    // !! prefixes:= true;
  except
    on e : ESAXException do
    begin
      // !! prefixes:= false;
    end;
  end;

  // !! Removed problematic while loop

  // Some SAX parsers can't handle null handlers -- bleech
  h:= TDefaultHandler2.Create();
  content:= h as IContentHandler;
  dtd:= h as IDTDHandler;
  error:= h as IErrorHandler;

  if (consumer <> nil) and (consumer.getContentHandler() <> nil) then
    producer.setContentHandler(consumer.getContentHandler())
  else
    producer.setContentHandler(content);

  if (consumer <> nil) and (consumer.getDTDHandler() <> nil) then
    producer.setDTDHandler(consumer.getDTDHandler())
  else
    producer.setDTDHandler(dtd);

  try
    if (consumer <> nil) then
    begin
      cprop:= IInterfaceProperty(consumer.getProperty(DeclHandlerProperty));
      dh:= IDeclHandler(cprop.getValue())
    end else
      dh:= nil;
    if (dh = nil) then
      dh:= h as IDeclHandler;
    pprop:= IInterfaceProperty(producer.getProperty(DeclHandlerProperty));
    pprop.setValue(dh);
  except
    // ignore
  end;

  try
    if (consumer <> nil) then
    begin
      cprop:= IInterfaceProperty(consumer.getProperty(LexicalHandlerProperty));
      lh:= ILexicalHandler(cprop.getValue())
    end else
      lh:= nil;
    if (lh = nil) then
      lh:= h as ILexicalHandler;
    pprop:= IInterfaceProperty(producer.getProperty(LexicalHandlerProperty));
    pprop.setValue(lh);
  except
    // ignore
  end;

  // this binding goes the other way around
  if (producer.getErrorHandler() = nil) then
    producer.setErrorHandler(h);
  if (consumer <> nil) then
    consumer.setErrorHandler(producer.getErrorHandler());
end;

procedure TEventFilter.chainTo(const next : IXMLFilter);
begin
  if (Self.next <> nil) then
    raise EIllegalStateException.Create('');

  docNext:= next.getContentHandler();
  if (docHandler = nil) then
    docHandler:= docNext;
  dtdNext:= next.getDTDHandler();
  if (dtdHandler = nil) then
    dtdHandler:= dtdNext;

  try
    declNext:= IDeclHandler(IInterfaceProperty(next.getProperty(
      DeclHandlerProperty)).getValue());
    if (declHandler = nil) then
      declHandler:= declNext;
  except
    // leave value null
  end;

  try
    lexNext:= ILexicalHandler(IInterfaceProperty(next.getProperty(
      LexicalHandlerProperty)).getValue());
    if (lexHandler = nil) then
      lexHandler:= lexNext;
  except
    // leave value null
  end;

  if (errHandler <> nil) then
    next.setErrorHandler(errHandler);
end;

procedure TEventFilter.setErrorHandler(
  const handler: IErrorHandler);
begin
{$IFDEF DELPHI6_UP}
  ReferenceInterface(errHandler, opRemove);
  errHandler:= handler;
  ReferenceInterface(errHandler, opInsert);
  {$ELSE}
  errHandler:= handler;
  {$ENDIF}
  if (next <> nil) then
    next.setErrorHandler(handler);
end;

function TEventFilter.getErrorHandler: IErrorHandler;
begin
  Result:= errHandler;
  Exit;
end;

function TEventFilter.getNext: IEventConsumer;
begin
  Result:= next;
  Exit;
end;

procedure TEventFilter.setContentHandler(
  const handler: IContentHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(docHandler, opRemove);
  docHandler:= handler;
  ReferenceInterface(docHandler, opInsert);
  {$ELSE}
  docHandler:= handler;
  {$ENDIF}
end;

function TEventFilter.getContentHandler: IContentHandler;
begin
  Result:= docHandler;
  Exit;
end;

procedure TEventFilter.setDTDHandler(const handler: IDTDHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(dtdHandler, opRemove);
  dtdHandler:= handler;
  ReferenceInterface(dtdHandler, opInsert);
  {$ELSE}
  dtdHandler:= handler;
  {$ENDIF}
end;

function TEventFilter.getDTDHandler: IDTDHandler;
begin
  Result:= dtdHandler;
  Exit;
end;

function TEventFilter.getProperty(
  const propertyId: SAXString): IProperty;
begin
  if (DeclHandlerProperty = propertyId) then
  begin
    Result:= IAElfredDeclHandlerProperty(Self);
    Exit;
  end;

  if (LexicalHandlerProperty = propertyId) then
  begin
    Result:= IAElfredLexicalHandlerProperty(Self);
    Exit;
  end;

  // !! We are not storing unknown properties
  raise ESAXNotRecognizedException.Create(propertyId);
end;

function TEventFilter.getDocumentLocator() : ILocator;
begin
  Result:= locator;
  Exit;
end;

procedure TEventFilter.setDocumentLocator(
  const locator: ILocator);
begin
  Self.locator:= locator;
  if (docNext <> nil) then
    docNext.setDocumentLocator(locator);
end;

procedure TEventFilter.startDocument;
begin
  if (docNext <> nil) then
    docNext.startDocument();
end;

procedure TEventFilter.skippedEntity(const name: SAXString);
begin
  if (docNext <> nil) then
    docNext.skippedEntity(name);
end;

procedure TEventFilter.processingInstruction(const target,
  data: SAXString);
begin
  if (docNext <> nil) then
    docNext.processingInstruction(target, data);
end;

procedure TEventFilter.characters(const ch : SAXString);
begin
  if (docNext <> nil) then
    docNext.characters(ch);
end;

procedure TEventFilter.ignorableWhitespace(const ch : SAXString);
begin
  if (docNext <> nil) then
    docNext.ignorableWhitespace(ch);
end;

procedure TEventFilter.startPrefixMapping(const prefix,
  uri: SAXString);
begin
  if (docNext <> nil) then
    docNext.startPrefixMapping(prefix, uri);
end;

procedure TEventFilter.startElement(const uri, localName,
  qName: SAXString; const atts: IAttributes);
begin
  if (docNext <> nil) then
    docNext.startElement(uri, localName, qName, atts);
end;

procedure TEventFilter.endElement(const uri, localName, qName: SAXString);
begin
  if (docNext <> nil) then
    docNext.endElement(uri, localName, qName);
end;

procedure TEventFilter.endPrefixMapping(const prefix: SAXString);
begin
  if (docNext <> nil) then
    docNext.endPrefixMapping(prefix);
end;

procedure TEventFilter.endDocument;
begin
  if (docNext <> nil) then
    docNext.endDocument();
  locator:= nil;
end;

procedure TEventFilter.unparsedEntityDecl(const name, publicId, systemId,
  notationName : SAXString);
begin
  if (dtdNext <> nil) then
    dtdNext.unparsedEntityDecl(name, publicId, systemId, notationName);
end;

procedure TEventFilter.notationDecl(const name, publicId, systemId : SAXString);
begin
  if (dtdNext <> nil) then
    dtdNext.notationDecl(name, publicId, systemId);
end;

procedure TEventFilter.startDTD(const name, publicId, systemId: SAXString);
begin
  if (lexNext <> nil) then
    lexNext.startDTD(name, publicId, systemId);
end;

procedure TEventFilter.endDTD;
begin
  if (lexNext <> nil) then
    lexNext.endDTD();
end;

procedure TEventFilter.comment(const ch : SAXString);
begin
  if (lexNext <> nil) then
    lexNext.comment(ch);
end;

procedure TEventFilter.startCDATA;
begin
  if (lexNext <> nil) then
    lexNext.startCDATA();
end;

procedure TEventFilter.endCDATA;
begin
  if (lexNext <> nil) then
    lexNext.endCDATA();
end;

procedure TEventFilter.startEntity(const name: SAXString);
begin
  if (lexNext <> nil) then
    lexNext.startEntity(name);
end;

procedure TEventFilter.endEntity(const name: SAXString);
begin
  if (lexNext <> nil) then
    lexNext.endEntity(name);
end;

procedure TEventFilter.elementDecl(const name,
  model: SAXString);
begin
  if (declNext <> nil) then
    declNext.elementDecl(name, model);
end;

procedure TEventFilter.attributeDecl(const eName, aName, attrType, mode,
 value: SAXString);
begin
  if (declNext <> nil) then
    declNext.attributeDecl(eName, aName, attrType, mode, value);
end;

procedure TEventFilter.externalEntityDecl(const name, publicId,
  systemId: SAXString);
begin
  if (declNext <> nil) then
    declNext.externalEntityDecl(name, publicId, systemId);
end;

procedure TEventFilter.internalEntityDecl(const name,
  value: SAXString);
begin
  if (declNext <> nil) then
    declNext.internalEntityDecl(name, value);
end;

function TEventFilter.QueryInterfaceDeclHandler(const IID: TGUID;
  out Obj): HResult; stdcall;

  function Equal(G1, G2 : TGUID) : Boolean;
  begin
    Result:= (G1.D1 = G2.D1) and
             (G1.D2 = G2.D2) and
             (G1.D3 = G2.D3) and
             (G1.D4[0] = G2.D4[0]) and
             (G1.D4[1] = G2.D4[1]) and
             (G1.D4[2] = G2.D4[2]) and
             (G1.D4[3] = G2.D4[3]) and
             (G1.D4[4] = G2.D4[4]) and
             (G1.D4[5] = G2.D4[5]) and
             (G1.D4[6] = G2.D4[6]);
  end;

begin
  if Equal(IID, IInterfaceProperty) then
  begin
    Result:= 0; // S_OK
    IAElfredDeclHandlerProperty(Obj):= Self;
  end else
    Result:= inherited QueryInterface(IID, Obj);
end;

function TEventFilter.getDeclHandlerPropertyName : SAXString;
begin
  Result:= DeclHandlerProperty;
end;

function TEventFilter.getDeclHandlerPropertyValue : IUnknown;
begin
  Result:= declHandler;
end;

procedure TEventFilter.setDeclHandler(const handler : IDeclHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(declHandler, opRemove);
  declHandler:= handler;
  ReferenceInterface(declHandler, opInsert);
  {$ELSE}
  declHandler:= handler;
  {$ENDIF}
end;

procedure TEventFilter.setDeclHandlerPropertyValue(const value : IUnknown);
begin
  if (value = declHandler) then
    Exit;
  setDeclHandler(IDeclHandler(value));
end;

function TEventFilter.QueryInterfaceLexicalHandler(const IID: TGUID;
  out Obj): HResult; stdcall;

  function Equal(G1, G2 : TGUID) : Boolean;
  begin
    Result:= (G1.D1 = G2.D1) and
             (G1.D2 = G2.D2) and
             (G1.D3 = G2.D3) and
             (G1.D4[0] = G2.D4[0]) and
             (G1.D4[1] = G2.D4[1]) and
             (G1.D4[2] = G2.D4[2]) and
             (G1.D4[3] = G2.D4[3]) and
             (G1.D4[4] = G2.D4[4]) and
             (G1.D4[5] = G2.D4[5]) and
             (G1.D4[6] = G2.D4[6]);
  end;

begin
  if Equal(IID, IInterfaceProperty) then
  begin
    Result:= 0; // S_OK
    IAElfredLexicalHandlerProperty(Obj):= Self;
  end else
    Result:= inherited QueryInterface(IID, Obj);
end;

function TEventFilter.getLexicalHandlerPropertyName : SAXString;
begin
  Result:= LexicalHandlerProperty;
end;

function TEventFilter.getLexicalHandlerPropertyValue : IUnknown;
begin
  Result:= lexHandler;
end;

procedure TEventFilter.setLexicalHandler(const handler : ILexicalHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(lexHandler, opRemove);
  lexHandler:= handler;
  ReferenceInterface(lexHandler, opInsert);
  {$ELSE}
  lexHandler:= handler;
  {$ENDIF}
end;

procedure TEventFilter.setLexicalHandlerPropertyValue(const value : IUnknown);
begin
  if (value = lexHandler) then
    Exit;
  setLexicalHandler(ILexicalHandler(value));
end;

end.
