// SAX for Pascal, Simple API for XML Interfaces in Pascal.
// Ver 1.1 July 4, 2003
// http://xml.defined.net/SAX (this will change!)
// Based on http://www.saxproject.org/
// No warranty; no copyright -- use this as you will.
unit SAXAdapters;

interface

uses SAX, SAXExt, BSAX, BSAXExt;

resourcestring
  SAdaptedNil = 'Adapted interface must not be nil';

  // functions to adapt from buffered to un-buffered interfaces
  function adapt(const value: IBufferedContentHandler;
    useAttributes2: Boolean): IContentHandler; overload;
  function adapt(const value: IBufferedContentHandler;
    const XMLReader: IXMLReader): IContentHandler; overload;
  function adapt(const value: IBufferedDTDHandler): IDTDHandler; overload;
  function adapt(const value: IBufferedDeclHandler): IDeclHandler; overload;
  function adapt(const value: IBufferedLexicalHandler): ILexicalHandler; overload;

  // functions to adapt from un-buffered to buffered interfaces
  function adapt(const value: IContentHandler;
    useAttributes2: Boolean): IBufferedContentHandler; overload;
  function adapt(const value: IContentHandler;
    const XMLReader: IBufferedXMLReader): IBufferedContentHandler; overload;
  function adapt(const value: IDTDHandler): IBufferedDTDHandler; overload;
  function adapt(const value: IDeclHandler): IBufferedDeclHandler; overload;
  function adapt(const value: ILexicalHandler): IBufferedLexicalHandler; overload;

type

  ISAXAdapter = interface
    ['{5794D4DF-E2B4-45CD-BD8B-52467E5A2F68}']
    function getAdaptedIntf: IUnknown;
    property AdaptedIntf: IUnknown read getAdaptedIntf;           
  end;

  TSAXAdapter = class(TInterfacedObject, ISAXAdapter)
  private
    FAdapted: IUnknown;
  protected
    property Adapted: IUnknown read FAdapted;  // convenience property
    { ISAXAdapter }
    function getAdaptedIntf: IUnknown;
  public
    constructor Create(const adaptedIntf: IUnknown);
  end;

  TBufferedAttributesAdapter = class;

  TAttributesAdapter = class(TInterfacedObject, IBufferedAttributes)
  private
    FAttributes: IAttributes;
  protected
    FUriStr,
    FLocalNameStr,
    FQNameStr,
    FTypeStr,
    FValueStr: SAXString;
    { IBufferedAttributes }
    function getLength() : Integer;
    procedure getURI(index : Integer;
      out uri: PSAXChar; out uriLength: Integer);
    procedure getLocalName(index : Integer;
     out localName: PSAXChar; out localNameLength: Integer);
    procedure getQName(index : Integer;
      out qName: PSAXChar; out qNameLength: Integer);
    procedure getType(index : Integer;
      out attType: PSAXChar; out attTypeLength: Integer); overload;
    procedure getType(uri: PSAXChar; uriLength: Integer;
      localName: PSAXChar;  localNameLength: Integer;
      out attType: PSAXChar; out attTypeLength: Integer); overload;
    procedure getType(qName: PSAXChar;  qNameLength: Integer;
      out attType: PSAXChar; out attTypeLength: Integer); overload;
    procedure getValue(index : Integer;
       out value: PSAXChar; out valueLength: Integer); overload;
    procedure getValue(uri : PSAXChar; uriLength : Integer;
       localName : PSAXChar; localNameLength : Integer;
       out value: PSAXChar; out valueLength: Integer); overload;
    procedure getValue(qName : PSAXChar; qNameLength : Integer;
       out value: PSAXChar; out valueLength: Integer); overload;
    function getIndex(uri : PSAXChar; uriLength : Integer;
       localName : PSAXChar; localNameLength : Integer) : Integer; overload;
    function getIndex(qName : PSAXChar;
      qNameLength : Integer) : Integer; overload;
  public
    procedure init(const attributes: IAttributes);
    function getInterface() : IBufferedAttributes; virtual;
  end;

  TAttributes2Adapter = class(TAttributesAdapter, IBufferedAttributes2)
  protected
    { IBufferedAttributes2 }
    function isDeclared(index : Integer) : Boolean; overload;
    function isDeclared(qName : PSAXChar;
      qNameLength : Integer) : Boolean; overload;
    function isDeclared(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer) : Boolean; overload;
    function isSpecified(index : Integer) : Boolean; overload;
    function isSpecified(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer) : Boolean; overload;
    function isSpecified(qName : PSAXChar;
      qNameLength : Integer) : Boolean; overload;
  public
    function getInterface() : IBufferedAttributes; override;
  end;

  TContentHandlerAdapter = class(TSAXAdapter, IBufferedContentHandler)
  private
    FAttributes: TBufferedAttributesAdapter;
  protected
    function getUseAttributes2: Boolean;
    procedure setUseAttributes2(Value: Boolean);
    { IBufferedContentHandler }
    procedure setDocumentLocator(const locator: ILocator);
    procedure startDocument();
    procedure endDocument();
    procedure startPrefixMapping(prefix: PSAXChar;
      prefixLength: Integer; uri: PSAXChar;
      uriLength: Integer);
    procedure endPrefixMapping(prefix: PSAXChar;
      prefixLength: Integer);
    procedure startElement(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer;
      qName : PSAXChar; qNameLength : Integer;
      const atts: IBufferedAttributes);
    procedure endElement(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer;
      qName : PSAXChar; qNameLength : Integer);
    procedure characters(ch : PSAXChar; chLength : Integer);
    procedure ignorableWhitespace(ch : PSAXChar; chLength : Integer);
    procedure processingInstruction(target : PSAXChar;
      targetLength : Integer; data : PSAXChar;
      dataLength : Integer);
    procedure skippedEntity(name : PSAXChar; nameLength : Integer);
  public
    constructor create(const contentHandler: IContentHandler;
      useAttributes2: Boolean);
    destructor Destroy; override;
    property UseAttributes2: Boolean
      read GetUseAttributes2 write SetUseAttributes2;
  end;

  TDTDHandlerAdapter = class(TSAXAdapter, IBufferedDTDHandler)
  protected
    { IBufferedDTDHandler }
    procedure notationDecl(name : PSAXChar; nameLength : Integer;
      publicId : PSAXChar; publicIdLength : Integer;
      systemId : PSAXChar; systemIdLength : Integer);
    procedure unparsedEntityDecl(name : PSAXChar;
      nameLength : Integer; publicId : PSAXChar;
      publicIdLength : Integer; systemId : PSAXChar;
      systemIdLength : Integer; notationName : PSAXChar;
      notationNameLength : Integer);
  public
    constructor create(const dtdHandler: IDTDHandler);
  end;

  TDeclHandlerAdapter = class(TSAXAdapter, IBufferedDeclHandler)
  protected
    { IBufferedDeclHandler }
    procedure elementDecl(name : PSAXChar; nameLength : Integer;
      model : PSAXChar; modelLength : Integer);
    procedure attributeDecl(eName : PSAXChar; eNameLength : Integer;
      aName : PSAXChar; aNameLength : Integer;
      attrType : PSAXChar; attrTypeLength : Integer;
      mode : PSAXChar; modeLength : Integer;
      value : PSAXChar; valueLength : Integer);
    procedure internalEntityDecl(name : PSAXChar;
      nameLength : Integer; value : PSAXChar;
      valueLength : Integer);
    procedure externalEntityDecl(name : PSAXChar;
      nameLength : Integer; publicId : PSAXChar;
      publicIdLength : Integer; systemId : PSAXChar;
      systemIdLength : Integer);
  public
    constructor create(const declHandler: IDeclHandler);
  end;

  TLexicalHandlerAdapter = class(TSAXAdapter, IBufferedLexicalHandler)
  protected
    { IBufferedLexicalHandler }
    procedure startDTD(name : PSAXChar; nameLength : Integer;
      publicId : PSAXChar; publicIdLength : Integer;
      systemId : PSAXChar; systemIdLength : Integer);
    procedure endDTD();
    procedure startEntity(name : PSAXChar; nameLength : Integer);
    procedure endEntity(name : PSAXChar; nameLength : Integer);
    procedure startCDATA();
    procedure endCDATA();
    procedure comment(ch : PSAXChar; chLength : Integer);
  public
    constructor create(const lexicalHandler: ILexicalHandler);
  end;

  TBufferedAttributesAdapter = class(TInterfacedObject, IAttributes)
  private
    FAttributes: IBufferedAttributes;
  protected
    { IAttributes }
    function getLength() : Integer;
    function getURI(index : Integer) : SAXString;
    function getLocalName(index : Integer) : SAXString;
    function getQName(index : Integer) : SAXString;
    function getType(index : Integer) : SAXString; overload;
    function getType(const uri, localName : SAXString) : SAXString; overload;
    function getType(const qName : SAXString) : SAXString; overload;
    function getValue(index : Integer) : SAXString; overload;
    function getValue(const uri, localName : SAXString) : SAXString; overload;
    function getValue(const qName : SAXString) : SAXString; overload;
    function getIndex(const uri, localName : SAXString) : Integer; overload;
    function getIndex(const qName : SAXString) : Integer; overload;
  public
    procedure init(const bufferedAttributes: IBufferedAttributes);
    function getInterface(): IAttributes; virtual;
  end;

  TBufferedAttributes2Adapter = class(TBufferedAttributesAdapter, IAttributes2)
  protected
    { IAttributes2 }
    function isDeclared(index : Integer) : Boolean; overload;
    function isDeclared(const qName : SAXString) : Boolean; overload;
    function isDeclared(const uri, localName : SAXString) : Boolean; overload;
    function isSpecified(index : Integer) : Boolean; overload;
    function isSpecified(const uri, localName : SAXString) : Boolean; overload;
    function isSpecified(const qName : SAXString) : Boolean; overload;
  public
    function getInterface(): IAttributes; override;
  end;

  TBufferedContentHandlerAdapter = class(TSAXAdapter, IContentHandler)
  private
    FAttributes: TAttributesAdapter;
  protected
    function getUseAttributes2: Boolean;
    procedure setUseAttributes2(Value: Boolean);
    { IContentHandler }
    procedure setDocumentLocator(const locator: ILocator);
    procedure startDocument();
    procedure endDocument();
    procedure startPrefixMapping(const prefix, uri : SAXString);
    procedure endPrefixMapping(const prefix : SAXString);
    procedure startElement(const uri, localName, qName: SAXString;
      const atts: IAttributes);
    procedure endElement(const uri, localName, qName: SAXString);
    procedure characters(const ch : SAXString);
    procedure ignorableWhitespace(const ch : SAXString);
    procedure processingInstruction(const target, data : SAXString);
    procedure skippedEntity(const name : SAXString);
  public
    constructor create(const contentHandler: IBufferedContentHandler;
      useAttributes2: Boolean);
    destructor Destroy; override;
    property UseAttributes2: Boolean
      read GetUseAttributes2 write SetUseAttributes2;
  end;

  TBufferedDTDHandlerAdapter = class(TSAXAdapter, IDTDHandler)
  protected
    { IDTDHandler }
    procedure notationDecl(const name, publicId, systemId : SAXString);
    procedure unparsedEntityDecl(const name, publicId, systemId,
      notationName : SAXString);
  public
    constructor create(const dtdHandler: IBufferedDTDHandler);
  end;

  TBufferedDeclHandlerAdapter = class(TSAXAdapter, IDeclHandler)
  protected
    { IDeclHandler }
    procedure elementDecl(const name, model : SAXString);
    procedure attributeDecl(const eName, aName, attrType, mode,
      value: SAXString);
    procedure internalEntityDecl(const name, value : SAXString);
    procedure externalEntityDecl(const name, publicId, systemId : SAXString);
  public
    constructor create(const declHandler: IBufferedDeclHandler);
  end;

  TBufferedLexicalHandlerAdapter = class(TSAXAdapter, ILexicalHandler)
  protected
    { ILexicalHandler }
    procedure startDTD(const name, publicId, systemId : SAXString);
    procedure endDTD();
    procedure startEntity(const name : SAXString);
    procedure endEntity(const name : SAXString);
    procedure startCDATA();
    procedure endCDATA();
    procedure comment(const ch : SAXString);
  public
    constructor create(const lexicalHandler: IBufferedLexicalHandler);
  end;


implementation

{ Functions to adapt from buffered to un-buffered interfaces}

function adapt(const value: IBufferedContentHandler;
  useAttributes2: Boolean): IContentHandler;
begin
  Result:= TBufferedContentHandlerAdapter.create(value, useAttributes2);
end;

function adapt(const value: IBufferedContentHandler;
  const XMLReader: IXMLReader): IContentHandler;
var
 useAttributes2: Boolean;
begin
  try
    useAttributes2:= XMLReader.Features[useAttributes2Feature];
  except
    on ESAXNotSupportedException do useAttributes2:= False;
    on ESAXNotRecognizedException do useAttributes2:= False;
    else raise;
  end;
  Result:= adapt(value, useAttributes2);
end;

function adapt(const value: IBufferedDTDHandler): IDTDHandler;
begin
  Result:= TBufferedDTDHandlerAdapter.create(value);
end;

function adapt(const value: IBufferedDeclHandler): IDeclHandler;
begin
  Result:= TBufferedDeclHandlerAdapter.create(value);
end;

function adapt(const value: IBufferedLexicalHandler): ILexicalHandler;
begin
  Result:= TBufferedLexicalHandlerAdapter.create(value);
end;

{ Functions to adapt from un-buffered to buffered interfaces}

function adapt(const value: IContentHandler;
  useAttributes2: Boolean): IBufferedContentHandler;
begin
  Result:= TContentHandlerAdapter.create(value, useAttributes2);
end;

function adapt(const value: IContentHandler;
  const XMLReader: IBufferedXMLReader): IBufferedContentHandler;
var
  useAttributes2: Boolean;
begin
  try
    useAttributes2:= XMLReader.Features[UseAttributes2Feature];
  except
    on ESAXNotSupportedException do useAttributes2:= False;
    on ESAXNotRecognizedException do useAttributes2:= False;
    else raise;
  end;
  Result:= adapt(value, useAttributes2);
end;

function adapt(const value: IDTDHandler): IBufferedDTDHandler;
begin
  Result:= TDTDHandlerAdapter.create(value);
end;

function adapt(const value: IDeclHandler): IBufferedDeclHandler;
begin
  Result:= TDeclHandlerAdapter.create(value);
end;

function adapt(const value: ILexicalHandler): IBufferedLexicalHandler;
begin
  Result:= TLexicalHandlerAdapter.create(value);
end;

{ TSAXAdapter }

constructor TSAXAdapter.Create(const adaptedIntf: IUnknown);
begin
  if adaptedIntf = nil then raise ESAXException.Create(SAdaptedNil);
  inherited Create;
  FAdapted := adaptedIntf;
end;

function TSAXAdapter.getAdaptedIntf: IUnknown;
begin
  Result := FAdapted;
end;

{ TAttributesAdapter }

function TAttributesAdapter.getIndex(uri: PSAXChar; uriLength: Integer;
  localName: PSAXChar; localNameLength: Integer): Integer;
begin
  Result:= FAttributes.getIndex(PSAXCharToSAXString(uri, uriLength),
    PSAXCharToSAXString(localName, localNameLength));
end;

function TAttributesAdapter.getIndex(qName: PSAXChar;
  qNameLength: Integer): Integer;
begin
  Result:= FAttributes.getIndex(PSAXCharToSAXString(qName, qNameLength));
end;

function TAttributesAdapter.getInterface: IBufferedAttributes;
begin
  Result:= Self;
end;

function TAttributesAdapter.getLength: Integer;
begin
  Result:= FAttributes.getLength();
end;

procedure TAttributesAdapter.getLocalName(index: Integer;
  out localName: PSAXChar; out localNameLength: Integer);
begin
  FLocalNameStr:= FAttributes.getLocalName(index);
  localName:= PSAXChar(FLocalNameStr);
  localNameLength:= Length(FLocalNameStr);
end;

procedure TAttributesAdapter.getQName(index: Integer; out qName: PSAXChar;
  out qNameLength: Integer);
begin
  FQNameStr:= FAttributes.getQName(index);
  qName:= PSAXChar(FQNameStr);
  qNameLength:= Length(FQNameStr);
end;

procedure TAttributesAdapter.getType(uri: PSAXChar; uriLength: Integer;
  localName: PSAXChar; localNameLength: Integer; out attType: PSAXChar;
  out attTypeLength: Integer);
begin
  FTypeStr:= FAttributes.getType(PSAXCharToSAXString(uri, uriLength),
    PSAXCharToSAXString(localName, localNameLength));
  attType:= PSAXChar(FTypeStr);
  attTypeLength:= Length(FTypeStr);
end;

procedure TAttributesAdapter.getType(qName: PSAXChar; qNameLength: Integer;
  out attType: PSAXChar; out attTypeLength: Integer);
begin
  FTypeStr:= FAttributes.getType(PSAXCharToSAXString(qName, qNameLength));
  attType:= PSAXChar(FTypeStr);
  attTypeLength:= Length(FTypeStr);
end;

procedure TAttributesAdapter.getType(index: Integer; out attType: PSAXChar;
  out attTypeLength: Integer);
begin
  FTypeStr:= FAttributes.getType(index);
  attType:= PSAXChar(FTypeStr);
  attTypeLength:= Length(FTypeStr);
end;

procedure TAttributesAdapter.getURI(index: Integer; out uri: PSAXChar;
  out uriLength: Integer);
begin
  FUriStr:= FAttributes.getUri(index);
  uri:= PSAXChar(FUriStr);
  uriLength:= Length(FUriStr);
end;

procedure TAttributesAdapter.getValue(qName: PSAXChar;
  qNameLength: Integer; out value: PSAXChar; out valueLength: Integer);
begin
  FValueStr:= FAttributes.getValue(PSAXCharToSAXString(qName, qNameLength));
  value:= PSAXChar(FValueStr);
  valueLength:= Length(FValueStr);
end;

procedure TAttributesAdapter.getValue(uri: PSAXChar; uriLength: Integer;
  localName: PSAXChar; localNameLength: Integer; out value: PSAXChar;
  out valueLength: Integer);
begin
  FValueStr:= FAttributes.getValue(PSAXCharToSAXString(uri, uriLength),
    PSAXCharToSAXString(localName, localNameLength));
  value:= PSAXChar(FValueStr);
  valueLength:= Length(FValueStr);
end;

procedure TAttributesAdapter.getValue(index: Integer; out value: PSAXChar;
  out valueLength: Integer);
begin
  FValueStr:= FAttributes.getValue(index);
  value:= PSAXChar(FValueStr);
  valueLength:= Length(FValueStr);
end;

procedure TAttributesAdapter.init(const attributes: IAttributes);
begin
  FAttributes:= attributes;
  FUriStr := '';
  FLocalNameStr := '';
  FQNameStr := '';
  FTypeStr := '';
  FValueStr := '';
end;

{ TAttributes2Adapter }

function TAttributes2Adapter.getInterface: IBufferedAttributes;
begin
  Result:= IBufferedAttributes2(Self);
end;

function TAttributes2Adapter.isDeclared(index : Integer) : Boolean;
begin
  Result:= IAttributes2(FAttributes).isDeclared(index);
end;

function TAttributes2Adapter.isDeclared(qName : PSAXChar; qNameLength : Integer) : Boolean;
begin
  Result:= IAttributes2(FAttributes).isDeclared(
    PSAXCharToSAXString(qName, qNameLength));
end;

function TAttributes2Adapter.isDeclared(uri : PSAXChar; uriLength : Integer;
  localName : PSAXChar; localNameLength : Integer) : Boolean;
begin
  Result:= IAttributes2(FAttributes).isDeclared(
    PSAXCharToSAXString(uri, uriLength),
    PSAXCharToSAXString(localName, localNameLength));
end;

function TAttributes2Adapter.isSpecified(index: Integer): Boolean;
begin
  Result:= IAttributes2(FAttributes).isSpecified(index);
end;

function TAttributes2Adapter.isSpecified(uri: PSAXChar; uriLength: Integer;
  localName: PSAXChar; localNameLength: Integer): Boolean;
begin
  Result:= IAttributes2(FAttributes).isSpecified(
    PSAXCharToSAXString(uri, uriLength),
    PSAXCharToSAXString(localName, localNameLength));
end;

function TAttributes2Adapter.isSpecified(qName: PSAXChar;
  qNameLength: Integer): Boolean;
begin
  Result:= IAttributes2(FAttributes).isSpecified(
    PSAXCharToSAXString(qName, qNameLength));
end;

{ TContentHandlerAdapter }

procedure TContentHandlerAdapter.characters(ch: PSAXChar;
  chLength: Integer);
begin
  IContentHandler(Adapted).characters(PSAXCharToSAXString(ch, chLength));
end;

constructor TContentHandlerAdapter.create(
  const contentHandler: IContentHandler; useAttributes2: Boolean);
begin
  inherited Create(contentHandler);
  if UseAttributes2 then
    FAttributes:= TBufferedAttributes2Adapter.create()
  else
    FAttributes:= TBufferedAttributesAdapter.create();
  FAttributes._AddRef;
end;

destructor TContentHandlerAdapter.destroy;
begin
  FAttributes._Release;
  inherited;
end;

procedure TContentHandlerAdapter.endDocument;
begin
  IContentHandler(Adapted).endDocument();
end;

procedure TContentHandlerAdapter.endElement(uri: PSAXChar;
  uriLength: Integer; localName: PSAXChar; localNameLength: Integer;
  qName: PSAXChar; qNameLength: Integer);
begin
  IContentHandler(Adapted).endElement(PSAXCharToSAXString(uri, uriLength),
    PSAXCharToSAXString(localName, localNameLength),
    PSAXCharToSAXString(qName, qNameLength));
end;

procedure TContentHandlerAdapter.endPrefixMapping(prefix: PSAXChar;
  prefixLength: Integer);
begin
  IContentHandler(Adapted).endPrefixMapping(PSAXCharToSAXString(prefix, prefixLength));
end;

function TContentHandlerAdapter.getUseAttributes2: Boolean;
begin
  Result := FAttributes is TBufferedAttributes2Adapter;
end;

procedure TContentHandlerAdapter.ignorableWhitespace(ch: PSAXChar;
  chLength: Integer);
begin
  IContentHandler(Adapted).ignorableWhitespace(PSAXCharToSAXString(ch, chLength));
end;

procedure TContentHandlerAdapter.processingInstruction(target: PSAXChar;
  targetLength: Integer; data: PSAXChar; dataLength: Integer);
begin
  IContentHandler(Adapted).processingInstruction(
    PSAXCharToSAXString(target, targetLength),
    PSAXCharToSAXString(data, dataLength));
end;

procedure TContentHandlerAdapter.setDocumentLocator(
  const locator: ILocator);
begin
  IContentHandler(Adapted).setDocumentLocator(locator);
end;

procedure TContentHandlerAdapter.setUseAttributes2(Value: Boolean);
begin
  if Value and (FAttributes is TBufferedAttributesAdapter) then
  begin
    FAttributes._Release;
    FAttributes := TBufferedAttributes2Adapter.Create;
    FAttributes._AddRef;
  end
  else if not Value and (FAttributes is TBufferedAttributes2Adapter) then
  begin
    FAttributes._Release;
    FAttributes := TBufferedAttributesAdapter.Create;
    FAttributes._AddRef;
  end;
end;

procedure TContentHandlerAdapter.skippedEntity(name: PSAXChar;
  nameLength: Integer);
begin
  IContentHandler(Adapted).skippedEntity(PSAXCharToSAXString(name, nameLength));
end;

procedure TContentHandlerAdapter.startDocument;
begin
  IContentHandler(Adapted).startDocument();
end;

procedure TContentHandlerAdapter.startElement(uri: PSAXChar;
  uriLength: Integer; localName: PSAXChar; localNameLength: Integer;
  qName: PSAXChar; qNameLength: Integer; const atts: IBufferedAttributes);
begin
  if (atts <> nil) then
  begin
    FAttributes.init(atts);
    IContentHandler(Adapted).startElement(PSAXCharToSAXString(uri, uriLength),
      PSAXCharToSAXString(localName, localNameLength),
      PSAXCharToSAXString(qName, qNameLength), FAttributes.getInterface());
  end else
  begin
    IContentHandler(Adapted).startElement(PSAXCharToSAXString(uri, uriLength),
      PSAXCharToSAXString(localName, localNameLength),
      PSAXCharToSAXString(qName, qNameLength), nil);
  end;
end;

procedure TContentHandlerAdapter.startPrefixMapping(prefix: PSAXChar;
  prefixLength: Integer; uri: PSAXChar; uriLength: Integer);
begin
  IContentHandler(Adapted).startPrefixMapping(PSAXCharToSAXString(prefix, prefixLength),
    PSAXCharToSAXString(uri, uriLength));
end;

{ TDTDHandlerAdapter }

constructor TDTDHandlerAdapter.create(const dtdHandler: IDTDHandler);
begin
  inherited create(dtdHandler);
end;

procedure TDTDHandlerAdapter.notationDecl(name: PSAXChar;
  nameLength: Integer; publicId: PSAXChar; publicIdLength: Integer;
  systemId: PSAXChar; systemIdLength: Integer);
begin
  IDTDHandler(Adapted).notationDecl(
    PSAXCharToSAXString(name, nameLength),
    PSAXCharToSAXString(publicId, publicIdLength),
    PSAXCharToSAXString(systemId, systemIdLength));
end;

procedure TDTDHandlerAdapter.unparsedEntityDecl(name: PSAXChar;
  nameLength: Integer; publicId: PSAXChar; publicIdLength: Integer;
  systemId: PSAXChar; systemIdLength: Integer; notationName: PSAXChar;
  notationNameLength: Integer);
begin
  IDTDHandler(Adapted).unparsedEntityDecl(
    PSAXCharToSAXString(name, nameLength),
    PSAXCharToSAXString(publicId, publicIdLength),
    PSAXCharToSAXString(systemId, systemIdLength),
    PSAXCharToSAXString(notationName, notationNameLength));
end;

{ TDeclHandlerAdapter }

procedure TDeclHandlerAdapter.attributeDecl(eName: PSAXChar;
  eNameLength: Integer; aName: PSAXChar; aNameLength: Integer;
  attrType: PSAXChar; attrTypeLength: Integer; mode: PSAXChar;
  modeLength: Integer; value: PSAXChar; valueLength: Integer);
begin
  IDeclHandler(Adapted).attributeDecl(
    PSAXCharToSAXString(eName, eNameLength),
    PSAXCharToSAXString(aName, aNameLength),
    PSAXCharToSAXString(attrType, attrTypeLength),
    PSAXCharToSAXString(mode, modeLength),
    PSAXCharToSAXString(value, valueLength));
end;

constructor TDeclHandlerAdapter.create(const declHandler: IDeclHandler);
begin
  inherited create(declHandler);
end;

procedure TDeclHandlerAdapter.elementDecl(name: PSAXChar;
  nameLength: Integer; model: PSAXChar; modelLength: Integer);
begin
  IDeclHandler(Adapted).elementDecl(
    PSAXCharToSAXString(name, nameLength),
    PSAXCharToSAXString(model, modelLength));
end;

procedure TDeclHandlerAdapter.externalEntityDecl(name: PSAXChar;
  nameLength: Integer; publicId: PSAXChar; publicIdLength: Integer;
  systemId: PSAXChar; systemIdLength: Integer);
begin
  IDeclHandler(Adapted).externalEntityDecl(
    PSAXCharToSAXString(name, nameLength),
    PSAXCharToSAXString(publicId, publicIdLength),
    PSAXCharToSAXString(systemId, systemIdLength));
end;

procedure TDeclHandlerAdapter.internalEntityDecl(name: PSAXChar;
  nameLength: Integer; value: PSAXChar; valueLength: Integer);
begin
  IDeclHandler(Adapted).internalEntityDecl(
    PSAXCharToSAXString(name, nameLength),
    PSAXCharToSAXString(value, valueLength));
end;

{ TLexicalHandlerAdapter }

procedure TLexicalHandlerAdapter.comment(ch: PSAXChar; chLength: Integer);
begin
  ILexicalHandler(Adapted).comment(PSAXCharToSAXString(ch, chLength));
end;

constructor TLexicalHandlerAdapter.create(
  const lexicalHandler: ILexicalHandler);
begin
  inherited Create(lexicalHandler);
end;

procedure TLexicalHandlerAdapter.endCDATA;
begin
  ILexicalHandler(Adapted).endCDATA();
end;

procedure TLexicalHandlerAdapter.endDTD;
begin
  ILexicalHandler(Adapted).endDTD();
end;

procedure TLexicalHandlerAdapter.endEntity(name: PSAXChar;
  nameLength: Integer);
begin
  ILexicalHandler(Adapted).endEntity(PSAXCharToSAXString(name, nameLength));
end;

procedure TLexicalHandlerAdapter.startCDATA;
begin
  ILexicalHandler(Adapted).startCDATA();
end;

procedure TLexicalHandlerAdapter.startDTD(name: PSAXChar;
  nameLength: Integer; publicId: PSAXChar; publicIdLength: Integer;
  systemId: PSAXChar; systemIdLength: Integer);
begin
  ILexicalHandler(Adapted).startDTD(
    PSAXCharToSAXString(name, nameLength),
    PSAXCharToSAXString(publicId, publicIdLength),
    PSAXCharToSAXString(systemId, systemIdLength));
end;

procedure TLexicalHandlerAdapter.startEntity(name: PSAXChar;
  nameLength: Integer);
begin
  ILexicalHandler(Adapted).startEntity(PSAXCharToSAXString(name, nameLength));
end;

{ TBufferedContentHandlerAdapter }

procedure TBufferedContentHandlerAdapter.characters(const ch: SAXString);
begin
  IBufferedContentHandler(Adapted).characters(PSAXChar(ch), Length(ch));
end;

constructor TBufferedContentHandlerAdapter.create(
  const contentHandler: IBufferedContentHandler; useAttributes2: Boolean);
begin
  inherited Create(contentHandler);
  if useAttributes2 then
    FAttributes:= TAttributes2Adapter.Create()
  else
    FAttributes:= TAttributesAdapter.Create();
  FAttributes._AddRef;
end;

destructor TBufferedContentHandlerAdapter.destroy;
begin
  FAttributes._Release;
  inherited;
end;

procedure TBufferedContentHandlerAdapter.endDocument;
begin
  IBufferedContentHandler(Adapted).endDocument();
end;

procedure TBufferedContentHandlerAdapter.endElement(const uri, localName,
  qName: SAXString);
begin
  IBufferedContentHandler(Adapted).endElement(PSAXChar(uri), Length(uri),
    PSAXChar(localName), Length(localName), PSAXCHar(qName), Length(qName));
end;

procedure TBufferedContentHandlerAdapter.endPrefixMapping(
  const prefix: SAXString);
begin
  IBufferedContentHandler(Adapted).endPrefixMapping(
    PSAXChar(prefix), Length(prefix));
end;

function TBufferedContentHandlerAdapter.getUseAttributes2: Boolean;
begin
  Result := FAttributes is TAttributes2Adapter;
end;

procedure TBufferedContentHandlerAdapter.ignorableWhitespace(
  const ch: SAXString);
begin
  IBufferedContentHandler(Adapted).ignorableWhitespace(
    PSAXChar(ch), Length(ch));
end;

procedure TBufferedContentHandlerAdapter.processingInstruction(
  const target, data: SAXString);
begin
  IBufferedContentHandler(Adapted).processingInstruction(PSAXChar(target),
    Length(target), PSAXChar(data), Length(data));
end;

procedure TBufferedContentHandlerAdapter.setDocumentLocator(
  const locator: ILocator);
begin
  IBufferedContentHandler(Adapted).setDocumentLocator(locator);
end;

procedure TBufferedContentHandlerAdapter.setUseAttributes2(Value: Boolean);
begin
  if Value and (FAttributes is TAttributesAdapter) then
  begin
    FAttributes._Release;
    FAttributes := TAttributes2Adapter.Create;
    FAttributes._AddRef;
  end
  else if not Value and (FAttributes is TAttributes2Adapter) then
  begin
    FAttributes._Release;
    FAttributes := TAttributesAdapter.Create;
    FAttributes._AddRef;
  end;
end;

procedure TBufferedContentHandlerAdapter.skippedEntity(
  const name: SAXString);
begin
  IBufferedContentHandler(Adapted).skippedEntity(PSAXChar(name), Length(name));
end;

procedure TBufferedContentHandlerAdapter.startDocument;
begin
  IBufferedContentHandler(Adapted).startDocument();
end;

procedure TBufferedContentHandlerAdapter.startElement(const uri, localName,
  qName: SAXString; const atts: IAttributes);
begin
  if (atts <> nil) then
  begin
    FAttributes.init(atts);
    IBufferedContentHandler(Adapted).startElement(PSAXChar(uri), Length(uri),
      PSAXChar(localName), Length(localName), PSAXChar(qName), Length(qName),
      FAttributes.getInterface());
  end else
  begin
    IBufferedContentHandler(Adapted).startElement(PSAXChar(uri), Length(uri),
      PSAXChar(localName), Length(localName), PSAXChar(qName), Length(qName), nil);
  end;
end;

procedure TBufferedContentHandlerAdapter.startPrefixMapping(const prefix,
  uri: SAXString);
begin
  IBufferedContentHandler(Adapted).startPrefixMapping(PSAXChar(prefix),
    Length(prefix), PSAXChar(uri), Length(uri));
end;

{ TBufferedDTDHandlerAdapter }

constructor TBufferedDTDHandlerAdapter.create(
  const dtdHandler: IBufferedDTDHandler);
begin
  inherited create(dtdHandler);
end;

procedure TBufferedDTDHandlerAdapter.notationDecl(const name, publicId,
  systemId: SAXString);
begin
  IBufferedDTDHandler(Adapted).notationDecl(
    PSAXChar(name), Length(name),
    PSAXChar(publicId), Length(publicId),
    PSAXChar(systemId), Length(systemId));
end;

procedure TBufferedDTDHandlerAdapter.unparsedEntityDecl(const name,
  publicId, systemId, notationName: SAXString);
begin
  IBufferedDTDHandler(Adapted).unparsedEntityDecl(
    PSAXChar(name), Length(name),
    PSAXChar(publicId), Length(publicId),
    PSAXChar(systemId), Length(systemId),
    PSAXChar(notationName), Length(notationName));
end;

{ TBufferedDeclHandlerAdapter }

procedure TBufferedDeclHandlerAdapter.attributeDecl(const eName, aName,
  attrType, mode, value: SAXString);
begin
  IBufferedDeclHandler(Adapted).attributeDecl(PSAXChar(eName), Length(eName),
    PSAXChar(aName), Length(aName), PSAXChar(attrType), Length(attrType),
    PSAXChar(mode), Length(mode), PSAXChar(value), Length(value));
end;

constructor TBufferedDeclHandlerAdapter.create(
  const declHandler: IBufferedDeclHandler);
begin
  inherited Create(declHandler);
end;

procedure TBufferedDeclHandlerAdapter.elementDecl(const name,
  model: SAXString);
begin
  IBufferedDeclHandler(Adapted).ElementDecl(
    PSAXChar(name), Length(name), PSAXChar(model), Length(model));
end;

procedure TBufferedDeclHandlerAdapter.externalEntityDecl(const name,
  publicId, systemId: SAXString);
begin
  IBufferedDeclHandler(Adapted).externalEntityDecl(
    PSAXChar(name), Length(name), PSAXChar(publicId), Length(publicId),
    PSAXChar(systemId), Length(systemId));
end;

procedure TBufferedDeclHandlerAdapter.internalEntityDecl(const name,
  value: SAXString);
begin
  IBufferedDeclHandler(Adapted).internalEntityDecl(
    PSAXChar(name), Length(name), PSAXChar(value), Length(value));
end;

{ TBufferedLexicalHandlerAdapter }

procedure TBufferedLexicalHandlerAdapter.comment(const ch: SAXString);
begin
  IBufferedLexicalHandler(Adapted).comment(PSAXChar(ch), Length(ch));
end;

constructor TBufferedLexicalHandlerAdapter.create(
  const lexicalHandler: IBufferedLexicalHandler);
begin
  inherited Create(lexicalHandler);
end;

procedure TBufferedLexicalHandlerAdapter.endCDATA;
begin
  IBufferedLexicalHandler(Adapted).endCDATA();
end;

procedure TBufferedLexicalHandlerAdapter.endDTD;
begin
  IBufferedLexicalHandler(Adapted).endDTD();
end;

procedure TBufferedLexicalHandlerAdapter.endEntity(const name: SAXString);
begin
  IBufferedLexicalHandler(Adapted).endEntity(PSAXChar(name), Length(name));
end;

procedure TBufferedLexicalHandlerAdapter.startCDATA;
begin
  IBufferedLexicalHandler(Adapted).startCDATA();
end;

procedure TBufferedLexicalHandlerAdapter.startDTD(const name, publicId,
  systemId: SAXString);
begin
  IBufferedLexicalHandler(Adapted).startDTD(PSAXChar(name), Length(name),
    PSAXChar(publicId), Length(publicId), PSAXChar(systemId), Length(systemId));
end;

procedure TBufferedLexicalHandlerAdapter.startEntity(const name: SAXString);
begin
  IBufferedLexicalHandler(Adapted).startEntity(PSAXChar(name), Length(name));
end;

{ TBufferedAttributesAdapter }

function TBufferedAttributesAdapter.getIndex(const uri,
  localName: SAXString): Integer;
begin
  Result:= FAttributes.getIndex(PSAXChar(uri), Length(uri),
    PSAXChar(localName), Length(localName));
end;

function TBufferedAttributesAdapter.getIndex(
  const qName: SAXString): Integer;
begin
  Result:= FAttributes.getIndex(PSAXChar(qName), Length(qName));
end;

function TBufferedAttributesAdapter.getInterface: IAttributes;
begin
  Result:= Self;
end;

function TBufferedAttributesAdapter.getLength: Integer;
begin
  Result:= FAttributes.getLength();
end;

function TBufferedAttributesAdapter.getLocalName(
  index: Integer): SAXString;
var
  locName: PSAXChar;
  locLen: Integer;
begin
  FAttributes.getLocalName(index, locName, locLen);
  Result:= PSAXCharToSAXString(locName, locLen);
end;

function TBufferedAttributesAdapter.getQName(index: Integer): SAXString;
var
  QName: PSAXChar;
  QLen: Integer;
begin
  FAttributes.getQName(index, QName, QLen);
  Result:= PSAXCharToSAXString(QName, QLen);
end;

function TBufferedAttributesAdapter.getType(const uri,
  localName: SAXString): SAXString;
var
  typeName: PSAXChar;
  typeLen: Integer;
begin
  FAttributes.getType(PSAXChar(uri), Length(uri),
    PSAXChar(localName), Length(localName), typeName, typeLen);
  Result:= PSAXCharToSAXString(typeName, typeLen);
end;

function TBufferedAttributesAdapter.getType(
  const qName: SAXString): SAXString;
var
  typeName: PSAXChar;
  typeLen: Integer;
begin
  FAttributes.getType(PSAXChar(qName), Length(qName), typeName, typeLen);
  Result:= PSAXCharToSAXString(typeName, typeLen);
end;

function TBufferedAttributesAdapter.getType(index: Integer): SAXString;
var
  typeName: PSAXChar;
  typeLen: Integer;
begin
  FAttributes.getType(index, typeName, typeLen);
  Result:= PSAXCharToSAXString(typeName, typeLen);
end;

function TBufferedAttributesAdapter.getURI(index: Integer): SAXString;
var
  uri: PSAXChar;
  uriLen: Integer;
begin
  FAttributes.getURI(index, uri, uriLen);
  Result:= PSAXCharToSAXString(uri, uriLen);
end;

function TBufferedAttributesAdapter.getValue(
  const qName: SAXString): SAXString;
var
  value: PSAXChar;
  valueLen: Integer;
begin
  FAttributes.getValue(PSAXChar(qName), Length(qName), value, valueLen);
  Result:= PSAXCharToSAXString(value, valueLen);
end;

function TBufferedAttributesAdapter.getValue(const uri,
  localName: SAXString): SAXString;
var
  value: PSAXChar;
  valueLen: Integer;
begin
  FAttributes.getValue(PSAXChar(uri), Length(uri), PSAXChar(localName),
    Length(localName), value, valueLen);
  Result:= PSAXCharToSAXString(value, valueLen);
end;

function TBufferedAttributesAdapter.getValue(index: Integer): SAXString;
var
  value: PSAXChar;
  valueLen: Integer;
begin
  FAttributes.getValue(index, value, valueLen);
  Result:= PSAXCharToSAXString(value, valueLen);
end;

procedure TBufferedAttributesAdapter.init(
  const bufferedAttributes: IBufferedAttributes);
begin
  FAttributes:= bufferedAttributes;
end;

{ TBufferedAttributes2Adapter }

function TBufferedAttributes2Adapter.getInterface: IAttributes;
begin
  Result:= IAttributes2(Self);
end;

function TBufferedAttributes2Adapter.isDeclared(index: Integer): Boolean;
begin
  Result:= IBufferedAttributes2(FAttributes).IsDeclared(index);
end;

function TBufferedAttributes2Adapter.isDeclared(const uri,
  localName: SAXString): Boolean;
begin
  Result:= IBufferedAttributes2(FAttributes).IsDeclared(PSAXChar(uri),
    Length(uri), PSAXChar(localName), Length(localName));
end;

function TBufferedAttributes2Adapter.isDeclared(
  const qName: SAXString): Boolean;
begin
  Result:= IBufferedAttributes2(FAttributes).IsDeclared(
    PSAXChar(qName), Length(qName));
end;

function TBufferedAttributes2Adapter.isSpecified(index: Integer): Boolean;
begin
  Result:= IBufferedAttributes2(FAttributes).IsSpecified(index);
end;

function TBufferedAttributes2Adapter.isSpecified(const uri,
  localName: SAXString): Boolean;
begin
  Result:= IBufferedAttributes2(FAttributes).IsSpecified(PSAXChar(uri),
    Length(uri), PSAXChar(localName), Length(localName));
end;

function TBufferedAttributes2Adapter.isSpecified(
  const qName: SAXString): Boolean;
begin
  Result:= IBufferedAttributes2(FAttributes).IsSpecified(
    PSAXChar(qName), Length(qName));
end;


end.
