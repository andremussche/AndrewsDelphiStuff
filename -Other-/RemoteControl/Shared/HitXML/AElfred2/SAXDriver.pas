// Filename : SAXDriver.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt

unit SAXDriver;

// "!!"
// "@@"

interface

{$I SAX.INC}

uses
{$IFDEF DELPHI6_UP}
  Variants,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, Contnrs, SAX, SAXExt, SAXHelpers, Dialogs, Hashtable;


type

  TBooleanDynarray = array of Boolean;
  TByteDynarray = array of Byte;
  TPointerDynarray = array of Pointer;

  EIllegalStateException = class(Exception);
  EEOFException = class(Exception);
  ECharConversionException = class(Exception);
  EUnsupportedEncodingException = class(Exception);
  ERuntimeException = class(Exception);
  EEmptyStackException = class(Exception);

  IAElfredDeclHandlerProperty = interface(IInterfaceProperty)
    ['{C942CA8F-AC48-4A1A-A1B2-49F0E7C8DE8E}']
  end;

  IAElfredLexicalHandlerProperty = interface(IInterfaceProperty)
    ['{035FA8DA-0072-4ADE-A4CD-33876CC93422}']
  end;


  TSAXDriver = class(TComponent, IUnknown, ILocator, IAttributes2, IXMLReader,
    IAElfredDeclHandlerProperty, IAElfredLexicalHandlerProperty)
  private
    base : IUnknown;
    parser : TObject;
    entityResolver : IEntityResolver;
    resolver2 : IEntityResolver2;
    contentHandler : IContentHandler;
    dtdHandler : IDTDHandler;
    errorHandler : IErrorHandler;
    declHandler : IDeclHandler;
    lexicalHandler : ILexicalHandler;
    elementName : SAXString;
    entityStack : TStack;
    attributeNames : SAXStringArray;
    attributeNamespaces : SAXStringArray;
    attributeLocalNames : SAXStringArray;
    attributeValues : SAXStringArray;
    attributeSpecified : TBooleanDynarray;
    attributeDeclared : TBooleanDynarray;
    namespaces : Boolean;
    xmlNames : Boolean;
    extGE : Boolean;
    extPE : Boolean;
    resolveAll : Boolean;
    useResolver2 : Boolean;
    attributes : Boolean;
    attributeCount  : Integer;
    nsTemp : TNamespaceParts;
    prefixStack : TNamespaceSupport;
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
    procedure declarePrefix(const prefix, uri: SAXString);
  protected
    refCount : Integer;
    ownerIsComponent : Boolean;
    function getDeclHandler() : IDeclHandler;
    function resolveURIs : Boolean;
    function absolutize(const baseURI, systemId: SAXString;
      nice: Boolean): SAXString;
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
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function getEntityResolver() : IEntityResolver;
    procedure setEntityResolver(const resolver : IEntityResolver);
    function getDTDHandler() : IDTDHandler;
    procedure setDTDHandler(const handler: IDTDHandler);
    function getContentHandler() : IContentHandler;
    procedure setContentHandler(const handler: IContentHandler);
    function getErrorHandler() : IErrorHandler;
    procedure setErrorHandler(const handler: IErrorHandler);
    procedure parseDTD(const source : IInputSource); overload;
    procedure parseDTD(const systemId : SAXString); overload;
    procedure parse(const source : IInputSource); overload;
    procedure parse(const systemId : SAXString); overload;
    function getFeature(const featureId : SAXString) : Boolean;
    procedure setFeature(const featureId : SAXString; value : Boolean);
    function getProperty(const propertyId : SAXString) : IProperty;
    procedure startDocument();
    procedure skippedEntity(const name : SAXString);
    function getExternalSubset(const name, baseURI : SAXString) : IInputSource;
    function resolveEntity(isPE : Boolean; const name: SAXString;
      inp : IInputSource; const baseURI : SAXString): IInputSource;
    procedure startExternalEntity(const name, systemId : SAXString;
      stackOnly : Boolean);
    procedure endExternalEntity(const name : SAXString);
    procedure startInternalEntity(const name : SAXString);
    procedure endInternalEntity(const name : SAXString);
    procedure doctypeDecl(const name, publicId, systemId : SAXString);
    procedure notationDecl(const name : SAXString; const ids : SAXStringArray);
    procedure unparsedEntityDecl(const name : SAXString;
      const ids : SAXStringArray; const notation : SAXString);
    procedure endDoctype();
    procedure attribute(const qname, value : SAXString; const isSpecified, isDeclared : Boolean);
    procedure startElement(const elname : SAXString);
    procedure endElement(const elname : SAXString);
    procedure startCDATA();
    procedure charData(const ch : SAXCharArray; const start, length : Integer);
    procedure endCDATA();
    procedure ignorableWhitespace(const ch : SAXCharArray; const start, length : Integer);
    procedure processingInstruction(const target, data : SAXString);
    procedure comment(const ch : SAXCharArray; const start, length : Integer);
    procedure fatal(const _message : SAXString);
    procedure verror(const _message : SAXString);
    procedure warn(const _message : SAXString);
    // IAttributes
    function getLength() : Integer;
    function getURI(index : Integer) : SAXString;
    function getLocalName(index : Integer) : SAXString;
    function getQName(index : Integer) : SAXString;
    function getType(index : Integer) : SAXString; overload;
    function getValue(index : Integer) : SAXString; overload;
    function getIndex(const uri, local : SAXString) : Integer; overload;
    function getIndex(const xmlName : SAXString) : Integer; overload;
    function getType(const uri, local : SAXString) : SAXString; overload;
    function getType(const xmlName : SAXString) : SAXString; overload;
    function getValue(const uri, local : SAXString) : SAXString; overload;
    function getValue(const xmlName : SAXString) : SAXString; overload;
    function isSpecified(index : Integer) : Boolean; overload;
    function isSpecified(const uri, local : SAXString) : Boolean; overload;
    function isSpecified(const qName : SAXString) : Boolean; overload;
    function isDeclared(index : Integer) : Boolean; overload;
    function isDeclared(const uri, local : SAXString) : Boolean; overload;
    function isDeclared(const qName : SAXString) : Boolean; overload;
    // ILocator
    function getPublicId() : PSAXChar;
    function getSystemId() : PSAXChar;
    function getLineNumber() : Integer;
    function getColumnNumber() : Integer;
  end;

// Internal Wrapper Types

type

  TSAXElementWrapper = class
    contentType : Byte;
    contentModel : SAXString;
    attList : THashtable;
    constructor create;
    destructor Destroy; override;
  end;

  TSAXAttributeWrapper = class
    _type : SAXString;
    value : SAXString;
    valueType : Byte;
    enumeration : SAXString;
    expandedValue : SAXString;
    constructor create;
    destructor Destroy; override;
  end;

  TSAXEntityWrapper = class
    eClass : Byte;
    ids : SAXStringArray;
    value : SAXString;
    nName : SAXString;
    constructor create;
    destructor Destroy; override;
  end;

  TSAXNotationWrapper = class
    pubid : SAXString;
    sysid : SAXString;
    constructor create;
    destructor Destroy; override;
  end;

  TSAXInputWrapper = class
    sourceType : Byte;
    externalEntity : TStream;
    readBuffer : SAXCharArray;
    readBufferPos : Integer;
    readBufferLength : Integer;
    line : Integer;
    encoding : Integer;
    readBufferOverflow : Integer;
    stream : TStream;
    currentByteCount : Integer;
    column : Integer;
    constructor create;
    destructor Destroy; override;
  end;

  TSAXStringWrapper = class
    Value : SAXString;
    constructor create(s : SAXString);
    destructor Destroy; override;
  end;

  TSAXStringArrayWrapper = class
    Value : SAXStringArray;
    constructor create(s : SAXStringArray);
    destructor Destroy; override;
  end;

  procedure DeleteStringArrayBlock(var A : SAXStringArray; FromIndex, ToIndex : Integer);
  procedure arrayCopy(Source, Dest : SAXCharArray; const start, len, destStart : Integer);

{$IFDEF DEBUG}
var hCreated, hFreed : Integer;
{$ENDIF}

implementation

uses XmlParser;

type

  TSAXStack = class(TStack)
  public
    property List;
  end;

  procedure DeleteStringArrayBlock(var A : SAXStringArray; FromIndex, ToIndex : Integer);
  var Source : Pointer;
      Dest : Pointer;
      FromBytes, ToBytes, TotalBytes, NewLen, I : Integer;
  begin
    for I:= FromIndex to ToIndex do
     A[I]:= '';

    Dest:= Pointer(A);
    Source:= Pointer(A);
    FromBytes:= SizeOf(SAXString) * FromIndex;
    ToBytes:= SizeOf(SAXString) * (ToIndex + 1);
    TotalBytes:= SizeOf(SAXString) * (Length(A) - ToIndex);
    NewLen:= Length(A) - ((ToIndex - FromIndex) + 1);
    Inc(Cardinal(Dest), FromBytes);
    Inc(Cardinal(Source), ToBytes);
    Move(Source^, Dest^, TotalBytes);
    SetLength(A, NewLen);
  end;

  procedure arrayCopy(Source, Dest : SAXCharArray; const start, len, destStart : Integer);
  var PSource : Pointer;
      PDest : Pointer;
      FromBytes, ToBytes, TotalBytes : Integer;
  begin
    //Expand the buffer
    if (Length(Dest) < (destStart + len)) then
      SetLength(Dest, destStart + len);

    PDest:= Pointer(Dest);
    PSource:= Pointer(Source);
    FromBytes:= SizeOf(SAXChar) * start;
    ToBytes:= SizeOf(SAXChar) * destStart;
    TotalBytes:= SizeOf(SAXChar) * len;
    Inc(Cardinal(PSource), FromBytes);
    Inc(Cardinal(PDest), ToBytes);
    Move(PSource^, PDest^, TotalBytes);
  end;


{ TSAXDriver }

constructor TSAXDriver.Create(AOwner : TComponent);
begin
{$IFDEF DEBUG}
Inc(hcreated);
{$ENDIF}
  inherited Create(AOwner);
  base:= TDefaultHandler2.Create as IUnknown;
  entityResolver:= base as IEntityResolver;
  resolver2:= nil;
  contentHandler:= base as IContentHandler;
  dtdHandler:= base as IDTDHandler;
  errorHandler:= base as IErrorHandler;
  declHandler:= base as IDeclHandler;
  lexicalHandler:= base as ILexicalHandler;
  entityStack:= TSAXStack.Create();
  namespaces:= true;
  xmlNames:= false;
  extGE:= true;
  extPE:= true;
  resolveAll:= true;
  useResolver2:= true;
  attributeCount:= 0;
end;

destructor TSAXDriver.Destroy;
var i : Integer;
begin
{$IFDEF DEBUG}
Inc(hfreed);
{$ENDIF}
  // Don't free base <g/>
  entityResolver:= nil;
  contentHandler:= nil;
  dtdHandler:= nil;
  errorHandler:= nil;
  declHandler:= nil;
  lexicalHandler:= nil;
  for i:= 0 to entityStack.Count-1 do
    TSAXStringWrapper(TSAXStack(entityStack).List[i]).Free;
  entityStack.Free;
  prefixStack.Free;
  inherited Destroy;
end;

procedure TSAXDriver.AfterConstruction;
begin
  inherited;
  ownerIsComponent:= (Owner <> nil) and (Owner is TComponent);
  InterlockedDecrement(refCount);
end;

procedure TSAXDriver.BeforeDestruction;
begin
  inherited;
end;

class function TSAXDriver.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TSAXDriver(Result).refCount:= 1;
end;

function TSAXDriver._AddRef: Integer;
begin
  Result:= InterlockedIncrement(refCount);
end;

function TSAXDriver._Release: Integer;
begin
  Result:= InterlockedDecrement(refCount);
  if (Result = 0) and (not ownerIsComponent) then
    Destroy;
end;

function TSAXDriver.getEntityResolver: IEntityResolver;
begin
  if (entityResolver = base) then
    Result:= nil
  else
    Result:= entityResolver;
  Exit;
end;

procedure TSAXDriver.setEntityResolver(const resolver: IEntityResolver);
var r2 : IEntityResolver2;
begin
  if (Supports(resolver, IEntityResolver2, r2)) then
    resolver2:= r2
  else
    resolver2:= nil;

  if (resolver = nil) then
    entityResolver:= base as IEntityResolver
  else
    entityResolver:= resolver;
end;

function TSAXDriver.getDTDHandler: IDTDHandler;
begin
  if (dtdHandler = base) then
    Result:= nil
  else
    Result:= dtdHandler;
  Exit;
end;

procedure TSAXDriver.setDTDHandler(const handler: IDTDHandler);
begin
  if (handler = nil) then
    dtdHandler:= base as IDTDHandler
  else
    dtdHandler:= handler;
end;

function TSAXDriver.getContentHandler: IContentHandler;
begin
  if (contentHandler = base) then
    Result:= nil
  else
    Result:= contentHandler;
  Exit;
end;

procedure TSAXDriver.setContentHandler(const handler: IContentHandler);
begin
  if (handler = nil) then
    contentHandler:= base as IContentHandler
  else
    contentHandler:= handler;
end;

function TSAXDriver.getErrorHandler: IErrorHandler;
begin
  if (errorHandler = base) then
    Result:= nil
  else
    Result:= errorHandler;
  Exit;
end;

procedure TSAXDriver.setErrorHandler(const handler: IErrorHandler);
begin
  if (handler = nil) then
    errorHandler:= base as IErrorHandler
  else
    errorHandler:= handler;
end;

procedure TSAXDriver.parse(const source: IInputSource);
var i : Integer;
    isis : IStreamInputSource;
    stream : TStream;
begin
  parser:= TXmlParser.Create();
  if (namespaces) then
    prefixStack:= TNamespaceSupport.Create();
  TXmlParser(parser).setHandler(self);
  try
    attributeCount:= 0;

    try
      if (Supports(source, IStreamInputSource, isis)) then
        stream:= isis.getByteStream()
      else
        stream:= nil;

      TXmlParser(parser).doParse(
              source.getSystemId(),
              source.getPublicId(),
              stream,
              source.getEncoding());
    except
      on s : ESAXException do
      begin
        raise;
      end;
      on e : Exception do
      begin
        raise;
      end;
    end;
  finally
    contentHandler.endDocument();
    for i:= 0 to entityStack.Count-1 do
      TSAXStringWrapper(TSAXStack(entityStack).List[i]).Free;
    TSAXStack(entityStack).List.Clear();
    attributeNames:= nil;
    attributeNamespaces:= nil;
    attributeLocalNames:= nil;
    attributeValues:= nil;
    nsTemp[nsURI]:= '';
    nsTemp[nsLocal]:= '';
    nsTemp[nsQName]:= '';
    TXmlParser(parser).Free;
    parser:= nil;
    prefixStack.Free;
    prefixStack:= nil;
  end;
end;

procedure TSAXDriver.parse(const systemId: SAXString);
var input : IInputSource;
begin
  input:= TInputSource.Create(systemId) as IInputSource;
  try
    parse(input);
  finally
    input:= nil;
  end;
end;

procedure TSAXDriver.parseDTD(const source: IInputSource);
var i : Integer;
    isis : IStreamInputSource;
    stream : TStream;
begin
  parser:= TXmlParser.Create();
  if (namespaces) then
    prefixStack:= TNamespaceSupport.Create();
  TXmlParser(parser).setHandler(self);
  try
    attributeCount:= 0;

    try
      if (Supports(source, IStreamInputSource, isis)) then
        stream:= isis.getByteStream()
      else
        stream:= nil;

      TXmlParser(parser).doParseDTD(
              source.getSystemId(),
              source.getPublicId(),
              stream,
              source.getEncoding());
    except
      on e : ESAXException do
      begin
        raise;
      end;
      on e : Exception do
      begin
        raise ESAXParseException.Create(e.Message);
      end;
    end;
  finally
    for i:= 0 to entityStack.Count-1 do
      TSAXStringWrapper(TSAXStack(entityStack).List[i]).Free;
    TSAXStack(entityStack).List.Clear();
    attributeNames:= nil;
    attributeNamespaces:= nil;
    attributeLocalNames:= nil;
    attributeValues:= nil;
    nsTemp[nsURI]:= '';
    nsTemp[nsLocal]:= '';
    nsTemp[nsQName]:= '';
    TXmlParser(parser).Free;
    parser:= nil;
    prefixStack.Free;
    prefixStack:= nil;
  end;
end;

procedure TSAXDriver.parseDTD(const systemId: SAXString);
var input : IInputSource;
begin
  input:= TInputSource.Create(systemId) as IInputSource;
  try
    parseDTD(input);
  finally
    input:= nil;
  end;
end;

function TSAXDriver.getFeature(const featureId: SAXString): Boolean;
begin
  if (ValidationFeature = featureId) then
  begin
    Result:= false;
    Exit;
  end;
  // external entities (both types) are optionally included
  if (ExternalGeneralFeature = featureId) then
  begin
    Result:= extGE;
    Exit;
  end;
  if (ExternalParameterFeature = featureId) then
  begin
    Result:= extPE;
    Exit;
  end;

  // element/attribute names are as written in document; no mangling
  if (NamespacePrefixesFeature = featureId) then
  begin
    Result:= xmlNames;
    Exit;
  end;

  // report element/attribute namespaces?
  if (NamespacesFeature = featureId) then
  begin
    Result:= namespaces;
    Exit;
  end;

  // all PEs and GEs are reported
  if (LexicalParameterFeature = featureId) then
  begin
    Result:= true;
    Exit;
  end;

  // never interns
  if ((Features + 'string-interning') = (featureId)) then
  begin
//!!    Result:= true;
    Result:= false;
    Exit;
  end;

  // EXTENSIONS 1.1

  // always returns isSpecified info
  if (UseAttributes2Feature = featureId) then
  begin
    Result:= true;
    Exit;
  end;

  // meaningful between startDocument/endDocument
  if (IsStandaloneFeature = featureId) then
  begin
    if (parser = nil) then
      raise ESAXNotSupportedException.Create(featureId);
    Result:= TXmlParser(parser).isStandalone();
    Exit;
  end;

  // optionally don't absolutize URIs in declarations
  if (ResolveDTDURIsFeature = featureId) then
  begin
    Result:= resolveAll;
    Exit;
  end;

  // optionally use resolver2 interface methods, if possible
  if (UseEntityResolver2Feature = featureId) then
  begin
    Result:= useResolver2;
    Exit;
  end;

  raise ESAXNotRecognizedException.Create(featureId);
end;

function TSAXDriver.getDeclHandler() : IDeclHandler;
begin
  Result:= declHandler;
  Exit;
end;

function TSAXDriver.resolveURIs : Boolean;
begin
  Result:= resolveAll;
  Exit;
end;

function TSAXDriver.getProperty(const propertyId: SAXString): IProperty;
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

  // unknown properties
  raise ESAXNotRecognizedException.Create(propertyId);
end;

procedure TSAXDriver.setFeature(const featureId: SAXString;
  value: Boolean);
var state : Boolean;
begin
  // Features with a defined value, we just change it if we can.
  state:= getFeature(featureId);

  if (state = value) then
    Exit;

  if (parser <> nil) then
    raise ESAXNotSupportedException.Create('not while parsing');

  if (NamespacePrefixesFeature = featureId) then
  begin
    // in this implementation, this only affects xmlns reporting
    xmlNames:= value;
    // forcibly prevent illegal parser state
    if (not xmlNames) then
      namespaces:= true;
    Exit;
  end;

  if (NamespacesFeature = featureId) then
  begin
    namespaces:= value;
    // forcibly prevent illegal parser state
    if (not namespaces) then
      xmlNames:= true;
    Exit;
  end;

  if (ExternalGeneralFeature = featureId) then
  begin
    extGE:= value;
    Exit;
  end;

  if (ExternalParameterFeature = featureId) then
  begin
    extPE:= value;
    Exit;
  end;

  if (ResolveDTDURIsFeature = featureId) then
  begin
    resolveAll:= value;
    Exit;
  end;

  if (UseEntityResolver2Feature = featureId) then
  begin
    useResolver2:= value;
    Exit;
  end;

  raise ESAXNotSupportedException.Create(featureId);
end;

procedure TSAXDriver.startDocument;
begin
  contentHandler.setDocumentLocator(self);
  contentHandler.startDocument();
  SetLength(attributeNames, 0);
  SetLength(attributeValues, 0);
end;

procedure TSAXDriver.skippedEntity(const name : SAXString);
begin
  contentHandler.skippedEntity(name);
end;

function TSAXDriver.getExternalSubset(const name,
  baseURI : SAXString) : IInputSource;
begin
  if ((resolver2 = nil) or (not useResolver2) or (not extPE)) then
  begin
    Result:= nil;
    Exit;
  end;
  Result:= resolver2.getExternalSubset(name, baseURI);
end;

function TSAXDriver.resolveEntity(isPE : Boolean; const name: SAXString;
  inp : IInputSource; const baseURI : SAXString): IInputSource;
var source : IInputSource;
begin

  // external entities might be skipped
  if ((isPE) and (not extPE)) then
  begin
    Result:= nil;
    Exit;
  end;

  if ((not isPE) and (not extGE)) then
  begin
    Result:= nil;
    Exit;
  end;

  // ... or not
  lexicalHandler.startEntity(name);
  if ((resolver2 <> nil) and (useResolver2)) then
  begin
    source:= resolver2.resolveEntity(name, inp.getPublicId(), baseURI,
      inp.getSystemId());
    if (source = nil) then
    begin
      inp.setSystemId(absolutize(baseURI, inp.getSystemId(), false));
      source:= inp;
    end;
  end else
  begin
    inp.setSystemId(absolutize(baseURI, inp.getSystemId(), false));
    source:= entityResolver.resolveEntity(inp.getPublicId(), inp.getSystemId());
    if (source = nil) then
      source:= inp;
  end;
  startExternalEntity(name, source.getSystemId(), true);
  Result:= source;
end;

function TSAXDriver.absolutize(const baseURI, systemId: SAXString;
  nice : Boolean): SAXString;
var P : Integer;
begin
  // FIXME normalize system IDs -- when?
  // - Convert to UTF-8
  // - Map reserved and non-ASCII characters to %HH

  // !! Cheating on this function...
  P:= Pos(':', systemId);

  if (baseURI = '') then
  begin
    warn('No base URI; SYSTEM id must be absolute: ' + systemId);
    Result:= systemId;
  end else
  begin
    if (P > 0) then
      Result:= systemId
    else begin
      Result:= ExtractFilePath(StringReplace(baseURI, '/', '\', [rfReplaceAll])) + systemId;
    end;
  end;
end;

procedure TSAXDriver.startExternalEntity(const name, systemId: SAXString;
  stackOnly : Boolean);
begin
  if (systemId = '') then
    warn('URI was not reported to parser for entity ' + name);
  if (not stackOnly) then  // spliced [dtd] needs startEntity
    lexicalHandler.startEntity(name);

  entityStack.push(TSAXStringWrapper.Create(systemId));
end;

procedure TSAXDriver.endExternalEntity(const name: SAXString);
var w : TSAXStringWrapper;
begin
  if (not ('[document]' = name)) then
    lexicalHandler.endEntity(name);
  w:= TSAXStringWrapper(entityStack.pop());
  w.Free;
end;

procedure TSAXDriver.startInternalEntity(const name: SAXString);
begin
  lexicalHandler.startEntity(name);
end;

procedure TSAXDriver.endInternalEntity(const name: SAXString);
begin
  lexicalHandler.endEntity(name);
end;

procedure TSAXDriver.doctypeDecl(const name, publicId,
  systemId: SAXString);
begin
  lexicalHandler.startDTD(name, publicId, systemId);

  // ... the "name" is a declaration and should be given
  // to the DeclHandler(but sax2 doesn't).

  // the IDs for the external subset are lexical details,
  // as are the contents of the internal subset; but sax2
  // doesn't provide the internal subset "pre-parse"
end;

procedure TSAXDriver.notationDecl(const name : SAXString;
  const ids : SAXStringArray);
begin
  try
    if ((resolveAll) and (ids[1] <> '')) then
      dtdHandler.notationDecl(name, ids[0], absolutize(ids[2], ids[1], true))
    else
      dtdHandler.notationDecl(name, ids[0], ids [1]);
  except
    on e : Exception do
    begin
      // "can't happen"
      raise ESAXParseException.Create(e.message);
    end;
  end;
end;

procedure TSAXDriver.unparsedEntityDecl(const name : SAXString;
  const ids : SAXStringArray; const notation : SAXString);
begin
  try
    if (resolveAll) then
      dtdHandler.unparsedEntityDecl (name, ids[0], absolutize(ids[2], ids[1], true), notation)
    else
      dtdHandler.unparsedEntityDecl (name, ids[0], ids[1], notation)
  except
    on e : Exception do
    begin
      // "can't happen"
      raise ESAXParseException.Create(e.message);
    end;
  end;
end;

procedure TSAXDriver.endDoctype;
begin
  lexicalHandler.endDTD();
end;

procedure TSAXDriver.declarePrefix(const prefix, uri : SAXString);
var index : Integer;
begin
  index:= Pos(':', uri);

  // many versions of nwalsh docbook stylesheets
  // have bogus URLs; so this can't be an error...
  if ((index < 1) and (Length(uri) <> 0)) then
    warn('relative URI for namespace: ' + uri);

  // FIXME:  char [0] must be ascii alpha; chars [1..index]
  // must be ascii alphanumeric or in "+-." [RFC 2396]
  prefixStack.declarePrefix(prefix, uri);
  contentHandler.startPrefixMapping(prefix, uri);
end;

procedure TSAXDriver.attribute(const qname, value: SAXString;
  const isSpecified, isDeclared: Boolean);
var prefix : SAXString;
begin
  if (not attributes) then
  begin
    attributes:= true;
    if (namespaces) then
      prefixStack.pushContext();
  end;

  // process namespace decls immediately
  // then maybe forget this as an attribute
  if (namespaces) then
  begin
    // default NS declaration?
    if ('xmlns' = qname) then
    begin
      declarePrefix('', value);
      if (not xmlNames) then
        Exit;
    end
    // NS prefix declaration?
    else if (Pos(':', qname) = 6) and (Pos('xmlns', qname) = 1) then
    begin
      prefix:= Copy(qname, 7, length(qname));
      if (length(value) = 0) then
      begin
        verror('missing URI in namespace decl attribute: ' + qname);
      end else
        declarePrefix(prefix, value);
      if (not xmlNames) then
        Exit;
    end;
  end;

  // remember this attribute ...
  if (attributeCount = Length(attributeSpecified)) then // grow array?
  begin
    SetLength(attributeSpecified, Length(attributeSpecified) + 5);
    SetLength(attributeDeclared, Length(attributeDeclared) + 5);
  end;
  attributeSpecified[attributeCount]:= isSpecified;
  attributeDeclared[attributeCount]:= isDeclared;

  Inc(attributeCount);

  SetLength(attributeNames, Length(attributeNames) + 1);
  attributeNames[Length(attributeNames)-1]:= qname;

  // attribute type comes from querying parser's DTD records
  SetLength(attributeValues, Length(attributeValues) + 1);
  attributeValues[Length(attributeValues)-1]:= value;

  // ... patching {lname, uri} later, if needed
  SetLength(attributeNamespaces, Length(attributeNamespaces) + 1);
  attributeNamespaces[Length(attributeNamespaces)-1]:= '';
  SetLength(attributeLocalNames, Length(attributeLocalNames) + 1);
  attributeLocalNames[Length(attributeLocalNames)-1]:= '';
end;

procedure TSAXDriver.startElement(const elname: SAXString);
var handler : IContentHandler;
    i, index : Integer;
    qname : SAXString;
    parts : TNamespaceParts;
begin
  handler:= contentHandler;

  //
  // NOTE:  this implementation of namespace support adds something
  // like six percent to parsing CPU time, in a large(~50 MB)
  // document that doesn't use namespaces at all. (Measured by PC
  // sampling, with a bug where endElement processing was omitted.)
  //

  if (not attributes) then
  begin
    if (namespaces) then
      prefixStack.pushContext()
  end
  else if (namespaces) then
  begin
    // now we can patch up namespace refs; we saw all the
    // declarations, so now we'll do the Right Thing
    for i:= 0 to attributeCount-1 do
    begin
      qname:= attributeNames[i];
      // default NS declaration?
      if ('xmlns' = qname) then
        continue;

      index:= Pos(':', qname);

      // NS prefix declaration?
      if ((index = 6) and (Pos('xmlns', qname) = 1)) then
        continue;


      // it's not a NS decl; patch namespace info items
      parts:= prefixStack.processName(qname, nsTemp, true);
      if (parts[nsURI] = '') and
         (parts[nsLocal] = '') and
         (parts[nsQName] = '') then
      begin
        verror('undeclared attribute prefix in: ' + qname);
      end else
      begin
        attributeNamespaces[i]:= nsTemp[nsURI];
        attributeLocalNames[i]:= nsTemp[nsLocal];
      end;
    end;
  end;

  // save element name so attribute callbacks work
  elementName:= elname;
  if (namespaces) then
  begin
    parts:= prefixStack.processName(elname, nsTemp, false);
    if (parts[nsURI] = '') and
       (parts[nsLocal] = '') and
       (parts[nsQName] = '') then
    begin
      verror('undeclared element prefix in: ' + elname);
      nsTemp[nsURI]:= '';
      nsTemp[nsLocal]:= '';
    end;
    handler.startElement(nsTemp[nsURI], nsTemp[nsLocal], elname, IAttributes2(Self));
  end else
    handler.startElement('', '', elname, IAttributes2(Self));
  // elementName:= null;

  // elements with no attributes are pretty common!
  if (attributes) then
  begin
    SetLength(attributeNames, 0);
    SetLength(attributeNamespaces, 0);
    SetLength(attributeLocalNames, 0);
    SetLength(attributeValues, 0);
    attributeCount:= 0;
    attributes:= False;
  end;
end;

procedure TSAXDriver.endElement(const elname: SAXString);
var handler : IContentHandler;
    i : Integer;
    prefixes : SAXStringArray;
begin
  prefixes:= nil;

  handler:= contentHandler;

  if (not namespaces) then
  begin
    handler.endElement('', '', elname);
    Exit;
  end;
  prefixStack.processName(elname, nsTemp, false);
  handler.endElement(nsTemp[nsURI], nsTemp[nsLocal], elname);

  prefixes:= prefixStack.getDeclaredPrefixes();

  for i:= 0 to Length(prefixes)-1 do
  begin
    handler.endPrefixMapping(prefixes[i]);
  end;

  prefixStack.popContext();
end;

procedure TSAXDriver.startCDATA;
begin
  lexicalHandler.startCDATA();
end;

procedure TSAXDriver.charData(const ch: SAXCharArray; const start,
  length: Integer);
var S : SAXString;
begin
  SAXCharArrayToSAXString(Copy(ch, start, length), S);
  contentHandler.characters(S);
end;

procedure TSAXDriver.endCDATA;
begin
  lexicalHandler.endCDATA();
end;

procedure TSAXDriver.ignorableWhitespace(const ch: SAXCharArray;
  const start, length: Integer);
var P : PSAXChar;
begin
  P:= Pointer(ch);
  Inc(Cardinal(P), start * SizeOf(SAXChar));
  contentHandler.ignorableWhitespace(PSAXCharToSAXString(P, length));
end;

procedure TSAXDriver.processingInstruction(const target, data: SAXString);
begin
  contentHandler.processingInstruction(target, data);
end;

procedure TSAXDriver.comment(const ch: SAXCharArray; const start,
  length: Integer);
var P : PSAXChar;
begin
  P:= Pointer(ch);
  Inc(Cardinal(P), start * SizeOf(SAXChar));
  if (lexicalHandler <> (base as ILexicalHandler)) then
    lexicalHandler.comment(PSAXCharToSAXString(P, length));
end;

procedure TSAXDriver.fatal(const _message: SAXString);
begin
  errorHandler.fatalError(TSAXParseError.create(PSAXChar(_message), Self) as ISAXParseError);
  // Even if the application can continue ... we can't!
  raise ESAXParseException.Create(_message, Self);
end;

// We can safely report a few validity errors that
// make layered SAX2 DTD validation more conformant
procedure TSAXDriver.verror(const _message: SAXString);
var err : ISAXParseError;
begin
  err:= TSAXParseError.create(PSAXChar(_message), Self) as ISAXParseError;
  errorHandler.error(err);
end;

procedure TSAXDriver.warn(const _message: SAXString);
var err : ISAXParseError;
begin
  err:= TSAXParseError.create(PSAXChar(_message), Self) as ISAXParseError;
  errorHandler.warning(err);
end;

function TSAXDriver.QueryInterfaceDeclHandler(const IID: TGUID;
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

function TSAXDriver.getDeclHandlerPropertyName : SAXString;
begin
  Result:= DeclHandlerProperty;
end;

function TSAXDriver.getDeclHandlerPropertyValue : IUnknown;
begin
  if (declHandler = base) then
    Result:= nil
  else
    Result:= declHandler;
  Exit;
end;

procedure TSAXDriver.setDeclHandlerPropertyValue(const value : IUnknown);
var handler : IDeclHandler;
begin
  if (value = nil) then
  begin
    declHandler:= base as IDeclHandler;
  end else if (not Supports(value, IDeclHandler, handler)) then
    raise ESAXNotSupportedException.Create(DeclHandlerProperty)
  else
    declHandler:= IDeclHandler(value);
end;

function TSAXDriver.QueryInterfaceLexicalHandler(const IID: TGUID;
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

function TSAXDriver.getLexicalHandlerPropertyName : SAXString;
begin
  Result:= LexicalHandlerProperty;
end;

function TSAXDriver.getLexicalHandlerPropertyValue : IUnknown;
begin
  if (lexicalHandler = base) then
    Result:= nil
  else
    Result:= lexicalHandler;
  Exit;
end;

procedure TSAXDriver.setLexicalHandlerPropertyValue(const value : IUnknown);
var handler : ILexicalHandler;
begin
  if (value = nil) then
  begin
    lexicalHandler:= base as ILexicalHandler;
  end else if (not Supports(value, ILexicalHandler, handler)) then
    raise ESAXNotSupportedException.Create(LexicalHandlerProperty)
  else
    lexicalHandler:= ILexicalHandler(value);
end;

function TSAXDriver.getLength: Integer;
begin
  Result:= Length(attributeNames);
  Exit;
end;

function TSAXDriver.getURI(index: Integer): SAXString;
begin
  Result:= attributeNamespaces[index];
  Exit;
end;

function TSAXDriver.getLocalName(index: Integer): SAXString;
begin
  Result:= attributeLocalNames[index];
  Exit;
end;

function TSAXDriver.getQName(index: Integer): SAXString;
begin
  Result:= attributeNames[index];
  Exit;
end;

function TSAXDriver.getType(index: Integer): SAXString;
var attrType : SAXString;
begin
  attrType:= TXmlParser(parser).getAttributeType(elementName, getQName(index));
  if (attrType = '') then
  begin
    Result:= 'CDATA';
    Exit;
  end;
  // ... use DeclHandler.attributeDecl to see enumerations
  if (attrType = 'ENUMERATION') then
  begin
    Result:= 'NMTOKEN';
  end;
  Result:= attrType;
end;

function TSAXDriver.getValue(index: Integer): SAXString;
begin
  Result:= attributeValues[index];
  Exit;
end;

function TSAXDriver.getIndex(const uri, local: SAXString): Integer;
var i, length : Integer;
begin
  length:= getLength();

  for i:= 0 to length-1 do
  begin
    if (not (getURI(i) = uri)) then
      continue;
    if (getLocalName(i) = (local)) then
    begin
      Result:= i;
      Exit;
    end;
  end;
  Result:= -1;
  Exit;
end;

function TSAXDriver.getIndex(const xmlName: SAXString): Integer;
var i, length : Integer;
begin
  length:= getLength();

  for i:= 0 to length-1 do
  begin
    if (getQName(i) = (xmlName)) then
    begin
      Result:= i;
      Exit;
    end;
  end;
  Result:= -1;
  Exit;
end;

function TSAXDriver.getType(const uri, local: SAXString): SAXString;
var index : Integer;
begin
  index:= getIndex(uri, local);

  if (index < 0) then
  begin
    Result:= '';
    Exit;
  end;
  Result:= getType(index);
  Exit;
end;

function TSAXDriver.getType(const xmlName: SAXString): SAXString;
var index : Integer;
begin
  index:= getIndex(xmlName);

  if (index < 0) then
  begin
    Result:= '';
    Exit;
  end;
  Result:= getType(index);
  Exit;
end;

function TSAXDriver.getValue(const uri, local: SAXString): SAXString;
var index : Integer;
begin
  index:= getIndex(uri, local);

  if (index < 0) then
  begin
    Result:= '';
    Exit;
  end;
  Result:= getValue(index);
  Exit;
end;

function TSAXDriver.getValue(const xmlName: SAXString): SAXString;
var index : Integer;
begin
  index:= getIndex(xmlName);

  if (index < 0) then
  begin
    Result:= '';
    Exit;
  end;
  Result:= getValue(index);
  Exit;
end;

function TSAXDriver.isSpecified(index : Integer) : Boolean;
begin
  if ((index < 0) or (index >= attributeCount)) then
    raise ESAXIllegalArgumentException.Create('');
  Result:= attributeSpecified[index];
end;


function TSAXDriver.isSpecified(const uri, local : SAXString) : Boolean;
var index : Integer;
begin
  index:= getIndex(uri, local);

  if (index < 0) then
    raise ESAXIllegalArgumentException.Create('');
  Result:= attributeSpecified[index];
end;

function TSAXDriver.isSpecified(const qName : SAXString) : Boolean;
var index : Integer;
begin
  index:= getIndex(qName);

  if (index < 0) then
    raise ESAXIllegalArgumentException.Create('');
  Result:= attributeSpecified[index];
end;

function TSAXDriver.isDeclared(index : Integer) : Boolean;
begin
  if ((index < 0) or (index >= attributeCount)) then
    raise ESAXIllegalArgumentException.Create('');
  Result:= attributeDeclared[index];
end;


function TSAXDriver.isDeclared(const uri, local : SAXString) : Boolean;
var index : Integer;
begin
  index:= getIndex(uri, local);

  if (index < 0) then
    raise ESAXIllegalArgumentException.Create('');
  Result:= attributeDeclared[index];
end;

function TSAXDriver.isDeclared(const qName : SAXString) : Boolean;
var index : Integer;
begin
  index:= getIndex(qName);

  if (index < 0) then
    raise ESAXIllegalArgumentException.Create('');
  Result:= attributeDeclared[index];
end;

function TSAXDriver.getPublicId: PSAXChar;
begin
  Result:= '';   // FIXME track public IDs too
  Exit;
end;

function TSAXDriver.getSystemId: PSAXChar;
begin
  if (entityStack.Count = 0) then
  begin
    Result:= '';
    Exit;
  end else
  begin
    // peeking doesn't remove the item
    Result:= PSAXChar(TSAXStringWrapper(entityStack.peek()).Value);
    Exit;
  end;
end;

function TSAXDriver.getLineNumber: Integer;
begin
  //!! add check
  if (parser <> nil) then
  begin
    Result:= TXmlParser(parser).getLineNumber();
    Exit;
  end else
    Result:= 0;
end;

function TSAXDriver.getColumnNumber: Integer;
begin
  //!! add check
  if (parser <> nil) then
  begin
    Result:= TXmlParser(parser).getColumnNumber();
    Exit;
  end else
    Result:= 0;
end;

{ TSAXElementWrapper }

constructor TSAXElementWrapper.create;
begin
{$IFDEF DEBUG}
Inc(hcreated);
{$ENDIF}
  inherited Create();
end;

destructor TSAXElementWrapper.destroy;
begin
{$IFDEF DEBUG}
Inc(hfreed);
{$ENDIF}
  inherited Destroy;
end;

{ TSAXAttributeWrapper }

constructor TSAXAttributeWrapper.create;
begin
{$IFDEF DEBUG}
Inc(hcreated);
{$ENDIF}
  inherited Create();
end;

destructor TSAXAttributeWrapper.destroy;
begin
{$IFDEF DEBUG}
Inc(hfreed);
{$ENDIF}
  inherited Destroy;
end;

{ TSAXEntityWrapper }

constructor TSAXEntityWrapper.create;
begin
{$IFDEF DEBUG}
Inc(hcreated);
{$ENDIF}
  inherited Create();
end;

destructor TSAXEntityWrapper.destroy;
begin
{$IFDEF DEBUG}
Inc(hfreed);
{$ENDIF}
  inherited Destroy;
end;

{ TSAXNotationWrapper }

constructor TSAXNotationWrapper.create;
begin
{$IFDEF DEBUG}
Inc(hcreated);
{$ENDIF}
  inherited Create();
end;

destructor TSAXNotationWrapper.destroy;
begin
{$IFDEF DEBUG}
Inc(hfreed);
{$ENDIF}
  inherited Destroy;
end;

{ TSAXInputWrapper }

constructor TSAXInputWrapper.create;
begin
{$IFDEF DEBUG}
Inc(hcreated);
{$ENDIF}
  inherited Create();
end;

destructor TSAXInputWrapper.destroy;
begin
{$IFDEF DEBUG}
Inc(hfreed);
{$ENDIF}
  readBuffer:= nil;
  inherited Destroy;
end;

{ TSAXStringWrapper }

constructor TSAXStringWrapper.create(s: SAXString);
begin
{$IFDEF DEBUG}
Inc(hcreated);
{$ENDIF}
  inherited Create();
  Value:= s;
end;

destructor TSAXStringWrapper.destroy;
begin
{$IFDEF DEBUG}
Inc(hfreed);
{$ENDIF}
  inherited Destroy;
end;

{ TSAXStringArrayWrapper }

constructor TSAXStringArrayWrapper.create(s: SAXStringArray);
begin
{$IFDEF DEBUG}
Inc(hcreated);
{$ENDIF}
  inherited Create();
  Value:= s;
end;

destructor TSAXStringArrayWrapper.destroy;
begin
{$IFDEF DEBUG}
Inc(hfreed);
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF DEBUG}

initialization
  hCreated:= 0;
  hFreed:= 0;

finalization

{$ENDIF}

end.
