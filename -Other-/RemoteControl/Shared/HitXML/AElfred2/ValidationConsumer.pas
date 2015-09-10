// Filename : ValidationConsumer.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit ValidationConsumer;

// "!!"

interface

uses Classes, SysUtils, Contnrs, SAX, SAXHelpers, SAXExt, Dialogs, EventFilter,
     EventConsumer, Hashtable, XmlChars;

type

  TValidationConsumer = class (TEventFilter)
  private
    // for tracking active content models
    rootName : SAXString;
    contentStack : TList;
    // flags for "saved DTD" processing
    disableDeclarations : Boolean;
    disableReset : Boolean;
    // key = element name; value = ElementInfo
    elements : THashTable;
    // some VCs relate to ID/IDREF/IDREFS attributes
    // key = id; value = boolean true (defd) or false (refd)
    ids : THashTable;
    // we just record declared notation and unparsed entity names.
    // the implementation here is simple/slow; these features
    // are seldom used, one hopes they'll wither away soon
    notations : SAXStringArray;
    nDeferred : SAXStringArray;
    unparsed : SAXStringArray;
    uDeferred : SAXStringArray;
    procedure resetState();
    procedure warning(const description : SAXString);
    procedure error(const description : SAXString);
    procedure fatalError(const description : SAXString);
    function isName(const name, context, id : SAXString) : Boolean;
    function isNmtoken(const nmtoken, context, id : SAXString) : Boolean;
    procedure checkEnumeration(const value, valType, name : SAXString);
    function hasMatch(const value, orList : SAXString) : Boolean;
    function expandDefaultRefs(const s : SAXString) : SAXString;
  public
    constructor Create(AOwner : TComponent; const next : IEventConsumer); overload; override;
    constructor Create(AOwner : TComponent; rootName : SAXString; const publicId, systemId, internalSubset : SAXString; const resolver : IEntityResolver; const minimalDocument : SAXString; const vendorName : SAXString); reintroduce; overload; virtual;
    destructor Destroy(); override;
    procedure startDTD(const name, publicId, systemId : SAXString); override;
    procedure endDTD(); override;
    procedure attributeDecl(const eName, aName, attrType, mode, value : SAXString); override;
    procedure elementDecl(const name, model : SAXString); override;
    procedure internalEntityDecl(const name, value : SAXString); override;
    procedure externalEntityDecl(const name, publicId, systemId : SAXString); override;
    procedure notationDecl(const name, publicId, systemId : SAXString); override;
    procedure unparsedEntityDecl(const name, publicId, systemId, notationName : SAXString); override;
    procedure startDocument(); override;
    procedure skippedEntity(const name : SAXString); override;
    procedure startElement(const namespaceURI, localName, qName: SAXString; const atts: IAttributes); override;
    procedure characters(const ch : SAXString); override;
    procedure endElement(const namespaceURI, localName, qName: SAXString); override;
    procedure endDocument(); override;
    procedure Kill;
  end;

{$IFDEF VDEBUG}
 var VCreated : Integer = 0;
     VDestroyed : Integer = 0;
{$ENDIF}

implementation

type

  TRecognizer = class;
  TRecognizerArray = array of TRecognizer;

  TElementInfo = class
  private
    recognizer : TRecognizer;
    recognizers : TList;
  public
    name : SAXString;
    model : SAXString;
    attributes : THashtable;
    constructor Create(const n : SAXString);
    destructor Destroy(); override;
    function getRecognizer(const consumer : TValidationConsumer) : TRecognizer;
    procedure addRecognizer(const r : TRecognizer);
  end;

  TAttributeInfo = class
  public
    attrType : SAXString;
    mode : SAXString;
    value : SAXString;
    constructor Create();
    destructor Destroy(); override;
  end;

  // Base class defines the calls used to validate content,
  // and supports the "ANY" content model
  TRecognizer = class
  private
    FelType : TElementInfo;
    function getElType : TElementInfo;
    procedure setElType(const el : TElementInfo);
  public
    constructor Create(const t : TElementInfo); virtual;
    destructor Destroy(); override;
    function acceptCharacters() : Boolean; virtual;
    function acceptElement(const name : SAXString) : TRecognizer; virtual;
    function completed() : Boolean; virtual;
    function AsString() : SAXString; virtual;
    property elType : TElementInfo read getElType write setElType;
  end;

  // "EMPTY" content model -- no characters or elements
  TEmptyRecognizer = class(TRecognizer)
  public
    function acceptCharacters() : Boolean; override;
    function acceptElement(const name : SAXString) : TRecognizer; override;
  end;

  // "Mixed" content model -- ANY, but restricts elements
  TMixedRecognizer = class(TRecognizer)
  private
    permitted : SAXStringArray;
  public
    constructor Create(const t : TElementInfo; const v : TValidationConsumer); reintroduce;
    function acceptElement(const name : SAXString) : TRecognizer; override;
  end;

  TChildrenRecognizer = class(TRecognizer)
  private
    // for reporting non-deterministic content models
    // ... a waste of space if we're not reporting those!
    // ... along with the 'model' member(in base class)
    consumer : TValidationConsumer;
    // for CHOICE nodes -- each component is an arc that
    // accepts a different NAME(or is EMPTY indicating
    // NDFA termination).
    components : TRecognizerArray;
    // for NAME/SEQUENCE nodes -- accepts that NAME and
    // then goes to the next node(CHOICE, NAME, EMPTY).
    name : SAXString;
    next : TRecognizer;
    // loops always point back to a CHOICE node. we mark such choice
    // nodes(F_LOOPHEAD) for diagnostics and faster deep cloning.
    // We also mark nodes before back pointers(F_LOOPNEXT), to ensure
    // termination when we patch sequences and loops.
    flags : Integer;
    // prevent a needless indirection between 'this' and 'node'
    procedure copyIn(node : TChildrenRecognizer);
    constructor Create(const vc : TValidationConsumer; const elType : TElementInfo); reintroduce; overload;
    function shallowClone() : TChildrenRecognizer;
    function deepClone() : TChildrenRecognizer; overload;
    function deepClone(const keys, items : TList) : TChildrenRecognizer; overload;
    procedure patchNext(const theNext : TRecognizer; keys, items : TList);
    function populate(const parseBuf : SAXString; const startPos,
      parseBufLength : Integer) : Integer;
  public
    constructor Create(const elType : TElementInfo; const vc : TValidationConsumer); reintroduce; overload;
    function acceptCharacters() : Boolean; override;
    function acceptElement(const elType : SAXString) : TRecognizer; override;
    function completed() : Boolean; override;
  end;

const

  // report error if we happen to notice a non-deterministic choice?
  // we won't report buggy content models; just buggy instances
  warnNonDeterministic : Boolean = false;
  fakeRootName : SAXString = ':Nobody:in:their_Right.Mind_would:use:this-name:1x:';

  ID_FOUND_TRUE = 1;
  ID_FOUND_FALSE = 2; // not 0!

  // !! added
  function contains(const s : SAXString; const arr : SAXStringArray) : Boolean;
  var I : Integer;
  begin
    Result:= False;
    for I:= 0 to Length(arr)-1 do
      if (arr[I] = s) then
      begin
        Result:= True;
        Exit;
      end;
  end;

  function tokenize(const str : SAXString;
    delims : SAXString) : SAXStringArray; overload;
  var p, l, len : Integer;
      tmp : SAXString;
  begin
    l:= 0;
    p:= 1;
    len:= Length(str);
    tmp:= '';
    Result:= nil;
    while (p <= len) do
    begin
      try
        if (Pos(str[p], delims) > 0) or (p = len) then
        begin
          // if this is the last char add it
          if (p = len) then
            tmp:= tmp + str[p];
          // check if we need to add the token
          if (Length(tmp) > 0) then
          begin
            // add it
            setLength(Result, l+1);
            Result[l]:= tmp;
            inc(l);
            // clear
            tmp:= '';
          end;
          continue;
        end else
          tmp:= tmp + str[p];
      finally
        inc(p);
      end;
    end;
  end;

  function tokenize(const str : SAXString) : SAXStringArray; overload;
  begin
    Result:= tokenize(str, SAXString(#13#10#9#32));
  end;

{ TValidationConsumer }

constructor TValidationConsumer.Create(AOwner : TComponent;
  const next : IEventConsumer);
begin
  inherited Create(AOwner, next);

  // Other stuff...
  contentStack:= TList.Create;
  ids:= THashtable.Create(511);
  elements:= THashtable.Create(511);

  setContentHandler(Self as IContentHandler);
  setDTDHandler(Self as IDTDHandler);
  try
    IInterfaceProperty(getProperty(
      DeclHandlerProperty)).setValue(Self as IDeclHandler);
  except
    on e : ESAXException do
    begin
      // can't happen
    end;
  end;
  try
    IInterfaceProperty(getProperty(
      LexicalHandlerProperty)).setValue(Self as ILexicalHandler);
  except
    on e : ESAXException do
    begin
      // can't happen
    end;
  end;
end;

constructor TValidationConsumer.Create(AOwner : TComponent;
  rootName : SAXString; const publicId, systemId,
  internalSubset: SAXString; const resolver: IEntityResolver;
  const minimalDocument: SAXString; const vendorName : SAXString);
var next : IEventConsumer;
    writer : TMemoryStream;
    producer : IXMLReader;
    input : IStreamInputSource;
begin
  next:= nil;
  Create(AOwner, next);

  disableReset:= true;
  if (rootName = '') then
    rootName:= fakeRootName;

  //
  // Synthesize document with that DTD; is it possible to do
  // better for the declaration of the root element?
  //
  // NOTE:  can't use SAX2 to write internal subsets.
  //
  writer:= TMemoryStream.Create();

  writer.Write(PSAXChar(SAXString('<!DOCTYPE '))^, SizeOf(SAXChar) * 10);
  writer.write(PSAXChar(rootName)^, Length(rootName) * SizeOf(SAXChar));
  if (systemId <> '') then
  begin
    writer.write(PSAXChar(SAXString(#10'  '))^, SizeOf(SAXChar) * 3);
    if (publicId <> '') then
    begin
      writer.write(PSAXChar(SAXString('PUBLIC '''))^, SizeOf(SAXChar) * 8);
      writer.write(PSAXChar(publicId)^, Length(publicId) * SizeOf(SAXChar));
      writer.write(PSAXChar(SAXString(''''#10#9''''))^, SizeOf(SAXChar) * 4);
    end else
      writer.write(PSAXChar(SAXString('SYSTEM '''))^, SizeOf(SAXChar) * 8);
    writer.write(PSAXChar(systemId)^, Length(systemId) * SizeOf(SAXChar));
    writer.write(PSAXChar(SAXString(''''))^, SizeOf(SAXChar));
  end;
  writer.write(PSAXChar(SAXString(' [ '))^, SizeOf(SAXChar) * 3);
  if (rootName = fakeRootName) then
  begin
    writer.write(PSAXChar(SAXString(#10'<!ELEMENT '))^, SizeOf(SAXChar) * 11);
    writer.write(PSAXChar(rootName)^, Length(rootName) * SizeOf(SAXChar));
    writer.write(PSAXChar(SAXString(' EMPTY>'))^, SizeOf(SAXChar) * 7);
  end;
  if (internalSubset <> '') then
    writer.write(PSAXChar(internalSubset)^, Length(internalSubset) * SizeOf(SAXChar));
  writer.write(PSAXChar(SAXString(#10' ]>'))^, SizeOf(SAXChar) * 4);

  if (minimalDocument <> '') then
  begin
    writer.write(PSAXChar(SAXString(#10))^, SizeOf(SAXChar));
    writer.write(PSAXChar(minimalDocument)^, Length(minimalDocument) * SizeOf(SAXChar));
    writer.write(PSAXChar(SAXString(#10))^, SizeOf(SAXChar));
  end else
  begin
    writer.write(PSAXChar(SAXString(' <'))^, SizeOf(SAXChar));
    writer.write(PSAXChar(rootName)^, Length(rootName) * SizeOf(SAXChar));
    writer.write(PSAXChar(SAXString('/>'#10))^, SizeOf(SAXChar) * 3);
  end;
  writer.Seek(0, 0);
  writer.Read(PSAXChar(minimalDocument)^, writer.Size);
  writer.Seek(0, 0);

  //
  // OK, load it
  //

  //!! changed from XMLReaderFactory...
  producer:= getSAXVendor(vendorName).XMLReader;
  bind(producer, self);

  if (resolver <> nil) then
   producer.setEntityResolver(resolver);

  input:= TStreamInputSource.create(writer, soOwned) as IStreamInputSource;
  producer.parse(input);

  disableDeclarations:= true;
  if (rootName = fakeRootName) then
    Self.rootName:= '';

end;

destructor TValidationConsumer.Destroy;
var I : Integer;
    els : TPointerDynarray;
begin
  contentStack.Free;
  ids.Free;
  els:= elements.getElements();
  for i:= 0 to Length(els)-1 do
    TElementInfo(els[i]).Free;
  elements.Free;
  inherited Destroy();
end;

procedure TValidationConsumer.resetState;
var I : Integer;
    els : TPointerDynarray;
begin
  els:= nil;
  if (not disableReset) then
  begin
    rootName:= '';
    contentStack.clear();
    els:= elements.getElements();
    for i:= 0 to Length(els)-1 do
      TElementInfo(els[i]).Free;
    elements.clear();
    ids.clear();
    notations:= nil;
    nDeferred:= nil;
    unparsed:= nil;
    uDeferred:= nil;
  end;
end;

procedure TValidationConsumer.warning(const description: SAXString);
var errHandler : IErrorHandler;
    locator : ILocator;
    err : ISAXParseError;
begin
  errHandler:= getErrorHandler();
  locator:= getDocumentLocator();

  if (errHandler = nil) then
    Exit;

  if (locator = nil) then
    err:= TSAXParseError.Create(description, nil, nil, -1, -1) as ISAXParseError
  else
    err:= TSAXParseError.create(PSAXChar(description),
      locator) as ISAXParseError;

  errHandler.warning(err);
end;

procedure TValidationConsumer.error(const description: SAXString);
var errHandler : IErrorHandler;
    locator : ILocator;
    err : ISAXParseError;
begin
  errHandler:= getErrorHandler();
  locator:= getDocumentLocator();

  if (errHandler = nil) then
    Exit;

  if (locator = nil) then
    err:= TSAXParseError.Create(description, nil, nil, -1, -1) as ISAXParseError
  else
    err:= TSAXParseError.create(PSAXChar(description),
      locator) as ISAXParseError;

  if (errHandler <> nil) then
    errHandler.error(err)
  else
    // else we always treat it as fatalnot
    raise ESAXParseException.Create(err);
end;

procedure TValidationConsumer.fatalError(const description: SAXString);
var errHandler : IErrorHandler;
    locator : ILocator;
    err : ISAXParseError;
begin
  errHandler:= getErrorHandler();
  locator:= getDocumentLocator();

  if (errHandler = nil) then
    Exit;

  if (locator = nil) then
    err:= TSAXParseError.Create(description, nil, nil, -1, -1) as ISAXParseError
  else
    err:= TSAXParseError.create(PSAXChar(description),
      locator) as ISAXParseError;

  if (errHandler <> nil) then
    errHandler.fatalError(err);

  // we always treat this as fatal, regardless of the handler
  raise ESAXParseException.Create(err);
end;

function TValidationConsumer.isName(const name, context,
  id: SAXString): Boolean;
var pass : Boolean;
    i, max : Integer;
    c : SAXChar;
begin
  max:= Length(name);

  if (max = 0) then
  begin
    Result:= False;
    Exit;
  end;

  pass:= true;

  if (not isNameStartChar(name[1])) then
    pass:= false
  else begin
    for I:= 2 to max do
    begin
      c:= name[I];
      if (not isNameChar(c)) then
      begin
        pass:= false;
        break;
      end;
    end;
  end;

  if (not pass) then
    error('In ' + context + ' for ' + id + ', ''' + name + ''' is not a name');

  Result:= pass;        // true == OK
  Exit;
end;

function TValidationConsumer.isNmtoken(const nmtoken, context,
  id: SAXString): Boolean;
var pass : Boolean;
    i, max : Integer;
    c : SAXChar;
begin
  max:= Length(nmToken);

  if (max = 0) then
  begin
    Result:= False;
    Exit;
  end;

  pass:= true;

  for I:= 1 to max do
  begin
    c:= nmToken[I];
    if (not isNameChar(c)) then
    begin
      pass:= false;
      break;
    end;
  end;

  if (not pass) then
    error('In ' + context + ' for ' + id + ', ''' + nmToken +
      ''' is not a name token');

  Result:= pass;        // true == OK
  Exit;
end;

procedure TValidationConsumer.checkEnumeration(const value, valType,
  name: SAXString);
begin
  if (not hasMatch(value, valType)) then
    // VC: Enumeration
    error('Value ''' + value + ''' for attribute ''' + name +
      ''' is not permitted: ' + valType);
end;

function TValidationConsumer.hasMatch(const value,
  orList: SAXString): Boolean;
var len,
    max,
    start,
    p : Integer;
    c : SAXChar;
begin
  len:= Length(value);
  max:= Length(orList) - len;

  // !! This "Pos from Pos" is awful....
  start:= Pos(value, orList);
  while (start <> -1) do
  begin
    try
      if (start > max) then
        break;
      c:= orList[start - 1];
      if (c <> '|' ) and ( c <> '(') then
        continue;
      c:= orList[start + len];
      if (c <> '|' ) and ( c <> ')') then
        continue;
      Result:= true;
      Exit;
    finally
      // !!  is this the right order?
      Inc(start);
      // !! This "Pos from Pos" is awful....
      p:= Pos(value, Copy(orList, start, Length(orList)));
      // Now append the current amount...
      if (p <> -1) then
        Inc(start, p-1)
      else
        start:= -1;  
    end;
  end;
  Result:=  false;
end;

procedure TValidationConsumer.startDTD(const name, publicId,
  systemId: SAXString);
begin
  if (disableDeclarations) then
    Exit;

  rootName:= name;
  inherited startDTD(name, publicId, systemId);
end;

procedure TValidationConsumer.endDTD;
var len, i : Integer;
    notation : SAXString;
    entity : SAXString;
begin
  if (disableDeclarations) then
    Exit;

  // this is a convenient hook for end-of-dtd checks, but we
  // could also trigger it in the first startElement call.
  // locator info is more appropriate here though.

  // VC: Notation Declared(NDATA can refer to them before decls,
  //  as can NOTATION attribute enumerations and defaults)
  len:= Length(nDeferred);
  for I:= 0 to len-1 do
  begin
    notation:= nDeferred[i];
    if (not contains(notation, notations)) then
    begin
      error('A declaration referred to notation ''' + notation
        + ''' which was never declared');
    end;
  end;
  nDeferred:= nil;

  // VC: Entity Name(attribute values can refer to them
  //  before they're declared); VC Attribute Default Legal
  len:= Length(uDeferred);
  for I:= 0 to len-1 do
  begin
    entity:= uDeferred[i];
    if (not contains(entity, unparsed)) then
    begin
      error('An attribute default referred to entity ''' + entity
        + ''' which was never declared');
    end;
  end;
  uDeferred:= nil;
  inherited endDTD();
end;

const

  types : array[0..7] of SAXString = (
    'CDATA',
    'ID', 'IDREF', 'IDREFS',
    'NMTOKEN', 'NMTOKENS',
    'ENTITY', 'ENTITIES'
  );


procedure TValidationConsumer.attributeDecl(const eName, aName, attrType,
  mode, value: SAXString);
var info : TElementInfo;
    ainfo : TAttributeInfo;
    ainfo2 : TAttributeInfo;
    checkOne : Boolean;
    interned : Boolean;
    i, lastIndex : Integer;
    tokens : SAXStringArray;
    token : SAXString;
    e : SAXStringArray;
    e2 : TPointerDynarray;
    names : SAXStringArray;
    name : SAXString;
    freeAInfo : Boolean;
begin
  e:= nil;
  e2:= nil;
  tokens:= nil;
  names:= nil;

  if (disableDeclarations) then
    Exit;

  info:= TElementInfo(elements.get(eName));
  ainfo:= TAttributeInfo.Create();
  freeAInfo:= true;
  checkOne:= false;
  interned:= false;

  try

    // !! modified...
    // cheap interning of type names and #FIXED, #REQUIRED
    // for faster startElement(we can use '=')
    for i:= 0 to 7 do
    begin
      if (types[i] = attrType) then
      begin
        // !! later we can redirect the pointer to the local copy
        interned:= true;
        break;
      end;
    end;

    // !! Removed FIXED and REQIURED mode assignment

    ainfo.attrType:= attrType;
    ainfo.mode:= mode;
    ainfo.value:= value;

    // we might not have seen the content model yet
    if (info = nil) then
    begin
      info:= TElementInfo.Create(eName);
      elements.put(eName, info);
    end;
    if ('ID' = attrType) then
    begin
      checkOne:= true;
      if (not (('#REQUIRED' = mode) or ('#IMPLIED'= mode))) then
      begin
        // VC: ID Attribute Default
        error('ID attribute ''' + aName + ''' must be #IMPLIED or #REQUIRED');
      end;

    end else if ((not interned) and (Pos('NOTATION ', attrType) = 1)) then
    begin
      checkOne:= true;

      //!! we have no lastIndex string function
      lastIndex:= -1;
      for i:= Length(attrType) downto 11 do
        if (attrType[i] = ')') then
        begin
          lastIndex:= i;
          Break;
        end;

      // VC: Notation Attributes(notations must be declared)
      tokens:= tokenize(Copy(attrType, 11, lastIndex-11), '|');
      for i:= 0 to Length(tokens)-1 do
      begin
        token:= tokens[i];
        if (not contains(token, notations)) then
        begin
          setLength(nDeferred, Length(nDeferred)+1);
          nDeferred[Length(nDeferred)-1]:= token;
        end;
      end;
    end;

    if (checkOne) then
    begin
      e:= SAXStringArray(info.attributes.getKeys());
      e2:= TPointerDynarray(info.attributes.getElements());
      for i:= 0 to Length(e)-1 do
      begin
        name:= e[i];
        ainfo2:= TAttributeInfo(e2[i]);

        // !! added check for initialized Hashtable
        if (ainfo2 = nil) then
          continue;

        if (attrType = ainfo2.attrType) or (not interned { NOTATION }) then
        begin
          // VC: One ID per Element Type
          // VC: One Notation per Element TYpe
          if (interned) then
            error('Element ''' + eName + ''' already has an attribute of type '
              + 'NOTATION' + '(''' + name + ''') so ''' + aName
              + ''' is a validity error')
          else
            error('Element ''' + eName + ''' already has an attribute of type '
              + attrType + '(''' + name + ''') so ''' + aName
              + ''' is a validity error')
        end;
      end;
    end;

    // VC: Attribute Default Legal
    if (value <> '') then begin

      if ('CDATA' = attrType) then
      begin
        // event source rejected '<'
      end else if ('NMTOKEN' = attrType) then
      begin
        // VC: Name Token(is a nmtoken)
        isNmtoken(value, 'attribute default', aName);
      end else if ('NMTOKENS' = attrType) then
      begin
        // VC: Name Token(is a nmtoken; at least one value)
        tokens:= tokenize(value);
        if (not (Length(tokens) > 0)) then
          error('Default for attribute ''' + aName +
            ''' must have at least one name token.')
        else begin
          for I:= 0 to Length(tokens)-1 do
          begin
            token:= tokens[i];
            isNmtoken(token, 'attribute default', aName);
          end;
        end;
      end else if ('IDREF' = attrType) or ('ENTITY' = attrType) then
      begin
        // VC: Entity Name(is a name)
        // VC: IDREF(is a name)(is declared)
        isName(value, 'attribute default', aName);
        if ('ENTITY' = attrType) and (not contains(value, unparsed)) then
        begin
          setLength(uDeferred, Length(uDeferred)+1);
          uDeferred[Length(uDeferred)-1]:= value;
        end;
      end else if ('IDREFS' = attrType) or ('ENTITIES' = attrType) then
      begin
        // VC: Entity Name(is a name; at least one value)
        // VC: IDREF(is a name; at least one value)
        names:= tokenize(value);
        if (not (Length(names) > 0)) then
          error('Default for attribute ''' + aName
            + ''' must have at least one name.')
        else begin
          for i:= 0 to Length(names)-1 do
          begin
            name:= names[i];
            isName(name, 'attribute default', aName);
            if ('ENTITIES' = attrType) and (not contains(name, unparsed)) then
            begin
              setLength(uDeferred, Length(uDeferred)+1);
              uDeferred[Length(uDeferred)-1]:= value;
            end;
          end;
        end;

      end else if (attrType[1] = '(') then
      begin
        // VC: Enumeration(must match)
        checkEnumeration(value, attrType, aName);

      end else if (not interned) and (checkOne) then
      begin
        { NOTATION }
        // VC: Notation attributes(must be names)
        isName(value, 'attribute default', aName);

        // VC: Notation attributes(must be declared)
        if (not contains(value, notations)) then
        begin
          setLength(nDeferred, Length(nDeferred)+1);
          nDeferred[Length(nDeferred)-1]:= value;
        end;

        // VC: Enumeration(must match)
        checkEnumeration(value, attrType, aName);

      end else if ('ID' <> attrType) then
        raise ESAXParseException.Create('illegal attribute type: ' + attrType);
    end;

    if (info.attributes.get(aName) = nil) then
    begin
      info.attributes.put(aName, ainfo);
      freeAInfo:= false;
    end;

    if ('xml:space' = aName) then
    begin
      if (not (
        ('(default|preserve)' = attrType) or
        ('(preserve|default)' = attrType) or
        // these next two are arguable; XHTML's DTD doesn't
        // deserve errors.  After all, it's not like any
        // illegal _value_ could pass ...
        ('(preserve)' = attrType) or
        ('(default)' = attrType))) then
        error('xml:space attribute type must be like ''(default|preserve)'''
          + ' not ''' + attrType + '''');
    end;

    inherited attributeDecl(eName, aName, attrType, mode, value);
  finally
    if (freeAInfo) then
      ainfo.Free;
  end;
end;

procedure TValidationConsumer.elementDecl(const name, model: SAXString);
var info : TElementInfo;
begin
  if (disableDeclarations) then
    Exit;

  info:= TElementInfo(elements.get(name));

  // we might have seen an attribute decl already
  if (info = nil) then
  begin
    info:= TElementInfo.Create(name);
    elements.put(name, info);
  end;

  if (info.model <> '') then
  begin
    // NOTE:  not all parsers can report such duplicates.
    // VC: Unique Element Type Declaration
    error('Element type ''' + name + ''' was already declared.');
  end else
  begin
    info.model:= model;

    // VC: No Duplicate Types(in mixed content models)
    if (model[2] = '#') then  //(#PCDATA...
      info.getRecognizer(Self);
  end;

  inherited elementDecl(name, model);
end;

procedure TValidationConsumer.internalEntityDecl(const name,
  value: SAXString);
begin
  if (not disableDeclarations) then
    inherited internalEntityDecl(name, value);
end;

procedure TValidationConsumer.externalEntityDecl(const name, publicId,
  systemId: SAXString);
begin
  if (not disableDeclarations) then
    inherited externalEntityDecl(name, publicId, systemId);
end;

procedure TValidationConsumer.notationDecl(const name, publicId,
  systemId: SAXString);
begin
  if (disableDeclarations) then
    Exit;

  SetLength(notations, Length(notations)+1);
  notations[Length(notations)-1]:= name;
  inherited notationDecl(name, publicId, systemId);
end;

procedure TValidationConsumer.unparsedEntityDecl(const name, publicId,
  systemId, notationName: SAXString);
begin
  if (disableDeclarations) then
    Exit;

  SetLength(unparsed, Length(unparsed)+1);
  unparsed[Length(unparsed)-1]:= name;
  if (not contains(notationName, notations)) then
  begin
    SetLength(nDeferred, Length(nDeferred)+1);
    nDeferred[Length(nDeferred)-1]:= notationName;
  end;
  inherited unparsedEntityDecl(name, publicId, systemId, notationName);
end;

procedure TValidationConsumer.startDocument;
begin
  resetState();
  inherited startDocument();
end;

procedure TValidationConsumer.skippedEntity(const name: SAXString);
begin
  fatalError('may not skip entities');
end;

function TValidationConsumer.expandDefaultRefs(
  const s: SAXString): SAXString;
var message : SAXString;
begin
  if (Pos('&', s) = 0) then
  begin
    Result:= s;
    Exit;
  end;

// FIXME: handle &#nn; &#xnn; &name;
  message:= 'Can''t expand refs in attribute default: ' + s;
  warning(message);

  Result:= s;
  Exit;
end;

procedure TValidationConsumer.startElement(const namespaceURI, localName,
  qName: SAXString; const atts: IAttributes);
var state : TRecognizer;
    newState : TRecognizer;
    info : TElementInfo;
    i, j, len : Integer;
    aname : SAXString;
    ainfo : TAttributeInfo;
    value : SAXString;
    expanded : SAXString;
    tokens : SAXStringArray;
    token : SAXString;
    id : SAXString;
    entity : SAXString;
    table : THashtable;
    e : SAXStringArray;
    e2 : TPointerDynarray;
begin
  tokens:= nil;
  e:= nil;
  e2:= nil;

  //
  // First check content model for the enclosing scope.
  //
  if (contentStack.Count = 0) then
  begin
    // VC:  Root Element Type
    if (qName <> rootName) then
    begin
      if (rootName = '') then
        warning('This document has no DTD, can''t be valid')
      else
        error('Root element type ''' + qName + ''' was declared to be ''' +
          rootName + '''');
    end;
  end else
  begin
    state:= TRecognizer(contentStack.items[contentStack.Count-1]);

    if (state <> nil) then
    begin
      newstate:= state.acceptElement(qName);

      if (newstate = nil) then
        error('Element type ''' + qName + ''' in element ''' + state.elType.name
          + ''' violates content model ' + state.elType.model);

      if (newstate <> state) then
      begin
        contentStack.delete(contentStack.Count-1);
        contentStack.add(newstate);
      end;
    end;
  end;

  //
  // Then check that this element was declared, and push the
  // object used to validate its content model onto our stack.
  //
  // This is where the recognizer gets created, if needed; if
  // it's a 'children' (elements) content model, an NDFA is
  // created. (One recognizer is used per content type, no
  // matter how complex that recognizer is.)
  //
  info:= TElementInfo(elements.get(qName));
  if (info = nil) or (info.model = '') then
  begin
    // VC: Element Valid (base clause)
    error('Element type ''' + qName + ''' was not declared');
    contentStack.add(nil);

    // for less diagnostic noise, fake a declaration.
    elementDecl(qName, 'ANY');
  end else
    contentStack.add(info.getRecognizer(Self));

  //
  // Then check each attribute present
  //
  if (atts <> nil) then
    len:= atts.getLength()
  else
    len:= 0;

  for i:= 0 to len-1 do
  begin
    aname:= atts.getQName(i);

    // !! split this check
    if (info = nil) then
    begin
      // VC: Attribute Value Type
      error('Attribute ''' + aname + ''' was not declared for element type '
        + qName);
      continue;
    end;

    ainfo:= TAttributeInfo(info.attributes.get(aname));

    if (ainfo = nil) then
    begin
      // VC: Attribute Value Type
      error('Attribute ''' + aname + ''' was not declared for element type '
        + qName);
      continue;
    end;

    value:= atts.getValue(i);

    // note that '=' for type names and '#FIXED' is correct
    // (and fast) since we've interned those literals.
    // !! uh... not really

    if ('#FIXED' = ainfo.mode) then
    begin
      expanded:= expandDefaultRefs(ainfo.value);

      // VC: Fixed Attribute Default
      if (value <> expanded) then
      begin
        error('Attribute ''' + aname + ''' must match ' + expanded);
        continue;
      end;
    end;

    if ('CDATA' = ainfo.attrType) then
      continue;

    //
    // For all other attribute types, there are various
    // rules to follow.
    //

    if ('ID' = ainfo.attrType) then
    begin
      // VC: ID(must be a name)
      if (isName(value, 'ID attribute', aname)) then
      begin
        if (ID_FOUND_TRUE = Integer(ids.get(value))) then
          // VC: ID(appears once)
          error('ID attribute ' + aname
            + ' uses an ID value ''' + value
            + ''' which was already declared.')
        else
          // any forward refs are no longer problems
         ids.put(value, Pointer(ID_FOUND_TRUE));
      end;
      continue;
    end;

    if ('IDREF' = ainfo.attrType) then
    begin
      // VC: IDREF (value must be a name)
      if (isName(value, 'IDREF attribute', aname)) then
      begin
        // VC: IDREF (must match some ID attribute)
        if (ids.get(value) = nil) then
          // new -- assume it's a forward ref
          ids.put(value, Pointer(ID_FOUND_FALSE));
      end;
      continue;
    end;

    if ('IDREFS' = ainfo.attrType) then
    begin
      tokens:= tokenize(value, ' ');

      if (Length(tokens) = 0) then
      begin
        // VC: IDREF(one or more values)
        error('IDREFS attribute ' + aname
          + ' must have at least one ID ref');
      end else
      begin
        for j:= 0 to Length(tokens)-1 do
        begin
          id:= tokens[j];
          // VC: IDREF(value must be a name)
          if (isName(id, 'IDREFS attribute', aname)) then
          begin
            // VC: IDREF(must match some ID attribute)
            if (ids.get(id) = nil) then
              // new -- assume it's a forward ref
              ids.put(id, Pointer(ID_FOUND_FALSE));
          end;
        end;
      end;
      continue;
    end;

    if ('NMTOKEN' = ainfo.attrType) then
    begin
      // VC: Name Token(is a name token)
      isNmtoken(value, 'NMTOKEN attribute', aname);
      continue;
    end;

    if ('NMTOKENS' = ainfo.attrType) then
    begin
      tokens:= tokenize(value, ' ');

      if (Length(tokens) = 0) then
      begin
        // VC: Name Token(one or more values)
        error('NMTOKENS attribute ' + aname
          + ' must have at least one name token');
      end else
      begin
        for j:= 0 to Length(tokens)-1 do
        begin
          token:= tokens[j];

          // VC: Name Token(is a name token)
          isNmtoken(token, 'NMTOKENS attribute', aname);
        end;
      end;
      continue;
    end;

    if ('ENTITY' = ainfo.attrType) then
    begin
      if (not contains(value, unparsed)) then
        // VC: Entity Name
        error('Value of attribute ''' + aname
          + ''' refers to unparsed entity ''' + value
          + ''' which was not declared.');
      continue;
    end;

    if ('ENTITIES' = ainfo.attrType) then
    begin
      tokens:= tokenize(value, ' ');

      if (Length(tokens) = 0) then
      begin
        // VC: Entity Name(one or more values)
        error('ENTITIES attribute ' + aname
          + ' must have at least one name token');
      end else
      begin
        for j:= 0 to Length(tokens)-1 do
        begin
          entity:= tokens[j];

          if (not contains(entity, unparsed)) then
            // VC: Entity Name
            error('Value of attribute ''' + aname
              + ''' refers to unparsed entity ''' + entity
              + ''' which was not declared.');
        end;
      end;
      continue;
    end;

    //
    // check for enumerations last; more expensive
    //
    if ((ainfo.attrType[1] = '(') or
      (Pos('NOTATION ', ainfo.attrType) = 1)) then
    begin
      // VC: Enumeration(value must be defined)
      checkEnumeration(value, ainfo.attrType, aname);
      continue;
    end;

  end;

  //
  // Last, check that all #REQUIRED attributes were provided
  //
  if (info <> nil) then
  begin
    table:= info.attributes;

    if (table.getLength() <> 0) then
    begin
      e:= SAXStringArray(table.getKeys());
      e2:= TPointerDynarray(table.getElements);
      // XXX table.keys uses the heap, bleech -- slows things

      for j:= 0 to Length(e)-1 do
      begin
        aname:= e[j];
        ainfo:= TAttributeInfo(e2[j]);

        // !! added check (this is needed for the intitially expanded Hashtable)
        if (ainfo = nil) then
          continue;

        // '#REQUIRED' mode was interned in attributeDecl
        if ('#REQUIRED' = ainfo.mode) and (atts.getValue(aname) = '') then
        begin
          // VC: Required Attribute
          error('Attribute ''' + aname + ''' must be specified '
            + 'for element type ' + qName);
        end;
      end;
    end;
  end;

  inherited startElement(namespaceURI, localName, qName, atts);
end;

procedure TValidationConsumer.characters(const ch : SAXString);
var state : TRecognizer;
begin
  if (contentStack.Count = 0) then
    state:= nil
  else
    state:= TRecognizer(contentStack.items[contentStack.Count-1]);

  // NOTE:  if this ever supports with SAX parsers that don't
  // report ignorable whitespace as such (only XP?), this class
  // needs to morph it into ignorableWhitespace() as needed ...

  if (state <> nil) and (not state.acceptCharacters()) then
  begin
    // VC: Element Valid (clauses three, four -- see recognizer)
    error('Character content not allowed in element ' + state.elType.name);
  end;

  inherited characters(ch);
end;

procedure TValidationConsumer.endElement(const namespaceURI, localName,
  qName: SAXString);
var state : TRecognizer;
begin
  try
    state:= TRecognizer(contentStack.items[contentStack.Count-1]);
    contentStack.delete(contentStack.Count-1);

    if (state <> nil) and (not state.completed()) then
    begin
        // VC: Element valid (clauses two, three, four; see Recognizer)
        error ('Premature end for element '''
            + state.elType.name
            + ''', content model '
            + state.elType.model);

    end;

    // could insist on match of start element, but that's
    // something the input stream must to guarantee.

  except
    on e : Exception do
    begin
      if (namespaceURI  = '') then
        fatalError('endElement without startElement: ' + qName)
      else
        fatalError('endElement without startElement: ' + qName +
          ' { ''' + namespaceURI + ''', ' + localName + ' }');
    end;
  end;

  inherited endElement(namespaceURI, localName, qName);
end;

procedure TValidationConsumer.endDocument;
var idNames : SAXStringArray;
    idEls : TPointerDynarray;
    i : Integer;
    id : SAXString;
begin
  idNames:= SAXStringArray(ids.getKeys());
  idEls:= TPointerDynarray(ids.getElements());
  for i:= 0 to Length(idNames)-1 do
  begin;
    id:= idNames[i];
    if (ID_FOUND_FALSE = Integer(idEls[i])) then
    begin
      // VC: IDREF (must match ID)
      error('Undeclared ID value ''' + id
          + ''' was referred to by an IDREF/IDREFS attribute');
    end;
  end;

  resetState();
  inherited endDocument();
end;

procedure TValidationConsumer.Kill;
begin
  _Release;
  _Release;
  _Release;
  _Release;
end;

{ TElementInfo }

procedure TElementInfo.addRecognizer(const r: TRecognizer);
begin
  recognizers.Add(r);
end;

constructor TElementInfo.Create(const n: SAXString);
begin
  inherited Create;
  {$IFDEF VDEBUG}
  inc(VCreated);
  {$ENDIF}
  name:= n;
  attributes:= THashtable.Create(11);
  recognizers:= TList.Create;
end;

destructor TElementInfo.Destroy;
var i : Integer;
    els : TPointerDynarray;
begin
  {$IFDEF VDEBUG}
  inc(VDestroyed);
  {$ENDIF}
  els:= attributes.getElements();
  for i:= 0 to Length(els)-1 do
    TAttributeInfo(els[i]).Free;
  attributes.Free;

  //?? Free the recognizer if not nil
  for I:= 0 to recognizers.Count-1 do
  begin
    TObject(recognizers[i]).Free;
  end;

  recognizers.Free;

  inherited Destroy;
end;

function TElementInfo.getRecognizer(
  const consumer: TValidationConsumer): TRecognizer;
begin
  if (recognizer = nil) then
  begin
    if ('ANY' = model) then
      //!! departure from global ANY
      recognizer:= TRecognizer.Create(nil)
    else if ('EMPTY' = model) then
      recognizer:= TEmptyRecognizer.Create(Self)
    else if (Pos('#', model) = 2) then
      // n.b. this constructor does a validity check
      recognizer:= TMixedRecognizer.Create(Self, consumer)
    else
      recognizer:= TChildrenRecognizer.Create(Self, consumer)
  end;
  Result:= recognizer;
  Exit;
end;

{ TAttributeInfo }

constructor TAttributeInfo.Create;
begin
  inherited;
  {$IFDEF VDEBUG}
  inc(VCreated);
  {$ENDIF}
end;

destructor TAttributeInfo.Destroy;
begin
  {$IFDEF VDEBUG}
  inc(VDestroyed);
  {$ENDIF}
  inherited;
end;

{ TRecognizer }

constructor TRecognizer.Create(const t: TElementInfo);
begin
  inherited Create();
  elType:= t;
  if (elType <> nil) then
    elType.addRecognizer(Self);
  {$IFDEF VDEBUG}
  inc(VCreated);
  {$ENDIF}

end;

destructor TRecognizer.Destroy;
begin
  {$IFDEF VDEBUG}
  inc(VDestroyed);
  {$ENDIF}
  inherited Destroy();
end;

function TRecognizer.acceptCharacters: Boolean;
begin
  // return true iff character data is legal here
  // VC: Element Valid(third and fourth clauses)
  Result:= true;
  Exit;
end;

function TRecognizer.acceptElement(const name: SAXString): TRecognizer;
begin
  // null return = failure
  // otherwise, next state(like an FSM)
  // prerequisite: tested that name was declared
  // VC: Element Valid(fourth clause)
  Result:= Self;
  Exit;
end;

function TRecognizer.completed: Boolean;
begin
  // return true iff model is completed, can finish
  // VC: Element Valid(fourth clause)
  Result:= true;
  Exit;
end;

function TRecognizer.AsString: SAXString;
begin
  // n.b. "children" is the interesting case!
  if (elType = nil) then
  begin
    Result:= 'ANY';
    Exit;
  end else
  begin
    Result:= elType.model;
    Exit;
  end;
end;

function TRecognizer.getElType: TElementInfo;
begin
  Result:= FelType;
end;

procedure TRecognizer.setElType(const el: TElementInfo);
begin
  FelType:= el;
end;

{ TEmptyRecognizer }

function TEmptyRecognizer.acceptCharacters: Boolean;
begin
  // VC: Element Valid(first clause)
  Result:= false;
  Exit;
end;

function TEmptyRecognizer.acceptElement(
  const name: SAXString): TRecognizer;
begin
  // VC: Element Valid(first clause)
  Result:= nil;
  Exit;
end;

{ TMixedRecognizer }

constructor TMixedRecognizer.Create(const t: TElementInfo;
  const v: TValidationConsumer);
var tokens : SAXStringArray;
    vec : SAXStringArray;
    i, lastIndex : Integer;
    token : SAXString;
begin
  // N.B. constructor tests for duplicated element names(VC)
  inherited Create(t);

  //(#PCDATA...)* or (#PCDATA) ==> ... or empty
  // with the "..." being "|elname|..."

  lastIndex:= -1;
  for i:= Length(t.model) downto 9 do
    if (t.model[i] = ')') then
    begin
      lastIndex:= i;
      Break;
    end;

  tokens:= tokenize(Copy(t.model, 9, lastIndex-9), '|');
  for i:= 0 to Length(tokens)-1 do
  begin
    token:= tokens[i];
    if (contains(token, vec)) then
      v.error('element ' + token
        + ' is repeated in mixed content model: '
        + t.model)
    else begin
      SetLength(vec, Length(vec)+1);
      vec[Length(vec)-1]:= token;
    end;
  end;

  permitted:= vec;

  // in one large machine-derived DTD sample, most of about
  // 250 mixed content models were empty, and 25 had ten or
  // more entries.  2 had over a hundred elements.  Linear
  // search isn't obviously wrong.
end;

function TMixedRecognizer.acceptElement(
  const name: SAXString): TRecognizer;
var len, i : Integer;
begin
  // VC: Element Valid(third clause)

  len:= Length(permitted);

  // first pass -- optimistic w.r.t. event source interning
  // (and document validity)
  // !! Actually this is our only pass this could be updated
  // !! to a buffered model and simply compare pointers...?
  for i:= 0 to len-1 do
  begin
    if (permitted[i] = name) then
    begin
      Result:= Self;
      Exit;
    end;
  end;

  // second pass -- pessimistic w.r.t. event source interning
  // !! (we don't need this now as we don't intern externally)

  Result:= nil;
  Exit;
end;

{ TChildrenRecognizer }

const

  F_LOOPHEAD : Integer = $01;
  F_LOOPNEXT : Integer = $02;

procedure TChildrenRecognizer.copyIn(node: TChildrenRecognizer);
begin
  // model & consumer are already set
  components:= node.components;
  name:= node.name;
  next:= node.next;
  flags:= node.flags;
end;

constructor TChildrenRecognizer.Create(const elType: TElementInfo;
  const vc: TValidationConsumer);
var r : TEmptyRecognizer;
begin
  Create(vc, elType);
  populate(elType.model, 1, Length(elType.model));
  r:= TEmptyRecognizer.Create(elType);
  patchNext(r, nil, nil);
end;

constructor TChildrenRecognizer.Create(const vc: TValidationConsumer;
  const elType: TElementInfo);
begin
  inherited Create(elType);
  consumer:= vc;
end;

function TChildrenRecognizer.shallowClone: TChildrenRecognizer;
begin
  // When rewriting some graph nodes we need deep clones in one case;
  // mostly shallow clones are fine.
  Result:= TChildrenRecognizer.Create(Self.consumer, Self.elType); 
  Result.components:= components;
  Result.name:= name;
  Result.flags:= flags;
  Result.next:= next;
end;

function TChildrenRecognizer.deepClone: TChildrenRecognizer;
var keys : TList;
    items : TList;
begin
  keys:= TList.Create;
  items:= TList.Create;
  try
    keys.Capacity:= 57;
    items.Capacity:= 57;
    Result:= deepClone(keys, items);
    Exit;
  finally
    keys.Free;
    items.Free;
  end;
end;

function TChildrenRecognizer.deepClone(
  const keys, items : TList): TChildrenRecognizer;
var retval : TChildrenRecognizer;
    temp : TRecognizer;
    i : integer;

  procedure put(key, item : pointer);
  var idx : integer;
  begin
    idx:= keys.IndexOf(key);
    if (idx = -1) then
    begin
      keys.Add(key);
      items.Add(item);
    end else
      items[idx]:= item;
  end;

  function get(key : pointer) : pointer;
  var idx : Integer;
  begin
    idx:= keys.IndexOf(key);
    if (idx = -1) then
      Result:= nil
    else
      Result:= items[idx];
  end;

begin
  if ((flags and F_LOOPHEAD) <> 0) then
  begin
    retval:= TChildrenRecognizer(get(Self));
    if (retval <> nil) then
    begin
      Result:= Self;
      Exit;
    end;

    retval:= shallowClone();
    put(Self, retval);
  end else
    retval:= shallowClone();

  if (next <> nil) then
  begin
    if (next is TChildrenRecognizer) then
      retval.next:= TChildrenRecognizer(next).deepClone(keys, items)
    else if (not (next is TEmptyRecognizer)) then
      raise ESAXParseException.Create('deepClone');
  end;

  if (components <> nil) then
  begin
    SetLength(retval.components, Length(components));
    for i:= 0 to Length(components)-1 do
    begin
      temp:= components[i];

      if (temp = nil) then
        retval.components[i]:= nil
      else if (temp is TChildrenRecognizer) then
        retval.components[i]:= TChildrenRecognizer(temp).deepClone(keys, items)
      else if (not (temp is TEmptyRecognizer)) then
        raise ESAXParseException.Create('deepClone');
    end;
  end;

  Result:= retval;
  Exit;
end;

procedure TChildrenRecognizer.patchNext(const theNext: TRecognizer;
  keys, items : TList);
var i : Integer;
    owned : Boolean;

  procedure put(key, item : pointer);
  var idx : integer;
  begin
    idx:= keys.IndexOf(key);
    if (idx = -1) then
    begin
      keys.Add(key);
      items.Add(item);
    end else
      items[idx]:= item;
  end;

  function get(key : pointer) : pointer;
  var idx : Integer;
  begin
    idx:= keys.IndexOf(key);
    if (idx = -1) then
      Result:= nil
    else
      Result:= items[idx];
  end;
  
begin
  // connect subgraphs, first to next(sequencing)
  // backpointers must not be repatched or followed
  if ((flags and F_LOOPNEXT) <> 0) then
    Exit;

  // XXX this table "shouldn't" be needed, right?
  // but some choice nodes looped if it isn't there.
  if ((keys <> nil) and (get(Self) <> nil)) then
    Exit;

  owned:= false;

  if (keys = nil) then
  begin
    keys:= TList.Create;
    items:= TList.Create;
    owned:= true;
  end;

  try
    // NAME/SEQUENCE
    if (name <> '') then
    begin
      if (next = nil) then
        next:= theNext
      else if (next is TChildrenRecognizer) then
      begin
        TChildrenRecognizer(next).patchNext(theNext, keys, items);
      end else if (not (next is TEmptyRecognizer)) then
        raise ESAXParseException.Create('patchNext');
      Exit;
    end;

    // CHOICE
    for i:= 0 to Length(components)-1 do
    begin
      if (components[i] = nil) then
        components[i]:= theNext
      else if (TObject(components[i]) is TChildrenRecognizer) then
      begin
         TChildrenRecognizer(components[i]).patchNext(theNext, keys, items);
      end else if (not (TObject(components[i]) is TEmptyRecognizer)) then
        raise ESAXParseException.Create('patchNext');
    end;

    if ((keys <> nil) and ((flags or F_LOOPHEAD) <> 0)) then
      put(Self, Self);

  finally
    if (owned) then
    begin
      keys.Free;
      items.Free;
    end;
  end;
end;

function TChildrenRecognizer.populate(const parseBuf : SAXString;
  const startPos, parseBufLength : Integer): Integer;
var nextPos : Integer;
    c, separator : SAXChar;
    done : Boolean;
    first : TChildrenRecognizer;
    current : TChildrenRecognizer;
    link : TChildrenRecognizer;
    loop : TChildrenRecognizer;
    choice : TChildrenRecognizer;
    once : TRecognizer;
    v : TRecognizerArray;
begin
  nextPos:= startPos + 1;

  // !! Modified bounds...
  if ((nextPos < 1) or (nextPos > Length(parseBuf))) then
    raise Exception.Create('index out of bounds');

  // Grammar of the string is from the XML spec, but
  // with whitespace removed by the SAX parser.

  // children ::=(choice | seq)('?' | '*' | '+')?
  // cp ::=(Name | choice | seq)('?' | '*' | '+')?
  // choice ::= '(' cp('|' choice)* ')'
  // seq ::= '(' cp(',' choice)* ')'

  // interior nodes only
  //   cp ::= name ...
  if (parseBuf[startPos] <> '(') then
  begin
    done:= false;
    repeat
      c:= parseBuf[nextPos];
      case (c) of
        '?',
        '*',
        '+',
        '|',
        ',',
        ')':
        begin
          done:= true;
          continue;
        end;
        else begin
          Inc(nextPos);
          continue;
        end;
      end;
    until (done);

    name:= Copy(parseBuf, startPos, nextPos - startPos);

    // interior OR toplevel nodes
    //   cp ::= choice ..
    //   cp ::= seq ..
  end else begin
    // collect everything as a separate list, and merge it
    // into "this" later if we can(SEQUENCE or singleton)

    first:= TChildrenRecognizer.Create(consumer, elType); 
    nextPos:= first.populate(parseBuf, nextPos, parseBufLength);
    c:= parseBuf[nextPos];
    Inc(nextPos);

    if ((c = ',') or (c = '|')) then
    begin
      current:= first;
      separator:= c;
      v:= nil;

      if (separator = '|') then
      begin
        SetLength(v, 1);
        v[0]:= first;
      end;

      repeat
        link:= TChildrenRecognizer.Create(consumer, elType);
        nextPos:= link.populate(parseBuf, nextPos, parseBufLength);

        if (separator = ',') then
        begin
          current.patchNext(link, nil, nil);
          current:= link;
        end else
        begin
          SetLength(v, Length(v)+1);
          v[Length(v)-1]:= link;
        end;

        c:= parseBuf[nextPos];
        Inc(nextPos);
      until (c <> separator);

      // choice ... collect everything into one array.
      if (separator = '|') then
      begin
        // assert v.size() > 1
        components:= Copy(v, 0, Length(v));

      // sequence ... merge into "this" to be smaller.
      end else
        copyIn(first);

    // treat singletons like one-node sequences.
    end else
        copyIn(first);

    if (c <> ')') then
      raise ESAXParseException.Create('corrupt content model');
  end;

  //
  // Arity is optional, and the root of all fun.  We keep the
  // FSM state graph simple by only having NAME/SEQUENCE and
  // CHOICE nodes(or EMPTY to terminate a model), easily
  // evaluated.  So we rewrite each node that has arity, using
  // those primitives.  We create loops here, if needed.
  //
  if (nextPos <= parseBufLength) then
  begin
    c:= parseBuf[nextPos];
    if ((c = '?') or (c = '*') or (c = '+')) then
    begin
      Inc(nextPos);

      if (c = '?') then
      begin
        // Rewrite 'zero-or-one' "?" arity to a CHOICE:
        //   - SEQUENCE(clone, what's next)
        //   - or, what's next
        // Size cost: N --> N + 1

        once:= shallowClone();
        components:= nil;
        SetLength(components, 2);
        components[0]:= once;
        // components [1] initted to null
        name:= '';
        next:= nil;
        flags:= 0;

      end else if (c = '*') then
      begin
        // Rewrite 'zero-or-more' "*" arity to a CHOICE.
        //   - LOOP(clone, back to this CHOICE)
        //   - or, what's next
        // Size cost: N --> N + 1

        loop:= shallowClone();

        loop.patchNext(Self, nil, nil);
        loop.flags:= loop.flags or F_LOOPNEXT;
        flags:= F_LOOPHEAD;

        components:= nil;
        SetLength(components, 2);
        components[0]:= loop;
        // components [1] initted to null
        name:= '';
        next:= nil;

      end else if (c = '+') then
      begin
        // Rewrite 'one-or-more' "+" arity to a SEQUENCE.
        // Basically(a)+ -->((a),(a) * ).
        //   - this
        //   - CHOICE
        //      * LOOP(clone, back to the CHOICE)
        //      * or, whatever's next
        // Size cost: N --> 2N + 1

        loop:= deepClone();

        choice:= TChildrenRecognizer.Create(consumer, elType); 
        loop.patchNext(choice, nil, nil);
        loop.flags:= loop.flags or F_LOOPNEXT;
        choice.flags:= F_LOOPHEAD;

        choice.components:= nil;
        SetLength(choice.components, 2);
        choice.components[0]:= loop;
        // choice.components [1] initted to null
        // choice.name, choice.next initted to null

        patchNext(choice, nil, nil);
      end;
    end;
  end;

  Result:= nextPos;
  Exit;
end;

function TChildrenRecognizer.acceptCharacters: Boolean;
begin
  // VC: Element Valid(second clause)
  Result:= false;
  Exit;
end;

function TChildrenRecognizer.acceptElement(
  const elType: SAXString): TRecognizer;
var retval, temp : TRecognizer;
    i : Integer;
begin
  // VC: Element Valid (second clause)

  // NAME/SEQUENCE
  if (name <> '') then
  begin
    if (name = elType) then
    begin
      Result:= next;
      Exit;
    end;
    Result:= nil;
    Exit;
  end;

  // CHOICE ... optionally reporting nondeterminism we
  // run across.  we won't check out every transition
  // for nondeterminism; only the ones we follow.
  retval:= nil;

  for I:= 0 to Length(components)-1 do
  begin
    temp:= components[i].acceptElement(elType);

    if (temp = nil) then
      continue
    else if (not warnNonDeterministic) then
    begin
      Result:= temp;
      Exit;
    end else if (retval = nil) then
      retval:= temp
    else if (retval <> temp) then
      consumer.error('Content model ' + Self.elType.model
        + ' is non-deterministic for ' + elType);
  end;

  Result:= retval;
  Exit;
end;

function TChildrenRecognizer.completed: Boolean;
var I : Integer;
begin
  // VC: Element Valid (second clause)

  // expecting a specific element
  if (name <> '') then
  begin
    Result:= false;
    Exit;
  end;

  // choice, some sequences
  for i:= 0 to Length(components)-1 do
  begin
    // !!!! NOT IN JAVA WHY DO I NEED IT?
    if (components[i] = nil) then
    begin
      Result:= True;
      Exit;
    end;
    if (components[i].completed()) then
    begin
      Result:= true;
      Exit;
    end;
  end;

  Result:= false;
end;

end.
