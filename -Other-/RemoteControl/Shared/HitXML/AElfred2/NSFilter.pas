// Filename : NSFilter.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit NSFilter;

// * $Id: NSFilter.java,v 1.1 2001/06/22 15:29:17 db Exp $

interface

uses Classes, SysUtils, Contnrs, SAX, SAXHelpers, SAXExt, Dialogs,
  EventFilter, EventConsumer;

type

  TNSFilter = class(TEventFilter)
  private
    nsStack : TNamespaceSupport;
    elementStack : TStack;
    pushedContext : Boolean;
    nsTemp : TNamespaceParts;
    attributes : TAttributesImpl;
//!! not used    useDefault : Boolean;
  public
    constructor Create(AOwner : TComponent; const next : IEventConsumer);
      override;
    destructor Destroy(); override;
    procedure fatalError(const _message : SAXString);
    procedure startDocument(); override;
    procedure startPrefixMapping(const prefix, uri : SAXString); override;
    function fixName(const ns, l, name : SAXString; const isAttr : Boolean) : SAXString;
    procedure startElement(const uri, localName, qName: SAXString; const atts: IAttributes); override;
    procedure endElement(const uri, localName, qName: SAXString); override;
    procedure endPrefixMapping(const prefix : SAXString); override;
    procedure endDocument(); override;
  end;

implementation

uses SAXDriver;

const

  prefixRoot : SAXString = 'prefix-';

type

  TSAXStack = class(TStack)
  public
    property List;
  end;

{ TNSFilter }

constructor TNSFilter.Create(AOwner : TComponent; const next : IEventConsumer);
begin
  inherited Create(AOwner, next);
  nsStack:= TNamespaceSupport.Create();
  elementStack:= TSAXStack.Create;
  attributes:= TAttributesImpl.Create();

  setContentHandler(Self);
end;

destructor TNSFilter.Destroy;
var i : Integer;
begin
  nsStack.Free;
  for i:= 0 to elementStack.Count-1 do
    TSAXStringWrapper(TSAXStack(elementStack).List[i]).Free;
  elementStack.Free;
  attributes.Free;
  inherited;
end;

procedure TNSFilter.fatalError(const _message: SAXString);
var e : ISAXParseError;
    handler : IErrorHandler;
    locator : ILocator;
begin
  handler:= getErrorHandler();
  locator:= getDocumentLocator();

  if (locator = nil) then
    e:= TSAXParseError.Create(_message, nil, nil, -1, -1) as ISAXParseError
  else
    e:= TSAXParseError.Create(PSAXChar(_message), locator) as ISAXParseError;

  if (handler <> nil) then
    handler.fatalError(e);

  raise ESAXParseException.Create(e);
end;

procedure TNSFilter.startDocument;
var i : Integer;
begin
  for i:= 0 to elementStack.Count-1 do
    TSAXStringWrapper(TSAXStack(elementStack).List[i]).Free;
  TSAXStack(elementStack).List.Clear();
  nsStack.reset();
  pushedContext:= false;
  inherited startDocument();
end;

procedure TNSFilter.startPrefixMapping(const prefix, uri: SAXString);
var e : SAXStringArray;
    i : Integer;
    declared : SAXString;
begin
  if (pushedContext = false) then
  begin
    nsStack.pushContext();
    pushedContext:= true;
  end;

  // this check is awkward, but the paranoia prevents big trouble
  e:= nsStack.getDeclaredPrefixes();
  for i:= 0 to Length(e)-1 do
  begin
    declared:= e[i];
    if (not (declared = prefix)) then
      continue;
    if (uri = nsStack.getURI(prefix)) then
    begin
      Exit;
    end;
    fatalError('inconsistent binding for prefix ''' + prefix
      + ''' ... ' + uri + ' (was ' + nsStack.getURI(prefix) + ')');
  end;

  if (not nsStack.declarePrefix(prefix, uri)) then 
    fatalError('illegal prefix declared: ' + prefix);
end;

function TNSFilter.fixName(const ns, l, name: SAXString;
  const isAttr: Boolean): SAXString;
var temp, i : Integer;
    e : SAXStringArray;
    prefix : SAXString;
    uri : SAXString;
    n : SAXString;
    parts : TNamespaceParts;
begin
  n:= name;
  e:= nil;

  if ('' = n) then
  begin
    n:= l;
    if ('' = n) then
      fatalError('empty/null name');
  end;

  // can we correctly process the name as-is?
  // handles "element scope" attribute names here.
  parts:= nsStack.processName(n, nsTemp, isAttr);
  if ((parts[nsURI] <> '') and
      (parts[nsLocal] <> '') and
      (parts[nsQName] <> '') and
      (nsTemp[nsURI] = ns)) then
  begin
    Result:= nsTemp[nsQName];
    Exit;
  end;

  // nope, gotta modify the name or declare a default mapping

  // get rid of any current prefix
  temp:= Pos(':', n);
  //!! switched this to ">" not ">="
  if (temp > 0) then
  begin
    n:= Copy(n, temp + 1, Length(n));

    // ... maybe that's enough (use/prefer default namespace) ...
    parts:= nsStack.processName(n, nsTemp, false);
    if ((not isAttr) and
        (parts[nsURI] <> '') and
        (parts[nsLocal] <> '') and
        (parts[nsQName] <> '') and
        (nsTemp[nsURI] = ns)) then
    begin
      Result:= nsTemp[nsQName];
      Exit;
    end;
  end;

  // must we define and use the default/undefined prefix?
  if ('' = ns) then
  begin
    if (isAttr) then
      fatalError('processName bug');
    if (IAttributes(attributes).getIndex('xmlns') <> -1) then
      fatalError('need to undefine default NS, but it''s bound: ' + IAttributes(attributes).getValue('xmlns'));

    nsStack.declarePrefix('', '');
    attributes.addAttribute('', '', 'xmlns', 'CDATA', '');
    Result:= n;
    Exit;
  end;

  // is there at least one non-null prefix we can use?
  e:= nsStack.getDeclaredPrefixes();
  for i:= 0 to Length(e)-1 do
  begin
    prefix:= e[i];
    uri:= nsStack.getURI(prefix);
    if ((uri = '') or (not (uri = ns))) then
      continue;
    Result:= prefix + ':' + n;
    Exit;
  end;

  // no such luck.  create a prefix name, declare it, use it.
  temp:= 0;
  while (temp >= 0) do
  begin
    prefix:= prefixRoot + IntToStr(temp);
    if (nsStack.getURI(prefix) = '') then
    begin
      nsStack.declarePrefix(prefix, ns);
      attributes.addAttribute('', '', 'xmlns:' + prefix, 'CDATA', ns);
      Result:= prefix + ':' + n;
      Exit;
    end;

    Inc(temp);
  end;

  fatalError('too many prefixes genned');
  // NOTREACHED
  Result:= '';
end;

procedure TNSFilter.startElement(const uri, localName, qName: SAXString;
  const atts: IAttributes);
var len, i : Integer;
    prefix : SAXString;
    e : SAXStringArray;
    aName : SAXString;
    aNS : SAXString;
    aLocal : SAXString;
    aType : SAXString;
    aValue : SAXString;
    newName : SAXString;
begin
  if (not pushedContext) then
    nsStack.pushContext();
  pushedContext:= false;

  // make sure we have all NS declarations handy before we start
  len:= atts.getLength();

  for i:= 0 to len-1 do
  begin
    aName:= atts.getQName(i);

    if (not Pos('xmlns', aName) = 1) then
      continue;

    if ('xmlns' = aName) then
      prefix:= ''
    else if (Pos(':', aName) = 6) then
      prefix:= Copy(aName, 7, Length(aName))
    else // xmlnsfoo etc.
      continue;
    startPrefixMapping(prefix, atts.getValue(i));
  end;

  // put namespace decls at the start of our regenned attlist
  attributes.clear();
  e:= nsStack.getDeclaredPrefixes();
  for i:= 0 to Length(e)-1 do
  begin
    prefix:= e[i];

    if ('' = prefix) then
      attributes.addAttribute('', '', 'xmlns', 'CDATA', nsStack.getURI(prefix))
    else
      attributes.addAttribute('', '', 'xmlns:' + prefix, 'CDATA', nsStack.getURI(prefix));
  end;

  // name fixups:  element, then attributes.
  // fixName may declare a new prefix or, for the element,
  // redeclare the default (if element name needs it).
  newName:= fixName(uri, localName, qName, false);

  //!! Hmmmm should we be using len here? or a newly obatined length?
  for i:= 0 to len-1 do
  begin
    aName:= atts.getQName(i);
    aNS:= atts.getURI(i);
    aLocal:= atts.getLocalName(i);
    aType:= atts.getType(i);
    aValue:= atts.getValue(i);

    if (Pos('xmlns', aName) = 1) then
      continue;
    aName:= fixName(aNS, aLocal, aName, true);
    attributes.addAttribute(aNS, aLocal, aName, aType, aValue);
  end;

  elementStack.push(TSAXStringWrapper.Create(newName));

  // pass event along, with cleaned-up names and decls.
  inherited startElement(uri, localName, newName, attributes);
end;

procedure TNSFilter.endElement(const uri, localName, qName: SAXString);
var w : TSAXStringWrapper;
    name : SAXString;
begin
  nsStack.popContext();
  w:= TSAXStringWrapper(elementStack.pop());
  name:= w.Value;
  w.Free();
  inherited endElement(uri, localName, name);
end;

procedure TNSFilter.endPrefixMapping(const prefix: SAXString);
begin
  // * This call is not passed to the next consumer in the chain.
  // * Prefix declarations and scopes are only exposed in their
  // * attribute form.
end;

procedure TNSFilter.endDocument;
var i : Integer;
begin
  for i:= 0 to elementStack.Count-1 do
    TSAXStringWrapper(TSAXStack(elementStack).List[i]).Free;
  TSAXStack(elementStack).List.Clear();
  nsStack.reset();
  inherited endDocument();
end;


end.
