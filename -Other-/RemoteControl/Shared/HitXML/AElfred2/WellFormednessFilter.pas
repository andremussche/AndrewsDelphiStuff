// Filename : WellFormednessFilter.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit WellFormednessFilter;

interface

uses Classes, SysUtils, Contnrs, SAX, SAXHelpers, SAXExt, Dialogs,
     EventFilter, EventConsumer;

type

  TWellFormednessFilter = class(TEventFilter)
  private
    startedDoc : Boolean;
    elementStack : TStack;
    startedCDATA : Boolean;
    dtdState : SAXString;
    function getError(const message : SAXString) : ISAXParseError;
  public
    constructor Create(AOwner : TComponent; const consumer : IEventConsumer);
      override;
    destructor Destroy(); override;
    procedure reset();
    procedure fatalError(const _message : SAXString);
    procedure setDocumentLocator(const locator: ILocator); override;
    procedure startDocument(); override;
    procedure startElement(const uri, localName, qName: SAXString; const atts: IAttributes); override;
    procedure endElement(const uri, localName, qName: SAXString); override;
    procedure endDocument(); override;
    procedure startDTD(const root, publicId, systemId : SAXString); override;
    procedure characters(const ch : SAXString); override;
    procedure ignorableWhitespace(const ch : SAXString); override;
    procedure processingInstruction(const target, data : SAXString); override;
    procedure comment(const ch : SAXString); override;
    procedure notationDecl(const name, publicId, systemId : SAXString); override;
    procedure unparsedEntityDecl(const name, publicId, systemId, notationName : SAXString); override;
    procedure endDTD(); override;
    procedure startCDATA(); override;
    procedure endCDATA(); override;
  end;

implementation

uses SAXDriver;

type

  TSAXStack = class(TStack)
  public
    property List;
  end;

{ TWellFormednessFilter }

constructor TWellFormednessFilter.Create(AOwner : TComponent;
  const consumer : IEventConsumer);
begin
  inherited Create(AOwner, consumer);
  dtdState:= 'before';
  elementStack:= TSAXStack.Create;
  setContentHandler(Self as IContentHandler);
  setDTDHandler(Self as IDTDHandler);
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

destructor TWellFormednessFilter.Destroy;
var i : Integer;
begin
  for i:= 0 to elementStack.Count-1 do
    TSAXStringWrapper(TSAXStack(elementStack).List[i]).Free;
  elementStack.Free();
  inherited;
end;

procedure TWellFormednessFilter.reset;
var i : Integer;
begin
  startedDoc:= false;
  startedCDATA:= false;
  for i:= 0 to elementStack.Count-1 do
    TSAXStringWrapper(TSAXStack(elementStack).List[i]).Free;
  TSAXStack(elementStack).List.Clear();
end;

function TWellFormednessFilter.getError(
  const message : SAXString) : ISAXParseError;
var locator : ILocator;
begin
  locator:= getDocumentLocator;
  if (locator = nil) then
  begin
    Result:= TSAXParseError.create(message, nil, nil, -1, -1) as ISAXParseError;
    Exit;
  end else
  begin
    Result:= TSAXParseError.create(PSAXChar(message),
      locator) as ISAXParseError;
    Exit;
  end;
end;

procedure TWellFormednessFilter.fatalError(
  const _message: SAXString);
var e : ISAXParseError;
    handler : IErrorHandler;
begin
  e:= getError(_message);
  handler:= getErrorHandler();

  if (handler <> nil) then
    handler.fatalError(e);
  raise ESAXParseException.Create(e);
end;

procedure TWellFormednessFilter.setDocumentLocator(
  const locator: ILocator);
begin
  if (startedDoc) then
    raise EIllegalStateException.Create('setDocumentLocator called after startDocument');
  inherited setDocumentLocator(locator);
end;

procedure TWellFormednessFilter.startDocument;
var i : Integer;
begin
  if (startedDoc) then
    fatalError('startDocument called more than once');
  startedDoc:= true;
  startedCDATA:= false;
  for i:= 0 to elementStack.Count-1 do
    TSAXStringWrapper(TSAXStack(elementStack).List[i]).Free;
  TSAXStack(elementStack).List.Clear;
  inherited startDocument();
end;

procedure TWellFormednessFilter.startElement(const uri, localName,
  qName: SAXString; const atts: IAttributes);
begin
  if (not startedDoc) then
    fatalError('callback outside of document?');
  if ('inside' = dtdState) then
    fatalError('element inside DTD?')
  else
    dtdState:= 'after';
  if (startedCDATA) then
    fatalError('element inside CDATA section');
  if ('' = qName) then
    fatalError('startElement name missing');
  elementStack.push(TSAXStringWrapper.Create(qName));
  inherited startElement(uri, localName, qName, atts);
end;

procedure TWellFormednessFilter.endElement(const uri, localName,
  qName: SAXString);
var top : SAXString;
    w : TSAXStringWrapper;
begin
  if (not startedDoc) then
    fatalError('callback outside of document?');
  if (startedCDATA) then
    fatalError('element inside CDATA section');
  if ('' = qName) then
    fatalError('endElement name missing');

  try
    //!! Added...
    if (elementStack.Count = 0) then
      raise EEmptyStackException.Create('element stack is empty');

    w:= TSAXStringWrapper(elementStack.pop());
    top:= w.Value;
    w.Free;

    if (not (qName = top)) then
      fatalError('<' + top + ' ...>...</' + qName + '>');
      // XXX could record/test namespace info
  except
    on e: EEmptyStackException do
    begin
      fatalError('endElement without startElement:  </' + qName + '>');
    end;
  end;
  inherited endElement(uri, localName, qName);
end;

procedure TWellFormednessFilter.endDocument;
begin
  if (not startedDoc) then
    fatalError('callback outside of document?');
  startedDoc:= false;
  dtdState:= 'before';
  inherited endDocument();
end;

procedure TWellFormednessFilter.startDTD(const root, publicId,
  systemId: SAXString);
begin
  if (not startedDoc) then
    fatalError('callback outside of document?');
  if ('before' <> dtdState) then
    fatalError('two DTDs?');
  if (not elementStack.Count = 0) then
    fatalError('DTD must precede root element');
  dtdState:= 'inside';
  inherited startDTD(root, publicId, systemId);
end;

procedure TWellFormednessFilter.characters(const ch : SAXString);
var here, e: integer;
begin
  if (elementStack.Count() = 0) then
    fatalError('characters must be in an element');
  here:= 1;
  e:= Length(ch);
  while (here <= e) do
  begin
    Inc(here);
    if (ch[here-1] <> ']') then
      continue;
    if (here > e) then  // potential problem ...
      continue;
    Inc(here);
    if (ch[here-1] <> ']') then
      continue;
    if (here > e) then  // potential problem ...
      continue;
    Inc(here);
    if (ch[here-1] = '>') then
      fatalError('character data can''t contain "]]>"');
  end;
  inherited characters(ch);
end;

procedure TWellFormednessFilter.ignorableWhitespace(const ch : SAXString);
var here, e: integer;
begin
  if (elementStack.Count() = 0) then
    fatalError('characters must be in an element');
  here:= 1;
  e:= Length(ch);
  while (here <= e) do
  begin
    Inc(here);
    if (ch[here-1] = #13) then
      fatalError('whitespace can''t contain CR');
  end;
  inherited ignorableWhitespace(ch);
end;

procedure TWellFormednessFilter.processingInstruction(const target,
  data : SAXString);
begin
  if (Pos(#13, data) > 0) then
    fatalError('PIs can''t contain CR');
  if (Pos('?>', data) > 0) then
    fatalError('PIs can''t contain "?>"');
end;

procedure TWellFormednessFilter.comment(const ch : SAXString);
var here, e: integer;
begin
  if (not startedDoc) then
    fatalError('callback outside of document?');
  if (startedCDATA) then
    fatalError('comments can''t nest in CDATA');
  here:= 1;
  e:= Length(ch);
  while (here <= e) do
  begin
    if (ch[here] = #13) then
      fatalError('comments can''t contain CR');
    Inc(here);
    if (ch[here-1] <> '-') then
      continue;
    if (here > e) then
      fatalError('comments can''t end with "--->"');
    Inc(here);
    if (ch[here-1] = '-') then
      fatalError('comments can''t contain "--"');
  end;
  inherited comment(ch);
end;

procedure TWellFormednessFilter.notationDecl(const name, publicId,
  systemId : SAXString);
begin
// FIXME: not all parsers will report startDTD() ...
// we'd rather insist we're "inside".
  if ('after' = dtdState) then
    fatalError('not inside DTD');
  inherited notationDecl(name, publicId, systemId);
end;

procedure TWellFormednessFilter.unparsedEntityDecl(const name, publicId,
  systemId, notationName : SAXString);
begin
// FIXME: not all parsers will report startDTD() ...
// we'd rather insist we're "inside".
  if ('after' = dtdState) then
    fatalError('not inside DTD');
  inherited unparsedEntityDecl(name, publicId, systemId, notationName);
end;

procedure TWellFormednessFilter.endDTD;
begin
  if (not startedDoc) then
    fatalError('callback outside of document?');
  if ('inside' <> dtdState) then
    fatalError('DTD ends without start?');
  dtdState:= 'after';
  inherited endDTD();
end;

procedure TWellFormednessFilter.startCDATA;
begin
  if (not startedDoc) then
    fatalError('callback outside of document?');
  if (startedCDATA) then
    fatalError('CDATA starts can''t nest');
  startedCDATA:= true;
  inherited startCDATA();
end;

procedure TWellFormednessFilter.endCDATA;
begin
  if (not startedDoc) then
    fatalError('callback outside of document?');
  if (not startedCDATA) then
    fatalError('CDATA end without start?');
  startedCDATA:= false;
  inherited endCDATA();
end;

end.
