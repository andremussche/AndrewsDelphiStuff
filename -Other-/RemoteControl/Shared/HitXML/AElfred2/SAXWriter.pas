// Filename : SAXWriter.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit SAXWriter;

// "!!"
// "(*..*)"

// Canonical support is very very bad!!!!


interface

uses Classes, SysUtils, SAX, SAXHelpers, SAXExt;

type

  TSAXWriter = class(TXMLFilterImpl)
  private
    canonical : Boolean;
    started : Boolean;
    trimOutput : Boolean;
    encoding : SAXString;
    encodingType : Byte;
    isRoot : Boolean;
    inCDATA : Boolean;
    indent : Integer;
    state : TList;
    prefixMappings : SAXString;
    wantIndent: Boolean;
    wrapAttributes: Boolean;
  protected
    output : TStream;
    function sortAttributes(atts : IAttributes) : IAttributes;
  public
    constructor Create(); reintroduce; overload;
    constructor Create(const canonical : Boolean); reintroduce; overload;
    constructor Create(const encoding : SAXString; const canonical : Boolean); reintroduce; overload;
    destructor Destroy(); override;
    procedure write(const value : SAXString);
    procedure writecontent(const value : SAXString);
    procedure writeln();
    procedure flush();
    function normalize(s : SAXString) : SAXString;
    // Handlers
    function resolveEntity(const publicId, systemId : SAXString) : IInputSource; override;
    procedure notationDecl(const name, publicId, systemId : SAXString); override;
    procedure unparsedEntityDecl(const name, publicId, systemId,
      notationName : SAXString); override;
    procedure setDocumentLocator(const locator: ILocator); override;
    procedure startDocument(); override;
    procedure endDocument(); override;
    procedure startPrefixMapping(const prefix, uri : SAXString); override;
    procedure endPrefixMapping(const prefix : SAXString); override;
    procedure startElement(const namespaceURI, localName, qName: SAXString;
      const atts: IAttributes); override;
    procedure endElement(const namespaceUri, localName, qName: SAXString); override;
    procedure characters(const ch : SAXString); override;
    procedure ignorableWhitespace(const ch : SAXString); override;
    procedure processingInstruction(const target, data : SAXString); override;
    procedure skippedEntity(const name : SAXString); override;
    procedure warning(const e : ISAXParseError); override;
    procedure error(const e : ISAXParseError); override;
    procedure fatalError(const e : ISAXParseError); override;
    // Ext handler
    procedure startCDATA;
    procedure endCDATA;
    // Setters
    procedure setEncoding(const encoding : SAXString);
    procedure setCanonical(const canonical : Boolean);
    procedure setOutput(const output : TStream);
    procedure setTrimOutput(const trimOutput : Boolean);
    procedure setWantIndent(const wantIndent : Boolean);
    procedure setWrapAttributes(const wrapAttributes : Boolean);
  end;

  TSAXWriterEx = class(TSAXWriter)
  public
    procedure startElement(const tag : SAXString); reintroduce; overload;
    procedure startElement(const tag : SAXString; const atts : IAttributes);  reintroduce; overload;
    procedure endElement(const tag : SAXString); reintroduce; overload;
    procedure element(const tag, content : SAXString); overload;
    procedure element(const tag, content : SAXString; const atts : IAttributes); overload;
  end;


implementation

uses SAXDriver, Windows;


const

  UTF16_BOM : Integer = $FEFF;

  ENCODING_UTF8 = 0;
  ENCODING_UTF16 = 1;
  ENCODING_OTHER = 2; // All other encodings treated as 8 bit!

{ TSAXWriter }

constructor TSAXWriter.Create();
begin
  inherited Create;
  Self.canonical:= false;
  Self.output:= nil;
  Self.trimOutput:= false;
  Self.inCDATA:= false;
  if (canonical) then
  begin
    Self.wantIndent:= false;
    Self.wrapAttributes:= false;
  end else
  begin
    Self.wantIndent:= true;
    Self.wrapAttributes:= true;
  end;
  state:= TList.Create();
end;

constructor TSAXWriter.Create(const canonical: Boolean);
begin
  inherited Create;
  Self.canonical:= canonical;
  Self.output:= nil;
  Self.trimOutput:= false;
  Self.inCDATA:= false;
  if (canonical) then
  begin
    Self.wantIndent:= false;
    Self.wrapAttributes:= false;
  end else
  begin
    Self.wantIndent:= true;
    Self.wrapAttributes:= true;
  end;
  state:= TList.Create();
end;

constructor TSAXWriter.Create(const encoding: SAXString;
  const canonical: Boolean);
begin
  inherited Create;
  if (encoding = '') then
{$IFDEF SAX_WIDESTRINGS}
    Self.encoding:= 'UTF-16'
{$ELSE}
    Self.encoding:= 'UTF-8'
{$ENDIF}
  else
    Self.encoding:= encoding;
  Self.canonical:= canonical;
  Self.output:= nil;
  Self.trimOutput:= false;
  Self.inCDATA:= false;
  if (canonical) then
  begin
    Self.wantIndent:= false;
    Self.wrapAttributes:= false;
  end else
  begin
    Self.wantIndent:= true;
    Self.wrapAttributes:= true;
  end;
  state:= TList.Create();
end;

destructor TSAXWriter.Destroy;
begin
  state.Free;
  inherited;
end;

procedure TSAXWriter.processingInstruction(const target, data: SAXString);
begin
  inherited;
  if (not started) then
  begin
    started:= true;
    write('>');
  end;
  write('<?');
  write(target);
  if (data <> '') then
  begin
    write(' ');
    write(data);
  end;
  write('?>');
  flush();
end;

procedure TSAXWriter.startDocument;
begin
  inherited;
  state.clear;
  indent:= 0;
  isRoot:= true;
  started:= true;

  {$IFDEF SAX_WIDESTRINGS}
  if (encoding = '') or (AnsiSameText(encoding, 'UTF-16')) then
    encodingType:= ENCODING_UTF16
  else if (AnsiSameText(encoding, 'UTF-8')) then
    encodingType:= ENCODING_UTF8
  else
    encodingType:= ENCODING_OTHER;
  {$ELSE}
  if (encoding = '') or (AnsiSameText(encoding, 'UTF-8')) then
    encodingType:= ENCODING_UTF8
  else if (AnsiSameText(encoding, 'UTF-16')) then
    encodingType:= ENCODING_UTF16
  else
    encodingType:= ENCODING_OTHER;
  {$ENDIF}

  // Write the BOM in all cases?
  if (encodingType = ENCODING_UTF16) then
    write(SAXChar(UTF16_BOM));
  // Write the encoding
  if (not canonical) then
  begin
    write('<?xml version="1.0" encoding="');
    if (encoding <> '') then
      write(encoding)
    else
{$IFDEF SAX_WIDESTRINGS}
      write('UTF-16');
{$ELSE}
      write('UTF-8');
{$ENDIF}
    write('"?>');
    writeln();
    flush();
  end;
end;

procedure TSAXWriter.startPrefixMapping(const prefix, uri: SAXString);
var prefixMapping : SAXString;
    i : Integer;
begin
  inherited;
  if (prefix <> '') then
    prefixMapping:= 'xmlns:' + prefix + '="' + uri + '"'
  else
    prefixMapping:= 'xmlns="' + uri + '"';
  if (wrapAttributes) then
  begin
    prefixMappings:= prefixMappings + SAXChar(#13) + SAXChar(#10);
    for i:= 1 to Indent*2 do
      prefixMappings:= prefixMappings + ' ';
    prefixMappings:= prefixMappings + '  ' + prefixMapping;
  end else
    prefixMappings:= prefixMappings + ' ' + prefixMapping;
end;

procedure TSAXWriter.startElement(const namespaceURI, localName,
  qName: SAXString; const atts: IAttributes);
var len, i : Integer;
    sortedAtts : IAttributes;
begin
  inherited;
  if (not started) then
  begin
    started:= true;
    write('>');
  end;
  if (not isRoot) then
  begin
    if (Boolean(state[state.Count-1]) = false) then
    begin
      writeln();
    end;
  end;
  Inc(Indent);
  state.Add(Pointer(false));
  isRoot:= false;
  write('<');
  write(qName);
  write(prefixMappings);
  if (atts <> nil) then
  begin
    sortedAtts:= sortAttributes(atts);
    len:= sortedAtts.getLength();
    for i:= 0 to len-1 do
    begin
      if ((i > 0) or (prefixMappings <> '')) and (wrapAttributes) then
      begin
        writeln();
      end else
        write(' ');
      write(sortedAtts.getQName(i));
      write('="');
      write(normalize(sortedAtts.getValue(i)));
      write('"');
    end;
  end;
  prefixMappings:= '';
  started:= false;
  flush();
end;

procedure TSAXWriter.characters(const ch : SAXString);
var n : SAXString;
begin
  if (not inCDATA) then
  begin
    n:= normalize(ch);
    if (n = '') then
      Exit;
    writecontent(n);
  end else
    writecontent(ch);
  inherited;
end;

procedure TSAXWriter.endElement(const namespaceURI, localName,
  qName: SAXString);
begin
  Dec(indent);
  try
    // write an empty tag
    if (not started) then
    begin
      started:= true;
      write('/>');
      Exit;
    end;
    // Write on a new line
    if (not Boolean(state[state.Count-1])) then
    begin
      writeln();
    end;
    write('</');
    write(qName);
    write('>');
    flush();
  finally
    state.Delete(state.Count-1);
  end;
  inherited;
end;

procedure TSAXWriter.ignorableWhitespace(const ch : SAXString);
begin
  if (not wantIndent) then
    characters(ch);
  inherited;
end;

procedure TSAXWriter.warning(const e: ISAXParseError);
begin
  inherited;
  raise ESAXParseException.Create(e);
end;

procedure TSAXWriter.error(const e: ISAXParseError);
begin
  inherited;
  raise ESAXParseException.Create(e);
end;

procedure TSAXWriter.fatalError(const e: ISAXParseError);
begin
  inherited;
  raise ESAXParseException.Create(e);
end;

procedure TSAXWriter.startCDATA;
begin
  if (not started) then
  begin
    started:= true;
    write('>');
  end;
  state[state.Count-1]:= Pointer(true);
  write('<![CDATA[');
  flush();
  inCDATA:= True;
  inherited;
end;

procedure TSAXWriter.endCDATA;
begin
  inCDATA:= False;
  write(']]>');
  flush();
  inherited;
end;

function TSAXWriter.normalize(s: SAXString): SAXString;
var str : SAXString;
    len, i : Integer;
    ch : SAXChar;
begin
  str:= '';
  if (trimOutput) then
    s:= Trim(s);
  len:= Length(s);
  for i:= 1 to len do
  begin
    ch:= s[i];
    case (ch) of
      '<' :
      begin
        str:= str + '&lt;';
      end;
      '>' :
      begin
        str:= str + '&gt;';
      end;
      '&' :
      begin
        str:= str + '&amp;';
      end;
      '"' :
      begin
        str:= str + '&quot;';
      end;
      #0 :
      begin
        // #0 is stipped out
      end;
      #10,
      #13 :
      begin
        //!! if (canonical) then
        //!! begin
        //!!   str:= str + '&#';
        //!!   str:= str + IntToStr(Word(ch));
        //!!   str:= str + ';';
        //!! end else
        str:= str + ch;
      end;
      // else, default append char
      else begin
        str:= str + ch;
      end;
    end;
  end;

  Result:= str;
end;

function TSAXWriter.sortAttributes(atts: IAttributes): IAttributes;
begin
  // no op for now
  Result:= atts;
end;

procedure TSAXWriter.write(const value: SAXString);
var len : Integer;
{$IFDEF DELPHI6_UP}
  S : String;
{$ENDIF}
begin
  if (output = nil) then
    raise ESAXException.Create('output stream not set');
  len:= Length(value);
  if (len > 0) then
  begin
    {$IFDEF DELPHI6_UP} { Special UTF-8 Encoding for Delphi 6 }
    if (encodingType = ENCODING_UTF8) then
    begin
      S:= UTF8Encode(Value);
      len:= Length(S);
      if (output.write(Pointer(S)^, len) <> len) then
        raise ESAXException.Create('unable to write output');
      // Break out of function so we don't do "normal" output
      Exit;
    end;
    {$ENDIF}

    if (encodingType = ENCODING_UTF16) then
    begin
      len:= len * 2;
      if (output.write(Pointer(WideString(Value))^, len) <> len) then
        raise ESAXException.Create('unable to write output');
    end else
    begin
      if (output.write(Pointer(String(Value))^, len) <> len) then
        raise ESAXException.Create('unable to write output');
    end;
  end;
end;

procedure TSAXWriter.writeln;
var s : SAXString;
    i : Integer;
begin
  if (not wantIndent) then Exit;
  s:= '' + SAXChar(#13) + SAXChar(#10);
  write(s);
  for i:= 1 to Indent*2 do
    write(SAXChar(#32));
end;

procedure TSAXWriter.writecontent(const value : SAXString);
begin
  if (not started) then
  begin
    started:= true;
    write('>');
  end;
  if (state.Count > 0) then
    state[state.Count-1]:= Pointer(true);
  write(value);
  flush();
end;

procedure TSAXWriter.flush;
begin
  // no op
end;

procedure TSAXWriter.setOutput(const output: TStream);
begin
  Self.output:= output;
end;

procedure TSAXWriter.setCanonical(const canonical: Boolean);
begin
  Self.canonical:= canonical;
end;

procedure TSAXWriter.setEncoding(const encoding: SAXString);
begin
  Self.encoding:= encoding;
end;

procedure TSAXWriter.setTrimOutput(const trimOutput: Boolean);
begin
  Self.trimOutput:= trimOutput;
end;

procedure TSAXWriter.setWantIndent(const wantIndent: Boolean);
begin
  Self.wantIndent:= wantIndent;
end;

procedure TSAXWriter.setWrapAttributes(const wrapAttributes: Boolean);
begin
  Self.wrapAttributes:= wrapAttributes;
end;

procedure TSAXWriter.endDocument;
begin
  inherited;
end;

procedure TSAXWriter.endPrefixMapping(const prefix: SAXString);
begin
  inherited;
end;

procedure TSAXWriter.notationDecl(const name, publicId,
  systemId: SAXString);
begin
  inherited;
end;

function TSAXWriter.resolveEntity(const publicId,
  systemId: SAXString): IInputSource;
begin
  Result:= inherited resolveEntity(publicId, systemId);
end;

procedure TSAXWriter.setDocumentLocator(const locator: ILocator);
begin
  inherited;
end;

procedure TSAXWriter.skippedEntity(const name: SAXString);
begin
  inherited;
end;

procedure TSAXWriter.unparsedEntityDecl(const name, publicId, systemId,
  notationName: SAXString);
begin
  inherited;
end;

{ TSAXWriterEx }

procedure TSAXWriterEx.element(const tag, content: SAXString);
begin
  startElement(tag);
  characters(content);
  endElement(tag);
end;

procedure TSAXWriterEx.element(const tag, content: SAXString;
  const atts: IAttributes);
begin
  startElement(tag, atts);
  characters(content);
  endElement(tag);
end;

procedure TSAXWriterEx.endElement(const tag: SAXString);
begin
  inherited endElement('', tag, tag);
end;

procedure TSAXWriterEx.startElement(const tag: SAXString);
begin
  inherited startElement('', tag, tag, nil);
end;

procedure TSAXWriterEx.startElement(const tag: SAXString;
  const atts: IAttributes);
begin
  inherited startElement('', tag, tag, atts);
end;

end.

