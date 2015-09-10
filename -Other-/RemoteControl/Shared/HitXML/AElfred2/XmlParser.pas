// Filename : XmlParser.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit XmlParser;

// "!!"
// "(*..*)"

// Alternate encodings are may not work (byte shifting may be wrong)
// Bytes in Java are signed (-128..127) so checking b1 > 0 has significance

interface

uses Classes, SysUtils, Contnrs, SAX, SAXHelpers, SAXDriver, Hashtable, Dialogs;

{$DEFINE _DEBUG}
{$DEFINE _SAFELOOP}

const

  // parse from buffer, avoiding slow per-character readCh()
  USE_CHEATS = true;

  // don't waste too much space in Hashtables
  DEFAULT_ATTR_COUNT = 23;

  // Constants for element content type.
  CONTENT_UNDECLARED = 0;
  CONTENT_ANY = 1;
  CONTENT_EMPTY = 2;
  CONTENT_MIXED = 3;
  CONTENT_ELEMENTS = 4;

  // Constants for the entity type.
  ENTITY_UNDECLARED = 0;
  ENTITY_INTERNAL = 1;
  ENTITY_NDATA = 2;
  ENTITY_TEXT = 3;

  // Constants for supported encodings.  "external" is just a flag.
  ENCODING_EXTERNAL = 0;
  ENCODING_UTF_8 = 1;
  ENCODING_ISO_8859_1 = 2;
  ENCODING_UCS_2_12 = 3;
  ENCODING_UCS_2_21 = 4;
  ENCODING_UCS_4_1234 = 5;
  ENCODING_UCS_4_4321 = 6;
  ENCODING_UCS_4_2143 = 7;
  ENCODING_UCS_4_3412 = 8;
  ENCODING_ASCII = 9;

  // Constants for attribute default value.
  ATTRIBUTE_DEFAULT_UNDECLARED = 30;
  ATTRIBUTE_DEFAULT_SPECIFIED = 31;
  ATTRIBUTE_DEFAULT_IMPLIED = 32;
  ATTRIBUTE_DEFAULT_REQUIRED = 33;
  ATTRIBUTE_DEFAULT_FIXED = 34;

  // Constants for input.
  INPUT_NONE = 0;
  INPUT_INTERNAL = 1;
  INPUT_STREAM = 3;
  INPUT_READER = 5;

  // Flags for reading literals.

  // expand general entity refs (attribute values in dtd and content)
  LIT_ENTITY_REF = 2;

  // normalize this value (space chars) (attributes, public ids)
  LIT_NORMALIZE = 4;

  // literal is an attribute value
  LIT_ATTRIBUTE = 8;

  // don't expand parameter entities
  LIT_DISABLE_PE = 16;

  // don't expand [or parse] character refs
  LIT_DISABLE_CREF = 32;

  // don't parse general entity refs
  LIT_DISABLE_EREF = 64;

  // literal is a public ID value
  LIT_PUBID = 256;

  // Flags affecting PE handling in DTDs (if expandPE is true).
  // PEs expand with space padding, except inside literals.
  CONTEXT_NORMAL = 0;
  CONTEXT_LITERAL = 1;

  // Buffer for undecoded raw byte input.
  READ_BUFFER_MAX = 16384;

  // Buffer for parsed character data.
  DATA_BUFFER_INITIAL = 4096;

  // Buffer for parsed names.
  NAME_BUFFER_INITIAL = 1024;

  // Symbol table, for caching interned names.
  SYMBOL_TABLE_LENGTH = 1087;

type

  TXMLParser = class(TObject)
  private
    handler : TSAXDriver;
    stream : TStream; // Was (InputStream is) and (Reader reader)
    line : Integer;
    column : Integer;
    //POS
    saveLine : Integer;
    saveColumn : Integer;
    useSavePos : Boolean;
    sourceType : Integer;
    inputStack : TStack;
    externalEntity : TStream; // Was URLConnection?...
    encoding : Integer;
    currentByteCount : Integer;
    scratch : IStreamInputSource;
    readBuffer : SAXCharArray;
    readBufferPos : Integer;
    readBufferLength : Integer;
    readBufferOverflow : Integer;
    rawReadBuffer : TByteDynarray;
    dataBuffer : SAXCharArray;
    dataBufferPos : Integer;
    nameBuffer : SAXCharArray;
    nameBufferPos : Integer;
    docIsStandalone : Boolean;
    elementInfo : THashtable;
    entityInfo : THashtable;
    notationInfo : THashtable;
    skippedPE : Boolean;
    currentElement : SAXString;
    currentElementContent : Integer;
    entityStack : TStack;
    inLiteral : Boolean;
    expandPE : Boolean;
    peIsError : Boolean;
    // can't report entity expansion inside two constructs:
    // - attribute expansions (internal entities only)
    // - markup declarations (parameter entities only)
    doReport : Boolean;
    // symbolTable? for caching interned names
    tagAttributes : SAXStringArray;
    tagAttributePos : Integer;
    sawCR : Boolean;
    inCDATA : Boolean;
  protected
    // Error reporting.
    procedure error(_message : SAXString; const textFound, textExpected : SAXString); overload;
    procedure error(_message : SAXString; const textFound : SAXChar; const textExpected : SAXString); overload;
    procedure error(_message : SAXString); overload;
    // Major syntactic productions.
    procedure parseDocument();
    procedure parseComment();
    procedure parsePI();
    procedure parseCDSect();
    function parseProlog() : Boolean;
    procedure checkLegalVersion(const version : SAXString);
    function parseXMLDecl(const ignoreEncoding : Boolean) : SAXString;
    function parseTextDecl(const ignoreEncoding : Boolean) : SAXString;
    procedure setupDecoding(encodingName : SAXString);
    procedure parseMisc();
    procedure parseDoctypeDecl();
    procedure parseMarkupDecl();
    procedure parseElement(maybeGetSubset : Boolean);
    procedure parseAttribute(const name : SAXString);
    procedure parseEq();
    procedure parseETag();
    procedure parseContent();
    procedure parseElementDecl();
    procedure parseContentspec(const name : SAXString);
    procedure parseElements(const saved : SAXCharArray);
    procedure parseCp();
    procedure parseMixed(const saved : SAXCharArray);
    procedure parseAttListDecl();
    procedure parseAttDef(const elementName : SAXString);
    function readAttType() : SAXString;
    procedure parseEnumeration(const isNames : Boolean);
    procedure parseNotationType();
    procedure parseDefault(const elementName, name : SAXString; _type : SAXString; const enum : SAXString);
    procedure parseConditionalSect(const saved : SAXCharArray);
    procedure parseCharRef();
    procedure parseEntityRef(const externalAllowed : Boolean);
    procedure parsePEReference();
    procedure parseEntityDecl();
    procedure parseNotationDecl();
    procedure parseCharData();
    // High-level reading and scanning methods.
    procedure requireWhitespace();
    procedure skipWhitespace();
    function readNmToken(const isName : Boolean) : SAXString;
    function isExtender(const c : SAXChar) : Boolean;
    function readLiteral(const flags : Integer) : SAXString;
    function readExternalIds(const inNotation, isSubset : Boolean) : SAXStringArray;
    function isWhitespace(const c : SAXChar) : Boolean;
    // Utility routines.
    procedure dataBufferAppend(const c : SAXChar); overload;
    procedure dataBufferAppend(const s : SAXString); overload;
    procedure dataBufferAppend(const ch : SAXCharArray; const start, len : Integer); overload;
    procedure dataBufferNormalize();
    function dataBufferToString() : SAXString;
    procedure dataBufferFlush();
    procedure require(const delim : SAXString); overload;
    procedure require(const delim : SAXChar); overload;
    procedure extendArray(var arr : SAXCharArray; const currentSize, requiredSize : Integer);
    // XML query routines.
    function getContentType(const element : TObject; const defaultType : Integer) : Integer;
    procedure setElement(const name : SAXString; const contentType : Integer; const contentModel : SAXString; const attributes : THashtable);
    function getElementAttributes(const name : SAXString) : THashtable;
    function declaredAttributes(const element : TObject) : SAXStringArray; overload;
    procedure setAttribute(const elName, name, _type : SAXString; const enumeration, value : SAXString; const valueType : Integer);
    function getAttribute(const elName, name : SAXString) : TObject;
    procedure setInternalEntity(const eName, value : SAXString);
    procedure setExternalEntity(const eName: SAXString; const eClass: Integer;
      const ids : SAXStringArray; const nName: SAXString);
    procedure setNotation(const nname : SAXString; const ids : SAXStringArray);
    // High-level I/O.
    function readCh() : SAXChar;
    procedure unread(const c : SAXChar); overload;
    procedure unread(const ch : SAXCharArray; const length : Integer); overload;
    procedure pushURL(isPE : Boolean; const ename: SAXString; const ids : SAXStringArray; stream: TStream; encoding: SAXString; doResolve : Boolean);
    function tryEncodingDecl(const ignoreEncoding : Boolean) : SAXString;
    procedure detectEncoding();
    class function tryEncoding(const sig : TByteDynarray; const b1, b2, b3, b4 : Byte) : Boolean; overload;
    class function tryEncoding(const sig : TByteDynarray; const b1, b2 : Byte) : Boolean; overload;
    procedure pushString(const ename, s : SAXString);
    procedure pushCharArray(const ename : SAXString; const ch : SAXCharArray; const start, length : Integer);
    procedure pushInput(const ename : SAXString);
    procedure popInput();
    function tryRead(const delim : SAXChar) : Boolean; overload;
    function tryRead(const delim : SAXString) : Boolean; overload;
    function tryRead(const delim : SAXCharArray) : Boolean; overload;
    function tryWhitespace() : Boolean;
    procedure parseUntil(const delim : SAXString); overload;
    procedure parseUntil(const delim : SAXCharArray); overload;
    procedure prefetchASCIIEncodingDecl();
    // Low-level I/O.
    procedure readDataChunk();
    procedure filterCR(const moreData : Boolean);
    procedure copyUtf8ReadBuffer(const count : Integer);
    function getNextUtf8Byte(const pos, count : Integer) : Integer;
    procedure copyIso8859_1ReadBuffer(const count : Integer; const mask : SAXChar);
    procedure copyUcs2ReadBuffer(const count, shift1, shift2 : Integer);
    procedure copyUcs4ReadBuffer(const count, shift1, shift2, shift3, shift4 : Integer);
    procedure encodingError(_message : SAXString; const value, offset : Integer);
    // Local Variables.
    procedure initializeVariables();
    procedure cleanupVariables();
  public
    constructor Create();
    destructor Destroy(); override;
    procedure setHandler(const driver : TSAXDriver);
    procedure doParse(const systemId, publicId : SAXString; const stream : TStream; encoding : SAXString);
    procedure doParseDTD(const systemId, publicId : SAXString; const stream : TStream; encoding : SAXString);
    function intern(const ch : SAXCharArray; const start, len : Integer) : SAXString;
    function isStandalone() : Boolean;
    function getElementContentType(const name : SAXString) : Integer;
    function declaredAttributes(const elName : SAXString) : SAXStringArray; overload;
    function getAttributeType(const name, aname : SAXString) : SAXString;
    function getAttributeEnumeration(const name, aname : SAXString) : SAXString;
    function getAttributeDefaultValue(const name, aname : SAXString) : SAXString;
    // !! function getAttributeExpandedValue(const name, aname : SAXString) : SAXString;
    function getAttributeDefaultValueType(const name, aname : SAXString) : Integer;
    function getEntityType(const ename : SAXString) : Integer;
    function getEntityIds(const ename : SAXString) : SAXStringArray;
    function getEntityValue(const ename : SAXString) : SAXString;
    function getLineNumber() : Integer;
    function getColumnNumber : Integer;
  end;

  function isExtender(const c : SAXChar) : Boolean;
  function isDigit(const c : SAXChar) : Boolean;
  function isCombiningChar(const c : SAXChar) : Boolean;
  function isIdeographic(const c : SAXChar) : Boolean;
  function isBaseChar(const c : SAXChar) : Boolean;
  function isLetter(const c : SAXChar) : Boolean;
  function isPubidChar(const c : SAXChar) : Boolean;

implementation

const

  startDelimComment = SAXString('<!--');
  endDelimComment = SAXString('--');
  startDelimPI = SAXString('<?');
  endDelimPI = SAXString('?>');
  endDelimCDATA = SAXString(']]>');


type

  TSAXDriverCracker = class(TSAXDriver);

  TSAXStack = class(TStack)
  public
    property List;
  end;

  // Appendix B - Unicode Constructs

  // 84    Letter    ::=    BaseChar , Ideographic
  function isLetter(const c : SAXChar) : Boolean;
  begin
    Result:= (isBaseChar(c)) or (isIdeographic(c));
  end;

  // 85    BaseChar
  function isBaseChar(const c : SAXChar) : Boolean;
  begin
    case (Word(c)) of
      $0041..$005A , $0061..$007A , $00C0..$00D6 , $00D8..$00F6 ,
      $00F8..$00FF , $0100..$0131 , $0134..$013E , $0141..$0148 ,
      $014A..$017E , $0180..$01C3 , $01CD..$01F0 , $01F4..$01F5 ,
      $01FA..$0217 , $0250..$02A8 , $02BB..$02C1 , $0386 ,
      $0388..$038A , $038C , $038E..$03A1 , $03A3..$03CE ,
      $03D0..$03D6 , $03DA , $03DC , $03DE , $03E0 , $03E2..$03F3 ,
      $0401..$040C , $040E..$044F , $0451..$045C , $045E..$0481 ,
      $0490..$04C4 , $04C7..$04C8 , $04CB..$04CC , $04D0..$04EB ,
      $04EE..$04F5 , $04F8..$04F9 , $0531..$0556 , $0559 ,
      $0561..$0586 , $05D0..$05EA , $05F0..$05F2 , $0621..$063A ,
      $0641..$064A , $0671..$06B7 , $06BA..$06BE , $06C0..$06CE ,
      $06D0..$06D3 , $06D5 , $06E5..$06E6 , $0905..$0939 , $093D ,
      $0958..$0961 , $0985..$098C , $098F..$0990 , $0993..$09A8 ,
      $09AA..$09B0 , $09B2 , $09B6..$09B9 , $09DC..$09DD ,
      $09DF..$09E1 , $09F0..$09F1 , $0A05..$0A0A , $0A0F..$0A10 ,
      $0A13..$0A28 , $0A2A..$0A30 , $0A32..$0A33 , $0A35..$0A36 ,
      $0A38..$0A39 , $0A59..$0A5C , $0A5E , $0A72..$0A74 ,
      $0A85..$0A8B , $0A8D , $0A8F..$0A91 , $0A93..$0AA8 ,
      $0AAA..$0AB0 , $0AB2..$0AB3 , $0AB5..$0AB9 , $0ABD , $0AE0 ,
      $0B05..$0B0C , $0B0F..$0B10 , $0B13..$0B28 , $0B2A..$0B30 ,
      $0B32..$0B33 , $0B36..$0B39 , $0B3D , $0B5C..$0B5D ,
      $0B5F..$0B61 , $0B85..$0B8A , $0B8E..$0B90 , $0B92..$0B95 ,
      $0B99..$0B9A , $0B9C , $0B9E..$0B9F , $0BA3..$0BA4 ,
      $0BA8..$0BAA , $0BAE..$0BB5 , $0BB7..$0BB9 , $0C05..$0C0C ,
      $0C0E..$0C10 , $0C12..$0C28 , $0C2A..$0C33 , $0C35..$0C39 ,
      $0C60..$0C61 , $0C85..$0C8C , $0C8E..$0C90 , $0C92..$0CA8 ,
      $0CAA..$0CB3 , $0CB5..$0CB9 , $0CDE , $0CE0..$0CE1 ,
      $0D05..$0D0C , $0D0E..$0D10 , $0D12..$0D28 , $0D2A..$0D39 ,
      $0D60..$0D61 , $0E01..$0E2E , $0E30 , $0E32..$0E33 ,
      $0E40..$0E45 , $0E81..$0E82 , $0E84 , $0E87..$0E88 , $0E8A ,
      $0E8D , $0E94..$0E97 , $0E99..$0E9F , $0EA1..$0EA3 , $0EA5 ,
      $0EA7 , $0EAA..$0EAB , $0EAD..$0EAE , $0EB0 , $0EB2..$0EB3 ,
      $0EBD , $0EC0..$0EC4 , $0F40..$0F47 , $0F49..$0F69 ,
      $10A0..$10C5 , $10D0..$10F6 , $1100 , $1102..$1103 ,
      $1105..$1107 , $1109 , $110B..$110C , $110E..$1112 , $113C ,
      $113E , $1140 , $114C , $114E , $1150 , $1154..$1155 , $1159 ,
      $115F..$1161 , $1163 , $1165 , $1167 , $1169 , $116D..$116E ,
      $1172..$1173 , $1175 , $119E , $11A8 , $11AB , $11AE..$11AF ,
      $11B7..$11B8 , $11BA , $11BC..$11C2 , $11EB , $11F0 , $11F9 ,
      $1E00..$1E9B , $1EA0..$1EF9 , $1F00..$1F15 , $1F18..$1F1D ,
      $1F20..$1F45 , $1F48..$1F4D , $1F50..$1F57 , $1F59 , $1F5B ,
      $1F5D , $1F5F..$1F7D , $1F80..$1FB4 , $1FB6..$1FBC , $1FBE ,
      $1FC2..$1FC4 , $1FC6..$1FCC , $1FD0..$1FD3 , $1FD6..$1FDB ,
      $1FE0..$1FEC , $1FF2..$1FF4 , $1FF6..$1FFC , $2126 ,
      $212A..$212B , $212E , $2180..$2182 , $3041..$3094 ,
      $30A1..$30FA , $3105..$312C , $AC00..$D7A3 :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  // 86    Ideographic
  function isIdeographic(const c : SAXChar) : Boolean;
  begin
    case (Word(c)) of
      $4E00..$9FA5 , $3007 , $3021..$3029 :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  // 87    CombiningChar
  function isCombiningChar(const c : SAXChar) : Boolean;
  begin
    case (Word(c)) of
      $0300..$0345 , $0360..$0361 , $0483..$0486 , $0591..$05A1 ,
      $05A3..$05B9 , $05BB..$05BD , $05BF , $05C1..$05C2 , $05C4 ,
      $064B..$0652 , $0670 , $06D6..$06DC , $06DD..$06DF ,
      $06E0..$06E4 , $06E7..$06E8 , $06EA..$06ED , $0901..$0903 ,
      $093C , $093E..$094C , $094D , $0951..$0954 , $0962..$0963 ,
      $0981..$0983 , $09BC , $09BE , $09BF , $09C0..$09C4 ,
      $09C7..$09C8 , $09CB..$09CD , $09D7 , $09E2..$09E3 , $0A02 ,
      $0A3C , $0A3E , $0A3F , $0A40..$0A42 , $0A47..$0A48 ,
      $0A4B..$0A4D , $0A70..$0A71 , $0A81..$0A83 , $0ABC ,
      $0ABE..$0AC5 , $0AC7..$0AC9 , $0ACB..$0ACD , $0B01..$0B03 ,
      $0B3C , $0B3E..$0B43 , $0B47..$0B48 , $0B4B..$0B4D ,
      $0B56..$0B57 , $0B82..$0B83 , $0BBE..$0BC2 , $0BC6..$0BC8 ,
      $0BCA..$0BCD , $0BD7 , $0C01..$0C03 , $0C3E..$0C44 ,
      $0C46..$0C48 , $0C4A..$0C4D , $0C55..$0C56 , $0C82..$0C83 ,
      $0CBE..$0CC4 , $0CC6..$0CC8 , $0CCA..$0CCD , $0CD5..$0CD6 ,
      $0D02..$0D03 , $0D3E..$0D43 , $0D46..$0D48 , $0D4A..$0D4D ,
      $0D57 , $0E31 , $0E34..$0E3A , $0E47..$0E4E , $0EB1 ,
      $0EB4..$0EB9 , $0EBB..$0EBC , $0EC8..$0ECD , $0F18..$0F19 ,
      $0F35 , $0F37 , $0F39 , $0F3E , $0F3F , $0F71..$0F84 ,
      $0F86..$0F8B , $0F90..$0F95 , $0F97 , $0F99..$0FAD ,
      $0FB1..$0FB7 , $0FB9 , $20D0..$20DC , $20E1 , $302A..$302F ,
      $3099 , $309A :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  // 88    Digit
  function isDigit(const c : SAXChar) : Boolean;
  begin
    case (Word(c)) of
      $0030..$0039 , $0660..$0669 , $06F0..$06F9 , $0966..$096F ,
      $09E6..$09EF , $0A66..$0A6F , $0AE6..$0AEF , $0B66..$0B6F ,
      $0BE7..$0BEF , $0C66..$0C6F , $0CE6..$0CEF , $0D66..$0D6F ,
      $0E50..$0E59 , $0ED0..$0ED9 , $0F20..$0F29 :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  // 89    Extender
  function isExtender(const c : SAXChar) : Boolean;
  begin
    case (Word(c)) of
      $00B7 , $02D0 , $02D1 , $0387 , $0640 , $0E46 , $0EC6 , $3005 ,
      $3031..$3035 , $309D..$309E , $30FC..$30FE :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  function isPubidChar(const c : SAXChar) : Boolean;
  begin
    case (c) of
      #32, #13, #10, 'a'..'z', 'A'..'Z', '0'..'9', '-', '''', '(', ')',
      '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%' :
      Result:= True;
    else
      Result:= False;
    end;
  end;

{ TXMLParser }

constructor TXMLParser.Create;
begin
{$IFDEF DEBUG}
Inc(hcreated);
{$ENDIF}
  inherited Create();
  cleanupVariables();
end;

destructor TXMLParser.Destroy;
begin
{$IFDEF DEBUG}
Inc(hfreed);
{$ENDIF}
  inherited Destroy();
end;

procedure TXMLParser.setHandler(const driver: TSAXDriver);
begin
  handler:= driver;
end;

procedure TXMLParser.doParse(const systemId, publicId: SAXString;
  const stream: TStream; encoding: SAXString);
var ids : SAXStringArray;
begin
  if (handler = nil) then
    raise EIllegalStateException.Create('no callback handler');

  initializeVariables();

  // predeclare the built-in entities here (replacement texts)
  // we don't need to intern(), since we're guaranteed literals
  // are always (globally) interned.
  setInternalEntity('amp', '&#38;');
  setInternalEntity('lt', '&#60;');
  setInternalEntity('gt', '&#62;');
  setInternalEntity('apos', '&#39;');
  setInternalEntity('quot', '&#34;');

  SetLength(ids, 3);
  ids[0]:= publicId;
  ids[1]:= systemId;
  // default baseURI: ''
  ids[2]:= '';

  // !! pushURL and startDocument moved into try block
  try
    pushURL(false, '[document]', ids, stream, encoding, false);
    handler.startDocument();
    parseDocument();
  finally
    cleanupVariables();
  end;

end;

procedure TXMLParser.doParseDTD(const systemId, publicId: SAXString;
  const stream: TStream; encoding: SAXString);
var ids : SAXStringArray;
begin
  if (handler = nil) then
    raise EIllegalStateException.Create('no callback handler');

  initializeVariables();

  // predeclare the built-in entities here (replacement texts)
  // we don't need to intern(), since we're guaranteed literals
  // are always (globally) interned.
  setInternalEntity('amp', '&#38;');
  setInternalEntity('lt', '&#60;');
  setInternalEntity('gt', '&#62;');
  setInternalEntity('apos', '&#39;');
  setInternalEntity('quot', '&#34;');

  SetLength(ids, 3);
  ids[0]:= publicId;
  ids[1]:= systemId;
  // default baseURI: ''
  ids[2]:= '';

  try
    pushURL(false, '[dtd]', ids, stream, encoding, false);
    skipWhitespace();
    // loop until the subset ends
    while (true) do
    begin
      try
        expandPE:= true;
        doReport:= true;
        skipWhitespace();
        expandPE:= false;
        doReport:= false;
      except
        on e : EEOFException do
        begin
          Exit;
        end;
      end;
      // WFC, PEs in internal subset (only between decls)
      expandPE:= true;
      peIsError:= false;
      parseMarkupdecl();
      expandPE:= false;
    end;
  finally
    cleanupVariables();
  end;
end;

procedure TXMLParser.error(_message : SAXString; const textFound,
  textExpected: SAXString);
begin
  if (textFound <> '') then
    _message:= _message + ' (found "' + textFound + '")';

  if (textExpected <> '') then
    _message:= _message + ' (expected "' + textExpected + '")';

  error(_message);

  // "can't happen"
  raise ESAXException.Create(_message);
end;

procedure TXMLParser.error(_message: SAXString;
  const textFound: SAXChar; const textExpected: SAXString);
begin
  error(_message, '' + textFound, textExpected);
end;

procedure TXMLParser.error(_message: SAXString);
begin
  handler.fatal(_message);
end;

procedure TXMLParser.parseDocument;
var c : SAXChar;
    sawDTD : Boolean;
begin
  try // added by MHK
    sawDTD:= parseProlog();
    require(SAXChar('<'));
    parseElement(not sawDTD);
  except // added by MHK
    on ee : EEOFException do
    begin
      error('premature end of file', '[EOF]', '');
    end;
  end;

  try
    parseMisc(); // skip all white, PIs, and comments
    c:= readCh(); // if this doesn't throw an exception...
    error('unexpected characters after document end', c, '');
  except
    on e : EEOFException do
    begin
      Exit;
    end;
  end;
end;

procedure TXMLParser.parseComment;
var saved : Boolean;
begin
    saved:= expandPE;

  expandPE:= false;
  parseUntil(endDelimComment);
  require(SAXChar('>'));
  expandPE:= saved;
  handler.comment(dataBuffer, 0, dataBufferPos);
  dataBufferPos:= 0;
end;

procedure TXMLParser.parsePI;
var name : SAXString;
    saved : Boolean;
begin
  saved:= expandPE;

  expandPE:= false;
  name:= readNmToken(true);
  if (CompareText('xml', name) = 0) then
    error('illegal processing instruction argument', name, '');

  if (not tryRead(endDelimPI)) then
  begin
    requireWhitespace();
    parseUntil(endDelimPI);
  end;
  expandPE:= saved;
  handler.processingInstruction(name, dataBufferToString());
end;

procedure TXMLParser.parseCDSect;
begin
  parseUntil(endDelimCDATA);
  dataBufferFlush();
end;

function TXMLParser.parseProlog : Boolean;
begin
  parseMisc();

  if (tryRead('<!DOCTYPE')) then
  begin
    parseDoctypedecl();
    parseMisc();
    Result:= True;
    Exit;
  end;
  Result:= False;
end;

procedure TXMLParser.checkLegalVersion(const version : SAXString);
var I, len : Integer;
    c : SAXChar;
begin
  len:= Length(version);
  for i:= 1 to len do
  begin
    c:= version[I];
    if (('0' <= c) and (c <= '9')) then
      continue;
    if ((c = '_') or (c = '.') or (c = ':') or (c = '-')) then
      continue;
    if (('a' <= c) and (c <= 'z')) then
      continue;
    if (('A' <= c) and (c <= 'Z')) then
      continue;
    saveColumn:= saveColumn + I;  
    error('illegal character in version', version, '1.0');
  end;

end;

function TXMLParser.parseXMLDecl(const ignoreEncoding : Boolean) : SAXString;
var
  version : SAXString;
  encodingName : SAXString;
  standalone : SAXString;
  white : Boolean;
  flags : Integer;
begin

  flags:= LIT_DISABLE_CREF or LIT_DISABLE_PE or LIT_DISABLE_EREF;

  // Read the version.
  require('version');
  parseEq();
  useSavePos:= True;
  saveLine:= line;
  saveColumn:= column;
  version:= readLiteral(flags);
  checkLegalVersion(version);
  if (not (version = '1.0')) then
  begin
    handler.warn('expected XML version 1.0, not: ' + version);
  end;
  useSavePos:= False;

  // Try reading an encoding declaration.
  white:= tryWhitespace();
  if (tryRead('encoding')) then
  begin
    if (not white) then
      error('whitespace required before ''encoding=''');
    parseEq();
    encodingName:= readLiteral(flags);
    if (not ignoreEncoding) then
      setupDecoding(encodingName);
  end;

  // Try reading a standalone declaration
  if (encodingName <> '') then
    white:= tryWhitespace();
  if (tryRead ('standalone')) then
  begin
    if (not white) then
      error('whitespace required before ''standalone=''');
    parseEq();
    standalone:= readLiteral(flags);
    if ('yes' = standalone) then
      docIsStandalone:= true
    else if ('no' <> standalone) then
      error('standalone flag must be ''yes'' or ''no''');
  end;

  skipWhitespace();
  require('?>');

  Result:= encodingName;
  Exit;
end;

function TXMLParser.parseTextDecl(const ignoreEncoding : Boolean) : SAXString;
var encodingName : SAXString;
    version : SAXString;
    flags : Integer;
begin
  flags:= LIT_DISABLE_CREF or LIT_DISABLE_PE or LIT_DISABLE_EREF;

  // Read an optional version.
  if (tryRead('version')) then
  begin
    parseEq();
    version:= readLiteral(flags);
    checkLegalVersion(version);
    if (not (version = '1.0')) then
    begin
      handler.warn('expected XML version 1.0, not: ' + version);
    end;
    requireWhitespace();
  end;


  // Read the encoding.
  require('encoding');
  parseEq();
  encodingName:= readLiteral(flags);
  if (not ignoreEncoding) then
    setupDecoding(encodingName);

  skipWhitespace();
  require('?>');

  Result:= encodingName;
  Exit;
end;

procedure TXMLParser.setupDecoding(encodingName: SAXString);
var encodingNameUpper : SAXString;
begin
  encodingNameUpper:= UpperCase(encodingName);

  // ENCODING_EXTERNAL indicates an encoding that wasn't
  // autodetected ... we can use builtin decoders, or
  // ones from the JVM(InputStreamReader).

  // Otherwise we can only tweak what was autodetected, and
  // only for single byte(ASCII derived) builtin encodings.

  // ASCII-derived encodings
  if ((encoding = ENCODING_UTF_8) or (encoding = ENCODING_EXTERNAL)) then
  begin
    if ((encodingNameUpper = 'ISO-8859-1') or
        (encodingNameUpper = '8859_1') or
        (encodingNameUpper = 'ISO8859_1')) then
    begin
      encoding:= ENCODING_ISO_8859_1;
      Exit;
    end else if ((encodingNameUpper = 'US-ASCII') or
                 (encodingNameUpper = 'ASCII')) then
    begin
      encoding:= ENCODING_ASCII;
      Exit;
    end else if ((encodingNameUpper = 'UTF-8') or
                 (encodingNameUpper = 'UTF8')) then
    begin
      encoding:= ENCODING_UTF_8;
      Exit;
    end else if (encoding <> ENCODING_EXTERNAL) then
    begin
      // used to start with a new reader ...
      raise EUnsupportedEncodingException.Create(encodingName);
    end;
    // else fallthrough ...
    // it's ASCII-ish and something other than a builtin
  end;

  // Unicode and such
  if ((encoding = ENCODING_UCS_2_12) or (encoding = ENCODING_UCS_2_21)) then
  begin
    if (not ((encodingNameUpper = 'ISO-10646-UCS-2') or
             (encodingNameUpper = 'UTF-16') or
             (encodingNameUpper = 'UTF-16BE') or
             (encodingNameUpper = 'UTF-16LE'))) then
      error('unsupported Unicode encoding', encodingNameUpper,'UTF-16');
    Exit;
  end;

  // four byte encodings
  if ((encoding = ENCODING_UCS_4_1234) or
      (encoding = ENCODING_UCS_4_4321) or
      (encoding = ENCODING_UCS_4_2143) or
      (encoding = ENCODING_UCS_4_3412)) then
  begin
    // Strictly:  "UCS-4" == "UTF-32BE"; also, "UTF-32LE" exists
    if (not (encodingNameUpper = 'ISO-10646-UCS-4')) then
      error('unsupported 32-bit encoding', encodingNameUpper, 'ISO-10646-UCS-4');
    Exit;
  end;

  // assert encoding == ENCODING_EXTERNAL
  // if(encoding not ENCODING_EXTERNAL)
  //     throw new RuntimeException('encoding = ' + encoding);

  if (encodingNameUpper = 'UTF-16BE') then
  begin
    encoding:= ENCODING_UCS_2_12;
    Exit;
  end;

  if (encodingNameUpper = 'UTF-16LE') then
  begin
    encoding:= ENCODING_UCS_2_21;
    Exit;
  end;

  // We couldn't use the builtin decoders at all.  But we can try to
  // create a reader, since we haven't messed up buffering.  Tweak
  // the encoding name if necessary.

  (*if(encodingName.equals('UTF-16')
    || encodingName.equals('ISO-10646-UCS-2'))
      encodingName = 'Unicode';
  // Ignoring all the EBCDIC aliases here

  reader = new InputStreamReader(is, encodingName);
  sourceType = INPUT_READER;*)

   //!! We are not handling others yet?
   error('unsupported encoding', encodingName, '');


end;

procedure TXMLParser.parseMisc;
begin
  while (true) do
  begin
    skipWhitespace();
    if (tryRead(startDelimPI)) then
    begin
      parsePI();
    end else if (tryRead(startDelimComment)) then begin
      parseComment();
    end else begin
      Exit;
    end;
  end;
end;

procedure TXMLParser.parseDoctypeDecl;
var rootName : SAXString;
    ids : SAXStringArray;
    subids : SAXStringArray;
    subset : IInputSource;
    isis : IStreamInputSource;
    isis_stream : TStream;
begin
  // Read the document type name.
  requireWhitespace();
  rootName:= readNmtoken(true);

  // Read the External subset's IDs
  skipWhitespace();
  ids:= readExternalIds(false, true);

  // report (a) declaration of name, (b) lexical info (ids)
  handler.doctypeDecl(rootName, ids[0], ids[1]);

  // Internal subset is parsed first, if present
  skipWhitespace();
  if (tryRead(SAXChar('['))) then
  begin
    // loop until the subset ends
    while (true) do
    begin
      expandPE:= true;
      doReport:= true;
      skipWhitespace();
      expandPE:= false;
      doReport:= false;
      if (tryRead(SAXChar(']'))) then
      begin
        break;     // end of subset
      end else begin
        // WFC, PEs in internal subset (only between decls)
        expandPE:= true;
        peIsError:= true;
        parseMarkupdecl();
        expandPE:= false;
        peIsError:= false;
      end;
    end;
  end;

  skipWhitespace();
  require(SAXChar('>'));

  // Read the external subset, if any
  if (ids[1] = '') then
    subset:= handler.getExternalSubset(rootName,
      handler.getSystemId())
  else
    subset:= nil;

  if ((ids[1] <> '') or (subset <> nil)) then
  begin
    pushString('', '>');
    // NOTE:  [dtd] is so we say what SAX2 expects,
    // even though it's misleading (subset, not entire dtd)
    if (ids[1] <> '') then
      pushURL(true, '[dtd]', ids, nil, '', true)
    else begin
      handler.warn('modifying document by adding external subset');
      SetLength(subids, 3);
      subids[0]:= subset.getPublicId();
      subids[1]:= subset.getSystemId();
      subids[2]:= '';
      if (Supports(subset, IStreamInputSource, isis)) then
        isis_stream:= isis.getByteStream()
      else
        isis_stream:= nil;
      pushURL(true, '[dtd]', subids,
        isis_stream,
        subset.getEncoding (),
        false); 
    end;

    // Loop until we end up back at '>'
    while (true) do
    begin
      expandPE:= true;
      doReport:= true;
      skipWhitespace();
      expandPE:= false;
      doReport:= false;
      if (tryRead(SAXChar('>'))) then
      begin
        break;
      end else begin
        expandPE:= true;
        parseMarkupdecl();
        expandPE:= false;
      end;
    end;

    // the ">" string isn't popped yet
    if (inputStack.Count() <> 1) then
      error('external subset has unmatched ''>''');

  end;

  // done dtd
  handler.endDoctype();
  expandPE:= false;
  doReport:= true;
end;

procedure TXMLParser.parseMarkupDecl;
var saved : SAXCharArray;
    savedPE : Boolean;
begin
  saved:= nil;
  savedPE:= expandPE;

  // prevent "<%foo;" and ensures saved entity is right
  require(SAXChar('<'));
  unread(SAXChar('<'));
  expandPE:= false;

  if (tryRead('<!ELEMENT')) then
  begin
    saved:= readBuffer;
    expandPE:= savedPE;
    parseElementDecl();
  end
  else if (tryRead('<!ATTLIST')) then
  begin
    saved:= readBuffer;
    expandPE:= savedPE;
    parseAttlistDecl();
  end
  else if (tryRead('<!ENTITY')) then
  begin
    saved:= readBuffer;
    expandPE:= savedPE;
    parseEntityDecl();
  end
  else if (tryRead('<!NOTATION')) then
  begin
    saved:= readBuffer;
    expandPE:= savedPE;
    parseNotationDecl();
  end
  else if (tryRead(startDelimPI)) then
  begin
    saved:= readBuffer;
    expandPE:= savedPE;
    parsePI();
  end
  else if (tryRead(startDelimComment)) then
  begin
    saved:= readBuffer;
    expandPE:= savedPE;
    parseComment();
  end
  else if (tryRead('<![')) then
  begin
    saved:= readBuffer;
    expandPE:= savedPE;
    if (inputStack.Count() > 0) then
      parseConditionalSect(saved)
    else
      error('conditional sections illegal in internal subset');
  end else
    error('expected markup declaration');

  // VC: Proper Decl/PE Nesting
  if (readBuffer <> saved) then
    handler.verror('Illegal Declaration/PE nesting');

end;

procedure TXMLParser.parseElement(maybeGetSubset : Boolean);
var gi : SAXString;
    c : SAXChar;
    oldElementContent : Integer;
    oldElement : SAXString;
    element : TObject;
    white : Boolean;
    atts : SAXStringArray;
    aname : SAXString;
    value : SAXString;
    I, J : Integer;
    publicId, systemId : SAXString;
    subids : SAXStringArray;
    subset : IInputSource;
    isis : IStreamInputSource;
    isis_stream : TStream;
begin
  oldElementContent:= currentElementContent;
  oldElement:= currentElement;

  // This is the (global) counter for the
  // array of specified attributes.
  tagAttributePos:= 0;

  // Read the element type name.
  gi:= readNmtoken(true);

  // If we saw no DTD, and this is the document root element,
  // let the application modify the input stream by providing one.
  if (maybeGetSubset) then
  begin
    subset:= handler.getExternalSubset(gi, handler.getSystemId());
    if (subset <> nil) then
    begin
      publicId:= subset.getPublicId();
      systemId:= subset.getSystemId();
      handler.warn('modifying document by adding DTD');
      handler.doctypeDecl(gi, publicId, systemId);
      pushString('', '>');

      // NOTE:  [dtd] is so we say what SAX2 expects,
      // even though it's misleading (subset, not entire dtd)
      SetLength(subids, 3);
      subids[0]:= publicId;
      subids[1]:= systemId;
      subids[2]:= '';
      if (Supports(subset, IStreamInputSource, isis)) then
        isis_stream:= isis.getByteStream()
      else
        isis_stream:= nil;
      pushURL(true, '[dtd]', subids,
        isis_stream,
        subset.getEncoding (),
        false);

      // Loop until we end up back at '>'
      while (true) do
      begin
        doReport:= true;
        expandPE:= true;
        skipWhitespace();
        doReport:= false;
        expandPE:= false;
        if (tryRead(SAXChar('>'))) then
        begin
          break;
        end
        else begin
          expandPE:= true;
          parseMarkupdecl();
          expandPE:= false;
        end;
      end;

      // the ">" string isn't popped yet
      if (inputStack.Count() <> 1) then
        error('external subset has unmatched ''>''');

      handler.endDoctype ();
    end;
  end;

  // Determine the current content type.
  currentElement:= gi;
  element:= elementInfo.get(gi);
  currentElementContent:= getContentType(element, CONTENT_ANY);

  // Read the attributes, if any.
  // After this loop, "c" is the closing delimiter.
  white:= tryWhitespace();
  c:= readCh();
  while ((c <> '/') and (c <> '>')) do
  begin
    unread(c);
    if (not white) then
      error('need whitespace between attributes');
    parseAttribute(gi);
    white:= tryWhitespace();
    c:= readCh();
  end;

  // Supply any defaulted attributes.
  atts:= declaredAttributes(element);
  if (atts <> nil) then
  begin
    for I:= 0 to Length(atts)-1 do
    begin
      aname:= atts[I];
      // See if it was specified.
      for J:= 0 to tagAttributePos-1 do
      begin
        if (tagAttributes[J] = aname) then
        begin
          Break;
        end;
      end;
      // ... or has a default
      value:= getAttributeDefaultValue(gi, aname);
      if (value = '') then
        continue;
      handler.attribute(aname, value, false, false); //!!
    end;
  end;

  // Figure out if this is a start tag
  // or an empty element, and dispatch an
  // event accordingly.
  case (c) of
   '>':
   begin
     handler.startElement(gi);
     parseContent();
   end;
   '/':
   begin
     require(SAXChar('>'));
     handler.startElement(gi);
     handler.endElement(gi);
   end;
  end;

  // Restore the previous state.
  currentElement:= oldElement;
  currentElementContent:= oldElementContent;

end;

procedure TXMLParser.parseAttribute(const name: SAXString);
var aname : SAXString;
    _type : SAXString;
    value : SAXString;
    flags : Integer;
    I : Integer;
begin
  flags:= LIT_ATTRIBUTE or LIT_ENTITY_REF;

  // Read the attribute name.
  aname:= readNmtoken(true);
  _type:= getAttributeType(name, aname);

  // Parse '='
  parseEq();

  // Read the value, normalizing whitespace
  // unless it is CDATA.
  if ((_type = 'CDATA') or (_type = '')) then
  begin
    value:= readLiteral(flags);
  end else begin
    value:= readLiteral(flags or LIT_NORMALIZE);
  end;

  // WFC: no duplicate attributes
  for I:= 0 to tagAttributePos-1 do
  begin
    if (aname = tagAttributes[i]) then
      error('duplicate attribute', aname, '');
  end;

  // Inform the handler about the
  // attribute.
  handler.attribute(aname, value, true, false); //!! 
  dataBufferPos:= 0;

  // Note that the attribute has been
  // specified.
  if (tagAttributePos = Length(tagAttributes)) then //!! Should this be -1?
  begin
    SetLength(tagAttributes, Length(tagAttributes) * 2);
  end;
  tagAttributes[tagAttributePos]:= aname;
  Inc(tagAttributePos);
end;

procedure TXMLParser.parseEq;
begin
  skipWhitespace();
  require(SAXChar('='));
  skipWhitespace();
end;

procedure TXMLParser.parseETag;
begin
  require(currentElement);
  skipWhitespace();
  require(SAXChar('>'));
  handler.endElement(currentElement);
  // not re-reporting any SAXException re bogus end tags,
  // even though that diagnostic might be clearer ...
end;

procedure TXMLParser.parseContent;
var c : SAXChar;
begin

  while (true) do
  begin
    // consume characters (or ignorable whitspace) until delimiter
    parseCharData();

    // Handle delimiters
    c:= readCh();
    case (c) of
      '&':       // Found "&"
      begin
        c:= readCh();
        if (c = '#') then
        begin
          parseCharRef();
        end else begin
          unread(c);
          parseEntityRef(true);
        end;
      end;
      '<':       // Found "<"
      begin
        dataBufferFlush();
        c:= readCh();
        case (c) of
          '!':       // Found "<!"
          begin
            c:= readCh();
            case (c) of
              '-':     // Found "<!-"
              begin
                require(SAXChar('-'));
                parseComment();
              end;
              '[':     // Found "<!["
              begin
                require('CDATA[');
                handler.startCDATA();
                inCDATA:= true;
                parseCDSect();
                inCDATA:= false;
                handler.endCDATA();
              end;
              else begin
                error('expected comment or CDATA section', c, '');
              end;
            end;
          end;
          '?':     // Found "<?"
          begin
            parsePI();
          end;
          '/':     // Found "</"
          begin
            parseETag();
            Exit;
          end;
          else begin  // Found "<" followed by something else
            unread(c);
            parseElement(false);
          end;
        end;
      end;
    end;
  end;
end;

procedure TXMLParser.parseElementDecl;
var name : SAXString;
begin
  requireWhitespace();
  // Read the element type name.
  name:= readNmtoken(true);

  requireWhitespace();
  // Read the content model.
  parseContentspec(name);

  skipWhitespace();
  require(SAXChar('>'));
end;

procedure TXMLParser.parseContentspec(const name : SAXString);
var model : SAXString;
    saved : SAXCharArray;
begin
  saved:= nil;
  // FIXME: move elementDecl() into setElement(), pass EMTPY/ANY ...
  if (tryRead('EMPTY')) then
  begin
    setElement(name, CONTENT_EMPTY, '', nil);
    if (not skippedPE) then
      TSAXDriverCracker(handler).getDeclHandler().elementDecl(name, 'EMPTY');
    Exit;
  end else if (tryRead('ANY')) then
  begin
    setElement(name, CONTENT_ANY, '', nil);
    if (not skippedPE) then
      TSAXDriverCracker(handler).getDeclHandler().elementDecl (name, 'ANY');
    Exit;
  end else begin
    require(SAXChar('('));
    saved:= readBuffer;
    dataBufferAppend(SAXChar('('));
    skipWhitespace();
    if (tryRead('#PCDATA')) then
    begin
      dataBufferAppend('#PCDATA');
      parseMixed(saved);
      model:= dataBufferToString();
      setElement(name, CONTENT_MIXED, model, nil);
    end
    else begin
      parseElements(saved);
      model:= dataBufferToString();
      setElement(name, CONTENT_ELEMENTS, model, nil);
    end;
    if (not skippedPE) then
      TSAXDriverCracker(handler).getDeclHandler().elementDecl(name, model);
  end;
end;

procedure TXMLParser.parseElements(const saved : SAXCharArray);
var c : SAXChar;
    sep : SAXChar;
begin
  // Parse the first content particle
  skipWhitespace();
  parseCp();

  // Check for end or for a separator.
  skipWhitespace();
  c:= readCh();
  case (c) of
    ')':
    begin
      // VC: Proper Group/PE Nesting
      if (readBuffer <> saved) then
        handler.verror('Illegal Group/PE nesting');

      dataBufferAppend(SAXChar(')'));
      c:= readCh();
      case (c) of
        '*',
        '+',
        '?':
        begin
          dataBufferAppend(c);
        end;
        else begin
          unread(c);
        end;
      end;
      Exit;
    end;
    ',',       // Register the separator.
    '|':
    begin
      sep:= c;
      dataBufferAppend(c);
    end;
    else begin
      error('bad separator in content model', c, '');
      Exit;
    end;
  end;

  // Parse the rest of the content model.
  while (true) do
  begin
    skipWhitespace();
    parseCp();
    skipWhitespace();
    c:= readCh();
    if (c = ')') then
    begin
      // VC: Proper Group/PE Nesting
      if (readBuffer <> saved) then
        handler.verror('Illegal Group/PE nesting');
      dataBufferAppend(SAXChar(')'));
      break;
    end else if (c <> sep) then
    begin
      error ('bad separator in content model', c, '');
      Exit;
    end else begin
      dataBufferAppend(c);
    end;
  end;

  // Check for the occurrence indicator.
  c:= readCh();
  case (c) of
    '?',
    '*',
    '+':
    begin
      dataBufferAppend(c);
      Exit;
    end;
    else begin
      unread(c);
      Exit;
    end;
  end;

end;

procedure TXMLParser.parseCp;
var c : SAXChar;
begin
  if (tryRead(SAXChar('('))) then
  begin
    dataBufferAppend(SAXChar('('));
    parseElements(readBuffer);
  end else begin
    dataBufferAppend(readNmtoken(true));
    c:= readCh();
    case (c) of
      '?',
      '*',
      '+':
      begin
        dataBufferAppend(c);
      end;
      else begin
        unread(c);
      end;
    end;
  end;
end;

procedure TXMLParser.parseMixed(const saved : SAXCharArray);
begin
  // Check for PCDATA alone.
  skipWhitespace();
  if (tryRead(SAXChar(')'))) then
  begin
    // VC: Proper Group/PE Nesting
    if (readBuffer <> saved) then
      handler.verror('Illegal Group/PE nesting');
    dataBufferAppend(')*');
    tryRead(SAXChar('*'));
    Exit;
  end;

  // Parse mixed content.
  skipWhitespace();
  while (not tryRead(')')) do
  begin
    require(SAXChar('|'));
    dataBufferAppend(SAXChar('|'));
    skipWhitespace();
    dataBufferAppend(readNmtoken(true));
    skipWhitespace();
  end;

  // VC: Proper Group/PE Nesting
  if (readBuffer <> saved) then
    handler.verror('Illegal Group/PE nesting');

  require(SAXChar('*'));
  dataBufferAppend(')*');
end;

procedure TXMLParser.parseAttListDecl;
var elementName : SAXString;
    white : Boolean;
begin
  requireWhitespace();
  elementName:= readNmtoken(true);
  white:= tryWhitespace();
  while (not tryRead(SAXChar('>'))) do
  begin
    if (not white) then
      error('whitespace required before attribute definition');
    parseAttDef(elementName);
    white:= tryWhitespace();
  end;
end;

procedure TXMLParser.parseAttDef(const elementName : SAXString);
var name : SAXString;
    _type : SAXString;
    enum : SAXString;
begin
  // Read the attribute name.
  name:= readNmtoken(true);

  // Read the attribute type.
  requireWhitespace();
  _type:= readAttType();

  // Get the string of enumerated values
  // if necessary.
  if (('ENUMERATION' = _type) or ('NOTATION' = _type)) then
    enum:= dataBufferToString();

  // Read the default value.
  requireWhitespace();
  parseDefault(elementName, name, _type, enum);

end;

function TXMLParser.readAttType: SAXString;
var typeString : SAXString;
begin
  if (tryRead(SAXChar('('))) then
  begin
    parseEnumeration(false);
    Result:= 'ENUMERATION';
    Exit;
  end else begin
    typeString:= readNmtoken(true);
    if ('NOTATION' = typeString) then
    begin
      parseNotationType();
      Result:= typeString;
      Exit;
    end else if (('CDATA' = typeString)
                  or ('ID' = typeString)
                  or ('IDREF' = typeString)
                  or ('IDREFS' = typeString)
                  or ('ENTITY' = typeString)
                  or ('ENTITIES' = typeString)
                  or ('NMTOKEN' = typeString)
                  or ('NMTOKENS' = typeString)) then
    begin
      Result:= typeString;
      Exit;
    end;

    error('illegal attribute type', typeString, '');
    Result:= '';;
    Exit;
  end;
end;

procedure TXMLParser.parseEnumeration(const isNames: Boolean);
begin
  dataBufferAppend(SAXChar('('));

  // Read the first token.
  skipWhitespace();
  dataBufferAppend(readNmtoken(isNames));
  // Read the remaining tokens.
  skipWhitespace();
  while (not tryRead(SAXChar(')'))) do
  begin
    require(SAXChar('|'));
    dataBufferAppend(SAXChar('|'));
    skipWhitespace();
    dataBufferAppend(readNmtoken(isNames));
    skipWhitespace();
  end;
  dataBufferAppend(SAXChar(')'));
end;

procedure TXMLParser.parseNotationType;
begin
  requireWhitespace();
  require(SAXChar('('));

  parseEnumeration(true);
end;

procedure TXMLParser.parseDefault(const elementName, name : SAXString;
  _type : SAXString; const enum : SAXString);
var valueType : Integer;
    value : SAXString;
    flags : Integer;
    saved : Boolean;
    defaultType : SAXString;
begin
  valueType:= ATTRIBUTE_DEFAULT_SPECIFIED;
  flags:= LIT_ATTRIBUTE;
  saved:= expandPE;
  defaultType:= '';

  // LIT_ATTRIBUTE forces '<' checks now(ASAP) and turns whitespace
  // chars to spaces(doesn't matter when that's done if it doesn't
  // interfere with char refs expanding to whitespace).

  if (not skippedPE) then
  begin
    flags:= flags or LIT_ENTITY_REF;
    if ('CDATA' <> _type) then
      flags := flags or LIT_NORMALIZE;
  end;

  expandPE:= false;
  if (tryRead(SAXChar('#'))) then
  begin
    if (tryRead('FIXED')) then
    begin
      defaultType:= '#FIXED';
      valueType:= ATTRIBUTE_DEFAULT_FIXED;
      requireWhitespace();
      value:= readLiteral(flags);
    end else if (tryRead('REQUIRED')) then
    begin
      defaultType:= '#REQUIRED';
      valueType:= ATTRIBUTE_DEFAULT_REQUIRED;
    end else if (tryRead('IMPLIED')) then
    begin
      defaultType:= '#IMPLIED';
      valueType:= ATTRIBUTE_DEFAULT_IMPLIED;
    end else begin
      error('illegal keyword for attribute default value');
    end;
  end else
    value:= readLiteral(flags);
  expandPE:= saved;  
  setAttribute(elementName, name, _type, enum, value, valueType);

  if ('ENUMERATION' = _type) then
    _type:= enum
  else if ('NOTATION' = _type) then
    _type:= 'NOTATION ' + enum;
  if (not skippedPE) then
    TSAXDriverCracker(handler).getDeclHandler().attributeDecl(elementName, name,
      _type, defaultType, value);
end;

procedure TXMLParser.parseConditionalSect(const saved : SAXCharArray);
var c : SAXChar;
    nesting : Integer;
begin
  skipWhitespace();
  if (tryRead('INCLUDE')) then
  begin
    skipWhitespace();
    require(SAXChar('['));
    // VC: Proper Conditional Section/PE Nesting
    if (readBuffer <> saved) then
      handler.verror('Illegal Conditional Section/PE nesting');
    skipWhitespace();
    while (not tryRead(']]>')) do
    begin
      parseMarkupdecl();
      skipWhitespace();
    end;
  end else if (tryRead('IGNORE')) then
  begin
    skipWhitespace();
    require(SAXChar('['));
    // VC: Proper Conditional Section/PE Nesting
    if (readBuffer <> saved) then
      handler.verror('Illegal Conditional Section/PE nesting');
    expandPE:= false;
    nesting:= 1;
    while (nesting > 0) do
    begin
      c:= readCh();
      case (c) of
        '<':
        begin
          if (tryRead('![')) then
          begin
            Inc(nesting);
          end;
        end;
        ']':
        begin
          if (tryRead(']>')) then
          begin
            Dec(nesting);
          end;
        end;
      end;
    end;
    expandPE:= true;
  end else begin
    error('conditional section must begin with INCLUDE or IGNORE');
  end;

end;

procedure TXMLParser.parseCharRef;
var value : Integer;
    c : SAXChar;
    n : Word;
begin
  value:= 0;
  n:= 0;
  if (tryRead(SAXChar('x'))) then
  begin
    while(true) do
    begin
      c:= readCh();
      case (c) of
        '0','1','2','3','4','5','6','7','8','9':
        begin
          n:= Word(c) - Word('0');
        end;
        'a','b','c','d','e','f':
        begin
          n:= (Word(c) - Word('a')) + 10;
        end;
        'A','B','C','D','E','F':
        begin
          n:= (Word(c) - Word('A')) + 10;
        end;
        ';':
        begin
          break;
        end;
        else begin
          error('illegal character in character reference', c, '');
          break;
        end;
      end;
      value:= value * 16;
      value:= value + n;
    end;
  end else begin
    while(true) do
    begin
      c:= readCh();
      case (c) of
        '0','1','2','3','4','5','6','7','8','9':
        begin
          value:= value * 10;
          value:= value + Word(c) - Word('0');
        end;
        ';':
        begin
          break;
        end;
        else begin
          error('illegal character in character reference', c, '');
          break;
        end;
      end;
    end;
  end;

  // check for character refs being legal XML
  if (((value < $0020 ) and (not ((value = $000A) or (value = $0009) or (value = $000D)))) or
      ((value >= $D800) and (value <= $DFFF)) or
      (value = $FFFE) or
      (value = $FFFF) or
      (value > $0010FFFF)) then
     error('illegal XML character reference U+' + IntToHex(value, 4));

  // Check for surrogates: 00000000 0000xxxx yyyyyyyy zzzzzzzz
  // (1101|10xx|xxyy|yyyy + 1101|11yy|zzzz|zzzz:
  if (value <= $0000FFFF) then
  begin
    // no surrogates needed
    dataBufferAppend(SAXChar(value));
  end else if (value <= $0010FFFF) then
  begin
    value:= value - $10000;
    // > 16 bits, surrogate needed
    dataBufferAppend(SAXChar(($D800 or (value shr 10))));
    dataBufferAppend(SAXChar(($DC00 or (value and $0003FF))));
  end else begin
    // too big for surrogate
    error('character reference ' + IntToStr(value) + ' is too large for UTF-16', IntToStr(value), '');
 end;

end;

procedure TXMLParser.parseEntityRef(const externalAllowed: Boolean);
var name, _message : SAXString;
begin
  name:= readNmtoken(true);
  require(SAXChar(';'));
  case(getEntityType(name)) of
    ENTITY_UNDECLARED:
    begin
      // NOTE:  XML REC describes amazingly convoluted handling for
      // this case.  Nothing as meaningful as being a WFness error
      // unless the processor might _legitimately_ not have seen a
      // declaration ... which is what this implements.
      _message:= 'reference to undeclared general entity ' + name;
      if ((skippedPE) and (not docIsStandalone)) then
      begin
        handler.verror(_message);
        // we don't know this entity, and it might be external...
        if (externalAllowed) then
          handler.skippedEntity(name);
      end else
        error(_message);
    end;
    ENTITY_INTERNAL:
    begin
      pushString(name, getEntityValue(name));
    end;
    ENTITY_TEXT:
    begin
      if (externalAllowed) then
      begin
        pushURL(false, name, getEntityIds(name), nil, '', true);
      end else
      begin
        error('reference to external entity in attribute value.', name, '');
      end;
    end;
    ENTITY_NDATA:
    begin
      if (externalAllowed) then
      begin
        error('unparsed entity reference in content', name, '');
      end else
      begin
        error('reference to external entity in attribute value.', name, '');
      end;
    end;
    else begin
      raise ERuntimeException.Create('');
    end;
  end;
end;

procedure TXMLParser.parsePEReference;
var name : SAXString;
begin
  name:= '%' + readNmtoken(true);
  require(SAXChar(';'));
  case (getEntityType(name)) of
    ENTITY_UNDECLARED:
    begin
      // VC: Entity Declared
      handler.verror('reference to undeclared parameter entity ' + name);
      // we should disable handling of all subsequent declarations
      // unless this is a standalone document (info discarded)
    end;
    ENTITY_INTERNAL:
    begin
      if (inLiteral) then
        pushString(name, getEntityValue(name))
      else
        pushString(name, ' ' + getEntityValue(name) + ' ');
    end;
    ENTITY_TEXT:
    begin
      if (not inLiteral) then
        pushString('', ' ');
      pushURL(true, name, getEntityIds(name), nil, '', true);
      if (not inLiteral) then
        pushString('', ' ');
    end;
  end;
end;

procedure TXMLParser.parseEntityDecl;
var c : SAXChar;
    peFlag : Boolean;
    name, value, notationName : SAXString;
    ids : SAXStringArray;
    white : Boolean;
    flags : Integer;
begin
  SetLength(ids, 0);
  peFlag:= false;
  flags:= LIT_DISABLE_CREF;

  // Check for a parameter entity.
  expandPE:= false;
  requireWhitespace();
  if (tryRead(SAXChar('%'))) then
  begin
    peFlag:= true;
    requireWhitespace();
  end;
  expandPE:= true;

  // Read the entity name, and prepend
  // '%' if necessary.
  name:= readNmtoken(true);
  if (peFlag) then
  begin
    name:= '%' + name;
  end;

  // Read the entity value.
  requireWhitespace();
  c:= readCh();
  unread(c);
  if ((c = '"') or (c = '''')) then
  begin
    // Internal entity ... replacement text has expanded refs
    // to characters and PEs, but not to general entities
    value:= readLiteral(flags);
    setInternalEntity(name, value);
  end else
  begin
    // Read the external IDs
    ids:= readExternalIds(false, false);

    // Check for NDATA declaration.
    white:= tryWhitespace();
    if ((not peFlag) and tryRead('NDATA')) then
    begin
      if (not white) then
        error('whitespace required before NDATA');
      requireWhitespace();
      notationName:= readNmtoken(true);

      if (not skippedPE) then
      begin
        setExternalEntity(name, ENTITY_NDATA, ids, notationName);
        handler.unparsedEntityDecl(name, ids, notationName);
      end
    end else if (not skippedPE) then
    begin
      setExternalEntity(name, ENTITY_TEXT, ids, '');
      // FIXME: ASSUMES not skipped
      // "false" forces error on bad URI
      if (TSAXDriverCracker(handler).resolveURIs()) then
        TSAXDriverCracker(handler).getDeclHandler().externalEntityDecl(name, ids[0],
          TSAXDriverCracker(handler).absolutize(ids[2], ids[1], false))
      else
        TSAXDriverCracker(handler).getDeclHandler().externalEntityDecl(name, ids[0], ids[1]);
    end;
  end;

  // Finish the declaration.
  skipWhitespace();
  require(SAXChar('>'));
end;

procedure TXMLParser.parseNotationDecl;
var nname : SAXString;
    ids : SAXStringArray;
begin
  requireWhitespace();
  nname:= readNmtoken(true);

  requireWhitespace();

  // Read the external identifiers.
  ids:= readExternalIds(true, false);

  // Register the notation.
  setNotation(nname, ids);

  skipWhitespace();
  require(SAXChar('>'));

end;

procedure TXMLParser.parseCharData;
var c : SAXChar;
    lineAugment : Integer;
    columnAugment : Integer;
    I, length  : Integer;
    state : Integer;
    pureWhite : Boolean;
begin
  state:= 0;
  pureWhite:= false;

  // could have <![CDATA[...]]> text or char ref expansions
  if (dataBufferPos <> 0) then
    dataBufferFlush();

  // are we expecting pure whitespace?  it might be dirty...
  if ((currentElementContent = CONTENT_ELEMENTS) and (not inCDATA)) then
    pureWhite:= true;

  // always report right out of readBuffer
  // to minimize (pointless) buffer copies
  while (true) do
  begin
    lineAugment:= 0;
    columnAugment:= 0;
    I:= readBufferPos;
    while (I < readBufferLength) do
    begin
      c:= readBuffer[I];
      case (c) of
        #10:
        begin
          Inc(lineAugment);
          columnAugment:= 0;
          // pureWhite unmodified
        end;
        #13: // should not happen!!
        begin
          // !! this does not raise an exception in Java
          raise ESAXParseException.Create('should not happen');
        end;
        #9,
        ' ':
        begin
          // pureWhite unmodified
          Inc(columnAugment);
        end;
        '&',
        '<':
        begin
          Inc(columnAugment);
          // pureWhite unmodified
          // CLEAN end of text sequence
          state:= 1;
          break;
        end;
        ']':
        begin
          // that's not a whitespace char, and
          // can not terminate pure whitespace either
          pureWhite:= false;
          if((i + 2) < readBufferLength) then
          begin
            if ((readBuffer[i + 1] = ']') and (readBuffer[i + 2] = '>')) then
            begin
              // ERROR end of text sequence
              state:= 2;
              break;
            end;
          end
          else begin
            // FIXME missing two end-of-buffer cases
          end;
          Inc(columnAugment);
        end;
        else begin
          if (Word(c) < $0020) or (Word(c) > $FFFD) then
            error('illegal XML character U+' + IntToHex(Word(c), 4));
          // that's not a whitespace char
          pureWhite:= false;
          Inc(columnAugment);
        end;
      end;
      Inc(I);
    end;

    // report text thus far
    if (lineAugment > 0) then
    begin
      line:= line + lineAugment;
      column:= columnAugment;
    end else
    begin
      column:= column + columnAugment;
    end;


    // report characters/whitspace
    length:= i - readBufferPos;

    if (length <> 0) then
    begin
      if (pureWhite) then
        handler.ignorableWhitespace(readBuffer, readBufferPos, length)
      else
        handler.charData(readBuffer, readBufferPos, length);
      readBufferPos:= i;
    end;

    if (state <> 0) then
      break;

    // fill next buffer from this entity, or
    // pop stack and continue with previous entity
    unread(readCh());
  end;

   // finish, maybe with error
   if (state <> 1) then  // finish, no error
     error('character data may not contain '']]>''');
end;

procedure TXMLParser.requireWhitespace;
var c : SAXChar;
begin
  c:= readCh();
  if(isWhitespace(c)) then
  begin
    skipWhitespace();
  end else
  begin
    error('whitespace required', c, '');
  end;
end;

procedure TXMLParser.skipWhitespace;
var lineAugment : Integer;
    columnAugment : Integer;
    i : Integer;
    c : SAXChar;
begin
  // Start with a little cheat.  Most of
  // the time, the white space will fall
  // within the current read buffer; if
  // not, then fall through.
  if(USE_CHEATS) then
  begin
    lineAugment:= 0;
    columnAugment:= 0;
    i:= readBufferPos;
    while (i < readBufferLength) do
    begin
      case (readBuffer[i]) of
        ' ',
        #9,
        #13:
        begin
          Inc(columnAugment);
        end;
        #10:
        begin
          Inc(lineAugment);
          columnAugment:= 0;
        end;
        '%':
        begin
          if (expandPE) then
            Break;
          // !! We have to redo this code-- in the JAVA version this will
          // !! just fall through to the default handler
          readBufferPos:= i;
          if(lineAugment > 0) then
          begin
            line:= line + lineAugment;
            column:= columnAugment;
          end else
          begin
            column:= column + columnAugment;
          end;
          Exit;
        end;
        // else fall through...
        else begin
          readBufferPos:= i;
          if(lineAugment > 0) then
          begin
            line:= line + lineAugment;
            column:= columnAugment;
          end else
          begin
            column:= column + columnAugment;
          end;
          Exit;
        end;
      end;
      Inc(i);
    end;
  end;

  // OK, do it by the book.
  c:= readCh();
  while (isWhitespace(c)) do
  begin
    c:= readCh();
  end;
  unread(c);
end;

function TXMLParser.readNmToken(const isName: Boolean): SAXString;
var c : SAXChar;
    i : Integer;
    start : Integer;
    s : SAXString;
begin
  if (USE_CHEATS) then
  begin
    i:= readBufferPos;
    while (i < readBufferLength) do
    begin
      c:= readBuffer[i];
      case (c) of
        // !! We fall through automatically
        '%',
        // else fall through...

        // What may legitimately come AFTER a name/nmtoken?
        '<',
        '>',
        '&',
        ',',
        '|',
        '*',
        '+',
        '?',
        ')',
        '=',
        '''',
        '"',
        '[',
        ' ',
        #09,
        #13,
        #10,
        ';',
        '/':
        begin
          if (c = '%') then
          begin
            if (expandPE) then
              break;
          end;
          start:= readBufferPos;
          if (i = start) then
            error('name expected', readBuffer[i], '');
          readBufferPos:= i;
          Result:= intern(readBuffer, start, i - start);
          Exit;
        end;
        else begin
          // FIXME ... per IBM's OASIS test submission, these:
          //   ?    U+06dd
          // REJECT
          //   BaseChar  U+0132 U+0133 U+013F U+0140 U+0149 U+017F U+01C4 U+01CC
          //    U+01F1 U+01F3 U+0E46 U+1011 U+1104 U+1108 U+110A U+110D
          //    U+113B U+113F U+1141 U+114D U+114F U+1151 U+1156 U+1162
          //    U+1164 U+1166 U+116B U+116F U+1174 U+119F U+11AC U+11B6
          //    U+11B9 U+11BB U+11C3 U+11F1 U+212F U+0587
          //   Combining  U+309B

          // punt on exact tests from Appendix A; approximate
          // them using the Unicode ID start/part rules

          //!! Actually this should be on target now... not a punt
          if ((i = readBufferPos) and (isName)) then
          begin
           if ((not isLetter(c)) and (c <> ':') and (c <> '_')) then
              error('Not a name start character, U+' + IntToHex(Word(c), 4));
          end else if ((not isLetter(c)) and (not isDigit(c)) and (c <> '.') and (c <> '-') and (c <> '_') and (c <> ':') and (not isCombiningChar(c)) and (not isExtender(c))) then
            error('Not a name character, U+' + IntToHex(Word(c), 4));
        end;
      end;
      Inc(I);
    end;
  end;

  nameBufferPos:= 0;

  // Read the first character.
  while (true) do
  begin
    c:= readCh();
    case (c) of
      '%',
      '<',
      '>',
      '&',
      ',',
      '|',
      '*',
      '+',
      '?',
      ')',
      '=',
      '''',
      '"',
      '[',
      ' ',
      #09,
      #10,
      #13,
      ';',
      '/':
      begin
        unread(c);
        if(nameBufferPos = 0) then
        begin
          error('name expected');
        end;
        //!! Modified check
        if ((isName) and (not isLetter(nameBuffer[0])) and (nameBuffer[0] <> ':') and (nameBuffer[0] <> '_')) then
          error('Not a name start character, U+' + IntToHex(Word(nameBuffer[0]), 4));
        s:= intern(nameBuffer, 0, nameBufferPos);
        nameBufferPos:= 0;
        Result:= s;
        Exit;
      end;
      else begin
        // punt on exact tests from Appendix A, but approximate them
        if (((nameBufferPos <> 0) or (not isName)) and ((not isLetter(c)) and (not isDigit(c)) and (c <> '.') and (c <> '-') and (c <> '_') and (c <> ':') and (not isCombiningChar(c)) and (not isExtender(c)))) then
          error('Not a name character, U+' + IntToHex(Word(c), 4));
        if (nameBufferPos >= Length(nameBuffer)) then
          extendArray(nameBuffer, Length(nameBuffer), nameBufferPos);

        nameBuffer[nameBufferPos]:= c;
        Inc(nameBufferPos);
      end;
    end;
  end;
end;

function TXMLParser.isExtender(const c: SAXChar): Boolean;
begin
  // [88] Extender ::= ...
  Result:= ((Word(c) = $00b7) or (Word(c) = $02d0) or (Word(c) = $02d1) or (Word(c) = $0387) or
            (Word(c) = $0640) or (Word(c) = $0e46) or (Word(c) = $0ec6) or (Word(c) = $3005) or
            ((Word(c) >= $3031) and (Word(c) <= $3035)) or
            ((Word(c) >= $309d) and (Word(c) <= $309e)) or
            ((Word(c) >= $30fc) and (Word(c) <= $30fe)));
  Exit;
end;

function TXMLParser.readLiteral(const flags: Integer): SAXString;
var delim, c : SAXChar;
    startLine : Integer;
    saved : Boolean;
    savedReport : Boolean;
    ourBuf : SAXCharArray;
    name : SAXString;
begin
  ourBuf:= nil;
  startLine:= line;
  saved:= expandPE;
  savedReport:= doReport;

  // Find the first delimiter.
  delim:= readCh();
  if ((delim <> '"') and (delim <> '''')) then
  begin
    error('expected ''"'' or ''''''', delim, '');
    Result:= '';
    Exit;
  end;

  inLiteral:= true;
  if ((flags and LIT_DISABLE_PE) <> 0) then
    expandPE:= false;
  doReport:= false;

  // Each level of input source has its own buffer; remember
  // ours, so we won't read the ending delimiter from any
  // other input source, regardless of entity processing.
  ourBuf:= readBuffer;

  // Read the literal.
  try
    c:= readCh();
    while ( not((c = delim) and (readBuffer = ourBuf)))  do
    begin
      case (c) of
        // attributes and public ids are normalized
        // in almost the same ways
        #10,
        #13:
        begin
          if ((flags and (LIT_ATTRIBUTE or LIT_PUBID)) <> 0) then
            c:= ' ';
        end;
        #09:
        begin
          if ((flags and LIT_ATTRIBUTE) <> 0) then
            c:= ' ';
        end;
        '&':
        begin
          c:= readCh();
          // Char refs are expanded immediately, except for
          // all the cases where it's deferred.
          if (c = '#') then
          begin
            if ((flags and LIT_DISABLE_CREF) <> 0) then
            begin
              dataBufferAppend(SAXChar('&'));
            end;
            parseCharRef();
            // exotic WFness risk: this is an entity literal,
            // dataBuffer[dataBufferPos - 1] = '&', and
            // following chars are a _partial_ entity/char ref

          // It looks like an entity ref ...
          end else
          begin
            unread(c);
            // Expand it?
            if ((flags and LIT_ENTITY_REF) > 0) then
            begin
              parseEntityRef(false);
            // Is it just data?
            end else if ((flags and LIT_DISABLE_EREF) <> 0) then
            begin
              dataBufferAppend(SAXChar('&'));
            // OK, it will be an entity ref -- expanded later.
            end else
            begin
              name:= readNmtoken(true);
              require(SAXChar(';'));
              dataBufferAppend(SAXChar('&'));
              dataBufferAppend(name);
              dataBufferAppend(SAXChar(';'));
            end;
          end;
          c:= readCh();
          //!!continue loop;
          continue;
        end;
        '<':
        begin
          // and why?  Perhaps so '&foo;' expands the same
          // inside and outside an attribute?
          if ((flags and LIT_ATTRIBUTE) <> 0) then
            error('attribute values may not contain ''<''');
        end;
        // We don't worry about   '%' and PE refs, readCh does.
      end;
      dataBufferAppend(c);
      c:= readCh();
    end;
  except
    on E : EEOFException do
    begin
      error('end of input while looking for delimiter(started on line '
       + IntToStr(startLine) + ')', '', delim);
    end;
  end;
  inLiteral:= false;
  expandPE:= saved;
  doReport:= savedReport;

  // Normalise whitespace if necessary.
  if((flags and LIT_NORMALIZE) > 0) then
  begin
    dataBufferNormalize();
  end;

  // Return the value.
  Result:= dataBufferToString();
  Exit;
end;

function TXMLParser.readExternalIds(
  const inNotation, isSubset: Boolean): SAXStringArray;
var c : SAXChar;
    ids : SAXStringArray;
    flags : Integer;
    i : Integer;
begin
  SetLength(ids, 3);
  flags:= LIT_DISABLE_CREF or LIT_DISABLE_PE or LIT_DISABLE_EREF;

  if (tryRead('PUBLIC')) then
  begin
    requireWhitespace();
    ids[0]:= readLiteral(LIT_NORMALIZE or LIT_PUBID or flags);
    if (inNotation) then
    begin
      skipWhitespace();
      c:= readCh();
      unread(c);
      if ((c = '"') or (c = '''')) then
      begin
        ids[1]:= readLiteral(flags);
      end;
    end else
    begin
      requireWhitespace();
      ids[1]:= readLiteral(flags);
    end;

    for i:= 1 to Length(ids[0]) do
    begin
      c:= ids[0][i];
      if (isPubidChar(c)) then
        continue;
      error('illegal PUBLIC id character U+' + IntToHex(Word(c), 4));
    end;
  end
  else if (tryRead('SYSTEM')) then
  begin
    requireWhitespace();
    ids[1]:= readLiteral(flags);
  end
  else if (not isSubset) then
    error('missing SYSTEM or PUBLIC keyword');

  if (ids[1] <> '') then
  begin
    if (Pos('#', ids[1]) > 0) then
      handler.verror('SYSTEM id has a URI fragment: ' + ids [1]);
    ids[2]:= handler.getSystemId();
    if (ids[2] = '') then
      handler.warn('No base URI; URI must be absolute: ' + ids[1]);
  end;

  Result:= ids;
  Exit;
end;

function TXMLParser.isWhitespace(const c: SAXChar): Boolean;
begin
  if (Word(c) > $20) then
  begin
    Result:= false;
    Exit;
  end;

  if ((Word(c) = $20) or (Word(c) = $0a) or  (Word(c) = $09) or (Word(c) = $0d)) then
  begin
    Result:= true;
    Exit;
  end;

  Result:= false;  // illegal ...
  Exit;
end;

procedure TXMLParser.dataBufferAppend(const c: SAXChar);
begin
  // Expand buffer if necessary.
  if (dataBufferPos >= Length(dataBuffer)) then
    extendArray(dataBuffer, Length(dataBuffer), dataBufferPos);
  dataBuffer[dataBufferPos]:= c;
  Inc(dataBufferPos);
end;

procedure TXMLParser.dataBufferAppend(const s: SAXString);
var ch : SAXCharArray;
begin
  SAXStringToSAXCharArray(s, ch);
  dataBufferAppend(ch, 0, Length(ch));
end;

procedure TXMLParser.dataBufferAppend(const ch: SAXCharArray; const start,
  len: Integer);
begin
  extendArray(dataBuffer, Length(dataBuffer), dataBufferPos + len);
  arrayCopy(ch, dataBuffer, start, len, dataBufferPos);
  dataBufferPos:= dataBufferPos + len;
end;

procedure TXMLParser.dataBufferNormalize;
var i, j, _end : Integer;
    c : SAXChar;
begin
  i:= 0;
  j:= 0;
  _end:= dataBufferPos;

  // Skip spaces at the start.
  while ((j < _end) and (dataBuffer[j] = ' ')) do
  begin
    Inc(j);
  end;

  // Skip spaces at the end.
  while ((_end > j) and (dataBuffer[_end - 1] = ' ')) do
  begin
    Dec(_end);
  end;

  // Start copying to the left.
  while (j < _end) do
  begin
    c:= dataBuffer[j];
    Inc(j);

    // Normalise all other spaces to
    // a single space.
    if (c = ' ') then
    begin
      while ((j < _end) and (dataBuffer[j] = ' ')) do
      begin
        Inc(j);
      end;
      Inc(j);

      dataBuffer[i]:= ' ';
      Inc(i);
      dataBuffer[i]:= dataBuffer [j - 1];
      Inc(i);
    end else
    begin
      dataBuffer[i]:= c;
      Inc(i);
    end;
  end;

  // The new length is <= the old one.
  dataBufferPos:= i;
end;

function TXMLParser.dataBufferToString : SAXString;
var s : SAXString;
begin
  SAXCharArrayToSAXString(dataBuffer, s);
  SetLength(s, dataBufferPos);
  dataBufferPos:= 0;
  Result:= s;
  Exit;
end;

procedure TXMLParser.dataBufferFlush;
var i : Integer;
begin
  if ((currentElementContent = CONTENT_ELEMENTS) and (dataBufferPos > 0) and (not inCDATA)) then
  begin
    // We can't just trust the buffer to be whitespace, there
    // are (error) cases when it isn't
    for i:= 0 to dataBufferPos-1 do
    begin
      if(not isWhitespace(dataBuffer[i])) then
      begin
        handler.charData(dataBuffer, 0, dataBufferPos);
        dataBufferPos:= 0;
      end;
    end;
    if (dataBufferPos > 0) then
    begin
      handler.ignorableWhitespace(dataBuffer, 0, dataBufferPos);
      dataBufferPos:= 0;
    end;
  end else if (dataBufferPos > 0) then
  begin
    handler.charData(dataBuffer, 0, dataBufferPos);
    dataBufferPos:= 0;
  end;
end;

procedure TXMLParser.require(const delim: SAXString);
var len : Integer;
    ch, dch : SAXCharArray;
    offset : Integer;
    i : Integer;
begin
  len:= Length(delim);

  if (len < Length(dataBuffer)) then
  begin
    ch:= dataBuffer;
    SAXStringToSAXCharArray(delim, dch);
    arrayCopy(dch, ch, 0, len, 0);
  end else
    SAXStringToSAXCharArray(delim, ch);

  if ((USE_CHEATS) and (len <= (readBufferLength - readBufferPos))) then
  begin
    offset:= readBufferPos;

    for i:= 0 to len-1 do
    begin
      if (ch[i] <> readBuffer[offset]) then
        error('required string', '', delim);
      Inc(offset);
    end;
    readBufferPos:= offset;
    //POS
    column:= column + len;
  end else
  begin
    for i:= 0 to len-1 do
    begin
      require(ch[i]);
    end;
  end;
end;

procedure TXMLParser.require(const delim: SAXChar);
var c : SAXChar;
begin
  c:= readCh();
  if (c <> delim) then
  begin
    error('required character', c, delim);
  end;
end;

{$IFDEF SAFELOOP}
function TXMLParser.intern(const ch: SAXCharArray; const start,
  len: Integer): SAXString;
var i : Integer;
    w : SAXString;
begin
  SetLength(w, len);
  for i:= start to (start+len)-1 do
    w[(i-start)+1]:= ch[i];
  Result:= w;
end;
{$ELSE}
function TXMLParser.intern(const ch: SAXCharArray; const start,
  len: Integer): SAXString;
var w : SAXString;
    l : Integer;
    Source : Pointer;
begin
  l:= Length(ch);
  Source:= pointer(ch);
  Inc(Cardinal(Source), start * SizeOf(SAXChar));
  // Copy only to the end of the array
  if ((l - start) < len) then
  begin
    SetLength(w, l - start);
    move(source^, pointer(w)^, (l - start) * SizeOf(SAXChar));
  end else
  begin
    SetLength(w, len);
    move(source^, pointer(w)^, len * SizeOf(SAXChar));
  end;
  Result:= w;
end;
{$ENDIF}

procedure TXMLParser.extendArray(var arr: SAXCharArray; const currentSize,
  requiredSize: Integer);
var newSize : Integer;
begin
  if(requiredSize < currentSize) then
  begin
    //We do nothing here-- nothing to return
  end else
  begin
    newSize:= currentSize * 2;

    if(newSize <= requiredSize) then
      newSize:= requiredSize + 1;

    SetLength(arr, newSize);
  end;
end;

function TXMLParser.isStandalone() : Boolean;
begin
  Result:= docIsStandalone;
end;

function TXMLParser.getContentType(const element: TObject;
  const defaultType: Integer): Integer;
var retval : Integer;
begin
  if (element = nil) then
  begin
    Result:= defaultType;
    Exit;
  end;
  retval:= TSAXElementWrapper(element).contentType;
  if (retval = CONTENT_UNDECLARED) then
    retval:= defaultType;
  Result:= retval;
  Exit;
end;

function TXMLParser.getElementContentType(const name: SAXString): Integer;
var element : TList;
begin
  element:=  TList(elementInfo.get(name));
  Result:= getContentType(element, CONTENT_UNDECLARED);
  Exit;
end;

procedure TXMLParser.setElement(const name: SAXString;
  const contentType: Integer;
  const contentModel: SAXString;
  const attributes : THashtable);
var element : TSAXElementWrapper;
begin
  if (skippedPE) then
    Exit;

  element:= TSAXElementWrapper(elementInfo.get(name));

  // first <!ELEMENT ...> or <!ATTLIST ...> for this type?
  if (element = nil) then
  begin
    element:= TSAXElementWrapper.Create;
    element.contentType:= contentType;
    element.contentModel:= contentModel;
    element.attList:= attributes;
    elementInfo.put(name, element);
    Exit;
  end;

  // <!ELEMENT ...> declaration?
  if (contentType <> CONTENT_UNDECLARED) then
  begin
    // ... following an associated <!ATTLIST ...>
    if (element.contentType = CONTENT_UNDECLARED) then
    begin
      element.contentType:= contentType;
      element.contentModel:= contentModel;
    end else
      // VC: Unique Element Type Declaration
      handler.verror('multiple declarations for element type: ' + name);
  end
  // first <!ATTLIST ...>, before <!ELEMENT ...> ?
  else if (attributes <> nil) then
  begin
    element.attList:= attributes;
  end;
end;

function TXMLParser.getElementAttributes(
  const name: SAXString): THashtable;
var element : TSAXElementWrapper;
begin
  element:= TSAXElementWrapper(elementInfo.get(name));

  if (element = nil) then
  begin
    Result:= nil;
    Exit;
  end else
  begin
    Result:= element.attList;
    Exit;
  end;  
end;

function TXMLParser.declaredAttributes(
  const element: TObject): SAXStringArray;
var attlist : THashtable;
begin
  if (element = nil) then
  begin
    Result:= nil;
    Exit;
  end;

  attlist:= TSAXElementWrapper(element).attList;
  if (attlist = nil) then
  begin
    Result:= nil;
    Exit;
  end;
  Result:= SAXStringArray(attlist.getKeys());
  Exit;
end;

function TXMLParser.declaredAttributes(const elName: SAXString): SAXStringArray;
begin
  Result:= declaredAttributes(TObject(elementInfo.get(elname)));
  Exit;
end;

function TXMLParser.getAttributeType(const name,
  aname: SAXString): SAXString;
var attribute : TSAXAttributeWrapper;
begin
  attribute:= TSAXAttributeWrapper(getAttribute(name, aname));
  if (attribute = nil) then
  begin
    Result:= '';
    Exit;
  end else
  begin
    Result:= attribute._type;
    Exit;
  end;
end;

function TXMLParser.getAttributeEnumeration(const name,
  aname: SAXString): SAXString;
var attribute : TSAXAttributeWrapper;
begin
  attribute:= TSAXAttributeWrapper(getAttribute(name, aname));
  if (attribute = nil) then
  begin
    Result:= '';
    Exit;
  end else
  begin
    // assert:  attribute [0] is "ENUMERATION" or "NOTATION"
    Result:= attribute.enumeration;
    Exit;
  end;
end;

function TXMLParser.getAttributeDefaultValue(const name,
  aname: SAXString): SAXString;
var attribute : TSAXAttributeWrapper;
begin
  attribute:= TSAXAttributeWrapper(getAttribute(name, aname));
  if (attribute = nil) then
  begin
    Result:= '';
    Exit;
  end else
  begin
    Result:= attribute.value;
    Exit;
  end;
end;

(*
// FIXME:  Leaving this in, until W3C finally resolves the confusion
// between parts of the XML 2nd REC about when entity declararations
// are guaranteed to be known.  Current code matches what section 5.1
// (conformance) describes, but some readings of the self-contradicting
// text in 4.1 (the "Entity Declared" WFC and VC) seem to expect that
// attribute expansion/normalization must be deferred in some cases
// (just TRY to identify them!).
function TXMLParser.getAttributeExpandedValue(const name,
  aname: SAXString): SAXString;
var attribute : TSAXAttributeWrapper;
    buf : SAXCharArray;
    flags : Integer;
    _type : SAXString;
begin
  attribute:= TSAXAttributeWrapper(getAttribute(name, aname));

  if (attribute = nil) then
  begin
    Result:= '';
    Exit;
  end else if ((attribute.expandedValue = '') and (attribute.value <> '')) then
  begin
    // we MUST use the same buf for both quotes else the literal
    // can't be properly terminated
    SetLength(buf, 1);
    flags:= LIT_ENTITY_REF or LIT_ATTRIBUTE;
    _type:= getAttributeType(name, aname);

    if (('CDATA' <> _type) and ('' <> _type)) then
      flags := flags or LIT_NORMALIZE;
    buf[0]:= '"';
    pushCharArray('', buf, 0, 1);
    pushString('',attribute.value);
    pushCharArray('', buf, 0, 1);
    attribute.expandedValue:= readLiteral(flags);
  end;
  Result:= attribute.expandedValue;
  Exit;
end;*)

function TXMLParser.getAttributeDefaultValueType(const name,
  aname: SAXString): Integer;
var attribute : TSAXAttributeWrapper;
begin
  attribute:= TSAXAttributeWrapper(getAttribute(name, aname));
  if (attribute = nil) then
  begin
    Result:= ATTRIBUTE_DEFAULT_UNDECLARED;
    Exit;
  end else
  begin
    Result:= attribute.valueType;
    Exit;
  end;
end;

procedure TXMLParser.setAttribute(const elName, name, _type: SAXString;
  const enumeration, value: SAXString;
  const valueType: Integer);
var attlist : THashtable;
    attribute : TSAXAttributeWrapper;
begin
  if (skippedPE) then
    Exit;

  // Create a new Hashtable if necessary.
  attlist:= getElementAttributes(elName);
  if (attlist = nil) then
    attlist:= THashtable.Create(1); //!!11

  // ignore multiple attribute declarations!
  if (attlist.get(name) <> nil) then
  begin
    // warn ...
    Exit;
  end else
  begin
    attribute:= TSAXAttributeWrapper.Create;
    attribute._type:= _type;
    attribute.value:= value;
    attribute.valueType:= valueType;
    attribute.enumeration:= enumeration;
    attribute.expandedValue:= '';
    attlist.put(name, attribute);
    // save; but don't overwrite any existing <!ELEMENT ...>
    setElement (elName, CONTENT_UNDECLARED, '', attlist);
  end;
end;

function TXMLParser.getAttribute(const elName,
  name: SAXString): TObject;
var attlist : THashtable;
begin
  attlist:= getElementAttributes(elName);
  if (attlist = nil) then
  begin
    Result:= nil;
    Exit;
  end;

  Result:= TObject(attlist.get(name));
  Exit;
end;

function TXMLParser.getEntityType(const ename: SAXString): Integer;
var entity : TSAXEntityWrapper;
begin
  entity:= TSAXEntityWrapper(entityInfo.get(ename));
  if (entity = nil) then
  begin
    Result:= ENTITY_UNDECLARED;
    Exit;
  end else
  begin
    Result:= entity.eClass;
    Exit;
  end;
end;

function TXMLParser.getEntityIds(const ename: SAXString): SAXStringArray;
var entity : TSAXEntityWrapper;
begin
  entity:= TSAXEntityWrapper(entityInfo.get(ename));
  if (entity = nil) then
  begin
    Result:= nil;
    Exit;
  end else
  begin
    Result:= entity.ids;
    Exit;
  end;
end;

function TXMLParser.getEntityValue(const ename: SAXString): SAXString;
var entity : TSAXEntityWrapper;
begin
  entity:= TSAXEntityWrapper(entityInfo.get(ename));
  if (entity = nil) then
  begin
    Result:= '';
    Exit;
  end else
  begin
    Result:= entity.value;
    Exit;
  end;
end;

procedure TXMLParser.setInternalEntity(const eName, value: SAXString);
var entity : TSAXEntityWrapper;
begin
  if (skippedPE) then
    Exit;

  if (entityInfo.get(eName) = nil) then
  begin
    entity:= TSAXEntityWrapper.Create;
    // FIXME: shrink!!  [2] useless
    entity.eClass:= ENTITY_INTERNAL;
    entity.value:= value;
    entityInfo.put(eName, entity);
  end;

  if ('lt' = eName) or ('gt' = eName) or ('quot' = eName) or
    ('apos' = eName) or ('amp' = eName) then
    Exit;

  TSAXDriverCracker(handler).getDeclHandler().internalEntityDecl(eName, value);
end;

procedure TXMLParser.setExternalEntity(const eName: SAXString;
  const eClass: Integer; const ids : SAXStringArray; const nName: SAXString);
var entity : TSAXEntityWrapper;
begin
  if (entityInfo.get(eName) = nil) then
  begin
    entity:= TSAXEntityWrapper.Create;
    entity.eClass:= eClass;
    entity.ids:= ids;
    // FIXME: shrink!!  [2] no longer used, [4] irrelevant given [0]
    entity.nName:= nName;
    entityInfo.put(eName, entity);
  end;
end;

procedure TXMLParser.setNotation(const nname : SAXString;
  const ids : SAXStringArray);
var notation : TSAXNotationWrapper;
begin
  if (skippedPE) then
    Exit;

  handler.notationDecl(nname, ids);
  if (notationInfo.get(nname) = nil) then
  begin
    notation:= TSAXNotationWrapper.Create;
    notationInfo.put(nname, notation);
  end else
  begin
    // VC: Unique Notation Name
    handler.verror('Duplicate notation name decl: ' + nname);
  end;
end;

function TXMLParser.getLineNumber: Integer;
begin
  if (useSavePos) then
    Result:= saveLine
  else
    Result:= line;
  Exit;
end;

function TXMLParser.getColumnNumber: Integer;
begin
  if (useSavePos) then
    Result:= saveColumn
  else
    Result:= column;
  Exit;
end;

function TXMLParser.readCh: SAXChar;
var c : SAXChar;
begin
  // As long as there's nothing in the
  // read buffer, try reading more data
  //(for an external entity) or popping
  // the entity stack(for either).
  while (readBufferPos >= readBufferLength) do
  begin
    case (sourceType) of
      INPUT_READER,
      INPUT_STREAM:
      begin
        readDataChunk();
        while(readBufferLength < 1) do
        begin
          popInput();
          if (readBufferLength < 1) then
          begin
            readDataChunk();
          end;
        end;
      end;
      else begin
        popInput();
      end;
    end;
  end;

  c:= readBuffer[readBufferPos];
  Inc(readBufferPos);

  if (c = #10) then
  begin
    Inc(line);
    column:= 0;
  end else
  begin
    if (c = '<') then
    begin
      // the most common return to parseContent () ... NOP
    end else if ((Word(c) < $0020) and (c <> #09) and (c <> #13)) or (Word(c) > $FFFD) then
    begin
      error('illegal XML character U+' + IntToHex(Word(c), 4));

    // If we're in the DTD and in a context where PEs get expanded,
    // do so ... 1/14/2000 errata identify those contexts.  There
    // are also spots in the internal subset where PE refs are fatal
    // errors, hence yet another flag.
    end else if ((c = '%')) then
    begin
      if (expandPE) then
      begin
        if (peIsError) then
          error('PE reference within decl in internal subset.');
        parsePEReference();
        Result:= readCh();
        Exit;
      end;
    end;
    Inc(column);
  end;

  Result:= c;
  Exit;
end;

procedure TXMLParser.unread(const c: SAXChar);
begin
  // Normal condition.
  if (c = #10) then
  begin
    Dec(line);
    column:= -1;
  end else
    //POS
    Dec(column);
  if (readBufferPos > 0) then
  begin
    Dec(readBufferPos);
    readBuffer[readBufferPos]:= c;
  end else
  begin
    pushString('', c);
  end
end;

procedure TXMLParser.unread(const ch: SAXCharArray;
  const length: Integer);
var i : Integer;
begin
  for i:= 0 to length-1 do
  begin
    if (ch[i] = #10) then
    begin
      Dec(line);
      column:= -1;
    end
  end;
  if (length < readBufferPos) then
  begin
    readBufferPos:= readBufferPos - length;
  end else
  begin
    pushCharArray('', ch, 0, length);
  end;
end;

procedure TXMLParser.pushURL(isPE : Boolean; const ename: SAXString;
  const ids : SAXStringArray; stream: TStream; encoding: SAXString;
  doResolve : Boolean);
var ignoreEncoding : Boolean;
    url : SAXString;
    systemId : SAXString;
    source : IInputSource;
    isis : IStreamInputSource;
    isis_stream : TStream;
begin
  if (not isPE) then
    dataBufferFlush();

  scratch.setPublicId(ids[0]);
  scratch.setSystemId(ids[1]);

  // See if we should skip or substitute the entity.
  // If we're not skipping, resolving reports startEntity()
  // and updates the (handler's) stack of URIs.
  if (doResolve) then
  begin
    // assert (stream == null && reader == null && encoding == null)
    source:= handler.resolveEntity(isPE, ename, scratch, ids[2]);
    if (source = nil) then
    begin
      handler.warn('skipping entity: ' + ename);
      handler.skippedEntity(ename);
      if (isPE) then
        skippedPE:= true;
      Exit;
    end;

    // we might be using alternate IDs/encoding
    systemId:= source.getSystemId();
    if (systemId = '') then
    begin
      handler.warn('missing system ID, using ' + ids[1]);
      systemId:= ids[1];
    end;
  end else
  begin
    // "[document]", or "[dtd]" via getExternalSubset()
    scratch.setByteStream(stream);
    scratch.setEncoding(encoding);
    source:= scratch;
    systemId:= ids[1];
    handler.startExternalEntity(ename, systemId, '[document]' = ename);
  end;

  if (Supports(source, IStreamInputSource, isis)) then
    isis_stream:= isis.getByteStream()
  else
    isis_stream:= nil;

  // we may have been given I/O streams directly
  if (isis_stream <> nil) then
  begin
    encoding:= source.getEncoding();
    stream:= isis_stream;
    // !! cannot handle special encoding handling here with a reader
  end else if (systemId = '') then
    error('InputSource has no URI!');
  scratch.setByteStream(nil);
  scratch.setEncoding('');

  // Push the existing status.
  pushInput(ename);

  // Create a new read buffer.
  //(Note the four-character margin)
  readBuffer:= nil;
  SetLength(readBuffer, READ_BUFFER_MAX + 4);
  readBufferPos:= 0;
  readBufferLength:= 0;
  readBufferOverflow:= -1;

//!!  if (Self.stream <> nil) and (Self.stream <> Self.baseStream) then Self.stream.Free;
  Self.stream:= nil;
  line:= 1;
  column:= 0;
  currentByteCount:= 0;

  // We handle the conversion, and need to ensure
  // it's done right.
  sourceType:= INPUT_STREAM;
  if (stream <> nil) then
  begin
    Self.stream:= stream;
    url:= '';
  end else
  begin
    // We have to open our own stream to the URL.

(*!!    url:= new URL(systemId);
        externalEntity:= url.openConnection();
        externalEntity.connect();
        is:= externalEntity.getInputStream();*)

    try
      externalEntity:= TFileStream.Create(systemId, fmOpenRead or fmShareDenyNone);
    except
      externalEntity:= nil;
      raise;
    end;
    Self.stream:= externalEntity;
  end;

  // If we get to here, there must be
  // an InputStream available.
(*!!  if (!is.markSupported()) then
  begin
    is:= new BufferedInputStream(is);
  end;*)

  // Get any external encoding label.
  if (encoding = '') and (externalEntity <> nil) then
  begin
    // External labels can be untrustworthy; filesystems in
    // particular often have the wrong default for content
    // that wasn't locally originated.  Those we autodetect.
    (*!!if ( not 'file' = (externalEntity.getURL().getProtocol())) then
    begin
      int temp;

      // application/xml;charset=something;otherAttr=...
      // ... with many variants on 'something'
      encoding:= externalEntity.getContentType();
      // MHK code (fix for Saxon 5.5.1/007):
      // protect against encoding==null
      if (encoding = '') then
      begin
        temp:= -1;
      end else
      begin
        temp:= encoding.indexOf("charset");
      end;

      // RFC 2376 sez MIME text defaults to ASCII, but since the
      // JDK will create a MIME type out of thin air, we always
      // autodetect when there's no explicit charset attribute.
      if (temp < 0)
        encoding:= null;  // autodetect
      else {
        // only this one attribute
        if ((temp:= encoding.indexOf(';')) > 0)
        encoding:= encoding.substring(0, temp);
        if ((temp = encoding.indexOf ('=', temp + 7)) > 0) {
        encoding = encoding.substring (temp + 1);
        // attributes can have comment fields(RFC 822)
        if ((temp:= encoding.indexOf('(')) > 0)
        encoding:= encoding.substring(0, temp);
        // ... and values may be quoted
        if ((temp:= encoding.indexOf('"')) > 0)
        encoding:= encoding.substring(temp + 1,
        encoding.indexOf('"', temp + 2));
        encoding.trim();
      } else {
        handler.warn ("ignoring illegal MIME attribute: " + encoding);
        encoding = null;
      }
    end;*)
  end;

  // if we got an external encoding label, use it ...
  if (encoding <> '') then
  begin
    self.encoding:= ENCODING_EXTERNAL;
    setupDecoding(encoding);
    ignoreEncoding:= true;
  // ... else autodetect from first bytes
  end else
  begin
    detectEncoding();
    ignoreEncoding:= false;
  end;

  // Read any XML or text declaration.
  // If we autodetected, it may tell us the "real" encoding.
  try
    tryEncodingDecl(ignoreEncoding);
  except
    on x : EUnsupportedEncodingException do
    begin
      error('unsupported text encoding', encoding, '');

//!! New method for encoding
{      encoding:= x.message;

      // if we don't handle the declared encoding,
      // try letting a JVM InputStreamReader do it
      try
        if (sourceType <> INPUT_STREAM) then
          raise x;

        //!! is.reset ();
        //!! readBufferPos = 0;
        //!! readBufferLength = 0;
        //!! readBufferOverflow = -1;
        //!! line = 1;
        //!! currentByteCount = column = 0;

        //!! sourceType = INPUT_READER;
        //!! Self.reader = new InputStreamReader (is, encoding);
        //!! Self.stream:= nil;

        tryEncodingDecl (true);

      except
        on e : Exception do
        begin
          error('unsupported text encoding', encoding, '');
        end;
      end;
      raise;}
    end;
  end;
end;

function TXMLParser.tryEncodingDecl(
  const ignoreEncoding: Boolean): SAXString;
begin
  // Read the XML/text declaration.
  if (tryRead('<?xml')) then
  begin
    if (tryWhitespace()) then
    begin
      if (inputStack.Count() > 0) then
      begin
        Result:= parseTextDecl(ignoreEncoding);
        Exit;
      end else
      begin
        Result:= parseXMLDecl(ignoreEncoding);
        Exit;
      end;
    end else
    begin
      unread(SAXChar('l'));
      unread(SAXChar('m'));
      unread(SAXChar('x'));
      unread(SAXChar('?'));
      unread(SAXChar('<'));
      parsePI();
    end;
  end;
  Result:= '';
  Exit;
end;

procedure TXMLParser.detectEncoding;
var signature : TByteDynarray;
    b : byte;
begin
  SetLength(signature, 4);
  // Read the first four bytes for
  // autodetection.
  stream.read(Pointer(signature)^, 4);
  stream.seek(0, 0);
  //
  // FIRST:  four byte encodings(who uses these?)
  //
  if (tryEncoding(signature, byte($00), byte($00),
     byte($00), byte($3c))) then
  begin
    // UCS-4 must begin with "<?xml"
    // 0x00 0x00 0x00 0x3c: UCS-4, big-endian(1234)
    // "UTF-32BE"
    encoding:= ENCODING_UCS_4_1234;

  end else if (tryEncoding(signature, byte($3c), byte($00),
    byte($00), byte($00))) then
  begin
    // 0x3c 0x00 0x00 0x00: UCS-4, little-endian(4321)
    // "UTF-32LE"
    encoding:= ENCODING_UCS_4_4321;

  end else if (tryEncoding(signature, byte($00), byte($00),
    byte($3c), byte($00))) then
  begin
    // 0x00 0x00 0x3c 0x00: UCS-4, unusual(2143)
    encoding:= ENCODING_UCS_4_2143;

  end else if (tryEncoding(signature, byte($00), byte($3c),
    byte($00), byte($00))) then
  begin
    // 0x00 0x3c 0x00 0x00: UCS-4, unusual(3421)
    encoding:= ENCODING_UCS_4_3412;

    // 00 00 fe ff UCS_4_1234(with BOM)
    // ff fe 00 00 UCS_4_4321(with BOM)
  end

  //
  // SECOND:  two byte encodings
  // note ... with 1/14/2000 errata the XML spec identifies some
  // more "broken UTF-16" autodetection cases, with no XML decl,
  // which we don't handle here(that's legal too).
  //
  else if (tryEncoding(signature, byte($fe), byte($ff))) then
  begin
    // UCS-2 with a byte-order marker.(UTF-16)
    // 0xfe 0xff: UCS-2, big-endian(12)
    encoding:= ENCODING_UCS_2_12;
    stream.read(b, 1);
    stream.read(b, 1);

  end else if (tryEncoding(signature, byte($ff), byte($fe))) then
  begin
    // UCS-2 with a byte-order marker.(UTF-16)
    // 0xff 0xfe: UCS-2, little-endian(21)
    encoding:= ENCODING_UCS_2_21;
    stream.read(b, 1);
    stream.read(b, 1);

  end else if (tryEncoding(signature, byte($00), byte($3c),
    byte($00), byte($3f))) then
  begin
    // UTF-16BE(otherwise, malformed UTF-16)
    // 0x00 0x3c 0x00 0x3f: UCS-2, big-endian, no byte-order mark
    encoding:= ENCODING_UCS_2_12;
    error('no byte-order mark for UCS-2 entity');

  end else if (tryEncoding(signature, byte($3c), byte($00),
    byte($3f), byte($00))) then
  begin
    // UTF-16LE(otherwise, malformed UTF-16)
    // 0x3c 0x00 0x3f 0x00: UCS-2, little-endian, no byte-order mark
    encoding:= ENCODING_UCS_2_21;
    error('no byte-order mark for UCS-2 entity');
  end

  //
  // THIRD:  ASCII-derived encodings, fixed and variable lengths
  //
  else if (tryEncoding(signature, byte($3c), byte($3f),
      byte($78), byte($6d))) then
  begin
    // ASCII derived
    // 0x3c 0x3f 0x78 0x6d: UTF-8 or other 8-bit markup(read ENCODING)
    encoding:= ENCODING_UTF_8;
    prefetchASCIIEncodingDecl();
  end else if ((signature[0] = byte($ef)) and
               (signature[1] = byte($bb)) and
               (signature[2] = byte($bf))) then
  begin
    // 0xef 0xbb 0xbf: UTF-8 BOM (not part of document text)
    // this un-needed notion slipped into XML 2nd ed through a
    // "non-normative" erratum; now required by MSFT and UDDI,
    // and E22 made it normative.
    encoding:= ENCODING_UTF_8;
    stream.read(b, 1);
    stream.read(b, 1);
    stream.read(b, 1);
  end else
  begin
    // 4c 6f a7 94 ... we don't understand EBCDIC flavors
    // ... but we COULD at least kick in some fixed code page

    //(default) UTF-8 without encoding/XML declaration
    encoding:= ENCODING_UTF_8;
  end;

end;

class function TXMLParser.tryEncoding(const sig: TByteDynarray; const b1,
  b2, b3, b4: Byte): Boolean;
begin
  Result:= ((sig[0] = b1) and (sig[1] = b2)
    and (sig[2] = b3) and (sig[3] = b4));
  Exit;
end;

class function TXMLParser.tryEncoding(const sig: TByteDynarray; const b1,
  b2: Byte): Boolean;
begin
  Result:= ((sig[0] = b1) and (sig[1] = b2));
  Exit;
end;

procedure TXMLParser.pushString(const ename, s: SAXString);
var ch : SAXCharArray;
begin
  SAXStringToSAXCharArray(s, ch);
  pushCharArray(ename, ch, 0, Length(ch));
end;

procedure TXMLParser.pushCharArray(const ename: SAXString;
  const ch: SAXCharArray; const start, length: Integer);
begin
  // Push the existing status
  pushInput(ename);
  if ((ename <> '') and (doReport)) then
  begin
    dataBufferFlush();
    handler.startInternalEntity(ename);
  end;
  sourceType:= INPUT_INTERNAL;
  readBuffer:= ch;
  readBufferPos:= start;
  readBufferLength:= length;
  readBufferOverflow:= -1;
end;

procedure TXMLParser.pushInput(const ename: SAXString);
var input : TSAXInputWrapper;
    i : Integer;
    e : SAXString;
begin
  // Check for entity recursion.
  if (ename <> '') then
  begin
    for i:= 0 to TSAXStack(entityStack).List.Count-1 do
    begin
      e:= TSAXStringWrapper(TSAXStack(entityStack).List[i]).Value;
      if ((e <> '') and (e = ename)) then
      begin
        error('recursive reference to entity', ename, '');
      end;
    end;
  end;
  entityStack.push(TSAXStringWrapper.Create(ename));

  // Don't bother if there is no current input.
  if (sourceType = INPUT_NONE) then
  begin
    Exit;
  end;

  // Set up a snapshot of the current
  // input source.
  input:= TSAXInputWrapper.Create;
  input.sourceType:= sourceType;
  input.externalEntity:= externalEntity;
  input.readBuffer:= readBuffer;
  input.readBufferPos:= readBufferPos;
  input.readBufferLength:= readBufferLength;
  input.line:= line;
  input.encoding:= encoding;
  input.readBufferOverflow:= readBufferOverflow;
  input.stream:= stream;
  input.currentByteCount:= currentByteCount;
  input.column:= column;

  // Push it onto the stack.
  inputStack.push(input);
end;

procedure TXMLParser.popInput;
var input : TSAXInputWrapper;
    w : TSAXStringWrapper;
    ename : SAXString;
begin
  w:= TSAXStringWrapper(entityStack.pop());
  ename:= w.Value;
  w.Free;

  if (ename <> '') and (doReport) then
    dataBufferFlush();

  case (sourceType) of
    INPUT_STREAM:
    begin
      handler.endExternalEntity(ename);
//!!      stream.close();
      externalEntity.Free();
      externalEntity:= nil;
      stream:= nil;
    end;
    //!! INPUT_READER
    INPUT_INTERNAL:
    begin
      if (ename <> '') and (doReport) then
        handler.endInternalEntity(ename);
    end;
  end;

  // Throw an EOFException if there
  // is nothing else to pop.
  if (not inputStack.AtLeast(1)) then
  begin
    raise EEOFException.Create('no more input');
  end;

  input:= TSAXInputWrapper(inputStack.pop());

  sourceType:= input.sourceType;
  externalEntity:= input.externalEntity;
  readBuffer:= input.readBuffer;
  readBufferPos:= input.readBufferPos;
  readBufferLength:= input.readBufferLength;
  line:= input.line;
  encoding:= input.encoding;
  readBufferOverflow:= input.readBufferOverflow;
  stream:= input.stream;
  currentByteCount:= input.currentByteCount;
  column:= input.column;

  // free the input
  input.Free;
end;

function TXMLParser.tryRead(const delim: SAXChar): Boolean;
var c : SAXChar;
begin
  // Read the character
  c:= readCh();

  // Test for a match, and push the character
  // back if the match fails.
  if (c = delim) then
  begin
    Result:= True;
    Exit;
  end else
  begin
    unread(c);
    Result:= False;
    Exit;
  end;
end;

function TXMLParser.tryRead(const delim: SAXString): Boolean;
var ch : SAXCharArray;
begin
  SAXStringToSAXCharArray(delim, ch);
  Result:= tryRead(ch);
end;

function TXMLParser.tryRead(const delim: SAXCharArray): Boolean;
var c : SAXChar;
    i : Integer;
begin
  // Compare the input, character-
  // by character.
  for i:= 0 to Length(delim)-1 do
  begin
    c:= readCh();
    if (c <> delim[i]) then
    begin
      unread(c);
      if(i <> 0) then
      begin
        unread(delim, i);
      end;
      Result:= False;
      Exit;
    end;
  end;
  Result:= True;
  Exit;
end;

function TXMLParser.tryWhitespace: Boolean;
var c : SAXChar;
begin
  c:= readCh();
  if (isWhitespace(c)) then
  begin
    skipWhitespace();
    Result:= True;
    Exit;
  end else
  begin
    unread(c);
    Result:= False;
    Exit;
  end;
end;

procedure TXMLParser.parseUntil(const delim: SAXString);
var ch : SAXCharArray;
begin
  SAXStringToSAXCharArray(delim, ch);
  parseUntil(ch);
end;

procedure TXMLParser.parseUntil(const delim: SAXCharArray);
var c : SAXChar;
    startLine : Integer;
    s : SAXString;
begin
  startLine:= line;
  try
    while (not tryRead(delim)) do
    begin
      c:= readCh();
      dataBufferAppend(c);
    end;
  except
    on e : EEOFException do
    begin
      SAXCharArrayToSAXString(delim, s);
      error('end of input while looking for delimiter '
      + '(started on line ' + IntToStr(startLine)
      + ')', '', s);
    end;
  end;
end;

procedure TXMLParser.prefetchASCIIEncodingDecl;
var ch : Byte;
    count : Integer;
begin
  readBufferLength:= 0;
  readBufferPos:= readBufferLength;

  // !! is.mark (readBuffer.length);

  while (true) do
  begin
    count:= stream.read(ch, 1);
    readBuffer[readBufferLength]:= SAXChar(ch);
    Inc(readBufferLength);
    if (char(ch) = '>') then
      Exit;
    // !! We must use "0" not "-1"
    if (count = 0) then
       error('file ends before end of XML or encoding declaration.', '', '?>');
    if (Length(readBuffer) = readBufferLength) then
      error('unfinished XML or encoding declaration');
  end;
end;

procedure TXMLParser.readDataChunk;
var count : Integer;
begin

  // See if we have any overflow(filterCR sets for CR at end)
  if (readBufferOverflow > -1) then
  begin
    readBuffer[0]:= SAXChar(readBufferOverflow);
    readBufferOverflow:= -1;
    readBufferPos:= 1;
    sawCR:= true;
  end else
  begin
    readBufferPos:= 0;
    sawCR:= false;
  end;

  // Read as many bytes as possible into the raw buffer.
  count:= stream.read(Pointer(rawReadBuffer)^, READ_BUFFER_MAX);

  // Dispatch to an encoding-specific reader method to populate
  // the readBuffer.  In most parser speed profiles, these routines
  // show up at the top of the CPU usage chart.
  if (count > 0) then
  begin
    case (encoding) of
      // one byte builtins
      ENCODING_ASCII:
      begin
        copyIso8859_1ReadBuffer(count, SAXChar($0080));
      end;
      ENCODING_UTF_8:
      begin
        copyUtf8ReadBuffer(count);
      end;
      ENCODING_ISO_8859_1:
      begin
        copyIso8859_1ReadBuffer(count, SAXChar(0));
      end;
      // two byte builtins
      ENCODING_UCS_2_12:
      begin
        copyUcs2ReadBuffer(count, 8, 0);
      end;
      ENCODING_UCS_2_21:
      begin
        copyUcs2ReadBuffer(count, 0, 8);
      end;
      // four byte builtins
      ENCODING_UCS_4_1234:
      begin
        copyUcs4ReadBuffer(count, 24, 16, 8, 0);
      end;
      ENCODING_UCS_4_4321:
      begin
        copyUcs4ReadBuffer(count, 0, 8, 16, 24);
      end;
      ENCODING_UCS_4_2143:
      begin
        copyUcs4ReadBuffer(count, 16, 24, 0, 8);
      end;
      ENCODING_UCS_4_3412:
      begin
        copyUcs4ReadBuffer(count, 8, 0, 24, 16);
      end;
    end;
  end else
    readBufferLength:= readBufferPos;

  readBufferPos:= 0;

  // Filter out all carriage returns if we've seen any
  //(including any saved from a previous read)
  if (sawCR) then
  begin
    // !! We must use " > 0" not " >= 0"
    filterCR(count > 0);
    sawCR:= false;

    // must actively report EOF, lest some CRs get lost.
    // !! We must use " > 0" not " >= 0"
    if ((readBufferLength = 0) and (count > 0)) then
      readDataChunk();
  end;

  if (count > 0) then
    currentByteCount:= currentByteCount + count;
end;

procedure TXMLParser.filterCR(const moreData: Boolean);
var i, j : Integer;
begin
  readBufferOverflow:= -1;

  i:= readBufferPos;
  j:= readBufferPos;
  while (j < readBufferLength) do
  begin
    case (readBuffer[j]) of
      #13:
      begin
        if (j = readBufferLength - 1) then
        begin
          if (moreData) then
          begin
            readBufferOverflow:= 13;
            Dec(readBufferLength);
          end else
          begin   // CR at end of buffer
            readBuffer[i]:= #10;
            Inc(i);
          end;
          break;
        end else if (readBuffer[j + 1] = #10) then
        begin
          Inc(j);
        end;
        readBuffer[i]:= #10;
      end;
      else begin
        readBuffer[i]:= readBuffer[j];
      end;
    end;

    Inc(i);
    Inc(j);
  end;
  readBufferLength:= i;
end;

procedure TXMLParser.copyUtf8ReadBuffer(const count: Integer);
var i, j, b1 : Integer;
//!! This should be SAXChar-- but we have to ensure that it is wide enough
//!! for the result
    c : WideChar;
    iso646 : Integer;
begin
  i:= 0;
  j:= readBufferPos;
  c:= SAXChar(0);

  (*
  // check once, so the runtime won't(if it's smart enough)
  if (count < 0 || count > rawReadBuffer.length)
    throw new ArrayIndexOutOfBoundsException(Integer.toString(count));
  *)

  while(i < count) do
  begin
    b1:= Integer(rawReadBuffer[i]);
    Inc(i);
    // Determine whether we are dealing
    // with a one-, two-, three-, or four-
    // byte sequence.
    if (b1 > 127) then
    begin
      if ((b1 and $e0) = $c0) then
      begin
        // 2-byte sequence: 00000yyyyyxxxxxx:= 110yyyyy 10xxxxxx
        c:= WideChar(((b1 and $1f) shl 6) or getNextUtf8Byte(i, count));
        Inc(i);
        if (Word(c) < $0080) then
          encodingError('Illegal two byte UTF-8 sequence', Word(c), 0);
      end else if ((b1 and $f0) = $e0) then
      begin
        // 3-byte sequence:
        // zzzzyyyyyyxxxxxx:= 1110zzzz 10yyyyyy 10xxxxxx
        // most CJKV characters

        //!! This should be SAXChar-- but we have to ensure that it is wide enough
        //!! for the result
        c:= WideChar(((b1 and $0f) shl 12) or (getNextUtf8Byte(i, count) shl 6) or getNextUtf8Byte(i+1, count));
        Inc(i, 2);
        if ((Word(c) < $0800) or ((Word(c) >= $d800) and (Word(c) <= $dfff))) then
          encodingError('Illegal three byte UTF-8 sequence', Word(c), 0);
      end else if ((b1 and $f8) = $f0) then
      begin
        // 4-byte sequence: 11101110wwwwzzzzyy + 110111yyyyxxxxxx
        //  := 11110uuu 10uuzzzz 10yyyyyy 10xxxxxx
        //(uuuuu:= wwww + 1)
        // "Surrogate Pairs" ... from the "Astral Planes"
        // Unicode 3.1 assigned the first characters there
        iso646:= b1 and 07;
        iso646:=(iso646 shl 6) + getNextUtf8Byte(i, count);
        iso646:=(iso646 shl 6) + getNextUtf8Byte(i+1, count);
        iso646:=(iso646 shl 6) + getNextUtf8Byte(i+2, count);
        Inc(i, 3);

        if (iso646 <= $ffff) then
        begin
          encodingError('Illegal four byte UTF-8 sequence', iso646, 0);
        end else
        begin
          if (iso646 > $0010ffff) then
            encodingError('UTF-8 value out of range for Unicode', iso646, 0);
          iso646:= iso646 - $010000;
          readBuffer[j]:= SAXChar($d800 or (iso646 shr 10));
          Inc(j);
          readBuffer[j]:= SAXChar($dc00 or (iso646 and $03ff));
          Inc(j);
          continue;
        end;
      end else
      begin
        // The five and six byte encodings aren't supported;
        // they exceed the Unicode(and XML) range.
        encodingError('unsupported five or six byte UTF-8 sequence', $ff and b1, i);
        // NOTREACHED
        c:= SAXChar(0);
      end;
    end else
    begin
      // 1-byte sequence: 000000000xxxxxxx:= 0xxxxxxx
      //(US-ASCII character, "common" case, one branch to here)
      c:= WideChar(b1);
    end;
    readBuffer[j]:= SAXChar(c);
    Inc(j);
    if (c = #13) then
      sawCR:= true;
  end;
  // How many characters have we read?
  readBufferLength:= j;
end;

function TXMLParser.getNextUtf8Byte(const pos, count: Integer): Integer;
var val, c : Integer;
begin
  // Take a character from the buffer
  // or from the actual input stream.
  if (pos < count) then
  begin
    val:= Integer(rawReadBuffer[pos]);
  end else
  begin
    c:= stream.read(val, sizeOf(Val));
    // !! We must use "0" not "-1"
    if (c = 0) then
    begin
      encodingError('unfinished multi-byte UTF-8 sequence at EOF', -1, pos);
    end;
  end;

  // Check for the correct bits at the start.
  if ((val and $c0) <> $80) then
  begin
    encodingError('bad continuation of multi-byte UTF-8 sequence', val, pos + 1);
  end;

  // Return the significant bits.
  Result:= (val and $3f);
  Exit;
end;

procedure TXMLParser.copyIso8859_1ReadBuffer(const count: Integer;
  const mask: SAXChar);
var i, j : Integer;
    c : SAXChar;
begin
  j:= readBufferPos;
  for i:= 0 to count-1 do
  begin
    c:= SAXChar(Word(rawReadBuffer[i]) and $ff);
    if ((Word(c) and Word(mask)) <> 0) then
      raise ECharConversionException.Create('non-ASCII character U+' + IntToHex(Word(c), 4));
    readBuffer[j]:= c;
    if (c = #13) then
    begin
      sawCR:= true;
    end;
    Inc(j);
  end;
  readBufferLength:= j;
end;

procedure TXMLParser.copyUcs2ReadBuffer(const count, shift1,
  shift2: Integer);
var i, j : Integer;
    c : SAXChar;
begin
  j:= readBufferPos;

  if ((count > 0) and ((count mod 2) <> 0)) then
  begin
    encodingError('odd number of bytes in UCS-2 encoding', -1, count);
  end;
  // The loops are faster with less internal brancing; hence two
  if (shift1 = 0) then  // "UTF-16-LE"
  begin
    i:= 0;
    while (i < count) do
    begin
      c:= SAXChar(Word(rawReadBuffer[i + 1]) shl 8);
      c:= SAXChar(Word(c) or ($ff and Word(rawReadBuffer[i])));
      readBuffer[j]:= c;
      Inc(j);
      if (c = #13) then
        sawCR:= true;
      Inc(i, 2);
    end;
  end else  // "UTF-16-BE"
  begin
    i:= 0;
    while (i < count) do
    begin
      c:= SAXChar(Word(rawReadBuffer[i]) shl 8);
      c:= SAXChar(Word(c) or ($ff and Word(rawReadBuffer[i + 1])));
      readBuffer[j]:= c;
      Inc(j);
      if (c = #13) then
        sawCR:= true;
      Inc(i, 2);
    end;
  end;
  readBufferLength:= j;
end;

procedure TXMLParser.copyUcs4ReadBuffer(const count, shift1, shift2,
  shift3, shift4: Integer);
var i, j, value : Integer;
begin
  j:= readBufferPos;

  if (count > 0) and ((count mod 4) <> 0) then
  begin
    encodingError('number of bytes in UCS-4 encoding not divisible by 4', -1, count);
  end;
  i:= 0;
  while (i < count) do
  begin
    value:= (((Word(rawReadBuffer[i]) and $ff) shl shift1) or
             ((Word(rawReadBuffer[i + 1]) and $ff) shl shift2) or
             ((Word(rawReadBuffer[i + 2]) and $ff) shl shift3) or
             ((Word(rawReadBuffer[i + 3]) and $ff) shl shift4));
    if (value < $0000ffff) then
    begin
      readBuffer[j]:= SAXChar(value);
      Inc(j);
      if (value = $0D) then
      begin
        sawCR:= true;
      end;
    end else if (value < $0010ffff) then
    begin
      value:= value - $010000;
      readBuffer[j]:= SAXChar($d8 or ((value shr 10) and $03ff));
      Inc(j);
      readBuffer[j]:= SAXChar($dc or (value and $03ff));
      Inc(j);
    end else
    begin
      encodingError('UCS-4 value out of range for Unicode', value, i);
    end;
    Inc(i, 4);
  end;
  readBufferLength:= j;
end;

procedure TXMLParser.encodingError(_message: SAXString; const value,
  offset: Integer);
begin
  if (value <> -1) then
    _message:= _message + '(character code: 0x' + IntToHex(value, 4) + ')';
  error(_message);
end;

procedure TXMLParser.initializeVariables;
begin
  // First line
  line:= 1;
  column:= 0;
  saveLine:= -1;
  saveColumn:= -1;
  useSavePos:= False;


  // Set up the buffers for data and names
  dataBufferPos:= 0;
  dataBuffer:= nil;
  SetLength(dataBuffer, DATA_BUFFER_INITIAL);
  nameBufferPos:= 0;
  nameBuffer:= nil;
  SetLength(nameBuffer, NAME_BUFFER_INITIAL);

  //!! These should be freed if nec...?

  // Set up the DTD hash tables
  elementInfo:= THashtable.Create(1); //!! 511
  entityInfo:= THashtable.Create(1); //!! When 63 it doesn't work anymore!
  notationInfo:= THashtable.Create(1);
  skippedPE:= false;

  // Set up the variables for the current
  // element context.
  currentElement:= '';
  currentElementContent:= CONTENT_UNDECLARED;

  // Set up the input variables
  sourceType:= INPUT_NONE;
  inputStack:= TSAXStack.Create();
  entityStack:= TSAXStack.Create();
  externalEntity:= nil;
  tagAttributePos:= 0;
  tagAttributes:= nil;
  rawReadBuffer:= nil;
  SetLength(tagAttributes, 100);
  SetLength(rawReadBuffer, READ_BUFFER_MAX);
  readBufferOverflow:= -1;

  scratch:= TStreamInputSource.Create(nil, soReference) as IStreamInputSource;

  inLiteral:= false;
  expandPE:= false;
  peIsError:= false;

  doReport:= false;

  inCDATA:= false;

//!!  SetLength(symbolTable, SYMBOL_TABLE_LENGTH);
end;

procedure TXMLParser.cleanupVariables;
var i, j : Integer;
    p, p2 : TPointerDynarray;
begin
  useSavePos:= False;

  dataBuffer:= nil;
  nameBuffer:= nil;

  p:= nil;
  p2:= nil;

  if (elementInfo <> nil) then
  begin
    p:= elementInfo.getElements();
    for i:= 0 to elementInfo.Count-1 do
    begin
      if (p[i] <> nil) then
      begin
        if (TSAXElementWrapper(p[i]).attList <> nil) then
        begin
          // free the attlist attribute wrappers
          p2:= TSAXElementWrapper(p[i]).attList.getElements();
          for j:= 0 to TSAXElementWrapper(p[i]).attList.Count-1 do
            if (p2[j] <> nil) then TSAXAttributeWrapper(p2[j]).Free;
          // free the attlist
          TSAXElementWrapper(p[i]).attList.Free;
        end;
        // free this list
        TSAXElementWrapper(p[i]).Free;
      end;
    end;
  end;

  if (entityInfo <> nil) then
  begin
    p:= entityInfo.getElements();
    for i:= 0 to entityInfo.Count-1 do
    begin
      // free this list
      if (p[i] <> nil) then TSAXEntityWrapper(p[i]).Free;
    end;
  end;

  if (notationInfo <> nil) then
  begin
    p:= notationInfo.getElements();
    for i:= 0 to notationInfo.Count-1 do
    begin
      // free this list
      if ((p[i]) <> nil) then TSAXNotationWrapper(p[i]).Free;
    end;
  end;

  elementInfo.Free();
  entityInfo.Free();
  notationInfo.Free();
  elementInfo:= nil;
  entityInfo:= nil;
  notationInfo:= nil;

  currentElement:= '';;

  // try to popInput first...
  if (inputStack <> nil) then
  begin
    while inputStack.Count > 0 do
      popInput();
  end;

  // This code from popInput to free the current stream
  case (sourceType) of
    INPUT_STREAM:
    begin
      externalEntity.Free();
      externalEntity:= nil;
      stream:= nil;
    end;
  end;

  if (entityStack <> nil) then
  begin
    for i:= 0 to entityStack.Count-1 do
      TSAXStringWrapper(TSAXStack(entityStack).List[i]).Free;
  end;

  if (inputStack <> nil) then
  begin
    for i:= 0 to inputStack.Count-1 do
      TSAXInputWrapper(TSAXStack(inputStack).List[i]).Free;
  end;

  inputStack.Free();
  entityStack.Free();
  inputStack:= nil;
  entityStack:= nil;
  externalEntity:= nil;
  stream:= nil;

  tagAttributes:= nil;
  rawReadBuffer:= nil;

  scratch:= nil;
end;

end.
