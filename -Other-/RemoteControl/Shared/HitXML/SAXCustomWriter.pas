unit SAXCustomWriter;

  {*************************************************************************}
  {                                                                         }
  {  This unit expands features of the SAX for Pascal. It corresponds       }
  {  interface and implimentation to write data xml file.		    }
  {                                                                         }
  {                                                                         }
  {  Copyright (c) 2000-2008 Hitsoft LLC.                                   }
  {                                                                         }
  {*************************************************************************}


  {*************************************************************************}
  {                                                                         }
  {  Hitsoft Xml Object Library                                             }
  {                                                                         }
  {  Copyright (C) 2009    Hitsoft LLC. (http://opensource.hitsoft-it.com)  }
  {                                                                         }
  {  This program is free software: you can redistribute it and/or modify   }
  {  it under the terms of the GNU General Public License as published by   }
  {  the Free Software Foundation, either version 3 of the License, or      }
  {  (at your option) any later version.                                    }
  {                                                                         }
  {  This program is distributed in the hope that it will be useful,        }
  {  but WITHOUT ANY WARRANTY; without even the implied warranty of         }
  {  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          }
  {  GNU General Public License for more details.                           }
  {                                                                         }
  {  You should have received a copy of the GNU General Public License      }
  {  along with this program.  If not, see <http://www.gnu.org/licenses/>.  }
  {                                                                         }
  {*************************************************************************}

interface
uses
  Classes, SAX, SAXExt;

type
  IXMLWriter = interface(IContentHandler)
  ['{D6545614-F93A-481F-9BFA-7E68371714F8}']
    function GetEmptyShortForm: Boolean;
    procedure SetEmptyShortForm(Value: Boolean);
    function GetExpandEntities: Boolean;
    procedure SetExpandEntities(Value: Boolean);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetIncludeProlog: Boolean;
    procedure SetIncludeProlog(Value: Boolean);
    function GetIndent: Boolean;
    procedure SetIndent(Value: Boolean);
    function GetStandalone: Boolean;
    procedure SetStandalone(Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(Value: string);
    function GetStream: TStream;
    procedure SetStream(const Value: TStream);

    property EmptyShortForm: Boolean read GetEmptyShortForm write SetEmptyShortForm;
    property ExpandEntities: Boolean read GetExpandEntities write SetExpandEntities;
    property FileName: string read GetFileName write SetFileName;
    property Stream: TStream read GetStream write SetStream;
    property IncludeProlog: Boolean read GetIncludeProlog write SetIncludeProlog;
    property Indent: Boolean read GetIndent write SetIndent;
    property Standalone: Boolean read GetStandalone write SetStandalone;
    property Version: string read GetVersion write SetVersion;
  end;

function CreateWriter: IXMLWriter;
implementation
uses
  SysUtils, StrUtils;

resourcestring
  MismatchElement = 'Mismatched element: %s vs %s';
  NoReader        = 'No XMLReader has been established';
  NoVendor        = 'Unknown SAX vendor %s';
  OpenElements    = 'Open elements at end of document';

type
  TSAXCustomDocumentWriter = class(TInterfacedObject, IContentHandler, IDeclHandler,
    IDTDHandler, ILexicalHandler, IXMLWriter)
  private
    FCurIndent: Integer;        // Current indent level in characters
    FElements: TStringList;     // List of elements currently open
    FEmptyShortForm: Boolean;   // True to use the short form for empty elements,
                                // false to write them out in full
    FExpandEntities: Boolean;   // True to write out entity contents rather than
                                // the reference, false to always write the reference
    FFilename: string;          // The name of the output file, or '' if in memory
    FHasEntityContent: Boolean; // True if in an entity and encountered content
    FHasSubElements: Boolean;   // Flag for indenting elements based on content
    FIncludeProlog: Boolean;    // True to output an XML prolog, false to bypass
    FIndent: Boolean;           // True to indent generated XML, false to leave as is
    FInDTD: Boolean;            // True if in DTD and need to close it, false otherwise
    FInEntity: Boolean;         // True if in entity reference and ignoring events
    FStandAlone: Boolean;       // True if standalone, false otherwise
    FStream: TStream;           // The stream being written to
    FUserStream: Boolean;       // True if user supplied a stream,
                                // false if created based on filename or in memory
    FVersion: string;           // The XML version, defaults to '1.0'
    FXMLNS: SAXString;          // Temporary storage for namespace declarations
    procedure CheckInDTD;
    procedure CheckInElement(NewElement: Boolean);
    procedure CheckNotInDTD;
  protected
    function GetPublicSystem(const PublicId, SystemId: SAXString): SAXString;
    function MakeIndent(Level: Integer): string; virtual;
    procedure Write(const Text: SAXString); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { IContentHandler }
    procedure SetDocumentLocator(const Locator: ILocator); virtual;
    procedure StartDocument; virtual;
    procedure EndDocument; virtual;
    procedure StartPrefixMapping(const Prefix, URI: SAXString); virtual;
    procedure EndPrefixMapping(const Prefix: SAXString); virtual;
    procedure StartElement(const NamespaceURI, LocalName, QName: SAXString;
      const Atts: IAttributes); virtual;
    procedure EndElement(const NamespaceURI, LocalName, QName: SAXString);
      virtual;
    procedure Characters(const PCh: SAXString); virtual;
    procedure IgnorableWhitespace(const PCh: SAXString);
      virtual;
    procedure ProcessingInstruction(const Target, Data: SAXString); virtual;
    procedure SkippedEntity(const Name: SAXString); virtual;
    { IDeclHandler }
    procedure AttributeDecl(
      const EName, AName, AttrType, ValueDefault, Value: SAXString); virtual;
    procedure ElementDecl(const Name, Model: SAXString); virtual;
    procedure ExternalEntityDecl(const Name, PublicId, SystemId: SAXString);
      virtual;
    procedure InternalEntityDecl(const Name, Value: SAXString); virtual;
    { IDTDHandler }
    procedure NotationDecl(const Name, PublicId, SystemId: SAXString); virtual;
    procedure UnparsedEntityDecl(const Name, PublicId, SystemId,
      NotationName: SAXString); virtual;
    { ILexicalHandler }
    procedure Comment(const PCh: SAXString); virtual;
    procedure EndCData; virtual;
    procedure EndDTD; virtual;
    procedure EndEntity(const Name: SAXString); virtual;
    procedure StartCData; virtual;
    procedure StartDTD(const Name, PublicId, SystemId: SAXString); virtual;
    procedure StartEntity(const Name: SAXString); virtual;
    { IXMLWriter }
    function GetEmptyShortForm: Boolean;
    procedure SetEmptyShortForm(Value: Boolean);
    function GetExpandEntities: Boolean;
    procedure SetExpandEntities(Value: Boolean);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    function GetIncludeProlog: Boolean;
    procedure SetIncludeProlog(Value: Boolean);
    function GetIndent: Boolean;
    procedure SetIndent(Value: Boolean);
    function GetStandalone: Boolean;
    procedure SetStandalone(Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(Value: string);
    function GetStream: TStream;
    procedure SetStream(const Value: TStream);

    property EmptyShortForm: Boolean read GetEmptyShortForm write SetEmptyShortForm default True;
    property ExpandEntities: Boolean read GetExpandEntities write SetExpandEntities default False;
    property Filename: string read GetFilename write SetFilename;
    property IncludeProlog: Boolean read GetIncludeProlog write SetIncludeProlog    default True;
    property Indent: Boolean read GetIndent write SetIndent default False;
    property Standalone: Boolean read GetStandAlone write SetStandAlone;
    property Version: string read GetVersion write SetVersion;
  end;


function CreateWriter: IXMLWriter;
begin
  Result := TSAXCustomDocumentWriter.Create;
end;

function ReplaceSpecChars(const S: string): string;
  procedure ReplaceSpecChar(C: String; Replace: string);
  var
    CurrPos: Integer;
    I: Integer;
  begin
    CurrPos := 1;
    repeat
      I := PosEx(C, Result, CurrPos);
      if I > 0 then
      begin
        Result := Copy(Result, 1, I-1) + Replace + Copy(Result, I+1, MaxInt);
        CurrPos := I + 1;
      end;
    until I = 0;
  end;

  procedure ReplaceAnds;
  var
    I: Integer;
    Res: string;
  begin
    if Pos('&', Result) > 0 then
    begin
      Res := '';
      for I := Pos('&', Result) to Length(Result) do
      begin
        if S[I] = '&' then
          Res := Res + '&#38;'
        else
          Res := Res + S[I];
      end;
      Result := Res;
    end;
  end;

begin
  Result := S;
  ReplaceSpecChar('&', '&#38;');
  ReplaceSpecChar('"', '&#34;');
  ReplaceSpecChar('<', '&#60;');
  ReplaceSpecChar('>', '&#62;');
  ReplaceSpecChar(#13, '&#13;');
  ReplaceSpecChar(#10, '&#10;');
end;

{ TSAXCustomDocumentWriter ----------------------------------------------------------}

const
  CRLF: array [Boolean] of string = ('', #13#10);
  ELEM_OPEN    = 1;
  ELEM_CONTENT = 2;

constructor TSAXCustomDocumentWriter.Create;
begin
  inherited Create;
  FElements       := TStringList.Create;
  FEmptyShortForm := True;
  FExpandEntities := False;
  FIncludeProlog  := True;
  FIndent         := False;
  FStream         := nil;
  FUserStream     := False;
  FVersion        := '1.0';
end;

destructor TSAXCustomDocumentWriter.Destroy;
begin
  FElements.Free;
  if (not FUserStream) and (FStream <> nil) then
  begin
{$IFDEF VER120}  { Delphi 4 }
    FStream.Free;
    FStream := nil;
{$ELSE}
    FreeAndNil(FStream);
{$ENDIF}
  end;
  inherited Destroy;
end;

procedure TSAXCustomDocumentWriter.AttributeDecl(const EName, AName, AttrType,
  ValueDefault, Value: SAXString);
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  CheckInDTD;
  Write('<!ATTLIST ' + EName + ' ' + AName + ' (' + AttrType + ') ' +
    ValueDefault + ' ' + Value + '>' + CRLF[Indent]);
end;

procedure TSAXCustomDocumentWriter.Characters(const PCh: SAXString);
var
  AllWhiteSpace: Boolean;
  Index: Integer;
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  AllWhiteSpace     := Indent;
  if Indent then
    // Ignore strings that are all whitespace
    for Index := 1 to Length(PCh) do
      if (PCh[Index] <> ' ') and (PCh[Index] <> #9) and
        (PCh[Index] <> #10) and (PCh[Index] <> #13) then
      begin
        AllWhiteSpace := False;
        Break;
      end;
  if not AllWhiteSpace then
  begin
    CheckInElement(False);
    Write(PCh);
  end;
end;

{ Write out a DTD entry if necessary, for example when not using
  LexicalHandler and notations or entities are encountered (DTDHandler). }
procedure TSAXCustomDocumentWriter.CheckInDTD;
begin
  if not FInDTD then
  begin
    FInDTD := True;
    Write('<!DOCTYPE [' + CRLF[Indent]);
    Inc(FCurIndent);
  end;
end;

{ Close off an open element if necessary - allows for the detection
  and proper display of empty elements in the shorthand form. }
procedure TSAXCustomDocumentWriter.CheckInElement(NewElement: Boolean);
begin
  if FElements.Count = 0 then
    Exit;
  if Integer(FElements.Objects[FElements.Count - 1]) = ELEM_OPEN then
  begin
    Write('>' + CRLF[NewElement and Indent]);
    FElements.Objects[FElements.Count - 1] := TObject(ELEM_CONTENT);
  end;
end;

{ Ensure that any open DTD is closed before continuing. }
procedure TSAXCustomDocumentWriter.CheckNotInDTD;
begin
  if FInDTD then
  begin
    FInDTD := False;
    Write(']>' + CRLF[Indent]);
    Dec(FCurIndent);
  end;
end;

procedure TSAXCustomDocumentWriter.Comment(const PCh: SAXString);
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  CheckInElement(False);
  Write('<!--' + PCh + '-->' + CRLF[Indent]);
end;

procedure TSAXCustomDocumentWriter.ElementDecl(const Name, Model: SAXString);
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  CheckInDTD;
  Write(MakeIndent(FCurIndent) + '<!ELEMENT ' + Name + ' ' + Model + '>' +
    CRLF[Indent]);
end;

procedure TSAXCustomDocumentWriter.EndCData;
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  Write(']]>');
end;

{ Check for any unclosed elements and raise an error if found. }
procedure TSAXCustomDocumentWriter.EndDocument;
begin
  if (not FUserStream) and (FStream <> nil) then
  begin
{$IFDEF VER120}  { Delphi 4 }
    FStream.Free;
    FStream := nil;
{$ELSE}
    FreeAndNil(FStream);
{$ENDIF}
  end;

  if FElements.Count <> 0 then
    raise ESAXException.Create(OpenElements);
end;

procedure TSAXCustomDocumentWriter.EndDTD;
begin
  Write(']>' + CRLF[Indent]);
  FInDTD := False;
end;

{ Close off an element, using the shorthand form if appropriate.
  Also check that elements are closed off in the reverse order to being
  opened - the well-formedness check.
  Handle indenting as necessary. }
procedure TSAXCustomDocumentWriter.EndElement(const NamespaceURI, LocalName,
  QName: SAXString);
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  if FElements[FElements.Count - 1] <> QName then
    raise ESAXException.Create(
      Format(MismatchElement, [QName, FElements[FElements.Count - 1]]));

  if FHasSubElements then
    Dec(FCurIndent);
  if Integer(FElements.Objects[FElements.Count -1]) = ELEM_OPEN then
  begin
    if EmptyShortForm then
      Write('/>' + CRLF[Indent])
    else
      Write('></' + QName + '>' + CRLF[Indent]);
  end
  else
  begin
    if FHasSubElements then
      Write(MakeIndent(FCurIndent) + '</' + QName + '>' + CRLF[Indent])
    else
      Write('</' + QName + '>' + CRLF[Indent]);
  end;
  if not FHasSubElements then
    Dec(FCurIndent);
  FHasSubElements := True;
  FElements.Delete(FElements.Count - 1);
end;

procedure TSAXCustomDocumentWriter.EndEntity(const Name: SAXString);
begin
  FInEntity := False;
  if ExpandEntities and not FHasEntityContent then
    Write('&' + Name + ';');
end;

procedure TSAXCustomDocumentWriter.EndPrefixMapping(const Prefix: SAXString);
begin
  // Do nothing
end;

procedure TSAXCustomDocumentWriter.ExternalEntityDecl(const Name, PublicId,
  SystemId: SAXString);
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  CheckInDTD;
  Write(MakeIndent(FCurIndent) + '<!ENTITY ' + Name + ' ' +
    GetPublicSystem(PublicId, SystemId) + '>' + CRLF[Indent]);
end;

{ Format public and system id references for output. }
function TSAXCustomDocumentWriter.GetPublicSystem(
  const PublicId, SystemId: SAXString): SAXString;
begin
  if PublicId <> '' then
  begin
    Result := 'PUBLIC "' + PublicId + '"';
    if SystemId <> '' then
      Result := Result + ' "' + SystemId + '"';
  end
  else
    Result := 'SYSTEM "' + SystemId + '"';
end;

{ Only ignore this whitespace if indenting ourselves. }
procedure TSAXCustomDocumentWriter.IgnorableWhitespace(const PCh: SAXString);
begin
  if FInEntity then
    Exit;
  if not Indent then
  begin
    FHasEntityContent := True;
    CheckInElement(False);
    Write(PCh);
  end;
end;

procedure TSAXCustomDocumentWriter.InternalEntityDecl(
  const Name, Value: SAXString);
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  CheckInDTD;
  Write(MakeIndent(FCurIndent) + '<!ENTITY ' + Name + ' ' + Value + '>' +
    CRLF[Indent]);
end;

{ Build an indent of the appropriate depth. }
function TSAXCustomDocumentWriter.MakeIndent(Level: Integer): string;
var
  Index: Integer;
begin
  Result := '';
  if Indent then
    for Index := 1 to Level do
      Result := Result + '  ';
end;

procedure TSAXCustomDocumentWriter.NotationDecl(const Name, PublicId, SystemId:
  SAXString);
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  CheckInDTD;
  Write(MakeIndent(FCurIndent) + '<!NOTATION ' + Name + ' ' +
    GetPublicSystem(PublicId, SystemId) + '>' + CRLF[Indent]);
end;

procedure TSAXCustomDocumentWriter.ProcessingInstruction(const Target,
  Data: SAXString);
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  CheckInElement(False);
  Write('<?' + Target + ' ' + Data + '?>' + CRLF[Indent]);
end;

procedure TSAXCustomDocumentWriter.SetDocumentLocator(const Locator: ILocator);
begin
  // Do nothing
end;

procedure TSAXCustomDocumentWriter.SetFileName(const Value: string);
begin
  if FFilename <> Value then
  begin
    FFilename   := Value;
    FUserStream := False;
  end;
end;

{ Save the stream to write to. If this is nil a memory stream is used,
  which can then be read out by the user. }
procedure TSAXCustomDocumentWriter.SetStream(const Value: TStream);
begin
  if FStream <> Value then
  begin
    if (not FUserStream) and (FStream <> nil) then
    begin
{$IFDEF VER120}  { Delphi 4 }
      FStream.Free;
      FStream     := nil;
{$ELSE}
      FreeAndNil(FStream);
{$ENDIF}
    end;
    FStream     := Value;
    FFilename   := '';
    FUserStream := Assigned(Value);
  end;
end;

procedure TSAXCustomDocumentWriter.SkippedEntity(const Name: SAXString);
begin
  Write('&' + Name + ';');
end;

procedure TSAXCustomDocumentWriter.StartCData;
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  Write('<![CDATA[');
end;

{ Prepare the output stream for use and initialise variables for generating. }
procedure TSAXCustomDocumentWriter.StartDocument;
const
  YesNo: array [Boolean] of string = ('no', 'yes');
begin
  if (not FUserStream) and (FStream <> nil) then
  begin
{$IFDEF VER120}  { Delphi 4 }
    FStream.Free;
    FStream := nil;
{$ELSE}
    FreeAndNil(FStream);
{$ENDIF}
  end;

  if FFilename <> '' then
    FStream := TFileStream.Create(FFilename, fmCreate or fmShareExclusive)
  else if not FUserStream then
    FStream := TMemoryStream.Create;
  FStream.Size    := 0;
  FCurIndent      := 0;
  FHasSubElements := False;
  FInDTD          := False;
  FInEntity       := False;
  FXMLNS          := '';
  FElements.Clear;
  if IncludeProlog then
//    Write('<?xml version="' + Version + '" encoding="ISO-8859-1" standalone="' +
//      YesNo[StandAlone] + '"?>' + CRLF[Indent]);
    Write('<?xml version="' + Version + '" encoding="UTF-8" standalone="' +
      YesNo[StandAlone] + '"?>' + CRLF[Indent]);
end;

procedure TSAXCustomDocumentWriter.StartDTD(const Name, PublicId,
  SystemId: SAXString);
begin
  FInDTD := True;
  Write('<!DOCTYPE ' + Name + ' ' + GetPublicSystem(PublicId, SystemId) + '[' +
    CRLF[Indent]);
  Inc(FCurIndent);
end;

procedure TSAXCustomDocumentWriter.StartElement(const NamespaceURI, LocalName,
  QName: SAXString; const Atts: IAttributes);
var
  Index: Integer;
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  CheckNotInDTD;
  CheckInElement(True);
  Write(MakeIndent(FCurIndent) + '<' + QName);
  Write(FXMLNS);
  if Assigned(Atts) then
  begin
    for Index := 0 to Atts.Length - 1 do
      Write(' ' + Atts.GetQName(Index) + '="' + ReplaceSpecChars(Atts.GetValue(Index)) + '"');
  end;
  Inc(FCurIndent);
  FHasSubElements := False;
  FXMLNS          := '';
  FElements.AddObject(QName, TObject(ELEM_OPEN));
end;

{ Ignore entity contents, just output the reference. }
procedure TSAXCustomDocumentWriter.StartEntity(const Name: SAXString);
begin
  if not ExpandEntities then
    Write('&' + Name + ';');
  FInEntity         := not ExpandEntities;
  FHasEntityContent := False;
end;

{ Accumulate namespace declarations for inclusion on the next element. }
procedure TSAXCustomDocumentWriter.StartPrefixMapping(
  const Prefix, URI: SAXString);
begin
  FXMLNS := FXMLNS + ' xmlns:' + Prefix + '="' + URI + '"';
end;

procedure TSAXCustomDocumentWriter.UnparsedEntityDecl(const Name, PublicId,
  SystemId, NotationName: SAXString);
begin
  if FInEntity then
    Exit;
  FHasEntityContent := True;
  CheckInDTD;
  Write(MakeIndent(FCurIndent) + '<!ENTITY ' + Name + ' ' +
    GetPublicSystem(PublicId, SystemId) + ' NDATA ' + NotationName + '>' +
    CRLF[Indent]);
end;

{ Actually write data to the stream (in the appropriate format). }
procedure TSAXCustomDocumentWriter.Write(const Text: SAXString);
var
//  Str: string;
//  Len: Integer;
  UStr: UTF8String;
begin
  //Len  := Length(Text) * SizeOf(SAXChar);
  UStr := Utf8Encode(Text);
//  if Length(UStr) < Len then
//  begin
//    Str := Text;
//    if Str <> UStr then
//      FStream.Write(Pointer(UStr)^, Length(UStr) * StringElementSize(UStr))
    if Length(UStr) > 0 then
      FStream.Write(UStr[1], Length(UStr))
//    else
//      FStream.Write(Pointer(Str)^, Length(Str) * StringElementSize(Str));
//      FStream.Write(Str[1], Length(Str) * StringElementSize(Str));
//  end
//  else
//    FStream.Write(PChar(Text)^, Len * StringElementSize(Text));
end;

function TSAXCustomDocumentWriter.GetEmptyShortForm: Boolean;
begin
  Result := FEmptyShortForm
end;

function TSAXCustomDocumentWriter.GetExpandEntities: Boolean;
begin
  Result := FExpandEntities
end;

function TSAXCustomDocumentWriter.GetFileName: string;
begin
  Result := FFilename
end;

function TSAXCustomDocumentWriter.GetIncludeProlog: Boolean;
begin
  Result := FIncludeProlog
end;

function TSAXCustomDocumentWriter.GetIndent: Boolean;
begin
  Result := FIndent
end;

function TSAXCustomDocumentWriter.GetStandalone: Boolean;
begin
  Result := FStandAlone
end;

function TSAXCustomDocumentWriter.GetVersion: string;
begin
  Result := FVersion
end;

procedure TSAXCustomDocumentWriter.SetEmptyShortForm(Value: Boolean);
begin
  FEmptyShortForm := Value
end;

procedure TSAXCustomDocumentWriter.SetExpandEntities(Value: Boolean);
begin
  FExpandEntities := Value
end;

procedure TSAXCustomDocumentWriter.SetIncludeProlog(Value: Boolean);
begin
  FIncludeProlog := Value
end;

procedure TSAXCustomDocumentWriter.SetIndent(Value: Boolean);
begin
  FIndent := Value
end;

procedure TSAXCustomDocumentWriter.SetStandalone(Value: Boolean);
begin
  FStandAlone := Value
end;

procedure TSAXCustomDocumentWriter.SetVersion(Value: string);
begin
  FVersion := Value
end;

function TSAXCustomDocumentWriter.GetStream: TStream;
begin
  Result := FStream
end;

end.
