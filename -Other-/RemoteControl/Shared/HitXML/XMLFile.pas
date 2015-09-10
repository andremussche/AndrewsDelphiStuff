unit XMLFile;

  {*************************************************************************}
  {                                                                         }
  {  This unit contains procedure and functions to save\load                }
  {  data from xml file.					                                          }
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
uses RttiClasses, SAXAElfred2;

procedure SaveToFile( FileName: string; RootObj: TRttiEnabled; ClassResolver: TRttiClassResolver = nil);
function SaveToString(RootObj: TRttiEnabled; ClassResolver: TRttiClassResolver = nil): string;
procedure LoadFromFile( FileName: string; RootObj: TRttiEnabled; MarshalableFactory: TRttiClassFactory = nil);
procedure LoadFromString(Data: string; RootObj: TRttiEnabled; MarshalableFactory: TRttiClassFactory = nil);
function CreateFromString(Data: string; Factory: TRttiClassFactory): TRttiEnabled;

implementation
uses SAX, SAX2Rtti, Classes, SysUtils, Dialogs, SAXAdapters, SAXCustomWriter, BSAX;

type
  TFastStream = class(TStream)
  private
    FDataString: string;
    FPosition: Integer;
    FSize: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
    function GetDataString: string;
  public
    constructor Create(const AString: string);
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(Count: Longint): string;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const AString: string);
    property DataString: string read GetDataString;
  end;

function RootName(RootObj: TRttiEnabled): string;
begin
  Result := RootObj.ClassName;
  Assert(Length(Result)> 0);
  if Result[1] = 'T' then
    Delete(Result, 1, 1);
  Result := CamelToXml(Result, False);
end;


function SaveToString(RootObj: TRttiEnabled; ClassResolver: TRttiClassResolver = nil): string;
var
  Writer: IXMLWriter;
//  Stream: TFastStream;
  Stream: TStringStream;
begin
//  Stream := TFastStream.Create('');
  Stream := TStringStream.Create('');
  try
    Writer := CreateWriter;
    Writer.Stream := Stream;
    Writer.IncludeProlog := True;
    Writer.Standalone := True;
    Writer.Indent := True;
    Writer.StartDocument;
    WriteRttiObject(Writer, RootObj, RootName(RootObj), False, ClassResolver);
    Writer.EndDocument;

    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure SaveToFile( FileName: string; RootObj: TRttiEnabled; ClassResolver: TRttiClassResolver = nil);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Text := SaveToString(RootObj, ClassResolver);
    if not DirectoryExists(ExtractFileDir(FileName)) then
      CreateDir(ExtractFileDir(FileName));
    Strings.SaveToFile(FileName);
  finally
    Strings.Free;
  end;
end;


procedure DoLoadFromString(Data: string; var RootObj: TRttiEnabled; Factory: TRttiClassFactory);
var
  XMLReader : IXMLReader;
  XMLBufReader: IBufferedXMLReader;
  Vendor: TSAXVendor;
  ContentHandler: IContentHandler;
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Data);
  try
    Vendor:= GetSAXVendor;
    ContentHandler := RttiContentHandler(RootObj, Factory);
    if Vendor is TBufferedSAXVendor then
    begin
      XMLBufReader:= TBufferedSAXVendor(Vendor).BufferedXMLReader;
      XMLBufReader.setContentHandler(Adapt(ContentHandler, XMLBufReader));
      XMLBufReader.setErrorHandler(RttiErrorHandler);
      XMLBufReader.parse(TStreamInputSource.Create(Stream, soReference) as IInputSource);
      XMLBufReader:= nil;
    end else
    begin
      XMLReader:= Vendor.XMLReader;
      XMLReader.setContentHandler(ContentHandler);
      XMLReader.setErrorHandler(RttiErrorHandler);
      XMLReader.parse(TStreamInputSource.Create(Stream, soReference) as IInputSource);
      XMLReader:= nil;
    end;
  finally
    Stream.Free;
  end;
end;

procedure LoadFromString(Data: string; RootObj: TRttiEnabled; MarshalableFactory: TRttiClassFactory = nil);
begin
  DoLoadFromString(Data, RootObj, MarshalableFactory);
end;

function CreateFromString(Data: string; Factory: TRttiClassFactory): TRttiEnabled;
begin
  Result := nil;
  DoLoadFromString(Data, Result, Factory);
end;

procedure LoadFromFile( FileName: string; RootObj: TRttiEnabled; MarshalableFactory: TRttiClassFactory = nil);
var
  XMLReader : IXMLReader;
  XMLBufReader: IBufferedXMLReader;
  Vendor: TSAXVendor;
  ContentHandler: IContentHandler;
  sMsg: string;
begin
  try
    Vendor:= GetSAXVendor;
    ContentHandler := RttiContentHandler(RootObj, MarshalableFactory);
    if Vendor is TBufferedSAXVendor then
    begin
      XMLBufReader:= TBufferedSAXVendor(Vendor).BufferedXMLReader;
      XMLBufReader.setContentHandler(Adapt(ContentHandler, XMLBufReader));
      XMLBufReader.setErrorHandler(RttiErrorHandler);
      XMLBufReader.parse(FileName);
      XMLBufReader:= nil;
    end else
    begin
      XMLReader:= Vendor.XMLReader;
      XMLReader.setContentHandler(ContentHandler);
      XMLReader.setErrorHandler(RttiErrorHandler);
      XMLReader.parse(FileName);
      XMLReader:= nil;
    end;
  except on E: Exception do
    if FileExists(FileName) then
    begin
      sMsg := Format('Config file %s is corrupted. '^M^J'Default settings will be used'^M^J'Error: %s', [FileName, E.Message]);
      TThread.Queue(nil,
        procedure
        begin
          ShowMessage(sMsg);
        end);
    end;
  end;
end;

{ TFastStream }

constructor TFastStream.Create(const AString: string);
begin
  inherited Create;
  FDataString := AString;
end;

function TFastStream.GetDataString: string;
begin
  SetLength(FDataString, FSize);
  Result := FDataString;
end;

function TFastStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FSize - FPosition;
  if Result > Count then Result := Count;
  Move(PChar(@FDataString[FPosition + 1])^, Buffer, Result);
  Inc(FPosition, Result);
end;

function TFastStream.ReadString(Count: Integer): string;
var
  Len: Integer;
begin
  Len := FSize - FPosition;
  if Len > Count then Len := Count;
  SetString(Result, PChar(@FDataString[FPosition + 1]), Len);
  Inc(FPosition, Len);
end;

function TFastStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := FSize - Offset;
  end;
  if FPosition > FSize then
    FPosition := FSize
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

const
  MemoryDelta = $2000; { Must be a power of 2 }

procedure TFastStream.SetSize(NewSize: Integer);
var
  Capacity: Integer;
begin
  FSize := NewSize;
  if Length(FDataString) < FSize then
  begin
    Capacity := (FSize + (MemoryDelta - 1)) and not (MemoryDelta - 1);
    SetLength(FDataString, Capacity);
  end;
  if FPosition > NewSize then FPosition := NewSize;
end;

function TFastStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := Count;
  if Count = 0 then
    Exit;
  SetSize(FPosition + Result);
  Move(Buffer, PChar(@FDataString[FPosition + 1])^, Result);
  Inc(FPosition, Result);
end;

procedure TFastStream.WriteString(const AString: string);
begin
  Write(PChar(AString)^, Length(AString));
end;

end.
