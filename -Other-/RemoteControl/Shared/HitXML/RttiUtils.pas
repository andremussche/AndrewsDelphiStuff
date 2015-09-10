unit RttiUtils;

  {*************************************************************************}
  {                                                                         }
  {  This unit contains TRttiEnabled utils and is not used other modules    }
  {  of ÕmlHit library							    }
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
uses RttiClasses;

function CloneRttiEnabled(Origin: TRttiEnabled): TRttiEnabled;
procedure FreeRttiArray(Origin: TRttiEnabledArray);

implementation

uses Classes, SAX, SAX2Rtti, SysUtils, SAXCustomWriter, BSAX, SAXAdapters;

function CloneRttiEnabled(Origin: TRttiEnabled): TRttiEnabled;
var
  Writer: IXMLWriter;
  Stream: TStringStream;
  XMLReader : IXMLReader;
  XMLBufReader: IBufferedXMLReader;
  Vendor: TSAXVendor;
  ContentHandler: IContentHandler;
begin
  Stream := TStringStream.Create('');
  Writer := CreateWriter;
  Writer.Stream := Stream;
  Writer.IncludeProlog := True;
  Writer.Standalone := True;
  Writer.Indent := False;
  Writer.StartDocument;
  WriteRttiObject(Writer, Origin, 'clone');
  Writer.EndDocument;

  Result := TRttiEnabled(TRttiClass(Origin.ClassType).Create);
  Vendor:= GetSAXVendor;
  ContentHandler := RttiContentHandler(Result, nil);
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
  Stream.Free;
end;

procedure FreeRttiArray(Origin: TRttiEnabledArray);
var
  I: Integer;
begin
  for I := 0 to High(Origin) do
    if Assigned(Origin[I]) then
      FreeAndNil(Origin[I]);
end;

end.
