// Filename : SAXAElfred2.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit SAXAElfred2;

interface

uses
  Classes,
  SAX,
  XmlReader,
  SAXDriver;

type

  // Streamlined Driver
  TSAXAelfred2Vendor = class(TSAXVendor)
    function Description: string; override;
    function XMLReader: IXMLReader; override;
  end;

  // Validation Support Reader
  TSAXAelfred2XmlReaderVendor = class(TSAXVendor)
    function Description: string; override;
    function XMLReader: IXMLReader; override;
  end;

  TSAXAElfred2 = class(TComponent)
  private
    function GetVendor: string;
  published
    property Vendor: string read GetVendor;
  end;

  TSAXAElfred2XMLReader = class(TComponent)
  private
    function GetVendor: string;
  published
    property Vendor: string read GetVendor;
  end;


  procedure FormatDocument(const Input, Output : TStream;
    const Canonical : Boolean = False);

  procedure InitializeVendor;

const

  SAX_VENDOR_AELFRED2 = SAXString('AElfred2');
  SAX_VENDOR_AELFRED2_XMLREADER = SAXString('AElfred2 XMLReader');

implementation

uses SAXWriter;

  procedure FormatDocument(const Input, Output : TStream;
    const Canonical : Boolean = False);
  var AXMLReader : IXMLReader;
      ASAXVendor : TSAXVendor;
      ASAXWriter : TSAXWriter;
  begin
    Input.Seek(0, 0);
    ASAXVendor:= GetSAXVendor(SAX_VENDOR_AELFRED2);
    if not Assigned(ASAXVendor) then
      raise ESAXException.CreateFmt(SUnknownVendor, [SAX_VENDOR_AELFRED2]);
    AXMLReader:= ASAXVendor.XMLReader as IXMLReader;
    ASAXWriter:= TSAXWriter.Create(Canonical);
    ASAXWriter.setOutput(Output);
    ASAXWriter.setEncoding('UTF-8');
    ASAXWriter.setTrimOutput(true);
    AXMLReader.setContentHandler(ASAXWriter as IContentHandler);
    AXMLReader.parse(TStreamInputSource.Create(Input, soReference) as IStreamInputSource);
    Output.Seek(0, 0);
  end;


{ TSAXAelfred2Vendor }

function TSAXAelfred2Vendor.Description: string;
begin
  Result := SAX_VENDOR_AELFRED2;
end;

function TSAXAelfred2Vendor.XMLReader: IXMLReader;
begin
  Result := TSAXDriver.Create(nil) as IXMLReader;
end;

{ TSAXAelfred2XmlReaderVendor }

function TSAXAelfred2XmlReaderVendor.Description: string;
begin
  Result := SAX_VENDOR_AELFRED2_XMLREADER;
end;

function TSAXAelfred2XmlReaderVendor.XMLReader: IXMLReader;
begin
  Result := TXmlReader.Create(nil);
end;

var
  SAXVendor: TSAXVendor;
  SAXXmlReaderVendor: TSAXVendor;

function TSAXAElfred2.GetVendor: string;
begin
  Result := SAXVendor.Description;
end;

function TSAXAElfred2XMLReader.GetVendor: string;
begin
  Result := SAXXmlReaderVendor.Description;
end;

procedure InitializeVendor;
begin
  SAXVendor:= TSAXAelfred2Vendor.Create;
  RegisterSAXVendor(SAXVendor);
  SAXXmlReaderVendor:= TSAXAelfred2XmlReaderVendor.Create;
  RegisterSAXVendor(SAXXmlReaderVendor);
  DefaultSAXVendor:= SAXXmlReaderVendor.Description;
end;

initialization
  InitializeVendor;

finalization

  UnRegisterSAXVendor(SAXVendor);
  UnRegisterSAXVendor(SAXXmlReaderVendor);

end.
