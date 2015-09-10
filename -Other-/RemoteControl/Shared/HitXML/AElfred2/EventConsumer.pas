// Filename : EventConsumer.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit EventConsumer;

interface

uses Classes, SAX;

type

  IEventConsumer = interface(IUnknown)
    ['{2129F823-B408-4B1F-A9EC-577EFE95946C}']
    function getContentHandler() : IContentHandler;
    function getDTDHandler() : IDTDHandler;
    function getProperty(const propertyId: SAXString): IProperty;
    procedure setErrorHandler(const handler : IErrorHandler);
  end;

implementation

end.
