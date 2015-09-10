// SAX for Pascal, Simple API for XML Components in Pascal.
// Ver 1.1 July 4, 2003
// http://xml.defined.net/SAX (this will change!)
// Based on http://www.saxproject.org/
// No warranty; no copyright -- use this as you will.
unit SAXComps;

interface

{$I SAX.inc}

uses
{$IFDEF DELPHI6_UP}
  Variants,
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, SAX, SAXExt, SAXHelpers;

type
  { Forward declarations }
  TSAXInterfacedComponent = class;

  TSAXCustomContentHandler = class;
  TSAXCustomDeclHandler = class;
  TSAXCustomDTDHandler = class;
  TSAXCustomEntityResolver = class;
  TSAXCustomErrorHandler = class;
  TSAXCustomLexicalHandler = class;
  TSAXCustomDocumentWriter = class;

  TSAXCustomFilter = class;

  { Define local types to handle D5- vs D6+ differences }
{$IFDEF DELPHI6_UP}
  TContentHandler = IContentHandler;
  TDeclHandler = IDeclHandler;
  TDTDHandler = IDTDHandler;
  TEntityResolver = IEntityResolver;
  TErrorHandler = IErrorHandler;
  TLexicalHandler = ILexicalHandler;
{$ELSE}
  TContentHandler = TSAXCustomContentHandler;
  TDeclHandler = TSAXCustomDeclHandler;
  TDTDHandler = TSAXCustomDTDHandler;
  TEntityResolver = TSAXCustomEntityResolver;
  TErrorHandler = TSAXCustomErrorHandler;
  TLexicalHandler = TSAXCustomLexicalHandler;
{$ENDIF}

  { A SAX Interfaced component operates as a standard component when
    created with an Owner that is a TComponent. Otherwise it works as
    a ref'counted interface and its lifetime will be managed by the number
    of references to it. }
  TSAXInterfacedComponent = class(TComponent, IUnknown)
  protected
    FRefCount : Integer;
    FOwnerIsComponent : Boolean;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  { The standard features available to SAX XML readers. }
  TSAXOption = (soNamespaces, soNamespacePrefix, soValidate,
    soExtGeneralEntities, soExtParamEntities, soIsStandalone,
    soUseAttributes2, soUseLocator2, soUseEntityResolver2,
    soResolveDTDURIs, soLexParamEntities);

  TSAXOptions = set of TSAXOption;

  TInvalidFeatureEvent = procedure (Sender: TObject; const Name: string;
    Value: Boolean; const Error: ESAXException) of object;
  TInvalidPropertyEvent = procedure (Sender: TObject; const Name: string;
    const Value: Variant; const Error: ESAXException) of object;

  { A SAX XML reader wrapped in a component.
    This base component defines all the functionality but leaves it protected.
    To use this, you drop it on a form, add components for various handlers,
    and attach them to this reader. Then in code, call the parse method to
    start the process. Parse has several forms, allowing you to work with
    a file (Filename property), a string (XML property), or an input source
    or system id (overloaded versions). }
  TSAXCustomXMLReader = class(TSAXInterfacedComponent, IXMLReader)
  private
    FContentHandler: TContentHandler;
    FDeclHandler: TDeclHandler;
    FDTDHandler: TDTDHandler;
    FEntityResolver: TEntityResolver;
    FErrorHandler: TErrorHandler;
    FLexicalHandler: TLexicalHandler;
    FOnInvalidFeature: TInvalidFeatureEvent;
    FOnInvalidProperty: TInvalidPropertyEvent;
    FURL: string;
    FVendor: string;
    FXML: TStrings;
    FXMLReader: IXMLReader;
    function GetFeature(const Name: SAXString) : Boolean;
    function GetOptions: TSAXOptions;
    function GetProperty(const Name: SAXString) : IProperty;
    { IXMLReader }
    function GetContentHandlerP: TContentHandler;
    function GetDeclHandlerP: TDeclHandler;
    function GetDTDHandlerP: TDTDHandler;
    function GetEntityResolverP: TEntityResolver;
    function GetErrorHandlerP: TErrorHandler;
    function GetLexicalHandlerP: TLexicalHandler;
    procedure SetContentHandlerP(const Value: TContentHandler);
    procedure SetDeclHandlerP(const Value: TDeclHandler);
    procedure SetDTDHandlerP(const Value: TDTDHandler);
    procedure SetEntityResolverP(const Value: TEntityResolver);
    procedure SetErrorHandlerP(const Value: TErrorHandler);
    procedure SetLexicalHandlerP(const Value: TLexicalHandler);
    procedure SetFeature(const Name: SAXString; Value: Boolean);
    procedure SetOptions(const Value: TSAXOptions);
    procedure SetURL(const Value: string);
    procedure SetVendor(const Value: string);
    procedure SetXML(const Value: TStrings);
  protected
    property Vendor: string read FVendor write SetVendor;
    property ContentHandler: TContentHandler
      read GetContentHandlerP write SetContentHandlerP;
    property DeclHandler: TDeclHandler
      read GetDeclHandlerP write SetDeclHandlerP;
    property DTDHandler: TDTDHandler
      read GetDTDHandlerP write SetDTDHandlerP;
    property EntityResolver: TEntityResolver
      read GetEntityResolverP write SetEntityResolverP;
    property ErrorHandler: TErrorHandler
      read GetErrorHandlerP write SetErrorHandlerP;
    property LexicalHandler: TLexicalHandler
      read GetLexicalHandlerP write SetLexicalHandlerP;
    property Features[const Name: SAXString]: Boolean
      read GetFeature write SetFeature;
    property OnInvalidFeature: TInvalidFeatureEvent
      read FOnInvalidFeature write FOnInvalidFeature;
    property OnInvalidProperty: TInvalidPropertyEvent
      read FOnInvalidProperty write FOnInvalidProperty;
    property Options: TSAXOptions read GetOptions write SetOptions;
    property URL: string read FURL write SetURL;
    property XML: TStrings read FXML write SetXML;
    procedure DoInvalidFeature(Name: string; Value: Boolean;
      Error: ESAXException);
    procedure DoInvalidProperty(Name: string; Value : IProperty;
      Error: ESAXException);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure ParseSetup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Parse; overload;
    procedure Parse(const Input: IInputSource); overload;
    procedure Parse(const SystemId: SAXString); overload;
    function GetContentHandler: IContentHandler;
    function GetDTDHandler: IDTDHandler;
    function GetEntityResolver: IEntityResolver;
    function GetErrorHandler: IErrorHandler;
    procedure SetContentHandler(const Value: IContentHandler);
    procedure SetDeclHandler(const Value: IDeclHandler);
    procedure SetDTDHandler(const Value: IDTDHandler);
    procedure SetEntityResolver(const Value: IEntityResolver);
    procedure SetErrorHandler(const Value: IErrorHandler);
    procedure SetLexicalHandler(const Value: ILexicalHandler);
  end;

  { Actual XML reader component with published properties. }
  TSAXXMLReader = class(TSAXCustomXMLReader)
  public
    property Features;
  published
    property Vendor;
    property ContentHandler;
    property DeclHandler;
    property DTDHandler;
    property EntityResolver;
    property ErrorHandler;
    property LexicalHandler;
    property OnInvalidFeature;
    property OnInvalidProperty;
    property Options;
    property URL;
    property XML;
  end;

  TDocumentLocatorEvent = procedure (Sender: TObject; const Locator: ILocator)
    of object;
  TStartPrefixMappingEvent = procedure (Sender: TObject;
    const Prefix, URI: SAXString) of object;
  TEndPrefixMappingEvent = procedure (Sender: TObject; const Prefix: SAXString)
    of object;
  TStartElementEvent = procedure (Sender: TObject; const NamespaceURI,
    LocalName, QName: SAXString; const Atts: IAttributes) of object;
  TEndElementEvent = procedure (Sender: TObject; const NamespaceURI,
    LocalName, QName: SAXString) of object;
  TCharactersEvent = procedure (Sender: TObject; const PCh: SAXString) of object;
  TProcessingInstructionEvent = procedure (Sender: TObject;
    const Target, Data: SAXString) of object;
  TSkippedEntityEvent = procedure (Sender: TObject; const Name: SAXString)
    of object;

  { A SAX content handler wrapped in a component.
    This base component defines all the functionality but leaves it protected.
    Just add code to the events, attach this component to an XML reader,
    and start the parse process. }
  TSAXCustomContentHandler = class(TSAXInterfacedComponent, IContentHandler)
  private
    FOnCharacters: TCharactersEvent;
    FOnIgnorableWhitespace: TCharactersEvent;
    FOnSetDocumentLocator: TDocumentLocatorEvent;
    FOnEndElement: TEndElementEVent;
    FOnEndPrefixMapping: TEndPrefixMappingEvent;
    FOnStartDocument: TNotifyEvent;
    FOnEndDocument: TNotifyEvent;
    FOnProcessingInstruction: TProcessingInstructionEvent;
    FOnSkippedEntity: TSkippedEntityEvent;
    FOnStartElement: TStartElementEvent;
    FOnStartPrefixMapping: TStartPrefixMappingEvent;
  protected
    property OnSetDocumentLocator: TDocumentLocatorEvent
      read FOnSetDocumentLocator write FOnSetDocumentLocator;
    property OnStartDocument: TNotifyEvent
      read FOnStartDocument write FOnStartDocument;
    property OnEndDocument: TNotifyEvent
      read FOnEndDocument write FOnEndDocument;
    property OnStartPrefixMapping: TStartPrefixMappingEvent
      read FOnStartPrefixMapping write FOnStartPrefixMapping;
    property OnEndPrefixMapping: TEndPrefixMappingEvent
      read FOnEndPrefixMapping write FOnEndPrefixMapping;
    property OnStartElement: TStartElementEvent
      read FOnStartElement write FOnStartElement;
    property OnEndElement: TEndElementEvent
      read FOnEndElement write FOnEndElement;
    property OnCharacters: TCharactersEvent
      read FOnCharacters write FOnCharacters;
    property OnIgnorableWhitespace: TCharactersEvent
      read FOnIgnorableWhitespace write FOnIgnorableWhitespace;
    property OnProcessingInstruction: TProcessingInstructionEvent
      read FOnProcessingInstruction write FOnProcessingInstruction;
    property OnSkippedEntity: TSkippedEntityEvent
      read FOnSkippedEntity write FOnSkippedEntity;
  public
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
  end;

  { Actual content handler component with published properties. }
  TSAXContentHandler = class(TSAXCustomContentHandler)
  published
    property OnSetDocumentLocator;
    property OnStartDocument;
    property OnEndDocument;
    property OnStartPrefixMapping;
    property OnEndPrefixMapping;
    property OnStartElement;
    property OnEndElement;
    property OnCharacters;
    property OnIgnorableWhitespace;
    property OnProcessingInstruction;
    property OnSkippedEntity;
  end;

  { Adapter content handler component for document writer. }
  TSAXContentWriter = class(TSAXCustomContentHandler)
  private
    FDocumentWriter: TSAXCustomDocumentWriter;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { IContentHandler }
    procedure SetDocumentLocator(const Locator: ILocator); override;
    procedure StartDocument; override;
    procedure EndDocument; override;
    procedure StartPrefixMapping(const Prefix, URI: SAXString); override;
    procedure EndPrefixMapping(const Prefix: SAXString); override;
    procedure StartElement(const NamespaceURI, LocalName, QName: SAXString;
      const Atts: IAttributes); override;
    procedure EndElement(const NamespaceURI, LocalName, QName: SAXString);
      override;
    procedure Characters(const PCh: SAXString); override;
    procedure IgnorableWhitespace(const PCh: SAXString);
      override;
    procedure ProcessingInstruction(const Target, Data: SAXString); override;
    procedure SkippedEntity(const Name: SAXString); override;
  published
    property DocumentWriter: TSAXCustomDocumentWriter
      read FDocumentWriter write FDocumentWriter;
  end;

  TAttributeDeclEvent = procedure (Sender: TObject;
    const EName, AName, AttrType, ValueDefault, Value: SAXString) of object;
  TElementDeclEvent = procedure (Sender: TObject; const Name, Model: SAXString)
    of object;
  TExternalEntityDeclEvent = procedure (Sender: TObject;
    const Name, PublicId, SystemId: SAXString) of object;
  TInternalEntityDeclEvent = procedure (Sender: TObject;
    const Name, Value: SAXString) of object;

  { A SAX declaration handler wrapped in a component.
    This base component defines all the functionality but leaves it protected.
    Just add code to the events, attach this component to an XML reader,
    and start the parse process. }
  TSAXCustomDeclHandler = class(TSAXInterfacedComponent, IDeclHandler)
  private
    FOnAttributeDecl: TAttributeDeclEvent;
    FOnElementDecl: TElementDeclEvent;
    FOnExternalEntityDecl: TExternalEntityDeclEvent;
    FOnInternalEntityDecl: TInternalEntityDeclEvent;
  protected
    property OnAttributeDecl: TAttributeDeclEvent
      read FOnAttributeDecl write FOnAttributeDecl;
    property OnElementDecl: TElementDeclEvent
      read FOnElementDecl write FOnElementDecl;
    property OnExternalEntityDecl: TExternalEntityDeclEvent
      read FOnExternalEntityDecl write FOnExternalEntityDecl;
    property OnInternalEntityDecl: TInternalEntityDeclEvent
      read FOnInternalEntityDecl write FOnInternalEntityDecl;
  public
    { IDeclHandler }
    procedure AttributeDecl(
      const EName, AName, AttrType, ValueDefault, Value: SAXString); virtual;
    procedure ElementDecl(const Name, Model: SAXString); virtual;
    procedure ExternalEntityDecl(const Name, PublicId, SystemId: SAXString);
      virtual;
    procedure InternalEntityDecl(const Name, Value: SAXString); virtual;
  end;

  { Actual declaration handler component with published properties. }
  TSAXDeclHandler = class(TSAXCustomDeclHandler)
  published
    property OnAttributeDecl;
    property OnElementDecl;
    property OnExternalEntityDecl;
    property OnInternalEntityDecl;
  end;

  { Adapter declaration handler component for document writer. }
  TSAXDeclWriter = class(TSAXCustomDeclHandler)
  private
    FDocumentWriter: TSAXCustomDocumentWriter;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { IDeclHandler }
    procedure AttributeDecl(
      const EName, AName, AttrType, ValueDefault, Value: SAXString); override;
    procedure ElementDecl(const Name, Model: SAXString); override;
    procedure ExternalEntityDecl(const Name, PublicId, SystemId: SAXString);
      override;
    procedure InternalEntityDecl(const Name, Value: SAXString); override;
  published
    property DocumentWriter: TSAXCustomDocumentWriter
      read FDocumentWriter write FDocumentWriter;
  end;

  TNotationDeclEvent = procedure (Sender: TObject;
    const Name, PublicId, SystemId: SAXString) of object;
  TUnparsedEntityDeclEvent = procedure (Sender: TObject;
    const Name, PublicId, SystemId, NotationName: SAXString) of object;

  { A SAX DTD handler wrapped in a component.
    This base component defines all the functionality but leaves it protected.
    Just add code to the events, attach this component to an XML reader,
    and start the parse process. }
  TSAXCustomDTDHandler = class(TSAXInterfacedComponent, IDTDHandler)
  private
    FOnNotationDecl: TNotationDeclEvent;
    FOnUnparsedEntityDecl: TUnparsedEntityDeclEvent;
  protected
    property OnNotationDecl: TNotationDeclEvent
      read FOnNotationDecl write FOnNotationDecl;
    property OnUnparsedEntityDecl: TUnparsedEntityDeclEvent
      read FOnUnparsedEntityDecl write FOnUnparsedEntityDecl;
  public
    { IDTDHandler }
    procedure NotationDecl(const Name, PublicId, SystemId: SAXString); virtual;
    procedure UnparsedEntityDecl(const Name, PublicId, SystemId,
      NotationName: SAXString); virtual;
  end;

  { Actual DTD handler component with published properties. }
  TSAXDTDHandler = class(TSAXCustomDTDHandler)
  published
    property OnNotationDecl;
    property OnUnparsedEntityDecl;
  end;

  { Adapter DTD handler component for document writer. }
  TSAXDTDWriter = class(TSAXCustomDTDHandler)
  private
    FDocumentWriter: TSAXCustomDocumentWriter;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { IDTDHandler }
    procedure NotationDecl(const Name, PublicId, SystemId: SAXString); override;
    procedure UnparsedEntityDecl(const Name, PublicId, SystemId,
      NotationName: SAXString); override;
  published
    property DocumentWriter: TSAXCustomDocumentWriter
      read FDocumentWriter write FDocumentWriter;
  end;

  TResolveEntityEvent = function (Sender: TObject;
    const PublicId, SystemId: SAXString): IInputSource of object;

  { A SAX entity resolver wrapped in a component.
    This base component defines all the functionality but leaves it protected.
    Just add code to the events, attach this component to an XML reader,
    and start the parse process. }
  TSAXCustomEntityResolver = class(TSAXInterfacedComponent, IEntityResolver)
  private
    FOnResolveEntity: TResolveEntityEvent;
  protected
    property OnResolveEntity: TResolveEntityEvent
      read FOnResolveEntity write FOnResolveEntity;
  public
    { IEntityResolver }
    function ResolveEntity(const PublicId, SystemId: SAXString): IInputSource;
      virtual;
  end;

  { Actual entity resolver component with published properties. }
  TSAXEntityResolver = class(TSAXCustomEntityResolver)
  published
    property OnResolveEntity;
  end;

  TErrorEvent = procedure (Sender: TObject; const Error: ISAXParseError)
    of object;

  { A SAX error handler wrapped in a component.
    This base component defines all the functionality but leaves it protected.
    Just add code to the events, attach this component to an XML reader,
    and start the parse process. }
  TSAXCustomErrorHandler = class(TSAXInterfacedComponent, IErrorHandler)
  private
    FOnError: TErrorEvent;
    FOnFatalError: TErrorEvent;
    FOnWarning: TErrorEvent;
  protected
    property OnWarning: TErrorEvent read FOnWarning write FOnWarning;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnFatalError: TErrorEvent read FOnFatalError write FOnFatalError;
  public
    { IErrorHandler }
    procedure Warning(const Error: ISAXParseError); virtual;
    procedure Error(const Error: ISAXParseError); virtual;
    procedure FatalError(const Error: ISAXParseError); virtual;
  end;

  { Actual error handler component with published properties. }
  TSAXErrorHandler = class(TSAXCustomErrorHandler)
  published
    property OnWarning;
    property OnError;
    property OnFatalError;
  end;

  TCommentEvent = procedure (Sender: TObject; const PCh: SAXString) of object;
  TEndEntityEvent = procedure (Sender: TObject; const Name: SAXString) of object;
  TStartDTDEvent = procedure (Sender: TObject;
    const Name, PublicId, SystemId: SAXString) of object;
  TStartEntityEvent = procedure (Sender: TObject; const Name: SAXString)
    of object;

  { A SAX lexical handler wrapped in a component.
    This base component defines all the functionality but leaves it protected.
    Just add code to the events, attach this component to an XML reader,
    and start the parse process. }
  TSAXCustomLexicalHandler = class(TSAXInterfacedComponent, ILexicalHandler)
  private
    FOnComment: TCommentEvent;
    FOnEndEntity: TEndEntityEvent;
    FOnStartCData: TNotifyEvent;
    FOnEndCData: TNotifyEvent;
    FOnEndDTD: TNotifyEvent;
    FOnStartDTD: TStartDTDEvent;
    FOnStartEntity: TStartEntityEvent;
  protected
    property OnComment: TCommentEvent read FOnComment write FOnComment;
    property OnEndCData: TNotifyEvent read FOnEndCData write FOnEndCData;
    property OnEndDTD: TNotifyEvent read FOnEndDTD write FOnEndDTD;
    property OnEndEntity: TEndEntityEvent read FOnEndEntity write FOnEndEntity;
    property OnStartCData: TNotifyEvent read FOnStartCData write FOnStartCData;
    property OnStartDTD: TStartDTDEvent read FOnStartDTD write FOnStartDTD;
    property OnStartEntity: TStartEntityEvent
      read FOnStartEntity write FOnStartEntity;
  public
    { ILexicalHandler }
    procedure Comment(const PCh: SAXString); virtual;
    procedure EndCData; virtual;
    procedure EndDTD; virtual;
    procedure EndEntity(const Name: SAXString); virtual;
    procedure StartCData; virtual;
    procedure StartDTD(const Name, PublicId, SystemId: SAXString); virtual;
    procedure StartEntity(const Name: SAXString); virtual;
  end;

  { Actual lexical handler component with published properties. }
  TSAXLexicalHandler = class(TSAXCustomLexicalHandler)
  published
    property OnComment;
    property OnEndCData;
    property OnEndDTD;
    property OnEndEntity;
    property OnStartCData;
    property OnStartDTD;
    property OnStartEntity;
  end;

  { Adapter lexical handler component for document writer. }
  TSAXLexicalWriter = class(TSAXCustomLexicalHandler)
  private
    FDocumentWriter: TSAXCustomDocumentWriter;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    { ILexicalHandler }
    procedure Comment(const PCh: SAXString); override;
    procedure EndCData; override;
    procedure EndDTD; override;
    procedure EndEntity(const Name: SAXString); override;
    procedure StartCData; override;
    procedure StartDTD(const Name, PublicId, SystemId: SAXString); override;
    procedure StartEntity(const Name: SAXString); override;
  published
    property DocumentWriter: TSAXCustomDocumentWriter
      read FDocumentWriter write FDocumentWriter;
  end;

  { A SAX content handler, declaration handler, DTD handler,
    and lexical handler wrapped in a single component.
    It generates an XML document based on the events that it receives.
    This base component defines all the functionality but leaves it protected.
    Just add code to the events, attach this component to an XML reader
    in each of the handler categories, and start the parse process.
    Under Delphi 4 and 5 you need to go through the various adapter components -
    TSAXContentWriter, TSAXDeclWriter, TSAXDTDWriter, TSAXLexicalWriter.
    Or use it separately to generate a new document.
    This component owns the stream that it writes to. }
  TSAXCustomDocumentWriter = class(TSAXInterfacedComponent, IContentHandler, IDeclHandler,
    IDTDHandler, ILexicalHandler)
  private
    FCurIndent: Integer;        // Current indent level in characters
    FElements: TStringList;     // List of elements currently open
    FEmptyShortForm: Boolean;   // True to use the short form for empty elements,
                                // false to write them out in full
    FExpandEntities: Boolean;   // True to write out entity contents rather than
                                // the reference, false to always write the reference
    FFilename: TFilename;       // The name of the output file, or '' if in memory
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
    procedure SetFilename(const Value: TFilename);
    procedure SetStream(const Value: TStream);
  protected
    property EmptyShortForm: Boolean read FEmptyShortForm write FEmptyShortForm
      default True;
    property ExpandEntities: Boolean read FExpandEntities write FExpandEntities
      default False;
    property Filename: TFilename read FFilename write SetFilename;
    property IncludeProlog: Boolean read FIncludeProlog write FIncludeProlog
      default True;
    property Indent: Boolean read FIndent write FIndent default False;
    property Standalone: Boolean read FStandAlone write FStandAlone;
    property Version: string read FVersion write FVersion;
    function GetPublicSystem(const PublicId, SystemId: SAXString): SAXString;
    function MakeIndent(Level: Integer): string; virtual;
    procedure Write(const Text: SAXString); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Stream: TStream read FStream write SetStream;
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
  end;

  { Actual document writer component with published properties. }
  TSAXDocumentWriter = class(TSAXCustomDocumentWriter)
  published
    property EmptyShortForm;
    property ExpandEntities;
    property Filename;
    property IncludeProlog;
    property Indent;
    property Standalone;
    property Version;
  end;


  TFilteredNotifyEvent = procedure (Sender: TObject;
    var Filtered : Boolean) of object;

  { IEntityResolver }

  TFilteredResolveEntityEvent = function (Sender : TObject; var publicId,
    systemId : SAXString; var Filtered : Boolean) : IInputSource of object;

  { IDTDHandler }

  TFilteredNotationDeclEvent = procedure (Sender: TObject;
    var Name, PublicId, SystemId: SAXString;
    var Filtered : Boolean) of object;
  TFilteredUnparsedEntityDeclEvent = procedure (Sender: TObject;
    var Name, PublicId, SystemId, NotationName: SAXString;
    var Filtered : Boolean) of object;

  { IContentHandler }

  TFilteredDocumentLocatorEvent = procedure (Sender: TObject;
    var Locator: ILocator; var Filtered : Boolean) of object;
  TFilteredStartPrefixMappingEvent = procedure (Sender: TObject;
    var Prefix, URI: SAXString; var Filtered : Boolean) of object;
  TFilteredEndPrefixMappingEvent = procedure (Sender: TObject;
    var Prefix: SAXString; var Filtered : Boolean) of object;
  TFilteredStartElementEvent = procedure (Sender: TObject; var NamespaceURI,
    LocalName, QName: SAXString; var Atts: IAttributes;
    var Filtered : Boolean) of object;
  TFilteredEndElementEvent = procedure (Sender: TObject; var NamespaceURI,
    LocalName, QName: SAXString; var Filtered : Boolean) of object;
  TFilteredCharactersEvent = procedure (Sender: TObject;
    var PCh: SAXString; var Filtered : Boolean) of object;
  TFilteredProcessingInstructionEvent = procedure (Sender: TObject;
    var Target, Data: SAXString; var Filtered : Boolean) of object;
  TFilteredSkippedEntityEvent = procedure (Sender: TObject;
    var Name: SAXString; var Filtered : Boolean) of object;

  { IErrorHandler }

  TFilteredErrorEvent = procedure (Sender: TObject; var Error: ISAXParseError;
    var Filtered : Boolean) of object;

  { ILexicalHandler }

  TFilteredCommentEvent = procedure (Sender: TObject; var Ch: SAXString;
    var Filtered : Boolean) of object;
  TFilteredEndEntityEvent = procedure (Sender: TObject; var Name: SAXString;
    var Filtered : Boolean) of object;
  TFilteredStartDTDEvent = procedure (Sender: TObject; var Name, PublicId,
    SystemId: SAXString; var Filtered : Boolean) of object;
  TFilteredStartEntityEvent = procedure (Sender: TObject; var Name: SAXString;
    var Filtered : Boolean) of object;

  { IDeclHandler }

  TFilteredAttributeDeclEvent = procedure (Sender: TObject;
    var EName, AName, AttrType, ValueDefault, Value: SAXString;
    var Filtered : Boolean) of object;
  TFilteredElementDeclEvent = procedure (Sender: TObject; var Name,
    Model: SAXString; var Filtered : Boolean) of object;
  TFilteredExternalEntityDeclEvent = procedure (Sender: TObject;
    var Name, PublicId, SystemId: SAXString; var Filtered : Boolean) of object;
  TFilteredInternalEntityDeclEvent = procedure (Sender: TObject;
    var Name, Value: SAXString; var Filtered : Boolean) of object;

  // Fires events
  // If filtered then event ends
  // If not filtered then event passes through
  TSAXCustomFilter = class(TSAXInterfacedComponent,
    IEntityResolver, IDTDHandler, IContentHandler, IErrorHandler,
    ILexicalHandler, IDeclHandler)
  private
    FOnIgnorableWhitespace: TFilteredCharactersEvent;
    FOnCharacters: TFilteredCharactersEvent;
    FOnEndElement: TFilteredEndElementEvent;
    FOnEndPrefixMapping: TFilteredEndPrefixMappingEvent;
    FOnWarning: TFilteredErrorEvent;
    FOnError: TFilteredErrorEvent;
    FOnFatalError: TFilteredErrorEvent;
    FOnNotationDecl: TFilteredNotationDeclEvent;
    FOnProcessingInstruction: TFilteredProcessingInstructionEvent;
    FOnResolveEntity: TFilteredResolveEntityEvent;
    FOnSkippedEntity: TFilteredSkippedEntityEvent;
    FOnStartElement: TFilteredStartElementEvent;
    FOnStartPrefixMapping: TFilteredStartPrefixMappingEvent;
    FOnUnparsedEntityDecl: TFilteredUnparsedEntityDeclEvent;
    FOnSetDocumentLocator: TFilteredDocumentLocatorEvent;
    FOnEndDocument: TFilteredNotifyEvent;
    FOnStartDocument: TFilteredNotifyEvent;
    FOnAttributeDecl: TFilteredAttributeDeclEvent;
    FOnComment: TFilteredCommentEvent;
    FOnElementDecl: TFilteredElementDeclEvent;
    FOnEndEntity: TFilteredEndEntityEvent;
    FOnExternalEntityDecl: TFilteredExternalEntityDeclEvent;
    FOnInternalEntityDecl: TFilteredInternalEntityDeclEvent;
    FOnEndDTD: TFilteredNotifyEvent;
    FOnEndCData: TFilteredNotifyEvent;
    FOnStartCData: TFilteredNotifyEvent;
    FOnStartDTD: TFilteredStartDTDEvent;
    FOnStartEntity: TFilteredStartEntityEvent;
    procedure setEntityResolver(const resolver : TEntityResolver); virtual;
    function getEntityResolver() : TEntityResolver; virtual;
    procedure setDTDHandler(const handler : TDTDHandler); virtual;
    function getDTDHandler() : TDTDHandler; virtual;
    procedure setContentHandler(const handler : TContentHandler); virtual;
    function getContentHandler() : TContentHandler; virtual;
    procedure setErrorHandler(const handler : TErrorHandler); virtual;
    function getErrorHandler() : TErrorHandler; virtual;
    procedure setLexicalHandler(const handler : TLexicalHandler); virtual;
    function getLexicalHandler() : TLexicalHandler; virtual;
    procedure setDeclHandler(const handler : TDeclHandler); virtual;
    function getDeclHandler() : TDeclHandler; virtual;
  protected
    FEntityResolver : TEntityResolver;
    FDTDHandler : TDTDHandler;
    FContentHandler : TContentHandler;
    FErrorHandler : TErrorHandler;
    FDeclHandler : TDeclHandler;
    FLexicalHandler : TLexicalHandler;
    { IEntityResolver }
    function resolveEntity(const publicId,
      systemId : SAXString) : IInputSource; virtual;
    { IDTDHandler }
    procedure notationDecl(const name, publicId, systemId : SAXString); virtual;
    procedure unparsedEntityDecl(const name, publicId, systemId,
      notationName : SAXString); virtual;
    { IContentHandler }
    procedure setDocumentLocator(const Locator: ILocator); virtual;
    procedure startDocument(); virtual;
    procedure endDocument(); virtual;
    procedure startPrefixMapping(const prefix, uri : SAXString); virtual;
    procedure endPrefixMapping(const prefix : SAXString); virtual;
    procedure startElement(const uri, localName, qName: SAXString;
      const atts: IAttributes); virtual;
    procedure endElement(const uri, localName,
      qName: SAXString); virtual;
    procedure characters(const ch : SAXString); virtual;
    procedure ignorableWhitespace(const ch : SAXString); virtual;
    procedure processingInstruction(const target, data : SAXString); virtual;
    procedure skippedEntity(const name : SAXString); virtual;
    { IErrorHandler }
    procedure warning(const e : ISAXParseError); virtual;
    procedure error(const e : ISAXParseError); virtual;
    procedure fatalError(const e : ISAXParseError); virtual;
    { ILexicalHandler }
    procedure startDTD(const name, publicId, systemId : SAXString); virtual;
    procedure endDTD(); virtual;
    procedure startEntity(const name : SAXString); virtual;
    procedure endEntity(const name : SAXString); virtual;
    procedure startCDATA(); virtual;
    procedure endCDATA(); virtual;
    procedure comment(const ch : SAXString); virtual;
    { IDeclHandler }
    procedure elementDecl(const name, model : SAXString); virtual;
    procedure attributeDecl(const eName, aName, attrType, mode,
      value: SAXString); virtual;
    procedure internalEntityDecl(const name, value : SAXString); virtual;
    procedure externalEntityDecl(const name, publicId, systemId : SAXString); virtual;
    { Properties }
    property OnResolveEntity : TFilteredResolveEntityEvent
      read FOnResolveEntity write FOnResolveEntity;
    property OnNotationDecl : TFilteredNotationDeclEvent
      read FOnNotationDecl write FOnNotationDecl;
    property OnUnparsedEntityDecl : TFilteredUnparsedEntityDeclEvent
      read FOnUnparsedEntityDecl write FOnUnparsedEntityDecl;
    property OnSetDocumentLocator: TFilteredDocumentLocatorEvent
      read FOnSetDocumentLocator write FOnSetDocumentLocator;
    property OnStartDocument: TFilteredNotifyEvent
      read FOnStartDocument write FOnStartDocument;
    property OnEndDocument: TFilteredNotifyEvent
      read FOnEndDocument write FOnEndDocument;
    property OnStartPrefixMapping : TFilteredStartPrefixMappingEvent
      read FOnStartPrefixMapping write FOnStartPrefixMapping;
    property OnEndPrefixMapping : TFilteredEndPrefixMappingEvent
      read FOnEndPrefixMapping write FOnEndPrefixMapping;
    property OnStartElement : TFilteredStartElementEvent
      read FOnStartElement write FOnStartElement;
    property OnEndElement : TFilteredEndElementEvent
      read FOnEndElement write FOnEndElement;
    property OnCharacters : TFilteredCharactersEvent
      read FOnCharacters write FOnCharacters;
    property OnIgnorableWhitespace : TFilteredCharactersEvent
      read FOnIgnorableWhitespace write FOnIgnorableWhitespace;
    property OnProcessingInstruction : TFilteredProcessingInstructionEvent
      read FOnProcessingInstruction write FOnProcessingInstruction;
    property OnSkippedEntity : TFilteredSkippedEntityEvent
      read FOnSkippedEntity write FOnSkippedEntity;
    property OnWarning : TFilteredErrorEvent
      read FOnWarning write FOnWarning;
    property OnError : TFilteredErrorEvent
      read FOnError write FOnError;
    property OnFatalError : TFilteredErrorEvent
      read FOnFatalError write FOnFatalError;
    property OnComment: TFilteredCommentEvent
      read FOnComment write FOnComment;
    property OnEndCData: TFilteredNotifyEvent
      read FOnEndCData write FOnEndCData;
    property OnEndDTD: TFilteredNotifyEvent
      read FOnEndDTD write FOnEndDTD;
    property OnEndEntity: TFilteredEndEntityEvent
      read FOnEndEntity write FOnEndEntity;
    property OnStartCData: TFilteredNotifyEvent
      read FOnStartCData write FOnStartCData;
    property OnStartDTD: TFilteredStartDTDEvent
      read FOnStartDTD write FOnStartDTD;
    property OnStartEntity: TFilteredStartEntityEvent
      read FOnStartEntity write FOnStartEntity;
    property OnAttributeDecl: TFilteredAttributeDeclEvent
      read FOnAttributeDecl write FOnAttributeDecl;
    property OnElementDecl: TFilteredElementDeclEvent
      read FOnElementDecl write FOnElementDecl;
    property OnExternalEntityDecl: TFilteredExternalEntityDeclEvent
      read FOnExternalEntityDecl write FOnExternalEntityDecl;
    property OnInternalEntityDecl: TFilteredInternalEntityDeclEvent
      read FOnInternalEntityDecl write FOnInternalEntityDecl;
    property ContentHandler: TContentHandler
      read getContentHandler write setContentHandler;
    property DTDHandler: TDTDHandler
      read getDTDHandler write setDTDHandler;
    property EntityResolver: TEntityResolver
      read getEntityResolver write setEntityResolver;
    property ErrorHandler: TErrorHandler
      read getErrorHandler write setErrorHandler;
    property LexicalHandler: TLexicalHandler
      read getLexicalHandler write setLexicalHandler;
    property DeclHandler: TDeclHandler
      read getDeclHandler write setDeclHandler;
  end;

  TSAXFilter = class(TSAXCustomFilter)
  published
    property OnResolveEntity;
    property OnNotationDecl;
    property OnUnparsedEntityDecl;
    property OnSetDocumentLocator;
    property OnStartDocument;
    property OnEndDocument;
    property OnStartPrefixMapping;
    property OnEndPrefixMapping;
    property OnStartElement;
    property OnEndElement;
    property OnCharacters;
    property OnIgnorableWhitespace;
    property OnProcessingInstruction;
    property OnSkippedEntity;
    property OnWarning;
    property OnError;
    property OnFatalError;
    property OnComment;
    property OnEndCData;
    property OnEndDTD;
    property OnEndEntity;
    property OnStartCData;
    property OnStartDTD;
    property OnStartEntity;
    property OnAttributeDecl;
    property OnElementDecl;
    property OnExternalEntityDecl;
    property OnInternalEntityDecl;
    property ContentHandler;
    property DTDHandler;
    property EntityResolver;
    property ErrorHandler;
    property LexicalHandler;
    property DeclHandler;
  end;

  // Has a first and a next handler always sends to both
  // unless there is an exception
  // endDocument is guaranteed
  TSAXSplitter = class(TSAXInterfacedComponent, IEntityResolver,
    IDTDHandler, IContentHandler, IErrorHandler, ILexicalHandler, IDeclHandler)
  private
    FEntityResolver : TEntityResolver;
    FDTDHandler : TDTDHandler;
    FContentHandler : TContentHandler;
    FErrorHandler : TErrorHandler;
    FDeclHandler : TDeclHandler;
    FLexicalHandler : TLexicalHandler;
    FNextEntityResolver : TEntityResolver;
    FNextDTDHandler : TDTDHandler;
    FNextContentHandler : TContentHandler;
    FNextErrorHandler : TErrorHandler;
    FNextDeclHandler : TDeclHandler;
    FNextLexicalHandler : TLexicalHandler;
    function getContentHandler: TContentHandler;
    function getDeclHandler: TDeclHandler;
    function getDTDHandler: TDTDHandler;
    function getEntityResolver: TEntityResolver;
    function getErrorHandler: TErrorHandler;
    function getLexicalHandler: TLexicalHandler;
    function getNextContentHandler: TContentHandler;
    function getNextDeclHandler: TDeclHandler;
    function getNextDTDHandler: TDTDHandler;
    function getNextEntityResolver: TEntityResolver;
    function getNextErrorHandler: TErrorHandler;
    function getNextLexicalHandler: TLexicalHandler;
    procedure setContentHandler(const Value: TContentHandler);
    procedure setDeclHandler(const Value: TDeclHandler);
    procedure setDTDHandler(const Value: TDTDHandler);
    procedure setEntityResolver(const Value: TEntityResolver);
    procedure setErrorHandler(const Value: TErrorHandler);
    procedure setLexicalHandler(const Value: TLexicalHandler);
    procedure setNextContentHandler(const Value: TContentHandler);
    procedure setNextDeclHandler(const Value: TDeclHandler);
    procedure setNextDTDHandler(const Value: TDTDHandler);
    procedure setNextEntityResolver(const Value: TEntityResolver);
    procedure setNextErrorHandler(const Value: TErrorHandler);
    procedure setNextLexicalHandler(const Value: TLexicalHandler);
  protected
    { IEntityResolver }
    function resolveEntity(const publicId,
      systemId : SAXString) : IInputSource; virtual;
    { IDTDHandler }
    procedure notationDecl(const name, publicId, systemId : SAXString); virtual;
    procedure unparsedEntityDecl(const name, publicId, systemId,
      notationName : SAXString); virtual;
    { IContentHandler }
    procedure setDocumentLocator(const Locator: ILocator); virtual;
    procedure startDocument(); virtual;
    procedure endDocument(); virtual;
    procedure startPrefixMapping(const prefix, uri : SAXString); virtual;
    procedure endPrefixMapping(const prefix : SAXString); virtual;
    procedure startElement(const uri, localName, qName: SAXString;
      const atts: IAttributes); virtual;
    procedure endElement(const uri, localName,
      qName: SAXString); virtual;
    procedure characters(const ch : SAXString); virtual;
    procedure ignorableWhitespace(const ch : SAXString); virtual;
    procedure processingInstruction(const target, data : SAXString); virtual;
    procedure skippedEntity(const name : SAXString); virtual;
    { IErrorHandler }
    procedure warning(const e : ISAXParseError); virtual;
    procedure error(const e : ISAXParseError); virtual;
    procedure fatalError(const e : ISAXParseError); virtual;
    { ILexicalHandler }
    procedure startDTD(const name, publicId, systemId : SAXString); virtual;
    procedure endDTD(); virtual;
    procedure startEntity(const name : SAXString); virtual;
    procedure endEntity(const name : SAXString); virtual;
    procedure startCDATA(); virtual;
    procedure endCDATA();  virtual;
    procedure comment(const ch : SAXString); virtual;
    { IDeclHandler }
    procedure elementDecl(const name, model : SAXString); virtual;
    procedure attributeDecl(const eName, aName, attrType, mode,
      value: SAXString); virtual;
    procedure internalEntityDecl(const name, value : SAXString); virtual;
    procedure externalEntityDecl(const name, publicId, systemId : SAXString); virtual;
  published
    property ContentHandler: TContentHandler
      read getContentHandler write setContentHandler;
    property DTDHandler: TDTDHandler
      read getDTDHandler write setDTDHandler;
    property EntityResolver: TEntityResolver
      read getEntityResolver write setEntityResolver;
    property ErrorHandler: TErrorHandler
      read getErrorHandler write setErrorHandler;
    property LexicalHandler: TLexicalHandler
      read getLexicalHandler write setLexicalHandler;
    property DeclHandler: TDeclHandler
      read getDeclHandler write setDeclHandler;
    property NextContentHandler: TContentHandler
      read getNextContentHandler write setNextContentHandler;
    property NextDTDHandler: TDTDHandler
      read getNextDTDHandler write setNextDTDHandler;
    property NextEntityResolver: TEntityResolver
      read getNextEntityResolver write setNextEntityResolver;
    property NextErrorHandler: TErrorHandler
      read getNextErrorHandler write setNextErrorHandler;
    property NextLexicalHandler: TLexicalHandler
      read getNextLexicalHandler write setNextLexicalHandler;
    property NextDeclHandler: TDeclHandler
      read getNextDeclHandler write setNextDeclHandler;
  end;


implementation

resourcestring
  MismatchElement = 'Mismatched element: %s vs %s';
  NoReader        = 'No XMLReader has been established';
  NoVendor        = 'Unknown SAX vendor %s';
  OpenElements    = 'Open elements at end of document';

{ TSAXInterfacedComponent -----------------------------------------------------}

procedure TSAXInterfacedComponent.AfterConstruction;
begin
  inherited;
  FOwnerIsComponent:= (Owner <> nil) and (Owner is TComponent);
  _AddRef;
end;

procedure TSAXInterfacedComponent.BeforeDestruction;
begin
  inherited;
end;

class function TSAXInterfacedComponent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TSAXInterfacedComponent(Result).FRefCount:= 1;
end;

function TSAXInterfacedComponent._AddRef: Integer;
begin
  Result:= InterlockedIncrement(FRefCount);
end;

function TSAXInterfacedComponent._Release: Integer;
begin
  Result:= InterlockedDecrement(FRefCount);
  if (Result = 0) and (not FOwnerIsComponent) then
    Destroy;
end;


{ TSAXCustomXMLReader ---------------------------------------------------------}

const
  OptionNames: array [TSAXOption] of string =
    (NamespacesFeature,
     NamespacePrefixesFeature,
     ValidationFeature,
     ExternalGeneralFeature,
     ExternalParameterFeature,
     IsStandaloneFeature,
     UseAttributes2Feature,
     UseLocator2Feature,
     UseEntityResolver2Feature,
     ResolveDTDURIsFeature,
     LexicalParameterFeature
    );

constructor TSAXCustomXMLReader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXML   := TStringList.Create;
  FVendor := DefaultSAXVendor;
end;

destructor TSAXCustomXMLReader.Destroy;
begin
  FXML.Free;
  inherited Destroy;
end;

procedure TSAXCustomXMLReader.DoInvalidFeature(Name: string; Value: Boolean;
  Error: ESAXException);
begin
  if Assigned(FOnInvalidFeature) then
    FOnInvalidFeature(Self, Name, Value, Error)
  else
    raise Error;
end;

procedure TSAXCustomXMLReader.DoInvalidProperty(Name: string; Value: IProperty;
  Error: ESAXException);
begin
  if Assigned(FOnInvalidProperty) then
    FOnInvalidProperty(Self, Name, Value, Error)
  else
    raise Error;
end;

function TSAXCustomXMLReader.GetContentHandler: IContentHandler;
begin
  Result := FContentHandler;
end;

function TSAXCustomXMLReader.GetContentHandlerP: TContentHandler;
begin
  Result := FContentHandler;
end;

function TSAXCustomXMLReader.GetDeclHandlerP: TDeclHandler;
begin
  Result := FDeclHandler;
end;

function TSAXCustomXMLReader.GetDTDHandler: IDTDHandler;
begin
  Result := FDTDHandler;
end;

function TSAXCustomXMLReader.GetDTDHandlerP: TDTDHandler;
begin
  Result := FDTDHandler;
end;

function TSAXCustomXMLReader.GetEntityResolver: IEntityResolver;
begin
  Result := FEntityResolver;
end;

function TSAXCustomXMLReader.GetEntityResolverP: TEntityResolver;
begin
  Result := FEntityResolver;
end;

function TSAXCustomXMLReader.GetErrorHandler: IErrorHandler;
begin
  Result := FErrorHandler;
end;

function TSAXCustomXMLReader.GetErrorHandlerP: TErrorHandler;
begin
  Result := FErrorHandler;
end;

function TSAXCustomXMLReader.GetLexicalHandlerP: TLexicalHandler;
begin
  Result := FLexicalHandler;
end;

function TSAXCustomXMLReader.GetFeature(const Name: SAXString): Boolean;
begin
  Result := False;
  try
    if Assigned(FXMLReader) then
      Result := FXMLReader.Features[Name];
  except on E: ESAXException do
    DoInvalidFeature(Name, False, E);
  end;
end;

{ Convert SAX features into options. }
function TSAXCustomXMLReader.GetOptions: TSAXOptions;
var
  Option: TSAXOption;

  procedure CheckOption(Option: TSAXOption);
  begin
    try
      if FXMLReader.Features[OptionNames[Option]] then
        Include(Result, Option);
    except
      { Ignore }
    end;
  end;

begin
  Result := [];
  if Assigned(FXMLReader) then
    for Option := Low(TSAXOption) to High(TSAXOption) do
      CheckOption(Option);
end;

function TSAXCustomXMLReader.GetProperty(const Name: SAXString) : IProperty;
begin
  Result:= nil;
  try
    if Assigned(FXMLReader) then
      Result:= FXMLReader.getProperty(Name);
  except on E: ESAXException do
    DoInvalidProperty(Name, Result, E);
  end;
end;

{ Tidy up if attached components are deleted. }
procedure TSAXCustomXMLReader.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  {$IFDEF DELPHI6_UP}
  if Operation = opRemove then
  begin
    { Need to check based on interfaces. }
    if Assigned(ContentHandler) and
        AComponent.IsImplementorOf(ContentHandler) then
      ContentHandler := nil;
    if Assigned(DeclHandler) and AComponent.IsImplementorOf(DeclHandler) then
      DeclHandler := nil;
    if Assigned(DTDHandler) and AComponent.IsImplementorOf(DTDHandler) then
      DTDHandler := nil;
    if Assigned(EntityResolver) and
        AComponent.IsImplementorOf(EntityResolver) then
      EntityResolver := nil;
    if Assigned(ErrorHandler) and AComponent.IsImplementorOf(ErrorHandler) then
      ErrorHandler := nil;
    if Assigned(LexicalHandler) and
        AComponent.IsImplementorOf(LexicalHandler) then
      LexicalHandler := nil;
  end;
  {$ENDIF}
end;

{ Start the parse process on the XML document
  identified by the URL or XML properties. }
procedure TSAXCustomXMLReader.Parse;
var
  Input: IStreamInputSource;
  XMLStr: SAXString;
begin
  if not Assigned(FXMLReader) then
    raise ESAXException.Create(NoReader);

  if URL <> '' then
    Input := TStreamInputSource.Create(TFileStream.Create(URL, fmOpenRead), soOwned) as IStreamInputSource
  else begin
    XMLStr:= XML.Text;
    Input := TStreamInputSource.Create(TStringStream.Create(XMLStr), soOwned) as IStreamInputSource;
  end;
  try
    ParseSetup;
    FXMLReader.Parse(Input);
  finally
    Input:= nil;
  end;
end;

{ Start the parse process on the supplied stream
  (embedded in the input source). }
procedure TSAXCustomXMLReader.Parse(const Input: IInputSource);
begin
  if not Assigned(FXMLReader) then
    raise ESAXException.Create(NoReader);

  ParseSetup;
  FXMLReader.Parse(Input);
end;

{ Start the parse process on the document identified by the supplied URL. }
procedure TSAXCustomXMLReader.Parse(const SystemId: SAXString);
begin
  if not Assigned(FXMLReader) then
    raise ESAXException.Create(NoReader);

  ParseSetup;
  FXMLReader.Parse(SystemId);
end;

{ Attach the handlers to the reader. }
procedure TSAXCustomXMLReader.ParseSetup;
var IDecl : IInterfaceProperty;
    ILexical : IInterfaceProperty;
begin
  FXMLReader.ContentHandler := ContentHandler;
  FXMLReader.DTDHandler     := DTDHandler;
  FXMLReader.EntityResolver := EntityResolver;
  FXMLReader.ErrorHandler   := ErrorHandler;

  if Assigned(DeclHandler) then
  begin
    try
      IDecl:= FXMLReader.getProperty(DeclHandlerProperty) as IInterfaceProperty;
      IDecl.Value:= DeclHandler as IDeclHandler;
    except
      raise;
      //!! Showmessage('Error');
    end;
  end;

  if Assigned(LexicalHandler) then
  begin
    try
      ILexical:= FXMLReader.getProperty(LexicalHandlerProperty) as IInterfaceProperty;
      ILexical.Value:= LexicalHandler as ILexicalHandler;
    except
      //!! Showmessage('Error');
    end;
  end;
end;

procedure TSAXCustomXMLReader.SetContentHandler(const Value: IContentHandler);
begin
  {$IFDEF DELPHI6_UP}
  SetContentHandlerP(Value);
  {$ELSE}
  raise ESAXException.Create('Please use the property instead');
  {$ENDIF}
end;

{ These properties are interfaces in D6,
  so need extra work to preserve connections. }
procedure TSAXCustomXMLReader.SetContentHandlerP(const Value: TContentHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FContentHandler, opRemove);
  FContentHandler := Value;
  ReferenceInterface(FContentHandler, opInsert);
  {$ELSE}
  FContentHandler := Value;
  {$ENDIF}
  if (FXMLReader <> nil) then
    FXMLReader.ContentHandler := ContentHandler;
end;

procedure TSAXCustomXMLReader.SetDeclHandler(const Value: IDeclHandler);
begin
  {$IFDEF DELPHI6_UP}
  SetDeclHandlerP(Value);
  {$ELSE}
  raise ESAXException.Create('Please use the property instead');
  {$ENDIF}
end;

procedure TSAXCustomXMLReader.SetDeclHandlerP(const Value: TDeclHandler);
var IDecl : IInterfaceProperty;
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FDeclHandler, opRemove);
  FDeclHandler := Value;
  ReferenceInterface(FDeclHandler, opInsert);
  {$ELSE}
  FDeclHandler := Value;
  {$ENDIF}
  if (FXMLReader <> nil) then
    if Assigned(DeclHandler) then
    begin
      try
        IDecl:= FXMLReader.getProperty(DeclHandlerProperty) as IInterfaceProperty;
        IDecl.Value:= DeclHandler as IDeclHandler;
      except
        raise;
        //!! Showmessage('Error');
      end;
    end;
end;

procedure TSAXCustomXMLReader.SetDTDHandler(const Value: IDTDHandler);
begin
  {$IFDEF DELPHI6_UP}
  SetDTDHandlerP(Value);
  {$ELSE}
  raise ESAXException.Create('Please use the property instead');
  {$ENDIF}
end;

procedure TSAXCustomXMLReader.SetDTDHandlerP(const Value: TDTDHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FDTDHandler, opRemove);
  FDTDHandler := Value;
  ReferenceInterface(FDTDHandler, opInsert);
  {$ELSE}
  FDTDHandler := Value;
  {$ENDIF}
  if (FXMLReader <> nil) then
    FXMLReader.DTDHandler := DTDHandler;
end;

procedure TSAXCustomXMLReader.SetEntityResolver(const Value: IEntityResolver);
begin
  {$IFDEF DELPHI6_UP}
  SetEntityResolverP(Value);
  {$ELSE}
  raise ESAXException.Create('Please use the property instead');
  {$ENDIF}
end;

procedure TSAXCustomXMLReader.SetEntityResolverP(const Value: TEntityResolver);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FEntityResolver, opRemove);
  FEntityResolver := Value;
  ReferenceInterface(FEntityResolver, opInsert);
  {$ELSE}
  FEntityResolver := Value;
  {$ENDIF}
  if (FXMLReader <> nil) then
    FXMLReader.EntityResolver := EntityResolver;
end;

procedure TSAXCustomXMLReader.SetErrorHandler(const Value: IErrorHandler);
begin
  {$IFDEF DELPHI6_UP}
  SetErrorHandlerP(Value);
  {$ELSE}
  raise ESAXException.Create('Please use the property instead');
  {$ENDIF}
end;

procedure TSAXCustomXMLReader.SetErrorHandlerP(const Value: TErrorHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FErrorHandler, opRemove);
  FErrorHandler := Value;
  ReferenceInterface(FErrorHandler, opInsert);
  {$ELSE}
  FErrorHandler := Value;
  {$ENDIF}
  if (FXMLReader <> nil) then
    FXMLReader.ErrorHandler := ErrorHandler;
end;

procedure TSAXCustomXMLReader.SetFeature(const Name: SAXString;
  Value: Boolean);
begin
  try
    if Assigned(FXMLReader) then
      FXMLReader.Features[Name] := Value;
  except on E: ESAXException do
    DoInvalidFeature(Name, Value, E);
  end;
end;

procedure TSAXCustomXMLReader.SetLexicalHandler(const Value: ILexicalHandler);
begin
  {$IFDEF DELPHI6_UP}
  SetLexicalHandlerP(Value);
  {$ELSE}
  raise ESAXException.Create('Please use the property instead');
  {$ENDIF}
end;

procedure TSAXCustomXMLReader.SetLexicalHandlerP(const Value: TLexicalHandler);
var ILexical : IInterfaceProperty;
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FLexicalHandler, opRemove);
  FLexicalHandler := Value;
  ReferenceInterface(FLexicalHandler, opInsert);
  {$ELSE}
  FLexicalHandler := Value;
  {$ENDIF}
  if (FXMLReader <> nil) then
    if Assigned(LexicalHandler) then
    begin
      try
        ILexical:= FXMLReader.getProperty(LexicalHandlerProperty) as IInterfaceProperty;
        ILexical.Value:= LexicalHandler as ILexicalHandler;
      except
        //!! Showmessage('Error');
      end;
    end;
end;

{ Convert options into SAX features. }
procedure TSAXCustomXMLReader.SetOptions(const Value: TSAXOptions);
var
  Option: TSAXOption;

  procedure SetOption(Option: TSAXOption; Value: Boolean);
  begin
    try
      FXMLReader.Features[OptionNames[Option]] := Value;
    except
      { Ignore }
    end;
  end;

begin
  if Assigned(FXMLReader) then
    for Option := Low(TSAXOption) to High(TSAXOption) do
      SetOption(Option, Option in Value);
end;

procedure TSAXCustomXMLReader.SetURL(const Value: string);
begin
  FURL := Value;
  FXML.Clear;
end;

{ Find the nominated vendor and create a new reader for them.
  Raises an error if the vendor is not known.
  Vendors register themselves through the RegisterSAXVendor method
  in the SAX unit - usually in their initialization sections. }
procedure TSAXCustomXMLReader.SetVendor(const Value: string);
var
  SAXVendor: TSAXVendor;
begin
  //v.p 13/09/2001 - added check for 
  // FXMLReader assigned, otherwise the Vendor property needed to be 
  // changed and then reset back to original value.
  if (FVendor <> Value) or (not Assigned(FXMLReader)) then
  begin
    SAXVendor := GetSAXVendor(Value);
    if not Assigned(SAXVendor) then
      raise ESAXException.Create(Format(NoVendor, [Value]));
    FVendor    := Value;
    FXMLReader := SAXVendor.XMLReader as IXMLReader;
  end;
end;

procedure TSAXCustomXMLReader.SetXML(const Value: TStrings);
begin
  FXML.Assign(Value);
  FURL := '';
end;

{ TSAXCustomContentHandler ----------------------------------------------------}

{ Pass on all SAX events to the attached event handlers (if present). }
procedure TSAXCustomContentHandler.Characters(const PCh: SAXString);
begin
  if Assigned(FOnCharacters) then
    FOnCharacters(Self, PCh);
end;

procedure TSAXCustomContentHandler.EndDocument;
begin
  if Assigned(FOnEndDocument) then
    FOnEndDocument(Self);
end;

procedure TSAXCustomContentHandler.EndElement(const NamespaceURI, LocalName,
  QName: SAXString);
begin
  if Assigned(FOnEndElement) then
    FOnEndElement(Self, NamespaceURI, LocalName, QName);
end;

procedure TSAXCustomContentHandler.EndPrefixMapping(const Prefix: SAXString);
begin
  if Assigned(FOnEndPrefixMapping) then
    FOnEndPrefixMapping(Self, Prefix);
end;

procedure TSAXCustomContentHandler.IgnorableWhitespace(const PCh: SAXString);
begin
  if Assigned(FOnIgnorableWhitespace) then
    FOnIgnorableWhitespace(Self, PCh);
end;

procedure TSAXCustomContentHandler.ProcessingInstruction(const Target, Data:
  SAXString);
begin
  if Assigned(FOnProcessingInstruction) then
    FOnProcessingInstruction(Self, Target, Data);
end;

procedure TSAXCustomContentHandler.SetDocumentLocator(const Locator: ILocator);
begin
  if Assigned(FOnSetDocumentLocator) then
    FOnSetDocumentLocator(Self, Locator);
end;

procedure TSAXCustomContentHandler.SkippedEntity(const Name: SAXString);
begin
  if Assigned(FOnSkippedEntity) then
    FOnSkippedEntity(Self, Name);
end;

procedure TSAXCustomContentHandler.StartDocument;
begin
  if Assigned(FOnStartDocument) then
    FOnStartDocument(Self);
end;

procedure TSAXCustomContentHandler.StartElement(const NamespaceURI, LocalName,
  QName: SAXString; const Atts: IAttributes);
begin
  if Assigned(FOnStartElement) then
    FOnStartElement(Self, NamespaceURI, LocalName, QName, Atts);
end;

procedure TSAXCustomContentHandler.StartPrefixMapping(const Prefix, URI:
  SAXString);
begin
  if Assigned(FOnStartPrefixMapping) then
    FOnStartPrefixMapping(Self, Prefix, URI);
end;

{ TSAXContentWriter -----------------------------------------------------------}

{ Pass on all SAX events to the attached document writer (if present).
  This adapter component is necessary in Delphi 4 and 5 since published
  properties cannot be interfaces, and you can't have multiple inheritance. }
procedure TSAXContentWriter.Characters(const PCh: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.Characters(PCh);
end;

procedure TSAXContentWriter.EndDocument;
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.EndDocument;
end;

procedure TSAXContentWriter.EndElement(const NamespaceURI, LocalName,
  QName: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.EndElement(NamespaceURI, LocalName, QName);
end;

procedure TSAXContentWriter.EndPrefixMapping(const Prefix: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.EndPrefixMapping(Prefix);
end;

procedure TSAXContentWriter.IgnorableWhitespace(const PCh: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.IgnorableWhitespace(PCh);
end;

{ Tidy up if attached components are deleted. }
procedure TSAXContentWriter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if Assigned(DocumentWriter) and (DocumentWriter = AComponent) then
      DocumentWriter := nil;
  end;
end;

procedure TSAXContentWriter.ProcessingInstruction(const Target, Data: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.ProcessingInstruction(Target, Data);
end;

procedure TSAXContentWriter.SetDocumentLocator(const Locator: ILocator);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.SetDocumentLocator(Locator);
end;

procedure TSAXContentWriter.SkippedEntity(const Name: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.SkippedEntity(Name);
end;

procedure TSAXContentWriter.StartDocument;
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.StartDocument;
end;

procedure TSAXContentWriter.StartElement(const NamespaceURI, LocalName,
  QName: SAXString; const Atts: IAttributes);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.StartElement(NamespaceURI, LocalName, QName, Atts);
end;

procedure TSAXContentWriter.StartPrefixMapping(const Prefix, URI: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.StartPrefixMapping(Prefix, URI);
end;

{ TSAXCustomDeclHandler -------------------------------------------------------}

{ Pass on all SAX events to the attached event handlers (if present). }
procedure TSAXCustomDeclHandler.AttributeDecl(const EName, AName, AttrType,
  ValueDefault, Value: SAXString);
begin
  if Assigned(FOnAttributeDecl) then
    FOnAttributeDecl(Self, EName, AName, AttrType, ValueDefault, Value);
end;

procedure TSAXCustomDeclHandler.ElementDecl(const Name, Model: SAXString);
begin
  if Assigned(FOnElementDecl) then
    FOnElementDecl(Self, Name, Model);
end;

procedure TSAXCustomDeclHandler.ExternalEntityDecl(const Name, PublicId,
  SystemId: SAXString);
begin
  if Assigned(FOnExternalEntityDecl) then
    FOnExternalEntityDecl(Self, Name, PublicId, SystemId);
end;

procedure TSAXCustomDeclHandler.InternalEntityDecl(
  const Name, Value: SAXString);
begin
  if Assigned(FOnInternalEntityDecl) then
    FOnInternalEntityDecl(Self, Name, Value);
end;

{ TSAXDeclWriter --------------------------------------------------------------}

{ Pass on all SAX events to the attached document writer (if present).
  This adapter component is necessary in Delphi 4 and 5 since published
  properties cannot be interfaces, and you can't have multiple inheritance. }
procedure TSAXDeclWriter.AttributeDecl(const EName, AName, AttrType,
  ValueDefault, Value: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.AttributeDecl(EName, AName, AttrType, ValueDefault, Value);
end;

procedure TSAXDeclWriter.ElementDecl(const Name, Model: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.ElementDecl(Name, Model);
end;

procedure TSAXDeclWriter.ExternalEntityDecl(const Name, PublicId,
  SystemId: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.ExternalEntityDecl(Name, PublicId, SystemId);
end;

procedure TSAXDeclWriter.InternalEntityDecl(const Name, Value: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.InternalEntityDecl(Name, Value);
end;

{ Tidy up if attached components are deleted. }
procedure TSAXDeclWriter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if Assigned(DocumentWriter) and (DocumentWriter = AComponent) then
      DocumentWriter := nil;
  end;
end;

{ TSAXCustomDTDHandler --------------------------------------------------------}

{ Pass on all SAX events to the attached event handlers (if present). }
procedure TSAXCustomDTDHandler.NotationDecl(const Name, PublicId,
  SystemId: SAXString);
begin
  if Assigned(FOnNotationDecl) then
    FOnNotationDecl(Self, Name, PublicId, SystemId);
end;

procedure TSAXCustomDTDHandler.UnparsedEntityDecl(const Name, PublicId,
  SystemId, NotationName: SAXString);
begin
  if Assigned(FOnUnparsedEntityDecl) then
    FOnUnparsedEntityDecl(Self, Name, PublicId, SystemId, NotationName);
end;

{ TSAXDTDWriter ---------------------------------------------------------------}

{ Pass on all SAX events to the attached document writer (if present).
  This adapter component is necessary in Delphi 4 and 5 since published
  properties cannot be interfaces, and you can't have multiple inheritance. }
procedure TSAXDTDWriter.NotationDecl(const Name, PublicId, SystemId: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.NotationDecl(Name, PublicId, SystemId);
end;

{ Tidy up if attached components are deleted. }
procedure TSAXDTDWriter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if Assigned(DocumentWriter) and (DocumentWriter = AComponent) then
      DocumentWriter := nil;
  end;
end;

procedure TSAXDTDWriter.UnparsedEntityDecl(const Name, PublicId, SystemId,
  NotationName: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.UnparsedEntityDecl(Name, PublicId, SystemId, NotationName);
end;

{ TSAXCustomEntityResolver ----------------------------------------------------}

{ Pass on all SAX events to the attached event handlers (if present). }
function TSAXCustomEntityResolver.ResolveEntity(const PublicId, SystemId:
  SAXString): IInputSource;
begin
  if Assigned(FOnResolveEntity) then
    Result := FOnResolveEntity(Self, PublicId, SystemId)
  else
    Result := nil;
end;

{ TSAXCustomErrorHandler ------------------------------------------------------}

{ Pass on all SAX events to the attached event handlers (if present). }
procedure TSAXCustomErrorHandler.Error(const Error: ISAXParseError);
begin
  if Assigned(FOnError) then
    FOnError(Self, Error);
end;

procedure TSAXCustomErrorHandler.FatalError(
  const Error: ISAXParseError);
begin
  if Assigned(FOnFatalError) then
    FOnFatalError(Self, Error)
  else begin
    { Fatal errors should be notified if not otherwise handled. }
    raise ESAXParseException.Create(Error.getMessage(), Error.getPublicId(),
      Error.getSystemId(), Error.getLineNumber(), Error.getColumnNumber());
  end;
end;

procedure TSAXCustomErrorHandler.Warning(const Error: ISAXParseError);
begin
  if Assigned(FOnWarning) then
    FOnWarning(Self, Error);
end;

{ TSAXCustomLexicalHandler ----------------------------------------------------}

{ Pass on all SAX events to the attached event handlers (if present). }
procedure TSAXCustomLexicalHandler.Comment(const PCh: SAXString);
begin
  if Assigned(FOnComment) then
    FOnComment(Self, PCh);
end;

procedure TSAXCustomLexicalHandler.EndCData;
begin
  if Assigned(FOnEndCData) then
    FOnEndCData(Self);
end;

procedure TSAXCustomLexicalHandler.EndDTD;
begin
  if Assigned(FOnEndDTD) then
    FOnEndDTD(Self);
end;

procedure TSAXCustomLexicalHandler.EndEntity(const Name: SAXString);
begin
  if Assigned(FOnEndEntity) then
    FOnEndEntity(Self, Name);
end;

procedure TSAXCustomLexicalHandler.StartCData;
begin
  if Assigned(FOnStartCData) then
    FOnStartCData(Self);
end;

procedure TSAXCustomLexicalHandler.StartDTD(const Name, PublicId,
  SystemId: SAXString);
begin
  if Assigned(FOnStartDTD) then
    FOnStartDTD(Self, Name, PublicId, SystemId);
end;

procedure TSAXCustomLexicalHandler.StartEntity(const Name: SAXString);
begin
  if Assigned(FOnStartEntity) then
    FOnStartEntity(Self, Name);
end;

{ TSAXLexicalWriter -----------------------------------------------------------}

{ Pass on all SAX events to the attached document writer (if present).
  This adapter component is necessary in Delphi 4 and 5 since published
  properties cannot be interfaces, and you can't have multiple inheritance. }
procedure TSAXLexicalWriter.Comment(const PCh: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.Comment(PCh);
end;

procedure TSAXLexicalWriter.EndCData;
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.EndCData;
end;

procedure TSAXLexicalWriter.EndDTD;
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.EndDTD;
end;

procedure TSAXLexicalWriter.EndEntity(const Name: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.EndEntity(Name);
end;

{ Tidy up if attached components are deleted. }
procedure TSAXLexicalWriter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if Assigned(DocumentWriter) and (DocumentWriter = AComponent) then
      DocumentWriter := nil;
  end;
end;

procedure TSAXLexicalWriter.StartCData;
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.StartCData;
end;

procedure TSAXLexicalWriter.StartDTD(const Name, PublicId, SystemId: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.StartDTD(Name, PublicId, SystemId);
end;

procedure TSAXLexicalWriter.StartEntity(const Name: SAXString);
begin
  if Assigned(DocumentWriter) then
    DocumentWriter.StartEntity(Name);
end;

{ TSAXCustomDocumentWriter ----------------------------------------------------------}

const
  CRLF: array [Boolean] of string = ('', #13#10);
  ELEM_OPEN    = 1;
  ELEM_CONTENT = 2;

constructor TSAXCustomDocumentWriter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
{$IFDEF DELPHI5_UP}
    FreeAndNil(FStream);
{$ELSE}
    FStream.Free;
    FStream := nil;
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
{$IFDEF DELPHI5_UP}
    FreeAndNil(FStream);
{$ELSE}
    FStream.Free;
    FStream := nil;
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

procedure TSAXCustomDocumentWriter.SetFilename(const Value: TFilename);
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
{$IFDEF DELPHI5_UP}
      FreeAndNil(FStream);
{$ELSE}
      FStream.Free;
      FStream := nil;
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
  CheckInElement(False);
  Write('<![CDATA[');
end;

{ Prepare the output stream for use and initialise variables for generating. }
procedure TSAXCustomDocumentWriter.StartDocument;
const
  YesNo: array [Boolean] of string = ('no', 'yes');
begin
  if (not FUserStream) and (FStream <> nil) then
  begin
{$IFDEF DELPHI5_UP}
    FreeAndNil(FStream);
{$ELSE}
    FStream.Free;
    FStream := nil;
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
      Write(' ' + Atts.GetQName(Index) + '="' + Atts.GetValue(Index) + '"');
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
  Str: string;
{$IFDEF DELPHI6_UP}
  Len: Integer;
  UStr: UTF8String;
begin
  Len  := Length(Text)* SizeOf(SAXChar);
  UStr := Utf8Encode(Text);
  if Length(UStr) < Len then
  begin
    Str := Text;
    if Str <> UStr then
      FStream.Write(Pointer(UStr)^, Length(UStr))
    else
      FStream.Write(Pointer(Str)^, Length(Str));
  end
  else
    FStream.Write(Pointer(Text)^, Len);
{$ELSE}
begin
  Str := Text;
  FStream.Write(Pointer(Str)^, Length(Str));
{$ENDIF}
end;

{ TSAXCustomFilter }

procedure TSAXCustomFilter.setEntityResolver(
  const resolver : TEntityResolver);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FEntityResolver, opRemove);
  FEntityResolver := resolver;
  ReferenceInterface(FEntityResolver, opInsert);
  {$ELSE}
  FEntityResolver := resolver;
  {$ENDIF}
end;

function TSAXCustomFilter.getEntityResolver() : TEntityResolver;
begin
  Result := FEntityResolver;
end;

procedure TSAXCustomFilter.setDTDHandler(
  const handler : TDTDHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FDTDHandler, opRemove);
  FDTDHandler := handler;
  ReferenceInterface(FDTDHandler, opInsert);
  {$ELSE}
  FDTDHandler := handler;
  {$ENDIF}
end;

function TSAXCustomFilter.getDTDHandler() : TDTDHandler;
begin
  Result := FDTDHandler;
end;

procedure TSAXCustomFilter.setContentHandler(
  const handler : TContentHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FContentHandler, opRemove);
  FContentHandler := handler;
  ReferenceInterface(FContentHandler, opInsert);
  {$ELSE}
  FContentHandler := handler;
  {$ENDIF}
end;

function TSAXCustomFilter.getContentHandler() : TContentHandler;
begin
  Result := FContentHandler;
end;

procedure TSAXCustomFilter.setErrorHandler(
  const handler : TErrorHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FErrorHandler, opRemove);
  FErrorHandler := handler;
  ReferenceInterface(FErrorHandler, opInsert);
  {$ELSE}
  FErrorHandler := handler;
  {$ENDIF}
end;

function TSAXCustomFilter.getErrorHandler() : TErrorHandler;
begin
  Result := FErrorHandler;
end;

procedure TSAXCustomFilter.setLexicalHandler(const handler : TLexicalHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FLexicalHandler, opRemove);
  FLexicalHandler := handler;
  ReferenceInterface(FLexicalHandler, opInsert);
  {$ELSE}
  FLexicalHandler := handler;
  {$ENDIF}
end;

function TSAXCustomFilter.getLexicalHandler() : TLexicalHandler;
begin
  Result := FLexicalHandler;
end;

procedure TSAXCustomFilter.setDeclHandler(const handler : TDeclHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FDeclHandler, opRemove);
  FDeclHandler := handler;
  ReferenceInterface(FDeclHandler, opInsert);
  {$ELSE}
  FDeclHandler := handler;
  {$ENDIF}
end;

function TSAXCustomFilter.getDeclHandler() : TDeclHandler;
begin
  Result := FDeclHandler;
end;

function TSAXCustomFilter.resolveEntity(const publicId,
  systemId : SAXString) : IInputSource;
var Filtered : Boolean;
    _PublicId, _SystemId : SAXString;
begin
  Result:= nil;
  Filtered:= False;
  _PublicId:= publicId;
  _SystemId:= systemId;
  if (Assigned(FOnResolveEntity)) then
    Result:= FOnResolveEntity(Self, _PublicId, _SystemId,  Filtered);
  if (not Filtered) then
    if (Assigned(FEntityResolver)) then
      Result:= FEntityResolver.resolveEntity(_PublicId, _SystemId);
end;

procedure TSAXCustomFilter.notationDecl(const name, publicId,
  systemId : SAXString);
var Filtered : Boolean;
    _Name, _PublicId, _SystemId : SAXString;
begin
  Filtered:= False;
  _Name:= name;
  _PublicId:= publicId;
  _SystemId:= systemId;
  if (Assigned(FOnNotationDecl)) then
    FOnNotationDecl(Self, _Name, _PublicId, _SystemId, Filtered);
  if (not Filtered) then
    if (Assigned(FDTDHandler)) then
      FDTDHandler.notationDecl(_Name, _PublicId, _SystemId);
end;

procedure TSAXCustomFilter.unparsedEntityDecl(const name, publicId,
  systemId, notationName : SAXString);
var Filtered : Boolean;
    _Name, _PublicId, _SystemId, _NotationName : SAXString;
begin
  Filtered:= False;
  _Name:= name;
  _PublicId:= publicId;
  _SystemId:= systemId;
  _NotationName:= notationName;
  if (Assigned(FOnUnparsedEntityDecl)) then
    FOnUnparsedEntityDecl(Self, _Name, _PublicId, _SystemId, _NotationName,
      Filtered);
  if (not Filtered) then
    if (Assigned(FDTDHandler)) then
      FDTDHandler.unparsedEntityDecl(_Name, _PublicId, _SystemId, _NotationName);
end;

procedure TSAXCustomFilter.setDocumentLocator(const Locator: ILocator);
var Filtered : Boolean;
    _Locator : ILocator;
begin
  Filtered:= False;
  _Locator:= Locator;
  if (Assigned(FOnSetDocumentLocator)) then
    FOnSetDocumentLocator(Self, _Locator,  Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.setDocumentLocator(_Locator);
end;

procedure TSAXCustomFilter.startDocument();
var Filtered : Boolean;
begin
  Filtered:= False;
  if (Assigned(FOnStartDocument)) then
    FOnStartDocument(Self, Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.startDocument;
end;

procedure TSAXCustomFilter.endDocument();
var Filtered : Boolean;
begin
  Filtered:= False;
  if (Assigned(FOnEndDocument)) then
    FOnEndDocument(Self, Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.endDocument;
end;

procedure TSAXCustomFilter.startPrefixMapping(const prefix,
  uri : SAXString);
var Filtered : Boolean;
    _Prefix, _Uri : SAXString;
begin
  Filtered:= False;
  _Prefix:= prefix;
  _Uri:= uri;
  if (Assigned(FOnStartPrefixMapping)) then
    FOnStartPrefixMapping(Self, _Prefix, _Uri, Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.startPrefixMapping(_Prefix, _Uri);
end;

procedure TSAXCustomFilter.endPrefixMapping(const prefix : SAXString);
var Filtered : Boolean;
    _Prefix : SAXString;
begin
  Filtered:= False;
  _Prefix:= prefix;
  if (Assigned(FOnEndPrefixMapping)) then
    FOnEndPrefixMapping(Self, _Prefix, Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.endPrefixMapping(_Prefix);
end;

procedure TSAXCustomFilter.startElement(const uri, localName,
  qName: SAXString; const atts: IAttributes);
var Filtered : Boolean;
    _Uri, _LocalName, _QName : SAXString;
    _Atts : IAttributes;
begin
  Filtered:= False;
  _Uri:= uri;
  _LocalName:= localName;
  _QName:= qName;
  _Atts:= atts;
  if (Assigned(FOnStartElement)) then
    FOnStartElement(Self, _Uri, _LocalName, _QName, _Atts, Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.startElement(_Uri, _LocalName, _QName, _Atts);
end;

procedure TSAXCustomFilter.endElement(const uri, localName,
  qName: SAXString);
var Filtered : Boolean;
    _Uri, _LocalName, _QName : SAXString;
begin
  Filtered:= False;
  _Uri:= uri;
  _LocalName:= localName;
  _QName:= qName;
  if (Assigned(FOnEndElement)) then
    FOnEndElement(Self, _Uri, _LocalName, _QName, Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.endElement(_Uri, _LocalName, _QName);
end;

procedure TSAXCustomFilter.characters(const ch : SAXString);
var Filtered : Boolean;
    _Ch : SAXString;
begin
  Filtered:= False;
  _Ch:= ch;
  if (Assigned(FOnCharacters)) then
    FOnCharacters(Self, _Ch, Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.characters(_Ch);
end;

procedure TSAXCustomFilter.ignorableWhitespace(const ch : SAXString);
var Filtered : Boolean;
    _Ch : SAXString;
begin
  Filtered:= False;
  _Ch:= ch;
  if (Assigned(FOnIgnorableWhitespace)) then
    FOnIgnorableWhitespace(Self, _Ch, Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.ignorableWhitespace(_Ch);
end;

procedure TSAXCustomFilter.processingInstruction(const target,
  data : SAXString);
var Filtered : Boolean;
    _Target, _Data : SAXString;
begin
  Filtered:= False;
  _Target:= target;
  _Data:= data;
  if (Assigned(FOnProcessingInstruction)) then
    FOnProcessingInstruction(Self, _Target, _Data, Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.processingInstruction(_Target, _Data);
end;

procedure TSAXCustomFilter.skippedEntity(const name : SAXString);
var Filtered : Boolean;
    _Name : SAXString;
begin
  Filtered:= False;
  _Name:= name;
  if (Assigned(FOnSkippedEntity)) then
    FOnSkippedEntity(Self, _Name, Filtered);
  if (not Filtered) then
    if (Assigned(FContentHandler)) then
      FContentHandler.skippedEntity(_Name);
end;

procedure TSAXCustomFilter.warning(const e : ISAXParseError);
var Filtered : Boolean;
    _E : ISAXParseError;
begin
  Filtered:= False;
  _E:= e;
  if (Assigned(FOnWarning)) then
    FOnWarning(Self, _E, Filtered);
  if (not Filtered) then
    if (Assigned(FErrorHandler)) then
      FErrorHandler.warning(_E);
end;

procedure TSAXCustomFilter.error(const e : ISAXParseError);
var Filtered : Boolean;
    _E : ISAXParseError;
begin
  Filtered:= False;
  _E:= e;
  if (Assigned(FOnError)) then
    FOnError(Self, _E, Filtered);
  if (not Filtered) then
    if (Assigned(FErrorHandler)) then
      FErrorHandler.error(_E);
end;

procedure TSAXCustomFilter.fatalError(const e : ISAXParseError);
var Filtered : Boolean;
    _E : ISAXParseError;
begin
  Filtered:= False;
  _E:= e;
  if (Assigned(FOnFatalError)) then
    FOnFatalError(Self, _E, Filtered);
  if (not Filtered) then
    if (Assigned(FErrorHandler)) then
      FErrorHandler.fatalError(_E);
end;

procedure TSAXCustomFilter.startDTD(const name, publicId, systemId : SAXString);
var Filtered : Boolean;
    _Name, _PublicId, _SystemId : SAXString;
begin
  Filtered:= False;
  _Name:= name;
  _PublicId:= publicId;
  _SystemId:= systemId;
  if (Assigned(FOnStartDTD)) then
    FOnStartDTD(Self, _Name, _PublicId, _SystemId, Filtered);
  if (not Filtered) then
    if (Assigned(FLexicalHandler)) then
      FLexicalHandler.startDTD(_Name, _PublicId, _SystemId);
end;

procedure TSAXCustomFilter.endDTD();
var Filtered : Boolean;
begin
  Filtered:= False;
  if (Assigned(FOnEndDTD)) then
    FOnEndDTD(Self, Filtered);
  if (not Filtered) then
    if (Assigned(FLexicalHandler)) then
      FLexicalHandler.endDTD;
end;

procedure TSAXCustomFilter.startEntity(const name : SAXString);
var Filtered : Boolean;
    _Name : SAXString;
begin
  Filtered:= False;
  _Name:= name;
  if (Assigned(FOnStartEntity)) then
    FOnStartEntity(Self, _Name, Filtered);
  if (not Filtered) then
    if (Assigned(FLexicalHandler)) then
      FLexicalHandler.startEntity(_Name);
end;

procedure TSAXCustomFilter.endEntity(const name : SAXString);
var Filtered : Boolean;
    _Name : SAXString;
begin
  Filtered:= False;
  _Name:= name;
  if (Assigned(FOnEndEntity)) then
    FOnEndEntity(Self, _Name, Filtered);
  if (not Filtered) then
    if (Assigned(FLexicalHandler)) then
      FLexicalHandler.endEntity(_Name);
end;

procedure TSAXCustomFilter.startCDATA();
var Filtered : Boolean;
begin
  Filtered:= False;
  if (Assigned(FOnStartCDATA)) then
    FOnStartCDATA(Self, Filtered);
  if (not Filtered) then
    if (Assigned(FLexicalHandler)) then
      FLexicalHandler.startCDATA;
end;

procedure TSAXCustomFilter.endCDATA();
var Filtered : Boolean;
begin
  Filtered:= False;
  if (Assigned(FOnEndCDATA)) then
    FOnEndCDATA(Self, Filtered);
  if (not Filtered) then
    if (Assigned(FLexicalHandler)) then
      FLexicalHandler.endCDATA;
end;

procedure TSAXCustomFilter.comment(const ch : SAXString);
var Filtered : Boolean;
    _Ch : SAXString;
begin
  Filtered:= False;
  _Ch:= ch;
  if (Assigned(FOnComment)) then
    FOnComment(Self, _Ch, Filtered);
  if (not Filtered) then
    if (Assigned(FLexicalHandler)) then
      FLexicalHandler.comment(_Ch);
end;

procedure TSAXCustomFilter.elementDecl(const name, model : SAXString);
var Filtered : Boolean;
    _Name, _Model : SAXString;
begin
  Filtered:= False;
  _Name:= name;
  _Model:= model;
  if (Assigned(FOnElementDecl)) then
    FOnElementDecl(Self, _Name, _Model, Filtered);
  if (not Filtered) then
    if (Assigned(FDeclHandler)) then
      FDeclHandler.elementDecl(_Name, _Model);
end;

procedure TSAXCustomFilter.attributeDecl(const eName, aName, attrType, mode,
  value: SAXString);
var Filtered : Boolean;
    _EName, _AName, _AttrType, _Mode, _Value : SAXString;
begin
  Filtered:= False;
  _EName:= eName;
  _AName:= aName;
  _AttrType:= attrType;
  _Mode:= mode;
  _Value:= value;
  if (Assigned(FOnAttributeDecl)) then
    FOnAttributeDecl(Self, _EName, _AName, _AttrType, _Mode, _Value, Filtered);
  if (not Filtered) then
    if (Assigned(FDeclHandler)) then
      FDeclHandler.attributeDecl(_EName, _AName, _AttrType, _Mode, _Value);
end;

procedure TSAXCustomFilter.internalEntityDecl(const name, value : SAXString);
var Filtered : Boolean;
    _Name, _Value : SAXString;
begin
  Filtered:= False;
  _Name:= name;
  _Value:= Value;
  if (Assigned(FOnInternalEntityDecl)) then
    FOnInternalEntityDecl(Self, _Name, _Value, Filtered);
  if (not Filtered) then
    if (Assigned(FDeclHandler)) then
      FDeclHandler.internalEntityDecl(_Name, _Value);
end;

procedure TSAXCustomFilter.externalEntityDecl(const name, publicId, systemId : SAXString);
var Filtered : Boolean;
    _Name, _PublicId, _SystemId : SAXString;
begin
  Filtered:= False;
  _Name:= name;
  _PublicId:= publicId;
  _SystemId:= systemId;
  if (Assigned(FOnExternalEntityDecl)) then
    FOnExternalEntityDecl(Self, _Name, _PublicId, _SystemId, Filtered);
  if (not Filtered) then
    if (Assigned(FDeclHandler)) then
      FDeclHandler.externalEntityDecl(_Name, _PublicId, _SystemId);
end;

{ TSAXSplitter }

function TSAXSplitter.getContentHandler: TContentHandler;
begin
  Result:= FContentHandler;
end;

function TSAXSplitter.getDeclHandler: TDeclHandler;
begin
  Result:= FDeclHandler;
end;

function TSAXSplitter.getDTDHandler: TDTDHandler;
begin
  Result:= FDTDHandler;
end;

function TSAXSplitter.getEntityResolver: TEntityResolver;
begin
  Result:= FEntityResolver;
end;

function TSAXSplitter.getErrorHandler: TErrorHandler;
begin
  Result:= FErrorHandler;
end;

function TSAXSplitter.getLexicalHandler: TLexicalHandler;
begin
  Result:= FLexicalHandler;
end;

function TSAXSplitter.getNextContentHandler: TContentHandler;
begin
  Result:= FNextContentHandler;
end;

function TSAXSplitter.getNextDeclHandler: TDeclHandler;
begin
  Result:= FNextDeclHandler;
end;

function TSAXSplitter.getNextDTDHandler: TDTDHandler;
begin
  Result:= FNextDTDHandler;
end;

function TSAXSplitter.getNextEntityResolver: TEntityResolver;
begin
  Result:= FNextEntityResolver;
end;

function TSAXSplitter.getNextErrorHandler: TErrorHandler;
begin
  Result:= FNextErrorHandler;
end;

function TSAXSplitter.getNextLexicalHandler: TLexicalHandler;
begin
  Result:= FNextLexicalHandler;
end;

procedure TSAXSplitter.setContentHandler(
  const Value: TContentHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FContentHandler, opRemove);
  FContentHandler := Value;
  ReferenceInterface(FContentHandler, opInsert);
  {$ELSE}
  FContentHandler := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setDeclHandler(const Value: TDeclHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FDeclHandler, opRemove);
  FDeclHandler := Value;
  ReferenceInterface(FDeclHandler, opInsert);
  {$ELSE}
  FDeclHandler := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setDTDHandler(const Value: TDTDHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FDTDHandler, opRemove);
  FDTDHandler := Value;
  ReferenceInterface(FDTDHandler, opInsert);
  {$ELSE}
  FDTDHandler := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setEntityResolver(
  const Value: TEntityResolver);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FEntityResolver, opRemove);
  FEntityResolver := Value;
  ReferenceInterface(FEntityResolver, opInsert);
  {$ELSE}
  FEntityResolver := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setErrorHandler(const Value: TErrorHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FErrorHandler, opRemove);
  FErrorHandler := Value;
  ReferenceInterface(FErrorHandler, opInsert);
  {$ELSE}
  FErrorHandler := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setLexicalHandler(
  const Value: TLexicalHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FLexicalHandler, opRemove);
  FLexicalHandler := Value;
  ReferenceInterface(FLexicalHandler, opInsert);
  {$ELSE}
  FLexicalHandler := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setNextContentHandler(
  const Value: TContentHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FNextContentHandler, opRemove);
  FNextContentHandler := Value;
  ReferenceInterface(FNextContentHandler, opInsert);
  {$ELSE}
  FNextContentHandler := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setNextDeclHandler(const Value: TDeclHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FNextDeclHandler, opRemove);
  FNextDeclHandler := Value;
  ReferenceInterface(FNextDeclHandler, opInsert);
  {$ELSE}
  FNextDeclHandler := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setNextDTDHandler(const Value: TDTDHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FNextDTDHandler, opRemove);
  FNextDTDHandler := Value;
  ReferenceInterface(FNextDTDHandler, opInsert);
  {$ELSE}
  FNextDTDHandler := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setNextEntityResolver(
  const Value: TEntityResolver);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FNextEntityResolver, opRemove);
  FNextEntityResolver := Value;
  ReferenceInterface(FNextEntityResolver, opInsert);
  {$ELSE}
  FNextEntityResolver := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setNextErrorHandler(
  const Value: TErrorHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FNextErrorHandler, opRemove);
  FNextErrorHandler := Value;
  ReferenceInterface(FNextErrorHandler, opInsert);
  {$ELSE}
  FNextErrorHandler := Value;
  {$ENDIF}
end;

procedure TSAXSplitter.setNextLexicalHandler(
  const Value: TLexicalHandler);
begin
  {$IFDEF DELPHI6_UP}
  ReferenceInterface(FNextLexicalHandler, opRemove);
  FNextLexicalHandler := Value;
  ReferenceInterface(FNextLexicalHandler, opInsert);
  {$ELSE}
  FNextLexicalHandler := Value;
  {$ENDIF}
end;

function TSAXSplitter.resolveEntity(const publicId,
  systemId : SAXString) : IInputSource;
begin
  if (Assigned(FEntityResolver)) then
    Result:= FEntityResolver.resolveEntity(publicId, systemId);
  if (Assigned(FNextEntityResolver)) then
    Result:= FNextEntityResolver.resolveEntity(publicId, systemId);
end;

procedure TSAXSplitter.notationDecl(const name, publicId, systemId : SAXString);
begin
  if (Assigned(FDtdHandler)) then
    FDtdHandler.notationDecl(name, publicId, systemId);
  if (Assigned(FNextDtdHandler)) then
    FNextDtdHandler.notationDecl(name, publicId, systemId);
end;

procedure TSAXSplitter.unparsedEntityDecl(const name, publicId, systemId,
  notationName : SAXString);
begin
  if (Assigned(FDtdHandler)) then
    FDtdHandler.unparsedEntityDecl(name, publicId, systemId, notationName);
  if (Assigned(FNextDtdHandler)) then
    FNextDtdHandler.unparsedEntityDecl(name, publicId, systemId, notationName);
end;

procedure TSAXSplitter.setDocumentLocator(const Locator: ILocator);
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.setDocumentLocator(Locator);
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.setDocumentLocator(Locator);
end;

procedure TSAXSplitter.startDocument();
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.startDocument();
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.startDocument();
end;

procedure TSAXSplitter.endDocument();
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.endDocument();
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.endDocument();
end;

procedure TSAXSplitter.startPrefixMapping(const prefix, uri : SAXString);
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.startPrefixMapping(prefix, uri);
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.startPrefixMapping(prefix, uri);
end;

procedure TSAXSplitter.endPrefixMapping(const prefix : SAXString);
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.endPrefixMapping(prefix);
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.endPrefixMapping(prefix);
end;

procedure TSAXSplitter.startElement(const uri, localName, qName: SAXString;
  const atts: IAttributes);
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.startElement(uri, localName, qName, atts);
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.startElement(uri, localName, qName, atts);
end;

procedure TSAXSplitter.endElement(const uri, localName,
  qName: SAXString);
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.endElement(uri, localName, qName);
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.endElement(uri, localName, qName);
end;

procedure TSAXSplitter.characters(const ch : SAXString);
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.characters(ch);
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.characters(ch);
end;

procedure TSAXSplitter.ignorableWhitespace(const ch : SAXString);
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.ignorableWhitespace(ch);
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.ignorableWhitespace(ch);
end;

procedure TSAXSplitter.processingInstruction(const target, data : SAXString);
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.processingInstruction(target, data);
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.processingInstruction(target, data);
end;

procedure TSAXSplitter.skippedEntity(const name : SAXString);
begin
  if (Assigned(FContentHandler)) then
    FContentHandler.skippedEntity(name);
  if (Assigned(FNextContentHandler)) then
    FNextContentHandler.skippedEntity(name);
end;

procedure TSAXSplitter.warning(const e : ISAXParseError);
begin
  if (Assigned(FErrorHandler)) then
    FErrorHandler.warning(e);
  if (Assigned(FNextErrorHandler)) then
    FNextErrorHandler.warning(e);
end;

procedure TSAXSplitter.error(const e : ISAXParseError);
begin
  if (Assigned(FErrorHandler)) then
    FErrorHandler.error(e);
  if (Assigned(FNextErrorHandler)) then
    FNextErrorHandler.error(e);
end;

procedure TSAXSplitter.fatalError(const e : ISAXParseError);
begin
  if (Assigned(FErrorHandler)) then
    FErrorHandler.fatalError(e);
  if (Assigned(FNextErrorHandler)) then
    FNextErrorHandler.fatalError(e);
end;

procedure TSAXSplitter.startDTD(const name, publicId, systemId : SAXString);
begin
  if (Assigned(FLexicalHandler)) then
    FLexicalHandler.startDTD(name, publicId, systemId);
  if (Assigned(FNextLexicalHandler)) then
    FNextLexicalHandler.startDTD(name, publicId, systemId);
end;

procedure TSAXSplitter.endDTD();
begin
  if (Assigned(FLexicalHandler)) then
    FLexicalHandler.endDTD();
  if (Assigned(FNextLexicalHandler)) then
    FNextLexicalHandler.endDTD();
end;

procedure TSAXSplitter.startEntity(const name : SAXString);
begin
  if (Assigned(FLexicalHandler)) then
    FLexicalHandler.startEntity(name);
  if (Assigned(FNextLexicalHandler)) then
    FNextLexicalHandler.startEntity(name);
end;

procedure TSAXSplitter.endEntity(const name : SAXString);
begin
  if (Assigned(FLexicalHandler)) then
    FLexicalHandler.endEntity(name);
  if (Assigned(FNextLexicalHandler)) then
    FNextLexicalHandler.endEntity(name);
end;

procedure TSAXSplitter.startCDATA();
begin
  if (Assigned(FLexicalHandler)) then
    FLexicalHandler.startCDATA();
  if (Assigned(FNextLexicalHandler)) then
    FNextLexicalHandler.startCDATA();
end;

procedure TSAXSplitter.endCDATA();
begin
  if (Assigned(FLexicalHandler)) then
    FLexicalHandler.endCDATA();
  if (Assigned(FNextLexicalHandler)) then
    FNextLexicalHandler.endCDATA();
end;

procedure TSAXSplitter.comment(const ch : SAXString);
begin
  if (Assigned(FLexicalHandler)) then
    FLexicalHandler.comment(ch);
  if (Assigned(FNextLexicalHandler)) then
    FNextLexicalHandler.comment(ch);
end;

procedure TSAXSplitter.elementDecl(const name, model : SAXString);
begin
  if (Assigned(FDeclHandler)) then
    FDeclHandler.elementDecl(name, model);
  if (Assigned(FNextDeclHandler)) then
    FNextDeclHandler.elementDecl(name, model);
end;

procedure TSAXSplitter.attributeDecl(const eName, aName, attrType, mode,
  value: SAXString);
begin
  if (Assigned(FDeclHandler)) then
    FDeclHandler.attributeDecl(eName, aName, attrType, mode, value);
  if (Assigned(FNextDeclHandler)) then
    FNextDeclHandler.attributeDecl(eName, aName, attrType, mode, value);
end;

procedure TSAXSplitter.internalEntityDecl(const name, value : SAXString);
begin
  if (Assigned(FDeclHandler)) then
    FDeclHandler.internalEntityDecl(name, value);
  if (Assigned(FNextDeclHandler)) then
    FNextDeclHandler.internalEntityDecl(name, value);
end;

procedure TSAXSplitter.externalEntityDecl(const name, publicId, systemId : SAXString);
begin
  if (Assigned(FDeclHandler)) then
    FDeclHandler.externalEntityDecl(name, publicId, systemId);
  if (Assigned(FNextDeclHandler)) then
    FNextDeclHandler.externalEntityDecl(name, publicId, systemId);
end;

end.
