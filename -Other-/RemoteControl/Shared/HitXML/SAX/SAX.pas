// SAX for Pascal, Simple API for XML Interfaces in Pascal.
// Ver 1.1 July 4, 2003
// http://xml.defined.net/SAX (this will change!)
// Based on http://www.saxproject.org/
// No warranty; no copyright -- use this as you will.
unit SAX;

interface

{$I SAX.inc}

uses Classes, SysUtils;

type


  // Char/String Classes

  {$IFDEF SAX_SEPARATETYPES}
  {$IFDEF SAX_WIDESTRINGS}
  SAXString = type WideString;
  SAXChar = type WideChar;
  PSAXChar = type PWideChar;
  {$ELSE}
  SAXString = type String;
  SAXChar = type Char;
  PSAXChar = type PChar;
  {$ENDIF}
  {$ELSE}
  {$IFDEF SAX_WIDESTRINGS}
  SAXString = WideString;
  SAXChar = WideChar;
  PSAXChar = PWideChar;
  {$ELSE}
  SAXString = String;
  SAXChar = Char;
  PSAXChar = PChar;
  {$ENDIF}
  {$ENDIF}

  SAXStringArray = array of SAXString;
  SAXCharArray = array of SAXChar;

const

  // Standard feature names
  Features                  = 'http://xml.org/sax/features/';
  NamespacesFeature         = SAXString(Features + 'namespaces');
  NamespacePrefixesFeature  = SAXString(Features + 'namespace-prefixes');
  ValidationFeature         = SAXString(Features + 'validation');
  ExternalGeneralFeature    = SAXString(Features + 'external-general-entities');
  ExternalParameterFeature  = SAXString(Features + 'external-parameter-entities');
  IsStandaloneFeature       = SAXString(Features + 'is-standalone');
  UseAttributes2Feature     = SAXString(Features + 'use-attributes2');
  UseLocator2Feature        = SAXString(Features + 'use-locator2');
  UseEntityResolver2Feature = SAXString(Features + 'use-entity-resolver2');
  ResolveDTDURIsFeature     = SAXString(Features + 'resolve-dtd-uris');
  LexicalParameterFeature   = SAXString(Features + 'lexical-handler/parameter-entities');

  // Standard property names
  Properties                = 'http://xml.org/sax/properties/';
  LexicalHandlerProperty    = SAXString(Properties + 'lexical-handler');
  DeclHandlerProperty       = SAXString(Properties + 'declaration-handler');
  XMLStringProperty         = SAXString(Properties + 'xml-string');
  DOMNodeProperty           = SAXString(Properties + 'dom-node');

  // XML Namespace URI
  XML_XMLNS = SAXString('http://www.w3.org/XML/1998/namespace');
  XML_NSDECL = SAXString('http://www.w3.org/xmlns/2000/');
  XML_NULLCHAR = SAXChar(#0);
  XML_COLON = SAXChar(':');
  XML_AMPERSAND = SAXChar('&');
  XML_SEMICOLON = SAXChar(';');
  XML_PERCENT = SAXChar('%');
  XML_CDATA = SAXString('CDATA');
  XML_DOCTYPESTART = SAXString('<!DOCTYPE');
  XML_DTDNAME = SAXString('[dtd]');

  {$IFDEF SAX_WIDESTRINGS}
  IID_IAttributes = '{B88C9D0F-1AFD-412A-B9EF-2313AB251CF1}';
  IID_IContentHandler = '{755DA957-795C-4888-B3D2-6FD4EA36B8EA}';
  IID_IDTDHandler = '{8CE723C8-EBC9-4FE9-97B5-CC70D4927476}';
  IID_IEntityResolver = '{FC8D2E95-DC19-4A3A-BADC-B8EEEA2E4D8C}';
  IID_IErrorHandler = '{F5157F20-8B53-489F-B41B-A67A41E3AF3F}';
  IID_ILocator = '{9D0C1A95-C9EA-40E4-B4A2-5E83FCC62CCF}';
  IID_IBaseXMLReader = '{08A49509-DA1B-4938-B3BB-0A23BCD8F437}';
  IID_IXMLReader = '{CA11495E-6F47-4C06-8A69-BEF7342BB81D}';
  IID_IXMLFilter = '{96CFADBE-A3C2-4940-A833-7E3784F176C0}';
  IID_IAttributeList = '{FD6FEB92-5496-44EB-BCE1-CB672BC806D1}';
  IID_IDocumentHandler = '{13640082-5BC7-495A-9751-4B9FB1EAC0C3}';
  IID_IParser = '{D0BB7E5A-A32A-496D-9EDC-AA4C9F6DD045}';
  IID_IProperty = '{2937C64B-FFC6-47BD-9D2C-B99F514CB0B3}';
  IID_IBooleanProperty = '{41AADAC0-20BF-482E-B350-6F0F41CFE114}';
  IID_IIntegerProperty = '{0F0A7E92-D039-40AA-A634-340631AE90A1}';
  IID_IInterfaceProperty = '{A3D3718E-9933-4928-81B8-5C4D20D220A2}';
  IID_IStringProperty = '{AE0E551F-57BB-4BAD-B9AB-68FD6F7DBF6E}';
  IID_ISAXError = '{D260FD87-1E6D-4FB0-910B-507409313871}';
  IID_ISAXParseError = '{AA08FEE9-4EB2-4030-834C-569E92B0AC28}';
  IID_ISAXNotRecognizedError = '{3C5582A1-0BDB-4573-A34E-36F114E85F00}';
  IID_ISAXNotSupportedError = '{22A27EC7-99D6-4F92-BAD7-66816BB36B14}';
  IID_ISAXIllegalStateError = '{562998E3-FBC4-49F3-A561-8A32111F84EE}';
  IID_ISAXIllegalArgumentError = '{E1AC9CEE-FBA9-4D5B-913B-813AEBFE5946}';
  IID_IInputSource = '{CFB5C09E-93E9-4DB1-BB60-1405E0B71D18}';
  IID_IStreamInputSource = '{DD6B99D0-AD5A-4A1A-94CD-23386E7A49FC}';
  {$ELSE}
  IID_IAttributes = '{B2D977A1-FF63-4EC9-9FEB-BC88C22BBB84}';
  IID_IContentHandler = '{CE75FA2A-70A0-4CC2-98E7-EB1626F1F830}';
  IID_IDTDHandler = '{6E96D2CD-7132-499B-AC53-8CAB627478A9}';
  IID_IEntityResolver = '{B509E6EA-1DA4-45AA-B084-9ABF8D27DAB7}';
  IID_IErrorHandler = '{6C442DD5-883C-4440-9454-FBC1182D98F4}';
  IID_ILocator = '{EAA15B91-AF8F-4607-B8DD-E4A32C49A240}';
  IID_IBaseXMLReader = '{E75AEE78-207D-4366-B917-C71B9B758EB2}';
  IID_IXMLReader = '{565DB109-0F5D-419A-B4EC-939F15EDF768}';
  IID_IXMLFilter = '{B2039F71-3266-4056-BBC6-E72DA723244F}';
  IID_IAttributeList = '{6061A5F3-A8AC-454F-A299-A74F7E4B96F8}';
  IID_IDocumentHandler = '{02DC9335-04EC-48ED-8055-875E5E8EF94A}';
  IID_IParser = '{A7A3F303-BF2F-4D20-AB37-0286EC06C357}';
  IID_IProperty = '{D6724650-417D-4A21-BE22-17B413380FC3}';
  IID_IBooleanProperty = '{E9F2B19A-9005-4426-A83A-189829722945}';
  IID_IIntegerProperty = '{F3FC7ABA-39ED-4952-936E-C2B551B9B8EC}';
  IID_IInterfaceProperty = '{ABC7FFC4-7FF7-4915-9D5B-E8B984EB4406}';
  IID_IStringProperty = '{6F888DEB-850B-4FE4-8D5F-12DDAEFC4D6D}';
  IID_ISAXError = '{E5A5BB50-AE5B-4DA2-A0C0-A446540F8B3B}';
  IID_ISAXParseError = '{ED98D309-8626-4E45-895E-AA5C1EFE02FE}';
  IID_ISAXNotRecognizedError = '{F79A6C38-010C-45D9-ADA1-6A58A32FC51F}';
  IID_ISAXNotSupportedError = '{00C4FEF1-400E-4191-AD0E-1F74F18B81C3}';
  IID_ISAXIllegalStateError = '{AC16CBBA-337A-495F-9CC6-2CFC333B1962}';
  IID_ISAXIllegalArgumentError = '{DDD514CA-B6FD-4850-8DA5-6517E24A89DE}';
  IID_IInputSource = '{DFAF4F74-8242-4E34-8067-8F92FE3A8755}';
  IID_IStreamInputSource = '{E395D618-AAA1-4B3A-BD76-E0AF98F52D2E}';
  {$ENDIF}

type

  // Default Interfaces
  IAttributes = interface;
  IContentHandler = interface;
  IDTDHandler = interface;
  IEntityResolver = interface;
  IErrorHandler = interface;
  ILocator = interface;
  IXMLReader = interface;
  IXMLFilter = interface;

  // Deprecated SAX 1 Interfaces
  IAttributeList = interface;
  IDocumentHandler = interface;
  IParser = interface;

  // Exception Classes
  ESAXException = class;
  ESAXParseException = class;
  ESAXNotRecognizedException = class;
  ESAXNotSupportedException = class;
  ESAXIllegalStateException = class;
  ESAXIllegalArgumentException = class;

  // Helper Interfaces
  IInputSource = interface;
  IStreamInputSource = interface;
  IProperty = interface;
  IBooleanProperty = interface;
  IIntegerProperty = interface;
  IInterfaceProperty = interface;
  IStringProperty = interface;
  ISAXError = interface;
  ISAXParseError = interface;
  ISAXNotRecognizedError = interface;
  ISAXNotSupportedError = interface;
  ISAXIllegalStateError = interface;
  ISAXIllegalArgumentError = interface;

  // Default Helper Classes
  TInputSource = class;
  TStreamInputSource = class;
  TSAXVendor = class;

  // Interface for a list of XML attributes.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This interface allows access to a list of attributes in
  // three different ways:</p>
  //
  // <ol>
  // <li>by attribute index;</li>
  // <li>by Namespace-qualified name; or</li>
  // <li>by qualified (prefixed) name.</li>
  // </ol>
  //
  // <p>The list will not contain attributes that were declared
  // #IMPLIED but not specified in the start tag.  It will also not
  // contain attributes used as Namespace declarations (xmlns*) unless
  // the <code>http://xml.org/sax/features/namespace-prefixes</code>
  // feature is set to <var>true</var> (it is <var>false</var> by
  // default).
  // Because SAX2 conforms to the original "Namespaces in XML"
  // recommendation, it normally does not
  // give namespace declaration attributes a namespace URI.</p>
  //
  // <p>Some SAX2 parsers may support using an optional feature flag
  // (<code>http://xml.org/sax/features/xmlns-uris</code>) to request that
  // those attributes be given URIs, conforming to a later
  // backwards-incompatible revision of that recommendation.
  // (The attribute's "local name" will be the prefix, or the empty
  // string when defining a default element namespace.)
  // For portability, handler code should always resolve that conflict,
  // rather than requiring parsers that can change the setting of that
  // feature flag.</p>
  //
  // <p>If the namespace-prefixes feature (see above) is <var>false</var>,
  // access by qualified name may not be available; if the
  // <code>http://xml.org/sax/features/namespaces</code>
  // feature is <var>false</var>, access by Namespace-qualified names
  // may not be available.</p>
  //
  // <p>This interface replaces the now-deprecated SAX1 (<a href="../SAX/IAttributeList.html">IAttributeList</a>)
  // interface, which does not contain Namespace support.
  // In addition to Namespace support, it adds the <var>getIndex</var>
  // methods (below).</p>
  //
  // <p>The order of attributes in the list is unspecified, and will
  // vary from implementation to implementation.</p>
  //
  // @since SAX 2.0
  // @see <a href="../SAXExt/IDeclHandler.html#attributeDecl">IDeclHandler.attributeDecl</a>
  IAttributes = interface(IUnknown)
    [IID_IAttributes]

    // Return the number of attributes in the list.
    //
    // <p>Once you know the number of attributes, you can iterate
    // through the list.</p>
    //
    // @return The number of attributes in the list.
    // @see <a href="../SAX/IAttributes.html#getURI.Integer">IAttributes.getURI(Integer)</a>
    // @see <a href="../SAX/IAttributes.html#getLocalName.Integer">IAttributes.getLocalName(Integer)</a>
    // @see <a href="../SAX/IAttributes.html#getQName.Integer">IAttributes.getQName(Integer)</a>
    // @see <a href="../SAX/IAttributes.html#getType.Integer">IAttributes.getType(Integer)</a>
    // @see <a href="../SAX/IAttributes.html#getValue.Integer">IAttributes.getValue(Integer)</a>
    function getLength() : Integer;

    // Look up an attribute's Namespace URI by index.
    //
    // @param index The attribute index (zero-based).
    // @return The Namespace URI, or the empty string if none
    //         is available or if the index is out of range.
    // @see <a href="../SAX/IAttributes.html#getLength">IAttributes.getLength</a>
    function getURI(index : Integer) : SAXString;

    // Look up an attribute's local name by index.
    //
    // @param index The attribute index (zero-based).
    // @return The local name, or the empty string if Namespace
    //         processing is not being performed or if the
    //         index is out of range.
    // @see <a href="../SAX/IAttributes.html#getLength">IAttributes.getLength</a>
    function getLocalName(index : Integer) : SAXString;

    //  Look up an attribute's XML 1.0 qualified name by index.
    //
    //  @param index The attribute index (zero-based).
    //  @return The XML 1.0 qualified name, or the empty string
    //          if none is available or if the index is out of
    //          range.
    // @see <a href="../SAX/IAttributes.html#getLength">IAttributes.getLength</a>
    function getQName(index : Integer) : SAXString;

    //  Look up an attribute's type by index.
    //
    //  <p>The attribute type is one of the strings "CDATA", "ID",
    //  "IDREF", "IDREFS", "NMTOKEN", "NMTOKENS", "ENTITY", "ENTITIES",
    //  or "NOTATION" (always in upper case).</p>
    //
    //  <p>If the parser has not read a declaration for the attribute,
    //  or if the parser does not report attribute types, then it must
    //  return the value "CDATA" as stated in the XML 1.0 Recommendation
    //  (clause 3.3.3, "Attribute-Value Normalization").</p>
    //
    //  <p>For an enumerated attribute that is not a notation, the
    //  parser will report the type as "NMTOKEN".</p>
    //
    //  @param index The attribute index (zero-based).
    //  @return The attribute's type as a string or an empty string if the
    //          index is out of range.
    // @see <a href="../SAX/IAttributes.html#getLength">IAttributes.getLength</a>
    function getType(index : Integer) : SAXString; overload;

    //  Look up an attribute's value by XML 1.0 qualified name.
    //
    //  <p>See <a href="../SAX/IAttributes.html#getValue.Integer">getValue(Integer)</a> for a description
    //  of the possible values.</p>
    //
    //  @param qName The XML 1.0 qualified name.
    //  @return The attribute value as a string, or an empty string if the
    //          attribute is not in the list or if qualified names
    //          are not available.
    function getType(const uri, localName : SAXString) : SAXString; overload;

    //  Look up an attribute's type by XML 1.0 qualified name.
    //
    //  <p>See <a href="../SAX/IAttributes.html#getType.Integer">getType(Integer)</a> for a description
    //  of the possible types.</p>
    //
    //  @param qName The XML 1.0 qualified name.
    //  @return The attribute type as a string, or an empty string if the
    //          attribute is not in the list or if qualified names
    //          are not available.
    function getType(const qName : SAXString) : SAXString; overload;

    //  Look up an attribute's value by index.
    //
    //  <p>If the attribute value is a list of tokens (IDREFS,
    //  ENTITIES, or NMTOKENS), the tokens will be concatenated
    //  into a single string with each token separated by a
    //  single space.</p>
    //
    //  @param index The attribute index (zero-based).
    //  @return The attribute's value as a string, or an empty string if the
    //          index is out of range.
    // @see <a href="../SAX/IAttributes.html#getLength">IAttributes.getLength</a>
    function getValue(index : Integer) : SAXString; overload;

    //  Look up an attribute's type by Namespace name.
    //
    //  <p>See <a href="../SAX/IAttributes.html#getType.Integer">getType(Integer)</a> for a description
    //  of the possible types.</p>
    //
    //  @param uri The Namespace URI, or the empty String if the
    //         name has no Namespace URI.
    //  @param localName The local name of the attribute.
    //  @return The attribute type as a string, or an empty string if the
    //          attribute is not in the list or if Namespace
    //          processing is not being performed.
    function getValue(const uri, localName : SAXString) : SAXString; overload;

    //  Look up an attribute's value by Namespace name.
    //
    //  <p>See <a href="../SAX/IAttributes.html#getValue.Integer">getValue(Integer)</a> for a description
    //  of the possible values.</p>
    //
    //  @param uri The Namespace URI, or the empty String if the
    //         name has no Namespace URI.
    //  @param localName The local name of the attribute.
    //  @return The attribute value as a string, or an empty string if the
    //          attribute is not in the list.
    function getValue(const qName : SAXString) : SAXString; overload;

    //  Look up the index of an attribute by Namespace name.
    //
    //  @param uri The Namespace URI, or the empty string if
    //         the name has no Namespace URI.
    //  @param localName The attribute's local name.
    //  @return The index of the attribute, or -1 if it does not
    //          appear in the list.
    function getIndex(const uri, localName : SAXString) : Integer; overload;

    //  Look up the index of an attribute by XML 1.0 qualified name.
    //
    //  @param qName The qualified (prefixed) name.
    //  @return The index of the attribute, or -1 if it does not
    //          appear in the list.
    function getIndex(const qName : SAXString) : Integer; overload;

    // Extension property to get the number of Attributes
    //
    // @return The number of attributes in the list.
    // @see <a href="../SAX/IAttributes.html#getLength">IAttributes.getLength</a>
    property Length : Integer
      read getLength;
  end;

  // Receive notification of the logical content of a document.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This is the main interface that most SAX applications
  // implement: if the application needs to be informed of basic parsing
  // events, it implements this interface and registers an instance with
  // the SAX parser using the <a href="../SAX/IXMLReader.html#setContentHandler">IXMLReader</a>
  // The parser uses the instance to report basic document-related events
  // like the start and end of elements and character data.</p>
  //
  // <p>The order of events in this interface is very important, and
  // mirrors the order of information in the document itself.  For
  // example, all of an element's content (character data, processing
  // instructions, and/or subelements) will appear, in order, between
  // the startElement event and the corresponding endElement event.</p>
  //
  // <p>This interface is similar to the now-deprecated SAX 1.0
  // DocumentHandler interface, but it adds support for Namespaces
  // and for reporting skipped entities (in non-validating XML
  // processors).</p>
  //
  // @since SAX 2.0
  // @see <a href="../SAX/IXMLReader.html">IXMLReader</a>
  // @see <a href="../SAX/IDTDHandler.html">IDTDHandler</a>
  // @see <a href="../SAX/IErrorHandler.html">IErrorHandler</a>
  IContentHandler = interface(IUnknown)
    [IID_IContentHandler]

    // Receive an object for locating the origin of SAX document events.
    //
    // <p>SAX parsers are strongly encouraged (though not absolutely
    // required) to supply a locator: if it does so, it must supply
    // the locator to the application by invoking this method before
    // invoking any of the other methods in the ContentHandler
    // interface.</p>
    //
    // <p>The locator allows the application to determine the end
    // position of any document-related event, even if the parser is
    // not reporting an error.  Typically, the application will
    // use this information for reporting its own errors (such as
    // character content that does not match an application's
    // business rules).  The information returned by the locator
    // is probably not sufficient for use with a search engine.</p>
    //
    // <p>Note that the locator will return correct information only
    // during the invocation SAX event callbacks after
    // <a href="../SAX/IContentHandler.html#startDocument">startDocument</a> returns and before
    // <a href="../SAX/IContentHandler.html#endDocument">endDocument</a> is called.  The
    // application should not attempt to use it at any other time.</p>
    //
    // @param locator An object that can return the location of
    //                any SAX document event.
    // @see <a href="../SAX/ILocator.html">ILocator</a>
    procedure setDocumentLocator(const locator: ILocator);

    // Receive notification of the beginning of a document.
    //
    // <p>The SAX parser will invoke this method only once, before any
    // other event callbacks (except for
    // <a href="../SAX/IContentHandler.html#setDocumentLocator">setDocumentLocator</a>).</p>
    //
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#endDocument">IContentHandler.endDocument</a>
    procedure startDocument();

    // Receive notification of the end of a document.
    //
    // <p>The SAX parser will invoke this method only once, and it will
    // be the last method invoked during the parse.  The parser shall
    // not invoke this method until it has either abandoned parsing
    // (because of an unrecoverable error) or reached the end of
    // input.</p>
    //
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#startDocument">IContentHandler.startDocument</a>
    procedure endDocument();

    // Begin the scope of a prefix-URI Namespace mapping.
    //
    // <p>The information from this event is not necessary for
    // normal Namespace processing: the SAX XML reader will
    // automatically replace prefixes for element and attribute
    // names when the <code>http://xml.org/sax/features/namespaces</code>
    // feature is <var>true</var> (the default).</p>
    //
    // <p>There are cases, however, when applications need to
    // use prefixes in character data or in attribute values,
    // where they cannot safely be expanded automatically; the
    // start/endPrefixMapping event supplies the information
    // to the application to expand prefixes in those contexts
    // itself, if necessary.</p>
    //
    // <p>Note that start/endPrefixMapping events are not
    // guaranteed to be properly nested relative to each other:
    // all startPrefixMapping events will occur immediately before the
    // corresponding <a href="../SAX/IContentHandler.html#startElement">startElement</a> event,
    // and all <a href="../SAX/IContentHandler.html#endPrefixMapping">endPrefixMapping</a>
    // events will occur immediately after the corresponding
    // <a href="../SAX/IContentHandler.html#endElement">endElement</a> event
    // but their order is not otherwise
    // guaranteed.</p>
    //
    // <p>There should never be start/endPrefixMapping events for the
    // "xml" prefix, since it is predeclared and immutable.</p>
    //
    // @param prefix The Namespace prefix being declared.
    //               An empty string is used for the default element namespace,
    //               which has no prefix.
    // @param uri The Namespace URI the prefix is mapped to.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    // @see <a href="../SAX/IContentHandler.html#endPrefixMapping">IContentHandler.endPrefixMapping</a>
    // @see <a href="../SAX/IContentHandler.html#startElement">IContentHandler.startElement</a>
    procedure startPrefixMapping(const prefix, uri : SAXString);

    // End the scope of a prefix-URI mapping.
    //
    // <p>See <a href="../SAX/IContentHandler.html#startPrefixMapping">startPrefixMapping</a> for
    // details.  These events will always occur immediately after the
    // corresponding <a href="../SAX/IContentHandler.html#endElement">endElement</a> event, but the order of
    // <a href="../SAX/IContentHandler.html#endPrefixMapping">endPrefixMapping</a> events is not otherwise
    // guaranteed.</p>
    //
    // @param prefix The prefix that was being mapped.
    //               Use the empty string when a default mapping scope ends.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    // @see <a href="../SAX/IContentHandler.html#startPrefixMapping">IContentHandler.startPrefixMapping</a>
    // @see <a href="../SAX/IContentHandler.html#endElement">IContentHandler.endElement</a>
    procedure endPrefixMapping(const prefix : SAXString);

    // Receive notification of the beginning of an element.
    //
    // <p>The Parser will invoke this method at the beginning of every
    // element in the XML document; there will be a corresponding
    // <a href="../SAX/IContentHandler.html#endElement">endElement</a> event for every startElement event
    // (even when the element is empty). All of the element's content will be
    // reported, in order, before the corresponding endElement
    // event.</p>
    //
    // <p>This event allows up to three name components for each
    // element:</p>
    //
    // <ol>
    // <li>the Namespace URI;</li>
    // <li>the local name; and</li>
    // <li>the qualified (prefixed) name.</li>
    // </ol>
    //
    // <p>Any or all of these may be provided, depending on the
    // values of the <var>http://xml.org/sax/features/namespaces</var>
    // and the <var>http://xml.org/sax/features/namespace-prefixes</var>
    // properties:</p>
    //
    // <ul>
    // <li>the Namespace URI and local name are required when
    // the namespaces property is <var>true</var> (the default), and are
    // optional when the namespaces property is <var>false</var> (if one is
    // specified, both must be);</li>
    // <li>the qualified name is required when the namespace-prefixes property
    // is <var>true</var>, and is optional when the namespace-prefixes property
    // is <var>false</var> (the default).</li>
    // </ul>
    //
    // <p>Note that the attribute list provided will contain only
    // attributes with explicit values (specified or defaulted):
    // #IMPLIED attributes will be omitted.  The attribute list
    // will contain attributes used for Namespace declarations
    // (xmlns* attributes) only if the
    // <code>http://xml.org/sax/features/namespace-prefixes</code>
    // property is true (it is false by default, and support for a
    // true value is optional).</p>
    //
    // <p>Like <a href="../SAX/IContentHandler.html#characters">characters</a>, attribute values may have
    // characters that need more than one <code>char</code> value.</p>
    //
    // @param uri The Namespace URI, or the empty string if the
    //        element has no Namespace URI or if Namespace
    //        processing is not being performed.
    // @param localName The local name (without prefix), or the
    //        empty string if Namespace processing is not being
    //        performed.
    // @param qName The qualified name (with prefix), or the
    //        empty string if qualified names are not available.
    // @param atts The attributes attached to the element.  If
    //        there are no attributes, it shall be an empty
    //        IAttributes object.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#endElement">IContentHandler.endElement</a>
    // @see <a href="../SAX/IAttributes.html">IAttributes</a>
    procedure startElement(const uri, localName, qName: SAXString;
      const atts: IAttributes);

    // Receive notification of the end of an element.
    //
    // <p>The SAX parser will invoke this method at the end of every
    // element in the XML document; there will be a corresponding
    // <a href="../SAX/IContentHandler.html#startElement">startElement</a> event for every endElement
    // event (even when the element is empty).</p>
    //
    // <p>For information on the names, see startElement.</p>
    //
    // @param uri The Namespace URI, or the empty string if the
    //        element has no Namespace URI or if Namespace
    //        processing is not being performed.
    // @param localName The local name (without prefix), or the
    //        empty string if Namespace processing is not being
    //        performed.
    // @param qName The qualified XML 1.0 name (with prefix), or the
    //        empty string if qualified names are not available.
    // @exception ESAXException Any SAX exception.
    procedure endElement(const uri, localName, qName: SAXString);

    // Receive notification of character data.
    //
    // <p>The Parser will call this method to report each chunk of
    // character data.  SAX parsers may return all contiguous character
    // data in a single chunk, or they may split it into several
    // chunks; however, all of the characters in any single event
    // must come from the same external entity so that the Locator
    // provides useful information.</p>
    //
    // <p>The application must not attempt to read from the array
    // outside of the specified range.</p>
    //
    // <p>Individual characters may consist of more than one Java
    // <code>char</code> value.  There are two important cases where this
    // happens, because characters can't be represented in just sixteen bits.
    // In one case, characters are represented in a <em>Surrogate Pair</em>,
    // using two special Unicode values. Such characters are in the so-called
    // "Astral Planes", with a code point above U+FFFF.  A second case involves
    // composite characters, such as a base character combining with one or
    // more accent characters. </p>
    //
    // <p> Your code should not assume that algorithms using
    // <code>char</code>-at-a-time idioms will be working in character
    // units; in some cases they will split characters.  This is relevant
    // wherever XML permits arbitrary characters, such as attribute values,
    // processing instruction data, and comments as well as in data reported
    // from this method.  It's also generally relevant whenever Java code
    // manipulates internationalized text; the issue isn't unique to XML.</p>
    //
    // <p>Note that some parsers will report whitespace in element
    // content using the <a href="../SAX/IContentHandler.html#ignorableWhitespace">ignorableWhitespace</a>
    // method rather than this one (validating parsers <em>must</em>
    // do so).</p>
    //
    // @param ch The characters from the XML document.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#ignorableWhitespace">IContentHandler.ignorableWhitespace</a>
    // @see <a href="../SAX/ILocator.html">ILocator</a>
    procedure characters(const ch : SAXString);

    // Receive notification of ignorable whitespace in element content.
    //
    // <p>Validating Parsers must use this method to report each chunk
    // of whitespace in element content (see the W3C XML 1.0 recommendation,
    // section 2.10): non-validating parsers may also use this method
    // if they are capable of parsing and using content models.</p>
    //
    // <p>SAX parsers may return all contiguous whitespace in a single
    // chunk, or they may split it into several chunks; however, all of
    // the characters in any single event must come from the same
    // external entity, so that the Locator provides useful
    // information.</p>
    //
    // <p>The application must not attempt to read from the array
    // outside of the specified range.</p>
    //
    // @param ch The characters from the XML document.
    // @exception ESAXException Any SAX exception.

    // @see <a href="../SAX/IContentHandler.html#characters">IContentHandler.characters</a>
    procedure ignorableWhitespace(const ch : SAXString);

    // Receive notification of a processing instruction.
    //
    // <p>The Parser will invoke this method once for each processing
    // instruction found: note that processing instructions may occur
    // before or after the main document element.</p>
    //
    // <p>A SAX parser must never report an XML declaration (XML 1.0,
    // section 2.8) or a text declaration (XML 1.0, section 4.3.1)
    // using this method.</p>
    //
    // <p>Like <a href="../SAX/IContentHandler.html#characters">characters</a>, processing instruction
    // data may have characters that need more than one <code>char</code>
    // value. </p>
    //
    // @param target The processing instruction target.
    // @param data The processing instruction data, or the empty string
    //        if none was supplied.  The data does not include any
    //        whitespace separating it from the target.
    // @exception ESAXException Any SAX exception.
    procedure processingInstruction(const target, data : SAXString);

    // Receive notification of a skipped entity.
    // This is not called for entity references within markup constructs 
    // such as element start tags or markup declarations.  (The XML
    // recommendation requires reporting skipped external entities.
    // SAX also reports internal entity expansion/non-expansion, except
    // within markup constructs.)
    //
    // <p>The Parser will invoke this method each time the entity is
    // skipped.  Non-validating processors may skip entities if they
    // have not seen the declarations (because, for example, the
    // entity was declared in an external DTD subset).  All processors
    // may skip external entities, depending on the values of the
    // <code>http://xml.org/sax/features/external-general-entities</code>
    // and the
    // <code>http://xml.org/sax/features/external-parameter-entities</code>
    // properties.</p>
    //
    // @param name The name of the skipped entity.  If it is a
    //        parameter entity, the name will begin with '%', and if
    //        it is the external DTD subset, it will be the string
    //        "[dtd]".
    // @exception ESAXException Any SAX exception.
    procedure skippedEntity(const name : SAXString);

  end;

  // Receive notification of basic DTD-related events.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>If a SAX application needs information about notations and
  // unparsed entities, then the application implements this
  // interface and registers an instance with the SAX parser using
  // the parser's setDTDHandler method.  The parser uses the
  // instance to report notation and unparsed entity declarations to
  // the application.</p>
  //
  // <p>Note that this interface includes only those DTD events that
  // the XML recommendation <em>requires</em> processors to report:
  // notation and unparsed entity declarations.</p>
  //
  // <p>The SAX parser may report these events in any order, regardless
  // of the order in which the notations and unparsed entities were
  // declared; however, all DTD events must be reported after the
  // document handler's startDocument event, and before the first
  // startElement event.
  // If the <a href="../SAX/ILexicalHandler.html">ILexicalHandler</a> is
  // used, these events must also be reported before the endDTD event.)</p>
  //
  // <p>It is up to the application to store the information for
  // future use (perhaps in a hash table or object tree).
  // If the application encounters attributes of type "NOTATION",
  // "ENTITY", or "ENTITIES", it can use the information that it
  // obtained through this interface to find the entity and/or
  // notation corresponding with the attribute value.</p>
  //
  // @since SAX 1.0
  // @see <a href="../SAX/IXMLReader.html#setDTDHandler">IXMLReader.setDTDHandler</a>
  IDTDHandler = interface(IUnknown)
    [IID_IDTDHandler]

    // Receive notification of a notation declaration event.
    //
    // <p>It is up to the application to record the notation for later
    // reference, if necessary;
    // notations may appear as attribute values and in unparsed entity
    // declarations, and are sometime used with processing instruction
    // target names.</p>
    //
    // <p>At least one of publicId and systemId must be non-empty.
    // If a system identifier is present, and it is a URL, the SAX
    // parser must resolve it fully before passing it to the
    // application through this event.</p>
    //
    // <p>There is no guarantee that the notation declaration will be
    // reported before any unparsed entities that use it.</p>
    //
    // @param name The notation name.
    // @param publicId The notation's public identifier, or empty if
    //        none was given.
    // @param systemId The notation's system identifier, or empty if
    //        none was given.
    // @exception ESAXException Any SAX exception.
    
    // @see <a href="../SAX/IDTDHandler.html#unparsedEntityDecl">IDTDHandler.unparsedEntityDecl</a>
    // @see <a href="../SAX/IAttributes.html">IAttributes</a>
    procedure notationDecl(const name, publicId, systemId : SAXString);

    // Receive notification of an unparsed entity declaration event.
    //
    // <p>Note that the notation name corresponds to a notation
    // reported by the <a href="../SAX/IDTDHandler.html#notationDecl">notationDecl</a> event.
    // It is up to the application to record the entity for later
    // reference, if necessary;
    // unparsed entities may appear as attribute values.</p>
    //
    // <p>If the system identifier is a URL, the parser must resolve it
    // fully before passing it to the application.</p>
    //
    // @exception ESAXException Any SAX exception.
    
    // @param name The unparsed entity's name.
    // @param publicId The entity's public identifier, or empty if none
    //        was given.
    // @param systemId The entity's system identifier.
    // @param notationName The name of the associated notation.
    // @see <a href="../SAX/IDTDHandler.html#notationDecl">IDTDHandler.notationDecl</a>
    // @see <a href="../SAX/IAttributes.html">IAttributes</a>
    procedure unparsedEntityDecl(const name, publicId, systemId,
      notationName : SAXString);

  end;

  // Basic interface for resolving entities.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>If a SAX application needs to implement customized handling
  // for external entities, it must implement this interface and
  // register an instance with the SAX driver using the
  // <a href="../SAX/IXMLReader.html#setEntityResolver">setEntityResolver</a>
  // method.</p>
  //
  // <p>The XML reader will then allow the application to intercept any
  // external entities (including the external DTD subset and external
  // parameter entities, if any) before including them.</p>
  //
  // <p>Many SAX applications will not need to implement this interface,
  // but it will be especially useful for applications that build
  // XML documents from databases or other specialised input sources,
  // or for applications that use URI types other than URLs.</p>
  //
  // <p>The following resolver would provide the application
  // with a special character stream for the entity with the system
  // identifier "http://www.myhost.com/today":</p>
  //
  // <pre>
  // TMyResolver = class(TInterfacedObject, IEntityResolver)
  // public
  //   function resolveEntity(const publicId,
  //     systemId : SAXString) : IInputSource;
  // end;
  //
  // ...
  //
  // function TMyResolver.resolveEntity(const publicId,
  //   systemId : SAXString) : IInputSource;
  // begin
  //   if (systemId = 'http://www.myhost.com/today') then
  //   begin
  //     // return a special input source
  //     FStream:= TMemoryStream.Create;
  //     FInputSource:= TInputSource.Create(FStream) as IInputSource;
  //     Result:= FInputSource;
  //   end else
  //   begin
  //     // use the default behaviour
  //     Result:= nil;
  //   end;
  // end;
  //
  // </pre>
  //
  // <p><strong>Warning:</strong>It is EXTREMELY important that the
  // implementor of the IEntityResolver free the created resources.
  // This can be done during the destruction of the implementing class.</p>
  //
  // <p>The application can also use this interface to redirect system
  // identifiers to local URIs or to look up replacements in a catalog
  // (possibly by using the public identifier).</p>
  //
  // @since SAX 1.0
  // @see <a href="../SAX/IXMLReader.html#setEntityResolver">IXMLReader.setEntityResolver</a>
  // @see <a href="../SAX/TInputSource.html">TInputSource</a>
  IEntityResolver = interface(IUnknown)
    [IID_IEntityResolver]

    // Allow the application to resolve external entities.


    // <p>The parser will call this method before opening any external
    // entity except the top-level document entity.  Such entities include
    // the external DTD subset and external parameter entities referenced
    // within the DTD (in either case, only if the parser reads external
    // parameter entities), and external general entities referenced
    // within the document element (if the parser reads external general
    // entities).  The application may request that the parser locate
    // the entity itself, that it use an alternative URI, or that it
    // use data provided by the application (as a character or byte
    // input stream).</p>

    // <p>Application writers can use this method to redirect external
    // system identifiers to secure and/or local URIs, to look up
    // public identifiers in a catalogue, or to read an entity from a
    // database or other input source (including, for example, a dialog
    // box).  Neither XML nor SAX specifies a preferred policy for using
    // public or system IDs to resolve resources.  However, SAX specifies
    // how to interpret any InputSource returned by this method, and that
    // if none is returned, then the system ID will be dereferenced as
    // a URL.</p>

    //
    // <p>If the system identifier is a URL, the SAX parser must
    // resolve it fully before reporting it to the application.</p>
    //
    // <p><strong>Warning:</strong>It is EXTREMELY important that the
    // implementor of the IEntityResolver free the created resources.
    // This can be done during the destruction of the implementing class.</p>
    //
    // @param publicId The public identifier of the external entity
    //        being referenced, or empty if none was supplied.
    // @param systemId The system identifier of the external entity
    //        being referenced.
    // @return An InputSource object describing the new input source,
    //         or null to request that the parser open a regular
    //         URI connection to the system identifier.
    // @exception ESAXException Any SAX exception.
    
    // @exception Exception A pascal-specific IO exception,
    //            possibly the result of creating a new Stream
    //            for the InputSource.
    // @see <a href="../SAX/TInputSource.html">TInputSource</a>
    function resolveEntity(const publicId, systemId : SAXString) : IInputSource;
  end;

  // Basic interface for SAX error handlers.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>If a SAX application needs to implement customized error
  // handling, it must implement this interface and then register an
  // instance with the XML reader using the
  // <a href="../SAX/IXMLReader.html#setErrorHandler">setErrorHandler</a>
  // method.  The parser will then report all errors and warnings
  // through this interface.</p>
  //
  // <p><strong>WARNING:</strong> If an application does <em>not</em>
  // register an ErrorHandler, XML parsing errors will go unreported,
  // except that <em>ESAXParseException</em>s will be thrown for fatal errors.
  // In order to detect validity errors, an ErrorHandler that does something
  // with <a href="../SAX/IErrorHandler.html#error">error</a> calls must be registered.</p>
  //
  // <p>For XML processing errors, a SAX driver must use this interface
  // in preference to throwing an exception: it is up to the application
  // to decide whether to throw an exception for different types of
  // errors and warnings.  Note, however, that there is no requirement that
  // the parser continue to report additional errors after a call to
  // <a href="../SAX/IErrorHandler.html#fatalError">fatalError</a>.  In other words, a SAX driver class
  // may throw an exception after reporting any fatalError. 
  // Also parsers may throw appropriate exceptions for non-XML errors.
  // For example, <a href="../SAX/IXMLReader.html#parse">IXMLReader.parse</a> would throw
  // an Exception for errors accessing entities or the document.</p>
  //
  // <p>SAX for Pascal does not follow the model of SAX for Java. To
  // better manage the life-cycle of errors, the ISAXError interface
  // was developed. For more information on errors in SAX for pascal
  // see <a href="../SAX/ISAXError.html">ISAXError</a></p>
  //
  // @since SAX 1.0
  // @see <a href="../SAX/IXMLReader.html#setErrorHandler">IXMLReader.setErrorHandler</a>
  // @see <a href="../SAX/ESAXParseException.html">ESAXParseException</a>
  IErrorHandler = interface(IUnknown)
    [IID_IErrorHandler]

    // Receive notification of a warning.
    //
    // <p>SAX parsers will use this method to report conditions that
    // are not errors or fatal errors as defined by the XML 1.0
    // recommendation.  The default behaviour is to take no action.</p>
    //
    // <p>The SAX parser must continue to provide normal parsing events
    // after invoking this method: it should still be possible for the
    // application to process the document through to the end.</p>
    //
    // <p>Filters may use this method to report other, non-XML warnings
    // as well.</p>
    //
    // @param e The warning information encapsulated in a
    //          SAX parse error interface.
    // @see <a href="../SAX/ISAXParseError.html">ISAXParseError</a>
    procedure warning(const e : ISAXParseError);

    // Receive notification of a recoverable error.
    //
    // <p>This corresponds to the definition of "error" in section 1.2
    // of the W3C XML 1.0 Recommendation.  For example, a validating
    // parser would use this callback to report the violation of a
    // validity constraint.  The default behaviour is to take no
    // action.</p>
    //
    // <p>The SAX parser must continue to provide normal parsing events
    // after invoking this method: it should still be possible for the
    // application to process the document through to the end.  If the
    // application cannot do so, then the parser should report a fatal
    // error even if the XML 1.0 recommendation does not require it to
    // do so.</p>
    //
    // <p>Filters may use this method to report other, non-XML errors
    // as well.</p>
    //
    // @param e The error information encapsulated in a
    //          SAX parse error interface.
    // @see <a href="../SAX/ISAXParseError.html">ISAXParseError</a>
    procedure error(const e : ISAXParseError);

    // Receive notification of a non-recoverable error.
    //
    // <p>This corresponds to the definition of "fatal error" in
    // section 1.2 of the W3C XML 1.0 Recommendation.  For example, a
    // parser would use this callback to report the violation of a
    // well-formedness constraint.</p>
    //
    // <p>The application must assume that the document is unusable
    // after the parser has invoked this method, and should continue
    // (if at all) only for the sake of collecting additional error
    // messages: in fact, SAX parsers are free to stop reporting any
    // other events once this method has been invoked.</p>
    //
    // @param e The error information encapsulated in a
    //          SAX parse error interface.
    // @see <a href="../SAX/ISAXParseError.html">ISAXParseError</a>
    procedure fatalError(const e : ISAXParseError);

  end;

  // Interface for associating a SAX event with a document location.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>If a SAX parser provides location information to the SAX
  // application, it does so by implementing this interface and then
  // passing an instance to the application using the content
  // handler's <a href="../SAX/IContentHandler.html#setDocumentLocator">
  // setDocumentLocator</a> method.  The application can use the
  // object to obtain the location of any other SAX event
  // in the XML source document.</p>
  //
  // <p>Note that the results returned by the object will be valid only
  // during the scope of each callback method: the application
  // will receive unpredictable results if it attempts to use the
  // locator at any other time, or after parsing completes.</p>
  //
  // <p>SAX parsers are not required to supply a locator, but they are
  // very strongly encouraged to do so.  If the parser supplies a
  // locator, it must do so before reporting any other document events.
  // If no locator has been set by the time the application receives
  // the <a href="../SAX/IContentHandler.html#startDocument">startDocument</a>
  // event, the application should assume that a locator is not
  // available.</p>
  //
  // @since SAX 1.0
  // @see <a href="../SAX/IContentHandler.html#setDocumentLocator">IContentHandler.setDocumentLocator</a>
  ILocator = interface(IUnknown)
    [IID_ILocator]

    // Return the public identifier for the current document event.
    //
    // <p>The return value is the public identifier of the document
    // entity or of the external parsed entity in which the markup
    // triggering the event appears.</p>
    //
    // @return A PSAXChar containing the public identifier, or
    //         empty if none is available.
    // @see <a href="../SAX/ILocator.html#getSystemId">ILocator.getSystemId</a>
    function getPublicId() : PSAXChar;

    // Return the system identifier for the current document event.
    //
    // <p>The return value is the system identifier of the document
    // entity or of the external parsed entity in which the markup
    // triggering the event appears.</p>
    //
    // <p>If the system identifier is a URL, the parser must resolve it
    // fully before passing it to the application.  For example, a file
    // name must always be provided as a <em>file:...</em> URL, and other
    // kinds of relative URI are also resolved against their bases.
    // WARNING: conformance in SAX for Pascal among vendors is not strong
    // in this area. Often the implementation will send only a file name with
    // no scheme.</p>
    //
    // @return A PSAXChar containing the system identifier, or empty
    //         if none is available.
    // @see <a href="../SAX/ILocator.html#getPublicId">ILocator.getPublicId</a>
    function getSystemId() : PSAXChar;

    // Return the line number where the current document event ends.
    // Lines are delimited by line ends, which are defined in
    // the XML specification
    //
    // <p><strong>Warning:</strong> The return value from the method
    // is intended only as an approximation for the sake of diagnostics;
    // it is not intended to provide sufficient information
    // to edit the character content of the original XML document.
    // In some cases, these "line" numbers match what would be displayed
    // as columns, and in others they may not match the source text
    // due to internal entity expansion.  </p>
    //
    // <p>The return value is an approximation of the line number
    // in the document entity or external parsed entity where the
    // markup triggering the event appears.</p>
    //
    // <p>If possible, the SAX driver should provide the line position
    // of the first character after the text associated with the document
    // event.  The first line is line 1.</p>
    //
    // @return The line number, or -1 if none is available.
    // @see <a href="../SAX/ILocator.html#getColumnNumber">ILocator.getColumnNumber</a>
    function getLineNumber() : Integer;

    // Return the column number where the current document event ends.
    // This is one-based number of <code>Char</code> or <code>WideChar</code>
    // values since the last line end. This is not the number of
    // physical bytes, but is the number of logical characters (which may use
    // more than one byte per character).
    //
    // <p><strong>Warning:</strong> The return value from the method
    // is intended only as an approximation for the sake of diagnostics;
    // it is not intended to provide sufficient information
    // to edit the character content of the original XML document.
    // For example, when lines contain combining character sequences, wide
    // characters, surrogate pairs, or bi-directional text, the value may
    // not correspond to the column in a text editor's display. </p>
    //
    // <p>The return value is an approximation of the column number
    // in the document entity or external parsed entity where the
    // markup triggering the event appears.</p>
    //
    // <p>If possible, the SAX driver should provide the line position
    // of the first character after the text associated with the document
    // event.</p>
    //
    // <p>If possible, the SAX driver should provide the line position
    // of the first character after the text associated with the document
    // event.  The first column in each line is column 1.</p>
    //
    // @return The column number, or -1 if none is available.
    // @see <a href="../SAX/ILocator.html#getLineNumber">ILocator.getLineNumber</a>
    function getColumnNumber() : Integer;

    // Extension property to get the public identifier
    //
    // @return A PSAXChar containing the public identifier, or
    //         empty if none is available.
    // @see <a href="../SAX/ILocator.html#getPublicId">ILocator.getPublicId</a>
    property PublicId : PSAXChar
      read getPublicId;

    // Extension property to get the system identifier
    //
    // @return A PSAXChar containing the system identifier, or
    //         empty if none is available.
    // @see <a href="../SAX/ILocator.html#getSystemId">ILocator.getSystemId</a>
    property SystemId : PSAXChar
      read getSystemId;

    // Extension property to get the line number
    //
    // @return The line number, or -1 if none is available.
    // @see <a href="../SAX/ILocator.html#getLineNumber">ILocator.getLineNumber</a>
    property LineNumber : Integer
      read getLineNumber;

    // Extension property to get the column number
    //
    // @return The column number, or -1 if none is available.
    // @see <a href="../SAX/ILocator.html#getColumnNumber">ILocator.getColumnNumber</a>
    property ColumnNumber : Integer
      read getColumnNumber;
  end;

  // Interface for reading an XML document using callbacks.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>XMLReader is the interface that an XML parser's SAX2 driver must
  // implement.  This interface allows an application to set and
  // query features and properties in the parser, to register
  // event handlers for document processing, and to initiate
  // a document parse.</p>
  //
  // <p>The XMLReader interface is split into a common base interface,
  // IBaseXMLReader, and the actual IXMLReader specification which derives
  // from it. This is done to improve type safety for applications which can
  // handle both, standard and buffered XMLReader implementations (see BSAX.pas).
  // Most applications need not be aware of this split definition.</p>
  //
  // <p>All SAX interfaces are assumed to be synchronous: the
  // <a href="../SAX/IXMLReader.html#parse">parse</a> methods must not return until parsing
  // is complete, and readers must wait for an event-handler callback
  // to return before reporting the next event.</p>
  //
  // <p>This interface replaces the (now deprecated) SAX 1.0
  // <a href="../SAX/IParser.html">Parser</a> interface.  The XMLReader interface
  // contains two important enhancements over the old Parser
  // interface (as well as some minor ones):</p>
  //
  // <ol>
  // <li>it adds a standard way to query and set features and
  //  properties; and</li>
  // <li>it adds Namespace support, which is required for many
  //  higher-level XML standards.</li>
  // </ol>
  //
  // <p>There are adapters available to convert a SAX1 Parser to
  // a SAX2 XMLReader and vice-versa.</p>
  //
  // @since SAX 2.0
  // @see <a href="../SAX/IXMLFilter.html">IXMLFilter</a>
  IBaseXMLReader = interface(IUnknown)
    [IID_IBaseXMLReader]

    // Look up the value of a feature flag.
    //
    // <p>The feature name is any fully-qualified URI.  It is
    // possible for an XMLReader to recognize a feature name but
    // temporarily be unable to return its value.
    // Some feature values may be available only in specific
    // contexts, such as before, during, or after a parse.
    // Also, some feature values may not be programmatically accessible.
    // (In the case of an adapter for SAX1 <a href="../SAX/IParser.html">IParser</a>, there is no
    // implementation-independent way to expose whether the underlying
    // parser is performing validation, expanding external entities,
    // and so forth.) </p>
    //
    // <p>All XMLReaders are required to recognize the
    // http://xml.org/sax/features/namespaces and the
    // http://xml.org/sax/features/namespace-prefixes feature names.</p>
    //
    // <p>Some feature values may be available only in specific
    // contexts, such as before, during, or after a parse.</p>
    //
    // <p>Typical usage is something like this:</p>
    //
    // <pre>
    // var
    //   r : IXMLReader;
    // begin
    //   r:= TMySAXDriver.Create() as IXMLReader;
    //
    //   // try to activate validation
    //   try
    //     r.setFeature('http://xml.org/sax/features/validation', true);
    //   except
    //     on e : ESAXException do
    //     begin
    //       Showmessage('Cannot activate validation.');
    //     end;
    //   end;
    //
    //   // register event handlers
    //   r.setContentHandler(TMyContentHandler.Create() as IContentHandler);
    //   r.setErrorHandler(TMyErrorHandler.Create() as IErrorHandler);
    //
    //   // parse the first document
    //   try
    //     r.parse('file://c:/mydoc.xml');
    //   except
    //     on e : ESAXException do
    //     begin
    //       Showmessage('XML exception reading document.');
    //     end;
    //     on e : Exception do
    //     begin
    //       Showmessage('Exception reading document.');
    //     end;
    //   end;
    // </pre>
    //
    // <p>Implementors are free (and encouraged) to invent their own features,
    // using names built on their own URIs.</p>
    //
    // @param name The feature name, which is a fully-qualified URI.
    // @return The current value of the feature (true or false).
    // @exception ESAXNotRecognizedException If the feature
    //            value can't be assigned or retrieved.
    // @exception ESAXNotSupportedException When the
    //            XMLReader recognizes the feature name but
    //            cannot determine its value at this time.
    // @see <a href="../SAX/IXMLReader.html#setFeature">IXMLReader.setFeature</a>
    function getFeature(const name : SAXString) : Boolean;

    // Set the value of a feature flag.
    //
    // <p>The feature name is any fully-qualified URI.  It is
    // possible for an XMLReader to expose a feature value but
    // to be unable to change the current value.
    // Some feature values may be immutable or mutable only
    // in specific contexts, such as before, during, or after
    // a parse.</p>
    //
    // <p>All XMLReaders are required to support setting
    // http://xml.org/sax/features/namespaces to true and
    // http://xml.org/sax/features/namespace-prefixes to false.</p>
    //
    // @param name The feature name, which is a fully-qualified URI.
    // @param state The requested value of the feature (true or false).
    // @exception ESAXNotRecognizedException If the feature
    //            value can't be assigned or retrieved.
    // @exception ESAXNotSupportedException When the
    //            XMLReader recognizes the feature name but
    //            cannot set the requested value.
    // @see <a href="../SAX/IXMLReader.html#getFeature">IXMLReader.getFeature</a>
    procedure setFeature(const name : SAXString; value : Boolean);

    // Look up the value of a property.
    //
    // <p>The property name is any fully-qualified URI.  It is
    // possible for an XMLReader to recognize a property name but
    // temporarily be unable to return its value
    // Some property values may be available only in specific
    // contexts, such as before, during, or after a parse.</p>
    //
    // <p>XMLReaders are not required to recognize any specific
    // property names, though an initial core set is documented for
    // SAX2.</p>
    //
    // <p>Implementors are free (and encouraged) to invent their own properties,
    // using names built on their own URIs.</p>
    //
    // <p> Within SAX for Pascal property setting is handled through
    // the interface that is returned by the call to getProperty. This
    // eliminates the need for multiple lookups in tight loop situations
    // as a user can maintain a reference to the interface.</p>
    //
    // @param name The property name, which is a fully-qualified URI.
    // @returns An Interface that allows the getting and setting of the property
    // @exception ESAXNotRecognizedException If the property
    //            interface cannot be retrieved.
    // @exception ESAXNotSupportedException When the
    //            XMLReader recognizes the property name but
    //            cannot determine its interface value at this time.
    function getProperty(const name : SAXString) : IProperty;

    // Allow an application to register an error event handler.
    //
    // <p>If the application does not register an error handler, all
    // error events reported by the SAX parser will be silently
    // ignored; however, normal processing may not continue.  It is
    // highly recommended that all SAX applications implement an
    // error handler to avoid unexpected bugs.</p>
    //
    // <p>Applications may register a new or different handler in the
    // middle of a parse, and the SAX parser must begin using the new
    // handler immediately.</p>
    //
    // @param handler The error handler.
    // @see <a href="../SAX/IXMLReader.html#getErrorHandler">IXMLReader.getErrorHandler</a>
    procedure setErrorHandler(const handler : IErrorHandler);

    // Return the current error handler.
    //
    // @return The current error handler, or nil if none
    //         has been registered.
    // @see <a href="../SAX/IXMLReader.html#setErrorHandler">IXMLReader.setErrorHandler</a>
    function getErrorHandler() : IErrorHandler;

    // Allow an application to register an entity resolver.
    //
    // <p>If the application does not register an entity resolver,
    // the XMLReader will perform its own default resolution.</p>
    //
    // <p>Applications may register a new or different resolver in the
    // middle of a parse, and the SAX parser must begin using the new
    // resolver immediately.</p>
    //
    // @param resolver The entity resolver.
    // @see <a href="../SAX/IXMLReader.html#getEntityResolver">IXMLReader.getEntityResolver</a>
    procedure setEntityResolver(const resolver : IEntityResolver);

    // Return the current entity resolver.
    //
    // @return The current entity resolver, or nil if none
    //         has been registered.
    // @see <a href="../SAX/IXMLReader.html#setEntityResolver">IXMLReader.setEntityResolver</a>
    function getEntityResolver() : IEntityResolver;

    // Parse an XML document.
    //
    // <p>The application can use this method to instruct the XML
    // reader to begin parsing an XML document from any valid input
    // source (a character stream, a byte stream, or a URI).</p>
    //
    // <p>Applications may not invoke this method while a parse is in
    // progress (they should create a new XMLReader instead for each
    // nested XML document).  Once a parse is complete, an
    // application may reuse the same XMLReader object, possibly with a
    // different input source.
    // Configuration of the XMLReader object (such as handler bindings and
    // values established for feature flags and properties) is unchanged
    // by completion of a parse, unless the definition of that aspect of
    // the configuration explicitly specifies other behavior.
    // (For example, feature flags or properties exposing
    // characteristics of the document being parsed.)
    // </p>
    //
    // <p>During the parse, the XMLReader will provide information
    // about the XML document through the registered event
    // handlers.</p>
    //
    // <p>This method is synchronous: it will not return until parsing
    // has ended.  If a client application wants to terminate
    // parsing early, it should throw an exception.</p>
    //
    // @param source The input source for the top-level of the
    //        XML document.
    // @exception ESAXException Any SAX exception.
    
    // @exception Exception An IO exception from the parser,
    //            possibly from a byte stream
    //            supplied by the application.
    // @see <a href="../SAX/TInputSource.html">TInputSource</a>
    // @see <a href="../SAX/IXMLReader.html#parse.SAXString">IXMLReader.parse(SAXString)</a>
    // @see <a href="../SAX/IXMLReader.html#setEntityResolver">IXMLReader.setEntityResolver</a>
    // @see <a href="../SAX/IXMLReader.html#setDTDHandler">IXMLReader.setDTDHandler</a>
    // @see <a href="../SAX/IXMLReader.html#setContentHandler">IXMLReader.setContentHandler</a>
    // @see <a href="../SAX/IXMLReader.html#setErrorHandler">IXMLReader.setErrorHandler</a>
    procedure parse(const input : IInputSource); overload;

    // Parse an XML document from a system identifier (URI).
    //
    // <p>This method is a shortcut for the common case of reading a
    // document from a system identifier.  It is the exact
    // equivalent of the following:</p>
    //
    // <pre>
    // input:= TInputSource.Create(systemId) as IInputSource;
    // try
    //   parse(input);
    // finally
    //   input:= nil;
    // end;
    // </pre>
    //
    // <p>If the system identifier is a URL, it must be fully resolved
    // by the application before it is passed to the parser.</p>
    //
    // @param systemId The system identifier (URI).
    // @exception ESAXException Any SAX exception.

    // @exception Exception An IO exception from the parser,
    //            possibly from a byte stream
    //            supplied by the application.
    // @see <a href="../SAX/IXMLReader.html#parse.TInputSource">IXMLReader.parse(TInputSource)</a>
    procedure parse(const systemId : SAXString); overload;

    // Extension property to get and set the ErrorHandler
    //
    // @return The current Error handler, or nil if none
    //         has been registered.
    // @see <a href="../SAX/IXMLReader.html#getErrorHandler">IXMLReader.getErrorHandler</a>
    // @see <a href="../SAX/IXMLReader.html#setErrorHandler">IXMLReader.setErrorHandler</a>
    property ErrorHandler: IErrorHandler
      read getErrorHandler write setErrorHandler;


    // Extension property to get and set the EntityResolver
    //
    // @return The current content handler, or nil if none
    //         has been registered.
    // @see <a href="../SAX/IXMLReader.html#getEntityResolver">IXMLReader.getEntityResolver</a>
    // @see <a href="../SAX/IXMLReader.html#setEntityResolver">IXMLReader.setEntityResolver</a>
    property EntityResolver: IEntityResolver
      read getEntityResolver write setEntityResolver;

    // Extension property to get and set the IXMLReader's features
    //
    // @index name The feature name, which is a fully-qualified URI.
    // @exception ESAXNotRecognizedException When the
    //            XMLReader does not recognize the feature name.
    // @exception ESAXNotSupportedException When the
    //            XMLReader recognizes the feature name but
    //            cannot set the requested value.
    // @return The state of the feature (true or false)
    // @see <a href="../SAX/IXMLReader.html#getFeature">IXMLReader.getFeature</a>
    // @see <a href="../SAX/IXMLReader.html#setFeature">IXMLReader.setFeature</a>
    property Features[const name: SAXString]: Boolean
      read getFeature write setFeature;

    // Extension property to get the IXMLReader's property interfaces
    //
    // @index name The property name, which is a fully-qualified URI.
    // @exception ESAXNotRecognizedException When the
    //            XMLReader does not recognize the property name.
    // @exception ESAXNotSupportedException When the
    //            XMLReader recognizes the property name but
    //            cannot set the requested value.
    // @return The value stored in the property as an IProperty interface
    // @see <a href="../SAX/IXMLReader.html#getProperty">IXMLReader.getProperty</a>
    property Properties[const name: SAXString]: IProperty
      read getProperty;
  end;

  IXMLReader = interface(IBaseXMLReader)
    [IID_IXMLReader]

    // Allow an application to register a DTD event handler.
    //
    // <p>If the application does not register a DTD handler, all DTD
    // events reported by the SAX parser will be silently ignored.</p>
    //
    // <p>Applications may register a new or different handler in the
    // middle of a parse, and the SAX parser must begin using the new
    // handler immediately.</p>
    //
    // @param handler The DTD handler.
    // @see <a href="../SAX/IXMLReader.html#getDTDHandler">IXMLReader.getDTDHandler</a>
    procedure setDTDHandler(const handler : IDTDHandler);

    // Return the current DTD handler.
    //
    // @return The current DTD handler, or null if none
    //         has been registered.
    // @see <a href="../SAX/IXMLReader.html#setDTDHandler">IXMLReader.setDTDHandler</a>
    function getDTDHandler() : IDTDHandler;

    // Allow an application to register a content event handler.
    //
    // <p>If the application does not register a content handler, all
    // content events reported by the SAX parser will be silently
    // ignored.</p>
    //
    // <p>Applications may register a new or different handler in the
    // middle of a parse, and the SAX parser must begin using the new
    // handler immediately.</p>
    //
    // @param handler The content handler.
    // @see <a href="../SAX/IXMLReader.html#getContentHandler">IXMLReader.getContentHandler</a>
    procedure setContentHandler(const handler : IContentHandler);

    // Return the current content handler.
    //
    // @return The current content handler, or nil if none
    //         has been registered.
    // @see <a href="../SAX/IXMLReader.html#setContentHandler">IXMLReader.setContentHandler</a>
    function getContentHandler() : IContentHandler;

    // Extension property to get and set the ContentHandler
    //
    // @return The current content handler, or nil if none
    //         has been registered.
    // @see <a href="../SAX/IXMLReader.html#getContentHandler">IXMLReader.getContentHandler</a>
    // @see <a href="../SAX/IXMLReader.html#setContentHandler">IXMLReader.setContentHandler</a>
    property ContentHandler: IContentHandler
      read getContentHandler write setContentHandler;

    // Extension property to get and set the DTDHandler
    //
    // @return The current DTD handler, or nil if none
    //         has been registered.
    // @see <a href="../SAX/IXMLReader.html#getDTDHandler">IXMLReader.getDTDHandler</a>
    // @see <a href="../SAX/IXMLReader.html#setDTDHandler">IXMLReader.setDTDHandler</a>
    property DTDHandler: IDTDHandler
      read getDTDHandler write setDTDHandler;
  end;

  // Interface for an XML filter.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>An XML filter is like an XML reader, except that it obtains its
  // events from another XML reader rather than a primary source like
  // an XML document or database.  Filters can modify a stream of
  // events as they pass on to the final application.</p>
  //
  // <p>The XMLFilterImpl helper class provides a convenient base
  // for creating SAX2 filters, by passing on all
  // <a href="../SAX/IEntityResolver.html">IEntityResolver</a>,
  // <a href="../SAX/IDTDHandler.html">IDTDHandler</a>,
  // <a href="../SAX/IContentHandler.html">IContentHandler</a> and
  // <a href="../SAX/IErrorHandler.html">IErrorHandler</a> events automatically.</p>
  //
  // @since SAX 2.0
  // @see <a href="../SAXHelpers/TXMLFilterImpl.html">TXMLFilterImpl</a>
  IXMLFilter = interface(IXMLReader)
    [IID_IXMLFilter]

    // Set the parent reader.
    //
    // <p>This method allows the application to link the filter to
    // a parent reader (which may be another filter).  The argument
    // may not be null.</p>
    //
    // @param parent The parent reader.
    procedure setParent(const parent : IXMLReader);

    // Get the parent reader.
    //
    // <p>This method allows the application to query the parent
    // reader (which may be another filter).  It is generally a
    // bad idea to perform any operations on the parent reader
    // directly: they should all pass through this filter.</p>
    //
    // @return The parent filter, or nil if none has been set.
    function getParent() : IXMLReader;

    // Extension property to get and set the IXMLFilters's parent
    //
    // @return The parent reader.
    // @see <a href="../SAX/IXMLFilter.html#getParent">IXMLFilter.getParent</a>
    // @see <a href="../SAX/IXMLFilter.html#setParent">IXMLFilter.setParent</a>
    property Parent: IXMLReader
      read getParent write setParent;
  end;

  // Deprecated SAX 1 Interfaces

  // Interface for an element's attribute specifications.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This is the original SAX1 interface for reporting an element's
  // attributes.  Unlike the new <a href="../SAX/IAttributes.html">IAttributes</a>
  // interface, it does not support Namespace-related information.</p>
  //
  // <p>When an attribute list is supplied as part of a
  // <a href="../SAX/IDocumentHandler.html#startElement">startElement</a>
  // event, the list will return valid results only during the
  // scope of the event; once the event handler returns control
  // to the parser, the attribute list is invalid.</p>
  //
  // <p>An attribute list includes only attributes that have been
  // specified or defaulted: #IMPLIED attributes will not be included.</p>
  //
  // <p>There are two ways for the SAX application to obtain information
  // from the Attribute List.  First, it can iterate through the entire
  // list:</p>
  //
  // <pre>
  // procedure TMyDocumentHandler.startElement(const name: SAXString;
  //   const atts: IAttributeList);
  // var i : Integer;
  //     attName : SAXString;
  //     attType : SAXString;
  //     attValue : SAXString;
  // begin
  //   for i:= 0 to atts.getLength()-1 do
  //   begin
  //     attName:= atts.getName(i);
  //     attType:= atts.getType(i);
  //     attValue:= atts.getValue(i);
  //     [...]
  //   end;
  // end;
  // </pre>
  //
  // <p>(Note that the result of getLength() will be zero if there
  // are no attributes.)</p>
  //
  // <p>As an alternative, the application can request the value or
  // type of specific attributes:</p>
  //
  // <pre>
  // procedure TMyDocumentHandler.startElement(const name: SAXString;
  //   const atts: IAttributeList);
  // var attId : SAXString;
  //     attLabel : SAXString;
  // begin
  //   attId:= atts.getValue('id');
  //   attLabel:= atts.getValue('label');
  //   [...]
  // end;
  // </pre>
  //
  // @deprecated This interface has been replaced by the SAX2
  //             <a href="../SAX/IAttributes.html">IAttributes</a>
  //             interface, which includes Namespace support.
  // @since SAX 1.0
  // @see <a href="../SAX/IDocumentHandler.html#startElement">IDocumentHandler.startElement</a>
  IAttributeList = interface(IUnknown)
    [IID_IAttributeList]

    // Return the number of attributes in this list.
    //
    // <p>The SAX parser may provide attributes in any
    // arbitrary order, regardless of the order in which they were
    // declared or specified.  The number of attributes may be
    // zero.</p>
    //
    // @return The number of attributes in the list.
    function getLength() : Integer;

    // Return the name of an attribute in this list (by position).
    //
    // <p>The names must be unique: the SAX parser shall not include the
    // same attribute twice.  Attributes without values (those declared
    // #IMPLIED without a value specified in the start tag) will be
    // omitted from the list.</p>
    //
    // <p>If the attribute name has a namespace prefix, the prefix
    // will still be attached.</p>
    //
    // @param i The index of the attribute in the list (starting at 0).
    // @return The name of the indexed attribute, or an empty string
    //         if the index is out of range.
    // @see <a href="../SAX/IAttributeList.html#getLength">IAttributeList.getLength</a>
    function getName(i : Integer) : SAXString;
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Return the type of an attribute in the list (by position).
    //
    // <p>The attribute type is one of the strings 'CDATA', 'ID',
    // 'IDREF', 'IDREFS', 'NMTOKEN', 'NMTOKENS', 'ENTITY', 'ENTITIES',
    // or 'NOTATION' (always in upper case).</p>
    //
    // <p>If the parser has not read a declaration for the attribute,
    // or if the parser does not report attribute types, then it must
    // return the value 'CDATA' as stated in the XML 1.0 Recommentation
    // (clause 3.3.3, "Attribute-Value Normalization").</p>
    //
    // <p>For an enumerated attribute that is not a notation, the
    // parser will report the type as 'NMTOKEN'.</p>
    //
    // @param i The index of the attribute in the list (starting at 0).
    // @return The attribute type as a string, or
    //         an empty string if the index is out of range.
    // @see <a href="../SAX/IAttributeList.html#getLength">IAttributeList.getLength</a>
    // @see <a href="../SAX/IAttributeList.html#getType.SAXString">IAttributeList.getType(SAXString)</a>
    function getType(i : Integer) : SAXString; overload;
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Return the value of an attribute in the list (by position).
    //
    // <p>If the attribute value is a list of tokens (IDREFS,
    // ENTITIES, or NMTOKENS), the tokens will be concatenated
    // into a single string separated by whitespace.</p>
    //
    // @param i The index of the attribute in the list (starting at 0).
    // @return The attribute value as a string, or
    //         an empty string if the index is out of range.
    // @see <a href="../SAX/IAttributeList.html#getLength">IAttributeList.getLength</a>
    // @see <a href="../SAX/IAttributeList.html#getValue.SAXString">IAttributeList.getValue(SAXString)</a>
    function getType(const name : SAXString) : SAXString; overload;
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Return the type of an attribute in the list (by name).
    //
    // <p>The return value is the same as the return value for
    // getType(Integer).</p>
    //
    // <p>If the attribute name has a namespace prefix in the document,
    // the application must include the prefix here.</p>
    //
    // @param name The name of the attribute.
    // @return The attribute type as a string, or an empty string if no
    //         such attribute exists.
    // @see <a href="../SAX/IAttributeList.html#getType.Integer">IAttributeList.getType(Integer)</a>
    function getValue(i : Integer) : SAXString; overload;
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Return the value of an attribute in the list (by name).
    //
    // <p>The return value is the same as the return value for
    // getValue(Integer).</p>
    //
    // <p>If the attribute name has a namespace prefix in the document,
    // the application must include the prefix here.</p>
    //
    // @param i The index of the attribute in the list.
    // @return The attribute value as a string, or an empty string if
    //         no such attribute exists.
    // @see <a href="../SAX/IAttributeList.html#getValue.Integer">IAttributeList.getValue(Integer)</a>
    function getValue(const name : SAXString) : SAXString; overload;
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}
  end;

  // Receive notification of general document events.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This was the main event-handling interface for SAX1; in
  // SAX2, it has been replaced by <a href="../SAX/IContentHandler.html">
  // IContentHandler</a>, which provides Namespace support and reporting
  // of skipped entities.  This interface is included in SAX2 only
  // to support legacy SAX1 applications.</p>
  //
  // <p>The order of events in this interface is very important, and
  // mirrors the order of information in the document itself.  For
  // example, all of an element's content (character data, processing
  // instructions, and/or subelements) will appear, in order, between
  // the startElement event and the corresponding endElement event.</p>
  //
  // <p>No default implementation is provided for this interface
  // The application can find
  // the location of any document event using the ILocator interface
  // supplied by the IParser through the setDocumentLocator method.</p>
  //
  // @deprecated This interface has been replaced by the SAX2
  //             <a href="../SAX/IContentHandler.html">IContentHandler</a>
  //             interface, which includes Namespace support.
  // @since SAX 1.0
  // @see <a href="../SAX/IParser.html#setDocumentHandler">IParser.setDocumentHandler</a>
  // @see <a href="../SAX/ILocator.html">ILocator</a>
  IDocumentHandler = interface(IUnknown)
    [IID_IDocumentHandler]

    // Receive an object for locating the origin of SAX document events.
    //
    // <p>SAX parsers are strongly encouraged (though not absolutely
    // required) to supply a locator: if it does so, it must supply
    // the locator to the application by invoking this method before
    // invoking any of the other methods in the IDocumentHandler
    // interface.</p>
    //
    // <p>The locator allows the application to determine the end
    // position of any document-related event, even if the parser is
    // not reporting an error.  Typically, the application will
    // use this information for reporting its own errors (such as
    // character content that does not match an application's
    // business rules).  The information returned by the locator
    // is probably not sufficient for use with a search engine.</p>
    //
    // <p>Note that the locator will return correct information only
    // during the invocation of the events in this interface.  The
    // application should not attempt to use it at any other time.</p>
    //
    // @param locator An object that can return the location of
    //                any SAX document event.
    // @see <a href="../SAX/ILocator.html">ILocator</a>
    procedure setDocumentLocator(const locator: ILocator);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Receive notification of the beginning of a document.
    //
    // <p>The SAX parser will invoke this method only once, before any
    // other methods in this interface or in IDTDHandler (except for
    // setDocumentLocator).</p>
    //
    // @exception ESAXException Any SAX exception.
    
    procedure startDocument();
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Receive notification of the end of a document.
    //
    // <p>The SAX parser will invoke this method only once, and it will
    // be the last method invoked during the parse.  The parser shall
    // not invoke this method until it has either abandoned parsing
    // (because of an unrecoverable error) or reached the end of
    // input.</p>
    //
    // @exception ESAXException Any SAX exception.
    
    procedure endDocument();
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Receive notification of the beginning of an element.
    //
    // <p>The Parser will invoke this method at the beginning of every
    // element in the XML document; there will be a corresponding
    // endElement() event for every startElement() event (even when the
    // element is empty). All of the element's content will be
    // reported, in order, before the corresponding endElement()
    // event.</p>
    //
    // <p>If the element name has a namespace prefix, the prefix will
    // still be attached.  Note that the attribute list provided will
    // contain only attributes with explicit values (specified or
    // defaulted): #IMPLIED attributes will be omitted.</p>
    //
    // @param name The element type name.
    // @param atts The attributes attached to the element, if any.
    // @exception ESAXException Any SAX exception.
    
    // @see <a href="../SAX/IDocumentHandler.html#endElement">IDocumentHandler.endElement</a>
    // @see <a href="../SAX/IAttributeList.html">IAttributeList</a>
    procedure startElement(const name: SAXString; const atts: IAttributeList);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Receive notification of the end of an element.
    //
    // <p>The SAX parser will invoke this method at the end of every
    // element in the XML document; there will be a corresponding
    // startElement() event for every endElement() event (even when the
    // element is empty).</p>
    //
    // <p>If the element name has a namespace prefix, the prefix will
    // still be attached to the name.</p>
    //
    // @param name The element type name
    // @exception ESAXException Any SAX exception.
    
    procedure endElement(const name: SAXString);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Receive notification of character data.
    //
    // <p>The Parser will call this method to report each chunk of
    // character data.  SAX parsers may return all contiguous character
    // data in a single chunk, or they may split it into several
    // chunks; however, all of the characters in any single event
    // must come from the same external entity, so that the Locator
    // provides useful information.</p>
    //
    // <p>The application must not attempt to read from the array
    // outside of the specified range.</p>
    //
    // <p>Note that some parsers will report whitespace using the
    // ignorableWhitespace() method rather than this one (validating
    // parsers must do so).</p>
    //
    // @param ch The characters from the XML document.
    // @exception ESAXException Any SAX exception.

    // @see <a href="../SAX/IDocumentHandler.html#ignorableWhitespace">IDocumentHandler.ignorableWhitespace</a>
    // @see <a href="../SAX/ILocator.html">ILocator</a>
    procedure characters(const ch : SAXString);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Receive notification of ignorable whitespace in element content.
    //
    // <p>Validating Parsers must use this method to report each chunk
    // of ignorable whitespace (see the W3C XML 1.0 recommendation,
    // section 2.10): non-validating parsers may also use this method
    // if they are capable of parsing and using content models.</p>
    //
    // <p>SAX parsers may return all contiguous whitespace in a single
    // chunk, or they may split it into several chunks; however, all of
    // the characters in any single event must come from the same
    // external entity, so that the Locator provides useful
    // information.</p>
    //
    // <p>The application must not attempt to read from the array
    // outside of the specified range.</p>
    //
    // @param ch The characters from the XML document.
    // @exception ESAXException Any SAX exception.

    // @see <a href="../SAX/IDocumentHandler.html#characters">IDocumentHandler.characters</a>
    procedure ignorableWhitespace(const ch : SAXString);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Receive notification of a processing instruction.
    //
    // <p>The Parser will invoke this method once for each processing
    // instruction found: note that processing instructions may occur
    // before or after the main document element.</p>
    //
    // <p>A SAX parser should never report an XML declaration (XML 1.0,
    // section 2.8) or a text declaration (XML 1.0, section 4.3.1)
    // using this method.</p>
    //
    // @param target The processing instruction target.
    // @param data The processing instruction data, or an empty string if
    //        none was supplied.
    // @exception ESAXException Any SAX exception.
    
    procedure processingInstruction(const target, data : SAXString);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}
  end;

  // Basic interface for SAX (Simple API for XML) parsers.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This was the main event supplier interface for SAX1; it has
  // been replaced in SAX2 by <a href="../SAX/IXMLReader.html">IXMLReader</a>,
  // which includes Namespace support and sophisticated configurability
  // and extensibility.</p>
  //
  // <p>All SAX1 parsers must implement this basic interface: it allows
  // applications to register handlers for different types of events
  // and to initiate a parse from a URI, or a character stream.</p>
  //
  // <p>All SAX1 parsers must also implement a zero-argument constructor
  // (though other constructors are also allowed).</p>
  //
  // <p>SAX1 parsers are reusable but not re-entrant: the application
  // may reuse a parser object (possibly with a different input source)
  // once the first parse has completed successfully, but it may not
  // invoke the parse() methods recursively within a parse.</p>
  //
  // @deprecated This interface has been replaced by the SAX2
  //             {@IXMLReader IXMLReader}
  //             interface, which includes Namespace support.
  // @since SAX 1.0
  // @see <a href="../SAX/IEntityResolver.html">IEntityResolver</a>
  // @see <a href="../SAX/IDTDHandler.html">IDTDHandler</a>
  // @see <a href="../SAX/IDocumentHandler.html">IDocumentHandler</a>
  // @see <a href="../SAX/IErrorHandler.html">IErrorHandler</a>
  // @see <a href="../SAX/TInputSource.html">TInputSource</a>
  IParser = interface(IUnknown)
    [IID_IParser]

    // Allow an application to request a locale for errors and warnings.
    //
    // <p>SAX parsers are not required to provide localisation for errors
    // and warnings; if they cannot support the requested locale,
    // however, they must throw a SAX exception.  Applications may
    // not request a locale change in the middle of a parse.</p>
    //
    // @param locale A string representing the locale.  This can be used
    //            to determine a locale for the specific system
    // @exception ESAXException Throws an exception
    //            (using the previous or default locale) if the
    //            requested locale is not supported.
    // @see <a href="../SAX/ESAXException.html">ESAXException</a>
    // @see <a href="../SAX/ESAXParseException.html">ESAXParseException</a>
    procedure setLocale(const locale : SAXString);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Allow an application to register a custom entity resolver.
    //
    // <p>If the application does not register an entity resolver, the
    // SAX parser will resolve system identifiers and open connections
    // to entities itself</p>
    //
    // <p>Applications may register a new or different entity resolver
    // in the middle of a parse, and the SAX parser must begin using
    // the new resolver immediately.</p>
    //
    // @param resolver The object for resolving entities.
    // @see <a href="../SAX/IEntityResolver.html">IEntityResolver</a>
    procedure setEntityResolver(const resolver : IEntityResolver);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Allow an application to register a DTD event handler.
    //
    // <p>If the application does not register a DTD handler, all DTD
    // events reported by the SAX parser will be silently
    // ignored</p>
    //
    // <p>Applications may register a new or different
    // handler in the middle of a parse, and the SAX parser must
    // begin using the new handler immediately.</p>
    //
    // @param handler The DTD handler.
    // @see <a href="../SAX/IDTDHandler.html">IDTDHandler</a>
    procedure setDTDHandler(const handler : IDTDHandler);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Allow an application to register a document event handler.
    //
    // <p>If the application does not register a document handler, all
    // document events reported by the SAX parser will be silently
    // ignored</p>
    //
    // <p>Applications may register a new or different handler in the
    // middle of a parse, and the SAX parser must begin using the new
    // handler immediately.</p>
    //
    // @param handler The document handler.
    // @see <a href="../SAX/IDocumentHandler.html">IDocumentHandler</a>
    procedure setDocumentHandler(const handler : IDocumentHandler);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Allow an application to register an error event handler.
    //
    // <p>If the application does not register an error event handler,
    // all error events reported by the SAX parser will be silently
    // ignored, except for fatalError, which will throw a ESAXException</p>
    //
    // <p>Applications may register a new or different handler in the
    // middle of a parse, and the SAX parser must begin using the new
    // handler immediately.</p>
    //
    // @param handler The error handler.
    // @see <a href="../SAX/IErrorHandler.html">IErrorHandler</a>
    // @see <a href="../SAX/ESAXException.html">ESAXException</a>
    procedure setErrorHandler(const handler : IErrorHandler);
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Parse an XML document.
    //
    // <p>The application can use this method to instruct the SAX parser
    // to begin parsing an XML document from any valid input
    // source (a character stream, a byte stream, or a URI).</p>
    //
    // <p>Applications may not invoke this method while a parse is in
    // progress (they should create a new IParser instead for each
    // additional XML document).  Once a parse is complete, an
    // application may reuse the same IParser object, possibly with a
    // different input source.</p>
    //
    // @param source The input source for the top-level of the
    //        XML document.
    // @exception ESAXException Any SAX exception.
    
    // @exception Exception An IO exception from the parser,
    //            possibly from a byte stream or character stream
    //            supplied by the application.
    // @see <a href="../SAX/TInputSource.html">TInputSource</a>
    // @see <a href="../SAX/IParser.html#parse.SAXString">IParser.parse(SAXString)</a>
    // @see <a href="../SAX/IParser.html#setEntityResolver">IParser.setEntityResolver</a>
    // @see <a href="../SAX/IParser.html#setDTDHandler">IParser.setDTDHandler</a>
    // @see <a href="../SAX/IParser.html#setDocumentHandler">IParser.setDocumentHandler</a>
    // @see <a href="../SAX/IParser.html#setErrorHandler">IParser.setErrorHandler</a>
    procedure parse(const input : IInputSource); overload;
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

    // Parse an XML document from a system identifier (URI).
    //
    // <p>This method is a shortcut for the common case of reading a
    // document from a system identifier.  It is the exact
    // equivalent of the following:</p>
    //
    // <pre>
    // input:= TInputSource.Create(systemId) as IInputSource;
    // try
    //   parse(input);
    // finally
    //   input:= nil;
    // end;
    // </pre>
    //
    // <p>If the system identifier is a URL, it must be fully resolved
    // by the application before it is passed to the parser.</p>
    //
    // @param systemId The system identifier (URI).
    // @exception ESAXException Any SAX exception.

    // @exception Exception An IO exception from the parser,
    //            possibly from a byte stream or character stream
    //            supplied by the application.
    // @see <a href="../SAX/IParser.html#parse.TInputSource">IParser.parse(TInputSource)</a>
    procedure parse(const systemId : SAXString); overload;
    {$IFDEF DELPHI6_UP}deprecated;{$ENDIF}

  end;

  IProperty = interface(IUnknown)
    [IID_IProperty]
    function getName() : SAXString;
  end;

  IBooleanProperty = interface(IProperty)
    [IID_IBooleanProperty]
    function getValue() : Boolean;
    procedure setValue(value : Boolean);
    property Value : Boolean
      read getValue write setValue;
  end;

  IIntegerProperty = interface(IProperty)
    [IID_IIntegerProperty]
    function getValue() : Integer;
    procedure setValue(value : Integer);
    property Value : Integer
      read getValue write setValue;
  end;

  IInterfaceProperty = interface(IProperty)
    [IID_IInterfaceProperty]
    function getValue() : IUnknown;
    procedure setValue(const value : IUnknown);
    property Value : IUnknown
      read getValue write setValue;
  end;

  IStringProperty = interface(IProperty)
    [IID_IStringProperty]
    function getValue() : SAXString;
    procedure setValue(const value : SAXString);
    property Value : SAXString
      read getValue write setValue;
  end;

  // Exception classes

  ISAXError = interface(IUnknown)
    [IID_ISAXError]
    function getMessage() : PSAXChar;
    function getNativeError: IUnknown;
  end;

  ISAXParseError = interface(ISAXError)
    [IID_ISAXParseError]
    function getPublicId() : PSAXChar;
    function getSystemId() : PSAXChar;
    function getLineNumber() : Integer;
    function getColumnNumber() : Integer;
  end;

  ISAXNotRecognizedError = interface(ISAXError)
    [IID_ISAXNotRecognizedError]
  end;

  ISAXNotSupportedError = interface(ISAXError)
    [IID_ISAXNotSupportedError]
  end;

  ISAXIllegalStateError = interface(ISAXError)
    [IID_ISAXIllegalStateError]
  end;

  ISAXIllegalArgumentError = interface(ISAXError)
    [IID_ISAXIllegalArgumentError]
  end;

  // Encapsulate a general SAX error or warning.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This class can contain basic error or warning information from
  // either the XML parser or the application: a parser writer or
  // application writer can subclass it to provide additional
  // functionality.  SAX handlers may throw this exception or
  // any exception subclassed from it.</p>
  //
  // <p>If the application needs to pass through other types of
  // exceptions, it must wrap those exceptions in a ESAXException
  // or an exception derived from a ESAXException.</p>
  //
  // <p>If the parser or application needs to include information about a
  // specific location in an XML document, it should use the
  // <a href="../SAX/ESAXParseException.html">ESAXParseException</a> subclass.</p>
  //
  // @since SAX 1.0
  // @see <a href="../SAX/ESAXParseException.html">ESAXParseException</a>
  ESAXException = class(Exception)
  public

    // Create a new ESAXException using an existing error Interface.
    //
    // <p>The existing error Interface will be embedded in the new
    // exception, and its message will become the default message for
    // the ESAXException. This is handled differently from SAX for Java
    // in order to better maintain the life-cycle of embedded errors.</p>
    //
    // @param e The interface to be wrapped in the new exception.
    constructor Create(const e : ISAXError); overload;

    // Return a detail message for this exception.
    //
    // <p>If there is an embedded exception, and if the ESAXException
    // has no detail message of its own, this method will return
    // the detail message from the embedded exception.</p>
    //
    // @return The error or warning message.
    function getMessage() : SAXString;

  end;

  // Encapsulate an XML parse error or warning.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This exception may include information for locating the error
  // in the original XML document, as if it came from a <a href="../SAX/ILocator.html">ILocator</a>
  // object.  Note that although the application
  // will receive a ESAXParseException as the argument to the handlers
  // in the <a href="../SAX/IErrorHandler.html">IErrorHandler</a> interface,
  // the application is not actually required to throw the exception;
  // instead, it can simply read the information in it and take a
  // different action.</p>
  //
  // <p>Since this exception is a subclass of <a href="../SAX/ESAXException.html">ESAXException</a>,
  // it inherits the ability to wrap another exception.</p>
  //
  // @since SAX 1.0
  // @see <a href="../SAX/ESAXException.html">ESAXException</a>
  // @see <a href="../SAX/ILocator.html">ILocator</a>
  // @see <a href="../SAX/IErrorHandler.html">IErrorHandler</a>
  ESAXParseException = class(ESAXException)
  private
    // @serial The public identifier, or an empty string.
    // @see <a href="../SAX/ESAXParseException.html#getPublicId">ESAXParseException.getPublicId</a>
    FpublicId : SAXString;

    // @serial The system identifier, or an empty string.
    // @see <a href="../SAX/ESAXParseException.html#getSystemId">ESAXParseException.getSystemId</a>
    FsystemId : SAXString;

    // @serial The line number, or -1.
    // @see <a href="../SAX/ESAXParseException.html#getLineNumber">ESAXParseException.getLineNumber</a>
    FlineNumber : Integer;

    // @serial The column number, or -1.
    // @see <a href="../SAX/ESAXParseException.html#getColumnNumber">ESAXParseException.getColumnNumber</a>
    FcolumnNumber : Integer;

    // @serial URI identifying the exception, or an empty string
    // @see <a href="../SAX/ESAXParseException.html#getExceptionId">ESAXParseException.getExceptionId</a>
    FexceptionId : SAXString;

  public

    // Create a new ESAXParseException from an ISAXParseError interface.
    //
    // @param e The interface info used to create the information
    // @see <a href="../SAX/ISAXParseError.html">ISAXParseError</a>
    constructor Create(const e: ISAXParseError); overload;

    // Create a new ESAXParseException from a message and a Locator.
    //
    // <p>This constructor is especially useful when an application is
    // creating its own exception from within a <a href="../SAX/IContentHandler.html">IContentHandler</a>
    // callback.</p>
    //
    // @param message The error or warning message.
    // @param locator The locator object for the error or warning (may be
    //        nil).
    // @see <a href="../SAX/ILocator.html">ILocator</a>
    constructor Create(const message : SAXString;
      const locator : ILocator); overload;

    // Create a new ESAXParseException.
    //
    // <p>This constructor is most useful for parser writers.</p>
    //
    // <p>All parameters except the message are as if
    // they were provided by a <a href="../SAX/ILocator.html">ILocator</a>.  For example, if the
    // system identifier is a URL (including relative filename), the
    // caller must resolve it fully before creating the exception.</p>
    //
    // @param message The error or warning message.
    // @param publicId The public identifier of the entity that generated
    //                 the error or warning.
    // @param systemId The system identifier of the entity that generated
    //                 the error or warning.
    // @param lineNumber The line number of the end of the text that
    //                   caused the error or warning.
    // @param columnNumber The column number of the end of the text that
    //                     cause the error or warning.
    constructor Create(const message, publicId, systemId : SAXString;
      lineNumber, columnNumber : Integer); overload;

    // Get the public identifier of the entity where the exception occurred.
    //
    // @return A string containing the public identifier, or an empty string
    //         if none is available.
    // @see <a href="../SAX/ILocator.html#getPublicId">ILocator.getPublicId</a>
    function getPublicId() : PSAXChar;

    // Get the system identifier of the entity where the exception occurred.
    //
    // <p>If the system identifier is a URL, it will have been resolved
    // fully.</p>
    //
    // @return A string containing the system identifier, or an empty string
    //         if none is available.
    // @see <a href="../SAX/ILocator.html#getSystemId">ILocator.getSystemId</a>
    function getSystemId() : PSAXChar;

    // The line number of the end of the text where the exception occurred.
    //
    // <p>The first line is line 1.</p>
    //
    // @return An integer representing the line number, or -1
    //         if none is available.
    // @see <a href="../SAX/ILocator.html#getLineNumber">ILocator.getLineNumber</a>
    function getLineNumber() : Integer;

    // The column number of the end of the text where the exception occurred.
    //
    // <p>The first column in a line is position 1.</p>
    //
    // @return An integer representing the column number, or -1
    //         if none is available.
    // @see <a href="../SAX/ILocator.html#getColumnNumber">ILocator.getColumnNumber</a>
    function getColumnNumber() : Integer;

    // Returns the identifier for the exception which is being reported.
    //
    // @return URI identifying the exception.  SAX standardizes the
    //  identifiers for errors relating to the specifications that it relies
    //  on for XML parsing (including <em>XML 1.0</em> and
    //  <em>Namespaces in XML</em>).
    //  See <a href="package-summary.html#exceptions">the <em>SAX</em>
    //  package summary</a> for more information.
    //
    // @since SAX 2.1
    ///
    function getExceptionId() : SAXString;

    // Assigns the identifier for the exception which is being reported,
    // if none has yet been assigned.
    //
    // @param id URI identifying the exception.  SAX standardizes the
    //  identifiers for errors relating to the specifications that it relies
    //  on for XML parsing (including <em>XML 1.0</em> and
    //  <em>Namespaces in XML</em>).
    //  See <a href="package-summary.html#exceptions">the <em>SAX</em>
    //  package summary</a> for more information.
    //
    // @exception ESAXIllegalStateException If the ID was already assigned.
    //
    // @since SAX 2.1
    //
    procedure setExceptionId(const id : SAXString);

    // Initialization method.
    //
    // @param publicId The public identifier of the entity which generated the exception,
    //        or an empty string.
    // @param systemId The system identifier of the entity which generated the exception,
    //        or an empty string.
    // @param lineNumber The line number of the error, or -1.
    // @param columnNumber The column number of the error, or -1.
    procedure init(const publicId, systemId : SAXString; lineNumber,
      columnNumber : Integer);
  end;

  // Exception class for an unrecognized identifier.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>An XMLReader will throw this exception when it finds an
  // unrecognized feature or property identifier; SAX applications and
  // extensions may use this class for other, similar purposes.</p>
  //
  // @since SAX 2.0
  // @see <a href="../SAX/ESAXNotSupportedException.html">ESAXNotSupportedException</a>
  ESAXNotRecognizedException = class(ESAXException);

  // Exception class for an unsupported operation.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>An XMLReader will throw this exception when it recognizes a
  // feature or property identifier, but cannot perform the requested
  // operation (setting a state or value).  Other SAX2 applications and
  // extensions may use this class for similar purposes.</p>
  //
  // @since SAX 2.0
  // @see <a href="../SAX/ESAXNotRecognizedException.html">ESAXNotRecognizedException</a>
  ESAXNotSupportedException = class(ESAXException);

  ESAXIllegalStateException = class(ESAXException);
  ESAXIllegalArgumentException = class(ESAXException);

  IInputSource = interface(IUnknown)
    {$IFDEF SAX_WIDESTRINGS}
    ['{CFB5C09E-93E9-4DB1-BB60-1405E0B71D18}']
    {$ELSE}
    ['{DFAF4F74-8242-4E34-8067-8F92FE3A8755}']
    {$ENDIF}
    procedure setEncoding(const encoding : SAXString);
    function getEncoding() : SAXString;
    procedure setPublicId(const publicId : SAXString);
    function getPublicId() : SAXString;
    procedure setSystemId(const systemId : SAXString);
    function getSystemId() : SAXString;
    property Encoding: SAXString
      read getEncoding write setEncoding;
    property PublicId: SAXString
      read getPublicId write setPublicId;
    property SystemId: SAXString
      read getSystemId write setSystemId;
  end;

  IStreamInputSource = interface(IInputSource)
    {$IFDEF SAX_WIDESTRINGS}
    ['{DD6B99D0-AD5A-4A1A-94CD-23386E7A49FC}']
    {$ELSE}
    ['{E395D618-AAA1-4B3A-BD76-E0AF98F52D2E}']
    {$ENDIF}
    procedure setByteStream(byteStream : TStream);
    function getByteStream() : TStream;
    property ByteStream: TStream
      read getByteStream write setByteStream;
  end;

  // A single input source for an XML entity.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This class allows a SAX application to encapsulate information
  // about an input source in a single object, which may include
  // a public identifier, a system identifier, and/or a byte stream (possibly
  // with a specified encoding).</p>
  //
  // <p>There are two places that the application can deliver an
  // input source to the parser: as the argument to the IParser.parse or
  // IXMLReader.parse method, or as the return value of the
  // IEntityResolver.resolveEntity method.</p>
  //
  // <p>The SAX parser will use the InputSource object to determine how
  // to read XML input.  If there is a byte stream available, the
  // parser will read that stream directly, disregarding any text
  // encoding declaration found in that stream.
  // If there is no character stream, but there is
  // a byte stream, the parser will use that byte stream, using the
  // encoding specified in the InputSource or else (if no encoding is
  // specified) autodetecting the character encoding using an algorithm
  // such as the one in the XML specification.  If neither a character
  // stream nor a
  // a byte stream is available, the parser will attempt to open a URI
  // connection to the resource identified by the system
  // identifier (this may be done using a TFilestream).</p>
  //
  // <p>For alternate InputSources sych as ones which implement byte streams
  // see <a href="../SAX/IStreamInputSource.html">IStreamInputSource</a>. Implementors are
  // encouraged to create alternate InputSources, but should be aware that
  // readers must be created that support them.</p>
  //
  // <p>An InputSource object belongs to the application: the SAX parser
  // shall never modify it in any way (it may modify a copy if
  // necessary). However, standard processing of both byte and
  // character streams is to close them as part of end-of-parse cleanup,
  // which means that the stream will be freed.
  // Applications should not attempt to re-use such streams after they
  // have been handed to a parser.</p>
  //
  // @since SAX 1.0
  // @see <a href="../SAX/IXMLReader.html#parse.IInputSource">IXMLReader.parse(IInputSource)</a>
  // @see <a href="../SAX/IEntityResolver.html#resolveEntity">IEntityResolver.resolveEntity</a>
  TInputSource = class(TInterfacedObject, IInputSource)
  private
    FpublicId : SAXString;
    FsystemId : SAXString;
    Fencoding : SAXString;
  protected

    // Set the public identifier for this input source.
    //
    // <p>The public identifier is always optional: if the application
    // writer includes one, it will be provided as part of the
    // location information.</p>
    //
    // @param publicId The public identifier as a string.
    // @see <a href="../SAX/TInputSource.html#getPublicId">TInputSource.getPublicId</a>
    // @see <a href="../SAX/ILocator.html#getPublicId">ILocator.getPublicId</a>
    // @see <a href="../SAX/ESAXParseException.html#getPublicId">ESAXParseException.getPublicId</a>
    procedure setPublicId(const publicId : SAXString);

    // Get the public identifier for this input source.
    //
    // @return The public identifier, or an empty string if none was supplied.
    // @see <a href="../SAX/TInputSource.html#setPublicId">TInputSource.setPublicId</a>
    function getPublicId() : SAXString;

    // Set the system identifier for this input source.
    //
    // <p>The system identifier is optional if there is a byte stream
    // or a character stream, but it is still useful to provide one,
    // since the application can use it to resolve relative URIs
    // and can include it in error messages and warnings (the parser
    // will attempt to open a connection to the URI only if
    // there is no byte stream specified).</p>
    //
    // <p>If the application knows the character encoding of the
    // object pointed to by the system identifier, it can register
    // the encoding using the setEncoding method.</p>
    //
    // <p>If the system identifier is a URL, it must be fully
    // resolved (it may not be a relative URL).</p>
    //
    // @param systemId The system identifier as a string.
    // @see <a href="../SAX/TInputSource.html#setEncoding">TInputSource.setEncoding</a>
    // @see <a href="../SAX/TInputSource.html#getSystemId">TInputSource.getSystemId</a>
    // @see <a href="../SAX/ILocator.html#getSystemId">ILocator.getSystemId</a>
    // @see <a href="../SAX/ESAXParseException.html#getSystemId">ESAXParseException.getSystemId</a>
    procedure setSystemId(const systemId : SAXString);

    // Get the system identifier for this input source.
    //
    // <p>The getEncoding method will return the character encoding
    // of the object pointed to, or an empty string if unknown.</p>
    //
    // <p>If the system ID is a URL, it will be fully resolved.</p>
    //
    // @return The system identifier, or a blank string if none was supplied.
    // @see <a href="../SAX/TInputSource.html#setSystemId">TInputSource.setSystemId</a>
    // @see <a href="../SAX/TInputSource.html#getEncoding">TInputSource.getEncoding</a>
    function getSystemId() : SAXString;

    // Set the character encoding, if known.
    //
    // <p>The encoding must be a string acceptable for an
    // XML encoding declaration (see section 4.3.3 of the XML 1.0
    // recommendation).</p>
    //
    // @param encoding A string describing the character encoding.
    // @see <a href="../SAX/TInputSource.html#setSystemId">TInputSource.setSystemId</a>
    // @see <a href="../SAX/TInputSource.html#setByteStream">TInputSource.setByteStream</a>
    // @see <a href="../SAX/TInputSource.html#getEncoding">TInputSource.getEncoding</a>
    procedure setEncoding(const encoding : SAXString);

    // Get the character encoding for a byte stream or URI.
    //
    // @return The encoding, or an empty string if none was supplied.
    // @see <a href="../SAX/TInputSource.html#setByteStream">TInputSource.setByteStream</a>
    // @see <a href="../SAX/TInputSource.html#getSystemId">TInputSource.getSystemId</a>
    // @see <a href="../SAX/TInputSource.html#getByteStream">TInputSource.getByteStream</a>
    function getEncoding() : SAXString;

  public
    // Zero-argument default constructor.
    //
    // @see <a href="../SAX/TInputSource.html#setPublicId">TInputSource.setPublicId</a>
    // @see <a href="../SAX/TInputSource.html#setSystemId">TInputSource.setSystemId</a>
    // @see <a href="../SAX/TInputSource.html#setByteStream">TInputSource.setByteStream</a>
    // @see <a href="../SAX/TInputSource.html#setCharacterStream">TInputSource.setCharacterStream</a>
    // @see <a href="../SAX/TInputSource.html#setEncoding">TInputSource.setEncoding</a>
    constructor Create(); overload;

    // Create a new input source with a system identifier.
    //
    // <p>Applications may use setPublicId to include a
    // public identifier as well, or setEncoding to specify
    // the character encoding, if known.</p>
    //
    // <p>If the system identifier is a URL, it must be fully
    // resolved (it may not be a relative URL).</p>
    //
    // @param systemId The system identifier (URI).
    // @see <a href="../SAX/TInputSource.html#setPublicId">TInputSource.setPublicId</a>
    // @see <a href="../SAX/TInputSource.html#setSystemId">TInputSource.setSystemId</a>
    // @see <a href="../SAX/TInputSource.html#setByteStream">TInputSource.setByteStream</a>
    // @see <a href="../SAX/TInputSource.html#setEncoding">TInputSource.setEncoding</a>
    // @see <a href="../SAX/TInputSource.html#setCharacterStream">TInputSource.setCharacterStream</a>
    constructor Create(const systemId : SAXString); overload;

    // Standard default destructor
    //
    // @see <a href="../SAX/TInputSource.html#create">TInputSource.create</a>
    destructor Destroy(); override;

  end;

  // A single stream input source for an XML entity.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This class allows a SAX application to encapsulate information
  // about an input source in a single object, which may include
  // a public identifier, a system identifier, and/or a byte stream (possibly
  // with a specified encoding).</p>
  //
  // <p>An InputSource object belongs to the application: the SAX parser
  // shall never modify it in any way (it may modify a copy if
  // necessary). However, standard processing of both byte and
  // character streams is to close them as part of end-of-parse cleanup,
  // which means that the stream will be freed.
  // Applications should not attempt to re-use such streams after they
  // have been handed to a parser. If StreamOwnership is soOwned then
  // TStreamInputSource frees any streams it has been passed when
  // necassary.</p>
  //
  // @since SAX 1.0
  // @see <a href="../SAX/IXMLReader.html#parse.IInputSource">IXMLReader.parse(IInputSource)</a>
  // @see <a href="../SAX/IEntityResolver.html#resolveEntity">IEntityResolver.resolveEntity</a>
  TStreamInputSource = class(TInputSource, IStreamInputSource)
  private
    // The internal byte stream
    FbyteStream : TStream;
    FstreamOwnership : TStreamOwnership;
  protected
    // Set the byte stream for this input source.
    //
    // <p>The SAX parser will use a byte stream in preference
    // to opening a URI connection itself.</p>
    //
    // <p>If the application knows the character encoding of the
    // byte stream, it should set it with the setEncoding method.</p>
    //
    // <p>If the freeByteStream property is true, the StreamInputSource will
    // free any stream it currently contains</p>
    //
    // @param byteStream A byte stream containing an XML document or
    //        other entity.
    // @see <a href="../SAX/TStreamInputSource.html#setEncoding">TStreamInputSource.setEncoding</a>
    // @see <a href="../SAX/TStreamInputSource.html#getByteStream">TStreamInputSource.getByteStream</a>
    // @see <a href="../SAX/TStreamInputSource.html#getEncoding">TStreamInputSource.getEncoding</a>
    procedure setByteStream(byteStream : TStream);

    // Get the byte stream for this input source.
    //
    // <p>The getEncoding method will return the character
    // encoding for this byte stream, or nil if unknown.</p>
    //
    // @return The byte stream, or nil if none was supplied.
    // @see <a href="../SAX/TStreamInputSource.html#getEncoding">TStreamInputSource.getEncoding</a>
    // @see <a href="../SAX/TStreamInputSource.html#setByteStream">TStreamInputSource.setByteStream</a>
    function getByteStream() : TStream;

  public
    // Create a new input source with a byte stream.
    //
    // <p>Application writers should use setSystemId() to provide a base
    // for resolving relative URIs, may use setPublicId to include a
    // public identifier, and may use setEncoding to specify the object's
    // character encoding.</p>
    //
    // <p><b>The property StreamOwnership is initialized to soReference</b></p>
    //
    // @param byteStream The raw byte stream containing the document.
    // @param streamOwnership Intializes the StreamOwnership property
    // @see <a href="../SAX/TStreamInputSource.html#setPublicId">TStreamInputSource.setPublicId</a>
    // @see <a href="../SAX/TStreamInputSource.html#setSystemId">TStreamInputSource.setSystemId</a>
    // @see <a href="../SAX/TStreamInputSource.html#setEncoding">TStreamInputSource.setEncoding</a>
    // @see <a href="../SAX/TStreamInputSource.html#setByteStream">TStreamInputSource.setByteStream</a>
    // @see <a href="../SAX/TStreamInputSource.html#setCharacterStream">TStreamInputSource.setCharacterStream</a>
    constructor Create(byteStream : TStream;
      streamOwnership : TStreamOwnership = soReference); overload;

    // Standard default destructor
    //
    // <p>If the StreamOwnership property is soOwned, the StreamInputSource will
    // free any stream it currently contains.</p>
    //
    // @see <a href="../SAX/TStreamInputSource.html#create">TStreamInputSource.create</a>
    destructor Destroy(); override;

  end;

  TSAXVendor = class(TObject)
  public
    function Description: string; virtual; abstract;
    function XMLReader: IXMLReader; virtual; abstract;
  end;

  procedure RegisterSAXVendor(Vendor: TSAXVendor);
  procedure UnregisterSAXVendor(Vendor: TSAXVendor);
  procedure ListSAXVendors(List: TStrings);
  function  GetSAXVendor(Name: string = '') : TSAXVendor;

  procedure SAXStringToSAXCharArray(const s : SAXString; var ch : SAXCharArray);
  procedure SAXCharArrayToSAXString(const ch: SAXCharArray; var s: SAXString); overload;
  procedure SAXCharArrayToSAXString(const ch: SAXCharArray; Len : Integer; var s: SAXString); overload;

  function PSAXCharToSAXString(PCh: PSAXChar; Length: Integer) : SAXString;
  function PSAXCharToString(CharsP: PSAXChar; Len: Integer): String;

  // Get the length of a PSAXChar. Precondition: CharsP <> nil
  function SAXStrLen(CharsP: PSAXChar): Integer;

  // Tests two Null-terminated PSAXChars for equality
  function SAXStrEqual(CharsP1, CharsP2: PSAXChar): Boolean; overload;

  // Tests two PSAXChars of equal length, or one null-terminated
  // PSAXChars and one PSAXChar of Len characters for equality
  // Precondition: if any of the two strings is not null-terminated,
  // then that string must have at least Len characters. Do not send
  // -1 as the Length to this function-- it will always return false
  // Returns True if both pointers are equal, without checking Len!
  function SAXStrEqual(CharsP1, CharsP2: PSAXChar; Len: Integer): Boolean; overload;

  // Tests two PSAXChars for equality. The Len1 and Len2 parameters
  // may or may not be -1 indicating the respective PSAXChar is null-terminated
  function SAXStrEqual(CharsP1: PSAXChar; Len1 : Integer; CharsP2: PSAXChar;
    Len2: Integer): Boolean; overload;


resourcestring

  SUnknownVendor = 'Unknown SAX Vendor %s';

var
  DefaultSAXVendor: string;

implementation

{ Vendors }

var
  SAXVendors: TStringList;

procedure RegisterSAXVendor(Vendor: TSAXVendor);
var
  Index: Integer;
begin
  Index := SAXVendors.IndexOf(Vendor.Description);
  if Index = -1 then
    SAXVendors.AddObject(Vendor.Description, Vendor)
  else
  begin
    SAXVendors.Objects[Index].Free;
    SAXVendors.Objects[Index]:= Vendor;
  end;
end;

procedure UnregisterSAXVendor(Vendor: TSAXVendor);
var
  Index: Integer;
begin
  Index := SAXVendors.IndexOf(Vendor.Description);
  if Index > -1 then
  begin
    SAXVendors.Objects[Index].Free;
    SAXVendors.Delete(Index);
  end;
end;

procedure ListSAXVendors(List: TStrings);
var
  Index: Integer;
begin
  List.Clear;
  for Index := 0 to SAXVendors.Count - 1 do
    List.AddObject(SAXVendors[Index], SAXVendors.Objects[Index]); 
end;

function GetSAXVendor(Name: string): TSAXVendor;
var
  Index: Integer;
begin
  if Name = '' then
    Name := DefaultSAXVendor;
  Index := SAXVendors.IndexOf(Name);
  if Index = -1 then
    raise ESAXException.CreateFmt(sUnknownVendor, [Name])
  else
    Result := TSAXVendor(SAXVendors.Objects[Index]);
end;

procedure ClearVendors;
var
  Index: Integer;
begin
  for Index := 0 to SAXVendors.Count - 1 do
    SAXVendors.Objects[Index].Free;
end;

procedure SAXStringToSAXCharArray(const s: SAXString;
  var ch: SAXCharArray);
begin
  SetLength(ch, length(s));
  move(pointer(s)^, pointer(ch)^, length(s)*SizeOf(SAXChar));
end;

procedure SAXCharArrayToSAXString(const ch: SAXCharArray;
  var s: SAXString);
begin
  SetLength(s, Length(ch));
  move(pointer(ch)^, pointer(s)^, Length(s) * SizeOf(SAXChar));
end;

procedure SAXCharArrayToSAXString(const ch: SAXCharArray;
  Len : Integer; var s: SAXString);
begin
  SetLength(s, Len);
  move(pointer(ch)^, pointer(s)^, Length(s) * SizeOf(SAXChar));
end;

function PSAXCharToSAXString(PCh: PSAXChar; Length: Integer): SAXString; overload;
begin
  // SetString can most likely be used with WideChar's though the documentation
  // is unclear
  if (Length <> -1) then
  begin
    SetLength(Result, Length);
    Move(PCh^, Pointer(Result)^, Length * SizeOf(SAXChar));
  end else
    Result:= PCh;
end;

function PSAXCharToString(CharsP: PSAXChar; Len: Integer): String;
begin
{$IFDEF SAX_WIDESTRINGS}
  if Len = -1 then Len := SAXStrLen(CharsP);
  Result := WideCharLenToString(PWideChar(CharsP), Len);
{$ELSE}
  if Len = -1 then Result := CharsP
  else SetString(Result, CharsP, Len);
{$ENDIF}
end;

function SAXStrLen(CharsP: PSAXChar): Integer;
var
  EndCharP: PSAXChar;
begin
  EndCharP := CharsP;
  while EndCharP^ <> XML_NULLCHAR do Inc(EndCharP);
  Result := EndCharP - CharsP;
end;

function SAXStrEqual(CharsP1, CharsP2: PSAXChar): Boolean;
begin
  Result := CharsP1 = CharsP2;
  if not Result and (CharsP1 <> nil) and (CharsP2 <> nil) then
  begin
    while CharsP1^ = CharsP2^ do
    begin
      if CharsP1^ = XML_NULLCHAR then
      begin
        Result := True;
        Exit;
      end;
      Inc(CharsP1);
      Inc(CharsP2);
    end;
  end;
end;  

function SAXStrEqual(CharsP1, CharsP2: PSAXChar; Len: Integer): Boolean;
var
  EndP: PSAXChar;
begin
  Result := CharsP1 = CharsP2;
  if not Result then
  begin
    //it seems the compiler performs correct pointer arithmetic here
    EndP := CharsP1 + Len;
    while CharsP1 <> EndP do
    begin
      if CharsP1^ <> CharsP2^ then Exit;
      Inc(CharsP1);
      Inc(CharsP2);
    end;
    Result := True;
  end;
end;

function SAXStrEqual(CharsP1: PSAXChar; Len1 : Integer; CharsP2:
  PSAXChar;  Len2: Integer): Boolean;
begin
  if Len1 = Len2 then
  begin
    if Len1 = -1 then
      Result:= SAXStrEqual(CharsP1, CharsP2)
    else
      Result:= SAXStrEqual(CharsP1, CharsP2, Len1);
  end
  else if Len1 = -1 then
    // treat CharsP1 as the null-term
    Result:= SAXStrEqual(CharsP1, CharsP2, Len2)
  else if Len2 = -1 then
    // treat CharsP2 as the null-term
    Result:= SAXStrEqual(CharsP2, CharsP1, Len1)
  else
    Result := False;
end;

{ ESAXException }

constructor ESAXException.Create(const e: ISAXError);
begin
  inherited Create(PSAXCharToString(e.getMessage(), -1));
end;

function ESAXException.getMessage: SAXString;
begin
  Result:= SAXString(message);
end;

{ ESAXParseException }

constructor ESAXParseException.Create(const e: ISAXParseError);
begin
  inherited Create(e);  //extracts the message
  init(e.getPublicId(), e.getSystemId(), e.getLineNumber(), e.getColumnNumber());
end;

constructor ESAXParseException.Create(const message, publicId,
  systemId: SAXString; lineNumber, columnNumber: Integer);
begin
  inherited Create(message);
  init(publicId, systemId, lineNumber, columnNumber);
end;

constructor ESAXParseException.Create(const message: SAXString;
  const locator: ILocator);
begin
  inherited Create(message);
  if locator <> nil then
    with locator do
      init(getPublicId(), getSystemId(), getLineNumber(), getColumnNumber())
  else
    init('', '', -1, -1);
end;

function ESAXParseException.getPublicId: PSAXChar;
begin
  Result:= PSAXChar(FpublicId);
end;

function ESAXParseException.getSystemId: PSAXChar;
begin
  Result:= PSAXChar(FsystemId);
end;

function ESAXParseException.getLineNumber: Integer;
begin
  Result:= FlineNumber;
end;

function ESAXParseException.getColumnNumber: Integer;
begin
  Result:= FcolumnNumber;
end;

function ESAXParseException.getExceptionId: SAXString;
begin
  Result:= FexceptionId;
end;

procedure ESAXParseException.setExceptionId(const id : SAXString);
begin
  if (FexceptionId = '') then
    FexceptionId:= id
  else
    raise ESAXIllegalStateException.Create('');
end;

procedure ESAXParseException.init(const publicId, systemId: SAXString;
  lineNumber, columnNumber: Integer);
begin
  FExceptionId:= '';
  FpublicId:= publicId;
  FsystemId:= systemId;
  FlineNumber:= lineNumber;
  FcolumnNumber:= columnNumber;
end;

{ TInputSource }

constructor TInputSource.Create;
begin
  inherited Create();
end;

constructor TInputSource.Create(const systemId: SAXString);
begin
  inherited Create();
  setSystemId(systemId);
end;

destructor TInputSource.destroy;
begin
  inherited Destroy();
end;

procedure TInputSource.setPublicId(const publicId: SAXString);
begin
  FpublicId:= publicId;
end;

function TInputSource.getPublicId: SAXString;
begin
  Result:= FpublicId
end;

procedure TInputSource.setSystemId(const systemId: SAXString);
begin
  FsystemId:= systemId;
end;

function TInputSource.getSystemId: SAXString;
begin
  Result:= FsystemId;
end;

procedure TInputSource.setEncoding(const encoding: SAXString);
begin
  Fencoding:= encoding;
end;

function TInputSource.getEncoding: SAXString;
begin
  Result:= Fencoding;
end;

{ TStreamInputSource }

constructor TStreamInputSource.Create(byteStream: TStream;
  streamOwnership : TStreamOwnership = soReference);
begin
  inherited Create();
  FstreamOwnership:= streamOwnership;
  setByteStream(byteStream);
end;

procedure TStreamInputSource.setByteStream(byteStream: TStream);
begin
  if (FstreamOwnership = soOwned) then
    FbyteStream.Free;

  FbyteStream:= byteStream;
end;

function TStreamInputSource.getByteStream: TStream;
begin
  Result:= FbyteStream;
end;

destructor TStreamInputSource.destroy;
begin
  if (FstreamOwnership = soOwned) then
    FbyteStream.Free;
  inherited;
end;

initialization
  SAXVendors := TStringList.Create;

finalization
  ClearVendors;
  SAXVendors.Free;

end.

