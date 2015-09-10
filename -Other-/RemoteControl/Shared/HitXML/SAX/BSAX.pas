// SAX for Pascal, Simple API for XML Buffered Interfaces in Pascal.
// Ver 1.1 July 4, 2003
// http://xml.defined.net/SAX (this will change!)
// Based on http://www.saxproject.org/
// No warranty; no copyright -- use this as you will.
unit BSAX;

interface

uses Classes, SysUtils, SAX;

const

  {$IFDEF SAX_WIDESTRINGS}
  IID_IBufferedAttributes = '{B00262C6-2AEB-41B5-9D9F-0C952DDC382C}';
  IID_IBufferedContentHandler = '{0B0C23E2-7C9C-483E-939C-C037D3C51392}';
  IID_IBufferedDTDHandler = '{FC72C5C4-B218-41E6-B766-A5DE61BF700E}';
  IID_IBufferedXMLReader = '{23F9C412-EDF7-4318-8A7F-621B9B586DE9}';
  IID_IBufferedXMLFilter = '{2C93D9AC-8938-457D-9996-A52726A17FD6}';
  {$ELSE}
  IID_IBufferedAttributes = '{4376FBEB-EB18-4509-8A7F-F8D5571D9A85}';
  IID_IBufferedContentHandler = '{F5B17559-C64E-42AE-83F5-A9E0DB9300F7}';
  IID_IBufferedDTDHandler = '{D99C7109-BAD8-4685-AB4B-439E4ABEC9F3}';
  IID_IBufferedXMLReader = '{8810E2B8-0D20-4057-BBF5-841010B397AB}';
  IID_IBufferedXMLFilter = '{DBFFB137-8706-46B5-BA25-2AFDCFDBAC14}';
  {$ENDIF}

type

  // Default Buffered Interfaces
  IBufferedAttributes = interface;
  IBufferedContentHandler = interface;
  IBufferedDTDHandler = interface;
  IBufferedXMLReader = interface;
  IBufferedXMLFilter = interface;

  // Default Vendor Class
  TBufferedSAXVendor = class;

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
  // <p>This interface replaces the now-deprecated SAX1 AttributeList
  // interface, which does not contain Namespace support.
  // In addition to Namespace support, it adds the <var>getIndex</var>
  // methods (below).</p>
  //
  // <p>The order of attributes in the list is unspecified, and will
  // vary from implementation to implementation.</p>
  //
  // <p>For reasons of generality and efficiency, strings that are returned
  // from the interface are declared as pointers (PSAXChar) and lengths.
  // This requires that the model use procedural out parameters rather
  // than functions as in the original interfaces.</p>
  //
  // @since SAX 2.0
  // @see <a href="../BSAXExt/IBufferedDeclHandler.html#attributeDecl">IBufferedDeclHandler.attributeDecl</a>
  IBufferedAttributes = interface(IUnknown)
    [IID_IBufferedAttributes]

    // Return the number of attributes in the list.
    //
    // <p>Once you know the number of attributes, you can iterate
    // through the list.</p>
    //
    // @return The number of attributes in the list.
    // @see <a href="../BSAX/IBufferedAttributes.html#getURI.Integer.PSAXChar.Integer">IBufferedAttributes.getURI(Integer,PSAXChar,Integer)</a>
    // @see <a href="../BSAX/IBufferedAttributes.html#getLocalName.Integer.PSAXChar.Integer">IBufferedAttributes.getLocalName(Integer,PSAXChar,Integer)</a>
    // @see <a href="../BSAX/IBufferedAttributes.html#getQName.Integer.PSAXChar.Integer">IBufferedAttributes.getQName(Integer,PSAXChar,Integer)</a>
    // @see <a href="../BSAX/IBufferedAttributes.html#getType.Integer.PSAXChar.Integer">IBufferedAttributes.getType(Integer,PSAXChar,Integer)</a>
    // @see <a href="../BSAX/IBufferedAttributes.html#getValue.Integer.PSAXChar.Integer">IBufferedAttributes.getValue(Integer,PSAXChar,Integer)</a>
    function getLength() : Integer;

    // Look up an attribute's Namespace URI by index.
    //
    // @param index The attribute index (zero-based).
    // @param uri The Namespace URI, or the empty string if none
    //         is available or if the index is out of range.
    // @param uriLength The length of the Namespace URI buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getLength">IBufferedAttributes.getLength</a>
    procedure getURI(index : Integer;
      out uri: PSAXChar; out uriLength: Integer);

    // Look up an attribute's local name by index.
    //
    // @param index The attribute index (zero-based).
    // @param localName The local name, or the empty string if Namespace
    //         processing is not being performed or if the
    //         index is out of range.
    // @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getLength">IBufferedAttributes.getLength</a>
    procedure getLocalName(index : Integer;
     out localName: PSAXChar; out localNameLength: Integer);

    // Look up an attribute's XML 1.0 qualified name by index.
    //
    // @param index The attribute index (zero-based).
    // @param qName The XML 1.0 qualified name, or the empty string
    //         if none is available or if the index is out of
    //         range.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getLength">IBufferedAttributes.getLength</a>
    procedure getQName(index : Integer;
      out qName: PSAXChar; out qNameLength: Integer);

    // Look up an attribute's type by index.
    //
    // <p>The attribute type is one of the strings "CDATA", "ID",
    // "IDREF", "IDREFS", "NMTOKEN", "NMTOKENS", "ENTITY", "ENTITIES",
    // or "NOTATION" (always in upper case).</p>
    //
    // <p>If the parser has not read a declaration for the attribute,
    // or if the parser does not report attribute types, then it must
    // return the value "CDATA" as stated in the XML 1.0 Recommendation
    // (clause 3.3.3, "Attribute-Value Normalization").</p>
    //
    // <p>For an enumerated attribute that is not a notation, the
    // parser will report the type as "NMTOKEN".</p>
    //
    // @param index The attribute index (zero-based).
    // @param attType The attribute's type as a string or an empty string if the
    //         index is out of range.
    // @param attTypeLength The length of the attType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getLength">IBufferedAttributes.getLength</a>
    procedure getType(index : Integer;
      out attType: PSAXChar; out attTypeLength: Integer); overload;

    // Look up an attribute's type by XML 1.0 local name and Namespace URI.
    //
    // <p>See <a href="../BSAX/IBufferedAttributes.html#getType.Integer.PSAXChar.Integer">getType(Integer, PSAXChar, Integer)</a> for a description
    // of the possible types.</p>
    //
    // @param uri The Namespace uri
    // @param uriLength The length of the uri Buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param localName The XML 1.0 local name
    // @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param uri The Namespace uri
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param attType The attribute's type as a string or an empty string if the
    //         attribute is not in the list
    // @param attTypeLength The length of the attType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    procedure getType(uri: PSAXChar; uriLength: Integer;
      localName: PSAXChar;  localNameLength: Integer;
      out attType: PSAXChar; out attTypeLength: Integer); overload;

    //  Look up an attribute's type by XML 1.0 qualified name.
    //
    //  <p>See <a href="../BSAX/IBufferedAttributes.html#getType.Integer.PSAXChar.Integer">getType(Integer, PSAXChar, Integer)</a>
    // for a description of the possible types.</p>
    //
    //  @param qName The XML 1.0 qualified name.
    //  @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    //  @param attType The attribute type as a string, or an empty string if the
    //          attribute is not in the list or if qualified names
    //          are not available.
    //  @param attTypeLength The length of the attType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    procedure getType(qName: PSAXChar;  qNameLength: Integer;
      out attType: PSAXChar; out attTypeLength: Integer); overload;

    //  Look up an attribute's value by index.
    //
    //  <p>If the attribute value is a list of tokens (IDREFS,
    //  ENTITIES, or NMTOKENS), the tokens will be concatenated
    //  into a single string with each token separated by a
    //  single space.</p>
    //
    //  @param index The attribute index (zero-based).
    //  @param value The attribute's value as a string, or an empty string if the
    //          index is out of range.
    //  @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getLength">IBufferedAttributes.getLength</a>
    procedure getValue(index : Integer;
       out value: PSAXChar; out valueLength: Integer); overload;

    //  Look up an attribute's value by Namespace name.
    //
    //  <p>See <a href="../BSAX/IBufferedAttributes.html#getValue.Integer.PSAXChar.Integer)">getValue(Integer, PSAXChar, Integer)</a>
    // for a description of the possible values.</p>
    //
    //  @param uri The Namespace URI, or the empty String if the
    //         name has no Namespace URI.
    //  @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    //  @param localName The local name of the attribute.
    //  @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    //  @param value The attribute value as a string, or an empty string if the
    //          attribute is not in the list.
    //  @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    procedure getValue(uri : PSAXChar; uriLength : Integer;
       localName : PSAXChar; localNameLength : Integer;
       out value: PSAXChar; out valueLength: Integer); overload;

    //  Look up an attribute's value by XML 1.0 QName
    //
    //  <p>See <a href="../BSAX/IBufferedAttributes.html#getValue.Integer.PSAXChar.Integer">getValue(Integer, PSAXChar, Integer)</a>
    // for a description of the possible values.</p>
    //
    //  @param qName The qualified (prefixed) name.
    //  @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    //  @param value The attribute value as a string, or an empty string if the
    //          attribute is not in the list.
    //  @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    procedure getValue(qName : PSAXChar; qNameLength : Integer;
       out value: PSAXChar; out valueLength: Integer); overload;

    //  Look up the index of an attribute by Namespace name.
    //
    //  @param uri The Namespace URI, or the empty string if
    //         the name has no Namespace URI.
    //  @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    //  @param localName The attribute's local name.
    //  @param loclaNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    //  @return The index of the attribute, or -1 if it does not
    //          appear in the list.
    function getIndex(uri : PSAXChar; uriLength : Integer;
       localName : PSAXChar; localNameLength : Integer) : Integer; overload;

    //  Look up the index of an attribute by XML 1.0 qualified name.
    //
    //  @param qName The qualified (prefixed) name.
    //  @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    //  @return The index of the attribute, or -1 if it does not
    //          appear in the list.
    function getIndex(qName : PSAXChar;
      qNameLength : Integer) : Integer; overload;

    // Extension property to get the number of Attributes
    //
    // @return The number of attributes in the list.
    // @see <a href="../BSAX/IBufferedAttributes.html#getLength">IBufferedAttributes.getLength</a>
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
  // the SAX parser using the <a href="../BSAX/IBufferedXMLReader.html#setContentHandler">IBufferedXMLReader</a>
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
  // <p>For reasons of generality and efficiency, strings that are returned
  // from the interface are declared as pointers (PSAXChar) and lengths.
  // This requires that the model use procedural out parameters rather
  // than functions as in the original interfaces.</p>
  //
  // @since SAX 2.0
  // @see <a href="../BSAX/IBufferedXMLReader.html">IBufferedXMLReader</a>
  // @see <a href="../BSAX/IBufferedDTDHandler.html">IBufferedDTDHandler</a>
  // @see <a href="../SAX/IErrorHandler.html">IErrorHandler</a>
  IBufferedContentHandler = interface(IUnknown)
    [IID_IBufferedContentHandler]

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
    // <a href="../BSAX/IBufferedContentHandler.html#startDocument">startDocument</a> returns and before
    // <a href="../BSAX/IBufferedContentHandler.html#endDocument">endDocument</a> is called.  The
    // application should not attempt to use it at any other time.</p>
    //
    // @param locator An object that can return the location of
    //                any SAX document event.
    // @see <a href="../BSAX/IBufferedLocator.html">IBufferedLocator</a>
    procedure setDocumentLocator(const locator: ILocator);

    // Receive notification of the beginning of a document.
    //
    // <p>The SAX parser will invoke this method only once, before any
    // other event callbacks (except for <a href="../BSAX/IBufferedContentHandler.html#setDocumentLocator">setDocumentLocator</a>).</p>
    //
    // @exception ESAXException Any SAX exception.
    
    // @see <a href="../BSAX/IBufferedContentHandler.html#endDocument">IBufferedContentHandler.endDocument</a>
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
    
    // @see <a href="../BSAX/IBufferedContentHandler.html#startDocument">IBufferedContentHandler.startDocument</a>
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
    // corresponding <a href="../BSAX/IBufferedContentHandler.html#startElement">startElement</a> event,
    // and all <a href="../BSAX/IBufferedContentHandler.html#endPrefixMapping">endPrefixMapping</a>
    // events will occur immediately after the corresponding
    // <a href="../BSAX/IBufferedContentHandler.html#endElement">endElement</a> event
    // but their order is not otherwise
    // guaranteed.</p>
    //
    // <p>There should never be start/endPrefixMapping events for the
    // "xml" prefix, since it is predeclared and immutable.</p>
    //
    // @param prefix The Namespace prefix being declared.
    //               An empty string is used for the default element namespace,
    //               which has no prefix.
    // @param prefixLength The length of the prefix buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param uri The Namespace URI the prefix is mapped to.
    // @param uriLength The length of the uri buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    // @see <a href="../BSAX/IBufferedContentHandler.html#endPrefixMapping">IBufferedContentHandler.endPrefixMapping</a>
    // @see <a href="../BSAX/IBufferedContentHandler.html#startElement">IBufferedContentHandler.startElement</a>
    procedure startPrefixMapping(prefix: PSAXChar;
      prefixLength: Integer; uri: PSAXChar;
      uriLength: Integer);

    // End the scope of a prefix-URI mapping.
    //
    // <p>See <a href="../BSAX/IBufferedContentHandler.html#startPrefixMapping">startPrefixMapping</a> for
    // details.  These events will always occur immediately after the
    // corresponding <a href="../BSAX/IBufferedContentHandler.html#endElement">endElement</a> event, but the order of
    // <a href="../BSAX/IBufferedContentHandler.html#endPrefixMapping">endPrefixMapping</a> events is not otherwise
    // guaranteed.</p>
    //
    // @param prefix The prefix that was being mapped.
    //               Use the empty string when a default mapping scope ends.
    // @param prefixLength The length of the prefix buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    // @see <a href="../BSAX/IBufferedContentHandler.html#startPrefixMapping">IBufferedContentHandler.startPrefixMapping</a>
    // @see <a href="../BSAX/IBufferedContentHandler.html#endElement">IBufferedContentHandler.endElement</a>
    procedure endPrefixMapping(prefix: PSAXChar;
      prefixLength: Integer);

    // Receive notification of the beginning of an element.
    //
    // <p>The Parser will invoke this method at the beginning of every
    // element in the XML document; there will be a corresponding
    // <a href="../BSAX/IBufferedContentHandler.html#endElement">endElement</a> event for every startElement event
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
    // <p>Like <a href="../BSAX/IBufferedContentHandler.html#characters">characters</a>, attribute values may have
    // characters that need more than one <code>char</code> value.</p>
    //
    // @param uri The Namespace URI, or the empty string if the
    //        element has no Namespace URI or if Namespace
    //        processing is not being performed.
    // @param uriLength The length of the uri buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param localName The local name (without prefix), or the
    //        empty string if Namespace processing is not being
    //        performed.
    // @param localNameLength The length of the localName buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param qName The qualified name (with prefix), or the
    //        empty string if qualified names are not available.
    // @param qNameLength The length of the qName buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param atts The attributes attached to the element.  If
    //        there are no attributes, it shall be an empty
    //        IBufferedAttributes object or nil.
    // @exception ESAXException Any SAX exception.
    
    // @see <a href="../BSAX/IBufferedContentHandler.html#endElement">IBufferedContentHandler.endElement</a>
    // @see <a href="../BSAX/IBufferedAttributes.html">IBufferedAttributes</a>
    procedure startElement(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer;
      qName : PSAXChar; qNameLength : Integer;
      const atts: IBufferedAttributes);

    // Receive notification of the end of an element.
    //
    // <p>The SAX parser will invoke this method at the end of every
    // element in the XML document; there will be a corresponding
    // <a href="../BSAX/IBufferedContentHandler.html#startElement">startElement</a> event for every endElement
    // event (even when the element is empty).</p>
    //
    // <p>For information on the names, see startElement.</p>
    //
    // @param uri The Namespace URI, or the empty string if the
    //        element has no Namespace URI or if Namespace
    //        processing is not being performed.
    // @param uriLength The length of the uri buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param localName The local name (without prefix), or the
    //        empty string if Namespace processing is not being
    //        performed.
    // @param localNameLength The length of the localName buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param qName The qualified name (with prefix), or the
    //        empty string if qualified names are not available.
    // @param qNameLength The length of the qName buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException Any SAX exception.
    procedure endElement(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer;
      qName : PSAXChar; qNameLength : Integer);

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
    // content using the <a href="../BSAX/IBufferedContentHandler.html#ignorableWhitespace">ignorableWhitespace</a>
    // method rather than this one (validating parsers <em>must</em>
    // do so).</p>
    //
    // @param ch Pointer to the characters from the XML document.
    // @param chLength The length of the ch buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException Any SAX exception.

    // @see <a href="../BSAX/IBufferedContentHandler.html#ignorableWhitespace">IBufferedContentHandler.ignorableWhitespace</a>
    // @see <a href="../SAX/ILocator.html">ILocator</a>
    procedure characters(ch : PSAXChar; chLength : Integer);

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
    // @param ch Pointer to the characters from the XML document.
    // @param chLength The length of the ch buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException Any SAX exception.

    // @see <a href="../BSAX/IBufferedContentHandler.html#characters">IBufferedContentHandler.characters</a>
    procedure ignorableWhitespace(ch : PSAXChar; chLength : Integer);

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
    // <p>Like <a href="../BSAX/IBufferedContentHandler.html#characters">characters</a>, processing instruction
    // data may have characters that need more than one <code>char</code>
    // value. </p>
    //
    // @param target The processing instruction target.
    // @param targetLength The length of the target buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param data The processing instruction data, or the empty string
    //        if none was supplied.  The data does not include any
    //        whitespace separating it from the target.
    // @param dataLength The length of the data buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException Any SAX exception.

    procedure processingInstruction(target : PSAXChar;
      targetLength : Integer; data : PSAXChar;
      dataLength : Integer);

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
    // @param nameLength The length of the name buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException Any SAX exception.

    procedure skippedEntity(name : PSAXChar; nameLength : Integer);

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
  // If the <a href="../BSAXExt/IBufferedLexicalHandler.html">IBufferedLexicalHandler</a> is
  // used, these events must also be reported before the endDTD event.)</p>
  //
  // <p>It is up to the application to store the information for
  // future use (perhaps in a hash table or object tree).
  // If the application encounters attributes of type "NOTATION",
  // "ENTITY", or "ENTITIES", it can use the information that it
  // obtained through this interface to find the entity and/or
  // notation corresponding with the attribute value.</p>
  //
  // <p>For reasons of generality and efficiency, strings that are returned
  // from the interface are declared as pointers (PSAXChar) and lengths.
  // This requires that the model use procedural out parameters rather
  // than functions as in the original interfaces.</p>
  //
  // @since SAX 1.0
  // @see <a href="../BSAX/IBufferedXMLReader.html#setDTDHandler">IBufferedXMLReader.setDTDHandler</a>
  IBufferedDTDHandler = interface(IUnknown)
    [IID_IBufferedDTDHandler]

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
    // @param nameLength The length of the name buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param publicId The notation's public identifier, or empty if
    //        none was given.
    // @param publicIdLength The length of the publicId buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param systemId The notation's system identifier, or empty if
    //        none was given.
    // @param systemIdLength The length of the systemId buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException Any SAX exception.
    
    // @see <a href="../BSAX/IBufferedDTDHandler.html#unparsedEntityDecl">IBufferedDTDHandler.unparsedEntityDecl</a>
    // @see <a href="../BSAX/IBufferedAttributes.html">IBufferedAttributes</a>
    procedure notationDecl(name : PSAXChar; nameLength : Integer;
      publicId : PSAXChar; publicIdLength : Integer;
      systemId : PSAXChar; systemIdLength : Integer);

    // Receive notification of an unparsed entity declaration event.
    //
    // <p>Note that the notation name corresponds to a notation
    // reported by the <a href="../BSAX/IBufferedDTDHandler.html#notationDecl">notationDecl</a> event.
    // It is up to the application to record the entity for later
    // reference, if necessary;
    // unparsed entities may appear as attribute values.</p>
    //
    // <p>If the system identifier is a URL, the parser must resolve it
    // fully before passing it to the application.</p>
    //
    // @exception ESAXException Any SAX exception.

    // @param name The unparsed entity's name.
    // @param nameLength The length of the name buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param publicId The entity's public identifier, or empty if none
    //        was given.
    // @param publicIdLength The length of the publicId buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param systemId The entity's system identifier.
    // @param systemIdLength The length of the systemId buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param notationName The name of the associated notation.
    // @param notationNameLength The length of the notationNameLength buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedDTDHandler.html#notationDecl">IBufferedDTDHandler.notationDecl</a>
    // @see <a href="../BSAX/IBufferedAttributes.html">IBufferedAttributes</a>
    procedure unparsedEntityDecl(name : PSAXChar;
      nameLength : Integer; publicId : PSAXChar;
      publicIdLength : Integer; systemId : PSAXChar;
      systemIdLength : Integer; notationName : PSAXChar;
      notationNameLength : Integer);

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
  // <p>All SAX interfaces are assumed to be synchronous: the
  // <a href="../BSAX/IBufferedXMLReader.html#parse">parse</a> methods must not return until parsing
  // is complete, and readers must wait for an event-handler callback
  // to return before reporting the next event.</p>
  //
  // <p>This interface replaces the (now deprecated) SAX 1.0 <a href="../SAX/IParser.html">IParser</a>
  // interface.  The XMLReader interface contains two important enhancements over the old Parser
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
  // <p>For reasons of generality and efficiency, strings that are returned
  // from the interface are declared as pointers (PSAXChar) and lengths.
  // This requires that the model use procedural out parameters rather
  // than functions as in the original interfaces.</p>
  //
  // @since SAX 2.0
  // @see <a href="../BSAX/IBufferedXMLFilter.html">IBufferedXMLFilter</a>
  IBufferedXMLReader = interface(IBaseXMLReader)
    [IID_IBufferedXMLReader]

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
    // @see <a href="../BSAX/IBufferedXMLReader.html#getDTDHandler">IBufferedXMLReader.getDTDHandler</a>
    procedure setDTDHandler(const handler : IBufferedDTDHandler);

    // Return the current DTD handler.
    //
    // @return The current DTD handler, or null if none
    //         has been registered.
    // @see <a href="../BSAX/IBufferedXMLReader.html#setDTDHandler">IBufferedXMLReader.setDTDHandler</a>
    function getDTDHandler() : IBufferedDTDHandler;

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
    // @see <a href="../BSAX/IBufferedXMLReader.html#getContentHandler">IBufferedXMLReader.getContentHandler</a>
    procedure setContentHandler(const handler : IBufferedContentHandler);

    // Return the current content handler.
    //
    // @return The current content handler, or nil if none
    //         has been registered.
    // @see <a href="../BSAX/IBufferedXMLReader.html#setContentHandler">IBufferedXMLReader.setContentHandler</a>
    function getContentHandler() : IBufferedContentHandler;

    // Extension property to get and set the ContentHandler
    //
    // @return The current content handler, or nil if none
    //         has been registered.
    // @see <a href="../BSAX/IBufferedXMLReader.html#getContentHandler">IBufferedXMLReader.getContentHandler</a>
    // @see <a href="../BSAX/IBufferedXMLReader.html#setContentHandler">IBufferedXMLReader.setContentHandler</a>
    property ContentHandler: IBufferedContentHandler
      read getContentHandler write setContentHandler;

    // Extension property to get and set the DTDHandler
    //
    // @return The current DTD handler, or nil if none
    //         has been registered.
    // @see <a href="../BSAX/IBufferedXMLReader.html#getDTDHandler">IBufferedXMLReader.getDTDHandler</a>
    // @see <a href="../BSAX/IBufferedXMLReader.html#setDTDHandler">IBufferedXMLReader.setDTDHandler</a>
    property DTDHandler: IBufferedDTDHandler
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
  // for creating SAX2 filters, by passing on all <a href="../SAX/IEntityResolver.html">IEntityResolver</a>,
  // <a href="../BSAX/IBufferedDTDHandler.html">IBufferedDTDHandler</a>,
  // <a href="../BSAX/IBufferedContentHandler.html">IBufferedContentHandler</a> and
  // <a href="../SAX/IErrorHandler.html">IErrorHandler</a> events automatically.</p>
  //
  // @since SAX 2.0
  // @see <a href="../BSAXHelpers/TBufferedXMLFilterImpl.html">TBufferedXMLFilterImpl</a>
  IBufferedXMLFilter = interface(IBufferedXMLReader)
    [IID_IBufferedXMLFilter]

    // Set the parent reader.
    //
    // <p>This method allows the application to link the filter to
    // a parent reader (which may be another filter).  The argument
    // may not be null.</p>
    //
    // @param parent The parent reader.
    procedure setParent(const parent : IBufferedXMLReader);

    // Get the parent reader.
    //
    // <p>This method allows the application to query the parent
    // reader (which may be another filter).  It is generally a
    // bad idea to perform any operations on the parent reader
    // directly: they should all pass through this filter.</p>
    //
    // @return The parent filter, or nil if none has been set.
    function getParent() : IBufferedXMLReader;

    // Extension property to get and set the IBufferedXMLFilters's parent
    //
    // @return The parent reader.
    // @see <a href="../BSAX/IBufferedXMLFilter.html#getParent">IBufferedXMLFilter.getParent</a>
    // @see <a href="../BSAX/IBufferedXMLFilter.html#setParent">IBufferedXMLFilter.setParent</a>
    property Parent: IBufferedXMLReader
      read getParent write setParent;
  end;

  TBufferedSAXVendor = class(TSAXVendor)
  public
    function BufferedXMLReader: IBufferedXMLReader; virtual; abstract;
  end;

implementation

end.

