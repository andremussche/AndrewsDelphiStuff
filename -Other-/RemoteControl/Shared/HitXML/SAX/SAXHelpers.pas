// SAX for Pascal Helper Classes, Simple API for XML Interfaces in Pascal.
// Ver 1.1 July 4, 2003
// http://xml.defined.net/SAX (this will change!)
// Based on http://www.saxproject.org/
// No warranty; no copyright -- use this as you will.
unit SAXHelpers;

interface

uses Classes, SysUtils, SAX, SAXExt;

type

  // Helper Classes
  TAttributesImpl = class;
  TDefaultHandler = class;
  TLocatorImpl = class;
  TNamespaceContext = class;
  TNamespaceSupport = class;
  TXMLFilterImpl = class;
  TXMLReaderImpl = class;

  // Extension Helper Classes
  TAttributes2Impl = class;
  TDefaultHandler2 = class;
  TLocator2Impl = class;

  // Error Helper Classes
  TSAXError = class;
  TSAXParseError = class;
  TSAXNotRecognizedError = class;
  TSAXNotSupportedError = class;
  TSAXIllegalStateError = class;
  TSAXIllegalArgumentError = class;

  // Default implementation of the Attributes interface.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This class provides a default implementation of the SAX2
  // <a href="../SAX/IAttributes.html">IAttributes</a> interface, with the
  // addition of manipulators so that the list can be modified or
  // reused.</p>
  //
  // <p>There are two typical uses of this class:</p>
  //
  // <ol>
  // <li>to take a persistent snapshot of an Attributes object
  //  in a <a href="../SAX/IContentHandler.html#startElement">startElement</a> event; or</li>
  // <li>to construct or modify an IAttributes object in a SAX2 driver or filter.</li>
  // </ol>
  //
  // <p>This class replaces the now-deprecated SAX1 AttributeListImpl
  // class; in addition to supporting the updated Attributes
  // interface rather than the deprecated <a href="../SAX/IAttributeList.html">IAttributeList</a>
  // interface, it also includes a much more efficient
  // implementation using a single array rather than a set of Vectors.</p>
  //
  // @since SAX 2.0
  TAttributesImpl = class(TInterfacedObject, IAttributes)
  private
    Flength : Integer;
    Fdata : array of SAXString;

    // Ensure the internal array's capacity.
    //
    // @param n The minimum number of attributes that the array must
    //        be able to hold.
    procedure ensureCapacity(n : Integer);

    // Report a bad array index in a manipulator.
    //
    // @param index The index to report.
    // @exception Exception Always.
    procedure badIndex(index : Integer);

  protected

    // Return the number of attributes in the list.
    //
    // @return The number of attributes in the list.
    // @see <a href="../SAX/IAttributes.html#getLength">IAttributes.getLength</a>
    function getLength() : Integer;

    // Return an attribute's Namespace URI.
    //
    // @param index The attribute's index (zero-based).
    // @return The Namespace URI, the empty string if none is
    //         available or if the index is out of range.
    // @see <a href="../SAX/IAttributes.html#getURI">IAttributes.getURI</a>
    function getURI(index : Integer) : SAXString;

    // Return an attribute's local name.
    //
    // @param index The attribute's index (zero-based).
    // @return The attribute's local name, the empty string if
    //         none is available or if the index if out of range.
    // @see <a href="../SAX/IAttributes.html#getLocalName">IAttributes.getLocalName</a>
    function getLocalName(index : Integer) : SAXString;

    // Return an attribute's qualified (prefixed) name.
    //
    // @param index The attribute's index (zero-based).
    // @return The attribute's qualified name, the empty string if
    //         none is available or if the index is out of bounds.
    // @see <a href="../SAX/IAttributes.html#getQName">IAttributes.getQName</a>
    function getQName(index : Integer) : SAXString;

    // Return an attribute's type by index.
    //
    // @param index The attribute's index (zero-based).
    // @return The attribute's type, 'CDATA' if the type is unknown, or an empty
    //         string if the index is out of bounds.
    // @see <a href="../SAX/IAttributes.html#getType.Integer">IAttributes.getType(Integer)</a>
    function getType(index : Integer) : SAXString; overload;

    // Look up an attribute's type by Namespace-qualified name.
    //
    // @param uri The Namespace URI, or the empty string for a name
    //        with no explicit Namespace URI.
    // @param localName The local name.
    // @return The attribute's type, or an empty if there is no
    //         matching attribute.
    // @see <a href="../SAX/IAttributes.html#getType.SAXString.SAXString">IAttributes.getType(SAXString,SAXString)</a>
    function getType(const uri, localName : SAXString) : SAXString; overload;

    // Look up an attribute's type by qualified (prefixed) name.
    //
    // @param qName The qualified name.
    // @return The attribute's type, or an empty string if there is no
    //         matching attribute.
    // @see <a href="../SAX/IAttributes.html#getType.SAXString">IAttributes.getType(SAXString)</a>
    function getType(const qName : SAXString) : SAXString; overload;

    // Return an attribute's value by index.
    //
    // @param index The attribute's index (zero-based).
    // @return The attribute's value or an empty string if the index is
    //         out of bounds.
    // @see <a href="../SAX/IAttributes.html#getValue.Integer">IAttributes.getValue(Integer)</a>
    function getValue(index : Integer) : SAXString; overload;

    // Look up an attribute's value by Namespace-qualified name.
    //
    // @param uri The Namespace URI, or the empty string for a name
    //        with no explicit Namespace URI.
    // @param localName The local name.
    // @return The attribute's value, or an empty string if there is no
    //         matching attribute.
    // @see <a href="../SAX/IAttributes.html#getValue.SAXString.SAXString">IAttributes.getValue(SAXString,SAXString)</a>
    function getValue(const uri, localName : SAXString) : SAXString; overload;

    // Look up an attribute's value by qualified (prefixed) name.
    //
    // @param qName The qualified name.
    // @return The attribute's value, or an empty string if there is no
    //         matching attribute.
    // @see <a href="../SAX/IAttributes.html#getValue.SAXString">IAttributes.getValue(SAXString)</a>
    function getValue(const qName : SAXString) : SAXString; overload;

    // Look up an attribute's index by Namespace name.
    //
    // <p>In many cases, it will be more efficient to look up the name once and
    // use the index query methods rather than using the name query methods
    // repeatedly.</p>
    //
    // @param uri The attribute's Namespace URI, or the empty
    //        string if none is available.
    // @param localName The attribute's local name.
    // @return The attribute's index, or -1 if none matches.
    // @see <a href="../SAX/IAttributes.html#getIndex.SAXString.SAXString">IAttributes.getIndex(SAXString,SAXString)</a>
    function getIndex(const uri, localName : SAXString) : Integer; overload;

    // Look up an attribute's index by qualified (prefixed) name.
    //
    // @param qName The qualified name.
    // @return The attribute's index, or -1 if none matches.
    // @see <a href="../SAX/IAttributes.html#getIndex.SAXString">IAttributes.getIndex(SAXString)</a>
    function getIndex(const qName : SAXString) : Integer; overload;

  public
    // Construct a new, empty AttributesImpl object.
    constructor Create(); overload;

    // Copy an existing Attributes object.
    //
    // <p>This constructor is especially useful inside a
    // <a href="../SAX/IContentHandler.html#startElement">startElement</a> event.</p>
    //
    // @param atts The existing Attributes object.
    constructor Create(const atts : IAttributes); overload;

    // Standard default destructor
    //
    // @see <a href="../SAXHelpers/TAttributesImpl.html#create">TAttributesImpl.create</a>
    destructor Destroy(); override;

    // Clear the attribute list for reuse.
    //
    // <p>Note that little memory is freed by this call:
    // the current array is kept so it can be
    // reused.</p>
    procedure clear();

    // Copy an entire Attributes object.
    //
    // <p>It may be more efficient to reuse an existing object
    // rather than constantly allocating new ones.</p>
    //
    // @param atts The attributes to copy.
    procedure setAttributes(const atts : IAttributes); virtual;

    // Add an attribute to the end of the list.
    //
    // <p>For the sake of speed, this method does no checking
    // to see if the attribute is already in the list: that is
    // the responsibility of the application.</p>
    //
    // @param uri The Namespace URI, or the empty string if
    //        none is available or Namespace processing is not
    //        being performed.
    // @param localName The local name, or the empty string if
    //        Namespace processing is not being performed.
    // @param qName The qualified (prefixed) name, or the empty string
    //        if qualified names are not available.
    // @param type The attribute type as a string.
    // @param value The attribute value.
    procedure addAttribute(const uri, localName, qName, attrType,
      value : SAXString); virtual;

    // Set an attribute in the list.
    //
    // <p>For the sake of speed, this method does no checking
    // for name conflicts or well-formedness: such checks are the
    // responsibility of the application.</p>
    //
    // @param index The index of the attribute (zero-based).
    // @param uri The Namespace URI, or the empty string if
    //        none is available or Namespace processing is not
    //        being performed.
    // @param localName The local name, or the empty string if
    //        Namespace processing is not being performed.
    // @param qName The qualified name, or the empty string
    //        if qualified names are not available.
    // @param type The attribute type as a string.
    // @param value The attribute value.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setAttribute(index : Integer; const uri, localName, qName,
      attrType, value : SAXString);

    // Remove an attribute from the list.
    //
    // @param index The index of the attribute (zero-based).
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure removeAttribute(index : Integer); virtual;

    // Set the Namespace URI of a specific attribute.
    //
    // @param index The index of the attribute (zero-based).
    // @param uri The attribute's Namespace URI, or the empty
    //        string for none.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setURI(index : Integer; const uri : SAXString);

    // Set the local name of a specific attribute.
    //
    // @param index The index of the attribute (zero-based).
    // @param localName The attribute's local name, or the empty
    //        string for none.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setLocalName(index : Integer; const localName : SAXString);

    // Set the qualified name of a specific attribute.
    //
    // @param index The index of the attribute (zero-based).
    // @param qName The attribute's qualified name, or the empty
    //        string for none.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setQName(index : Integer; const qName : SAXString);

    // Set the type of a specific attribute.
    //
    // @param index The index of the attribute (zero-based).
    // @param type The attribute's type.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setType(index : Integer; const attrType : SAXString);

    // Set the value of a specific attribute.
    //
    // @param index The index of the attribute (zero-based).
    // @param value The attribute's value.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setValue(index : Integer; const value : SAXString);

  end;

  // Default base class for SAX2 event handlers.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This class is available as a convenience base class for SAX2
  // applications: it provides default implementations for all of the
  // callbacks in the four core SAX2 handler classes:</p>
  //
  // <ul>
  // <li><a href="../SAX/IEntityResolver.html">IEntityResolver</a></li>
  // <li><a href="../SAX/IDTDHandler.html">IDTDHandler</a></li>
  // <li><a href="../SAX/IContentHandler.html">IContentHandler</a></li>
  // <li><a href="../SAX/IErrorHandler.html">IErrorHandler</a></li>
  // </ul>
  //
  // <p>Application writers can extend this class when they need to
  // implement only part of an interface; parser writers can
  // instantiate this class to provide default handlers when the
  // application has not supplied its own.</p>
  //
  // <p>This class replaces the deprecated SAX1
  // HandlerBase class.</p>
  //
  // @since SAX 2.0
  // @see <a href="../SAX/IEntityResolver.html">IEntityResolver</a>
  // @see <a href="../SAX/IDTDHandler.html">IDTDHandler</a>
  // @see <a href="../SAX/IContentHandler.html">IContentHandler</a>
  // @see <a href="../SAX/IErrorHandler.html">IErrorHandler</a>
  TDefaultHandler = class(TInterfacedObject, IEntityResolver, IDTDHandler,
    IContentHandler, IErrorHandler)
  protected
    // Resolve an external entity.
    //
    // <p>Always return nil, so that the parser will use the system
    // identifier provided in the XML document.  This method implements
    // the SAX default behaviour: application writers can override it
    // in a subclass to do special translations such as catalog lookups
    // or URI redirection.</p>
    //
    // @param publicId The public identifer, or an empty string if none is
    //                 available.
    // @param systemId The system identifier provided in the XML
    //                 document.
    // @return The new input source, or nil to require the
    //         default behaviour.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IEntityResolver.html#resolveEntity">IEntityResolver.resolveEntity</a>
    function resolveEntity(const publicId,
      systemId : SAXString) : IInputSource; virtual;

    // Receive notification of a notation declaration.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass if they wish to keep track of the notations
    // declared in a document.</p>
    //
    // @param name The notation name.
    // @param publicId The notation public identifier, or an empty string
    //                 if not available.
    // @param systemId The notation system identifier.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IDTDHandler.html#notationDecl">IDTDHandler.notationDecl</a>
    procedure notationDecl(const name, publicId, systemId : SAXString); virtual;

    // Receive notification of an unparsed entity declaration.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to keep track of the unparsed entities
    // declared in a document.</p>
    //
    // @param name The entity name.
    // @param publicId The entity public identifier, or an empty string
    //                 if not available.
    // @param systemId The entity system identifier.
    // @param notationName The name of the associated notation.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IDTDHandler.html#unparsedEntityDecl">IDTDHandler.unparsedEntityDecl</a>
    procedure unparsedEntityDecl(const name, publicId, systemId,
      notationName : SAXString); virtual;

    // Receive a Locator object for document events.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass if they wish to store the locator for use
    // with other document events.</p>
    //
    // @param locator A locator for all SAX document events.
    // @see <a href="../SAX/IContentHandler.html#setDocumentLocator">IContentHandler.setDocumentLocator</a>
    // @see <a href="../SAX/ILocator.html">ILocator</a>
    procedure setDocumentLocator(const Locator: ILocator); virtual;

    // Receive notification of the beginning of the document.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions at the beginning
    // of a document (such as allocating the root node of a tree or
    // creating an output file).</p>
    //
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#startDocument">IContentHandler.startDocument</a>
    procedure startDocument(); virtual;

    // Receive notification of the end of the document.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions at the end
    // of a document (such as finalising a tree or closing an output
    // file).</p>
    //
    // @exception ESAXException Any SAX exception
    // @see <a href="../SAX/IContentHandler.html#endDocument">IContentHandler.endDocument</a>
    procedure endDocument(); virtual;

    // Receive notification of the start of a Namespace mapping.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions at the start of
    // each Namespace prefix scope (such as storing the prefix mapping).</p>
    //
    // @param prefix The Namespace prefix being declared.
    // @param uri The Namespace URI mapped to the prefix.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#startPrefixMapping">IContentHandler.startPrefixMapping</a>
    procedure startPrefixMapping(const prefix, uri : SAXString); virtual;

    // Receive notification of the end of a Namespace mapping.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions at the end of
    // each prefix mapping.</p>
    //
    // @param prefix The Namespace prefix being declared.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#endPrefixMapping">IContentHandler.endPrefixMapping</a>
    procedure endPrefixMapping(const prefix : SAXString); virtual;

    // Receive notification of the start of an element.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions at the start of
    // each element (such as allocating a new tree node or writing
    // output to a file).</p>
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
    //        Attributes object.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#startElement">IContentHandler.startElement</a>
    procedure startElement(const uri, localName, qName: SAXString;
      const atts: IAttributes); virtual;

    // Receive notification of the end of an element.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions at the end of
    // each element (such as finalising a tree node or writing
    // output to a file).</p>
    //
    // @param uri The Namespace URI, or the empty string if the
    //        element has no Namespace URI or if Namespace
    //        processing is not being performed.
    // @param localName The local name (without prefix), or the
    //        empty string if Namespace processing is not being
    //        performed.
    // @param qName The qualified name (with prefix), or the
    //        empty string if qualified names are not available.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#endElement">IContentHandler.endElement</a>
    procedure endElement(const uri, localName,
      qName: SAXString); virtual;

    // Receive notification of character data inside an element.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method to take specific actions for each chunk of character data
    // (such as adding the data to a node or buffer, or printing it to
    // a file).</p>
    //
    // @param ch The characters from the XML Document.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#characters">IContentHandler.characters</a>
    procedure characters(const ch : SAXString); virtual;

    // Receive notification of ignorable whitespace in element content.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method to take specific actions for each chunk of ignorable
    // whitespace (such as adding data to a node or buffer, or printing
    // it to a file).</p>
    //
    // @param ch The whitespace characters.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#ignorableWhitespace">IContentHandler.ignorableWhitespace</a>
    procedure ignorableWhitespace(const ch : SAXString); virtual;

    // Receive notification of a processing instruction.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions for each
    // processing instruction, such as setting status variables or
    // invoking other methods.</p>
    //
    // @param target The processing instruction target.
    // @param data The processing instruction data, or an empty string if
    //             none is supplied.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#processingInstruction">IContentHandler.processingInstruction</a>
    procedure processingInstruction(const target, data : SAXString); virtual;

    // Receive notification of a skipped entity.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions for each
    // processing instruction, such as setting status variables or
    // invoking other methods.</p>
    //
    // @param name The name of the skipped entity.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IContentHandler.html#processingInstruction">IContentHandler.processingInstruction</a>
    procedure skippedEntity(const name : SAXString); virtual;

    // Receive notification of a parser warning.
    //
    // <p>The default implementation does nothing.  Application writers
    // may override this method in a subclass to take specific actions
    // for each warning, such as inserting the message in a log file or
    // printing it to the console.</p>
    //
    // @param e The warning information encoded as an exception.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IErrorHandler.html#warning">IErrorHandler.warning</a>
    // @see <a href="../SAX/ESAXParseException.html">ESAXParseException</a>
    procedure warning(const e : ISAXParseError); virtual;

    // Receive notification of a recoverable parser error.
    //
    // <p>The default implementation does nothing.  Application writers
    // may override this method in a subclass to take specific actions
    // for each error, such as inserting the message in a log file or
    // printing it to the console.</p>
    //
    // @param e The warning information encoded as an exception.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IErrorHandler.html#warning">IErrorHandler.warning</a>
    // @see <a href="../SAX/ESAXParseException.html">ESAXParseException</a>
    procedure error(const e : ISAXParseError); virtual;

    // Report a fatal XML parsing error.
    //
    // <p>The default implementation throws a SAXParseException.
    // Application writers may override this method in a subclass if
    // they need to take specific actions for each fatal error (such as
    // collecting all of the errors into a single report): in any case,
    // the application must stop all regular processing when this
    // method is invoked, since the document is no longer reliable, and
    // the parser may no longer report parsing events.</p>
    //
    // @param e The error information encoded as an exception.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../SAX/IErrorHandler.html#fatalError">IErrorHandler.fatalError</a>
    // @see <a href="../SAX/ESAXParseException.html">ESAXParseException</a>
    procedure fatalError(const e : ISAXParseError); virtual;
  end;

  // Provide an optional convenience implementation of Locator.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This class is available mainly for application writers, who
  // can use it to make a persistent snapshot of a locator at any
  // point during a document parse:</p>
  //
  // <pre>
  //  TMyHandler = class(TInterfacedObject, IContentHandler)
  //    Flocator : ILocator;
  //    Fstartloc : ILocator;
  //    [...]
  //  end;
  //
  //  procedure TMyHandler.setDocumentLocator(const locator: ILocator);
  //  begin
  //    // note the locator
  //    Flocator:= locator;
  //  end;
  //
  //  procedure startDocument();
  //  begin
  //    // save the location of the start of the document
  //    // for future use.
  //    Fstartloc:= TLocatorImpl.Create(locator) as ILocator;
  //  end;
  //</pre>
  //
  // <p>Normally, parser writers will not use this class, since it
  // is more efficient to provide location information only when
  // requested, rather than constantly updating a Locator object.</p>
  //
  // @since SAX 1.0
  // @see <a href="../SAX/ILocator.html">ILocator</a>
  TLocatorImpl = class(TInterfacedObject, ILocator)
  private
    FpublicId : PSAXChar;
    FsystemId : PSAXChar;
    FlineNumber : Integer;
    FcolumnNumber : Integer;
  protected
    // Return the saved public identifier.
    //
    // @return The public identifier as a string, or an empty string if none
    //         is available.
    // @see <a href="../SAX/ILocator.html#getPublicId">ILocator.getPublicId</a>
    // @see <a href="../SAXHelpers/TLocatorImpl.html#setPublicId">TLocatorImpl.setPublicId</a>
    function getPublicId() : PSAXChar;

    // Return the saved system identifier.
    //
    // @return The system identifier as a string, or an empty string if none
    //         is available.
    // @see <a href="../SAX/ILocator.html#getSystemId">ILocator.getSystemId</a>
    // @see <a href="../SAXHelpers/TLocatorImpl.html#setSystemId">TLocatorImpl.setSystemId</a>
    function getSystemId() : PSAXChar;

    // Return the saved line number (1-based).
    //
    // @return The line number as an integer, or -1 if none is available.
    // @see <a href="../SAX/ILocator.html#getLineNumber">ILocator.getLineNumber</a>
    // @see <a href="../SAXHelpers/TLocatorImpl.html#setLineNumber">TLocatorImpl.setLineNumber</a>
    function getLineNumber() : Integer;

    // Return the saved column number (1-based).
    //
    // @return The column number as an integer, or -1 if none is available.
    // @see <a href="../SAX/ILocator.html#getColumnNumber">ILocator.getColumnNumber</a>
    // @see <a href="../SAXHelpers/TLocatorImpl.html#setColumnNumber">TLocatorImpl.setColumnNumber</a>
    function getColumnNumber() : Integer;

  public
    // Zero-argument constructor.
    //
    // <p>This will not normally be useful, since the main purpose
    // of this class is to make a snapshot of an existing Locator.</p>
    constructor Create(); overload; virtual;

    // Copy constructor.
    //
    // <p>Create a persistent copy of the current state of a locator.
    // When the original locator changes, this copy will still keep
    // the original values (and it can be used outside the scope of
    // DocumentHandler methods).</p>
    //
    // @param locator The locator to copy.
    constructor Create(locator : ILocator); overload; virtual;

    // Set the public identifier for this locator.
    //
    // @param publicId The new public identifier, or an empty string
    //        if none is available.
    // @see <a href="../SAXHelpers/TLocatorImpl.html#getPublicId">TLocatorImpl.getPublicId</a>
    procedure setPublicId(publicId : PSAXChar);

    // Set the system identifier for this locator.
    //
    // @param systemId The new system identifier, or an empty string
    //        if none is available.
    // @see <a href="../SAXHelpers/TLocatorImpl.html#getSystemId">TLocatorImpl.getSystemId</a>
    procedure setSystemId(systemId : PSAXChar);

    // Set the line number for this locator (1-based).
    //
    // @param lineNumber The line number, or -1 if none is available.
    // @see <a href="../SAXHelpers/TLocatorImpl.html#getLineNumber">TLocatorImpl.getLineNumber</a>
    procedure setLineNumber(lineNumber : Integer);

    // Set the column number for this locator (1-based).
    //
    // @param columnNumber The column number, or -1 if none is available.
    // @see <a href="../SAXHelpers/TLocatorImpl.html#getColumnNumber">TLocatorImpl.getColumnNumber</a>
    procedure setColumnNumber(columnNumber : Integer);

  end;


  // Namespace Part
  TNamespacePart = (nsURI, nsLocal, nsQName);
  // An array of Namespace Parts
  TNamespaceParts = array [TNamespacePart] of SAXString;

  // Internal class for a single Namespace context.
  //
  // <p>This module caches and reuses Namespace contexts,
  // so the number allocated
  // will be equal to the element depth of the document, not to the total
  // number of elements (i.e. 5-10 rather than tens of thousands).
  // Also, data structures used to represent contexts are shared when
  // possible (child contexts without declarations) to further reduce
  // the amount of memory that's consumed.
  // </p>
  TNamespaceContext = class(TObject)
  private
    Fparent : TNamespaceContext;
    EMPTY_ENUMERATION : SAXStringArray;
    // Copy on write for the internal tables in this context.
    //
    // <p>This class is optimized for the normal case where most
    // elements do not contain Namespace declarations.</p>
    procedure copyTables();

    function getDefaultNS: PSAXChar;
    function getPrefixTable: SAXStringArray;
    function getPrefixTableElements: SAXStringArray;
    function getPrefixTableLength: Integer;
    function getUriTable: SAXStringArray;
    function getUriTableElements: SAXStringArray;
    function getUriTableLength: Integer;
    function getDeclarations: SAXStringArray;
  protected
    FnamespaceDeclUris : Boolean;
    FdeclSeen : Boolean;
    FdeclsOK : Boolean;
    Fdeclarations : SAXStringArray;
    FprefixTable : SAXStringArray;
    FprefixTableElements : SAXStringArray;
    FprefixTableLength : Integer;
    FuriTable : SAXStringArray;
    FuriTableElements : SAXStringArray;
    FuriTableLength : Integer;
    FelementNameTable : SAXStringArray;
    FelementNameTableElements : array of TNamespaceParts;
    FelementNameTableLength : Integer;
    FattributeNameTable : SAXStringArray;
    FattributeNameTableElements : array of TNamespaceParts;
    FattributeNameTableLength : Integer;
    FdefaultNS : SAXString;
    // Extension properties to get properties from self or parent
    property prefixTable : SAXStringArray read getPrefixTable;
    property prefixTableElements : SAXStringArray read getPrefixTableElements;
    property prefixTableLength : Integer read getPrefixTableLength;
    property uriTable : SAXStringArray read getUriTable;
    property uriTableElements : SAXStringArray read getUriTableElements;
    property uriTableLength : Integer read getUriTableLength;
    property defaultNS : PSAXChar read getDefaultNS;
    property declarations : SAXStringArray read getDeclarations;

  public
    // Create the root-level Namespace context.
    constructor Create(); overload;

    // Standard default destructor
    //
    // @see <a href="../SAXHelpers/TNamespaceContext.html#create">TNamespaceContext.create</a>
    destructor Destroy(); override;

    // (Re)set the parent of this Namespace context.
    // The context must either have been freshly constructed,
    // or must have been cleared
    //
    // @param context The parent Namespace context object.
    procedure setParent(const parent: TNamespaceContext);

    // Makes associated state become collectible,
    // invalidating this context.
    // <a href="../SAXHelpers/TNamespaceSupport.html#setParent">setParent</a> must be called before
    // this context may be used again.
    procedure clear();

    // Declare a Namespace prefix for this context.
    //
    // @param prefix The prefix to declare.
    // @param uri The associated Namespace URI.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#declarePrefix">TNamespaceSupport.declarePrefix</a>
    procedure declarePrefix(const prefix, uri : SAXString); overload;

    // Declare a Namespace prefix for this context.
    //
    // @param prefix The prefix to declare.
    // @param prefixLength The length of the prefix buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param uri The associated Namespace URI.
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#declarePrefix">TNamespaceSupport.declarePrefix</a>
    procedure declarePrefix(prefix : PSAXChar; prefixLength : Integer;
      uri : PSAXChar; uriLength : Integer); overload;

    // Process a raw XML 1.0 name in this context.
    //
    // @param qName The raw XML 1.0 name.
    // @param isAttribute true if this is an attribute name.
    // @return An array of three strings containing the
    //         URI part (or empty string), the local part,
    //         and the raw name, all internalized, or empty
    //         if there is an undeclared prefix.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#processName">TNamespaceSupport.processName</a>
    function processName(const qName : SAXString;
      isAttribute : Boolean) : TNamespaceParts; overload;

    // Process a raw XML 1.0 name in this context.
    //
    // @param qName The raw XML 1.0 name.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param isAttribute true if this is an attribute name.
    // @return An array of three PSAXChars (null terminated) containing the
    //         URI part (or empty string), the local part,
    //         and the raw name, all internalized, or empty
    //         if there is an undeclared prefix.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#processName">TNamespaceSupport.processName</a>
    function processName(qName : PSAXChar; qNameLength : Integer;
      isAttribute : Boolean) : TNamespaceParts; overload;

    // Look up the URI associated with a prefix in this context.
    //
    // @param prefix The prefix to look up.
    // @return The associated Namespace URI, or empty if none is
    //         declared.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getURI">TNamespaceSupport.getURI</a>
    function getURI(const prefix : SAXString) : SAXString; overload;

    // Look up the URI associated with a prefix in this context.
    //
    // @param prefix The prefix to look up.
    // @param prefixLength The length of the prefix buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return The associated Namespace URI, or empty if none is
    //         declared.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getURI">TNamespaceSupport.getURI</a>
    function getURI(prefix : PSAXChar;
      prefixLength : Integer) : PSAXChar; overload;

    // Look up one of the prefixes associated with a URI in this context.
    //
    // <p>Since many prefixes may be mapped to the same URI,
    // the return value may be unreliable.</p>
    //
    // @param uri The URI to look up.
    // @return The associated prefix, or empty if none is declared.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefix">TNamespaceSupport.getPrefix</a>
    function getPrefix(const uri : SAXString) : SAXString; overload;

    // Look up one of the prefixes associated with a URI in this context.
    //
    // <p>Since many prefixes may be mapped to the same URI,
    // the return value may be unreliable.</p>
    //
    // @param uri The URI to look up.
    // @param uriLength The length of the URI buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return The associated prefix, or empty if none is declared.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefix">TNamespaceSupport.getPrefix</a>
    function getPrefix(uri : PSAXChar; uriLength : Integer) : PSAXChar; overload;

    // Return an enumeration of prefixes declared in this context.
    //
    // @return An enumeration of prefixes (possibly empty).
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getDeclaredPrefixes">TNamespaceSupport.getDeclaredPrefixes</a>
    function getDeclaredPrefixes() : SAXStringArray;

    // Return an enumeration of all prefixes currently in force.
    //
    // <p>The default prefix, if in force, is <em>not</em>
    // returned, and will have to be checked for separately.</p>
    //
    // @return An enumeration of prefixes (never empty).
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefixes">TNamespaceSupport.getPrefixes</a>
    function getPrefixes() : SAXStringArray;

  end;

  // Encapsulate Namespace logic for use by applications using SAX,
  // or internally by SAX drivers.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This class encapsulates the logic of Namespace processing:
  // it tracks the declarations currently in force for each context
  // and automatically processes qualified XML 1.0 names into their
  // Namespace parts; it can also be used in reverse for generating
  // XML 1.0 from Namespaces.</p>
  //
  // <p>Namespace support objects are reusable, but the reset method
  // must be invoked between each session.</p>
  //
  // <p>Here is a simple session:</p>
  //
  // <pre>
  // var parts : TNamespaceParts;
  //     support : TNamespaceSupport;
  // begin
  //   SetLength(parts, 3);
  //   support:= TNamespaceSupport.Create();
  //
  // support.pushContext();
  // support.declarePrefix('', 'http://www.w3.org/1999/xhtml"';
  // support.declarePrefix('dc', 'http://www.purl.org/dc#');
  //
  // parts:= support.processName('p', parts, false);
  // Showmessage('Namespace URI: ' + parts[0]);
  // Showmessage('Local name: ' + parts[1]);
  // Showmessage('Raw name: ' + parts[2]);

  // parts:= support.processName('dc:title', parts, false);
  // Showmessage('Namespace URI: ' + parts[0]);
  // Showmessage('Local name: ' + parts[1]);
  // Showmessage('Raw name: ' + parts[2]);

  // support.popContext();
  // </pre>
  //
  // <p>Note that this class is optimized for the use case where most
  // elements do not contain Namespace declarations: if the same
  // prefix/URI mapping is repeated for each context (for example), this
  // class will be somewhat less efficient.</p>

  // <p>Although SAX drivers (parsers) may choose to use this class to
  // implement namespace handling, they are not required to do so.
  // Applications must track namespace information themselves if they
  // want to use namespace information.</p>
  //
  // @since SAX 2.0
  TNamespaceSupport = class(TObject)
  private
    Fcontexts : TList;
    FcurrentContext : TNamespaceContext;
    FcontextPos : Integer;
    FnamespaceDeclUris : Boolean;
    // Internal method for destroying the Contexts that are created by this
    // class
    //
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#destroy">TNamespaceSupport.destroy</a>
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#reset">TNamespaceSupport.reset</a>
    procedure freeContexts();
  public
    // Create a new Namespace support object.
    constructor Create();

    // Standard default destructor
    //
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#create">TNamespaceSupport.create</a>
    destructor Destroy; override;

    // Reset this Namespace support object for reuse.
    //
    // <p>It is necessary to invoke this method before reusing the
    // Namespace support object for a new session.  If namespace
    // declaration URIs are to be supported, that flag must also
    // be set to a non-default value.
    // </p>
    //
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#setNamespaceDeclUris">TNamespaceSupport.setNamespaceDeclUris</a>
    procedure reset();

    // Start a new Namespace context.
    //
    // The new context will automatically inherit
    // the declarations of its parent context, but it will also keep
    // track of which declarations were made within this context.
    //
    // <p>Event callback code should start a new context once per element.
    // This means being ready to call this in either of two places.
    // For elements that don't include namespace declarations, the
    // <em>ContentHandler.startElement()</em> callback is the right place.
    // For elements with such a declaration, it'd done in the first
    // <em>ContentHandler.startPrefixMapping()</em> callback.
    // A boolean flag can be used to
    // track whether a context has been started yet.  When either of
    // those methods is called, it checks the flag to see if a new context
    // needs to be started.  If so, it starts the context and sets the
    // flag.  After <em>ContentHandler.startElement()</em>
    // does that, it always clears the flag.</p>
    //
    // <p>Normally, SAX drivers would push a new context at the beginning
    // of each XML element.  Then they perform a first pass over the
    // attributes to process all namespace declarations, making
    // <em>ContentHandler.startPrefixMapping()</em> callbacks.
    // Then a second pass is made, to determine the namespace-qualified
    // names for all attributes and for the element name.
    // Finally all the information for the
    // <em>ContentHandler.startElement()</em> callback is available,
    // so it can then be made.</p>


    // <p>The Namespace support object always starts with a base context
    // already in force: in this context, only the 'xml' prefix is
    // declared.</p>
    //
    // @see <a href="../SAX/IContentHandler.html">IContentHandler</a>
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#popContext">TNamespaceSupport.popContext</a>
    procedure pushContext();

    // Revert to the previous Namespace context.
    //
    // <p>Normally, you should pop the context at the end of each
    // XML element.  After popping the context, all Namespace prefix
    // mappings that were previously in force are restored.</p>
    //
    // <p>You must not attempt to declare additional Namespace
    // prefixes after popping a context, unless you push another
    // context first.</p>
    //
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#pushContext">TNamespaceSupport.pushContext</a>
    procedure popContext();

    // Declare a Namespace prefix.  All prefixes must be declared
    // before they are referenced.  For example, a SAX driver (parser)
    // would scan an element's attributes
    // in two passes:  first for namespace declarations,
    // then a second pass using <a href="../SAXHelpers/TNamespaceSupport.html#processName">processName</a> to
    // interpret prefixes against (potentially redefined) prefixes.
    //
    // <p>This method declares a prefix in the current Namespace
    // context; the prefix will remain in force until this context
    // is popped, unless it is shadowed in a descendant context.</p>
    //
    // <p>To declare the default element Namespace, use the empty string as
    // the prefix.</p>
    //
    // <p>Note that you must <em>not</em> declare a prefix after
    // you've pushed and popped another Namespace.</p>
    //
    // <p>Note that you must <em>not</em> declare a prefix after
    // you've pushed and popped another Namespace context, or
    // treated the declarations phase as complete by processing
    // a prefixed name.</p>
    //
    // <p>Note that there is an asymmetry in this library:
    // <a href="../SAXHelpers/TNamespaceSupport.html#getPrefix">getPrefix</a>
    // will not return the "" prefix,
    // even if you have declared a default element namespace.
    // To check for a default namespace,
    // you have to look it up explicitly using <a href="../SAXHelpers/TNamespaceSupport.html#getURI">getURI</a>.
    // This asymmetry exists to make it easier to look up prefixes
    // for attribute names, where the default prefix is not allowed.</p>
    //
    // @param prefix The prefix to declare, or the empty string to
    //	indicate the default element namespace.  This may never have
    //	the value "xml" or "xmlns".
    // @param uri The Namespace URI to associate with the prefix.
    // @return true if the prefix was legal, false otherwise
    // @exception ESAXIllegalStateException when a prefix is declared
    //	after looking up a name in the context, or after pushing
    //	another context on top of it.
    //
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#processName">TNamespaceSupport.processName</a>
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getURI">TNamespaceSupport.getURI</a>
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefix">TNamespaceSupport.getPrefix</a>
    function declarePrefix(const prefix, uri : SAXString) : Boolean; overload;

    // Declare a Namespace prefix.
    //
    // @param prefix The prefix to declare, or the empty string to
    //	indicate the default element namespace.  This may never have
    //	the value "xml" or "xmlns".
    // @param prefixLength The length of the prefix buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param uri The Namespace URI to associate with the prefix.
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return true if the prefix was legal, false otherwise
    // @exception ESAXIllegalStateException when a prefix is declared
    //	after looking up a name in the context, or after pushing
    //	another context on top of it.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#declarePrefix.SAXString. SAXString">TNamespaceSupport.declarePrefix(SAXString, SAXString)</a>
    function declarePrefix(prefix : PSAXChar; prefixLength : Integer;
      uri : PSAXChar; uriLength : Integer) : Boolean; overload;

    // Process a raw XML 1.0 name, after all declarations in the current
    // context have been handled by <a href="../SAXHelpers/TNamespaceSupport.html#declarePrefix">declarePrefix</a>.
    //
    // <p>This method processes a raw XML 1.0 name in the current
    // context by removing the prefix and looking it up among the
    // prefixes currently declared.  The return value will be the
    // array supplied by the caller, filled in as follows:</p>
    //
    // <dl>
    // <dt>parts[0]</dt>
    // <dd>The Namespace URI, or an empty string if none is
    //  in use.</dd>
    // <dt>parts[1]</dt>
    // <dd>The local name (without prefix).</dd>
    // <dt>parts[2]</dt>
    // <dd>The original raw name.</dd>
    // </dl>
    //
    // <p>If the raw name has a prefix that has not been declared, then
    // the return value will be an empty string.</p>
    //
    // <p>Note that attribute names are processed differently than
    // element names: an unprefixed element name will receive the
    // default Namespace (if any), while an unprefixed attribute name
    // will not.</p>
    //
    // @param qName The raw XML 1.0 name to be processed.
    // @param parts An array supplied by the caller, capable of
    //        holding at least three members.
    // @param isAttribute A flag indicating whether this is an
    //        attribute name (true) or an element name (false).
    // @return The supplied array holding three internalized strings
    //        representing the Namespace URI (or empty string), the
    //        local name, and the raw XML 1.0 name; or '' if there
    //        is an undeclared prefix.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#declarePrefix">TNamespaceSupport.declarePrefix</a>
    function processName(const qName : SAXString; var parts : TNamespaceParts;
      isAttribute : Boolean) : TNamespaceParts; overload;

    // Process a raw XML 1.0 name, after all declarations in the current
    // context have been handled by <a href="../SAXHelpers/TNamespaceSupport.html#declarePrefix">declarePrefix</a>.
    //
    // @param qName The raw XML 1.0 name to be processed.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param parts An array supplied by the caller, capable of
    //        holding at least three members.
    // @param isAttribute A flag indicating whether this is an
    //        attribute name (true) or an element name (false).
    // @return The supplied array holding three internalized strings
    //        representing the Namespace URI (or empty string), the
    //        local name, and the raw XML 1.0 name; or '' if there
    //        is an undeclared prefix.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#declarePrefix.SAXString.TNamespaceParts.Boolean">TNamespaceSupport.declarePrefix(SAXString, TNamespaceParts, Boolean)</a>
    function processName(qName : PSAXChar; qNameLength : Integer;
      var parts : TNamespaceParts;
      isAttribute : Boolean) : TNamespaceParts; overload;

    // Look up a prefix and get the currently-mapped Namespace URI.
    //
    // <p>This method looks up the prefix in the current context.
    // Use the empty string ('') for the default Namespace.</p>
    //
    // @param prefix The prefix to look up.
    // @return The associated Namespace URI, or an empty string if the prefix
    //         is undeclared in this context.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefix">TNamespaceSupport.getPrefix</a>
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefixes">TNamespaceSupport.getPrefixes</a>
    function getURI(const prefix : SAXString) : SAXString; overload;

    // Look up a prefix and get the currently-mapped Namespace URI.
    //
    // @param prefix The prefix to look up.
    // @param prefixLength The length of the prefix buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return The associated Namespace URI, or an empty string if the prefix
    //         is undeclared in this context.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getURI.SAXString">TNamespaceSupport.getURI(SAXString)</a>
    function getURI(prefix : PSAXChar;
      prefixLength : Integer) : PSAXChar; overload;

    // Return an enumeration of all prefixes whose declarations are
    // active in the current context.
    // This includes declarations from parent contexts that have
    // not been overridden.
    //
    // <p><strong>Note:</strong> if there is a default prefix, it will not be
    // returned in this enumeration; check for the default prefix
    // using the <a href="../SAXHelpers/TNamespaceSupport.html#getURI">getURI</a> with an argument of ''.</p>
    //
    // @return An enumeration of prefixes (never empty).
    //
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getDeclaredPrefixes">TNamespaceSupport.getDeclaredPrefixes</a>
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getURI">TNamespaceSupport.getURI</a>
    function getPrefixes() : SAXStringArray; overload;

    // Return one of the prefixes mapped to a Namespace URI.
    //
    // <p>If more than one prefix is currently mapped to the same
    // URI, this method will make an arbitrary selection; if you
    // want all of the prefixes, use the <a href="../SAXHelpers/TNamespaceSupport.html#getPrefixes">getPrefixes</a>
    // method instead.</p>
    //
    // <p><strong>Note:</strong> this will never return the empty (default) prefix;
    // to check for a default prefix, use the <a href="../SAXHelpers/TNamespaceSupport.html#getURI">getURI</a>
    // method with an argument of ''.</p>
    //
    // @param uri The Namespace URI.
    // @param isAttribute true if this prefix is for an attribute
    //        (and the default Namespace is not allowed).
    // @return One of the prefixes currently mapped to the URI supplied,
    //         or an empty string if none is mapped or if the URI is assigned to
    //         the default Namespace.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefixes.SAXString">TNamespaceSupport.getPrefixes(SAXString)</a>
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getURI">TNamespaceSupport.getURI</a>
    function getPrefix(const uri : SAXString) : SAXString; overload;

    // Return one of the prefixes mapped to a Namespace URI.
    //
    // @param uri The Namespace URI.
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param isAttribute true if this prefix is for an attribute
    //        (and the default Namespace is not allowed).
    // @return One of the prefixes currently mapped to the URI supplied,
    //         or an empty string if none is mapped or if the URI is assigned to
    //         the default Namespace.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefix.SAXString">TNamespaceSupport.getPrefix(SAXString)</a>
    function getPrefix(uri : PSAXChar;
      uriLength : Integer) : PSAXChar; overload;

    // Return an enumeration of all prefixes for a given URI whose
    // declarations are active in the current context.
    // This includes declarations from parent contexts that have
    // not been overridden.
    //
    // <p>This method returns prefixes mapped to a specific Namespace
    // URI.  The xml: prefix will be included.  If you want only one
    // prefix that's mapped to the Namespace URI, and you don't care
    // which one you get, use the <a href="../SAXHelpers/TNamespaceSupport.html#getPrefix">getPrefix</a>
    // method instead.</p>
    //
    // <p><strong>Note:</strong> the empty (default) prefix is <em>never</em> included
    // in this enumeration; to check for the presence of a default
    // Namespace, use the <a href="../SAXHelpers/TNamespaceSupport.html#getURI">getURI</a> method with an
    // argument of ''.</p>
    //
    // @param uri The Namespace URI.
    // @return An enumeration of prefixes (never empty).
    //
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefix">TNamespaceSupport.getPrefix</a>
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getDeclaredPrefixes">TNamespaceSupport.getDeclaredPrefixes</a>
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getURI">TNamespaceSupport.getURI</a>
    function getPrefixes(const uri : SAXString) : SAXStringArray; overload;

    // Return an enumeration of all prefixes currently declared for a URI.
    //
    // @param uri The Namespace URI.
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return An enumeration of all prefixes declared in the
    //         current context.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefixes.SAXString">TNamespaceSupport.getPrefixes(SAXString)</a>
    function getPrefixes(uri : PSAXChar; uriLength : Integer) : SAXStringArray; overload;

    // Return an enumeration of all prefixes declared in this context.
    //
    // <p>The empty (default) prefix will be included in this
    // enumeration; note that this behaviour differs from that of
    // <a href="../SAXHelpers/TNamespaceSupport.html#getPrefix">getPrefix</a> and
    // <a href="../SAXHelpers/TNamespaceSupport.html#getPrefixes">getPrefixes</a>.</p>
    //
    // @return An enumeration of all prefixes declared in this
    //         context.
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getPrefixes">TNamespaceSupport.getPrefixes</a>
    // @see <a href="../SAXHelpers/TNamespaceSupport.html#getURI">TNamespaceSupport.getURI</a>
    function getDeclaredPrefixes() : SAXStringArray;

    // Controls whether namespace declaration attributes are placed
    // into the <code>http://www.w3.org/xmlns/2000/</code> namespace
    // by <a href="../SAXHelpers/TNamespaceSupport.html#processName">processName</a>.  This may only be
    // changed before any contexts have been pushed.
    //
    // @since SAX 2.1 alpha
    //
    // @exception EIllegalStateException when attempting to set this
    //  after any context has been pushed.
    procedure setNamespaceDeclUris(value : Boolean);

    // Returns true if namespace declaration attributes are placed into
    // a namespace.  This behavior is not the default.
    //
    // @since SAX 2.1 alpha
    ///
    function isNamespaceDeclUris() : Boolean;
  end;

  // Buffered Base class for deriving an XML filter.
  //
  // <p>This class is designed to sit between an <a href="../SAX/IXMLReader.html">IXMLReader</a>
  // and the client application's event handlers.  By default, it
  // does nothing but pass requests up to the reader and events
  // on to the handlers unmodified, but subclasses can override
  // specific methods to modify the event stream or the configuration
  // requests as they pass through.</p>
  //
  TXMLFilterImpl = class(TInterfacedObject, IXMLFilter, IXMLReader,
    IEntityResolver, IDTDHandler, IContentHandler, IErrorHandler)
  private
    Fparent : IXMLReader;
    Flocator : ILocator;
    FentityResolver : IEntityResolver;
    FdtdHandler : IDTDHandler;
    FcontentHandler : IContentHandler;
    FerrorHandler : IErrorHandler;

    // Set up before a parse.
    //
    // <p>Before every parse, check whether the parent is
    // non-nil, and re-register the filter for all of the
    // events.</p>
    procedure setupParse();

  protected
    // Set the parent reader.
    //
    // <p>This is the <a href="../SAX/IXMLReader.html">IXMLReader</a> from which
    // this filter will obtain its events and to which it will pass its
    // configuration requests.  The parent may itself be another filter.</p>
    //
    // <p>If there is no parent reader set, any attempt to parse
    // or to set or get a feature or property will fail.</p>

    // @param parent The parent reader.
    // @see <a href="../SAXHelpers/TXMLFilterImpl.html#getParent">TXMLFilterImpl.getParent</a>
    procedure setParent(const parent : IXMLReader); virtual;

    // Get the parent reader.
    //
    // <p>This method allows the application to query the parent
    // reader (which may be another filter).  It is generally a
    // bad idea to perform any operations on the parent reader
    // directly: they should all pass through this filter.</p>
    //
    // @return The parent filter, or nil if none has been set.
    // @see <a href="../SAXHelpers/TXMLFilterImpl.html#setParent">TXMLFilterImpl.setParent</a>
    function getParent() : IXMLReader; virtual;

    // Look up the value of a feature.
    //
    // <p>This will always fail if the parent is nil.</p>
    //
    // @param name The feature name.
    // @return The current value of the feature.
    // @exception ESAXNotRecognizedException If the feature
    //            value can't be assigned or retrieved from the parent.
    // @exception ESAXNotSupportedException When the
    //            parent recognizes the feature name but
    //            cannot determine its value at this time.
    function getFeature(const name : SAXString) : Boolean; virtual;

    // Set the value of a feature.
    //
    // <p>This will always fail if the parent is nil.</p>
    //
    // @param name The feature name.
    // @param value The requested feature value.
    // @exception ESAXNotRecognizedException If the feature
    //            value can't be assigned or retrieved from the parent.
    // @exception ESAXNotSupportedException When the
    //            parent recognizes the feature name but
    //            cannot set the requested value.
    procedure setFeature(const name : SAXString; value : Boolean); virtual;

    // Look up the interface value of a property.
    //
    // @param name The property name.
    // @param value The current value of the property as an interface.
    // @exception ESAXNotRecognizedException If the property interface
    //            value can't be retrieved from the parent.
    // @exception ESAXNotSupportedException When the
    //            parent recognizes the property name but
    //            cannot determine its interface at this time.
    function getProperty(const name : SAXString) : IProperty; virtual;

    // Set the entity resolver.
    //
    // @param resolver The new entity resolver.
    procedure setEntityResolver(const resolver : IEntityResolver); virtual;

    // Get the current entity resolver.
    //
    // @return The current entity resolver, or nil if none was set.
    function getEntityResolver() : IEntityResolver; virtual;

    // Set the DTD event handler.
    //
    // @param resolver The new DTD handler.
    procedure setDTDHandler(const handler : IDTDHandler); virtual;

    // Get the current DTD event handler.
    //
    // @return The current DTD handler, or nil if none was set.
    function getDTDHandler() : IDTDHandler; virtual;

    // Set the content event handler.
    //
    // @param resolver The new content handler.
    procedure setContentHandler(const handler : IContentHandler); virtual;

    // Get the content event handler.
    //
    // @return The current content handler, or nil if none was set.
    function getContentHandler() : IContentHandler; virtual;

    // Set the error event handler.
    //
    // @param handle The new error handler.
    procedure setErrorHandler(const handler : IErrorHandler); virtual;

    // Get the current error event handler.
    //
    // @return The current error handler, or nil if none was set.
    function getErrorHandler() : IErrorHandler; virtual;

    // Parse a document.
    //
    // @param input The input source for the document entity.
    // @exception ESAXException Any SAX exception.
    procedure parse(const input : IInputSource); overload; virtual;

    // Parse a document.
    //
    // @param systemId The system identifier as a fully-qualified URI.
    // @exception ESAXException Any SAX exception.
    procedure parse(const systemId : SAXString); overload; virtual;

    // Filter an external entity resolution.
    //
    // @param publicId The entity's public identifier, or null.
    // @param systemId The entity's system identifier.
    // @return A new InputSource or null for the default.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    function resolveEntity(const publicId, systemId : SAXString) : IInputSource; virtual;

    // Filter a notation declaration event.
    //
    // @param name The notation name.
    // @param publicId The notation's public identifier, or null.
    // @param systemId The notation's system identifier, or null.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure notationDecl(const name, publicId, systemId : SAXString); virtual;

    // Filter an unparsed entity declaration event.
    //
    // @param name The entity name.
    // @param publicId The entity's public identifier, or null.
    // @param systemId The entity's system identifier, or null.
    // @param notationName The name of the associated notation.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure unparsedEntityDecl(const name, publicId, systemId,
      notationName : SAXString); virtual;

    // Filter a new document locator event.
    //
    // @param locator The document locator.
    procedure setDocumentLocator(const locator: ILocator); virtual;

    // Filter a start document event.
    //
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure startDocument(); virtual;

    // Filter an end document event.
    //
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure endDocument(); virtual;

    // Filter a start Namespace prefix mapping event.
    //
    // @param prefix The Namespace prefix.
    // @param uri The Namespace URI.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure startPrefixMapping(const prefix, uri : SAXString); virtual;

    // Filter an end Namespace prefix mapping event.
    //
    // @param prefix The Namespace prefix.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure endPrefixMapping(const prefix : SAXString); virtual;

    // Filter a start element event.
    //
    // @param uri The element's Namespace URI, or the empty string.
    // @param localName The element's local name, or the empty string.
    // @param qName The element's qualified (prefixed) name, or the empty
    //        string.
    // @param atts The element's attributes.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure startElement(const uri, localName, qName: SAXString;
      const atts: IAttributes); virtual;

    // Filter an end element event.
    //
    // @param uri The element's Namespace URI, or the empty string.
    // @param localName The element's local name, or the empty string.
    // @param qName The element's qualified (prefixed) name, or the empty
    //        string.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure endElement(const uri, localName, qName: SAXString); virtual;

    // Filter a character data event.
    //
    // @param ch The characters in the XML Document.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure characters(const ch : SAXString); virtual;

    // Filter an ignorable whitespace event.
    //
    // @param ch The characters in the XML Document.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure ignorableWhitespace(const ch : SAXString); virtual;

    // Filter a processing instruction event.
    //
    // @param target The processing instruction target.
    // @param data The text following the target.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure processingInstruction(const target, data : SAXString); virtual;

    // Filter a skipped entity event.
    //
    // @param name The name of the skipped entity.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure skippedEntity(const name : SAXString); virtual;

    // Filter a warning event.
    //
    // @param e The warning as an exception.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure warning(const e : ISAXParseError); virtual;

    // Filter an error event.
    //
    // @param e The error as an exception.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure error(const e : ISAXParseError); virtual;

    // Filter a fatal error event.
    //
    // @param e The error as an exception.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure fatalError(const e : ISAXParseError); virtual;

  public

    // Construct an empty XML filter, with no parent.
    //
    // <p>This filter will have no parent: you must assign a parent
    // before you start a parse or do any configuration with
    // setFeature or getProperty.</p>
    //
    // @see <a href="../SAXHelpers/TXMLFilterImpl.html#setParent">TXMLFilterImpl.setParent</a>
    // @see <a href="../SAXHelpers/TXMLFilterImpl.html#setFeature">TXMLFilterImpl.setFeature</a>
    // @see <a href="../SAXHelpers/TXMLFilterImpl.html#getProperty">TXMLFilterImpl.getProperty</a>
    constructor create(); overload;

    // Construct an XML filter with the specified parent.
    //
    // @see <a href="../SAXHelpers/TXMLFilterImpl.html#setParent">TXMLFilterImpl.setParent</a>
    // @see <a href="../SAXHelpers/TXMLFilterImpl.html#getParent">TXMLFilterImpl.getParent</a>
    constructor create(const parent : IXMLReader); overload;

  end;

  // Base class for deriving an XML Reader.
  TXMLReaderImpl = class(TInterfacedObject, IXMLReader, ILocator)
  private
    FcontentHandler: IContentHandler;
    FdtdHandler: IDTDHandler;
    FentityResolver: IEntityResolver;
    FerrorHandler: IErrorHandler;
    FlineNumber: Integer;
    FcolumnNumber: Integer;
    FpublicId: SAXString;
    FsystemId: SAXString;
    Fnamespaces: Boolean;
    Fprefixes: Boolean;
    Fparsing: Boolean;

    // Check if we are already parsing
    //
    // <p>Throw an exception if we are parsing. Use this method to
    // detect illegal feature or property changes.</p>
    //
    // @param propType A string value containing the type of property that
    //                 cannot be changed while parsing.
    // @param name A string value containing the name of the property that
    //                 cannot be changed while parsing.
    procedure checkNotParsing(const propType, name: SAXString);
  protected
    procedure setColumnNumber(value: Integer); virtual;
    procedure setLineNumber(value: Integer); virtual;
    procedure setPublicId(value: PSAXChar); virtual;
    procedure setSystemId(value: PSAXChar); virtual;
    procedure parseInput(const input: IInputSource); virtual; abstract;
    property useNamespaces: Boolean read Fnamespaces write Fnamespaces;
    property usePrefixes: Boolean read Fprefixes write Fprefixes;

    // Look up the value of a feature.
    //
    // <p>The feature name is any fully-qualified URI.  It is
    // possible for an XMLReader to recognize a feature name but
    // to be unable to return its value; this is especially true
    // in the case of an adapter for a SAX1 Parser, which has
    // no way of knowing whether the underlying parser is
    // performing validation or expanding external entities.</p>
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
    // @return The current state of the feature (true or false).
    // @exception ESAXNotRecognizedException When the
    //            XMLReader does not recognize the feature name.
    // @exception ESAXNotSupportedException When the
    //            XMLReader recognizes the feature name but
    //            cannot determine its value at this time.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#setFeature">TXMLReaderImpl.setFeature</a>
    function getFeature(const name : SAXString) : Boolean; virtual;

    // Set the state of a feature.
    //
    // <p>The feature name is any fully-qualified URI.  It is
    // possible for an XMLReader to recognize a feature name but
    // to be unable to set its value; this is especially true
    // in the case of an adapter for a SAX1 <a href="../SAX/IParser.html">Parser</a>,
    // which has no way of affecting whether the underlying parser is
    // validating, for example.</p>
    //
    // <p>All XMLReaders are required to support setting
    // http://xml.org/sax/features/namespaces to true and
    // http://xml.org/sax/features/namespace-prefixes to false.</p>
    //
    // <p>Some feature values may be immutable or mutable only
    // in specific contexts, such as before, during, or after
    // a parse.</p>
    //
    // @param name The feature name, which is a fully-qualified URI.
    // @param state The requested state of the feature (true or false).
    // @exception ESAXNotRecognizedException When the
    //            XMLReader does not recognize the feature name.
    // @exception ESAXNotSupportedException When the
    //            XMLReader recognizes the feature name but
    //            cannot set the requested value.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#getFeature">TXMLReaderImpl.getFeature</a>
    procedure setFeature(const name : SAXString; value : Boolean); virtual;

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
    function getProperty(const name : SAXString) : IProperty; virtual;

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
    // @exception Exception If the resolver argument is nil.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#getEntityResolver">TXMLReaderImpl.getEntityResolver</a>
    procedure setEntityResolver(const resolver : IEntityResolver); virtual;

    // Return the current entity resolver.
    //
    // @return The current entity resolver, or nil if none
    //         has been registered.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#setEntityResolver">TXMLReaderImpl.setEntityResolver</a>
    function getEntityResolver() : IEntityResolver; virtual;

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
    // @exception Exception If the handler argument is nil.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#getDTDHandler">TXMLReaderImpl.getDTDHandler</a>
    procedure setDTDHandler(const handler : IDTDHandler); virtual;

    // Return the current DTD handler.
    //
    // @return The current DTD handler, or nil if none
    //         has been registered.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#setDTDHandler">TXMLReaderImpl.setDTDHandler</a>
    function getDTDHandler() : IDTDHandler; virtual;

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
    // @exception Exception If the handler argument is nil.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#getContentHandler">TXMLReaderImpl.getContentHandler</a>
    procedure setContentHandler(const handler : IContentHandler); virtual;

    // Return the current content handler.
    //
    // @return The current content handler, or nil if none
    //         has been registered.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#setContentHandler">TXMLReaderImpl.setContentHandler</a>
    function getContentHandler() : IContentHandler; virtual;

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
    // @exception Exception If the handler argument is nil.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#getErrorHandler">TXMLReaderImpl.getErrorHandler</a>
    procedure setErrorHandler(const handler : IErrorHandler); virtual;

    // Return the current error handler.
    //
    // @return The current error handler, or nil if none
    //         has been registered.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#setErrorHandler">TXMLReaderImpl.setErrorHandler</a>
    function getErrorHandler() : IErrorHandler; virtual;

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
    // different input source.</p>
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
    //            possibly from a byte stream or character stream
    //            supplied by the application.
    // @see <a href="../SAX/TInputSource.html">TInputSource</a>
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#parse.SAXString">TXMLReaderImpl.parse(SAXString)</a>
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#setEntityResolver">TXMLReaderImpl.setEntityResolver</a>
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#setDTDHandler">TXMLReaderImpl.setDTDHandler</a>
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#setContentHandler">TXMLReaderImpl.setContentHandler</a>
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#setErrorHandler">TXMLReaderImpl.setErrorHandler</a>
    procedure parse(const input : IInputSource); overload; virtual; 

    // Parse an XML document from a system identifier (URI).
    //
    // <p>This method is a shortcut for the common case of reading a
    // document from a system identifier.  It is the exact
    // equivalent of the following:</p>
    //
    // <pre>
    // input:= TInputSource.Create(systemId);
    // try
    //   parse(input);
    // finally
    //   input.Free;
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
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#parse.TInputSource">TXMLReaderImpl.parse(TInputSource)</a>
    procedure parse(const systemId : SAXString); overload; virtual;

    // Return the public identifier for the current document event.
    //
    // <p>The return value is the public identifier of the document
    // entity or of the external parsed entity in which the markup
    // triggering the event appears.</p>
    //
    // @return A string containing the public identifier, or
    //         empty if none is available.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#getSystemId">TXMLReaderImpl.getSystemId</a>
    function getPublicId() : PSAXChar; virtual;

    // Return the system identifier for the current document event.
    //
    // <p>The return value is the system identifier of the document
    // entity or of the external parsed entity in which the markup
    // triggering the event appears.</p>
    //
    // <p>If the system identifier is a URL, the parser must resolve it
    // fully before passing it to the application.</p>
    //
    // @return A string containing the system identifier, or empty
    //         if none is available.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#getPublicId">TXMLReaderImpl.getPublicId</a>
    function getSystemId() : PSAXChar; virtual;

    // Return the line number where the current document event ends.
    //
    // <p><strong>Warning:</strong> The return value from the method
    // is intended only as an approximation for the sake of error
    // reporting; it is not intended to provide sufficient information
    // to edit the character content of the original XML document.</p>
    //
    // <p>The return value is an approximation of the line number
    // in the document entity or external parsed entity where the
    // markup triggering the event appears.</p>
    //
    // <p>If possible, the SAX driver should provide the line position
    // of the first character after the text associated with the document
    // event.  The first line in the document is line 1.</p>
    //
    // @return The line number, or -1 if none is available.
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#getColumnNumber">TXMLReaderImpl.getColumnNumber</a>
    function getLineNumber() : Integer; virtual;

    // Return the column number where the current document event ends.
    //
    // <p><strong>Warning:</strong> The return value from the method
    // is intended only as an approximation for the sake of error
    // reporting; it is not intended to provide sufficient information
    // to edit the character content of the original XML document.</p>
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
    // @see <a href="../SAXHelpers/TXMLReaderImpl.html#getLineNumber">TXMLReaderImpl.getLineNumber</a>
    function getColumnNumber() : Integer; virtual;

  public

    // Construct an empty XML Reader, and initializes its values.
    constructor Create; virtual;

    // Default destructor for an XML Reader, freeing any items created during
    // intialization
    destructor Destroy; override;

  end;

  // SAX2 extension helper for additional Attributes information,
  // implementing the <a href="../SAXExt/IAttributes2.html">IAttributes2</a> interface.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This is not part of core-only SAX2 distributions.</p>
  //
  // <p>The <em>specified</em> flag for each attribute will always
  // be true, unless it has been set to false in the copy constructor
  // or using <a href="../SAXHelpers/TAttributes2Impl.html#setSpecified">setSpecified</a>.
  // Similarly, the <em>declared</em> flag for each attribute will
  // always be false, except for defaulted attributes (<em>specified</em>
  // is false), non-CDATA attributes, or when it is set to true using
  // <a href="../SAXHelpers/TAttributes2Impl.html#setDeclared">setDeclared</a>.
  // If you change an attribute's type by hand, you may need to modify
  // its <em>declared</em> flag to match.
  // </p>
  //
  // @since SAX 2.0 (extensions 1.1 alpha)
  TAttributes2Impl = class(TAttributesImpl, IAttributes2)
  private
    Fdeclared : array of Boolean;
    Fspecified : array of Boolean;

  protected

    // Returns false unless the attribute was declared in the DTD.
    // This helps distinguish two kinds of attributes that SAX reports
    // as CDATA:  ones that were declared (and hence are usually valid),
    // and those that were not (and which are never valid).
    //
    // @param index The attribute index (zero-based).
    // @return true if the attribute was declared in the DTD,
    //          false otherwise.
    // @exception Exception When the
    //            supplied index does not identify an attribute.
    //
    function isDeclared(index : Integer) : Boolean; overload;

    // Returns false unless the attribute was declared in the DTD.
    // This helps distinguish two kinds of attributes that SAX reports
    // as CDATA:  ones that were declared (and hence are usually valid),
    // and those that were not (and which are never valid).
    //
    // @param qName The XML 1.0 qualified name.
    // @return true if the attribute was declared in the DTD,
    //          false otherwise.
    // @exception ESAXIllegalArgumentException When the
    //            supplied name does not identify an attribute.
    function isDeclared(const qName : SAXString) : Boolean; overload;

    // Returns false unless the attribute was declared in the DTD.
    // This helps distinguish two kinds of attributes that SAX reports
    // as CDATA:  ones that were declared (and hence are usually valid),
    // and those that were not (and which are never valid).
    //
    // <p>Remember that since DTDs do not "understand" namespaces, the
    // namespace URI associated with an attribute may not have come from
    // the DTD.  The declaration will have applied to the attribute's
    // <em>qName</em>.</p>
    //
    // @param uri The Namespace URI, or the empty string if
    //        the name has no Namespace URI.
    // @param localName The attribute's local name.
    // @return true if the attribute was declared in the DTD,
    //          false otherwise.
    // @exception ESAXIllegalArgumentException When the
    //            supplied names do not identify an attribute.
    function isDeclared(const uri, localName : SAXString) : Boolean; overload;

    // Returns the current value of an attribute's "specified" flag.
    //
    // @param index The attribute index (zero-based).
    // @return current flag value
    // @exception EIllegalArgumentException When the
    //            supplied index does not identify an attribute.
    function isSpecified(index : Integer) : Boolean; overload;

    // Returns the current value of an attribute's "specified" flag.
    //
    // @param uri The Namespace URI, or the empty string if
    //        the name has no Namespace URI.
    // @param localName The attribute's local name.
    // @return current flag value
    // @exception ESAXIllegalArgumentException When the
    //            supplied names do not identify an attribute.
    function isSpecified(const uri, localName : SAXString) : Boolean; overload;

    // Returns the current value of an attribute's "specified" flag.
    //
    // @param qName The XML 1.0 qualified name.
    // @return current flag value
    // @exception ESAXIllegalArgumentException When the
    //            supplied name does not identify an attribute.
    function isSpecified(const qName : SAXString) : Boolean; overload;

  public
    // Copy an entire Attributes object.  The "specified" flags are
    // assigned as true, unless the object is an Attributes2 object
    // in which case those values are copied.
    //
    // @see <a href="../SAXHelpers/TAttributesImpl.html#setAttributes">TAttributesImpl.setAttributes</a>
    procedure setAttributes(const atts : IAttributes); override;

    // Add an attribute to the end of the list, setting its
    // "specified" flag to true.  To set that flag's value
    // to false, use <a href="../SAXHelpers/TAttributes2Impl.html#setSpecified">setSpecified</a>.
    //
    // <p>Unless the attribute <em>type</em> is CDATA, this attribute
    // is marked as being declared in the DTD.  To set that flag's value
    // to true for CDATA attributes, use <a href="../SAXHelpers/TAttributes2Impl.html#setDeclared">setDeclared</a>.</p>
    //
    // @see <a href="../SAXHelpers/TAttributesImpl.html#addAttribute">TAttributesImpl.addAttribute</a>
    procedure addAttribute (const uri, localName, qName,
      attrType, value : SAXString); override;

    // Remove an attribute declaration.
    //
    // @param index The index of the attribute (zero-based).
    procedure removeAttribute(index : Integer); override;

    // Assign a value to the "declared" flag of a specific attribute.
    // This is normally needed only for attributes of type CDATA,
    // including attributes whose type is changed to or from CDATA.
    //
    // @param index The index of the attribute (zero-based).
    // @param value The desired flag value.
    // @exception ESAXIllegalArgumentException When the
    //            supplied index does not identify an attribute.
    // @see <a href="../SAXHelpers/TAttributes2Impl.html#setType">TAttributes2Impl.setType</a>
    procedure setDeclared(index : Integer; value : Boolean);

    // Assign a value to the "specified" flag of a specific attribute.
    // This is the only way this flag can be cleared, except clearing
    // by initialization with the copy constructor.
    //
    // @param index The index of the attribute (zero-based).
    // @param value The desired flag value.
    // @exception ESAXIllegalArgumentException When the
    //            supplied index does not identify an attribute.
    procedure setSpecified(index : Integer; value : Boolean);
  end;

  // This class extends the SAX2 base handler class to support the
  // SAX2 <a href="../SAXExt/ILexicalHandler.html">ILexicalHandler</a>,
  // <a href="../SAXExt/IDeclHandler.html">IDeclHandler</a>, and
  // <a href="../SAXExt/IEntityResolver2.html">IEntityResolver2</a> extensions.
  // Except for overriding the
  // original SAX1 <a href="../SAXHelpers/TDefaultHandler.html#resolveEntity">resolveEntity</a>
  // method the added handler methods just return.  Subclassers may
  // override everything on a method-by-method basis.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p> <em>Note:</em> this class might yet learn that the
  // <em>IContentHandler.setDocumentLocator()</em> call might be passed a
  // <a href="../SAXExt/ILocator2.html">ILocator2</a> object, and that the
  // <em>IContentHandler.startElement()</em> call might be passed a
  // <a href="../SAXExt/IAttributes2.html">IAttributes2</a> object.</p>
  //
  // @since SAX 2.0 (extensions 1.1 alpha)
  TDefaultHandler2 = class(TDefaultHandler, ILexicalHandler, IDeclHandler,
    IEntityResolver2)
  protected

    // Report an element type declaration.
    //
    // @param name The element type name.
    // @param model The content model as a normalized string.
    // @exception ESAXException The application may raise an exception.
    ///
    procedure elementDecl(const name, model : SAXString); virtual;

    // Report an attribute type declaration.
    //
    // @param eName The name of the associated element.
    // @param aName The name of the attribute.
    // @param attrType A string representing the attribute type.
    // @@param mode A string representing the attribute defaulting mode
    //        ("#IMPLIED", "#REQUIRED", or "#FIXED") or '' if
    //        none of these applies.
    // @param value A string representing the attribute's default value,
    //        or '' if there is none.
    // @exception ESAXException The application may raise an exception.
    procedure attributeDecl(const eName, aName, attrType, mode,
      value: SAXString); virtual;

    // Report an internal entity declaration.
    //
    // @param name The name of the entity.  If it is a parameter
    //        entity, the name will begin with '%'.
    // @param value The replacement text of the entity.
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXHelpers/TDefaultHandler2.html#externalEntityDecl">TDefaultHandler2.externalEntityDecl</a>
    // @see <a href="../SAX/IDTDHandler.html#unparsedEntityDecl">IDTDHandler.unparsedEntityDecl</a>
    procedure internalEntityDecl(const name, value : SAXString); virtual;

    // Report a parsed external entity declaration.
    //
    // @param name The name of the entity.  If it is a parameter
    //        entity, the name will begin with '%'.
    // @param publicId The declared public identifier of the entity, or
    //        an empty string if none was declared.
    // @param systemId The declared system identifier of the entity.
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXHelpers/TDefaultHandler2.html#internalEntityDecl">TDefaultHandler2.internalEntityDecl</a>
    // @see <a href="../SAX/IDTDHandler.html#unparsedEntityDecl">IDTDHandler.unparsedEntityDecl</a>
    procedure externalEntityDecl(const name, publicId,
      systemId : SAXString); virtual;

    // Report the start of DTD declarations, if any.
    //
    // @param name The document type name.
    // @param publicId The declared public identifier for the
    //        external DTD subset, or an empty string if none was declared.
    // @param systemId The declared system identifier for the
    //        external DTD subset, or an empty string if none was declared.
    //        (Note that this is not resolved against the document
    //        base URI.)
    // @exception ESAXException The application may raise an
    //            exception.
    // @see <a href="../SAXHelpers/TDefaultHandler2.html#endDTD">TDefaultHandler2.endDTD</a>
    // @see <a href="../SAXHelpers/TDefaultHandler2.html#startEntity">TDefaultHandler2.startEntity</a>
    procedure startDTD(const name, publicId, systemId : SAXString); virtual;

    // Report the end of DTD declarations.
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXHelpers/TDefaultHandler2.html#startDTD">TDefaultHandler2.startDTD</a>
    procedure endDTD(); virtual;

    // Report the beginning of some internal and external XML entities.
    //
    // <ul>
    // <li>general entities within attribute values</li>
    // <li>parameter entities within declarations</li>
    // </ul>
    //
    // <p>These will be silently expanded, with no indication of where
    // the original entity boundaries were.</p>
    //
    // <p>Note also that the boundaries of character references (which
    // are not really entities anyway) are not reported.</p>
    //
    // <p>All start/endEntity events must be properly nested.</p>
    //
    // @param name The name of the entity.  If it is a parameter
    //        entity, the name will begin with '%', and if it is the
    //        external DTD subset, it will be "[dtd]".
    // @exception SAXException The application may raise an exception.
    // @see <a href="../SAXHelpers/TDefaultHandler2.html#endEntity">TDefaultHandler2.endEntity</a>
    // @see <a href="../SAXExt/IDeclHandler.html#internalEntityDecl">IDeclHandler.internalEntityDecl</a>
    // @see <a href="../SAXExt/IDeclHandler.html#externalEntityDecl">IDeclHandler.externalEntityDecl</a>
    procedure startEntity(const name : SAXString); virtual;

    // Report the end of an entity.
    //
    // @param name The name of the entity that is ending.
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXHelpers/TDefaultHandler2.html#startEntity">TDefaultHandler2.startEntity</a>
    procedure endEntity(const name : SAXString); virtual;

    // Report the start of a CDATA section.
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXHelpers/TDefaultHandler2.html#endCDATA">TDefaultHandler2.endCDATA</a>
    procedure startCDATA(); virtual;

    // Report the end of a CDATA section.
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXHelpers/TDefaultHandler2.html#startCDATA">TDefaultHandler2.startCDATA</a>
    procedure endCDATA(); virtual;

    // Report an XML comment anywhere in the document.
    //
    // @param ch The characters in the comment.
    // @exception ESAXException The application may raise an exception.
    procedure comment(const ch : SAXString); virtual;

    // Allows applications to provide an external subset for documents
    // that don't explicitly define one.
    //
    // @param name Identifies the document root element.  This name comes
    //  from a DOCTYPE declaration (where available) or from the actual
    //  root element.
    // @param baseURI The document's base URI, serving as an additional
    //  hint for selecting the external subset.  This is always an absolute
    //  URI, unless it is an empty string because the IXMLReader was given an
    //  IInputSource without one.
    //
    // @return An InputSource object describing the new external subset
    //  to be used by the parser, or nil to indicate that no external
    //  subset is provided.
    //
    // @exception ESAXException Any SAX exception.
    function getExternalSubset(const name,
      baseURI : SAXString) : IInputSource; virtual;

    // Tells the parser to resolve the systemId against the baseURI
    // and read the entity text from that resulting absolute URI.
    // Note that because the older
    // <a href="../SAXHelpers/TDefaultHandler.html#resolveEntity">TDefaultHandler.resolveEntity</a>
    // method is overridden to call this one, this method may sometimes
    // be invoked with null <em>name</em> and <em>baseURI</em>, and
    // with the <em>systemId</em> already absolutized.
    //
    // @param name Identifies the external entity being resolved.
    //  Either '[dtd]' for the external subset, or a name starting
    //  with '%' to indicate a parameter entity, or else the name of
    //  a general entity.
    // @param publicId The public identifier of the external entity being
    //  referenced (normalized as required by the XML specification), or
    //  an empty string if none was supplied.
    // @param baseURI The URI with respect to which relative systemIDs
    //  are interpreted.  This is always an absolute URI, unless it is
    //  nil because the XMLReader was given an IInputSource without one.
    //  This URI is defined by the XML specification to be the one
    //  associated with the '&lt;' starting the relevant declaration.
    // @param systemId The system identifier of the external entity
    //  being referenced; either a relative or absolute URI.
    //
    // @return An IInputSource object describing the new input source to
    //  be used by the parser.  Returning nil directs the parser to
    //  resolve the system ID against the base URI and open a connection
    //  to resulting URI.
    //
    // @exception ESAXException Any SAX exception.
    function resolveEntity(const name, publicId, baseURI,
      systemId : SAXString) : IInputSource; reintroduce; overload; virtual;

    // Invokes
    // <a href="../SAXExt/IEntityResolver2.html#resolveEntity">IEntityResolver2.resolveEntity</a>
    // with empty entity name and base URI.
    // You only need to override that method to use this class.
    function resolveEntity(const publicId, systemId : SAXString) : IInputSource;
      reintroduce; overload; virtual;

  end;

  // SAX2 extension helper for holding additional Entity information,
  // implementing the <a href="../SAXExt/ILocator2.html">ILocator2</a> interface.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p> This is not part of core-only SAX2 distributions.</p>
  //
  // @since SAX 2.0 (extensions 1.1 alpha)
  TLocator2Impl = class(TLocatorImpl, ILocator2)
  private
    Fencoding : PSAXChar;
    Fversion : PSAXChar;

  protected
    // Returns the current value of the version property.
    //
    // @see <a href="../SAXHelpers/TLocator2Impl.html#setXMLVersion">TLocator2Impl.setXMLVersion</a>
    ///
    function getXMLVersion() : PSAXChar;

    // Returns the current value of the encoding property.
    //
    // @see <a href="../SAXHelpers/TLocator2Impl.html#setEncoding">TLocator2Impl.setEncoding</a>
    ///
    function getEncoding() : PSAXChar;

  public
    // Copy an existing ILocator or ILocator2 object.
    // If the object implements ILocator2, values of the
    // <em>encoding</em> and <em>version</em>strings are copied,
    // otherwise they set to an empty string.
    //
    // @param locator The existing ILocator object.
    constructor Create(locator : ILocator); override;

    // Assigns the current value of the version property.
    //
    // @param version the new "version" value
    // @see <a href="../SAXHelpers/TLocator2Impl.html#getXMLVersion">TLocator2Impl.getXMLVersion</a>
    ///
    procedure setXMLVersion(version : PSAXChar);

    // Assigns the current value of the encoding property.
    //
    // @param version the new "encoding" value
    // @see <a href="../SAXHelpers/TLocator2Impl.html#getEncoding">TLocator2Impl.getEncoding</a>
    ///
    procedure setEncoding(encoding : PSAXChar);

  end;

  // In this implementation, the lifetime of FMessage only spans
  // the call where it is returned through the error handler interface
  TSAXError = class(TInterfacedObject, ISAXError)
  private
    FMessage: PSAXChar;
  protected
    function getNativeError: IUnknown; virtual;
  public
    constructor create(message : PSAXChar);
    procedure init(message : PSAXChar); virtual;
    function getMessage() : PSAXChar; virtual;
  end;

  // In this implementation, the lifetime of FpublicId and FsystemId only spans
  // the call where it is returned through the error handler interface
  TSAXParseError = class(TSAXError, ISAXParseError)
  private
    FpublicId : PSAXChar;
    FsystemId : PSAXChar;
    FlineNumber : Integer;
    FcolumnNumber : Integer;
  public
    constructor create(message : PSAXChar; const locator: ILocator); overload;
    constructor create(const message : SAXString; publicId, systemId : PSAXChar;
      lineNumber, columnNumber : Integer); overload;
    procedure init(message: PSAXChar; const locator: ILocator); reintroduce;
    function getPublicId() : PSAXChar;
    function getSystemId() : PSAXChar;
    function getLineNumber() : Integer;
    function getColumnNumber() : Integer;
  end;

  TSAXNotRecognizedError = class(TSAXError, ISAXNotRecognizedError);
  TSAXNotSupportedError = class(TSAXError, ISAXNotSupportedError);
  TSAXIllegalStateError = class(TSAXError, ISAXIllegalStateError);
  TSAXIllegalArgumentError = class(TSAXError, ISAXIllegalArgumentError);

var
  HandlerDefault: TDefaultHandler;

implementation

resourcestring
  sBadIndex             = 'Attempt to modify attribute at illegal index: %d';
  sCantChange           = 'Cannot change %s %s while parsing';
  sFeatureName          = 'Feature: %s';
  sFeatureCheck         = 'feature';
  sNoParent             = 'No parent for filter';
  sPropertyName         = 'Property: %s';
  sStackEmpty           = 'Namespace stack empty';
  sCantDeclare          = 'Cannot declare any more prefixes in this context';
  sNoAttributeAtIndex   = 'No attribute at index: %d';
  sNoAttributeLocalUri  = 'No such attribute: local= %s, namespace= %s';
  sNoAttributeQName     = 'No such attribute: %s';

{ TAttributesImpl }

constructor TAttributesImpl.Create;
begin
  inherited Create();
  Flength:= 0;
  Fdata:= nil;
end;

constructor TAttributesImpl.Create(const atts: IAttributes);
begin
  inherited Create();
  Flength:= 0;
  Fdata:= nil;
  setAttributes(atts);
end;

destructor TAttributesImpl.Destroy;
begin
  Fdata:= nil;
  inherited Destroy();
end;

function TAttributesImpl.getLength: Integer;
begin
  Result:= Flength;
end;

function TAttributesImpl.getURI(index: Integer): SAXString;
begin
  if ((index >= 0) and (index < Flength)) then
    Result:= Fdata[index*5]
  else
    Result:= '';
end;

function TAttributesImpl.getLocalName(index: Integer): SAXString;
begin
  if ((index >= 0) and (index < Flength)) then
    Result:= Fdata[index*5+1]
  else
    Result:= '';
end;

function TAttributesImpl.getQName(index: Integer): SAXString;
begin
  if ((index >= 0) and (index < Flength)) then
    Result:= Fdata[index*5+2]
  else
    Result:= '';
end;

function TAttributesImpl.getType(index: Integer): SAXString;
begin
  if ((index >= 0) and (index < Flength)) then
    Result:= Fdata[index*5+3]
  else
    Result:= '';
end;

function TAttributesImpl.getType(const uri,
  localName: SAXString): SAXString;
var max, I : Integer;
begin
  max:= Flength * 5;
  I:= 0;
  while I < max do
  begin
    if ((Fdata[I] = uri) and (Fdata[I+1] = localName)) then
    begin
      Result:= Fdata[I+3];
      Exit;
    end;
    Inc(I, 5);
  end;
  Result:= '';
end;

function TAttributesImpl.getType(const qName: SAXString): SAXString;
var max, I : Integer;
begin
  max:= Flength * 5;
  I:= 0;
  while I < max do
  begin
    if (Fdata[I+2] = qName) then
    begin
      Result:= Fdata[I+3];
      Exit;
    end;
    Inc(I, 5);
  end;
  Result:= '';
end;

function TAttributesImpl.getValue(index: Integer): SAXString;
begin
  if ((index >= 0) and (index < Flength)) then
    Result:= Fdata[index*5+4]
  else
    Result:= '';
end;

function TAttributesImpl.getValue(const uri,
  localName: SAXString): SAXString;
var max, I : Integer;
begin
  max:= Flength * 5;
  I:= 0;
  while I < max do
  begin
    if ((Fdata[I] = uri) and (Fdata[I+1] = localName)) then
    begin
      Result:= Fdata[I+4];
      Exit;
    end;
    Inc(I, 5);
  end;
  Result:= '';
end;

function TAttributesImpl.getValue(const qName: SAXString): SAXString;
var max, I : Integer;
begin
  max:= Flength * 5;
  I:= 0;
  while I < max do
  begin
    if (Fdata[I+2] = qName) then
    begin
      Result:= Fdata[I+4];
      Exit;
    end;
    Inc(I, 5);
  end;
  Result:= '';
end;

function TAttributesImpl.getIndex(const uri,
  localName: SAXString): Integer;
var max, I : Integer;
begin
  max:= Flength * 5;
  I:= 0;
  while I < max do
  begin
    if ((Fdata[I] = uri) and (Fdata[I+1] = localName)) then
    begin
      Result:= I div 5;
      Exit;
    end;
    Inc(I, 5);
  end;
  Result:= -1;
end;

function TAttributesImpl.getIndex(const qName: SAXString): Integer;
var max, I : Integer;
begin
  max:= Flength * 5;
  I:= 0;
  while I < max do
  begin
    if (Fdata[I+2] = qName) then
    begin
      Result:= I div 5;
      Exit;
    end;
    Inc(I, 5);
  end;
  Result:= -1;
end;

procedure TAttributesImpl.clear;
var i : Integer;
begin
  if (Fdata <> nil) then
  begin
    for i:= 0 to (Flength * 5)-1 do
      Fdata[i]:= '';
  end;
  Flength:= 0;
end;

procedure TAttributesImpl.setAttributes(const atts: IAttributes);
var I : Integer;
begin
  // Can this be sped up with a move or copy?
  clear();
  Flength:= atts.getLength();
  if (Flength > 0) then
  begin
    Fdata:= nil;
    SetLength(Fdata, Flength * 5);
    for I:= 0 to Flength-1 do
    begin
      Fdata[I*5]:= atts.getURI(I);
      Fdata[I*5+1]:= atts.getLocalName(I);
      Fdata[I*5+2]:= atts.getQName(I);
      Fdata[I*5+3]:= atts.getType(I);
      Fdata[I*5+4]:= atts.getValue(I);
    end;
  end;
end;

procedure TAttributesImpl.addAttribute(const uri, localName, qName, attrType,
  value: SAXString);
begin
  ensureCapacity(Flength + 1);
  Fdata[Flength*5]:= uri;
  Fdata[Flength*5+1]:= localName;
  Fdata[Flength*5+2]:= qName;
  Fdata[Flength*5+3]:= attrType;
  Fdata[Flength*5+4]:= value;
  Inc(Flength);
end;

procedure TAttributesImpl.setAttribute(index : Integer; const uri, localName,
  qName, attrType, value: SAXString);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    Fdata[index*5]:= uri;
    Fdata[index*5+1]:= localName;
    Fdata[index*5+2]:= qName;
    Fdata[index*5+3]:= attrType;
    Fdata[index*5+4]:= value;
  end else
    badIndex(index);
end;

procedure TAttributesImpl.removeAttribute(index: Integer);
var Source : Pointer;
    Dest : Pointer;
    FromBytes, ToBytes, TotalBytes, NewLen, I : Integer;
    FromIndex, ToIndex : Integer;
    aIndex : Integer;
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    if (index < Flength-1) then
    begin
      // Deletes a block from the array
      FromIndex:= index*5;
      ToIndex:= index*5+4;
      for I:= FromIndex to ToIndex do
       FData[I]:= '';

      Dest:= Pointer(FData);
      Source:= Pointer(FData);
      FromBytes:= SizeOf(SAXString) * FromIndex;
      ToBytes:= SizeOf(SAXString) * (ToIndex + 1);
      TotalBytes:= SizeOf(SAXString) * (Length(FData) - ToIndex);
      NewLen:= Length(FData) - ((ToIndex - FromIndex) + 1);
      Inc(Cardinal(Dest), FromBytes);
      Inc(Cardinal(Source), ToBytes);
      Move(Source^, Dest^, TotalBytes);
      SetLength(FData, NewLen);
    end;
    aIndex:= (Flength - 1) * 5;
    Fdata[aIndex]:= '';
    Fdata[aIndex+1]:= '';
    Fdata[aIndex+2]:= '';
    Fdata[aIndex+3]:= '';
    Fdata[aIndex+4]:= '';
    Dec(Flength);
  end else
    badIndex(index);
end;

procedure TAttributesImpl.setURI(index: Integer;
  const uri: SAXString);
begin
  if ((index >= 0) and (index < Flength)) then
    Fdata[index*5]:= uri
  else
    badIndex(index);
end;

procedure TAttributesImpl.setLocalName(index: Integer;
  const localName: SAXString);
begin
  if ((index >= 0) and (index < Flength)) then
    Fdata[index*5+1]:= localName
  else
    badIndex(index);
end;

procedure TAttributesImpl.setQName(index: Integer;
  const qName: SAXString);
begin
  if ((index >= 0) and (index < Flength)) then
    Fdata[index*5+2]:= qName
  else
    badIndex(index);
end;

procedure TAttributesImpl.setType(index: Integer;
  const attrType: SAXString);
begin
  if ((index >= 0) and (index < Flength)) then
    Fdata[index*5+3]:= attrType
  else
    badIndex(index);
end;

procedure TAttributesImpl.setValue(index: Integer;
  const value: SAXString);
begin
  if ((index >= 0) and (index < Flength)) then
    Fdata[index*5+4]:= value
  else
    badIndex(index);
end;

procedure TAttributesImpl.ensureCapacity(n: Integer);
var max : Integer;
begin
  if (n <= 0) then
  begin
    Exit;
  end;

  if ((Fdata = nil) or (Length(Fdata)=0)) then
  begin
    max:= 25;
  end
  else if (Length(Fdata) >= n * 5) then
  begin
    Exit;
  end
  else
  begin
    max:= Length(Fdata);
  end;

  while (max < n * 5) do
    max:= max * 2;

  SetLength(Fdata, max);
end;

procedure TAttributesImpl.badIndex(index: Integer);
begin
  raise Exception.Create(Format(sBadIndex, [index]));
end;

{ TDefaultHandler }

function TDefaultHandler.resolveEntity(const publicId,
  systemId: SAXString): IInputSource;
begin
  Result:= nil;
end;

procedure TDefaultHandler.notationDecl(const name, publicId,
  systemId: SAXString);
begin
  // no op
end;

procedure TDefaultHandler.unparsedEntityDecl(const name, publicId,
  systemId, notationName: SAXString);
begin
  // no op
end;

procedure TDefaultHandler.setDocumentLocator(const Locator: ILocator);
begin
  // no op
end;

procedure TDefaultHandler.startDocument;
begin
  // no op
end;

procedure TDefaultHandler.endDocument;
begin
  // no op
end;

procedure TDefaultHandler.startPrefixMapping(const prefix,
  uri: SAXString);
begin
  // no op
end;

procedure TDefaultHandler.endPrefixMapping(const prefix: SAXString);
begin
  // no op
end;

procedure TDefaultHandler.startElement(const uri, localName,
  qName: SAXString; const atts: IAttributes);
begin
  // no op
end;

procedure TDefaultHandler.endElement(const uri, localName,
  qName: SAXString);
begin
  // no op
end;

procedure TDefaultHandler.characters(const ch : SAXString);
begin
  // no op
end;

procedure TDefaultHandler.ignorableWhitespace(const ch : SAXString);
begin
  // no op
end;

procedure TDefaultHandler.processingInstruction(const target,
  data: SAXString);
begin
  // no op
end;

procedure TDefaultHandler.skippedEntity(const name: SAXString);
begin
  // no op
end;

procedure TDefaultHandler.warning(const e: ISAXParseError);
begin
  // no op
end;

procedure TDefaultHandler.error(const e: ISAXParseError);
begin
  // no op
end;

procedure TDefaultHandler.fatalError(const e: ISAXParseError);
begin
  // no op
end;

{ TLocatorImpl }

constructor TLocatorImpl.Create;
begin
  inherited Create();
end;

constructor TLocatorImpl.Create(locator: ILocator);
begin
  inherited Create();
  setPublicId(locator.getPublicId());
  setSystemId(locator.getSystemId());
  setLineNumber(locator.getLineNumber());
  setColumnNumber(locator.getColumnNumber());
end;

procedure TLocatorImpl.setPublicId(publicId: PSAXChar);
begin
  FpublicId:= publicId;
end;

procedure TLocatorImpl.setSystemId(systemId: PSAXChar);
begin
  FsystemId:= systemId;
end;

procedure TLocatorImpl.setLineNumber(lineNumber: Integer);
begin
  FlineNumber:= lineNumber;
end;

procedure TLocatorImpl.setColumnNumber(columnNumber: Integer);
begin
  FcolumnNumber:= columnNumber;
end;

function TLocatorImpl.getPublicId: PSAXChar;
begin
  Result:= FpublicId;
end;

function TLocatorImpl.getSystemId: PSAXChar;
begin
  Result:= FsystemId;
end;

function TLocatorImpl.getLineNumber: Integer;
begin
  Result:= FlineNumber;
end;

function TLocatorImpl.getColumnNumber: Integer;
begin
  Result:= FcolumnNumber;
end;

{ TContext }

constructor TNamespaceContext.Create;
begin
  inherited Create();
  FprefixTableLength:= 0;
  FuriTableLength:= 0;
  FelementNameTableLength:= 0;
  FattributeNameTableLength:= 0;
  FdeclSeen:= False;
  FdeclsOK:= true;
  FdefaultNS:= '';
  copyTables();
end;

destructor TNamespaceContext.Destroy;
begin
  inherited Destroy();
end;

procedure TNamespaceContext.setParent(const parent: TNamespaceContext);
begin
  Fparent:= parent;
  Fdeclarations:= nil;
  FdefaultNS:= Fparent.defaultNS;
  FdeclSeen:= false;
  FdeclsOK:= true;
end;

procedure TNamespaceContext.clear;
begin
  Fparent:= nil;
  Fdeclarations:= nil;
  FprefixTableLength:= 0;
  FuriTableLength:= 0;
  FelementNameTableLength:= 0;
  FattributeNameTableLength:= 0;
  FdefaultNS:= '';
end;

procedure TNamespaceContext.declarePrefix(const prefix, uri: SAXString);
begin
  declarePrefix(PSAXChar(prefix), Length(prefix), PSAXChar(uri), Length(uri));
end;

procedure TNamespaceContext.declarePrefix(prefix : PSAXChar; prefixLength : Integer;
  uri : PSAXChar; uriLength : Integer);
var i : Integer;
    found : Boolean;
begin
  // Lazy Processing...
  if (not FdeclsOK) then
    raise ESAXIllegalStateException.Create(sCantDeclare);

  if (not FdeclSeen) then
  begin
    // It is a ref to the parent tables and we need to modify-- copy and then modify
    copyTables();
  end;

  if (Fdeclarations = nil) then
  begin
    SetLength(Fdeclarations, 0);
  end;

  if (prefixLength = 0) or (prefix = '') then
  begin
    if (uriLength = 0) or (uri = '') then
      FdefaultNS:= ''
    else
      FdefaultNS:= PSAXCharToSAXString(uri, uriLength);
  end else
  begin
    found:= False;
    for i:= 0 to FprefixTableLength-1 do
    begin
      if (SAXStrEqual(PSAXChar(FprefixTable[i]), -1, prefix, prefixLength)) then
      begin
        found:= true;
        FprefixTableElements[i]:= PSAXCharToSAXString(uri, uriLength);
        break;
      end;
    end;
    if (not found) then
    begin
      Inc(FprefixTableLength);
      if (Length(FprefixTable) < FprefixTableLength) then
        SetLength(FprefixTable, FprefixTableLength);
      if (Length(FprefixTableElements) < FprefixTableLength) then
        SetLength(FprefixTableElements, FprefixTableLength);
      FprefixTable[FprefixTableLength-1]:= PSAXCharToSAXString(prefix, prefixLength);
      FprefixTableElements[FprefixTableLength-1]:= PSAXCharToSAXString(uri, uriLength);
    end;

    // may wipe out another prefix
    found:= False;
    for i:= 0 to FuriTableLength-1 do
    begin
      if (SAXStrEqual(PSAXChar(FuriTable[i]), -1, uri, uriLength)) then
      begin
        found:= true;
        FuriTableElements[i]:= PSAXCharToSAXString(prefix, prefixLength);
        break;
      end;
    end;
    if (not found) then
    begin
      Inc(FuriTableLength);
      if (Length(FuriTable) < FuriTableLength) then
        SetLength(FuriTable, FuriTableLength);
      if (Length(FuriTableElements) < FuriTableLength) then
        SetLength(FuriTableElements, FuriTableLength);
      FuriTable[FuriTableLength-1]:= PSAXCharToSAXString(uri, uriLength);
      FuriTableElements[FuriTableLength-1]:= PSAXCharToSAXString(prefix, prefixLength);
    end;
  end;
  SetLength(Fdeclarations, Length(Fdeclarations) + 1);
  Fdeclarations[Length(Fdeclarations)-1]:= PSAXCharToSAXString(prefix, prefixLength);
end;


function TNamespaceContext.processName(const qName: SAXString;
  isAttribute: Boolean): TNamespaceParts;
begin
  Result:= processName(PSAXChar(qName), -1, isAttribute);
end;

function TNamespaceContext.processName(qName : PSAXChar; qNameLength : Integer;
  isAttribute : Boolean) : TNamespaceParts;
var name : TNamespaceParts;
    index, i : Integer;
    prefix, local, uri : SAXString;
begin
  // detect errors in call sequence
  FdeclsOK:= false;

  // Start by looking in the cache, and return immediately if the name is already
  // known in this content
  if (isAttribute) then
  begin
    for i:= 0 to FattributeNameTableLength-1 do
    begin
      if (SAXStrEqual(PSAXChar(FattributeNameTable[i]), -1, qName, qNameLength)) then
      begin
        Result:= FattributeNameTableElements[i];
        Exit;
      end;
    end;
  end else
  begin
    for i:= 0 to FelementNameTableLength-1 do
    begin
      if (SAXStrEqual(PSAXChar(FelementNameTable[i]), -1, qName, qNameLength)) then
      begin
        Result:= FelementNameTableElements[i];
        Exit;
      end;
    end;
  end;

  // We haven't seen this name in this
  // context before.  Maybe in the parent
  // context, but we can't assume prefix
  // bindings are the same.
  name[nsQName]:= PSAXCharToSAXString(qName, qNameLength);
  index:= Pos(':', name[nsQName]);

  // No prefix.
  if (index < 1) then
  begin
    if (isAttribute) then
    begin
      if (qName = 'xmlns') and (FnamespaceDeclUris) then
        name[nsURI]:= XML_NSDECL
      else
        name[nsURI]:= '';
    end else if (FdefaultNS = '') then
      name[nsURI]:= ''
    else
      name[nsURI]:= FdefaultNS;
    name[nsLocal]:= name[nsQName];
  end else
  // Prefix
  begin
    prefix:= Copy(qName, 1, index-1);
    local:= Copy(qName, index+1, Length(qName));
    if ('' = prefix) then
      uri:= FdefaultNS
    else begin
      // again we have to check if we should use the parent
      uri:= '';
      for i:= 0 to prefixTableLength-1 do
      begin
        if (prefix = prefixTable[i]) then
        begin
          uri:= prefixTableElements[i];
          break;
        end;
      end;
    end;
    if ('' = uri) or
       ((not isAttribute) and ('xmlns' = prefix))  then
    begin
      name[nsURI]:= '';
      name[nsLocal]:= '';
      name[nsQName]:= '';
      Result:= name;
      Exit;
    end;
    name[nsURI]:= uri;
    name[nsLocal]:= local;
  end;

  // Save in the cache for future use.
  // (Could be shared with parent context...)
  if (isAttribute) then
  begin
    index:= -1;
    for i:= 0 to FattributeNameTableLength-1 do
    begin
      if (FattributeNameTable[i] = name[nsQName]) then
      begin
        FattributeNameTableElements[index]:= name;
        index:= i;
        break;
      end;
    end;
    if (index = -1) then
    begin
      Inc(FattributeNameTableLength);
      if (Length(FattributeNameTable) < FattributeNameTableLength) then
        SetLength(FattributeNameTable, FattributeNameTableLength);
      if (Length(FattributeNameTableElements) < FattributeNameTableLength) then
        SetLength(FattributeNameTableElements, FattributeNameTableLength);
      FattributeNameTable[FattributeNameTableLength-1]:= name[nsQName];
      FattributeNameTableElements[FattributeNameTableLength-1]:= name;
    end;
  end else
  begin
    index:= -1;
    for i:= 0 to FelementNameTableLength-1 do
    begin
      if (FelementNameTable[i] = name[nsQName]) then
      begin
        FelementNameTableElements[index]:= name;
        index:= i;
        break;
      end;
    end;
    if (index = -1) then
    begin
      Inc(FelementNameTableLength);
      if (Length(FelementNameTable) < FelementNameTableLength) then
        SetLength(FelementNameTable, FelementNameTableLength);
      if (Length(FelementNameTableElements) < FelementNameTableLength) then
        SetLength(FelementNameTableElements, FelementNameTableLength);
      FelementNameTable[FelementNameTableLength-1]:= name[nsQName];
      FelementNameTableElements[FelementNameTableLength-1]:= name;
    end;
  end;
  Result:= name;
end;

function TNamespaceContext.getURI(const prefix: SAXString): SAXString;
begin
  Result:= getURI(PSAXChar(prefix), -1);
end;

function TNamespaceContext.getURI(prefix : PSAXChar;
  prefixLength : Integer) : PSAXChar;
var i : Integer;
begin
  if ((prefixLength = 0) or ('' = prefix)) then
    Result:= PSAXChar(FdefaultNS)
  else if (FprefixTable = nil) then
    Result:= ''
  else begin
    // again we have to check if we should use the parent
    Result:= '';
    for i:= 0 to prefixTableLength-1 do
    begin
      if (SAXStrEqual(PSAXChar(prefixTable[i]), -1, prefix, prefixLength)) then
      begin
        Result:= PSAXChar(prefixTableElements[i]);
        break;
      end;
    end;
  end;
end;

function TNamespaceContext.getPrefix(const uri: SAXString): SAXString;
begin
end;

function TNamespaceContext.getPrefix(uri : PSAXChar;
  uriLength : Integer) : PSAXChar;
var i : Integer;
begin
  if (FuriTable = nil) then
    Result:= ''
  else begin
    // again we have to check if we should use the parent
    Result:= '';
    for i:= 0 to uriTableLength-1 do
    begin
      if (SAXStrEqual(PSAXChar(uriTable[i]), -1, uri, uriLength)) then
      begin
        Result:= PSAXChar(uriTableElements[i]);
        break;
      end;
    end;
  end;
end;

function TNamespaceContext.getDeclaredPrefixes: SAXStringArray;
begin
  if (Fdeclarations = nil) then
    Result:= EMPTY_ENUMERATION
  else begin
    Result:= Copy(declarations, 0, Length(declarations)-1);
  end;
end;

function TNamespaceContext.getPrefixes: SAXStringArray;
begin
  if (FprefixTable <> nil) then
    Result:= EMPTY_ENUMERATION
  else begin
    Result:= Copy(prefixTable, 0, prefixTableLength-1);
  end;
end;

procedure TNamespaceContext.copyTables;
var i : Integer;
begin
  if (Fparent <> nil) then
  begin
    // Create a clone
    FprefixTableLength:= Fparent.prefixTableLength;
    if (Length(FprefixTable) < FprefixTableLength) then
      SetLength(FprefixTable, FprefixTableLength);
    if (Length(FprefixTableElements) < FprefixTableLength) then
      SetLength(FprefixTableElements, FprefixTableLength);
    for i:= 0 to Fparent.prefixTableLength-1 do
    begin
      FprefixTable[i]:= Fparent.prefixTable[i];
      FprefixTableElements[i]:= Fparent.prefixTableElements[i];
    end;
    FuriTableLength:= Fparent.uriTableLength;
    if (Length(FuriTable) < FuriTableLength) then
      SetLength(FuriTable, FuriTableLength);
    if (Length(FuriTableElements) < FuriTableLength) then
      SetLength(FuriTableElements, FuriTableLength);
    for i:= 0 to Fparent.uriTableLength-1 do
    begin
      FuriTable[i]:= Fparent.uriTable[i];
      FuriTableElements[i]:= Fparent.uriTableElements[i];
    end;
  end;
  // Simply override the refs
  FelementNameTableLength:= 0;
  FattributeNameTableLength:= 0;
  // It's dirty
  FdeclSeen:= True;
end;

{ TNamespaceSupport }

constructor TNamespaceSupport.Create;
begin
  inherited Create();
  Fcontexts:= TList.Create;
  reset();
end;

destructor TNamespaceSupport.Destroy;
begin
  freeContexts();
  Fcontexts.Free;
  inherited Destroy();
end;

procedure TNamespaceSupport.freeContexts;
var I : Integer;
begin
  for I:= 0 to Fcontexts.Count-1 do
  begin
    if (Fcontexts[i] <> nil) then
      TNamespaceContext(Fcontexts[i]).Free;
  end;
  Fcontexts.Clear;
  Fcontexts.Count:= 32;
end;

procedure TNamespaceSupport.reset;
begin
  freeContexts();
  FnamespaceDeclUris:= false;
  FcontextPos:= 0;
  FcurrentContext:= TNamespaceContext.Create();
  FcurrentContext.declarePrefix('xml', XML_XMLNS);
  FcurrentContext.FnamespaceDeclUris:= FnamespaceDeclUris;
  Fcontexts[FcontextPos]:= FcurrentContext;
end;

procedure TNamespaceSupport.pushContext;
var max : Integer;
begin
  max:= Fcontexts.Count;

  TNamespaceContext(Fcontexts[FcontextPos]).FdeclsOK:= false;
  TNamespaceContext(Fcontexts[FcontextPos]).FnamespaceDeclUris:= FnamespaceDeclUris;

  Inc(FcontextPos);

  // Extend the array if necessary
  if (FcontextPos >= max) then
  begin
    Fcontexts.Count:= max * 2;
  end;

  // Allocate the context if necessary.
  FcurrentContext:= Fcontexts[FcontextPos];
  if (FcurrentContext = nil) then
  begin
    FcurrentContext:= TNamespaceContext.Create;
    FcurrentContext.FnamespaceDeclUris:= FnamespaceDeclUris;
    Fcontexts[FcontextPos]:= FcurrentContext;
  end;

  // Set the parent, if any.
  if (FcontextPos > 0) then
    FcurrentContext.setParent(Fcontexts[FcontextPos-1]);
end;

procedure TNamespaceSupport.popContext;
begin
  // We need to clear the current context
  FcurrentContext.clear();
  // Back to normal
  Dec(FcontextPos);
  if (FcontextPos < 0) then
    raise Exception.Create(sStackEmpty);
  FcurrentContext:= Fcontexts[FcontextPos];
end;

function TNamespaceSupport.declarePrefix(const prefix, uri: SAXString) : Boolean;
begin
  Result:= declarePrefix(PSAXChar(prefix), -1, PSAXChar(uri), -1);
end;

function TNamespaceSupport.declarePrefix(prefix : PSAXChar;
  prefixLength : Integer; uri : PSAXChar; uriLength : Integer) : Boolean;
var xml, xmlns : SAXString;
begin
  xml:= 'xml';
  xmlns:= 'xmlns';
  if (SAXStrEqual(PSAXChar(xml), -1, prefix, prefixLength) or
      SAXStrEqual(PSAXChar(xmlns), -1, prefix, prefixLength)) then
    Result:= False
  else begin
    FcurrentContext.declarePrefix(prefix, prefixLength, uri, uriLength);
    Result:= True;
  end;
end;

function TNamespaceSupport.processName(const qName: SAXString;
  var parts : TNamespaceParts; isAttribute: Boolean): TNamespaceParts;
begin
  Result:= processName(PSAXChar(qName), -1, parts, isAttribute);
end;

function TNamespaceSupport.processName(qName : PSAXChar; qNameLength : Integer;
  var parts : TNamespaceParts; isAttribute : Boolean) : TNamespaceParts;
var myParts : TNamespaceParts;
begin
  myParts:= FcurrentContext.processName(qName, qNameLength, isAttribute);
  if (myParts[nsURI]   = '') and
     (myParts[nsLocal] = '') and
     (myParts[nsQName] = '') then
    Result:= myParts
  else begin
    parts[nsURI]  := myParts[nsURI];
    parts[nsLocal]:= myParts[nsLocal];
    parts[nsQName]:= myParts[nsQName];
    Result:= parts;
  end;
end;

function TNamespaceSupport.getURI(const prefix: SAXString): SAXString;
begin
  Result:= getURI(PSAXChar(prefix), -1);
end;

function TNamespaceSupport.getURI(prefix : PSAXChar;
  prefixLength : Integer) : PSAXChar;
begin
  Result:= FcurrentContext.getURI(prefix, prefixLength);
end;

function TNamespaceSupport.getPrefixes: SAXStringArray;
begin
  Result:= FcurrentContext.getPrefixes();
end;

function TNamespaceSupport.getPrefix(const uri: SAXString): SAXString;
begin
  Result:= getPrefix(PSAXChar(uri), -1);
end;

function TNamespaceSupport.getPrefix(uri : PSAXChar;
  uriLength : Integer) : PSAXChar;
begin
  Result:= FcurrentContext.getPrefix(uri, uriLength);
end;

function TNamespaceSupport.getPrefixes(const uri : SAXString) : SAXStringArray;
begin
  Result:= getPrefixes(PSAXChar(uri), -1);
end;

function TNamespaceSupport.getPrefixes(uri : PSAXChar;
  uriLength : Integer) : SAXStringArray;
var prefixes : SAXStringArray;
    allPrefixes : SAXStringArray;
    i, Len : Integer;
    prefix : SAXString;
begin
  Len:= 0;
  SetLength(prefixes, Len);
  allPrefixes:= getPrefixes();
  for i:= 0 to Length(allPrefixes)-1 do
  begin
    prefix:= allPrefixes[i];
    if (SAXStrEqual(PSAXChar(getURI(prefix)), -1, uri, uriLength)) then
    begin
      Inc(Len);
      SetLength(prefixes, Len);
      prefixes[Len-1]:= prefix;
    end;
  end;
  Result:= prefixes;
end;

function TNamespaceSupport.getDeclaredPrefixes: SAXStringArray;
begin
  Result:= FcurrentContext.getDeclaredPrefixes();
end;

procedure TNamespaceSupport.setNamespaceDeclUris(value : Boolean);
begin
  if (FcontextPos <> 0) then
    raise ESAXIllegalStateException.Create('');
  if (value = FnamespaceDeclUris) then
    Exit;
  FnamespaceDeclUris:= value;
  if (value) then
    FcurrentContext.declarePrefix('xmlns', XML_NSDECL)
  else begin
    // Allocate the context if necessary.
    FcurrentContext:= Fcontexts[FcontextPos];
    if (FcurrentContext = nil) then
    begin
      FcurrentContext:= TNamespaceContext.Create;
      FcurrentContext.FnamespaceDeclUris:= FnamespaceDeclUris;
      Fcontexts[FcontextPos]:= FcurrentContext;
    end;
    FcurrentContext.declarePrefix('xml', XML_XMLNS);
  end;
end;

function TNamespaceSupport.isNamespaceDeclUris() : Boolean;
begin
  Result:= FnamespaceDeclUris;
end;

{ TXMLFilterImpl }

constructor TXMLFilterImpl.Create;
begin
  inherited Create();
  Fparent:= nil;
  Flocator:= nil;
  FentityResolver:= nil;
  FdtdHandler:= nil;
  FcontentHandler:= nil;
  FerrorHandler:= nil;
end;

constructor TXMLFilterImpl.Create(const parent: IXMLReader);
begin
  inherited Create();
  setParent(parent);
  Flocator:= nil;
  FentityResolver:= nil;
  FdtdHandler:= nil;
  FcontentHandler:= nil;
  FerrorHandler:= nil;
end;

procedure TXMLFilterImpl.setParent(const parent: IXMLReader);
begin
  Fparent:= parent;
end;

function TXMLFilterImpl.getParent: IXMLReader;
begin
  Result:= Fparent;
end;

procedure TXMLFilterImpl.setFeature(const name: SAXString;
  value: Boolean);
begin
  if (Fparent <> nil) then
    Fparent.setFeature(name, value)
  else
    raise ESAXNotRecognizedException.Create(Format(sFeatureName, [Name]));
end;

function TXMLFilterImpl.getFeature(const name: SAXString): Boolean;
begin
  if (Fparent <> nil) then
    Result:= Fparent.getFeature(name)
  else
    raise ESAXNotRecognizedException.Create(Format(sFeatureName, [Name]));
end;

function TXMLFilterImpl.getProperty(const name: SAXString) : IProperty;
begin
  if (Fparent <> nil) then
    Result:= Fparent.getProperty(name)
  else
    raise ESAXNotRecognizedException.Create(Format(sPropertyName, [Name]));
end;

procedure TXMLFilterImpl.setEntityResolver(
  const resolver: IEntityResolver);
begin
  FentityResolver:= resolver;
end;

function TXMLFilterImpl.getEntityResolver: IEntityResolver;
begin
  Result:= FentityResolver;
end;

procedure TXMLFilterImpl.setDTDHandler(const handler: IDTDHandler);
begin
  FdtdHandler:= handler;
end;

function TXMLFilterImpl.getDTDHandler: IDTDHandler;
begin
  Result:= FdtdHandler;
end;

procedure TXMLFilterImpl.setContentHandler(
  const handler: IContentHandler);
begin
  FcontentHandler:= handler;
end;

function TXMLFilterImpl.getContentHandler: IContentHandler;
begin
  Result:= FcontentHandler;
end;

procedure TXMLFilterImpl.setErrorHandler(const handler: IErrorHandler);
begin
  FerrorHandler:= handler;
end;

function TXMLFilterImpl.getErrorHandler: IErrorHandler;
begin
  Result:= FerrorHandler;
end;

procedure TXMLFilterImpl.parse(const input: IInputSource);
begin
  setupParse();
  Fparent.parse(input);
end;

procedure TXMLFilterImpl.parse(const systemId: SAXString);
var source : IInputSource;
begin
  source:= TInputSource.Create(systemId) as IInputSource;
  try
    parse(source);
  finally
    source:= nil;
  end;
end;

function TXMLFilterImpl.resolveEntity(const publicId,
  systemId: SAXString): IInputSource;
begin
  if (FentityResolver <> nil) then
    Result:= FentityResolver.resolveEntity(publicId, systemId)
  else
    Result:= nil;
end;

procedure TXMLFilterImpl.notationDecl(const name, publicId,
  systemId: SAXString);
begin
  if (FdtdHandler <> nil) then
    FdtdHandler.notationDecl(name, publicId, systemId);
end;

procedure TXMLFilterImpl.unparsedEntityDecl(const name, publicId, systemId,
  notationName: SAXString);
begin
  if (FdtdHandler <> nil) then
    FdtdHandler.unparsedEntityDecl(name, publicId, systemId, notationName);
end;

procedure TXMLFilterImpl.setDocumentLocator(const locator: ILocator);
begin
  Flocator:= locator;
  if (FcontentHandler <> nil) then
    FcontentHandler.setDocumentLocator(locator);
end;

procedure TXMLFilterImpl.startDocument;
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.startDocument();
end;

procedure TXMLFilterImpl.endDocument;
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.endDocument();
end;

procedure TXMLFilterImpl.startPrefixMapping(const prefix, uri: SAXString);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.startPrefixMapping(prefix, uri);
end;

procedure TXMLFilterImpl.endPrefixMapping(const prefix: SAXString);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.endPrefixMapping(prefix);
end;

procedure TXMLFilterImpl.startElement(const uri, localName,
  qName: SAXString; const atts: IAttributes);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.startElement(uri, localName, qName, atts);
end;

procedure TXMLFilterImpl.endElement(const uri, localName,
  qName: SAXString);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.endElement(uri, localName, qName);
end;

procedure TXMLFilterImpl.characters(const ch : SAXString);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.characters(ch);
end;

procedure TXMLFilterImpl.ignorableWhitespace(const ch : SAXString);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.ignorableWhitespace(ch);
end;

procedure TXMLFilterImpl.processingInstruction(const target,
  data: SAXString);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.processingInstruction(target, data);
end;

procedure TXMLFilterImpl.skippedEntity(const name: SAXString);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.skippedEntity(name);
end;

procedure TXMLFilterImpl.warning(const e: ISAXParseError);
begin
  if (FerrorHandler <> nil) then
    FerrorHandler.warning(e);
end;

procedure TXMLFilterImpl.error(const e: ISAXParseError);
begin
  if (FerrorHandler <> nil) then
    FerrorHandler.error(e);
end;

procedure TXMLFilterImpl.fatalError(const e: ISAXParseError);
begin
  if (FerrorHandler <> nil) then
    FerrorHandler.fatalError(e);
end;

procedure TXMLFilterImpl.setupParse;
begin
  if (Fparent = nil) then
    raise ESAXException.Create(sNoParent)
  else begin
    Fparent.setEntityResolver(self);
    Fparent.setDTDHandler(self);
    Fparent.setContentHandler(self);
    Fparent.setErrorHandler(self);
  end;
end;

{ TXMLReaderImpl }

constructor TXMLReaderImpl.Create;
begin
  inherited Create;
  setContentHandler(nil);
  setDTDHandler(nil);
  setEntityResolver(nil);
  setErrorHandler(nil);
  FcolumnNumber := -1;
  FlineNumber   := -1;
  Fnamespaces   := True;
  Fparsing      := False;
  Fprefixes     := False;
  FpublicId     := '';
  FsystemId     := '';
end;

destructor TXMLReaderImpl.Destroy;
begin
  FcontentHandler:= nil;
  FdtdHandler    := nil;
  FentityResolver:= nil;
  FerrorHandler  := nil;
  inherited;
end;

procedure TXMLReaderImpl.checkNotParsing(const propType, name: SAXString);
begin
  if Fparsing then
    raise ESAXNotSupportedException.Create(Format(sCantChange, [propType, name]));
end;

function TXMLReaderImpl.getColumnNumber: Integer;
begin
  Result:= FcolumnNumber;
end;

function TXMLReaderImpl.getContentHandler: IContentHandler;
begin
  Result:= FcontentHandler;
end;

function TXMLReaderImpl.getDTDHandler: IDTDHandler;
begin
  Result:= FdtdHandler;
end;

function TXMLReaderImpl.getEntityResolver: IEntityResolver;
begin
  Result:= FentityResolver;
end;

function TXMLReaderImpl.getErrorHandler: IErrorHandler;
begin
  Result:= FerrorHandler;
end;

function TXMLReaderImpl.getFeature(const name: SAXString): Boolean;
begin
  if (name = NamespacesFeature) then
    Result:= Fnamespaces
  else if (name = NamespacePrefixesFeature) then
    Result:= FPrefixes
  else if (name = ValidationFeature) or (name = ExternalGeneralFeature) or
      (name = ExternalParameterFeature) then
    raise ESAXNotSupportedException.Create(Format(sFeatureName, [name]))
  else
    raise ESAXNotRecognizedException.Create(Format(sFeatureName, [name]));
end;

function TXMLReaderImpl.getLineNumber: Integer;
begin
  Result:= FlineNumber;
end;

function TXMLReaderImpl.getProperty(const name: SAXString) : IProperty;
begin
  raise ESAXNotRecognizedException.Create(Format(sPropertyName, [name]));
end;

function TXMLReaderImpl.getPublicId: PSAXChar;
begin
  Result:= PSAXChar(FpublicId);
end;

function TXMLReaderImpl.getSystemId: PSAXChar;
begin
  Result:= PSAXChar(FsystemId);
end;

procedure TXMLReaderImpl.parse(const input: IInputSource);
begin
  FpublicId:= input.publicId;
  FsystemId:= input.systemId;
  Fparsing := True;
  try
    parseInput(input);
  finally
    Fparsing:= False;
  end;
end;

procedure TXMLReaderImpl.parse(const systemId: SAXString);
var
  input: IInputSource;
  resolver : IEntityResolver;
begin
  resolver:= getEntityResolver();
  if Assigned(resolver) then
    input:= resolver.resolveEntity('', systemId);
  if not Assigned(input) then
    input:= TInputSource.Create(systemId) as IInputSource;
  try
    parse(input);
  finally
    input:= nil;
  end;
end;

procedure TXMLReaderImpl.setColumnNumber(value: Integer);
begin
  FcolumnNumber:= value;
end;

procedure TXMLReaderImpl.setContentHandler(const handler: IContentHandler);
begin
  if Assigned(handler) then
    FcontentHandler:= handler
  else
    FcontentHandler:= handlerDefault;
end;

procedure TXMLReaderImpl.setDTDHandler(const handler: IDTDHandler);
begin
  if Assigned(handler) then
    FdtdHandler:= handler
  else
    FdtdHandler:= handlerDefault;
end;

procedure TXMLReaderImpl.setEntityResolver(
  const resolver: IEntityResolver);
begin
  if Assigned(resolver) then
    FentityResolver:= resolver
  else
    FentityResolver:= handlerDefault;
end;

procedure TXMLReaderImpl.setErrorHandler(const handler: IErrorHandler);
begin
  if Assigned(handler) then
    FerrorHandler:= handler
  else
    FerrorHandler:= handlerDefault;
end;

procedure TXMLReaderImpl.setFeature(const name: SAXString;
  value: Boolean);
begin
  // The only features supported are namespaces and namespace-prefixes.
  if name = namespacesFeature then
  begin
    checkNotParsing(sFeatureCheck, name);
    Fnamespaces:= value;
    if not Fnamespaces and not Fprefixes then
      Fprefixes:= True;
  end
  else if name = namespacePrefixesFeature then
  begin
    checkNotParsing(sFeatureCheck, name);
    Fprefixes:= Value;
    if not Fprefixes and not Fnamespaces then
      Fnamespaces:= True;
  end
  else if (name = ValidationFeature) or (name = ExternalGeneralFeature) or
      (name = ExternalParameterFeature) then
    raise ESAXNotSupportedException.Create(Format(sFeatureName, [name]))
  else
    raise ESAXNotRecognizedException.Create(Format(sFeatureName, [name]));
end;

procedure TXMLReaderImpl.setLineNumber(value: Integer);
begin
  FlineNumber:= value;
end;

procedure TXMLReaderImpl.setPublicId(value: PSAXChar);
begin
  FpublicId:= value;
end;

procedure TXMLReaderImpl.setSystemId(value: PSAXChar);
begin
  FsystemId:= value;
end;

{ TNamespaceContext }

function TNamespaceContext.getDefaultNS: PSAXChar;
begin
  if (FdeclSeen) then
    Result:= PSAXChar(FdefaultNS)
  else
    Result:= PSAXChar(Fparent.defaultNS);
end;

function TNamespaceContext.getPrefixTable: SAXStringArray;
begin
  if (FdeclSeen) then
    Result:= FprefixTable
  else
    Result:= Fparent.prefixTable;
end;

function TNamespaceContext.getPrefixTableElements: SAXStringArray;
begin
  if (FdeclSeen) then
    Result:= FprefixTableElements
  else
    Result:= Fparent.prefixTableElements;
end;

function TNamespaceContext.getPrefixTableLength: Integer;
begin
  if (FdeclSeen) then
    Result:= FprefixTableLength
  else
    Result:= Fparent.prefixTableLength;
end;

function TNamespaceContext.getUriTable: SAXStringArray;
begin
  if (FdeclSeen) then
    Result:= FuriTable
  else
    Result:= Fparent.uriTable;
end;

function TNamespaceContext.getUriTableElements: SAXStringArray;
begin
  if (FdeclSeen) then
    Result:= FuriTableElements
  else
    Result:= Fparent.uriTableElements;
end;

function TNamespaceContext.getUriTableLength: Integer;
begin
  if (FdeclSeen) then
    Result:= FuriTableLength
  else
    Result:= Fparent.uriTableLength;
end;

function TNamespaceContext.getDeclarations: SAXStringArray;
begin
  if (FdeclSeen) then
    Result:= Fdeclarations
  else
    Result:= Fparent.declarations;
end;

{ TAttributes2Impl }

procedure TAttributes2Impl.addAttribute(const uri, localName, qName, attrType,
  value: SAXString);
var len : Integer;
begin
  inherited addAttribute(uri, localName, qName, attrType, value);
  len:= getLength();

  if (len < Length(Fspecified)) then
  begin
    SetLength(Fdeclared, len);
    SetLength(Fspecified, len);
  end;

  Fspecified[len - 1]:= true;
  Fdeclared[len - 1]:= not (attrType = 'CDATA');
end;

function TAttributes2Impl.isDeclared(index : Integer) : Boolean;
begin
  if ((index < 0) or (index >= getLength())) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeAtIndex, [index]);
  Result:= Fdeclared[index];
end;

function TAttributes2Impl.isDeclared(const qName : SAXString) : Boolean;
var index : Integer;
begin
  index:= getIndex(qName);

  if (index < 0) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeQName, [qName]);
  Result:= Fdeclared[index];
end;

function TAttributes2Impl.isDeclared(const uri,
  localName : SAXString) : Boolean;
var index : Integer;
begin
  index:= getIndex(uri, localName);

  if (index < 0) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeLocalUri, [localName, uri]);
  Result:= Fdeclared[index];
end;

function TAttributes2Impl.isSpecified(const uri,
  localName: SAXString): Boolean;
var index : Integer;
begin
  index:= getIndex(uri, localName);

  if (index < 0) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeLocalUri, [localName, uri]);
  Result:= Fspecified[index];
end;

function TAttributes2Impl.isSpecified(index: Integer): Boolean;
begin
  if ((index < 0) or (index >= getLength())) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeAtIndex, [index]);
  Result:= Fspecified[index];
end;

function TAttributes2Impl.isSpecified(const qName: SAXString): Boolean;
var index : Integer;
begin
  index:= getIndex(qName);

  if (index < 0) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeQName, [qName]);
  Result:= Fspecified[index];
end;

procedure TAttributes2Impl.removeAttribute(index: Integer);
var origMax : Integer;
    PDest, PSource : Pointer;
begin
  origMax:= getLength() - 1;

  inherited removeAttribute(index);

  if (index <> origMax) then
  begin
    // This is just a faster way of deleting an item
    PDest:= Pointer(Fspecified);
    Inc(Cardinal(PDest), index);
    PSource:= PDest;
    Inc(Cardinal(PSource), 1);
    Move(PSource^, PDest^, origMax-index);
    SetLength(fspecified, origMax-1);

    // This is just a faster way of deleting an item
    PDest:= Pointer(Fdeclared);
    Inc(Cardinal(PDest), index);
    PSource:= PDest;
    Inc(Cardinal(PSource), 1);
    Move(PSource^, PDest^, origMax-index);
    SetLength(fdeclared, origMax-1);
  end;
end;

procedure TAttributes2Impl.setAttributes(const atts: IAttributes);
var length, I : Integer;
    flags : array of Boolean;
    a2 : IAttributes2;
begin
  length:= atts.getLength();

  inherited setAttributes(atts);

  SetLength(flags, length);

  if (atts.QueryInterface(IAttributes2, a2) = 0) then
  begin
    for I := 0 to length-1 do
      flags[I]:= a2.isSpecified(I);
  end
  else
  begin
    for I := 0 to length-1 do
      flags[I]:= true;
  end;
end;

procedure TAttributes2Impl.setDeclared(index: Integer;
  value: Boolean);
begin
  if ((index < 0) or (index >= getLength())) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeAtIndex, [index]);
  Fdeclared[index]:= value;
end;

procedure TAttributes2Impl.setSpecified(index: Integer;
  value: Boolean);
begin
  if ((index < 0) or (index >= getLength())) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeAtIndex, [index]);
  Fspecified[index]:= value;
end;

{ TDefaultHandler2 }

procedure TDefaultHandler2.attributeDecl(const eName, aName, attrType,
  mode, value: SAXString);
begin
  // do nothing
end;

procedure TDefaultHandler2.comment(const ch : SAXString);
begin
  // do nothing
end;

procedure TDefaultHandler2.elementDecl(const name, model: SAXString);
begin
  // do nothing
end;

procedure TDefaultHandler2.endCDATA;
begin
  // do nothing
end;

procedure TDefaultHandler2.endDTD;
begin
  // do nothing
end;

procedure TDefaultHandler2.endEntity(const name: SAXString);
begin
  // do nothing
end;

procedure TDefaultHandler2.externalEntityDecl(const name, publicId,
  systemId: SAXString);
begin
  // do nothing
end;

function TDefaultHandler2.getExternalSubset(const name,
  baseURI: SAXString): IInputSource;
begin
  Result:= nil;
end;

procedure TDefaultHandler2.internalEntityDecl(const name,
  value: SAXString);
begin
  // do nothing
end;

function TDefaultHandler2.resolveEntity(const name, publicId, baseURI,
  systemId: SAXString): IInputSource;
begin
  Result:= nil;
end;

function TDefaultHandler2.resolveEntity(const publicId,
  systemId : SAXString) : IInputSource;
begin
  Result:= resolveEntity('', publicId, '', systemId);
end;

procedure TDefaultHandler2.startCDATA;
begin
  // do nothing
end;

procedure TDefaultHandler2.startDTD(const name, publicId,
  systemId: SAXString);
begin
  // do nothing
end;

procedure TDefaultHandler2.startEntity(const name: SAXString);
begin
  // do nothing
end;

{ TLocator2Impl }

constructor TLocator2Impl.Create(locator: ILocator);
var l2 : ILocator2;
begin
  inherited create(locator);

  if (locator.QueryInterface(ILocator2, l2) = 0) then
  begin
    Fversion:= l2.getXMLVersion();
    Fencoding:= l2.getEncoding();
  end;

end;

function TLocator2Impl.getEncoding: PSAXChar;
begin
  Result:= Fencoding;
end;

function TLocator2Impl.getXMLVersion: PSAXChar;
begin
  Result:= Fversion;
end;

procedure TLocator2Impl.setEncoding(encoding: PSAXChar);
begin
  Fencoding:= encoding;
end;

procedure TLocator2Impl.setXMLVersion(version: PSAXChar);
begin
  Fversion:= version;
end;

{ TSAXError }

constructor TSAXError.create(message: PSAXChar);
begin
  inherited create();
  init(message);
end;

function TSAXError.getMessage: PSAXChar;
begin
  Result:= Fmessage;
end;

function TSAXError.getNativeError: IUnknown;
begin
  Result:= nil;
end;

procedure TSAXError.init(message: PSAXChar);
begin
  Fmessage:= message;
end;

{ TSAXParseError }

constructor TSAXParseError.create(message: PSAXChar;
  const locator: ILocator);
begin
  inherited create(message);
  FpublicId:= locator.getPublicId();
  FsystemId:= locator.getSystemId();
  FlineNumber:= locator.getLineNumber();
  FcolumnNumber:= locator.getColumnNumber();
end;

constructor TSAXParseError.create(const message : SAXString; publicId,
  systemId: PSAXChar; lineNumber, columnNumber: Integer);
begin
  inherited create(PSAXChar(message));
  FpublicId:= publicId;
  FsystemId:= systemId;
  FlineNumber:= lineNumber;
  FcolumnNumber:= columnNumber;
end;

function TSAXParseError.getColumnNumber: Integer;
begin
  Result:= FcolumnNumber;
end;

function TSAXParseError.getLineNumber: Integer;
begin
  Result:= FlineNumber;
end;

function TSAXParseError.getPublicId: PSAXChar;
begin
  Result:= FpublicId;
end;

function TSAXParseError.getSystemId: PSAXChar;
begin
  Result:= FsystemId;
end;

procedure TSAXParseError.init(message: PSAXChar; const locator: ILocator);
begin
  inherited init(message);
  FpublicId:= locator.getPublicId();
  FsystemId:= locator.getSystemId();
  FlineNumber:= locator.getLineNumber();
  FcolumnNumber:= locator.getColumnNumber();
end;

initialization

  // Create a default handler object
  HandlerDefault := TDefaultHandler.Create;
  // Keep it around
  HandlerDefault._AddRef;

finalization

  // Release the reference
  HandlerDefault._Release;

end.
