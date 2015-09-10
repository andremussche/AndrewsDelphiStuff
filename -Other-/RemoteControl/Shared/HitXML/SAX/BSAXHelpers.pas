// SAX for Pascal Buffered Helper Classes, Simple API for XML Interfaces in Pascal.
// Ver 1.1 July 4, 2003
// http://xml.defined.net/SAX (this will change!)
// Based on http://www.saxproject.org/
// No warranty; no copyright -- use this as you will.
unit BSAXHelpers;

interface

uses Classes, SysUtils, SAX, SAXExt, BSAX, BSAXExt;

type

  // Helper Classes
  TBufferedAttributesImpl = class;
  TBufferedDefaultHandler = class;
  TBufferedXMLFilterImpl = class;
  TBufferedXMLReaderImpl = class;

  // Extension Helper Classes
  TBufferedAttributes2Impl = class;
  TBufferedDefaultHandler2 = class;

  TBufferedAttributesRecord = record
    uri : PSAXChar;
    uriLength : Integer;
    localName : PSAXChar;
    localNameLength : Integer;
    qName : PSAXChar;
    qNameLength : Integer;
    attrType : PSAXChar;
    attrTypeLength : Integer;
    value : PSAXChar;
    valueLength : Integer;
  end;

  // Default implementation of the IBufferedAttributes interface.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This class provides a default implementation of the SAX2
  // <a href="../BSAX/IBufferedAttributes.html">IBufferedAttributes</a> interface, with the
  // addition of manipulators so that the list can be modified or
  // reused.</p>
  //
  // <p>There are two typical uses of this class:</p>
  //
  // <ol>
  // <li>to take a persistent snapshot of an Attributes object
  //  in a <a href="../BSAX/IBufferedContentHandler.html#startElement">startElement</a> event; or</li>
  // <li>to construct or modify an IAttributes object in a SAX2 driver or filter.</li>
  // </ol>
  //
  // <p>This class replaces the now-deprecated SAX1 AttributeListImpl
  // class; in addition to supporting the updated Attributes
  // interface rather than the deprecated <a href="../BSAX/IAttributeList.html">IAttributeList</a>
  // interface, it also includes a much more efficient
  // implementation using a single array rather than a set of Vectors.</p>
  //
  // @since SAX 2.0
  TBufferedAttributesImpl = class(TInterfacedObject, IBufferedAttributes)
  private
    Flength : Integer;
    Fdata : array of TBufferedAttributesRecord;

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
    // @see <a href="../BSAX/IBufferedAttributes.html#getLength">IBufferedAttributes.getLength</a>
    function getLength() : Integer;

    // Return an attribute's Namespace URI.
    //
    // @param index The attribute index (zero-based).
    // @param uri The Namespace URI, or the empty string if none
    //         is available or if the index is out of range.
    // @param uriLength The length of the Namespace URI buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getURI">IBufferedAttributes.getURI</a>
    procedure getURI(index : Integer; out uri: PSAXChar;
      out uriLength: Integer);

    // Return an attribute's local name.
    //
    // @param index The attribute index (zero-based).
    // @param localName The local name, or the empty string if Namespace
    //         processing is not being performed or if the
    //         index is out of range.
    // @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getLocalName">IBufferedAttributes.getLocalName</a>
    procedure getLocalName(index : Integer; out localName: PSAXChar;
      out localNameLength: Integer);

    // Return an attribute's qualified (prefixed) name.
    //
    // @param index The attribute index (zero-based).
    // @param qName The XML 1.0 qualified name, or the empty string
    //         if none is available or if the index is out of
    //         range.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getQName">IBufferedAttributes.getQName</a>
    procedure getQName(index : Integer; out qName: PSAXChar;
      out qNameLength: Integer);

    // Return an attribute's type by index.
    //
    // @param index The attribute index (zero-based).
    // @param attrType The attribute's type as a string or an empty string if the
    //         index is out of range.
    // @param attrTypeLength The length of the attrType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getType.Integer.PSAXChar.Integer">IBufferedAttributes.getType(Integer,PSAXChar,Integer)</a>
    procedure getType(index : Integer;
      out attrType: PSAXChar; out attrTypeLength: Integer); overload;

    // Look up an attribute's type by Namespace-qualified name.
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
    // @param attrType The attribute's type as a string or an empty string if the
    //         attribute is not in the list
    // @param attrTypeLength The length of the attrType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getType.PSAXChar.Integer.PSAXChar.Integer.PSAXChar.Integer">IBufferedAttributes.getType(PSAXChar,Integer,PSAXChar,Integer,PSAXChar,Integer)</a>
    procedure getType(uri: PSAXChar; uriLength: Integer;
      localName: PSAXChar;  localNameLength: Integer;
      out attrType: PSAXChar; out attrTypeLength: Integer); overload;

    // Look up an attribute's type by qualified (prefixed) name.
    //
    // @param qName The XML 1.0 qualified name.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param attrType The attribute type as a string, or an empty string if the
    //         attribute is not in the list or if qualified names
    //         are not available.
    // @param attrTypeLength The length of the attrType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getType.PSAXChar.Integer.PSAXChar.Integer">IBufferedAttributes.getType(PSAXChar,Integer,PSAXChar,Integer)</a>
    procedure getType(qName: PSAXChar;  qNameLength: Integer;
      out attrType: PSAXChar; out attrTypeLength: Integer); overload;

    // Return an attribute's value by index.
    //
    // @param index The attribute index (zero-based).
    // @param value The attribute's value as a string, or an empty string if the
    //         index is out of range.
    // @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getValue.Integer.PSAXChar.Integer">IBufferedAttributes.getValue(Integer,PSAXChar,Integer)</a>
    procedure getValue(index : Integer;
       out value: PSAXChar; out valueLength: Integer); overload;

    // Look up an attribute's value by Namespace-qualified name.
    //
    // @param uri The Namespace URI, or the empty String if the
    //        name has no Namespace URI.
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param localName The local name of the attribute.
    // @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param value The attribute value as a string, or an empty string if the
    //         attribute is not in the list.
    // @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getValue.PSAXChar.Integer.PSAXChar.Integer">IBufferedAttributes.getValue(PSAXChar,Integer,PSAXChar,Integer)</a>
    procedure getValue(uri : PSAXChar; uriLength : Integer;
       localName : PSAXChar; localNameLength : Integer;
       out value: PSAXChar; out valueLength: Integer); overload;

    // Look up an attribute's value by qualified (prefixed) name.
    //
    // @param qName The qualified (prefixed) name.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param value The attribute value as a string, or an empty string if the
    //         attribute is not in the list.
    // @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @see <a href="../BSAX/IBufferedAttributes.html#getValue.PSAXChar.Integer.PSAXChar.Integer">IBufferedAttributes.getValue(PSAXChar,Integer,PSAXChar,Integer)</a>
    procedure getValue(qName : PSAXChar; qNameLength : Integer;
       out value: PSAXChar; out valueLength: Integer); overload;

    // Look up an attribute's index by Namespace name.
    //
    // <p>In many cases, it will be more efficient to look up the name once and
    // use the index query methods rather than using the name query methods
    // repeatedly.</p>
    //
    // @param uri The Namespace URI, or the empty string if
    //        the name has no Namespace URI.
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param localName The attribute's local name.
    // @param loclaNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return The index of the attribute, or -1 if it does not
    //         appear in the list.
    // @see <a href="../BSAX/IBufferedAttributes.html#getIndex.PSAXChar.Integer.PSAXChar.Integer">IBufferedAttributes.getIndex(PSAXChar,Integer,PSAXChar,Integer)</a>
    function getIndex(uri : PSAXChar; uriLength : Integer;
       localName : PSAXChar; localNameLength : Integer) : Integer; overload;

    // Look up an attribute's index by qualified (prefixed) name.
    //
    // @param qName The qualified (prefixed) name.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return The index of the attribute, or -1 if it does not
    //         appear in the list.
    // @see <a href="../BSAX/IBufferedAttributes.html#getIndex.PSAXChar.Integer">IBufferedAttributes.getIndex(PSAXChar,Integer)</a>
    function getIndex(qName : PSAXChar;
      qNameLength : Integer) : Integer; overload;

  public
    // Construct a new, empty BufferedAttributesImpl object.
    constructor Create(); overload;

    // Copy an existing BufferedAttributes object.
    //
    // <p>This constructor is especially useful inside a
    // <a href="../BSAX/IBufferedContentHandler.html#startElement">startElement</a> event.</p>
    //
    // @param atts The existing IBufferedAttributes object.
    constructor Create(const atts : IBufferedAttributes); overload;

    // Standard default destructor
    //
    // @see <a href="../BSAXHelpers/TBufferedAttributesImpl.html#create">TBufferedAttributesImpl.create</a>
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
    procedure setAttributes(const atts : IBufferedAttributes); virtual;

    // Add an attribute to the end of the list.
    //
    // <p>For the sake of speed, this method does no checking
    // to see if the attribute is already in the list: that is
    // the responsibility of the application.</p>
    //
    // @param uri The Namespace URI, or the empty string if
    //        none is available or Namespace processing is not
    //        being performed.
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param localName The local name, or the empty string if
    //        Namespace processing is not being performed.
    // @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param qName The qualified (prefixed) name, or the empty string
    //        if qualified names are not available.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param attrType The attribute type as a string.
    // @param attrTypeLength The length of the attrType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param value The attribute value.
    // @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    procedure addAttribute(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer; qName : PSAXChar;
      qNameLength : Integer; attrType : PSAXChar; attrTypeLength : Integer;
      value : PSAXChar; valueLength : Integer); virtual;

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
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param localName The local name, or the empty string if
    //        Namespace processing is not being performed.
    // @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param qName The qualified name, or the empty string
    //        if qualified names are not available.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param attrType The attribute type as a string.
    // @param attrTypeLength The length of the attrType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param value The attribute value.
    // @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setAttribute(index : Integer; uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer; qName : PSAXChar;
      qNameLength : Integer; attrType : PSAXChar; attrTypeLength : Integer;
      value : PSAXChar; valueLength : Integer);

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
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setURI(index : Integer; uri : PSAXChar; uriLength : Integer);

    // Set the local name of a specific attribute.
    //
    // @param index The index of the attribute (zero-based).
    // @param localName The attribute's local name, or the empty
    //        string for none.
    // @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setLocalName(index : Integer; localName : PSAXChar;
      localNameLength : Integer);

    // Set the qualified name of a specific attribute.
    //
    // @param index The index of the attribute (zero-based).
    // @param qName The attribute's qualified name, or the empty
    //        string for none.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setQName(index : Integer; qName : PSAXChar;
      qNameLength : Integer);

    // Set the type of a specific attribute.
    //
    // @param index The index of the attribute (zero-based).
    // @param attrType The attribute's type.
    // @param attrTypeLength The length of the attrType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setType(index : Integer; attrType : PSAXChar; attrTypeLength : Integer);

    // Set the value of a specific attribute.
    //
    // @param index The index of the attribute (zero-based).
    // @param value The attribute's value.
    // @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception Exception when the supplied index does not
    //            point to an attribute in the list.
    procedure setValue(index : Integer; value : PSAXChar;
      valueLength : Integer);

  end;

  // Default base class for SAX2 Buffered event handlers.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This class is available as a convenience base class for SAX2
  // applications: it provides default implementations for all of the
  // callbacks in the four core SAX2 buffered handler classes:</p>
  //
  // <ul>
  // <li><a href="../SAX/IEntityResolver.html">IEntityResolver</a></li>
  // <li><a href="../BSAX/IBufferedDTDHandler.html">IBufferedDTDHandler</a></li>
  // <li><a href="../BSAX/IBufferedContentHandler.html">IBufferedContentHandler</a></li>
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
  // @see <a href="../BSAX/IBufferedDTDHandler.html">IBufferedDTDHandler</a>
  // @see <a href="../BSAX/IBufferedContentHandler.html">IBufferedContentHandler</a>
  // @see <a href="../SAX/IErrorHandler.html">IErrorHandler</a>
  TBufferedDefaultHandler = class(TInterfacedObject, IEntityResolver,
    IBufferedDTDHandler, IBufferedContentHandler, IErrorHandler)
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
    function resolveEntity(const publicId, systemId : SAXString) : IInputSource;

    // Receive notification of a notation declaration.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass if they wish to keep track of the notations
    // declared in a document.</p>
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
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#unparsedEntityDecl">TBufferedDefaultHandler.unparsedEntityDecl</a>
    // @see <a href="../BSAX/IBufferedAttributes.html">IBufferedAttributes</a>
    procedure notationDecl(name : PSAXChar; nameLength : Integer;
      publicId : PSAXChar; publicIdLength : Integer;
      systemId : PSAXChar; systemIdLength : Integer); virtual;

    // Receive notification of an unparsed entity declaration.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to keep track of the unparsed entities
    // declared in a document.</p>
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
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#notationDecl">TBufferedDefaultHandler.notationDecl</a>
    // @see <a href="../BSAX/IBufferedAttributes.html">IBufferedAttributes</a>
    procedure unparsedEntityDecl(name : PSAXChar;
      nameLength : Integer; publicId : PSAXChar;
      publicIdLength : Integer; systemId : PSAXChar;
      systemIdLength : Integer; notationName : PSAXChar;
      notationNameLength : Integer); virtual;

    // Receive a Locator object for document events.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass if they wish to store the locator for use
    // with other document events.</p>
    //
    // @param locator An object that can return the location of
    //                any SAX document event.
    // @see <a href="../SAX/ILocator.html">ILocator</a>
    procedure setDocumentLocator(const locator: ILocator); virtual;

    // Receive notification of the beginning of the document.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions at the beginning
    // of a document (such as allocating the root node of a tree or
    // creating an output file).</p>
    //
    // @exception ESAXException Any SAX exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#endDocument">TBufferedDefaultHandler.endDocument</a>
    procedure startDocument(); virtual;

    // Receive notification of the end of the document.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions at the end
    // of a document (such as finalising a tree or closing an output
    // file).</p>
    //
    // @exception ESAXException Any SAX exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#startDocument">TBufferedDefaultHandler.startDocument</a>
    procedure endDocument(); virtual;

    // Receive notification of the start of a Namespace mapping.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions at the start of
    // each Namespace prefix scope (such as storing the prefix mapping).</p>
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
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#endPrefixMapping">TBufferedDefaultHandler.endPrefixMapping</a>
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#startElement">TBufferedDefaultHandler.startElement</a>
    procedure startPrefixMapping(prefix: PSAXChar;
      prefixLength: Integer; uri: PSAXChar;
      uriLength: Integer); virtual;

    // Receive notification of the end of a Namespace mapping.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions at the end of
    // each prefix mapping.</p>
    //
    // @param prefix The prefix that was being mapping.
    //               Use the empty string when a default mapping scope ends.
    // @param prefixLength The length of the prefix buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#startPrefixMapping">TBufferedDefaultHandler.startPrefixMapping</a>
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#endElement">TBufferedDefaultHandler.endElement</a>
    procedure endPrefixMapping(prefix: PSAXChar;
      prefixLength: Integer); virtual;

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
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#endElement">TBufferedDefaultHandler.endElement</a>
    // @see <a href="../BSAX/IBufferedAttributes.html">IBufferedAttributes</a>
    procedure startElement(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer;
      qName : PSAXChar; qNameLength : Integer;
      const atts: IBufferedAttributes); virtual;

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
      qName : PSAXChar; qNameLength : Integer); virtual;

    // Receive notification of character data inside an element.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method to take specific actions for each chunk of character data
    // (such as adding the data to a node or buffer, or printing it to
    // a file).</p>
    //
    // @param ch Pointer to the characters from the XML document.
    // @param chLength The length of the ch buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#ignorableWhitespace">TBufferedDefaultHandler.ignorableWhitespace</a>
    // @see <a href="../SAX/ILocator.html">ILocator</a>
    procedure characters(ch : PSAXChar; chLength : Integer); virtual;

    // Receive notification of ignorable whitespace in element content.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method to take specific actions for each chunk of ignorable
    // whitespace (such as adding data to a node or buffer, or printing
    // it to a file).</p>
    //
    // @param ch Pointer to the characters from the XML document.
    // @param chLength The length of the ch buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException Any SAX exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler.html#characters">TBufferedDefaultHandler.characters</a>
    procedure ignorableWhitespace(ch : PSAXChar; chLength : Integer); virtual;

    // Receive notification of a processing instruction.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions for each
    // processing instruction, such as setting status variables or
    // invoking other methods.</p>
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
      dataLength : Integer); virtual;

    // Receive notification of a skipped entity.
    //
    // <p>By default, do nothing.  Application writers may override this
    // method in a subclass to take specific actions for each
    // processing instruction, such as setting status variables or
    // invoking other methods.</p>
    //
    // @param name The name of the skipped entity.  If it is a
    //        parameter entity, the name will begin with '%', and if
    //        it is the external DTD subset, it will be the string
    //        "[dtd]".
    // @param nameLength The length of the name buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException Any SAX exception.
    procedure skippedEntity(name : PSAXChar; nameLength : Integer); virtual;

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


  // Buffered Base class for deriving an XML filter.
  //
  // <p>This class is designed to sit between an <a href="../BSAX/IBufferedXMLReader.html">IBufferedXMLReader</a>
  // and the client application's event handlers.  By default, it
  // does nothing but pass requests up to the reader and events
  // on to the handlers unmodified, but subclasses can override
  // specific methods to modify the event stream or the configuration
  // requests as they pass through.</p>
  //
  TBufferedXMLFilterImpl = class(TInterfacedObject, IBufferedXMLFilter,
    IBufferedXMLReader, IEntityResolver, IBufferedDTDHandler,
    IBufferedContentHandler, IErrorHandler)
  private
    Fparent : IBufferedXMLReader;
    Flocator : ILocator;
    FentityResolver : IEntityResolver;
    FdtdHandler : IBufferedDTDHandler;
    FcontentHandler : IBufferedContentHandler;
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
    // <p>This is the <a href="../BSAX/IBufferedXMLReader.html">IBufferedXMLReader</a> from which
    // this filter will obtain its events and to which it will pass its
    // configuration requests.  The parent may itself be another filter.</p>
    //
    // <p>If there is no parent reader set, any attempt to parse
    // or to set or get a feature or property will fail.</p>

    // @param parent The parent reader.
    // @see <a href="../BSAXHelpers/TBufferedXMLFilterImpl.html#getParent">TBufferedXMLFilterImpl.getParent</a>
    procedure setParent(const parent : IBufferedXMLReader); virtual;

    // Get the parent reader.
    //
    // <p>This method allows the application to query the parent
    // reader (which may be another filter).  It is generally a
    // bad idea to perform any operations on the parent reader
    // directly: they should all pass through this filter.</p>
    //
    // @return The parent filter, or nil if none has been set.
    // @see <a href="../BSAXHelpers/TBufferedXMLFilterImpl.html#setParent">TBufferedXMLFilterImpl.setParent</a>
    function getParent() : IBufferedXMLReader; virtual;

    // Look up the value of a feature.
    //
    // <p>This will always fail if the parent is nil.</p>
    //
    // @param name The feature name, which is a fully-qualified URI.
    // @return The current value of the feature (true or false).
    // @exception ESAXNotRecognizedException If the feature
    //            value can't be assigned or retrieved.
    // @exception ESAXNotSupportedException When the
    //            XMLReader recognizes the feature name but
    //            cannot determine its value at this time.
    function getFeature(const name : SAXString) : Boolean; virtual;

    // Set the value of a feature.
    //
    // <p>This will always fail if the parent is nil.</p>
    //
    // @param name The feature name, which is a fully-qualified URI.
    // @param state The requested value of the feature (true or false).
    // @exception ESAXNotRecognizedException If the feature
    //            value can't be assigned or retrieved.
    // @exception ESAXNotSupportedException When the
    //            XMLReader recognizes the feature name but
    //            cannot set the requested value.
    procedure setFeature(const name : SAXString; value : Boolean); virtual;

    // Look up the interface value of a property.
    //
    // @param name The property name, which is a fully-qualified URI.
    // @returns An Interface that allows the getting and setting of the property
    // @exception ESAXNotRecognizedException If the property
    //            interface cannot be retrieved.
    // @exception ESAXNotSupportedException When the
    //            XMLReader recognizes the property name but
    //            cannot determine its interface value at this time.
    function getProperty(const name : SAXString) : IProperty; virtual;

    // Set the entity resolver.
    //
    // @param resolver The entity resolver.
    procedure setEntityResolver(const resolver : IEntityResolver); virtual;

    // Get the current entity resolver.
    //
    // @return The current entity resolver, or nil if none
    //         has been registered.
    function getEntityResolver() : IEntityResolver; virtual;

    // Set the DTD event handler.
    //
    // @param handler The DTD handler.
    procedure setDTDHandler(const handler : IBufferedDTDHandler); virtual;

    // Get the current DTD event handler.
    //
    // @return The current DTD handler, or null if none
    //         has been registered.
    function getDTDHandler() : IBufferedDTDHandler; virtual;

    // Set the content event handler.
    //
    // @param handler The content handler.
    procedure setContentHandler(const handler : IBufferedContentHandler); virtual;

    // Get the content event handler.
    //
    // @return The current content handler, or nil if none
    //         has been registered.
    function getContentHandler() : IBufferedContentHandler; virtual;

    // Set the error event handler.
    //
    // @param handler The error handler.
    procedure setErrorHandler(const handler : IErrorHandler); virtual;

    // Get the current error event handler.
    //
    // @return The current error handler, or nil if none
    //         has been registered.
    function getErrorHandler() : IErrorHandler; virtual;

    // Parse a document.
    //
    // @param source The input source for the top-level of the
    //        XML document.
    // @exception ESAXException Any SAX exception.
    // @exception Exception An IO exception from the parser,
    //            possibly from a byte stream
    //            supplied by the application.
    procedure parse(const input : IInputSource); overload; virtual;

    // Parse a document.
    //
    // @param systemId The system identifier (URI).
    // @exception ESAXException Any SAX exception.
    // @exception Exception An IO exception from the parser,
    //            possibly from a byte stream
    //            supplied by the application.
    procedure parse(const systemId : SAXString); overload; virtual;

    // Filter an external entity resolution.
    //
    // @param publicId The public identifier of the external entity
    //        being referenced, or empty if none was supplied.
    // @param systemId The system identifier of the external entity
    //        being referenced.
    // @return An IInputSource interface describing the new input source,
    //         or null to request that the parser open a regular
    //         URI connection to the system identifier.
    // @exception ESAXException Any SAX exception.
    // @exception Exception A pascal-specific IO exception,
    //            possibly the result of creating a new Stream
    //            for the InputSource.
    function resolveEntity(const publicId, systemId : SAXString) : IInputSource; virtual;

    // Filter a notation declaration event.
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
    procedure notationDecl(name : PSAXChar; nameLength : Integer;
      publicId : PSAXChar; publicIdLength : Integer;
      systemId : PSAXChar; systemIdLength : Integer); virtual;

    // Filter an unparsed entity declaration event.
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
    procedure unparsedEntityDecl(name : PSAXChar;
      nameLength : Integer; publicId : PSAXChar;
      publicIdLength : Integer; systemId : PSAXChar;
      systemIdLength : Integer; notationName : PSAXChar;
      notationNameLength : Integer); virtual;

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
    procedure startPrefixMapping(prefix: PSAXChar;
      prefixLength: Integer; uri: PSAXChar;
      uriLength: Integer); virtual;

    // Filter an end Namespace prefix mapping event.
    //
    // @param prefix The prefix that was being mapping.
    //               Use the empty string when a default mapping scope ends.
    // @param prefixLength The length of the prefix buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure endPrefixMapping(prefix: PSAXChar;
      prefixLength: Integer); virtual;

    // Filter a start element event.
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
    procedure startElement(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer;
      qName : PSAXChar; qNameLength : Integer;
      const atts: IBufferedAttributes); virtual;

    // Filter an end element event.
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
    procedure endElement(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer;
      qName : PSAXChar; qNameLength : Integer); virtual;

    // Filter a character data event.
    //
    // @param ch A pointer to a buffer of characters.
    // @param length The number of characters to use from the buffer.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure characters(ch : PSAXChar; length : Integer); virtual;

    // Filter an ignorable whitespace event.
    //
    // @param ch A pointer to a buffer of characters.
    // @param length The number of characters to use from the array.
    // @exception ESAXException The client may throw
    //            an exception during processing.
    procedure ignorableWhitespace(ch : PSAXChar; length : Integer); virtual;

    // Filter a processing instruction event.
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
      dataLength : Integer); virtual;

    // Filter a skipped entity event.
    //
    // @param name The name of the skipped entity.  If it is a
    //        parameter entity, the name will begin with '%', and if
    //        it is the external DTD subset, it will be the string
    //        "[dtd]".
    // @param nameLength The length of the name buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    procedure skippedEntity(name : PSAXChar; nameLength : Integer); virtual;

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
    // @see <a href="../BSAXHelpers/TBufferedXMLFilterImpl.html#setParent">TBufferedXMLFilterImpl.setParent</a>
    // @see <a href="../BSAXHelpers/TBufferedXMLFilterImpl.html#setFeature">TBufferedXMLFilterImpl.setFeature</a>
    // @see <a href="../BSAXHelpers/TBufferedXMLFilterImpl.html#getProperty">TBufferedXMLFilterImpl.getProperty</a>
    constructor create(); overload;

    // Construct an XML filter with the specified parent.
    //
    // @see <a href="../BSAXHelpers/TBufferedXMLFilterImpl.html#setParent">TBufferedXMLFilterImpl.setParent</a>
    // @see <a href="../BSAXHelpers/TBufferedXMLFilterImpl.html#getParent">TBufferedXMLFilterImpl.getParent</a>
    constructor create(const parent : IBufferedXMLReader); overload;

  end;

  // Buffered Base class for deriving an XML Reader.
  TBufferedXMLReaderImpl = class(TInterfacedObject, IBufferedXMLReader, ILocator)
  private
    FcontentHandler: IBufferedContentHandler;
    FdtdHandler: IBufferedDTDHandler;
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
    // @param propTypeLength The length of the propTypeBuffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param name A string value containing the name of the property that
    //                 cannot be changed while parsing.
    // @param nameLength The length of the nameBuffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    procedure checkNotParsing(propType, name : SAXString);
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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#setFeature">TBufferedXMLReaderImpl.setFeature</a>
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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#getFeature">TBufferedXMLReaderImpl.getFeature</a>
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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#getEntityResolver">TBufferedXMLReaderImpl.getEntityResolver</a>
    procedure setEntityResolver(const resolver : IEntityResolver); virtual;

    // Return the current entity resolver.
    //
    // @return The current entity resolver, or nil if none
    //         has been registered.
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#setEntityResolver">TBufferedXMLReaderImpl.setEntityResolver</a>
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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#getDTDHandler">TBufferedXMLReaderImpl.getDTDHandler</a>
    procedure setDTDHandler(const handler : IBufferedDTDHandler); virtual;

    // Return the current DTD handler.
    //
    // @return The current DTD handler, or nil if none
    //         has been registered.
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#setDTDHandler">TBufferedXMLReaderImpl.setDTDHandler</a>
    function getDTDHandler() : IBufferedDTDHandler; virtual;

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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#getContentHandler">TBufferedXMLReaderImpl.getContentHandler</a>
    procedure setContentHandler(const handler : IBufferedContentHandler); virtual;

    // Return the current content handler.
    //
    // @return The current content handler, or nil if none
    //         has been registered.
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#setContentHandler">TBufferedXMLReaderImpl.setContentHandler</a>
    function getContentHandler() : IBufferedContentHandler; virtual;

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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#getErrorHandler">TBufferedXMLReaderImpl.getErrorHandler</a>
    procedure setErrorHandler(const handler : IErrorHandler); virtual;

    // Return the current error handler.
    //
    // @return The current error handler, or nil if none
    //         has been registered.
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#setErrorHandler">TBufferedXMLReaderImpl.setErrorHandler</a>
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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#parse.SAXString">TBufferedXMLReaderImpl.parse(SAXString)</a>
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#setEntityResolver">TBufferedXMLReaderImpl.setEntityResolver</a>
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#setDTDHandler">TBufferedXMLReaderImpl.setDTDHandler</a>
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#setContentHandler">TBufferedXMLReaderImpl.setContentHandler</a>
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#setErrorHandler">TBufferedXMLReaderImpl.setErrorHandler</a>
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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#parse.TInputSource">TBufferedXMLReaderImpl.parse(TInputSource)</a>
    procedure parse(const systemId : SAXString); overload; virtual;

    // Return the public identifier for the current document event.
    //
    // <p>The return value is the public identifier of the document
    // entity or of the external parsed entity in which the markup
    // triggering the event appears.</p>
    //
    // @return A string containing the public identifier, or
    //         empty if none is available.
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#getSystemId">TBufferedXMLReaderImpl.getSystemId</a>
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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#getPublicId">TBufferedXMLReaderImpl.getPublicId</a>
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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#getColumnNumber">TBufferedXMLReaderImpl.getColumnNumber</a>
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
    // @see <a href="../BSAXHelpers/TBufferedXMLReaderImpl.html#getLineNumber">TBufferedXMLReaderImpl.getLineNumber</a>
    function getColumnNumber() : Integer; virtual;

  public

    // Construct an empty XML Reader, and initializes its values.
    constructor Create;

    // Default destructor for an XML Reader, freeing any items created during
    // intialization
    destructor Destroy; override;

  end;

  // SAX2 extension helper for additional Attributes information,
  // implementing the <a href="../BSAXExt/IBufferedAttributes2.html">IBufferedAttributes2</a> interface.
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
  // or using <a href="../BSAXHelpers/TBufferedAttributes2Impl.html#setSpecified">setSpecified</a>.
  // Similarly, the <em>declared</em> flag for each attribute will
  // always be false, except for defaulted attributes (<em>specified</em>
  // is false), non-CDATA attributes, or when it is set to true using
  // <a href="../BSAXHelpers/TBufferedAttributes2Impl.html#setDeclared">setDeclared</a>.
  // If you change an attribute's type by hand, you may need to modify
  // its <em>declared</em> flag to match.
  // </p>
  //
  // @since SAX 2.0 (extensions 1.1 alpha)
  TBufferedAttributes2Impl = class(TBufferedAttributesImpl,
    IBufferedAttributes2)
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
    // @param qNameLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return true if the attribute was declared in the DTD,
    //          false otherwise.
    // @exception ESAXIllegalArgumentException When the
    //            supplied name does not identify an attribute.
    function isDeclared(qName : PSAXChar;
      qNameLength : Integer) : Boolean; overload;

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
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param localName The attribute's local name.
    // @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return true if the attribute was declared in the DTD,
    //          false otherwise.
    // @exception ESAXIllegalArgumentException When the
    //            supplied names do not identify an attribute.
    function isDeclared(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer) : Boolean; overload;

    // Returns the current value of an attribute's "specified" flag.
    //
    // @param index The attribute index (zero-based).
    // @return true if the value was found in the XML text,
    //    false if the value was provided by DTD defaulting.
    // @exception ESAXIllegalArgumentException When the
    //            supplied index does not identify an attribute.
    function isSpecified(index : Integer) : Boolean; overload;

    // Returns the current value of an attribute's "specified" flag.
    //
    // @param uri The Namespace URI, or the empty string if
    //        the name has no Namespace URI.
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param localName The attribute's local name.
    // @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return true if the value was found in the XML text,
    //    false if the value was provided by DTD defaulting.
    // @exception ESAXIllegalArgumentException When the
    //            supplied names do not identify an attribute.
    function isSpecified(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer) : Boolean; overload;

    // Returns the current value of an attribute's "specified" flag.
    //
    // @param qName The XML 1.0 qualified name.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @return true if the value was found in the XML text,
    //    false if the value was provided by DTD defaulting.
    // @exception ESAXIllegalArgumentException When the
    //            supplied name does not identify an attribute.
    function isSpecified(qName : PSAXChar;
      qNameLength : Integer) : Boolean; overload;

  public
    // Copy an entire Attributes object.  The "specified" flags are
    // assigned as true, unless the object is an Attributes2 object
    // in which case those values are copied.
    //
    // @see <a href="../BSAXHelpers/TBufferedAttributesImpl.html#setAttributes">TBufferedAttributesImpl.setAttributes</a>
    procedure setAttributes(const atts : IBufferedAttributes); override;

    // Add an attribute to the end of the list, setting its
    // "specified" flag to true.  To set that flag's value
    // to false, use <a href="../BSAXHelpers/TBufferedAttributes2Impl.html#setSpecified">setSpecified</a>.
    //
    // <p>Unless the attribute <em>type</em> is CDATA, this attribute
    // is marked as being declared in the DTD.  To set that flag's value
    // to true for CDATA attributes, use <a href="../BSAXHelpers/TBufferedAttributes2Impl.html#setDeclared">setDeclared</a>.</p>
    //
    // @see <a href="../BSAXHelpers/TBufferedAttributesImpl.html#addAttribute">TBufferedAttributesImpl.addAttribute</a>
    // @param uri The Namespace URI, or the empty string if
    //        none is available or Namespace processing is not
    //        being performed.
    // @param uriLength The length of the uri buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param localName The local name, or the empty string if
    //        Namespace processing is not being performed.
    // @param localNameLength The length of the localName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param qName The qualified (prefixed) name, or the empty string
    //        if qualified names are not available.
    // @param qNameLength The length of the qName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param attrType The attribute type as a string.
    // @param attrTypeLength The length of the attrType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param value The attribute value.
    // @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    procedure addAttribute(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer; qName : PSAXChar;
      qNameLength : Integer; attrType : PSAXChar; attrTypeLength : Integer;
      value : PSAXChar; valueLength : Integer); override;

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
    // @see <a href="../BSAXHelpers/TBufferedAttributes2Impl.html#setType">TBufferedAttributes2Impl.setType</a>
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

  // This class extends the SAX2 base buffered handler class to support the
  // SAX2 <a href="../BSAXExt/IBufferedLexicalHandler.html">IBufferedLexicalHandler</a>,
  // <a href="../BSAXExt/IBufferedDeclHandler.html">IBufferedDeclHandler</a>, and
  // <a href="../SAXExt/IEntityResolver2.html">IEntityResolver2</a> extensions.
  // Except for overriding the
  // original SAX1 <a href="../BSAXHelpers/TBufferedDefaultHandler.html#resolveEntity">resolveEntity</a>
  // method the added handler methods just return.  Subclassers may
  // override everything on a method-by-method basis.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p> <em>Note:</em> this class might yet learn that the
  // <em>IBufferedContentHandler.setDocumentLocator()</em> call might be passed a
  // <a href="../BSAXExt/IBufferedLocator2.html">IBufferedLocator2</a> object, and that the
  // <em>IBufferedContentHandler.startElement()</em> call might be passed a
  // <a href="../BSAXExt/IBufferedAttributes2.html">IBufferedAttributes2</a> object. </p>
  //
  // <p>For reasons of generality and efficiency, strings that are returned
  // from the interface are declared as pointers (PSAXChar) and lengths.
  // This requires that the model use procedural out parameters rather
  // than functions as in the original interfaces.</p>
  //
  // @since SAX 2.0 (extensions 1.1 alpha)
  TBufferedDefaultHandler2 = class(TBufferedDefaultHandler,
    IBufferedLexicalHandler, IBufferedDeclHandler, IEntityResolver2)
  protected

    // Report an element type declaration.
    //
    // <p>The content model will consist of the string "EMPTY", the
    // string "ANY", or a parenthesised group, optionally followed
    // by an occurrence indicator.  The model will be normalized so
    // that all parameter entities are fully resolved and all whitespace
    // is removed,and will include the enclosing parentheses.  Other
    // normalization (such as removing redundant parentheses or
    // simplifying occurrence indicators) is at the discretion of the
    // parser.</p>
    //
    // @param name The element type name.
    // @param nameLength The length of the name buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param model The content model as a normalized string.
    // @param modelLength The length of the model buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The application may raise an exception.
    ///
    procedure elementDecl(name : PSAXChar; nameLength : Integer;
      model : PSAXChar; modelLength : Integer); virtual;

    // Report an attribute type declaration.
    //
    // <p>Only the effective (first) declaration for an attribute will
    // be reported.  The type will be one of the strings "CDATA",
    // "ID", "IDREF", "IDREFS", "NMTOKEN", "NMTOKENS", "ENTITY",
    // "ENTITIES", a parenthesized token group with
    // the separator "|" and all whitespace removed, or the word
    // "NOTATION" followed by a space followed by a parenthesized
    // token group with all whitespace removed.</p>
    //
    // <p>The value will be the value as reported to applications,
    // appropriately normalized and with entity and character
    // references expanded.</p>
    //
    // @param eName The name of the associated element.
    // @param eNameLength The length of the eName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param aName The name of the attribute.
    // @param aNameLength The length of the aName buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param attrType A string representing the attribute type.
    // @param attrTypeLength The length of the attrType buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param mode A string representing the attribute defaulting mode
    //        ("#IMPLIED", "#REQUIRED", or "#FIXED") or '' if
    //        none of these applies.
    // @param modeLength The length of the mode buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param value A string representing the attribute's default value,
    //        or an empty string if there is none.
    // @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The application may raise an exception.
    procedure attributeDecl(eName : PSAXChar; eNameLength : Integer;
      aName : PSAXChar; aNameLength : Integer;
      attrType : PSAXChar; attrTypeLength : Integer;
      mode : PSAXChar; modeLength : Integer;
      value : PSAXChar; valueLength : Integer); virtual;

    // Report an internal entity declaration.
    //
    // @param name The name of the entity.  If it is a parameter
    //        entity, the name will begin with '%'.
    // @param nameLength The length of the name buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param value The replacement text of the entity.
    // @param valueLength The length of the value buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler2.html#externalEntityDecl">TBufferedDefaultHandler2.externalEntityDecl</a>
    // @see <a href="../BSAX/IBufferedDTDHandler.html#unparsedEntityDecl">IBufferedDTDHandler.unparsedEntityDecl</a>
    procedure internalEntityDecl(name : PSAXChar;
      nameLength : Integer; value : PSAXChar;
      valueLength : Integer); virtual;

    // Report a parsed external entity declaration.
    //
    // @param name The name of the entity.  If it is a parameter
    //        entity, the name will begin with '%'.
    // @param nameLength The length of the name buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param publicId The declared public identifier of the entity, or
    //        the empty string if none was declared.
    // @param publicIdLength The length of the publicId buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param systemId The declared system identifier of the entity.
    // @param systemIdLength The length of the systemId buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler2.html#internalEntityDecl">TBufferedDefaultHandler2.internalEntityDecl</a>
    // @see <a href="../BSAX/IBufferedDTDHandler.html#unparsedEntityDecl">IBufferedDTDHandler.unparsedEntityDecl</a>
    procedure externalEntityDecl(name : PSAXChar;
      nameLength : Integer; publicId : PSAXChar;
      publicIdLength : Integer; systemId : PSAXChar;
      systemIdLength : Integer); virtual;

    // Report the start of DTD declarations, if any.
    //
    // @param name The document type name.
    // @param nameLength The length of the name buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param publicId The declared public identifier for the
    //        external DTD subset, or an empty string if none was declared.
    // @param publicIdLength The length of the publicId buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @param systemId The declared system identifier for the
    //        external DTD subset, or an empty string if none was declared.
    //        (Note that this is not resolved against the document
    //        base URI.)
    // @param systemIdLength The length of the systemId buffer.
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The application may raise an
    //            exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler2.html#endDTD">TBufferedDefaultHandler2.endDTD</a>
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler2.html#startEntity">TBufferedDefaultHandler2.startEntity</a>
    procedure startDTD(name : PSAXChar; nameLength : Integer;
      publicId : PSAXChar; publicIdLength : Integer;
      systemId : PSAXChar; systemIdLength : Integer); virtual;

    // Report the end of DTD declarations.
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler2.html#startDTD">TBufferedDefaultHandler2.startDTD</a>
    procedure endDTD(); virtual;

    // Report the beginning of some internal and external XML entities.
    //
    // @param name The name of the entity.  If it is a parameter
    //        entity, the name will begin with '%', and if it is the
    //        external DTD subset, it will be '[dtd]'.
    // @param nameLength The length of the name buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception SAXException The application may raise an exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler2.html#endEntity">TBufferedDefaultHandler2.endEntity</a>
    // @see <a href="../BSAXExt/IBufferedDeclHandler.html#internalEntityDecl">IBufferedDeclHandler.internalEntityDecl</a>
    // @see <a href="../BSAXExt/IBufferedDeclHandler.html#externalEntityDecl">IBufferedDeclHandler.externalEntityDecl</a>
    ///
    procedure startEntity(name : PSAXChar; nameLength : Integer); virtual;

    // Report the end of an entity.
    //
    // @param name The name of the entity that is ending.
    // @param nameLength The length of the name buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler2.html#startEntity">TBufferedDefaultHandler2.startEntity</a>
    procedure endEntity(name : PSAXChar; nameLength : Integer); virtual;

    // Report the start of a CDATA section.
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler2.html#endCDATA">TBufferedDefaultHandler2.endCDATA</a>
    procedure startCDATA(); virtual;

    // Report the end of a CDATA section.
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../BSAXHelpers/TBufferedDefaultHandler2.html#startCDATA">TBufferedDefaultHandler2.startCDATA</a>
    procedure endCDATA(); virtual;

    // Report an XML comment anywhere in the document.
    //
    // @param ch An array holding the characters in the comment.
    // @param chLength The length of the ch buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The application may raise an exception.
    procedure comment(ch : PSAXChar; chLength : Integer); virtual;

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
    function getExternalSubset(const name, baseURI : SAXString) : IInputSource; virtual;

    // Tells the parser to resolve the systemId against the baseURI
    // and read the entity text from that resulting absolute URI.
    // Note that because the older
    // <a href="../BSAXHelpers/TBufferedDefaultHandler.html#resolveEntity">TBufferedDefaultHandler.resolveEntity</a>
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
    function resolveEntity(const publicId, systemId : SAXString) : IInputSource; reintroduce; overload; virtual;
  end;

const

  DEFAULT_ATTRIBUTE_COUNT = 5;

var
  BufferedHandlerDefault: TBufferedDefaultHandler;

implementation

resourcestring
  sBadIndex             = 'Attempt to modify attribute at illegal index: %d';
  sCantChange           = 'Cannot change %s %s while parsing';
  sFeatureName          = 'Feature: %s';
  sFeatureCheck         = 'feature';
  sNoParent             = 'No parent for filter';
  sPropertyName         = 'Property: %s';
  sNoAttributeAtIndex   = 'No attribute at index: %d';
  sNoAttributeLocalUri  = 'No such attribute: local= %s, namespace= %s';
  sNoAttributeQName     = 'No such attribute: %s';

{ TBufferedAttributesImpl }

constructor TBufferedAttributesImpl.Create();
begin
  inherited Create();
  Flength:= 0;
  Fdata:= nil;
end;

constructor TBufferedAttributesImpl.Create(const atts : IBufferedAttributes);
begin
  inherited Create();
  Flength:= 0;
  Fdata:= nil;
  setAttributes(atts);
end;

destructor TBufferedAttributesImpl.Destroy();
begin
  inherited Destroy();
end;

function TBufferedAttributesImpl.getLength() : Integer;
begin
  Result:= Flength;
end;

procedure TBufferedAttributesImpl.getURI(index : Integer; out uri: PSAXChar;
  out uriLength: Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    uri:= Fdata[index].uri;
    uriLength:= Fdata[index].uriLength;
  end else begin
    uri:= nil;
    uriLength:= 0;
  end;
end;

procedure TBufferedAttributesImpl.getLocalName(index : Integer;
  out localName: PSAXChar; out localNameLength: Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    localName:= Fdata[index].localName;
    localNameLength:= FData[index].localNameLength;
  end else begin
    localName:= nil;
    localNameLength:= 0;
  end;
end;

procedure TBufferedAttributesImpl.getQName(index : Integer; out qName: PSAXChar;
  out qNameLength: Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    qName:= Fdata[index].qName;
    qNameLength:= Fdata[index].qNameLength;
  end else begin
    qName:= nil;
    qNameLength:= 0;
  end;
end;

procedure TBufferedAttributesImpl.getType(index : Integer;
  out attrType: PSAXChar; out attrTypeLength: Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    attrType:= Fdata[index].attrType;
    attrTypeLength:= Fdata[index].attrTypeLength;
  end else begin
    attrType:= nil;
    attrTypeLength:= 0;
  end;
end;

procedure TBufferedAttributesImpl.getType(uri: PSAXChar; uriLength: Integer;
  localName: PSAXChar;  localNameLength: Integer;
  out attrType: PSAXChar; out attrTypeLength: Integer);
var I : Integer;
begin
  I:= 0;
  while I < Flength do
  begin
    if ((SAXStrEqual(Fdata[I].uri, Fdata[I].uriLength, uri, uriLength)) and
        (SAXStrEqual(Fdata[I].localName, Fdata[I].localNameLength, localName, localNameLength))) then
    begin
      attrType:= Fdata[I].attrType;
      attrTypeLength:= Fdata[I].attrTypeLength;
      Exit;
    end;
    Inc(I);
  end;
  attrType:= nil;
  attrTypeLength:= 0;
end;

procedure TBufferedAttributesImpl.getType(qName: PSAXChar;
  qNameLength: Integer; out attrType: PSAXChar; out attrTypeLength: Integer);
var I : Integer;
begin
  I:= 0;
  while I < Flength do
  begin
    if (SAXStrEqual(Fdata[I].qName, Fdata[I].qNameLength, qName, qNameLength)) then
    begin
      attrType:= Fdata[I].attrType;
      attrTypeLength:= Fdata[I].attrTypeLength;
      Exit;
    end;
    Inc(I);
  end;
  attrType:= nil;
  attrTypeLength:= 0;
end;

procedure TBufferedAttributesImpl.getValue(index : Integer;
   out value: PSAXChar; out valueLength: Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    value:= Fdata[index].value;
    valueLength:= Fdata[index].valueLength;
  end else begin
    value:= nil;
    valueLength:= 0;
  end;
end;

procedure TBufferedAttributesImpl.getValue(uri : PSAXChar; uriLength : Integer;
   localName : PSAXChar; localNameLength : Integer;
   out value: PSAXChar; out valueLength: Integer);
var I : Integer;
begin
  I:= 0;
  while I < Flength do
  begin
    if ((SAXStrEqual(Fdata[I].uri, Fdata[I].uriLength, uri, uriLength)) and
        (SAXStrEqual(Fdata[I].localName, Fdata[I].localNameLength, localName, localNameLength))) then
    begin
      value:= Fdata[I].value;
      valueLength:= Fdata[I].valueLength;
      Exit;
    end;
    Inc(I);
  end;
  value:= nil;
  valueLength:= 0;
end;

procedure TBufferedAttributesImpl.getValue(qName : PSAXChar;
  qNameLength : Integer; out value: PSAXChar; out valueLength: Integer);
var I : Integer;
begin
  I:= 0;
  while I < Flength do
  begin
    if (SAXStrEqual(Fdata[I].qName, Fdata[I].qNameLength, qName, qNameLength)) then
    begin
      value:= Fdata[I].value;
      valueLength:= Fdata[I].valueLength;
      Exit;
    end;
    Inc(I);
  end;
  value:= nil;
  valueLength:= 0;
end;

function TBufferedAttributesImpl.getIndex(uri : PSAXChar; uriLength : Integer;
   localName : PSAXChar; localNameLength : Integer) : Integer;
var I : Integer;
begin
  I:= 0;
  while I < Flength do
  begin
    if ((SAXStrEqual(Fdata[I].uri, Fdata[I].uriLength, uri, uriLength)) and
        (SAXStrEqual(Fdata[I].localName, Fdata[I].localNameLength, localName, localNameLength))) then
    begin
      Result:= I;
      Exit;
    end;
    Inc(I);
  end;
  Result:= -1;
end;

function TBufferedAttributesImpl.getIndex(qName : PSAXChar;
  qNameLength : Integer) : Integer;
var I : Integer;
begin
  I:= 0;
  while I < Flength do
  begin
    if (SAXStrEqual(Fdata[I].qName, Fdata[I].qNameLength, qName, qNameLength)) then
    begin
      Result:= I;
      Exit;
    end;
    Inc(I);
  end;
  Result:= -1;
end;

procedure TBufferedAttributesImpl.clear();
begin
  // Everything is keyed off of length--
  // when space is reused it is reset as well
  Flength:= 0;
end;

procedure TBufferedAttributesImpl.setAttributes(
  const atts : IBufferedAttributes);
var I : Integer;
begin
  Flength:= atts.getLength();
  ensureCapacity(Flength);
  for I:= 0 to Flength-1 do
  begin
    with FData[I] do
    begin
      atts.getURI(I, uri, uriLength);
      atts.getLocalName(I, localName, localNameLength);
      atts.getQName(I, qName, qNameLength);
      atts.getType(I, attrType, attrTypeLength);
      atts.getValue(I, value, valueLength);
    end;
  end;
end;

procedure TBufferedAttributesImpl.addAttribute(uri : PSAXChar;
  uriLength : Integer; localName : PSAXChar; localNameLength : Integer;
  qName : PSAXChar; qNameLength : Integer; attrType : PSAXChar; attrTypeLength : Integer;
  value : PSAXChar; valueLength : Integer);
begin
  ensureCapacity(Flength + 1);
  Fdata[Flength].uri:= uri;
  Fdata[Flength].uriLength:= uriLength;
  Fdata[Flength].localName:= localName;
  Fdata[Flength].localNameLength:= localNameLength;
  Fdata[Flength].qName:= qName;
  Fdata[Flength].qNameLength:= qNameLength;
  Fdata[Flength].attrType:= attrType;
  Fdata[Flength].attrTypeLength:= attrTypeLength;
  Fdata[Flength].value:= value;
  Fdata[Flength].valueLength:= valueLength;
  Inc(Flength);
end;

procedure TBufferedAttributesImpl.setAttribute(index : Integer; uri : PSAXChar;
  uriLength : Integer; localName : PSAXChar; localNameLength : Integer;
  qName : PSAXChar; qNameLength : Integer; attrType : PSAXChar;
  attrTypeLength : Integer; value : PSAXChar; valueLength : Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    Fdata[index].uri:= uri;
    Fdata[index].uriLength:= uriLength;
    Fdata[index].localName:= localName;
    Fdata[index].localNameLength:= localNameLength;
    Fdata[index].qName:= qName;
    Fdata[index].qNameLength:= qNameLength;
    Fdata[index].attrType:= attrType;
    Fdata[index].attrTypeLength:= attrTypeLength;
    Fdata[index].value:= value;
    Fdata[index].valueLength:= valueLength;
  end else
    badIndex(index);
end;

procedure TBufferedAttributesImpl.removeAttribute(index : Integer);
var Source : Pointer;
    Dest : Pointer;
    FromBytes, ToBytes, TotalBytes, NewLen: Integer;
    FromIndex, ToIndex : Integer;
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    if (index < Flength-1) then
    begin
      // Deletes a block from the array
      FromIndex:= index;
      ToIndex:= index;
      Dest:= Pointer(FData);
      Source:= Pointer(FData);
      FromBytes:= SizeOf(SAXString) * FromIndex;
      ToBytes:= SizeOf(SAXString) * (ToIndex + 1);
      TotalBytes:= SizeOf(TBufferedAttributesRecord) * (Flength - ToIndex);
      NewLen:= Flength - ((ToIndex - FromIndex) + 1);
      Inc(Cardinal(Dest), FromBytes);
      Inc(Cardinal(Source), ToBytes);
      Move(Source^, Dest^, TotalBytes);
      FLength:= NewLen;
    end else
      Dec(Flength);
  end else
    badIndex(index);
end;

procedure TBufferedAttributesImpl.setURI(index : Integer; uri : PSAXChar;
  uriLength : Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    Fdata[index].uri:= uri;
    Fdata[index].uriLength:= uriLength;
  end else
    badIndex(index);
end;

procedure TBufferedAttributesImpl.setLocalName(index : Integer;
  localName : PSAXChar; localNameLength : Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    Fdata[index].localName:= localName;
    Fdata[index].localNameLength:= localNameLength;
  end else
    badIndex(index);
end;

procedure TBufferedAttributesImpl.setQName(index : Integer; qName : PSAXChar;
  qNameLength : Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    Fdata[index].qName:= qName;
    Fdata[index].qNameLength:= qNameLength;
  end else
    badIndex(index);
end;

procedure TBufferedAttributesImpl.setType(index : Integer; attrType : PSAXChar;
  attrTypeLength : Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    Fdata[index].attrType:= attrType;
    Fdata[index].attrTypeLength:= attrTypeLength;
  end else
    badIndex(index);
end;

procedure TBufferedAttributesImpl.setValue(index : Integer; value : PSAXChar;
  valueLength : Integer);
begin
  if ((index >= 0) and (index < Flength)) then
  begin
    Fdata[index].value:= value;
    Fdata[index].valueLength:= valueLength;
  end else
    badIndex(index);
end;

procedure TBufferedAttributesImpl.ensureCapacity(n : Integer);
begin
  if (n <= 0) then
    Exit;

  if (Fdata <> nil) and (Length(Fdata) >= n) then
    Exit;

  SetLength(Fdata, n);
end;

procedure TBufferedAttributesImpl.badIndex(index : Integer);
begin
  raise ESAXException.Create(Format(sBadIndex, [index]));
end;

{ TBufferedDefaultHandler }

function TBufferedDefaultHandler.resolveEntity(const publicId, systemId : SAXString) : IInputSource;
begin
  Result:= nil;
end;

procedure TBufferedDefaultHandler.notationDecl(name : PSAXChar;
  nameLength : Integer; publicId : PSAXChar; publicIdLength : Integer;
  systemId : PSAXChar; systemIdLength : Integer);
begin
  // no op
end;

procedure TBufferedDefaultHandler.unparsedEntityDecl(name : PSAXChar;
  nameLength : Integer; publicId : PSAXChar;
  publicIdLength : Integer; systemId : PSAXChar;
  systemIdLength : Integer; notationName : PSAXChar;
  notationNameLength : Integer);
begin
  // no op
end;

procedure TBufferedDefaultHandler.setDocumentLocator(const locator: ILocator);
begin
  // no op
end;

procedure TBufferedDefaultHandler.startDocument();
begin
  // no op
end;

procedure TBufferedDefaultHandler.endDocument();
begin
  // no op
end;

procedure TBufferedDefaultHandler.startPrefixMapping(prefix: PSAXChar;
  prefixLength: Integer; uri: PSAXChar;
  uriLength: Integer);
begin
  // no op
end;

procedure TBufferedDefaultHandler.endPrefixMapping(prefix: PSAXChar;
  prefixLength: Integer);
begin
  // no op
end;

procedure TBufferedDefaultHandler.startElement(uri : PSAXChar;
  uriLength : Integer; localName : PSAXChar; localNameLength : Integer;
  qName : PSAXChar; qNameLength : Integer;
  const atts: IBufferedAttributes);
begin
  // no op
end;

procedure TBufferedDefaultHandler.endElement(uri : PSAXChar;
  uriLength : Integer; localName : PSAXChar; localNameLength : Integer;
  qName : PSAXChar; qNameLength : Integer);
begin
  // no op
end;

procedure TBufferedDefaultHandler.characters(ch : PSAXChar;
  chLength : Integer);
begin
  // no op
end;

procedure TBufferedDefaultHandler.ignorableWhitespace(ch : PSAXChar;
  chLength : Integer);
begin
  // no op
end;

procedure TBufferedDefaultHandler.processingInstruction(target : PSAXChar;
  targetLength : Integer; data : PSAXChar;
  dataLength : Integer);
begin
  // no op
end;

procedure TBufferedDefaultHandler.skippedEntity(name : PSAXChar;
  nameLength : Integer);
begin
  // no op
end;

procedure TBufferedDefaultHandler.warning(const e : ISAXParseError);
begin
  // no op
end;

procedure TBufferedDefaultHandler.error(const e : ISAXParseError);
begin
  // no op
end;

procedure TBufferedDefaultHandler.fatalError(const e : ISAXParseError);
begin
  // no op
end;


{ TBufferedXMLFilterImpl }

constructor TBufferedXMLFilterImpl.create;
begin
  inherited Create();
  Fparent:= nil;
  Flocator:= nil;
  FentityResolver:= nil;
  FdtdHandler:= nil;
  FcontentHandler:= nil;
  FerrorHandler:= nil;
end;

constructor TBufferedXMLFilterImpl.create(
  const parent: IBufferedXMLReader);
begin
  inherited Create();
  setParent(parent);
  Flocator:= nil;
  FentityResolver:= nil;
  FdtdHandler:= nil;
  FcontentHandler:= nil;
  FerrorHandler:= nil;
end;

procedure TBufferedXMLFilterImpl.setParent(
  const parent: IBufferedXMLReader);
begin
  Fparent:= parent;
end;

function TBufferedXMLFilterImpl.getParent: IBufferedXMLReader;
begin
  Result:= Fparent;
end;

procedure TBufferedXMLFilterImpl.setFeature(const name: SAXString;
  value: Boolean);
begin
  if (Fparent <> nil) then
    Fparent.setFeature(name, value)
  else
    raise ESAXNotRecognizedException.Create(Format(sFeatureName, [Name]));
end;

function TBufferedXMLFilterImpl.getFeature(const name: SAXString): Boolean;
begin
  if (Fparent <> nil) then
    Result:= Fparent.getFeature(name)
  else
    raise ESAXNotRecognizedException.Create(Format(sFeatureName, [Name]));
end;

function TBufferedXMLFilterImpl.getProperty(
  const name: SAXString): IProperty;
begin
  if (Fparent <> nil) then
    Result:= Fparent.getProperty(name)
  else
    raise ESAXNotRecognizedException.Create(Format(sPropertyName, [Name]));
end;

procedure TBufferedXMLFilterImpl.setEntityResolver(const resolver: IEntityResolver);
begin
  FentityResolver:= resolver;
end;

function TBufferedXMLFilterImpl.getEntityResolver: IEntityResolver;
begin
  Result:= FentityResolver;
end;

procedure TBufferedXMLFilterImpl.setDTDHandler(
  const handler: IBufferedDTDHandler);
begin
  FdtdHandler:= handler;
end;

function TBufferedXMLFilterImpl.getDTDHandler: IBufferedDTDHandler;
begin
  Result:= FdtdHandler;
end;

procedure TBufferedXMLFilterImpl.setContentHandler(
  const handler: IBufferedContentHandler);
begin
  FcontentHandler:= handler;
end;

function TBufferedXMLFilterImpl.getContentHandler: IBufferedContentHandler;
begin
  Result:= FcontentHandler;
end;

procedure TBufferedXMLFilterImpl.setErrorHandler(
  const handler: IErrorHandler);
begin
  FerrorHandler:= handler;
end;

function TBufferedXMLFilterImpl.getErrorHandler: IErrorHandler;
begin
  Result:= FerrorHandler;
end;

procedure TBufferedXMLFilterImpl.parse(const input: IInputSource);
begin
  setupParse();
  Fparent.parse(input);
end;

procedure TBufferedXMLFilterImpl.parse(const systemId: SAXString);
var source : IInputSource;
begin
  source:= TInputSource.Create(systemId) as IInputSource;
  try
    parse(source);
  finally
    source:= nil;
  end;
end;

function TBufferedXMLFilterImpl.resolveEntity(const publicId, systemId: SAXString): IInputSource;
begin
  if (FentityResolver <> nil) then
    Result:= FentityResolver.resolveEntity(publicId, systemId)
  else
    Result:= nil;
end;

procedure TBufferedXMLFilterImpl.notationDecl(name: PSAXChar;
  nameLength: Integer; publicId: PSAXChar; publicIdLength: Integer;
  systemId: PSAXChar; systemIdLength: Integer);
begin
  if (FdtdHandler <> nil) then
    FdtdHandler.notationDecl(name, nameLength, publicId, publicIdLength,
      systemId, systemIdLength);
end;

procedure TBufferedXMLFilterImpl.unparsedEntityDecl(name: PSAXChar;
  nameLength: Integer; publicId: PSAXChar; publicIdLength: Integer;
  systemId: PSAXChar; systemIdLength: Integer; notationName: PSAXChar;
  notationNameLength: Integer);
begin
  if (FdtdHandler <> nil) then
    FdtdHandler.unparsedEntityDecl(name, nameLength, publicId, publicIdLength,
      systemId, systemIdLength, notationName, notationNameLength);
end;

procedure TBufferedXMLFilterImpl.setDocumentLocator(
  const locator: ILocator);
begin
  Flocator:= locator;
  if (FcontentHandler <> nil) then
    FcontentHandler.setDocumentLocator(locator);
end;

procedure TBufferedXMLFilterImpl.startDocument;
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.startDocument();
end;

procedure TBufferedXMLFilterImpl.endDocument;
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.endDocument();
end;

procedure TBufferedXMLFilterImpl.startPrefixMapping(prefix: PSAXChar;
  prefixLength: Integer; uri: PSAXChar; uriLength: Integer);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.startPrefixMapping(prefix, prefixLength, uri, uriLength);
end;

procedure TBufferedXMLFilterImpl.endPrefixMapping(prefix: PSAXChar;
  prefixLength: Integer);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.endPrefixMapping(prefix, prefixLength);
end;

procedure TBufferedXMLFilterImpl.startElement(uri: PSAXChar;
  uriLength: Integer; localName: PSAXChar; localNameLength: Integer;
  qName: PSAXChar; qNameLength: Integer; const atts: IBufferedAttributes);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.startElement(uri, uriLength, localName, localNameLength,
      qName, qNameLength, atts);
end;

procedure TBufferedXMLFilterImpl.endElement(uri: PSAXChar;
  uriLength: Integer; localName: PSAXChar; localNameLength: Integer;
  qName: PSAXChar; qNameLength: Integer);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.endElement(uri, uriLength, localName, localNameLength,
      qName, qNameLength);
end;

procedure TBufferedXMLFilterImpl.characters(ch: PSAXChar; length: Integer);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.characters(ch, length);
end;

procedure TBufferedXMLFilterImpl.ignorableWhitespace(ch: PSAXChar;
  length: Integer);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.ignorableWhitespace(ch, length);
end;

procedure TBufferedXMLFilterImpl.processingInstruction(target: PSAXChar;
  targetLength: Integer; data: PSAXChar; dataLength: Integer);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.processingInstruction(target, targetLength, data,
      dataLength);
end;

procedure TBufferedXMLFilterImpl.skippedEntity(name: PSAXChar;
  nameLength: Integer);
begin
  if (FcontentHandler <> nil) then
    FcontentHandler.skippedEntity(name, nameLength);
end;

procedure TBufferedXMLFilterImpl.warning(const e: ISAXParseError);
begin
  if (FerrorHandler <> nil) then
    FerrorHandler.warning(e);
end;

procedure TBufferedXMLFilterImpl.error(const e: ISAXParseError);
begin
  if (FerrorHandler <> nil) then
    FerrorHandler.error(e);
end;

procedure TBufferedXMLFilterImpl.fatalError(const e: ISAXParseError);
begin
  if (FerrorHandler <> nil) then
    FerrorHandler.fatalError(e);
end;

procedure TBufferedXMLFilterImpl.setupParse;
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

{ TBufferedXMLReaderImpl }

constructor TBufferedXMLReaderImpl.Create;
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

destructor TBufferedXMLReaderImpl.Destroy;
begin
  FcontentHandler:= nil;
  FdtdHandler    := nil;
  FentityResolver:= nil;
  FerrorHandler  := nil;
  inherited;
end;

procedure TBufferedXMLReaderImpl.checkNotParsing(propType, name: SAXString);
begin
  if Fparsing then
    raise ESAXNotSupportedException.Create(Format(sCantChange,
      [propType, name]));
end;

function TBufferedXMLReaderImpl.getColumnNumber: Integer;
begin
  Result:= FcolumnNumber;
end;

function TBufferedXMLReaderImpl.getContentHandler: IBufferedContentHandler;
begin
  Result:= FcontentHandler;
end;

function TBufferedXMLReaderImpl.getDTDHandler: IBufferedDTDHandler;
begin
  Result:= FdtdHandler;
end;

function TBufferedXMLReaderImpl.getEntityResolver: IEntityResolver;
begin
  Result:= FentityResolver;
end;

function TBufferedXMLReaderImpl.getErrorHandler: IErrorHandler;
begin
  Result:= FerrorHandler;
end;

function TBufferedXMLReaderImpl.getFeature(const name: SAXString): Boolean;
begin
  if (name = NamespacesFeature) then
    Result:= Fnamespaces
  else if (name = NamespacePrefixesFeature) then
    Result:= Fprefixes
  else if (name = ValidationFeature) or (name = ExternalGeneralFeature) or
      (name = ExternalParameterFeature) then
    raise ESAXNotSupportedException.Create(Format(sFeatureName, [name]))
  else
    raise ESAXNotRecognizedException.Create(Format(sFeatureName, [name]));
end;

function TBufferedXMLReaderImpl.getLineNumber: Integer;
begin
  Result:= FlineNumber;
end;

function TBufferedXMLReaderImpl.getProperty(
  const name: SAXString): IProperty;
begin
  raise ESAXNotRecognizedException.Create(Format(sPropertyName, [name]));
end;

function TBufferedXMLReaderImpl.getPublicId: PSAXChar;
begin
  Result:= PSAXChar(FpublicId);
end;

function TBufferedXMLReaderImpl.getSystemId: PSAXChar;
begin
  Result:= PSAXChar(FsystemId);
end;

procedure TBufferedXMLReaderImpl.parse(const input: IInputSource);
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

procedure TBufferedXMLReaderImpl.parse(const systemId: SAXString);
var
  input: IInputSource;
begin
  input:= getEntityResolver().resolveEntity('', systemId);
  if not Assigned(input) then
    input:= TInputSource.Create(systemId) as IInputSource;
  try
    parse(input);
  finally
    input:= nil;
  end;
end;

procedure TBufferedXMLReaderImpl.setColumnNumber(value: Integer);
begin
  FcolumnNumber:= value;
end;

procedure TBufferedXMLReaderImpl.setContentHandler(
  const handler: IBufferedContentHandler);
begin
  if Assigned(handler) then
    FcontentHandler:= handler
  else
    FcontentHandler:= bufferedHandlerDefault;
end;

procedure TBufferedXMLReaderImpl.setDTDHandler(
  const handler: IBufferedDTDHandler);
begin
  if Assigned(handler) then
    FdtdHandler:= handler
  else
    FdtdHandler:= bufferedHandlerDefault;
end;

procedure TBufferedXMLReaderImpl.setEntityResolver(const resolver: IEntityResolver);
begin
  if Assigned(resolver) then
    FentityResolver:= resolver
  else
    FentityResolver:= bufferedHandlerDefault;
end;

procedure TBufferedXMLReaderImpl.setErrorHandler(
  const handler: IErrorHandler);
begin
  if Assigned(handler) then
    FerrorHandler:= handler
  else
    FerrorHandler:= bufferedHandlerDefault;
end;

procedure TBufferedXMLReaderImpl.setFeature(const name: SAXString;
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

procedure TBufferedXMLReaderImpl.setLineNumber(value: Integer);
begin
  FlineNumber:= value;
end;

procedure TBufferedXMLReaderImpl.setPublicId(value: PSAXChar);
begin
  FpublicId:= value;
end;

procedure TBufferedXMLReaderImpl.setSystemId(value: PSAXChar);
begin
  FsystemId:= value;
end;

{ TBufferedDefaultHandler2 }

procedure TBufferedDefaultHandler2.attributeDecl(eName: PSAXChar;
  eNameLength: Integer; aName: PSAXChar; aNameLength: Integer;
  attrType: PSAXChar; attrTypeLength: Integer; mode: PSAXChar;
  modeLength: Integer; value: PSAXChar; valueLength: Integer);
begin
  // do nothing
end;

procedure TBufferedDefaultHandler2.comment(ch: PSAXChar;
  chLength: Integer);
begin
  // do nothing
end;

procedure TBufferedDefaultHandler2.elementDecl(name: PSAXChar;
  nameLength: Integer; model: PSAXChar; modelLength: Integer);
begin
  // do nothing
end;

procedure TBufferedDefaultHandler2.endCDATA;
begin
  // do nothing
end;

procedure TBufferedDefaultHandler2.endDTD;
begin
  // do nothing
end;

procedure TBufferedDefaultHandler2.endEntity(name: PSAXChar;
  nameLength: Integer);
begin
  // do nothing
end;

procedure TBufferedDefaultHandler2.externalEntityDecl(name: PSAXChar;
  nameLength: Integer; publicId: PSAXChar; publicIdLength: Integer;
  systemId: PSAXChar; systemIdLength: Integer);
begin
  // do nothing
end;

function TBufferedDefaultHandler2.getExternalSubset(const name, baseURI: SAXString): IInputSource;
begin
  Result:= nil;
end;

procedure TBufferedDefaultHandler2.internalEntityDecl(name: PSAXChar;
  nameLength: Integer; value: PSAXChar; valueLength: Integer);
begin
  // do nothing
end;

function TBufferedDefaultHandler2.resolveEntity(const name, publicId, baseURI,
  systemId: SAXString): IInputSource;
begin
  Result:= nil;
end;

function TBufferedDefaultHandler2.resolveEntity(const publicId, systemId : SAXString) : IInputSource;
begin
  Result:= resolveEntity('', publicId, '', systemId);
end;

procedure TBufferedDefaultHandler2.startCDATA;
begin
  // do nothing
end;

procedure TBufferedDefaultHandler2.startDTD(name: PSAXChar;
  nameLength: Integer; publicId: PSAXChar; publicIdLength: Integer;
  systemId: PSAXChar; systemIdLength: Integer);
begin
  // do nothing
end;

procedure TBufferedDefaultHandler2.startEntity(name: PSAXChar;
  nameLength: Integer);
begin
  // do nothing
end;

{ TBufferedAttributes2Impl }

procedure TBufferedAttributes2Impl.addAttribute(uri: PSAXChar;
  uriLength: Integer; localName: PSAXChar; localNameLength: Integer;
  qName: PSAXChar; qNameLength: Integer; attrType: PSAXChar;
  attrTypeLength: Integer; value: PSAXChar; valueLength: Integer);
var len : Integer;
begin
  inherited addAttribute(uri, uriLength, localName, localNameLength,
    qName, qNameLength, attrType, attrTypeLength, value, valueLength);
  len:= getLength();

  if (Length(Fspecified) < len) then
  begin
    SetLength(Fdeclared, len);
    SetLength(Fspecified, len);
  end;

  Fspecified[len - 1]:= true;
  Fdeclared[len - 1]:= not SAXStrEqual(attrType, attrTypeLength, PSAXChar(SAXString('CDATA')), 5);
end;

function TBufferedAttributes2Impl.isDeclared(uri: PSAXChar;
  uriLength: Integer; localName: PSAXChar;
  localNameLength: Integer): Boolean;
var index : Integer;
begin
  index:= getIndex(uri, uriLength, localName, localNameLength);

  if (index < 0) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeLocalUri,
      [PSAXCharToSAXString(localName, localNameLength),
       PSAXCharToSAXString(uri, uriLength)]);
  Result:= Fdeclared[index];
end;

function TBufferedAttributes2Impl.isDeclared(index: Integer): Boolean;
begin
  if ((index < 0) or (index >= getLength())) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeAtIndex, [index]);
  Result:= Fdeclared[index];
end;

function TBufferedAttributes2Impl.isDeclared(qName: PSAXChar;
  qNameLength: Integer): Boolean;
var index : Integer;
begin
  index:= getIndex(qName, qNameLength);

  if (index < 0) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeQName,
      [PSAXCharToSAXString(qName, qNameLength)]);
  Result:= Fdeclared[index];
end;

function TBufferedAttributes2Impl.isSpecified(uri: PSAXChar;
  uriLength: Integer; localName: PSAXChar;
  localNameLength: Integer): Boolean;
var index : Integer;
begin
  index:= getIndex(uri, uriLength, localName, localNameLength);

  if (index < 0) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeLocalUri,
      [PSAXCharToSAXString(localName, localNameLength),
       PSAXCharToSAXString(uri, uriLength)]);
  Result:= Fspecified[index];
end;

function TBufferedAttributes2Impl.isSpecified(index: Integer): Boolean;
begin
  if ((index < 0) or (index >= getLength())) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeAtIndex, [index]);
  Result:= Fspecified[index];
end;

function TBufferedAttributes2Impl.isSpecified(qName: PSAXChar;
  qNameLength: Integer): Boolean;
var index : Integer;
begin
  index:= getIndex(qName, qNameLength);

  if (index < 0) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeQName,
      [PSAXCharToSAXString(qName, qNameLength)]);
  Result:= Fspecified[index];
end;

procedure TBufferedAttributes2Impl.removeAttribute(index: Integer);
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

procedure TBufferedAttributes2Impl.setAttributes(
  const atts: IBufferedAttributes);
var length, I : Integer;
    flags : array of Boolean;
    a2 : IBufferedAttributes2;
begin
  length:= atts.getLength();

  inherited setAttributes(atts);

  SetLength(flags, length);

  if (atts.QueryInterface(IBufferedAttributes2, a2) = 0) then
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

procedure TBufferedAttributes2Impl.setDeclared(index: Integer;
  value: Boolean);
begin
  if ((index < 0) or (index >= getLength())) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeAtIndex, [index]);
  Fdeclared[index]:= value;
end;

procedure TBufferedAttributes2Impl.setSpecified(index: Integer;
  value: Boolean);
begin
  if ((index < 0) or (index >= getLength())) then
    raise ESAXIllegalArgumentException.CreateFmt(sNoAttributeAtIndex, [index]);
  Fspecified[index]:= value;
end;

initialization

  // Create a default handler object
  BufferedHandlerDefault := TBufferedDefaultHandler.Create;
  // Keep it around
  BufferedHandlerDefault._AddRef;

finalization

  // Release the reference
  BufferedHandlerDefault._Release;

end.
