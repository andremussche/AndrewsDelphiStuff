// SAX for Pascal, Simple API for XML Buffered Extension Interfaces in Pascal.
// Ver 1.1 July 4, 2003
// http://xml.defined.net/SAX (this will change!)
// Based on http://www.saxproject.org/
// No warranty; no copyright -- use this as you will.
unit BSAXExt;

interface

uses
  SAX, BSAX;

const

  {$IFDEF SAX_WIDESTRINGS}
  IID_IBufferedDeclHandler = '{97D7C698-3CA7-4352-9B2A-42421B6FCF13}';
  IID_IBufferedLexicalHandler = '{60A76959-D143-4115-99A6-C8D7C1D44FD7}';
  IID_IBufferedAttributes2 = '{DD2C7AF0-F8E8-4667-94C8-2ADB1CB28E61}';
  {$ELSE}
  IID_IBufferedDeclHandler = '{8B1D7EA7-4F60-4ECF-8607-9AD0AB9BB285}';
  IID_IBufferedLexicalHandler = '{BEAC09EF-F04C-4419-99D6-3C5229E2FE7E}';
  IID_IBufferedAttributes2 = '{638BEF5C-7741-48E9-979E-8C6800405E9D}';
  {$ENDIF}

type

  IBufferedDeclHandler = interface;
  IBufferedLexicalHandler = interface;
  IBufferedAttributes2 = interface;

  // SAX2 extension handler for DTD declaration events.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This is an optional extension handler for SAX2 to provide more
  // complete information about DTD declarations in an XML document.
  // XML readers are not required to recognize this handler, and it
  // is not part of core-only SAX2 distributions.</p>
  //
  // <p>Note that data-related DTD declarations (unparsed entities and
  // notations) are already reported through the <a href="../BSAX/IBufferedDTDHandler.html">IBufferedDTDHandler</a>
  // interface.</p>
  //
  // <p>If you are using the declaration handler together with a lexical
  // handler, all of the events will occur between the
  // <a href="../BSAXExt/IBufferedLexicalHandler.html#startDTD">startDTD</a> and the
  // <a href="../BSAXExt/IBufferedLexicalHandler.html#endDTD">endDTD</a> events.</p>
  //
  // <p>To set the DeclHandler for an XML reader, use the
  // <a href="../BSAX/IBufferedXMLReader.html#getProperty">getProperty</a> method
  // with the property name
  // <code>http://xml.org/sax/properties/declaration-handler</code>
  // and an object implementing this interface (or nil) as the value.
  // If the reader does not report declaration events, it will throw a
  // <a href="../SAX/ESAXNotRecognizedException.html">ESAXNotRecognizedException</a>
  // when you attempt to register the handler.</p>
  //
  // <p>For reasons of generality and efficiency, strings that are returned
  // from the interface are declared as pointers (PSAXChar) and lengths.
  // This requires that the model use procedural out parameters rather
  // than functions as in the original interfaces.</p>
  //
  // @since SAX 2.0 (extensions 1.0)
  // @see <a href="../BSAX/IBufferedXMLReader.html">IBufferedXMLReader</a>
  //
  IBufferedDeclHandler = interface(IUnknown)
    [IID_IBufferedDeclHandler]

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
      model : PSAXChar; modelLength : Integer);

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
      value : PSAXChar; valueLength : Integer);

    // Report an internal entity declaration.
    //
    // <p>Only the effective (first) declaration for each entity
    // will be reported.  All parameter entities in the value
    // will be expanded, but general entities will not.</p>
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
    // @see <a href="../BSAXExt/IBufferedDeclHandler.html#externalEntityDecl">IBufferedDeclHandler.externalEntityDecl</a>
    // @see <a href="../BSAX/IBufferedDTDHandler.html#unparsedEntityDecl">IBufferedDTDHandler.unparsedEntityDecl</a>
    procedure internalEntityDecl(name : PSAXChar;
      nameLength : Integer; value : PSAXChar;
      valueLength : Integer);

    // Report a parsed external entity declaration.
    //
    // <p>Only the effective (first) declaration for each entity
    // will be reported.</p>
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
    // @see <a href="../BSAXExt/IBufferedDeclHandler.html#internalEntityDecl">IBufferedDeclHandler.internalEntityDecl</a>
    // @see <a href="../BSAX/IBufferedDTDHandler.html#unparsedEntityDecl">IBufferedDTDHandler.unparsedEntityDecl</a>
    procedure externalEntityDecl(name : PSAXChar;
      nameLength : Integer; publicId : PSAXChar;
      publicIdLength : Integer; systemId : PSAXChar;
      systemIdLength : Integer);

  end;

  // SAX2 extension handler for lexical events.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>This is an optional extension handler for SAX2 to provide
  // lexical information about an XML document, such as comments
  // and CDATA section boundaries.
  // XML readers are not required to recognize this handler, and it
  // is not part of core-only SAX2 distributions.</p>
  //
  // <p>The events in the lexical handler apply to the entire document,
  // not just to the document element, and all lexical handler events
  // must appear between the content handler's startDocument and
  // endDocument events.</p>
  //
  // <p>To set the LexicalHandler for an XML reader, use the
  // <a href="../BSAX/IBufferedXMLReader.html#getProperty">getProperty</a> method
  // with the property name
  // <code>http://xml.org/sax/properties/lexical-handler</code>
  // and an object implementing this interface (or nil) as the value.
  // If the reader does not report lexical events, it will throw a
  // <a href="../SAX/ESAXNotRecognizedException.html">ESAXNotRecognizedException</a>
  // when you attempt to register the handler.</p>
  //
  // <p>For reasons of generality and efficiency, strings that are returned
  // from the interface are declared as pointers (PSAXChar) and lengths.
  // This requires that the model use procedural out parameters rather
  // than functions as in the original interfaces.</p>
  //
  // @since SAX 2.0 (extensions 1.0)
  IBufferedLexicalHandler = interface(IUnknown)
    [IID_IBufferedLexicalHandler]

    // Report the start of DTD declarations, if any.
    //
    // <p>This method is intended to report the beginning of the
    // DOCTYPE declaration; if the document has no DOCTYPE declaration,
    // this method will not be invoked.</p>
    //
    // <p>All declarations reported through
    // <a href="../BSAX/IBufferedDTDHandler.html">IBufferedDTDHandler</a> or
    // <a href="../BSAXExt/IBufferedDeclHandler.html">IBufferedDeclHandler</a> events must appear
    // between the startDTD and <a href="../BSAXExt/IBufferedLexicalHandler.html#endDTD">endDTD</a> events.
    // Declarations are assumed to belong to the internal DTD subset
    // unless they appear between <a href="../BSAXExt/IBufferedLexicalHandler.html#startEntity">startEntity</a>
    // and <a href="../BSAXExt/IBufferedLexicalHandler.html#endEntity">endEntity</a> events.  Comments and
    // processing instructions from the DTD should also be reported
    // between the startDTD and endDTD events, in their original
    // order of (logical) occurrence; they are not required to
    // appear in their correct locations relative to DTDHandler
    // or DeclHandler events, however.</p>
    //
    // <p>Note that the start/endDTD events will appear within
    // the start/endDocument events from IContentHandler and
    // before the first
    // <a href="../BSAX/IBufferedContentHandler.html#startElement">startElement</a>
    // event.</p>
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
    // @see <a href="../BSAXExt/IBufferedLexicalHandler.html#endDTD">IBufferedLexicalHandler.endDTD</a>
    // @see <a href="../BSAXExt/IBufferedLexicalHandler.html#startEntity">IBufferedLexicalHandler.startEntity</a>
    procedure startDTD(name : PSAXChar; nameLength : Integer;
      publicId : PSAXChar; publicIdLength : Integer;
      systemId : PSAXChar; systemIdLength : Integer);

    // Report the end of DTD declarations.
    //
    // <p>This method is intended to report the end of the
    // DOCTYPE declaration; if the document has no DOCTYPE declaration,
    // this method will not be invoked.</p>
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../BSAXExt/IBufferedLexicalHandler.html#startDTD">IBufferedLexicalHandler.startDTD</a>
    procedure endDTD();

    // Report the beginning of some internal and external XML entities.
    //
    // <p>The reporting of parameter entities (including
    // the external DTD subset) is optional, and SAX2 drivers that
    // report IBufferedLexicalHandler events may not implement it; you can use the
    // <code
    // >http://xml.org/sax/features/lexical-handler/parameter-entities</code>
    // feature to query or control the reporting of parameter entities.</p>
    //
    // <p>General entities are reported with their regular names,
    // parameter entities have '%' prepended to their names, and
    // the external DTD subset has the pseudo-entity name "[dtd]".</p>
    //
    // <p>When a SAX2 driver is providing these events, all other
    // events must be properly nested within start/end entity
    // events.  There is no additional requirement that events from
    // <a href="../BSAXExt/IBufferedDeclHandler.html">IBufferedDeclHandler</a> or
    // <a href="../BSAX/IBufferedDTDHandler.html">IBufferedDTDHandler</a> be properly ordered.</p>
    //
    // <p>Note that skipped entities will be reported through the
    // <a href="../BSAX/IBufferedContentHandler.html#skippedEntity">skippedEntity</a>
    // event, which is part of the IContentHandler interface.</p>
    //
    // <p>Because of the streaming event model that SAX uses, some
    // entity boundaries cannot be reported under any
    // circumstances:</p>
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
    //        external DTD subset, it will be '[dtd]'.
    // @param nameLength The length of the name buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception SAXException The application may raise an exception.
    // @see <a href="../BSAXExt/IBufferedLexicalHandler.html#endEntity">IBufferedLexicalHandler.endEntity</a>
    // @see <a href="../BSAXExt/IBufferedDeclHandler.html#internalEntityDecl">IBufferedDeclHandler.internalEntityDecl</a>
    // @see <a href="../BSAXExt/IBufferedDeclHandler.html#externalEntityDecl">IBufferedDeclHandler.externalEntityDecl</a>
    ///
    procedure startEntity(name : PSAXChar; nameLength : Integer);

    // Report the end of an entity.
    //
    // @param name The name of the entity that is ending.
    // @param nameLength The length of the name buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../BSAXExt/IBufferedLexicalHandler.html#startEntity">IBufferedLexicalHandler.startEntity</a>
    procedure endEntity(name : PSAXChar; nameLength : Integer);

    // Report the start of a CDATA section.
    //
    // <p>The contents of the CDATA section will be reported through
    // the regular <a href="../BSAX/IBufferedContentHandler.html#characters">characters</a>
    // event; this event is intended only to report
    // the boundary.</p>
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../BSAXExt/IBufferedLexicalHandler.html#endCDATA">IBufferedLexicalHandler.endCDATA</a>
    procedure startCDATA();

    // Report the end of a CDATA section.
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../BSAXExt/IBufferedLexicalHandler.html#startCDATA">IBufferedLexicalHandler.startCDATA</a>
    procedure endCDATA();

    // Report an XML comment anywhere in the document.
    //
    // <p>This callback will be used for comments inside or outside the
    // document element, including comments in the external DTD
    // subset (if read).  Comments in the DTD must be properly
    // nested inside start/endDTD and start/endEntity events (if
    // used).</p>
    //
    // @param ch An array holding the characters in the comment.
    // @param chLength The length of the ch buffer
    //   The value may be -1 which indicates that the buffer is
    //   null-terminated. If the value is 0 then the buffer may be nil.
    // @exception ESAXException The application may raise an exception.
    procedure comment(ch : PSAXChar; chLength : Integer);
  end;

  // SAX2 extension to augment the per-attribute information
  // provided though <a href="../BSAX/IBufferedAttributes.html">IBufferedAttributes</a>.
  // If an implementation supports this extension, the attributes
  // provided in <a href="../BSAX/IBufferedContentHandler.html#startElement">IBufferedContentHandler.startElement</a>
  // will implement this interface,
  // and the <em>http://xml.org/sax/features/use-attributes2</em>
  // feature flag will have the value <em>true</em>.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p> XMLReader implementations are not required to support this
  // information, and it is not part of core-only SAX2 distributions.</p>
  //
  // <p>Note that if an attribute was defaulted (<em>not isSpecified()</em>)
  // it will of necessity also have been declared (<em>isDeclared()</em>)
  // in the DTD.
  // Similarly if an attribute's type is anything except CDATA, then it
  // must have been declared.
  // </p>
  //
  // @since SAX 2.0 (extensions 1.1 alpha)
  IBufferedAttributes2 = interface(IBufferedAttributes)
    [IID_IBufferedAttributes2]

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

    // Returns true unless the attribute value was provided
    // by DTD defaulting.
    //
    // @param index The attribute index (zero-based).
    // @return true if the value was found in the XML text,
    //    false if the value was provided by DTD defaulting.
    // @exception ESAXIllegalArgumentException When the
    //            supplied index does not identify an attribute.
    function isSpecified(index : Integer) : Boolean; overload;

    // Returns true unless the attribute value was provided
    // by DTD defaulting.
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
    // @return true if the value was found in the XML text,
    //    false if the value was provided by DTD defaulting.
    // @exception ESAXIllegalArgumentException When the
    //            supplied names do not identify an attribute.
    function isSpecified(uri : PSAXChar; uriLength : Integer;
      localName : PSAXChar; localNameLength : Integer) : Boolean; overload;

    // Returns true unless the attribute value was provided
    // by DTD defaulting.
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
  end;

implementation

end.
