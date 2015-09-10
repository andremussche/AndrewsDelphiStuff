// SAX for Pascal, Simple API for XML Extension Interfaces in Pascal.
// Ver 1.1 July 4, 2003
// http://xml.defined.net/SAX (this will change!)
// Based on http://www.saxproject.org/
// No warranty; no copyright -- use this as you will.
unit SAXExt;

interface

uses
  SAX;

const

  // There are separate interface IDs based on Widestrings/Non widestrings

  {$IFDEF SAX_WIDESTRINGS}
  IID_IDeclHandler = '{75534E2D-BB12-4379-8298-486C00141B6A}';
  IID_ILexicalHandler = '{1EE9A4D2-BA2E-47C8-81CB-1C81704D9F20}';
  IID_IAttributes2 = '{7E9F5766-FE9E-46FB-AC3D-7EC2BEFAF35F}';
  IID_IEntityResolver2 = '{A0EFA85C-AD2C-4109-BEBA-C88B47ADA6AF}';
  IID_ILocator2 = '{2CEE7326-1647-4EAA-8670-9C21717B3779}';
  {$ELSE}
  IID_IDeclHandler = '{FC783826-A869-4CCC-A9F9-FEFB2EDB9B4C}';
  IID_ILexicalHandler = '{DAE1DDCC-A591-473E-8500-75626184C69D}';
  IID_IAttributes2 = '{F801E492-56B4-4E76-A201-B7E022A7A5B5}';
  IID_IEntityResolver2 = '{7CCC2B11-376A-4748-BB5A-C2EBE092F1FD}';
  IID_ILocator2 = '{BAED1685-030E-4CCB-88E4-FC2537516464}';
  {$ENDIF}

type

  IDeclHandler = interface;
  ILexicalHandler = interface;
  IAttributes2 = interface;
  IEntityResolver2 = interface;
  ILocator2 = interface;

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
  // notations) are already reported through the <a href="../SAX/IDTDHandler.html">IDTDHandler</a>
  // interface.</p>
  //
  // <p>If you are using the declaration handler together with a lexical
  // handler, all of the events will occur between the
  // <a href="../SAXExt/ILexicalHandler.html#startDTD">startDTD</a> and the
  // <a href="../SAXExt/ILexicalHandler.html#endDTD">endDTD</a> events.</p>
  //
  // <p>To set the DeclHandler for an XML reader, use the
  // <a href="../SAX/IXMLReader.html#getProperty">getProperty</a> method
  // with the property name
  // <code>http://xml.org/sax/properties/declaration-handler</code>
  // and an object implementing this interface (or nil) as the value.
  // If the reader does not report declaration events, it will throw a
  // <a href="../SAX/ESAXNotRecognizedException.html">ESAXNotRecognizedException</a>
  // when you attempt to register the handler.</p>
  //
  // @since SAX 2.0 (extensions 1.0)
  // @see <a href="../SAX/IXMLReader.html">IXMLReader</a>
  //
  IDeclHandler = interface(IUnknown)
    [IID_IDeclHandler]

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
    // @param model The content model as a normalized string.
    // @exception ESAXException The application may raise an exception.
    ///
    procedure elementDecl(const name, model : SAXString);

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
    // @param aName The name of the attribute.
    // @param attrType A string representing the attribute type.
    // @@param mode A string representing the attribute defaulting mode
    //        ("#IMPLIED", "#REQUIRED", or "#FIXED") or '' if
    //        none of these applies.
    // @param value A string representing the attribute's default value,
    //        or '' if there is none.
    // @exception ESAXException The application may raise an exception.
    procedure attributeDecl(const eName, aName, attrType, mode,
      value: SAXString);

    // Report an internal entity declaration.
    //
    // <p>Only the effective (first) declaration for each entity
    // will be reported.  All parameter entities in the value
    // will be expanded, but general entities will not.</p>
    //
    // @param name The name of the entity.  If it is a parameter
    //        entity, the name will begin with '%'.
    // @param value The replacement text of the entity.
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXExt/IDeclHandler.html#externalEntityDecl">IDeclHandler.externalEntityDecl</a>
    // @see <a href="../SAX/IDTDHandler.html#unparsedEntityDecl">IDTDHandler.unparsedEntityDecl</a>
    procedure internalEntityDecl(const name, value : SAXString);

    // Report a parsed external entity declaration.
    //
    // <p>Only the effective (first) declaration for each entity
    // will be reported.</p>
    //
    // @param name The name of the entity.  If it is a parameter
    //        entity, the name will begin with '%'.
    // @param publicId The declared public identifier of the entity, or
    //        an empty string if none was declared.
    // @param systemId The declared system identifier of the entity.
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXExt/IDeclHandler.html#internalEntityDecl">IDeclHandler.internalEntityDecl</a>
    // @see <a href="../SAX/IDTDHandler.html#unparsedEntityDecl">IDTDHandler.unparsedEntityDecl</a>
    procedure externalEntityDecl(const name, publicId, systemId : SAXString);

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
  // <a href="../SAX/IXMLReader.html#getProperty">getProperty</a> method
  // with the property name
  // <code>http://xml.org/sax/properties/lexical-handler</code>
  // and an object implementing this interface (or nil) as the value.
  // If the reader does not report lexical events, it will throw a
  // <a href="../SAX/ESAXNotRecognizedException.html">ESAXNotRecognizedException</a>
  // when you attempt to register the handler.</p>
  //
  // @since SAX 2.0 (extensions 1.0)
  ILexicalHandler = interface(IUnknown)
    [IID_ILexicalHandler]

    // Report the start of DTD declarations, if any.
    //
    // <p>This method is intended to report the beginning of the
    // DOCTYPE declaration; if the document has no DOCTYPE declaration,
    // this method will not be invoked.</p>
    //
    // <p>All declarations reported through
    // <a href="../SAX/IDTDHandler.html">IDTDHandler</a> or
    // <a href="../SAXExt/IDeclHandler.html">IDeclHandler</a> events must appear
    // between the startDTD and <a href="../SAXExt/ILexicalHandler.html#endDTD">endDTD</a> events.
    // Declarations are assumed to belong to the internal DTD subset
    // unless they appear between <a href="../SAXExt/ILexicalHandler.html#startEntity">startEntity</a>
    // and <a href="../SAXExt/ILexicalHandler.html#endEntity">endEntity</a> events.  Comments and
    // processing instructions from the DTD should also be reported
    // between the startDTD and endDTD events, in their original
    // order of (logical) occurrence; they are not required to
    // appear in their correct locations relative to DTDHandler
    // or DeclHandler events, however.</p>
    //
    // <p>Note that the start/endDTD events will appear within
    // the start/endDocument events from IContentHandler and
    // before the first
    // <a href="../SAX/IContentHandler.html#startElement">startElement</a>
    // event.</p>
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
    // @see <a href="../SAXExt/ILexicalHandler.html#endDTD">ILexicalHandler.endDTD</a>
    // @see <a href="../SAXExt/ILexicalHandler.html#startEntity">ILexicalHandler.startEntity</a>
    procedure startDTD(const name, publicId, systemId : SAXString);

    // Report the end of DTD declarations.
    //
    // <p>This method is intended to report the end of the
    // DOCTYPE declaration; if the document has no DOCTYPE declaration,
    // this method will not be invoked.</p>
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXExt/ILexicalHandler.html#startDTD">ILexicalHandler.startDTD</a>
    procedure endDTD();

    // Report the beginning of some internal and external XML entities.
    //
    // <p>The reporting of parameter entities (including
    // the external DTD subset) is optional, and SAX2 drivers that
    // report ILexicalHandler events may not implement it; you can use the
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
    // <a href="../SAXExt/IDeclHandler.html">IDeclHandler</a> or
    // <a href="../SAX/IDTDHandler.html">IDTDHandler</a> be properly ordered.</p>
    //
    // <p>Note that skipped entities will be reported through the
    // <a href="../SAX/IContentHandler.html#skippedEntity">skippedEntity</a>
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
    //        external DTD subset, it will be "[dtd]".
    // @exception SAXException The application may raise an exception.
    // @see <a href="../SAXExt/ILexicalHandler.html#endEntity">ILexicalHandler.endEntity</a>
    // @see <a href="../SAXExt/IDeclHandler.html#internalEntityDecl">IDeclHandler.internalEntityDecl</a>
    // @see <a href="../SAXExt/IDeclHandler.html#externalEntityDecl">IDeclHandler.externalEntityDecl</a>
    ///
    procedure startEntity(const name : SAXString);

    // Report the end of an entity.
    //
    // @param name The name of the entity that is ending.
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXExt/ILexicalHandler.html#startEntity">ILexicalHandler.startEntity</a>
    procedure endEntity(const name : SAXString);

    // Report the start of a CDATA section.
    //
    // <p>The contents of the CDATA section will be reported through
    // the regular <a href="../SAX/IContentHandler.html#characters">characters</a>
    // event; this event is intended only to report
    // the boundary.</p>
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXExt/ILexicalHandler.html#endCDATA">ILexicalHandler.endCDATA</a>
    procedure startCDATA();

    // Report the end of a CDATA section.
    //
    // @exception ESAXException The application may raise an exception.
    // @see <a href="../SAXExt/ILexicalHandler.html#startCDATA">ILexicalHandler.startCDATA</a>
    procedure endCDATA();

    // Report an XML comment anywhere in the document.
    //
    // <p>This callback will be used for comments inside or outside the
    // document element, including comments in the external DTD
    // subset (if read).  Comments in the DTD must be properly
    // nested inside start/endDTD and start/endEntity events (if
    // used).</p>
    //
    // @param ch The characters in the comment.
    // @exception ESAXException The application may raise an exception.
    procedure comment(const ch : SAXString);
  end;

  // SAX2 extension to augment the per-attribute information
  // provided though <a href="../SAX/IAttributes.html">IAttributes</a>.
  // If an implementation supports this extension, the attributes
  // provided in <a href="../SAX/IContentHandler.html#startElement">IContentHandler.startElement</a>
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
  IAttributes2 = interface(IAttributes)
    [IID_IAttributes2]

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
    // @param localName The attribute's local name.
    // @return true if the value was found in the XML text,
    //    false if the value was provided by DTD defaulting.
    // @exception ESAXIllegalArgumentException When the
    //            supplied names do not identify an attribute.
    function isSpecified(const uri, localName : SAXString) : Boolean; overload;

    // Returns true unless the attribute value was provided
    // by DTD defaulting.
    //
    // @param qName The XML 1.0 qualified name.
    // @return true if the value was found in the XML text,
    //    false if the value was provided by DTD defaulting.
    // @exception ESAXIllegalArgumentException When the
    //            supplied name does not identify an attribute.
    function isSpecified(const qName : SAXString) : Boolean; overload;
  end;

  // Extended interface for mapping external entity references to input
  // sources, or providing a missing external subset.  The
  // <a href="../SAX/IXMLReader.html#setEntityResolver">IXMLReader.setEntityResolver</a> method
  // is used to provide implementations of this interface to parsers.
  // When a parser uses the methods in this interface, the
  // <a href="../SAXExt/IEntityResolver2.html#resolveEntity">IEntityResolver2.resolveEntity</a>
  // method (in this interface) is used <em>instead of</em> the older (SAX 1.0)
  // <a href="../SAX/IEntityResolver.html#resolveEntity">IEntityResolver.resolveEntity</a> method.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p>If a SAX application requires the customized handling which this
  // interface defines for external entities, it must ensure that it uses
  // an IXMLReader with the
  // <em>http://xml.org/sax/features/use-entity-resolver2</em> feature flag
  // set to <em>true</em> (which is its default value when the feature is
  // recognized).  If that flag is unrecognized, or its value is false,
  // or the resolver does not implement this interface, then only the
  // <a href="../SAX/IEntityResolver.html">IEntityResolver</a> method will be used.
  // </p>
  //
  // <p>That supports three categories of application that modify entity
  // resolution.  <em>Old Style</em> applications won't know about this interface;
  // they will provide an IEntityResolver.
  // <em>Transitional Mode</em> provide an IEntityResolver2 and automatically
  // get the benefit of its methods in any systems (parsers or other tools)
  // supporting it, due to polymorphism.
  // Both <em>Old Style</em> and <em>Transitional Mode</em> applications will
  // work with any SAX2 parser.
  // <em>New style</em> applications will fail to run except on SAX2 parsers
  // that support this particular feature.
  // They will insist that feature flag have a value of "true", and the
  // IEntityResolver2 implementation they provide  might throw an exception
  // if the original SAX 1.0 style entity resolution method is invoked.
  // </p>
  //
  // @see <a href="../SAX/IXMLReader.html#setEntityResolver">IXMLReader.setEntityResolver</a>
  //
  // @since SAX 2.0 (extensions 1.1 alpha)
  IEntityResolver2 = interface(IEntityResolver)
    [IID_IEntityResolver2]

    // <p>Allows applications to provide an external subset for documents
    // that don't explicitly define one.  Documents with DOCTYPE declarations
    // that omit an external subset can thus augment the declarations
    // available for validation, entity processing, and attribute processing
    // (normalization, defaulting, and reporting types including ID).
    // This augmentation is reported
    // through the <a href="../SAXExt/ILexicalHandler.html#startDTD">startDTD</a> method as if
    // the document text had originally included the external subset;
    // this callback is made before any internal subset data or errors
    // are reported.</p>
    //
    // <p>This method can also be used with documents that have no DOCTYPE
    // declaration.  When the root element is encountered,
    // but no DOCTYPE declaration has been seen, this method is
    // invoked.  If it returns a value for the external subset, that root
    // element is declared to be the root element, giving the effect of
    // splicing a DOCTYPE declaration at the end the prolog of a document
    // that could not otherwise be valid.  The sequence of parser callbacks
    // in that case logically resembles this:</p>
    //
    // <pre>
    // ... comments and PIs from the prolog (as usual)
    // startDTD('rootName', source.getPublicId(), source.getSystemId());
    // startEntity ('[dtd]');
    // ... declarations, comments, and PIs from the external subset
    // endEntity ('[dtd]');
    // endDTD();
    // ... then the rest of the document (as usual)
    // startElement (..., 'rootName', ...);
    // </pre>
    //
    // <p>Note that the InputSource gets no further resolution.
    // Implementations of this method may wish to invoke
    // <a href="../SAXExt/IEntityResolver2.html#resolveEntity">resolveEntity</a> to gain benefits such as use
    // of local caches of DTD entities.  Also, this method will never be
    // used by a (non-validating) processor that is not including external
    // parameter entities. </p>
    //
    // <p>Uses for this method include facilitating data validation when
    // interoperating with XML processors that would always require
    // undesirable network accesses for external entities, or which for
    // other reasons adopt a "no DTDs" policy.
    // Non-validation motives include forcing documents to include DTDs so
    // that attributes are handled consistently.
    // For example, an XPath processor needs to know which attibutes have
    // type "ID" before it can process a widely used type of reference.</p>
    //
    // <p><strong>Warning:</strong> Returning an external subset modifies
    // the input document.  By providing definitions for general entities,
    // it can make a malformed document appear to be well formed.
    // </p>
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
    function getExternalSubset(const name, baseURI : SAXString) : IInputSource;

    // <p>TEST !!! Allows applications to map references to external entities into input
    // sources, or tell the parser it should use conventional URI resolution.
    // This method is only called for external entities which have been
    // properly declared.
    // This method provides more flexibility than the <a href="../SAX/IEntityResolver.html">IEntityResolver</a>
    // interface, supporting implementations of more complex catalogue
    // schemes such as the one defined by the
    // <a href="http://www.oasis-open.org/committees/entity/spec-2001-08-06.html">
    // OASIS XML Catalogs</a> specification.</p>
    //
    // <p>Parsers configured to use this resolver method will call it
    // to determine the input source to use for any external entity
    // being included because of a reference in the XML text.
    // That excludes the document entity, and any external entity returned
    // by <a href="../SAXExt/IEntityResolver2.html#getExternalSubset">getExternalSubset</a>.
    // When a (non-validating) processor is configured not to include
    // a class of entities (parameter or general) through use of feature
    // flags, this method is not invoked for such entities.</p>
    //
    // <p>Note that the entity naming scheme used here is the same one
    // used in the <a href="../SAXExt/ILexicalHandler.html">ILexicalHandler</a>, or in the
    // <a href="../SAX/IContentHandler.html#skippedEntity">IContentHandler.skippedEntity</a>
    // method. </p>
    //
    // @param name Identifies the external entity being resolved.
    //  Either '[dtd]' for the external subset, or a name starting
    //  with '%' to indicate a parameter entity, or else the name of
    //  a general entity.  This is never empty when invoked by a SAX2
    //  parser.
    //
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
    //  This is never empty when invoked by a SAX2 parser; only declared
    //  entities, and any external subset, are resolved by such parsers
    //
    // @return An IInputSource object describing the new input source to
    //  be used by the parser.  Returning nil directs the parser to
    //  resolve the system ID against the base URI and open a connection
    //  to resulting URI.
    //
    // @exception ESAXException Any SAX exception.
    function resolveEntity(const name, publicId, baseURI,
      systemId : SAXString) : IInputSource;
  end;

  // SAX2 extension to augment the entity information provided
  // though a <a href="../SAX/ILocator.html">ILocator</a>.
  // If an implementation supports this extension, the Locator
  // provided in <a href="../SAX/IContentHandler.html#setDocumentLocator">IContentHandler.setDocumentLocator</a>
  // will implement this interface, and the
  // <em>http://xml.org/sax/features/use-locator2</em> feature
  // flag will have the value <em>true</em>.
  //
  // <blockquote>
  // <em>This module, both source code and documentation, is in the
  // Public Domain, and comes with <strong>NO WARRANTY</strong>.</em>
  // </blockquote>
  //
  // <p> XMLReader implementations are not required to support this
  // information, and it is not part of core-only SAX2 distributions.</p>
  //
  // @since SAX 2.0 (extensions 1.1 alpha)
  ILocator2 = interface(ILocator)
    [IID_ILocator2]

    // Returns the version of XML used for the entity.  This will
    // normally be the identifier from the current entity's
    // <em>&lt;?xml&#160;version='...'&#160;...?&gt;</em> declaration,
    // or be defaulted by the parser.
    //
    // <p> At this writing, only one version ("1.0") is defined, but it
    // seems likely that a new version will be defined which has slightly
    // different rules about which characters are legal in XML names.</p>
    //
    // @return Identifier for the XML version being used to interpret
    //	the entity's text.
    function getXMLVersion() : PSAXChar;

    // Returns the name of the character encoding for the entity.
    // If the encoding was declared externally (for example, in a MIME
    // Content-Type header), that will be the name returned.  Else if there
    // was an <em>&lt;?xml&#160;...encoding='...'?&gt;</em> declaration at
    // the start of the document, that encoding name will be returned.
    // Otherwise the encoding will been inferred (normally to be UTF-8, or
    // some UTF-16 variant), and that inferred name will be returned.
    //
    // <p>When an <a href="../SAX/IInputSource.html">IInputSource</a> is used
    // to provide an entity's character stream, this method returns the
    // encoding provided in that input stream.</p>
    //
    // <p> Note that some recent W3C specifications require that text
    // in some encodings be normalized, using Unicode Normalization
    // Form C, before processing.  Such normalization must be performed
    // by applications, and would normally be triggered based on the
    // value returned by this method.</p>
    //
    // <p> Encoding names may be those used by the underlying JVM,
    // and comparisons should be case-insensitive.</p>
    //
    // @return Name of the character encoding being used to interpret
    //	the entity's text, or null if this was not provided for a 
    //  character stream passed through an InputSource.
    function getEncoding() : PSAXChar;

  end;

implementation

end.
