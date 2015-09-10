// Filename : XmlChars.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit XmlChars;

interface

uses SAX;

  // !! Eventually I may need to add a version attribute
  // !! for alternate XML characters maps

  function isExtender(const c : SAXChar) : Boolean;
  function isDigit(const c : SAXChar) : Boolean;
  function isCombiningChar(const c : SAXChar) : Boolean;
  function isIdeographic(const c : SAXChar) : Boolean;
  function isBaseChar(const c : SAXChar) : Boolean;
  function isPubidChar(const c : SAXChar) : Boolean;
  function isWhitespace(const c : SAXChar) : Boolean;
  function isXMLChar(const c : SAXChar) : Boolean;
  function isNameChar(const c : SAXChar) : Boolean;
  function isNameStartChar(const c : SAXChar) : Boolean;
  function isLetter(const c : SAXChar) : Boolean;


implementation

  // Appendix B - Unicode Constructs

  // 84    Letter    ::=    BaseChar , Ideographic
  function isLetter(const c : SAXChar) : Boolean;
  begin
    Result:= (isBaseChar(c)) or (isIdeographic(c));
  end;

  // 85    BaseChar
  function isBaseChar(const c : SAXChar) : Boolean;
  begin
    case (Word(c)) of
      $0041..$005A , $0061..$007A , $00C0..$00D6 , $00D8..$00F6 ,
      $00F8..$00FF , $0100..$0131 , $0134..$013E , $0141..$0148 ,
      $014A..$017E , $0180..$01C3 , $01CD..$01F0 , $01F4..$01F5 ,
      $01FA..$0217 , $0250..$02A8 , $02BB..$02C1 , $0386 ,
      $0388..$038A , $038C , $038E..$03A1 , $03A3..$03CE ,
      $03D0..$03D6 , $03DA , $03DC , $03DE , $03E0 , $03E2..$03F3 ,
      $0401..$040C , $040E..$044F , $0451..$045C , $045E..$0481 ,
      $0490..$04C4 , $04C7..$04C8 , $04CB..$04CC , $04D0..$04EB ,
      $04EE..$04F5 , $04F8..$04F9 , $0531..$0556 , $0559 ,
      $0561..$0586 , $05D0..$05EA , $05F0..$05F2 , $0621..$063A ,
      $0641..$064A , $0671..$06B7 , $06BA..$06BE , $06C0..$06CE ,
      $06D0..$06D3 , $06D5 , $06E5..$06E6 , $0905..$0939 , $093D ,
      $0958..$0961 , $0985..$098C , $098F..$0990 , $0993..$09A8 ,
      $09AA..$09B0 , $09B2 , $09B6..$09B9 , $09DC..$09DD ,
      $09DF..$09E1 , $09F0..$09F1 , $0A05..$0A0A , $0A0F..$0A10 ,
      $0A13..$0A28 , $0A2A..$0A30 , $0A32..$0A33 , $0A35..$0A36 ,
      $0A38..$0A39 , $0A59..$0A5C , $0A5E , $0A72..$0A74 ,
      $0A85..$0A8B , $0A8D , $0A8F..$0A91 , $0A93..$0AA8 ,
      $0AAA..$0AB0 , $0AB2..$0AB3 , $0AB5..$0AB9 , $0ABD , $0AE0 ,
      $0B05..$0B0C , $0B0F..$0B10 , $0B13..$0B28 , $0B2A..$0B30 ,
      $0B32..$0B33 , $0B36..$0B39 , $0B3D , $0B5C..$0B5D ,
      $0B5F..$0B61 , $0B85..$0B8A , $0B8E..$0B90 , $0B92..$0B95 ,
      $0B99..$0B9A , $0B9C , $0B9E..$0B9F , $0BA3..$0BA4 ,
      $0BA8..$0BAA , $0BAE..$0BB5 , $0BB7..$0BB9 , $0C05..$0C0C ,
      $0C0E..$0C10 , $0C12..$0C28 , $0C2A..$0C33 , $0C35..$0C39 ,
      $0C60..$0C61 , $0C85..$0C8C , $0C8E..$0C90 , $0C92..$0CA8 ,
      $0CAA..$0CB3 , $0CB5..$0CB9 , $0CDE , $0CE0..$0CE1 ,
      $0D05..$0D0C , $0D0E..$0D10 , $0D12..$0D28 , $0D2A..$0D39 ,
      $0D60..$0D61 , $0E01..$0E2E , $0E30 , $0E32..$0E33 ,
      $0E40..$0E45 , $0E81..$0E82 , $0E84 , $0E87..$0E88 , $0E8A ,
      $0E8D , $0E94..$0E97 , $0E99..$0E9F , $0EA1..$0EA3 , $0EA5 ,
      $0EA7 , $0EAA..$0EAB , $0EAD..$0EAE , $0EB0 , $0EB2..$0EB3 ,
      $0EBD , $0EC0..$0EC4 , $0F40..$0F47 , $0F49..$0F69 ,
      $10A0..$10C5 , $10D0..$10F6 , $1100 , $1102..$1103 ,
      $1105..$1107 , $1109 , $110B..$110C , $110E..$1112 , $113C ,
      $113E , $1140 , $114C , $114E , $1150 , $1154..$1155 , $1159 ,
      $115F..$1161 , $1163 , $1165 , $1167 , $1169 , $116D..$116E ,
      $1172..$1173 , $1175 , $119E , $11A8 , $11AB , $11AE..$11AF ,
      $11B7..$11B8 , $11BA , $11BC..$11C2 , $11EB , $11F0 , $11F9 ,
      $1E00..$1E9B , $1EA0..$1EF9 , $1F00..$1F15 , $1F18..$1F1D ,
      $1F20..$1F45 , $1F48..$1F4D , $1F50..$1F57 , $1F59 , $1F5B ,
      $1F5D , $1F5F..$1F7D , $1F80..$1FB4 , $1FB6..$1FBC , $1FBE ,
      $1FC2..$1FC4 , $1FC6..$1FCC , $1FD0..$1FD3 , $1FD6..$1FDB ,
      $1FE0..$1FEC , $1FF2..$1FF4 , $1FF6..$1FFC , $2126 ,
      $212A..$212B , $212E , $2180..$2182 , $3041..$3094 ,
      $30A1..$30FA , $3105..$312C , $AC00..$D7A3 :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  // 86    Ideographic
  function isIdeographic(const c : SAXChar) : Boolean;
  begin
    case (Word(c)) of
      $4E00..$9FA5 , $3007 , $3021..$3029 :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  // 87    CombiningChar
  function isCombiningChar(const c : SAXChar) : Boolean;
  begin
    case (Word(c)) of
      $0300..$0345 , $0360..$0361 , $0483..$0486 , $0591..$05A1 ,
      $05A3..$05B9 , $05BB..$05BD , $05BF , $05C1..$05C2 , $05C4 ,
      $064B..$0652 , $0670 , $06D6..$06DC , $06DD..$06DF ,
      $06E0..$06E4 , $06E7..$06E8 , $06EA..$06ED , $0901..$0903 ,
      $093C , $093E..$094C , $094D , $0951..$0954 , $0962..$0963 ,
      $0981..$0983 , $09BC , $09BE , $09BF , $09C0..$09C4 ,
      $09C7..$09C8 , $09CB..$09CD , $09D7 , $09E2..$09E3 , $0A02 ,
      $0A3C , $0A3E , $0A3F , $0A40..$0A42 , $0A47..$0A48 ,
      $0A4B..$0A4D , $0A70..$0A71 , $0A81..$0A83 , $0ABC ,
      $0ABE..$0AC5 , $0AC7..$0AC9 , $0ACB..$0ACD , $0B01..$0B03 ,
      $0B3C , $0B3E..$0B43 , $0B47..$0B48 , $0B4B..$0B4D ,
      $0B56..$0B57 , $0B82..$0B83 , $0BBE..$0BC2 , $0BC6..$0BC8 ,
      $0BCA..$0BCD , $0BD7 , $0C01..$0C03 , $0C3E..$0C44 ,
      $0C46..$0C48 , $0C4A..$0C4D , $0C55..$0C56 , $0C82..$0C83 ,
      $0CBE..$0CC4 , $0CC6..$0CC8 , $0CCA..$0CCD , $0CD5..$0CD6 ,
      $0D02..$0D03 , $0D3E..$0D43 , $0D46..$0D48 , $0D4A..$0D4D ,
      $0D57 , $0E31 , $0E34..$0E3A , $0E47..$0E4E , $0EB1 ,
      $0EB4..$0EB9 , $0EBB..$0EBC , $0EC8..$0ECD , $0F18..$0F19 ,
      $0F35 , $0F37 , $0F39 , $0F3E , $0F3F , $0F71..$0F84 ,
      $0F86..$0F8B , $0F90..$0F95 , $0F97 , $0F99..$0FAD ,
      $0FB1..$0FB7 , $0FB9 , $20D0..$20DC , $20E1 , $302A..$302F ,
      $3099 , $309A :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  // 88    Digit
  function isDigit(const c : SAXChar) : Boolean;
  begin
    case (Word(c)) of
      $0030..$0039 , $0660..$0669 , $06F0..$06F9 , $0966..$096F ,
      $09E6..$09EF , $0A66..$0A6F , $0AE6..$0AEF , $0B66..$0B6F ,
      $0BE7..$0BEF , $0C66..$0C6F , $0CE6..$0CEF , $0D66..$0D6F ,
      $0E50..$0E59 , $0ED0..$0ED9 , $0F20..$0F29 :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  // 89    Extender
  function isExtender(const c : SAXChar) : Boolean;
  begin
    case (Word(c)) of
      $00B7 , $02D0 , $02D1 , $0387 , $0640 , $0E46 , $0EC6 , $3005 ,
      $3031..$3035 , $309D..$309E , $30FC..$30FE :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  function isPubidChar(const c : SAXChar) : Boolean;
  begin
    case (c) of
      #32, #13, #10, 'a'..'z', 'A'..'Z', '0'..'9', '-', '''', '(', ')',
      '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%' :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  function isWhitespace(const c : SAXChar) : Boolean;
  begin
    case Word(c) of
      $20 , $9 , $D , $A :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  function isXMLChar(const c : SAXChar) : Boolean;
  begin
    case Word(c) of
      //!! Upper 4-byte range $10000..$10FFFF omitted !!
      $9 , $A , $D , $20..$D7FF , $E000..$FFFD :
      Result:= True;
    else
      Result:= False;
    end;
  end;

  function isNameChar(const c : SAXChar) : Boolean;
  begin
    Result:= (c = '.') or (c = '-') or (c = '_') or (c = ':') or
      isBaseChar(c) or isIdeographic(c) or isDigit(c) or isCombiningChar(c) or
      isExtender(c);
  end;

  function isNameStartChar(const c : SAXChar) : Boolean;
  begin
    Result:= (c = '_') or (c = ':') or isBaseChar(c) or isIdeographic(c);
  end;

end.
