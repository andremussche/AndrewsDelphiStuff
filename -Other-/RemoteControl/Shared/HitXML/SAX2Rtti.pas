unit SAX2Rtti;

  {*************************************************************************}
  {                                                                         }
  {  This unit correspond link between SAX and RTTI technologies.           }
  {                                                                         }
  {                                                                         }
  {                                                                         }
  {  Copyright (c) 2000-2008 Hitsoft LLC.                                   }
  {                                                                         }
  {*************************************************************************}


  {*************************************************************************}
  {                                                                         }
  {  Hitsoft Xml Object Library                                             }
  {                                                                         }
  {  Copyright (C) 2009    Hitsoft LLC. (http://opensource.hitsoft-it.com)  }
  {                                                                         }
  {  This program is free software: you can redistribute it and/or modify   }
  {  it under the terms of the GNU General Public License as published by   }
  {  the Free Software Foundation, either version 3 of the License, or      }
  {  (at your option) any later version.                                    }
  {                                                                         }
  {  This program is distributed in the hope that it will be useful,        }
  {  but WITHOUT ANY WARRANTY; without even the implied warranty of         }
  {  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          }
  {  GNU General Public License for more details.                           }
  {                                                                         }
  {  You should have received a copy of the GNU General Public License      }
  {  along with this program.  If not, see <http://www.gnu.org/licenses/>.  }
  {                                                                         }
  {*************************************************************************}

interface

{$I compilers.inc}

uses
  SAX, RttiClasses;

function RttiContentHandler(var RootObject: TRttiEnabled; Factory: TRttiClassFactory): IContentHandler;
function RttiErrorHandler: IErrorHandler;
procedure WriteRttiObject(ContentHandler: IContentHandler; RootObject: TRttiEnabled; RootName: string; Camelize: Boolean = True; ClassResolver: TRttiClassResolver = nil);
function CamelToXml(const S: string; DoConversion: Boolean = True): string;
function XmlToCamel(NodeName: string): string;

const
  sClassAttrName = 'CLASS';
  sArrayElementNodeName = 'value';

implementation
uses
  Contnrs, TypInfo, Classes, SysUtils, IntfInfo, SAXHelpers, Windows, Types,
  Dialogs, Controls;
type
  TObsoleteNode = class(TRttiEnabled)
  end;

  PRttiEnabled = ^TRttiEnabled;

  TRttiContentHandler = class( TInterfacedObject, IContentHandler)
  private
    FStrict: Boolean; // Controlleer strict of xml overeenkomt met objectstructuur

    FRootInitialized, FRootFinalized: Boolean;
    FProcessStack: TStringList;
    FCurrentObject: TRttiEnabled;
    FDestination: PRttiEnabled;
    FFactory: TRttiClassFactory;
    FCurrentName: string;
    FObsoleteNode: TRttiEnabled;
    procedure Push;
    procedure Pop;
    function ObjectPath: string;
    function FindClassAttribute(const Attrs: IAttributes; out ClassID: string) : Boolean;
    procedure HandleAttributes(const Attrs: IAttributes);
  public
    constructor Create(var RootObject: TRttiEnabled; Factory: TRttiClassFactory);
    destructor Destroy; override;
    { IContentHandler }
    procedure setDocumentLocator(const locator: ILocator);
    procedure startDocument;
    procedure endDocument;
    procedure startPrefixMapping(const prefix, uri : SAXString);
    procedure endPrefixMapping(const prefix : SAXString);
    procedure startElement(const uri, localName, qName: SAXString; const atts: IAttributes);
    procedure endElement(const uri, localName, qName: SAXString);
    procedure characters(const ch : SAXString);
    procedure ignorableWhitespace(const ch : SAXString);
    procedure processingInstruction(const target, data : SAXString);
    procedure skippedEntity(const name : SAXString);
  end;

  TErrorHandler = class( TInterfacedObject, IErrorHandler )
  private
  public
    constructor Create;
    destructor Destroy; override;
    { IErrorHandler }
    procedure warning(const e : ISAXParseError);
    procedure error(const e : ISAXParseError);
    procedure fatalError(const e : ISAXParseError);
  end;

function CamelToXml(const S: string; DoConversion: Boolean = True): string;
var
  I: Integer;
begin
  if DoConversion then
  begin
    Result := '';
    for I := 1 to Length(S) do
      if IsCharUpper(S[I]) then
        if I <> 1 then
          Result := Result + '-' + Char(CharLower(PChar(Ord(S[I]))))
        else
          Result := Result + Char(CharLower(PChar(Ord(S[I]))))
      else
        Result := Result + S[I];
  end else
  begin
    Result := S;
  end;
end;

function XmlToCamel(NodeName: string): string;
var
  I: Integer;
  UpcaseNext: Boolean;
begin
  Result := '';
  UpcaseNext := True;
  for I := 1 to Length(NodeName) do
    case NodeName[I] of
      '-': UpcaseNext := True;
      else
        begin
          if UpcaseNext then
            Result := Result + UpCase(NodeName[I])
          else
            Result := Result + LowerCase(NodeName[I]);
          UpcaseNext := False;
        end;
    end;
end;

function Hex2Bin(const HexSource: string): string;
begin
  SetLength(Result, Length(HexSource) div 2);
  HexToBin(PChar(LowerCase(HexSource)), PChar(Result), Length(Result));
end;

function Bin2Hex(const Source: string): string;
begin
  SetLength(Result, Length(Source) * 2);
  BinToHex(PChar(Source), PChar(Result), Length(Source));
end;

function RttiContentHandler(var RootObject: TRttiEnabled; Factory: TRttiClassFactory): IContentHandler;
begin
  Result := TRttiContentHandler.Create(RootObject, Factory);
end;

function RttiErrorHandler: IErrorHandler;
begin
  Result := TErrorHandler.Create;
end;

function GetDynArrayLength(P: Pointer): Integer;
begin
  asm
    MOV  EAX, DWORD PTR P
    CALL System.@DynArrayLength
    MOV DWORD PTR [Result], EAX
  end;
end;


procedure WriteRttiObject(ContentHandler: IContentHandler; RootObject: TRttiEnabled; RootName: string; Camelize: Boolean = True; ClassResolver: TRttiClassResolver = nil);

  procedure WriteObject(Obj: TRttiEnabled; NodeName: string; DefType: TClass);
  var
    Attrs: TAttributesImpl;
    PropCnt: Integer;
    PropList: PPropList;
    I,J: Integer;
    ElmTypeInfo: PTypeInfo;
    Dims: Integer;
    DynArray: Pointer;
    ObjArray: TRttiEnabledArray;
    StrArray: TStringDynArray;
    ArrayLength: Integer;
    ChildObj: TObject;
    ObjList: IObjList;
    IntValue: Integer;
    StrValue: string;
    ExtValue: Extended;
    PropTypeInfo: IPropTypeInfo;
    ClassField: IClassField;
    TypeData: PTypeData;
    ChildArrayName: string;
  begin
    NodeName := CamelToXml(NodeName, False);
    ObjArray := nil;
    // scavenge fields for attributes
    Attrs := TAttributesImpl.Create;
    if (Obj.ClassType <> DefType) and Assigned(ClassResolver) then
      Attrs.addAttribute('', '', sClassAttrName, '', ClassResolver(Obj));
    PropList := nil;
    PropCnt  := GetPropList(Obj, PropList);
    for I := 0 to PropCnt-1 do
      if IsStoredProp(Obj, PropList[I]) then          //only save "stored" properties
      case PropList[I].PropType^^.Kind of
        tkInteger,
        tkChar,
        tkWChar:
          begin
            IntValue := GetOrdProp(Obj, PropList[I]);
            if IntValue <> 0 then
              Attrs.addAttribute('', '', CamelToXml( string(PropList[I].Name), Camelize), '', IntToStr(IntValue));
          end;
        tkEnumeration:
          begin
            IntValue := GetOrdProp(Obj, PropList[I]);
            StrValue := GetEnumName(PropList[I].PropType^, IntValue);
            //Attrs.addAttribute('', '', CamelToXml( string(PropList[I].Name), Camelize), '', IntToStr(IntValue));
            Attrs.addAttribute('', '', CamelToXml( string(PropList[I].Name), Camelize), '', StrValue);
          end;
        tkString,
        {$ifdef COMPILER_12_UP}
        tkUString,
        {$endif}
        tkLString:
          begin
            StrValue := GetStrProp(Obj, PropList[I]);
            if Supports(Obj, IPropTypeInfo, PropTypeInfo) then
              if PropTypeInfo.IsBinaryProp( string(PropList[I].Name)) then
                StrValue := Bin2Hex(StrValue);
            if StrValue <> '' then
              Attrs.addAttribute('', '', CamelToXml( string(PropList[I].Name), Camelize), '', StrValue);
          end;
        tkFloat:
          begin
            ExtValue := GetFloatProp(Obj, PropList[I]);
            if ExtValue <> 0 then
              Attrs.addAttribute('', '', CamelToXml( string(PropList[I].Name), Camelize), '', FloatToStr(ExtValue));
          end;
        tkDynArray: ;
        tkClass:
          begin
            ChildObj := GetObjectProp(Obj, PropList[I]);
            if Supports(ChildObj, IClassField, ClassField) then
            begin
              StrValue := ClassField.ValueAsString;
              if ClassField.IsBinary then
                StrValue := Bin2Hex(StrValue);
              Attrs.addAttribute('', '', CamelToXml( string(PropList[I].Name)), '', StrValue);
            end;
            // skip to after start
          end
        else
        begin
          sleep(0);
          Assert(False);
        end;
      end;


    ContentHandler.StartElement('', NodeName, NodeName, Attrs);
    Attrs.Free;
    // scavenge fields for subnodes
    for I := 0 to PropCnt-1 do
      if IsStoredProp(Obj, PropList[I]) then          //only save "stored" properties
      case PropList[I].PropType^^.Kind of
        tkDynArray:
          begin
            GetDynArrayElTypeInfo(PropList[I].PropType^, ElmTypeInfo, Dims);
            if Dims > 1 then
              raise ERttiSchemaNotSupported.Create('Multidimension array are not supported');
            DynArray := GetDynArrayProp(Obj, PropList[I]);
            //DynArray := Obj.GetPointerProp(PropList[I]);
            ArrayLength := GetDynArrayLength(DynArray);
            TypeData := GetTypeData(ElmTypeInfo);

            ChildArrayName := CamelToXml( string(PropList[I].Name), Camelize);
            case ElmTypeInfo.Kind of
              tkClass:
                begin
                  ObjArray := TRttiEnabledArray(DynArray);
                  for J := 0 to ArrayLength-1 do
                    WriteObject(ObjArray[J],  string(PropList[I].Name), TypeData.ClassType);
                end;
              {$ifdef COMPILER_12_UP}
              tkUString,
              {$endif}
              tkLString:
                begin
                  ContentHandler.StartElement('', ChildArrayName, ChildArrayName, nil);
                  StrArray := TStringDynArray(DynArray);
                  for J := 0 to ArrayLength-1 do
                  begin
                    ContentHandler.StartElement('', sArrayElementNodeName, sArrayElementNodeName, nil);
                    ContentHandler.characters(StrArray[J]);

                    ContentHandler.endElement('', sArrayElementNodeName, sArrayElementNodeName);
                  end;
                  ContentHandler.EndElement('', ChildArrayName, ChildArrayName);
                end;



              else
                raise ERttiSchemaNotSupported.Create('Unsupported array element type');
            end;


          end;
        tkClass:
          begin
            ChildObj := GetObjectProp(Obj,  string(PropList[I].Name));
            TypeData := GetTypeData(PropList[I].PropType^);
            if ChildObj = nil then
            begin
              ChildObj := TypeData.ClassType.Create;
              if ChildObj is TRttiEnabled then
                WriteObject(ChildObj as TRttiEnabled,  string(PropList[I].Name), TypeData.ClassType);
            end
            else
            begin
              if Supports(ChildObj, IObjList, ObjList) then
              begin
                ObjArray := ObjList.AsArray;
                ArrayLength := Length(ObjArray);
                for J := 0 to ArrayLength-1 do
                  WriteObject(ObjArray[J],  string(PropList[I].Name), ObjArray[J].ClassType);
              end
              else if Supports(ChildObj, IClassField, ClassField) then
              begin
                // do noting
              end
              else
                if ChildObj is TRttiEnabled then
                  WriteObject(ChildObj as TRttiEnabled,  string(PropList[I].Name), TypeData.ClassType);
            end;

          end;
      end;

    if PropCnt > 0 then
      FreeMem(PropList);

    if Obj is TValuedRttiEnabled then
    begin
      ContentHandler.characters(TValuedRttiEnabled(Obj).NodeValue);
    end;

    ContentHandler.EndElement('', NodeName, NodeName);
  end;

begin
  WriteObject(RootObject, RootName, RootObject.ClassType);
end;

{ TRttiContentHandler }

procedure TRttiContentHandler.characters(const ch: SAXString);
begin
  if (FCurrentObject is TValuedRttiEnabled) then
    with TValuedRttiEnabled(FCurrentObject) do
      NodeValue := NodeValue + ch;
end;

constructor TRttiContentHandler.Create(var RootObject: TRttiEnabled; Factory: TRttiClassFactory);
begin
  FProcessStack := TStringList.Create;
  FProcessStack.Sorted := False;
  FCurrentObject := RootObject;
  FDestination := @RootObject;
  FFactory := Factory;

  FCurrentName := 'root';
  FObsoleteNode := TObsoleteNode.Create;

  FStrict := True; // default is strict controleren en bij fout: show message
end;

destructor TRttiContentHandler.Destroy;
begin
  FProcessStack.Free;
  FObsoleteNode.Free;
  inherited;
end;

procedure TRttiContentHandler.endDocument;
begin
  // do nothing
end;

procedure TRttiContentHandler.endElement(const uri, localName, qName: SAXString);
begin
  if FCurrentObject <> nil then
  begin
    FCurrentObject.AfterLoad;
  end;
  Pop;
end;

procedure TRttiContentHandler.endPrefixMapping(const prefix: SAXString);
begin
  // do nothing
end;

procedure TRttiContentHandler.ignorableWhitespace(const ch: SAXString);
begin
  // do nothing
end;

function TRttiContentHandler.ObjectPath: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FProcessStack.Count-1 do
    Result := FProcessStack[I] + '.' + Result;
  Result := Result + '.' + FCurrentName;
end;

procedure TRttiContentHandler.Pop;
begin
  if FProcessStack.Count > 0 then
  begin
    FCurrentObject := TRttiEnabled(FProcessStack.Objects[0]);
    FCurrentName := FProcessStack[0];
    FProcessStack.Delete(0);
  end else
  begin
//    Assert(not FRootFinalized);
    FRootFinalized := True;
  end;
end;

procedure TRttiContentHandler.processingInstruction(const target,
  data: SAXString);
begin
  // do nothing
end;

procedure TRttiContentHandler.Push;
begin
  FProcessStack.InsertObject(0, FCurrentName, FCurrentObject);
end;

procedure TRttiContentHandler.setDocumentLocator(const locator: ILocator);
begin
  // do nothing
end;

procedure TRttiContentHandler.skippedEntity(const name: SAXString);
begin
  // do nothing
end;

procedure TRttiContentHandler.startDocument;
begin
  // do nothing
end;

procedure TRttiContentHandler.HandleAttributes(const Attrs: IAttributes);
var
  I, iValue: Integer;
  PropInfo: PPropInfo;
  AttrName, AttrValue: string;
  PropTypeInfo: IPropTypeInfo;
  ClassField: IClassField;
  ChildObj: TObject;
begin
  for I := 0 to Attrs.Length-1 do
  begin
    AttrName := XmlToCamel(Attrs.getLocalName(I));

    if FCurrentObject is TExclusiveRttiEnabled then
    begin
      if not TExclusiveRttiEnabled(FCurrentObject).IsNode(Attrs.getLocalName(I)) then
        TExclusiveRttiEnabled(FCurrentObject).FindExclusion(Attrs.getLocalName(I), AttrName);
    end;

    AttrValue := Attrs.getValue(I);
    PropInfo := GetPropInfo(FCurrentObject, AttrName);
    if PropInfo = nil then
      continue;
//    raise ERttiBindException.CreateFmt('Cannot bind XML node with path %s on attributes %s', [ObjectPath, AttrName]);

    case PropInfo.PropType^.Kind of
      tkInteger,
      tkChar, tkWChar: SetOrdProp(FCurrentObject, PropInfo, StrToInt(AttrValue));
      tkEnumeration:
      begin
        iValue := GetEnumValue(PropInfo.PropType^, AttrValue);
        //SetOrdProp(FCurrentObject, PropInfo, StrToInt(AttrValue));
        SetOrdProp(FCurrentObject, PropInfo, iValue);
      end;
      {$ifdef COMPILER_12_UP}
      tkUString,
      {$endif}
      tkString,
      tkLString:
        begin
          if Supports(FCurrentObject, IPropTypeInfo, PropTypeInfo) then
            if PropTypeInfo.IsBinaryProp( string(PropInfo.Name)) then
              AttrValue := Hex2Bin(AttrValue);
          SetStrProp(FCurrentObject, PropInfo, AttrValue);
        end;
      tkFloat: SetFloatProp(FCurrentObject, PropInfo, StrToFloat(AttrValue));
      tkClass:
        begin
          ChildObj := GetObjectProp(FCurrentObject, PropInfo);
          if Supports(ChildObj, IClassField, ClassField) then
          begin
            if ClassField.IsBinary then
              AttrValue := Hex2Bin(AttrValue);
            ClassField.LoadFromString(AttrValue);
          end
          else
          begin
            if FStrict then
            begin
              if MessageDlg(Format('XML property "%s" kan niet worden ingeladen. '+#13#10+
                                   'Toch doorgaan met eventueel settings verlies tot gevolg?', [PropInfo.Name]),
                            mtWarning, [mbYes, mbNo], 0) = mrYes
              then
                FStrict := False
              else
                raise Exception.CreateFmt('XML Property "%s" kan niet worden ingeladen',[PropInfo.Name]);
            end;
          end;
        end;
      else
      begin
        if FStrict then
        begin
          if MessageDlg(Format('XML property "%s" kan niet worden ingeladen. '+#13#10+
                               'Toch doorgaan met eventueel settings verlies tot gevolg?', [PropInfo.Name]),
                        mtWarning, [mbYes, mbNo], 0) = mrYes
          then
            FStrict := False
          else
            raise Exception.CreateFmt('XML Property "%s" kan niet worden ingeladen',[PropInfo.Name]);
        end;
      end;
    end;
  end;
end;


procedure TRttiContentHandler.startElement(const uri, localName, qName: SAXString; const atts: IAttributes);
var
  PropInfo: PPropInfo;
  Obj: TObject;
  ChildObject: TRttiEnabled;
  PropName: string;
  ElmTypeInfo: PTypeInfo;
  Dims: Integer;
  TypeData: PTypeData;
  DynArray: Pointer;
  ArrayLength: Integer;
  ObjList: IObjList;
  ClassName: string;
begin
  PropName := XmlToCamel(localName);

  if not FRootInitialized then
  begin
    if FCurrentObject = nil then
    begin
      Assert(Assigned(FFactory));
      FCurrentObject := FFactory(PropName);
      FDestination^ := FCurrentObject;
    end;
    FRootInitialized := True;
    HandleAttributes(atts);
    Exit;
  end;

  if FCurrentObject is TExclusiveRttiEnabled then
  begin
    if TExclusiveRttiEnabled(FCurrentObject).IsNode(localName) then
      TExclusiveRttiEnabled(FCurrentObject).FindExclusion(localName, PropName);
  end;

  if FCurrentObject <> nil then
    PropInfo := GetPropInfo(FCurrentObject, PropName)
  else
    PropInfo := nil;

  if PropInfo = nil then
  begin
    Push;
    FCurrentName := PropName;
    FCurrentObject := FObsoleteNode;
  end
  else
    case PropInfo.PropType^.Kind of
     tkClass:
      begin
        Obj := GetObjectProp(FCurrentObject, PropInfo);
        if Supports(Obj, IObjList, ObjList) then
        begin
          ChildObject := ObjList.ElementClass.Create;
          ChildObject.Traversed := True;
          ArrayLength := ObjList.Add(ChildObject) + 1;
          Push;
          FCurrentObject := ChildObject;
          FCurrentName := PropName + Format('[%d]', [ArrayLength-1]);
          HandleAttributes(atts);
        end
        else
        begin
          if FindClassAttribute(atts, ClassName) and Assigned(FFactory) then
          begin
            Obj.Free;
            Obj := FFactory(ClassName);
            SetObjectProp(FCurrentObject, PropInfo, Obj);
          end;

          ChildObject := Obj as TRttiEnabled;
          if ChildObject <> nil then
          begin
            Assert(ChildObject <> nil, Format('Path %s object of class %s is null', [ObjectPath, GetObjectPropClass(FCurrentObject, PropInfo).ClassName]));
            ChildObject.Traversed := True;
            ChildObject.Line := 0;
            ChildObject.Column := 0;
            Push;
            FCurrentObject := ChildObject;
          end
          else
          begin
            Push;
            FCurrentObject := FObsoleteNode;
          end;

          FCurrentName := PropName;

          // HC 5376
          if FCurrentObject <> nil then
            FCurrentObject.BeforeLoad;

          HandleAttributes(atts);
        end;
      end;
     tkDynArray:
      begin
        GetDynArrayElTypeInfo(PropInfo.PropType^, ElmTypeInfo, Dims);
        if Dims > 1 then
          raise ERttiSchemaNotSupported.Create('Multidimension array are not supported');

        case ElmTypeInfo.Kind of

          tkClass:
          begin
            TypeData := GetTypeData(ElmTypeInfo);

            if FindClassAttribute(atts, ClassName) and Assigned(FFactory) then
              ChildObject := FFactory(ClassName)
            else
              ChildObject := TRttiClass(TypeData.ClassType).Create as TRttiEnabled;
            ChildObject.Traversed := True;
            DynArray := GetDynArrayProp(FCurrentObject, PropInfo);
 //           DynArray := FCurrentObject.GetPointerProp(PropInfo);
            ArrayLength := GetDynArrayLength(DynArray);
            Inc(ArrayLength);
            DynArraySetLength(DynArray, PropInfo.PropType^, 1, @ArrayLength);
            TRttiEnabledArray(DynArray)[ArrayLength-1] := ChildObject;
            FCurrentObject.SetPointerProp(PropInfo, DynArray);
            Push;
            FCurrentObject := ChildObject;
            FCurrentName := PropName + Format('[%d]', [ArrayLength-1]);

            // HC 5376
            if FCurrentObject <> nil then
              FCurrentObject.BeforeLoad;

            HandleAttributes(atts);
          end;

          {$ifdef COMPILER_12_UP}
          tkUString,
          {$endif}
          tkLString:
          begin
            raise ERttiSchemaNotSupported.Create('Nested elements are not nodes');
            DynArray := GetDynArrayProp(FCurrentObject, PropInfo);
//            DynArray := FCurrentObject.GetPointerProp(PropInfo);
            ArrayLength := GetDynArrayLength(DynArray);
            Inc(ArrayLength);
            DynArraySetLength(DynArray, PropInfo.PropType^, 1, @ArrayLength);

            //TStringDynArray(DynArray)[ArrayLength-1] := ChildObject;
            FCurrentObject.SetPointerProp(PropInfo, DynArray);
            Push;
            FCurrentObject := ChildObject;
            FCurrentName := PropName + Format('[%d]', [ArrayLength-1]);

            // HC 5376
            if FCurrentObject <> nil then
              FCurrentObject.BeforeLoad;

            HandleAttributes(atts);
          end;

        else
          raise ERttiSchemaNotSupported.Create('Nested elements are not nodes');
        end;
      end;
      else
        if FStrict then
        begin
          if MessageDlg(Format('XML property "%s" kan niet worden ingeladen. '+#13#10+
                               'Toch doorgaan met eventueel settings verlies tot gevolg?', [PropInfo.Name]),
                        mtWarning, [mbYes, mbNo], 0) = mrYes
          then
            FStrict := False
          else
            raise Exception.CreateFmt('XML Property "%s" kan niet worden ingeladen',[PropInfo.Name]);
        end;
//        Assert(False);
    end;
end;

procedure TRttiContentHandler.startPrefixMapping(const prefix, uri: SAXString);
begin
  // do nothing
end;


function TRttiContentHandler.FindClassAttribute(const Attrs: IAttributes; out ClassID: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Attrs.Length-1 do
  begin
    if Attrs.getLocalName(I) = sClassAttrName then
    begin
      Result := True;
      ClassID := Attrs.getValue(I);
      Break;
    end;
  end;
end;

{ TErrorHandler }

constructor TErrorHandler.Create;
begin
end;

destructor TErrorHandler.Destroy;
begin
  inherited;
end;

procedure TErrorHandler.error(const e: ISAXParseError);
begin
  raise ERttiException.Create(e.getMessage);
end;

procedure TErrorHandler.fatalError(const e: ISAXParseError);
begin
  raise ERttiException.Create(e.getMessage);
end;

procedure TErrorHandler.warning(const e: ISAXParseError);
begin
  //raise ERttiException.Create(e.getMessage);
end;

end.
