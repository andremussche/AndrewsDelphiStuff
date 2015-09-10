unit RttiClasses;

  {*************************************************************************}
  {                                                                         }
  {  This unit incapsulate RTTI algorithms and contains parent 		    }
  {  class TRttiEnabled to build various hierarchiesies. This               }
  {  modul is used en masse other units.				    }
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
uses
  TypInfo, IntfInfo, SysUtils, Classes;
type
{$TYPEINFO ON}
  TPropsProc = procedure(const PropInfo: TPropInfo) of Object;

  TRttiEnabled = class
  private
    FLine, FColumn: Integer;
    FTraversed: Boolean;
    FOwner: TRttiEnabled;
    procedure CreateChildObject(const PropInfo: TPropInfo);
    procedure FreeChildObject(const PropInfo: TPropInfo);
    procedure SetObjectProp(const PropInfo: TPropInfo; const Obj: TObject);
    function RecursionDepth: Integer;
    constructor CreateOwned(Owner: TRttiEnabled); 
  protected
    procedure OnDefaultCreate(Owner: TRttiEnabled; PropName: string); virtual;
    class function MaxRecursionDepth: Integer; virtual;
  public
    procedure Finalize;
    procedure Initialize;
    function GetObjectProp(const PropInfo: TPropInfo): TObject;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    function GetPointerProp(PropInfo: PPropInfo): Pointer;
    procedure SetPointerProp(PropInfo: PPropInfo; Value: Pointer);
    procedure IterateProps(PropsProc: TPropsProc);
    function GetNamedValue(PropName: string): Variant;
    procedure SetNamedValue(PropName: string; Value: Variant);
    function GetObjectByName(const FieldName: string): TObject;
    procedure ListChildObjects(Strings: TStrings);
    property NamedValue [PropName: string] : Variant read GetNamedValue write SetNamedValue;
    property Line: Integer read FLine write FLine;
    property Column: Integer read FColumn write FColumn;
    property Traversed: Boolean read FTraversed write FTraversed;
    property Owner: TRttiEnabled read FOwner;

    procedure BeforeLoad;virtual;
    procedure AfterLoad;virtual;
  end;
  TRttiClass = class of TRttiEnabled;

  TValuedRttiEnabled = class(TRttiEnabled)
  private
    FNodeValue: string;
  public
    property NodeValue : string read FNodeValue write FNodeValue;
  end;

  TExclusion = record
    Node: string;
    Prop: string;
    IsNode: Boolean;
  end;

  TExclusiveRttiEnabled = class(TRttiEnabled)
  private
    FExclusions: array of TExclusion;
  protected
    procedure AddExclusion(Node, Prop: string; IsNode: Boolean = False);
    procedure SetupExclusions; virtual;
  public
    constructor Create; override;
    function FindExclusion(Node: string; var Prop: string): Boolean;
    function IsNode(Node: string): Boolean;
  end;

{$TYPEINFO OFF}


  TRttiClassFactory = function(ClassName: string): TRttiEnabled;
  TRttiClassResolver = function(Obj: TRttiEnabled): string;


  ERttiException = class(Exception) end;
  ERttiSchemaNotSupported = class(ERttiException) end;
  ERttiBindException = class(ERttiException) end;

  TRttiEnabledArray = array of TRttiEnabled;
  
  IObjList = interface
  ['{CE6164EC-7561-40C5-B491-14DFD1DBD632}']
    function AsArray: TRttiEnabledArray;
    function ElementClass: TRttiClass;
    procedure Clear;
    function Add(Element: TRttiEnabled): Integer;
  end;
  
  IClassField = interface
  ['{DE4D9598-9537-43C6-A5BD-67D4B98A92CD}']
    function ValueAsString: string;
    procedure LoadFromString(const S: string);
    function IsBinary: Boolean;
  end;

  IPropTypeInfo = interface
  ['{65F7DEF9-6CFD-4718-868F-11A8459A9AE2}']
    function IsBinaryProp(PropName: string): Boolean;
  end;

function GetDynArrayLength(P: Pointer): Integer;

implementation

{ TRttiEnabled }

procedure TRttiEnabled.AfterConstruction;
begin
  inherited;
  Initialize;
end;

procedure TRttiEnabled.AfterLoad;
begin
  //
end;

procedure TRttiEnabled.BeforeLoad;
begin
  //
end;

constructor TRttiEnabled.Create;
begin
  inherited Create;
  //Initialize;    moved to AfterConstruction
end;

constructor TRttiEnabled.CreateOwned(Owner: TRttiEnabled);
begin
  FOwner := Owner;
  Create;
end;


procedure TRttiEnabled.Initialize;
begin
  IterateProps(CreateChildObject);
end;

function TRttiEnabled.RecursionDepth: Integer;
begin
  Result := 0;
  if Assigned(FOwner) and (ClassType = FOwner.ClassType) then
    Result := FOwner.RecursionDepth + 1;
end;

procedure TRttiEnabled.CreateChildObject(const PropInfo: TPropInfo);
var
  Obj: TObject;
  ClassType: TClass;
begin
  if PropInfo.PropType^.Kind = tkClass then
  begin
    ClassType := GetTypeData(PropInfo.PropType^).ClassType;
    if ClassType.InheritsFrom(TRttiEnabled) then
    begin
      if (ClassType <> Self.ClassType) or (RecursionDepth < MaxRecursionDepth) then
      begin
        Obj := TRttiClass(ClassType).CreateOwned(Self);
        SetObjectProp(PropInfo, Obj);
        TRttiEnabled(Obj).OnDefaultCreate(Self, string(PropInfo.Name) );
      end;
    end;
  end;
end;

destructor TRttiEnabled.Destroy;
begin
  Finalize;
  inherited;
end;

function _GetPointerProp(Instance: TObject; PropInfo: PPropInfo): Pointer;
asm
    { ->    EAX Pointer to instance         }
    {       EDX Pointer to property info    }
    { <-    EAX Longint result              }

    PUSH    EBX
    PUSH    EDI
    MOV     EDI,[EDX].TPropInfo.PropType
    MOV     EDI,[EDI]
    MOV     BL,otSLong
    CMP     [EDI].TTypeInfo.Kind,tkClass
    JE      @@isClass
    XOR     ECX,ECX
    MOV     CL,[EDI].TTypeInfo.Name.Byte[0]
    MOV     BL,[EDI].TTypeInfo.Name[ECX+1].TTypeData.OrdType
@@isClass:
    MOV     ECX,[EDX].TPropInfo.GetProc
    CMP     [EDX].TPropInfo.GetProc.Byte[3],$FE
    MOV     EDX,[EDX].TPropInfo.Index
    JB      @@isStaticMethod
    JA      @@isField

    {       the GetProc is a virtual method }
    MOVSX   ECX,CX                  { sign extend slot offs }
    ADD     ECX,[EAX]               { vmt   + slotoffs      }
    CALL    dword ptr [ECX]         { call vmt[slot]        }
    JMP     @@final

@@isStaticMethod:
    CALL    ECX
    JMP     @@final

@@isField:
    AND     ECX,$00FFFFFF
    ADD     ECX,EAX
    MOV     EAX,[ECX]
@@final:
@@exit:
    POP     EDI
    POP     EBX
end;

procedure _SetPointerProp(Instance: TObject; PropInfo: PPropInfo; Value: Pointer); assembler;
asm
    { ->    EAX Pointer to instance         }
    {       EDX Pointer to property info    }
    {       ECX Value                       }

    PUSH    EBX
    PUSH    ESI
    PUSH    EDI
    MOV     EDI,EDX

    MOV     ESI,[EDI].TPropInfo.PropType
    MOV     ESI,[ESI]
    MOV     BL,otSLong
    CMP     [ESI].TTypeInfo.Kind,tkClass
    JE      @@isClass
    XOR     EBX,EBX
    MOV     BL,[ESI].TTypeInfo.Name.Byte[0]
    MOV     BL,[ESI].TTypeInfo.Name[EBX+1].TTypeData.OrdType
@@isClass:
    MOV     EDX,[EDI].TPropInfo.Index       { pass Index in DX      }
    CMP     EDX,$80000000
    JNE     @@hasIndex
    MOV     EDX,ECX                         { pass value in EDX     }
@@hasIndex:
    MOV     ESI,[EDI].TPropInfo.SetProc
    CMP     [EDI].TPropInfo.SetProc.Byte[3],$FE
    JA      @@isField
    JB      @@isStaticMethod

    {       SetProc turned out to be a virtual method. call it      }
    MOVSX   ESI,SI                          { sign extend slot offset }
    ADD     ESI,[EAX]                       { vmt   + slot offset   }
    CALL    dword ptr [ESI]
    JMP     @@exit

@@isStaticMethod:
    CALL    ESI
    JMP     @@exit

@@isField:
    AND     ESI,$00FFFFFF
    ADD     EAX,ESI
    MOV     [EAX],ECX
@@exit:
    POP     EDI
    POP     ESI
    POP     EBX
end;

function GetDynArrayLength(P: Pointer): Integer;
begin
  asm
    MOV  EAX, DWORD PTR P
    CALL System.@DynArrayLength
    MOV DWORD PTR [Result], EAX
  end;
end;

procedure TRttiEnabled.Finalize;
begin
  IterateProps(FreeChildObject);
end;

procedure TRttiEnabled.FreeChildObject(const PropInfo: TPropInfo);
var
  LObject: TObject;
  ElmTypeInfo: PTypeInfo;
  I, Dims: Integer;
  DynArray: Pointer;
  ArrayLength: Integer;
  Ptr: PInteger;
begin
  if PropInfo.PropType^.Kind = tkClass then
  begin
    LObject := GetObjectProp(PropInfo);
    if LObject <> nil then
    begin
      SetObjectProp(PropInfo, nil);
      LObject.Free;
    end;
  end
  else if PropInfo.PropType^.Kind = tkDynArray then
  begin
    GetDynArrayElTypeInfo(PropInfo.PropType^, ElmTypeInfo, Dims);
    if Dims > 1 then
      raise ERttiSchemaNotSupported.Create('Multidemension array marshaling is not implemented');

    DynArray := GetDynArrayProp(Self, @PropInfo);
    //DynArray := GetPointerProp(@PropInfo);

    ArrayLength := GetDynArrayLength(DynArray);
    if ElmTypeInfo.Kind in [tkClass] then
    begin
      for I := 0 to ArrayLength - 1 do
      begin
        Ptr := PInteger(Integer(DynArray) + (I * 4));
        TObject(Ptr^).Free;
      end;
    end;
  end;
end;

function TRttiEnabled.GetNamedValue(PropName: string): Variant;
begin
  Result := GetPropValue(Self, PropName, False);
end;

procedure TRttiEnabled.SetNamedValue(PropName: string; Value: Variant);
begin
  SetPropValue(Self, PropName, Value);
end;


function TRttiEnabled.GetObjectProp(const PropInfo: TPropInfo): TObject;
begin
  Result := TypInfo.GetObjectProp(Self, @PropInfo)
end;

function TRttiEnabled.GetPointerProp(PropInfo: PPropInfo): Pointer;
begin
  Result := _GetPointerProp(Self, PropInfo)
end;

procedure TRttiEnabled.IterateProps(PropsProc: TPropsProc);
var
  Count, I: Integer;
  PropList: PPropList;
begin
  Count := GetPropList(Self, PropList);
  if Count > 0 then
    try
      for I := 0 to Count-1 do
        PropsProc(PropList[I]^);
    finally
      FreeMem(PropList);
    end;
end;

procedure TRttiEnabled.SetObjectProp(const PropInfo: TPropInfo;
  const Obj: TObject);
begin
  if PropInfo.SetProc <> nil then
    TypInfo.SetObjectProp(Self, @PropInfo, Obj);
end;

procedure TRttiEnabled.SetPointerProp(PropInfo: PPropInfo; Value: Pointer);
begin
  _SetPointerProp(Self, PropInfo, Value);
end;


procedure TRttiEnabled.OnDefaultCreate(Owner: TRttiEnabled; PropName: string);
begin
end;

function TRttiEnabled.GetObjectByName(const FieldName: string): TObject;
begin
  Result := TypInfo.GetObjectProp(Self, FieldName);
end;

procedure TRttiEnabled.ListChildObjects(Strings: TStrings);
var
  Count, I: Integer;
  PropList: PPropList;
begin
  Strings.Clear;
  Count := GetPropList(Self, PropList);
  if Count > 0 then
    try
      for I := 0 to Count-1 do
        if PropList[I]^.PropType^.Kind = tkClass then
          Strings.AddObject( string(PropList[I]^.Name), GetObjectProp(PropList[I]^));
    finally
      FreeMem(PropList);
    end;
end;

{ TCustomRttiEnabled }

procedure TExclusiveRttiEnabled.AddExclusion(Node, Prop: string;
  IsNode: Boolean = False);
begin
  SetLength(FExclusions, Length(FExclusions) + 1);
  FExclusions[High(FExclusions)].Node := Node;
  FExclusions[High(FExclusions)].Prop := Prop;
  FExclusions[High(FExclusions)].IsNode := IsNode;
end;

constructor TExclusiveRttiEnabled.Create;
begin
  inherited Create;
  SetupExclusions;
end;

function TExclusiveRttiEnabled.FindExclusion(Node: string;
  var Prop: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FExclusions) do
  begin
    if SameText(FExclusions[I].Node, Node) then
    begin
      Prop := FExclusions[I].Prop;
      Result := True;
      Break;
    end;
  end;
end;

function TExclusiveRttiEnabled.IsNode(Node: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FExclusions) do
  begin
    if SameText(FExclusions[I].Node, Node) then
    begin
      Result := FExclusions[I].IsNode;
      Break;
    end;
  end;
end;

procedure TExclusiveRttiEnabled.SetupExclusions;
begin
end;


class function TRttiEnabled.MaxRecursionDepth: Integer;
begin
  Result := 1;
end;

initialization
end.


