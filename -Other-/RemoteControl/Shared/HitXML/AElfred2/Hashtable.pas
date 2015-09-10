// Filename : Hashtable.pas
// Version  : 1.1 (Delphi)
// Date     : July 4, 2003
// Author   : Jeff Rafter
// Details  : http://xml.defined.net/SAX/aelfred2
// License  : Please read License.txt
unit Hashtable;

// !! This is meant to be upgraded !! -- I know it is awful. 

interface

{$WEAKPACKAGEUNIT ON}

uses Classes;

type

  TWideStringDynarray = array of WideString;
  TPointerDynarray = array of Pointer;

  THashtable = class(TObject)
  private
    Fkeys : TWideStringDynarray;
    Felements : TPointerDynarray;
    FwideStringElements : TWideStringDynarray;
    Flength : Integer;
    FwideStringTable : Boolean;
    function indexOf(const a : TWideStringDynarray; const s : WideString) : Integer;
    procedure ensureCapacity(const n : Integer);
    function getCount: Integer;
  public
    constructor Create(); overload;
    constructor Create(const size : Integer); overload;
    constructor Create(const wideStringTable : boolean); overload;
    destructor Destroy(); override;
    function getKeys() : TWideStringDynarray;
    function getElements() : TPointerDynarray;
    function getWideStringElements() : TWideStringDynarray;
    function getLength() : Integer;
    procedure put(const key : WideString; const element : Pointer);
    function get(const key : WideString) : Pointer;
    procedure putWideString(const key : WideString; const element : WideString);
    function getWideString(const key : WideString) : WideString;
    function containsKey(const key : WideString) : Boolean;
    function has(const key : WideString) : Boolean;
    procedure clear();
    property Count : Integer read getCount;
  end;

implementation

function THashtable.getCount: Integer;
begin
  Result:= Length(getKeys());
end;

{ THashtable }

constructor THashtable.Create;
begin
  inherited Create();
  Flength:= 0;
  FwideStringTable:= false;
end;

constructor THashtable.Create(const size: Integer);
begin
  inherited Create();
  Flength:= 0;
  FwideStringTable:= false;
  ensureCapacity(size);
end;

constructor THashtable.Create(const wideStringTable: boolean);
begin
  inherited Create();
  Flength:= 0;
  FwideStringTable:= wideStringTable;
end;

destructor THashtable.Destroy;
begin
  inherited;
end;

function THashtable.getElements: TPointerDynarray;
begin
  Result:= Felements;
end;

function THashtable.getKeys: TWideStringDynarray;
begin
  Result:= Fkeys;
end;

procedure THashtable.putWideString(const key, element: WideString);
var index : Integer;
begin
  index:= indexOf(Fkeys, key);
  if (index <> -1) then
  begin
    FwideStringElements[index]:= element;
  end else
  begin
    Inc(Flength);
    ensureCapacity(Flength);
    Fkeys[Flength-1]:= key;
    FwideStringElements[Flength-1]:= element;
  end;
end;

procedure THashtable.put(const key : WideString; const element : Pointer);
var index : Integer;
begin
  index:= indexOf(Fkeys, key);
  if (index <> -1) then
  begin
    Felements[index]:= element;
  end else
  begin
    Inc(Flength);
    ensureCapacity(Flength);
    Fkeys[Flength-1]:= key;
    Felements[Flength-1]:= element;
  end;
end;

function THashtable.getWideString(const key: WideString): WideString;
var index : Integer;
begin
  index:= indexOf(Fkeys, key);
  if (index <> -1) then
    Result:= FwideStringElements[index]
  else
    Result:= '';
end;

function THashtable.get(const key: WideString): Pointer;
var index : Integer;
begin
  index:= indexOf(Fkeys, key);
  if (index <> -1) then
    Result:= Felements[index]
  else
    Result:= nil;
end;

function THashtable.indexOf(const a : TWideStringDynarray;
  const s: WideString): Integer;
var I : Integer;
begin
  if (a = nil) then
  begin
    Result:= -1;
    Exit;
  end;

  for I:= 0 to Length(a)-1 do
  begin
    if (a[I] = s) then
    begin
      Result:= I;
      Exit;
    end;
  end;
  Result:= -1;
end;

procedure THashtable.ensureCapacity(const n: Integer);
var max : Integer;
begin
  if ((n > 0) and (Length(Fkeys)=0)) then
  begin
    SetLength(Fkeys, n);
    SetLength(FwideStringElements, n);
    SetLength(Felements, n);
  end;

  max:= Length(Fkeys);
  if (max >= n) then Exit;

  while (max < n) do
    max:= max * 2;

  SetLength(Fkeys, max);
  SetLength(FwideStringElements, max);
  SetLength(Felements, max);
end;

function THashtable.containsKey(const key: WideString): Boolean;
begin
  Result:= indexOf(Fkeys, key) <> -1;
end;

function THashtable.getLength: Integer;
begin
  Result:= Flength;
end;

function THashtable.getWideStringElements: TWideStringDynarray;
begin
  Result:= FwideStringElements;
end;

procedure THashtable.clear;
begin
  Flength:= 0;
  SetLength(Fkeys, 0);
  SetLength(FwideStringElements, 0);
  SetLength(Felements, 0);
end;

function THashtable.has(const key: WideString): Boolean;
begin
  Result:= containsKey(key);
end;

end.
