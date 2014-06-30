unit mcCodeInsideSymbols;

interface

uses
  Classes,
  ToolsAPI;

type
  TCodeInsightSymbolList = class(TInterfacedObject,
                                 IOTACodeInsightSymbolList)
  private
    FStrings: Tstrings;
  public
    procedure  AfterConstruction;override;
    destructor Destroy; override;

    procedure AddSymbol(const aText: string);
  protected
    { Implementor should clear its symbol list }
    procedure Clear;
    { returns the count of the symbols in the list - may be modified by setting a filter - }
    function GetCount: Integer;
    { returns whether the symbol is able to be read from and written to }
    function GetSymbolIsReadWrite(I: Integer): Boolean;
    { returns whether the symbols is abstract.  Viewer draws these in the 'need to implement' color }
    function GetSymbolIsAbstract(I: Integer): Boolean;
    { return the symbol flags for the item at index 'I'.  I is the index in the filtered list }
    function GetViewerSymbolFlags(I: Integer): TOTAViewerSymbolFlags;
    { return the visibility flags for the item at index 'I'.  I is the index in the filtered list }
    function GetViewerVisibilityFlags(I: Integer): TOTAViewerVisibilityFlags;
    { return the procedure flags for the item at index 'I'.  I is the index in the filtered list }
    function GetProcDispatchFlags(I: Integer): TOTAProcDispatchFlags;
    { The list was requested to be sorted by 'Value' }
    procedure SetSortOrder(const Value: TOTASortOrder);
    { returns the sort order of the list }
    function GetSortOrder: TOTASortOrder;
    { given an identifier, return the index of the closest partial match }
    function FindIdent(const AnIdent: string): Integer;
    { given an identifier, find the 'Index' of an exact match in the list and return True.  Otherwise return False }
    function FindSymIndex(const Ident: string; var Index: Integer): Boolean;
    { set the lists filter to 'FilterText'.  It is up to the implementor to determine how to filter or if they even want to filter }
    procedure SetFilter(const FilterText: string);
    { return the symbol text for item 'Index'.  i.e. Form1 }
    function GetSymbolText(Index: Integer): string;
    { return the symbol type text for item 'Index'.  i.e. TForm1 }
    function GetSymbolTypeText(Index: Integer): string;
    { return the symbol class text for item 'Index'.  i.e. 'var', 'function', 'type', etc }
    function GetSymbolClassText(I: Integer): string;

    property SymbolClassText[I: Integer]: string read GetSymbolClassText;
    property SymbolTypeText[I: Integer]: string read GetSymbolTypeText;
    property SymbolText[I: Integer]: string read GetSymbolText;
    property SymbolFlags[I: Integer]: TOTAViewerSymbolFlags read GetViewerSymbolFlags;
    property SymbolVisibility[I: Integer]: TOTAViewerVisibilityFlags read GetViewerVisibilityFlags;
    property SymbolIsAbstract[I: Integer]: Boolean read GetSymbolIsAbstract;
    property SymbolIsReadWrite[I: Integer]: Boolean read GetSymbolIsReadWrite;
    property FuncDispatchFlags[I: Integer]: TOTAProcDispatchFlags read GetProcDispatchFlags;
    property SortOrder: TOTASortOrder read GetSortOrder write SetSortOrder;
    property Count: Integer read GetCount;
  end;

implementation

{ TCodeInsightSymbolList }

procedure TCodeInsightSymbolList.AddSymbol(const aText: string);
begin
  FStrings.Add(aText);
end;

procedure TCodeInsightSymbolList.AfterConstruction;
begin
  inherited;
  FStrings := TStringList.Create;
end;

procedure TCodeInsightSymbolList.Clear;
begin
  FStrings.Clear;
end;

destructor TCodeInsightSymbolList.Destroy;
begin
  FStrings.Free;
  inherited;
end;

function TCodeInsightSymbolList.FindIdent(const AnIdent: string): Integer;
begin
  Result := 0;
end;

function TCodeInsightSymbolList.FindSymIndex(const Ident: string;
  var Index: Integer): Boolean;
begin
  Index  := FStrings.IndexOf(Ident);
  Result := Index >= 0;
end;

function TCodeInsightSymbolList.GetCount: Integer;
begin
  Result := FStrings.Count;
end;

function TCodeInsightSymbolList.GetProcDispatchFlags(
  I: Integer): TOTAProcDispatchFlags;
begin
  Result := pdfNone;
end;

function TCodeInsightSymbolList.GetSortOrder: TOTASortOrder;
begin
  Result := soAlpha;
end;

function TCodeInsightSymbolList.GetSymbolClassText(I: Integer): string;
begin
  Result := 'My class';
end;

function TCodeInsightSymbolList.GetSymbolIsAbstract(I: Integer): Boolean;
begin
  Result := False;
end;

function TCodeInsightSymbolList.GetSymbolIsReadWrite(I: Integer): Boolean;
begin
  Result := False;
end;

function TCodeInsightSymbolList.GetSymbolText(Index: Integer): string;
begin
  Result := FStrings.Strings[Index];
end;

function TCodeInsightSymbolList.GetSymbolTypeText(Index: Integer): string;
begin
  Result := 'My type';
end;

function TCodeInsightSymbolList.GetViewerSymbolFlags(
  I: Integer): TOTAViewerSymbolFlags;
begin
  Result := vsfProcedure;
end;

function TCodeInsightSymbolList.GetViewerVisibilityFlags(
  I: Integer): TOTAViewerVisibilityFlags;
begin
  Result := 0;
end;

procedure TCodeInsightSymbolList.SetFilter(const FilterText: string);
begin
  //
end;

procedure TCodeInsightSymbolList.SetSortOrder(const Value: TOTASortOrder);
begin
  //
end;

end.
