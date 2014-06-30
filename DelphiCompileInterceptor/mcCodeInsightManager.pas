unit mcCodeInsightManager;

interface

uses
  SysUtils,
  ToolsAPI;

type
  TCustomCodeInsightManager = class(TInterfacedObject,
                                    IOTACodeInsightManager)
  public
    function GetOptionSetName: string;

    { returns a description of the language which we handle }
    function GetName: string;
    { returns a unique IDString to the services module }
    function GetIDString: string;
    { returns whether we should be able to be invoked or not }
    function GetEnabled: Boolean;
    { sets the active state to Value so this manager may be turned off }
    procedure SetEnabled(Value: Boolean);
    {
      returns a charset used to get the token at the current editor position.  This is
      used for retrieving the seed text when code completion is invoked as well as
      retrieving the token from the editor when we are typing for look ahead.
      The PreValidating parameter should be used to add special tokens to the charset for retrieval
      from the editor.  For instance, C++ might add ['.', '-', '>'] to the returned charset
      when it is prevalidating.
    }
    function EditorTokenValidChars(PreValidating: Boolean): TSysCharSet;
    {
      the implementor should set Allow to True if it wishes to be invoked for the key 'Key'.
      'Key' is the key which the user pressed to invoke code completion.
      There are four special values to 'Key' when invoked by the code insight timer.

      They are as follows:
        #0 : Code completion was requested.
        #1 : Parameter insight was requested.
        #2 : A browse was requested.
        #3 : a symbol hint was requested.
    }
    procedure AllowCodeInsight(var Allow: Boolean; const Key: Char);
    {
      the implementor should return true if it wishes to allow the token 'str' to be
      a valid code point for Code Insight.
    }
    function PreValidateCodeInsight(const Str: string): Boolean;
    { returns whether the symbol at index 'Index' as browseable in the Code completion viewer }
    function IsViewerBrowsable(Index: Integer): Boolean;
    { returns whether the code completion viewer allows multi-select }
    function GetMultiSelect: Boolean;
    { returns the symbol list to the caller }
    procedure GetSymbolList(out SymbolList: IOTACodeInsightSymbolList);
    {
      determines whether or not the key 'Key' which was entered into the editor should close
      the code completion viewer or not (set CloseViewer to True or False depending on your choice).
      Also, the implementor should inform the manager whether or not it should accept the symbol
      at the currently selected index/indices.
    }
    procedure OnEditorKey(Key: Char; var CloseViewer: Boolean; var Accept: Boolean);
    { returns true if this manager should handle this file }
    function HandlesFile(const AFileName: string): Boolean;
    { returns the longest symbol class text for measurement for the viewer.  i.e.  'constructor' is longer than 'var' }
    function GetLongestItem: string;
    { returns a parameter list to the manager }
    procedure GetParameterList(out ParameterList: IOTACodeInsightParameterList);
    {
      given key 'AChar' which was entered into the editor and the current element (atComment, atIdentifier, etc),
      return how code insight should be invoked and which type of invocation it should be.

      As an example, GetCodeInsightType() might be implemented something like this:
      ...
      begin
        InvokeType := itManual;
        if not ((AElement = atString) and (AChar <> #1)) and not (AElement = atComment) then
        begin
          case AChar of
            #0: CodeInsightType := citCodeInsight;
            #1: CodeInsightType := citParameterCodeInsight;
            #2: CodeInsightType := citBrowseCodeInsight;
            #3: CodeInsightType := citHintCodeInsight;
            '.':
            begin
              CodeInsightType := citCodeInsight;
              InvokeType := itTimer;
            end;
            '(':
            begin
              CodeInsightType := citParameterCodeInsight;
              InvokeType := itTimer;
            end;
          end;
        end
        else
          CodeInsightType := citNone;
      end;
    }
    procedure GetCodeInsightType(AChar: Char; AElement: Integer; out CodeInsightType: TOTACodeInsightType;
      out InvokeType: TOTAInvokeType);
    {
      returns true if invocation was successful.  HowInvoked informs the implementor whether
      it was invoked via timer, manual, etc...  Str is the text to seed to viewer with and
      is used for the initial filtering in the viewer.
    }
    function InvokeCodeCompletion(HowInvoked: TOTAInvokeType; var Str: string): Boolean;
    {
      returns true if invocation was successful.  HowInvoked informs the implementor whether
      it was invoked via timer, manual, etc...  SelectedIndex is the index of the current parameter
      for the method/proc.
    }
    function InvokeParameterCodeInsight(HowInvoked: TOTAInvokeType; var SelectedIndex: Integer): Boolean;
    {
      tells the manager where it should anchor the parameter hint window.
      A default value (EdPos) is provided for the implementor to change if they so wish.
    }
    procedure ParameterCodeInsightAnchorPos(var EdPos: TOTAEditPos);
    {
      returns the index of the parameter which should be highlighted based upon EdPos.
      This is used to reduce extra codeinsight invocations as an implementor might
      store off the editor positions of parameters on the first invocation.
      return a -1 if you want to be reinvoked.
    }
    function ParameterCodeInsightParamIndex(EdPos: TOTAEditPos): Integer;
    { return the hint string for the position in the editor (HintLine/HintCol are the editor coordinates) }
    function GetHintText(HintLine, HintCol: Integer): string;
    {
      return a FileName and LineNumber for the symbol which is requested to be browsed to.
      if Index > -1 then it is an index into the symbol list and the browse was requested
      by a user clicking in the code completion viewer.
      return false if you'd like to inform the user that the requested operation failed otherwise return true.
      if you wish to fail by not informing the user, set AFileName = '' and ALineNum = 0.
      if Index is -1, you should use the global CodeInsightServices() and request the EditView from it.
      This should be able to give you any information you require.
    }
    function GotoDefinition(out AFileName: string; out ALineNum: Integer; Index: Integer = -1): Boolean;
    {
      called when the code completion is completed.  Accepted is true if the user has requested
      the item hinted to them in the viewer otherwise Accepted is false.
      DisplayParams should be set to true if the implementor would like to be requeried
      for parameter invocation.  It is up to the implementor to insert the text into the editor.
      One way might be to use CodeInsightServices.InsertText(StrToInsert, ShouldReplace);
      Another might be to acquire the EditView from CodeInsightServices.GetEditView() and do
      the insertion yourself.
    }
    procedure Done(Accepted: Boolean; out DisplayParams: Boolean);
    property Name: string read GetName;
    property MultiSelect: Boolean read GetMultiSelect;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

implementation

uses
  DbugIntf, Dialogs, mcCodeInsideSymbols, StrUtils;

{ TCustomCodeInsightManager }

procedure TCustomCodeInsightManager.AllowCodeInsight(var Allow: Boolean;
  const Key: Char);
begin
  Allow := True;
end;

procedure TCustomCodeInsightManager.Done(Accepted: Boolean;
  out DisplayParams: Boolean);
begin
  if not Accepted then Exit;
  DisplayParams := False;

  (BorlandIDEServices as IOTACodeInsightServices).InsertText('%message%', True);

  DbugIntf.SendDebugEx(Format('INTERCEPT: Done', []),
                       mtInformation );
end;

function TCustomCodeInsightManager.EditorTokenValidChars(
  PreValidating: Boolean): TSysCharSet;
begin
  Result := ['%','m','e','s','a','g'];
end;

procedure TCustomCodeInsightManager.GetCodeInsightType(AChar: Char;
  AElement: Integer; out CodeInsightType: TOTACodeInsightType;
  out InvokeType: TOTAInvokeType);
begin
  InvokeType := itManual;
  if not ((AElement = atString) and (AChar <> #1)) and not (AElement = atComment) then
  begin
    case AChar of
      #0: CodeInsightType := citCodeInsight;
      #1: CodeInsightType := citParameterCodeInsight;
      #2: CodeInsightType := citBrowseCodeInsight;
      #3: CodeInsightType := citHintCodeInsight;
      '.':
      begin
        CodeInsightType := citCodeInsight;
        InvokeType := itTimer;
      end;
      '(':
      begin
        CodeInsightType := citParameterCodeInsight;
        InvokeType := itTimer;
      end;
    end;
  end
  else
    CodeInsightType := citNone;
end;

function TCustomCodeInsightManager.GetEnabled: Boolean;
begin
  Result := True;
end;

function TCustomCodeInsightManager.GetHintText(HintLine,
  HintCol: Integer): string;
begin
  Result := 'Custom Hint text';
end;

function TCustomCodeInsightManager.GetIDString: string;
begin
  Result := 'My Custom Code Insight';
end;

function TCustomCodeInsightManager.GetLongestItem: string;
begin
  Result := 'abcdefghijklmnopqrstuvw';
end;

function TCustomCodeInsightManager.GetMultiSelect: Boolean;
begin
  Result := False;
end;

function TCustomCodeInsightManager.GetName: string;
begin
  Result := 'Delphi';
end;

function TCustomCodeInsightManager.GetOptionSetName: string;
begin
  Result := 'My Custom Options';
end;

procedure TCustomCodeInsightManager.GetParameterList(
  out ParameterList: IOTACodeInsightParameterList);
begin
  ParameterList := nil;

  DbugIntf.SendDebugEx(Format('INTERCEPT: GetParameterList', []),
                       mtInformation );
end;

procedure TCustomCodeInsightManager.GetSymbolList(
  out SymbolList: IOTACodeInsightSymbolList);
var
  symbols: TCodeInsightSymbolList;
begin
  symbols    := TCodeInsightSymbolList.Create;
  SymbolList := symbols;

  symbols.AddSymbol('message');

  DbugIntf.SendDebugEx(Format('INTERCEPT: GetSymbolList', []),
                       mtInformation );
end;

function TCustomCodeInsightManager.GotoDefinition(out AFileName: string;
  out ALineNum: Integer; Index: Integer): Boolean;
begin
  Result := False;  //not supported
  DbugIntf.SendDebugEx(Format('INTERCEPT: GotoDefinition', []),
                       mtInformation );
end;

function TCustomCodeInsightManager.HandlesFile(
  const AFileName: string): Boolean;
begin
  //Result := ExtractFileName(AFileName) = 'Unit7.pas';
  Result := StartsText('my', ExtractFileName(AFileName));
end;

function TCustomCodeInsightManager.InvokeCodeCompletion(
  HowInvoked: TOTAInvokeType; var Str: string): Boolean;
begin
  Result := True;

  DbugIntf.SendDebugEx(Format('INTERCEPT: InvokeCodeCompletion(%s)', [str]),
                       mtInformation );
end;

function TCustomCodeInsightManager.InvokeParameterCodeInsight(
  HowInvoked: TOTAInvokeType; var SelectedIndex: Integer): Boolean;
begin
  Result := False;
  DbugIntf.SendDebugEx(Format('INTERCEPT: InvokeParameterCodeInsight', []),
                       mtInformation );
end;

function TCustomCodeInsightManager.IsViewerBrowsable(Index: Integer): Boolean;
begin
  Result := False;
  DbugIntf.SendDebugEx(Format('INTERCEPT: IsViewerBrowsable', []),
                       mtInformation );
end;

procedure TCustomCodeInsightManager.OnEditorKey(Key: Char; var CloseViewer,
  Accept: Boolean);
begin
  CloseViewer := False;
  Accept      := True;
  DbugIntf.SendDebugEx(Format('INTERCEPT: OnEditorKey(%s)', [Key]),
                       mtInformation );
end;

procedure TCustomCodeInsightManager.ParameterCodeInsightAnchorPos(
  var EdPos: TOTAEditPos);
begin
  DbugIntf.SendDebugEx(Format('INTERCEPT: ParameterCodeInsightAnchorPos', []),
                       mtInformation );
end;

function TCustomCodeInsightManager.ParameterCodeInsightParamIndex(
  EdPos: TOTAEditPos): Integer;
begin
  Result := 0;
  DbugIntf.SendDebugEx(Format('INTERCEPT: ParameterCodeInsightParamIndex', []),
                       mtInformation );
end;

function TCustomCodeInsightManager.PreValidateCodeInsight(
  const Str: string): Boolean;
begin
  Result := True;

  DbugIntf.SendDebugEx(Format('INTERCEPT: PreValidateCodeInsight(%s)', [str]),
                       mtInformation );
end;

procedure TCustomCodeInsightManager.SetEnabled(Value: Boolean);
begin
  DbugIntf.SendDebugEx(Format('INTERCEPT: SetEnabled(%s)', [BoolToStr(Value, True)]),
                       mtInformation );
end;

var
  idx: Integer;

initialization
  (BorlandIDEServices as IOTACodeInsightServices).AddCodeInsightManager( TCustomCodeInsightManager.Create );

finalization
  (BorlandIDEServices as IOTACodeInsightServices).RemoveCodeInsightManager(idx);

end.
