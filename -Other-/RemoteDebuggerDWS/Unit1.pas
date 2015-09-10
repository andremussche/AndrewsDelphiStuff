unit Unit1;

//https://developers.google.com/chrome-developer-tools/docs/protocol/tot/runtime

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  SynEdit, SynHighlighterDWS, SynEditHighlighter, SynHighlighterJScript,
  dwsJSFilter, dwsComp, dwsCompiler, dwsHtmlFilter, dwsExprs,
  dorWebsocket, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, superobject;

type
  TForm1 = class(TForm)
    SynJScriptSyn1: TSynJScriptSyn;
    SynDWSSyn1: TSynDWSSyn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    synedtDWS: TSynEdit;
    Panel1: TPanel;
    synedtJS: TSynEdit;
    btnCompileAndRun: TButton;
    tsRemoteDebug: TTabSheet;
    Memo1: TMemo;
    btnConnect: TButton;
    IdHTTP1: TIdHTTP;
    btnLoadDebugScript: TButton;
    tsRemoteJS: TTabSheet;
    Panel2: TPanel;
    Label1: TLabel;
    lbStack: TListBox;
    Label2: TLabel;
    lbScope: TListBox;
    Splitter1: TSplitter;
    Label3: TLabel;
    lbProperties: TListBox;
    Panel3: TPanel;
    btnPause: TButton;
    btnContinue: TButton;
    btnStepOver: TButton;
    btnStepInto: TButton;
    pgRemoteJS: TPageControl;
    tsRemoteJS_JS: TTabSheet;
    tsRemoteJS_DWS: TTabSheet;
    synedtRemoteJS: TSynEdit;
    synedtRemoteDWS: TSynEdit;
    tsEmbeddedChrome: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure btnCompileAndRunClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnLoadDebugScriptClick(Sender: TObject);
    procedure synedtRemoteJSDblClick(Sender: TObject);
    procedure lbStackDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbScopeDblClick(Sender: TObject);
    procedure btnContinueClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStepOverClick(Sender: TObject);
    procedure btnStepIntoClick(Sender: TObject);
    procedure synedtRemoteJSMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure synedtRemoteDWSMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FFilter: TdwsJSFilter;
    FHtmlFilter: TdwsHtmlFilter;
    FDWSMain: TDelphiWebScript;
    FDWSJS: TDelphiWebScript;
  private
    FWebS : IWebSocket;
    FseqNo: Integer;
    FScripts: TStrings;
    FStackList: TInterfaceList;
    FScopeList: TInterfaceList;
    procedure ConnectWebsocket(const aHost: string = 'localhost'; aPort: Integer = 9222);
    procedure HandleWSMessage(const aData: string);
    procedure HandleWSPaused(const aJSON: ISuperObject);
    procedure UpdateDebuggerState(aRunning: boolean);
    procedure ActiveStack(const aCallFrame: ISuperObject);
    procedure LoadScope(const aCallFrame: ISuperObject);
    procedure ActiveScope(const aScope: ISuperObject);

    procedure ShowHintForVar(const aVarName: string; aControl: TControl);

    procedure ws_EnableDebugger;
      procedure ws_PauseDebugger;
      procedure ws_ContinueDebugger;
      procedure ws_StepIntoDebugger;
      procedure ws_StepOverDebugger;
      procedure ws_SetBreakpoint(aLine: Integer);
      procedure ws_LoadScript(const aScriptID: string);
    procedure ws_GetObjectProperties(const aObjectId: string);
  public
  end;

var
  Form1: TForm1;

implementation

uses
  ShellAPI, StrUtils;

{$R *.dfm}

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  ConnectWebsocket();

  //start the debugger
  Inc(FseqNo);
  ws_EnableDebugger();

  tsRemoteDebug.TabVisible := True;
  PageControl1.ActivePage  := tsRemoteDebug;
end;

procedure TForm1.btnContinueClick(Sender: TObject);
begin
  ws_ContinueDebugger;
end;

procedure TForm1.btnLoadDebugScriptClick(Sender: TObject);
var
  sid: string;
begin
  sid := FScripts.Names[FScripts.Count-1];
  ws_LoadScript(sid);
end;

procedure TForm1.btnPauseClick(Sender: TObject);
begin
  ws_PauseDebugger;
end;

procedure TForm1.btnStepIntoClick(Sender: TObject);
begin
  ws_StepIntoDebugger;
end;

procedure TForm1.btnStepOverClick(Sender: TObject);
begin
  ws_StepOverDebugger;
end;

procedure TForm1.ConnectWebsocket(const aHost: string = 'localhost'; aPort: Integer = 9222);
var
  node, json: ISuperObject;
  strm: TMemoryStream;
begin
  //first we must connect via http
  strm := TMemoryStream.Create;
  try
    IdHTTP1.Get( Format('http://%s:%d/json',[aHost, aPort]), strm);
    //Memo1.Lines.LoadFromStream(strm);
    //Memo2.Clear;
    json := TSuperObject.ParseStream(strm, False);
    for node in json do
      Memo1.Lines.Add( node.S['title'] + '=' + node.S['webSocketDebuggerUrl'] );
  finally
    strm.Free;
  end;

  FScripts.Free;
  FScripts := TStringList.Create;

  //then connect via websocket
  if FWebS = nil then
    FWebS := TWebSocket.Create
  else
    FWebS.Close;

  tsRemoteDebug.TabVisible := True;

  //message handler (for processing results)
  FWebS.OnMessage :=
    procedure(const msg: string)
    begin
      HandleWSMessage(msg);
    end;
  //we assume we are the only page, so page 1
  FWebS.Open( Format('ws://%s:%d/devtools/page/1', [aHost, aPort]) );
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FHtmlFilter := TdwsHtmlFilter.Create(Self);
//    PatternClose = '?>'
//    PatternEval = '='
//    PatternOpen = '<?'

  FDWSMain := TDelphiWebScript.Create(Self);
  FDWSMain.Config.Filter := FHtmlFilter;
  FDWSMain.AddUnit(TdwsHTMLUnit.Create(Self));

  FDWSJS   := TDelphiWebScript.Create(Self);

  FFilter          := TdwsJSFilter.Create(Self);
  FFilter.Compiler := FDWSJS;

  FHtmlFilter.SubFilter := FFilter;

  PageControl1.ActivePageIndex := 0;
  pgRemoteJS.ActivePageIndex   := 0;
  synedtJS.Clear;
  synedtRemoteJS.Clear;
  Memo1.Clear;
  tsRemoteDebug.TabVisible   := False;
  tsRemoteJS.TabVisible      := False;
  btnLoadDebugScript.Enabled := False;

  FStackList := TInterfaceList.Create;
  FScopeList := TInterfaceList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FScopeList.Free;
  FStackList.Free;

  FHtmlFilter.Free;
  FDWSMain.Free;
  FDWSJS.Free;
  FFilter.Free;

  FWebS := nil;
  FScripts.Free;
end;

procedure TForm1.HandleWSMessage(const aData: string);
var
  json, result, params, propdescr: ISuperObject;
  resultarray: TSuperArray;
  sScript: string;
  sLog: string;
  i: Integer;
begin
  json := SO(aData);

  if json.S['error'] <> '' then
    MessageDlg(aData, mtError, [mbOK], 0);

  //Debugger.enable
  if json.S['method'] = 'Debugger.scriptParsed' then
  begin
    params := json.O['params'];
    if (params <> nil) and (params.S['url'] <> '') then
    begin
      Memo1.Lines.Add( params.S['scriptId'] + '=' + params.S['url'] );
      FScripts.Add   ( params.S['scriptId'] + '=' + params.S['url'] );

      btnLoadDebugScript.Enabled := True;
    end
    else
      Exit;  //skip empty url's -> come from "setInterval" or "setTimeout"
  end;

  //Debugger.getScriptSource
  result  := json.O['result'];
  if result <> nil then
  begin
    sScript := result.S['scriptSource'];
    if sScript <> '' then
    begin
      synedtRemoteJS.Lines.Text := sScript;

      tsRemoteJS.TabVisible     := True;
      PageControl1.ActivePage   := tsRemoteJS;
      UpdateDebuggerState(True{running});
    end;
  end;

  //Runtime.getProperties
  (*
    response: {
    "id": <number>,
    "error": <object>,
    "result": {
      "result": <array of PropertyDescriptor>
    }
    }
  *)
  if result <> nil then
  begin
    resultarray := result.N['result'].AsArray;
    if resultarray <> nil then
      for i := 0 to resultarray.Length - 1 do
      begin
        propdescr := resultarray.O[i];
        if propdescr = nil then Continue;

        lbProperties.Items.Add( Format('%s = %s (%s)', [propdescr.S['name'], propdescr.N['value'].S['value'], propdescr.AsJSon()]) );
      end;
  end;


  (* {"result":
        {"breakpointId":"22:13:0",
         "actualLocation":
           {"scriptId":"22",
            "lineNumber":13,
            "columnNumber":3}
           },
         "id":3} *)

  //Debugger.setBreakpoint
  if json.S['method'] = 'Debugger.paused' then
    HandleWSPaused(json);

  sLog := StringReplace(aData, #13, '', [rfReplaceAll]);
  sLog := StringReplace(sLog , #10, '', [rfReplaceAll]);
  Memo1.Lines.Add( FormatDateTime('hh:nn:ss:zzz', Now) + ' - ' + sLog);
end;

procedure TForm1.HandleWSPaused(const aJSON: ISuperObject);
var
  params, callFrame: ISuperObject;
  callFrames: TSuperArray;
  i: Integer;
  sFunc: string;
begin
  UpdateDebuggerState(False{paused});

  params := aJSON.O['params'];
  if params = nil then Exit;
  callFrames := params.N['callFrames'].AsArray;

  lbStack.Clear;
  FStackList.Clear;
  lbScope.Clear;
  FScopeList.Clear;
  lbProperties.Clear;

  for i := 0 to callFrames.Length - 1 do
  begin
    callFrame := callFrames.O[i];
    if callFrame = nil then Continue;

    sFunc := callFrame.S['functionName'];
    if sFunc = '' then
      sFunc :=  '(anonymous)';
    lbStack.Items.AddObject(sFunc, nil); //TObject(callFrame.Clone));
    FStackList.Add(callFrame.Clone);

    //use top of stack: goto line in editor etc
    if i = 0 then
    begin
      lbStack.ItemIndex := 0;
      ActiveStack(callFrame);
    end;
  end;

  (* {"method":"Debugger.paused",
      "params":
        {"callFrames":
           [{"callFrameId":"{\"ordinal\":0,\"injectedScriptId\":1}",
             "functionName":"Mandel",
             "location": {"scriptId":"22",
                          "lineNumber":13,
                          "columnNumber":3},
             "scopeChain":[{"object":{
                             "type":"object",
                             "objectId":"{\"injectedScriptId\":1,\"id\":1}",
                             "className":"Object",
                             "description":"Object"},
                           "type":"local"},
                          {"object":{
                             "type":"object",
                             "objectId":"{\"injectedScriptId\":1,\"id\":2}",
                             "className":"DOMWindow",
                             "description":"DOMWindow"},
                           "type":"global"}],
             "this":{
               "type":"object",
               "objectId":"{\"injectedScriptId\":1,\"id\":3}",
               "className":"DOMWindow",
               "description":"DOMWindow"}
            },
            {"callFrameId":"{\"ordinal\":1,\"injectedScriptId\":1}",
             "functionName":"calcAndShowMandel",
             "location":{"scriptId":"22",
                         "lineNumber":61,
                         "columnNumber":3},
             "scopeChain":[{"object":{
                              "type":"object",
                              "objectId":"{\"injectedScriptId\":1,\"id\":4}",
                              "className":"Object",
                              "description":"Object"},
                            "type":"local"},
                           {"object":{
                              "type":"object",
                              "objectId":"{\"injectedScriptId\":1,\"id\":5}",
                              "className":"DOMWindow",
                              "description":"DOMWindow"},
                            "type":"global"}],
             "this":{"type":"object",
                     "objectId":"{\"injectedScriptId\":1,\"id\":6}",
                     "className":"DOMWindow",
                     "description":"DOMWindow"}},
            {"callFrameId":"{\"ordinal\":2,\"injectedScriptId\":1}",
             "functionName":"",
             "location":{"scriptId":"137","lineNumber":0,"columnNumber":0},"scopeChain":[{"object":{"type":"object","objectId":"{\"injectedScriptId\":1,\"id\":7}","className":"DOMWindow","description":"DOMWindow"},"type":"global"}],"this":{"type":"object","objectId":"{\"injectedScriptId\":1,\"id\":8}","className":"DOMWindow","description":"DOMWindow"}}],
         "reason":"other"}}  *)
end;

procedure TForm1.lbScopeDblClick(Sender: TObject);
var
  scope: ISuperObject;
begin
  scope := FScopeList.Items[lbScope.ItemIndex] as ISuperObject;
  ActiveScope(scope);
end;

procedure TForm1.ActiveScope(const aScope: ISuperObject);
var
  obj: ISuperObject;
begin
  lbProperties.Clear;

  if aScope = nil then Exit;
  obj   := aScope.O['object'];
  if obj = nil then Exit;

  ws_GetObjectProperties(obj.S['objectId']);
end;

procedure TForm1.lbStackDblClick(Sender: TObject);
var
  callFrame: ISuperObject;
begin
  callFrame := FStackList.Items[lbStack.ItemIndex] as ISuperObject;
  ActiveStack(callFrame);
end;

procedure TForm1.ActiveStack(const aCallFrame: ISuperObject);
var
  location: ISuperObject;
  pos, posend: TBufferCoord;
  sLine: string;
  iLine,
  iOffset: Integer;
  i: Integer;
begin
  location  := aCallFrame.O['location'];
  if location = nil then Exit;
  //ws_LoadScript();

  iLine       := location.I['lineNumber'] - 1;
  synedtRemoteJS.GotoLineAndCenter(iLine);
  //synedtRemoteJS.GotoBookMark(1);
  pos.Line    := iLine;
  pos.Char    := 0;
  posend      := pos;
  sLine       := synedtRemoteJS.LineText;
  posend.Char := Length(sLine);
  synedtRemoteJS.SetCaretAndSelection(pos, pos, posend);

  // /*@14*/for(i=0;i<=498;i++) {
  if System.Pos('/*@', sLine) > 0 then
  begin
    pgRemoteJS.ActivePage := tsRemoteJS_DWS;

    iOffset     := 0;
    for i := 0 to synedtRemoteDWS.Lines.Count - 1 do
    begin
      if System.Pos('<%pas2js', synedtRemoteDWS.Lines[i]) > 0 then
      begin
        iOffset := i + 1;
        Break;
      end;
    end;

    synedtRemoteDWS.GotoLineAndCenter(iLine + iOffset);
    pos.Line    := iLine + iOffset;
    pos.Char    := 0;
    posend      := pos;
    sLine       := synedtRemoteDWS.LineText;
    posend.Char := Length(sLine) + 1;
    synedtRemoteDWS.SetCaretAndSelection(pos, pos, posend);
    synedtRemoteDWS.SetFocus;
  end
  else
  begin
    pgRemoteJS.ActivePage := tsRemoteJS_JS;
    synedtRemoteJS.SetFocus;
  end;

  LoadScope(aCallFrame);
end;

procedure TForm1.LoadScope(const aCallFrame: ISuperObject);
var
  scope, obj: ISuperObject;
  scopeChain: TSuperArray;
  j: Integer;
begin
  lbScope.Clear;
  FScopeList.Clear;
  lbProperties.Clear;

  scopeChain  := aCallFrame.N['scopeChain'].AsArray;
  if scopeChain <> nil then
    for j := 0 to scopeChain.Length - 1 do
    begin
      scope := scopeChain.O[j];
      if scope = nil then Continue;
      obj   := scope.O['object'];
      if obj = nil then Continue;

      lbScope.Items.AddObject( Format('%s: %s(%s) - %s', [scope.S['type'], obj.S['className'],
                                                          obj.S['description'], obj.S['objectId']]),
                               nil);//TObject(scope.Clone) );
      FScopeList.Add(scope.Clone);

      //load properties of first scope
      if j = 0 then
      begin
        lbScope.ItemIndex := 0;
        ActiveScope(scope);
      end;
    end;
end;

procedure TForm1.ShowHintForVar(const aVarName: string; aControl: TControl);
var
  ptMouse: TPoint;
  iItem: integer;
  i: Integer;
  s: string;
begin
  aControl.Hint     := '';
  aControl.ShowHint := False;

  if aVarName <> '' then
  begin
    s     := aVarName + ' =';
    iItem := -1;
    for i := 0 to lbProperties.Items.Count - 1 do
    begin
      if StartsText(s, lbProperties.Items.Strings[i]) then
      begin
        iItem := i;
        Break;
      end;
    end;
    if iItem < 0 then Exit;
    aControl.Hint     := lbProperties.Items.Strings[iItem];
    aControl.ShowHint := True;

    GetCursorPos(ptMouse);
    Application.ActivateHint(ptMouse);
    Application.HintPause     := 0;  //direct show
    Application.HintHidePause := 15 * 1000;  //show 15s
  end;
end;

procedure TForm1.synedtRemoteDWSMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  ShowHintForVar(synedtRemoteDWS.WordAtMouse, synedtRemoteDWS);
end;

procedure TForm1.synedtRemoteJSDblClick(Sender: TObject);
var
  ptMouse, ptClient: TPoint;
  iLine: Integer;
begin
  GetCursorPos(ptMouse);
  ptClient := synedtRemoteJS.ScreenToClient(ptMouse);
  if ptClient.X < synedtRemoteJS.Gutter.Width then
  begin
    iLine := synedtRemoteJS.DisplayToBufferPos(
                 synedtRemoteJS.PixelsToRowColumn(ptClient.X, ptClient.Y)
                 ).Line;
    synedtRemoteJS.SetBookMark(1, 0, iLine);
    ws_SetBreakpoint(iLine);
  end;
end;

procedure TForm1.synedtRemoteJSMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  ShowHintForVar(synedtRemoteJS.WordAtMouse, synedtRemoteJS);
end;

procedure TForm1.UpdateDebuggerState(aRunning: boolean);
begin
  btnPause.Enabled    := aRunning;
  btnContinue.Enabled := not aRunning;
  btnStepOver.Enabled := not aRunning;
  btnStepInto.Enabled := not aRunning;

  if aRunning then
  begin
    lbStack.Clear;
    lbScope.Clear;
    lbProperties.Clear;

    FStackList.Clear;
    FScopeList.Clear;
  end;
end;

procedure TForm1.ws_ContinueDebugger;
var s: string;
begin
  UpdateDebuggerState(True{running});

  s := '{"id":%d,' +
        '"method":"Debugger.resume"' +
       '}';
  s := Format(s, [FseqNo]);
  FWebS.Send(s);
  Inc(FseqNo);
end;

procedure TForm1.ws_EnableDebugger;
var
  s: string;
begin
//  FWebS.OnMessage :=
//    procedure(const msg: string)
//    var
//      json, params: ISuperObject;
//    begin
//      json := SO(msg);
//      if json.S['error'] <> '' then
//        ShowMessage(msg);
//
//      (*
//        "method": "Debugger.scriptParsed",
//        "params": {
//          "scriptId": <ScriptId>,
//          "url": <string>,
//          "startLine": <integer>,
//          "startColumn": <integer>,
//          "endLine": <integer>,
//          "endColumn": <integer>,
//          "isContentScript": <boolean>,
//          "sourceMapURL": <string>
//        }
//      *)
//
//      if json.S['method'] = 'Debugger.scriptParsed' then
//      begin
//        params   := json.N['params'];
//        if params.S['url'] <> '' then
//        begin
//          Memo1.Lines.Add( params.S['scriptId'] + '=' + params.S['url'] );
//          FScripts.Add   ( params.S['scriptId'] + '=' + params.S['url'] );
//        end;
//      end;
//    end;

  (*
  request: {
  "id": <number>,
  "method": "Debugger.enable"
  *)
  s := '{"id":%d,' +
        '"method":"Debugger.enable"' +
       '}';
  s := Format(s, [FseqNo]);
  FWebS.Send(s);
  Inc(FseqNo);
end;

procedure TForm1.ws_GetObjectProperties(const aObjectId: string);
var
  s, sobjid: string;
begin
  lbProperties.Clear;

  s := '{"id":%d,' +
        '"method":"Runtime.getProperties",' +
        '"params":{"objectId":"%s"' +
                  '}' +
       '}';
  sobjid := StringReplace(aObjectId, '"', '\"', [rfReplaceAll]);
  s := Format(s, [FseqNo, sobjid]);
  FWebS.Send(s);
  Inc(FseqNo);
end;

procedure TForm1.ws_LoadScript(const aScriptID: string);
var
  s, sid: string;
begin
  s := '{"id":%d,' +
        '"method":"Debugger.getScriptSource",' +
        '"params":{"scriptId":"%s"' +
                  '}' +
       '}';
  //if InputQuery('ScriptID', 'ScriptID: ', sid) then
  if sid = '' then
    sid := FScripts.Names[FScripts.Count-1]  //last known scriptid
  else
    sid := aScriptID;
  s := Format(s, [FseqNo, sid]);
  FWebS.Send(s);
  Inc(FseqNo);
end;

procedure TForm1.ws_PauseDebugger;
var s: string;
begin
  s := '{"id":%d,' +
        '"method":"Debugger.pause"' +
       '}';
  s := Format(s, [FseqNo]);
  FWebS.Send(s);
  Inc(FseqNo);
end;

procedure TForm1.ws_SetBreakpoint(aLine: Integer);
var
  s, sid: string;
begin
//  FWebS.OnMessage :=
//    procedure(const msg: string)
//    begin
//      Showmessage(msg);
//    end;

  (*
  request: {
  "id": <number>,
  "method": "Debugger.setBreakpoint",
  "params": {
    "location": <Location>,
    "condition": <string> } }
  *)
  s := '{"id":%d,' +
        '"method":"Debugger.setBreakpoint",' +
        '"params":{"location":{"lineNumber":%d,"scriptId":"%s"}' +
                 '}' +
       '}';
  //if not InputQuery('ScriptID', 'ScriptID: ', sid) then Exit;
  //if not InputQuery('lineNumber', 'lineNumber: ', sline) then Exit;
  sid := FScripts.Names[FScripts.Count-1];
  s   := Format(s, [FseqNo, aLine + 1, sid]);
  FWebS.Send(s);
  Inc(FseqNo);

  (*
  request: {
  "id": <number>,
  "method": "Debugger.setBreakpointsActive",
  "params": {
    "active": <boolean> } }
  *)
  s := '{"id":%d,' +
        '"method":"Debugger.setBreakpointsActive",' +
        '"params":{"active":true' +
                 '}' +
        '}';
  s := Format(s, [FseqNo]);
  FWebS.Send(s);
  Inc(FseqNo);
end;

procedure TForm1.ws_StepIntoDebugger;
var s: string;
begin
  UpdateDebuggerState(True{running});

  s := '{"id":%d,' +
        '"method":"Debugger.stepInto"' +
       '}';
  s := Format(s, [FseqNo]);
  FWebS.Send(s);
  Inc(FseqNo);
end;

procedure TForm1.ws_StepOverDebugger;
var s: string;
begin
  UpdateDebuggerState(True{running});

  s := '{"id":%d,' +
        '"method":"Debugger.stepOver"' +
       '}';
  s := Format(s, [FseqNo]);
  FWebS.Send(s);
  Inc(FseqNo);
end;

procedure TForm1.btnCompileAndRunClick(Sender: TObject);
var
  prog : IdwsProgram;
  exec : IdwsProgramExecution;
  sOutput : String;
  sUrl: string;
var
  h: THandle;
  Style, ExStyle: Cardinal;
begin
  synedtRemoteDWS.Text := synedtDWS.Text;

  prog := FDWSMain.Compile(synedtDWS.Lines.Text);
  if prog.Msgs.HasErrors then
  begin
//     MEOutput.Lines.Text := prog.Msgs.AsInfo;
//     PageControl1.ActivePageIndex:=2;
     MessageDlg(prog.Msgs.AsInfo, mtError, [mbOK], 0);
     Exit;
  end;

  //execute "print" script
  exec := prog.Execute;

  if exec.Msgs.Count>0 then
  begin
    MessageDlg(prog.Msgs.AsInfo, mtError, [mbOK], 0);
//    MEOutput.Lines.Text:=prog.Msgs.AsInfo;
//    PageControl1.ActivePageIndex:=2;
    Exit;
  end;

  soutput                      := exec.Result.ToString;
  synedtJS.Lines.Text          := soutput;
  PageControl1.ActivePageIndex := 1;

  synedtJS.Lines.SaveToFile('output.html');
  //Chromium.Browser.MainFrame.LoadString(output, 'test');
  //sUrl := '--app=file:///C:/-Andre-/-Projects-/dwscript/Demos/RemoteDebug/output.html';
  sUrl := '--app=file:///' + ExtractFilePath(Application.ExeName) + 'output.html' +
          ' --remote-debugging-port=9222 --user-data-dir=dwsremotetest';
  ShellExecute(self.WindowHandle, 'open', 'chrome.exe', PChar(sUrl), nil, SW_SHOWMINIMIZED);

  h := 0;
  while h = 0 do
  begin
    h := Windows.FindWindow(nil, 'DwsRemoteChrome');
    Sleep(100);
  end;

  Windows.ShowWindow(h, SW_NORMAL);
  Windows.SetParent(h, tsEmbeddedChrome.Handle);
  Windows.MoveWindow(h, 0, 0, Self.Width, Self.Height, False);

  Style   := GetWindowLong(h, GWL_STYLE);
  ExStyle := GetWindowLong(h, GWL_EXSTYLE);
  //Self.BorderStyle := bsNone;
  Style   := Style and not (WS_POPUP or WS_CAPTION or WS_BORDER or WS_THICKFRAME or WS_DLGFRAME or DS_MODALFRAME);
  ExStyle := ExStyle and not (WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE or WS_EX_TOOLWINDOW);
  SetWindowLong(h, GWL_STYLE, Style);
  SetWindowLong(h, GWL_EXSTYLE, ExStyle);

  PageControl1.ActivePage := tsEmbeddedChrome;
end;

end.
