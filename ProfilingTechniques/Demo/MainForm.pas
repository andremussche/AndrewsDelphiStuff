unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, DB, ADODB, uADStanIntf, uADStanOption,
  uADStanError, uADGUIxIntf, uADPhysIntf, uADStanDef, uADStanPool, uADStanAsync,
  uADPhysManager, uADGUIxFormsWait, uADStanParam, uADDatSManager, uADDAptIntf,
  uADDAptManager, uADPhysODBCBase, uADPhysMSSQL, uADCompDataSet, uADCompClient,
  uADCompGUIx, Inifiles, Generics.Collections;

type
  TfrmMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    prgForLoop2: TProgressBar;
    btnForLoop: TButton;
    btnForLoopProgress: TButton;
    lblForLoop: TLabel;
    lblForLoop2: TLabel;
    ADOConnection1: TADOConnection;
    ADOQuery1: TADOQuery;
    mmoQuery: TMemo;
    btnExecuteQuery: TButton;
    lblExecuteQuery: TLabel;
    Label1: TLabel;
    lblExecuteQuery2: TLabel;
    btnExecuteLowlevel: TButton;
    lblExecuteQueryLow: TLabel;
    btnExecuteQuery2: TButton;
    bntOpenOffice: TButton;
    TabSheet4: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    dbMain: TADConnection;
    ADGUIxWaitCursor1: TADGUIxWaitCursor;
    qryFire: TADQuery;
    ADPhysMSSQLDriverLink1: TADPhysMSSQLDriverLink;
    TabSheet5: TTabSheet;
    Button4: TButton;
    TabSheet6: TTabSheet;
    Button5: TButton;
    TabSheet7: TTabSheet;
    Button6: TButton;
    TabSheet8: TTabSheet;
    Button7: TButton;
    Memo1: TMemo;
    Button8: TButton;
    Button9: TButton;
    procedure btnForLoopClick(Sender: TObject);
    procedure btnForLoopProgressClick(Sender: TObject);
    procedure btnExecuteQueryClick(Sender: TObject);
    procedure btnExecuteQuery2Click(Sender: TObject);
    procedure btnExecuteLowlevelClick(Sender: TObject);
    procedure bntOpenOfficeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  DateUtils, UHojaCalc, _uAsmProfDllLoader;
  //StringConcatBench;

{$R *.dfm}

procedure TfrmMain.bntOpenOfficeClick(Sender: TObject);
var hcalc: THojaCalc;
  i: Integer;
begin
  if (Sender = nil) or not _uAsmProfDllLoader.IsProfilerDllLoaded then
  begin
    HCalc:= THojaCalc.create(thcOpenOffice, false); //OpenOffice doc if possible, please
    HCalc.FileName := ExtractFilePath(Application.ExeName) + 'testdoc'; //Needs a file name before you SaveDoc!
    //Change a cell value.
    for i := 1 to 1 * 1000 do
    begin
      IF HCalc.CellText[i,2] = '' THEN
        HCalc.CellText[i,2] := 'Hello world!';
    end;
    HCalc.Free;
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    bntOpenOfficeClick(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;
end;

procedure TfrmMain.btnExecuteLowlevelClick(Sender: TObject);
var
  i: Integer;
  tstart: TDateTime;
  data: _Recordset;
begin
  if (Sender = nil) or not _uAsmProfDllLoader.IsProfilerDllLoaded then
  begin
    ADOConnection1.Connected := True;
    tstart := Now;
    for i := 0 to 1000 do
    begin
      data := ADOConnection1.Execute(ADOQuery1.SQL.Text);
      if (data.RecordCount > 0) then
        data.Fields.Item[0].Value;
    end;
    lblExecuteQueryLow.Caption := Format('Duration: %dms',[MilliSecondsBetween(Now, tStart)]);
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    btnExecuteLowlevelClick(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;
end;

procedure TfrmMain.btnExecuteQuery2Click(Sender: TObject);
var
  i: Integer;
  tstart: TDateTime;
  data: _Recordset;
begin
  if (Sender = nil) or not _uAsmProfDllLoader.IsProfilerDllLoaded then
  begin
    ADOConnection1.Connected := True;
    tstart := Now;
  //  mmoQuery.Lines.BeginUpdate;
    try
      for i := 0 to 1000 do
      begin
        mmoQuery.Lines.Add(ADOQuery1.SQL.Text);
        data := ADOConnection1.Execute(ADOQuery1.SQL.Text);
        if (data.RecordCount > 0) then
          data.Fields.Item[0].Value;
      end;
    finally
  //    mmoQuery.Lines.EndUpdate;
    end;
    lblExecuteQuery2.Caption := Format('Duration: %dms',[MilliSecondsBetween(Now, tStart)]);
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    btnExecuteQuery2Click(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;
end;

procedure TfrmMain.btnExecuteQueryClick(Sender: TObject);
var
  i: Integer;
  tstart: TDateTime;
begin
  if (Sender = nil) or not _uAsmProfDllLoader.IsProfilerDllLoaded then
  begin
    ADOConnection1.Connected := True;
    tstart := Now;
    for i := 0 to 100 do
    begin
      ADOQuery1.Active := True;
      ADOQuery1.Active := False;
    end;
    lblExecuteQuery.Caption := Format('Duration: %dms',[MilliSecondsBetween(Now, tStart)]);
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    btnExecuteQueryClick(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;
end;

procedure TfrmMain.btnForLoopClick(Sender: TObject);
var
  i,j: Integer;
  tstart: TDateTime;
begin
  if (Sender = nil) or not _uAsmProfDllLoader.IsProfilerDllLoaded then
  begin
    tstart := Now;
    for i := 0 to 10 * 1000 * 1000 do
    begin
      j := i*2;
    end;
    if j > 0 then ;
    lblForLoop.Caption := Format('Duration: %dms',[MilliSecondsBetween(Now, tStart)]);
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    btnForLoopClick(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;
end;

procedure TfrmMain.btnForLoopProgressClick(Sender: TObject);
var
  i,j: Integer;
  tstart: TDateTime;
begin
  if (Sender = nil) or not _uAsmProfDllLoader.IsProfilerDllLoaded then
  begin
    tstart := Now;
    prgForLoop2.Max := 10 * 1000 * 1000;
    for i := 0 to 10 * 1000 * 1000 do
    begin
      j := i*2;
      //if i mod 1000 = 0 then      //update each 1000 items (e.g. remainder of 2000 / 1000 = 0)
      if i mod 100 = 0 then      //update each 100 items (e.g. remainder of 2000 / 100 = 0)
      begin
        prgForLoop2.Position := i;
        prgForLoop2.Update;
      end;
    end;
    if j > 0 then ;
    lblForLoop2.Caption := Format('Duration: %dms',[MilliSecondsBetween(Now, tStart)]);
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    btnForLoopProgressClick(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);

  procedure Sleep100;
  begin
    Sleep(100);
  end;

  procedure Sleep1000;
  var
    i: Integer;
  begin
    for i := 1 to 10 do
      Sleep100;
  end;

var
  i: Integer;
begin
  //trick to get Button1Click in profiler
  if (Sender = nil) or not _uAsmProfDllLoader.IsProfilerDllLoaded then
  begin
    for i := 1 to 10 do
      Sleep1000;
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    Button1Click(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  i: Integer;
  tstart: TDateTime;
begin
  if (Sender = nil) or not _uAsmProfDllLoader.IsProfilerDllLoaded then
  begin
    dbMain.Connected := True;
    qryFire.SQL.Text := ADOQuery1.SQL.Text;

    tstart := Now;
    for i := 0 to 1000 do
    begin
      qryFire.Active := True;
      qryFire.Active := False;
    end;
    lblExecuteQueryLow.Caption := Format('Duration: %dms',[MilliSecondsBetween(Now, tStart)]);
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    Button2Click(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
var
  i: Integer;
  tstart: TDateTime;
  data: variant;
begin
  if (Sender = nil) or not _uAsmProfDllLoader.IsProfilerDllLoaded then
  begin
    dbMain.Connected := True;
    tstart := Now;
    for i := 0 to 1000 do
    begin
      data := dbMain.ExecSQLScalar(ADOQuery1.SQL.Text);
    end;
    lblExecuteQueryLow.Caption := Format('Duration: %dms',[MilliSecondsBetween(Now, tStart)]);
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    Button3Click(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;
end;


function MiniDumpWriteDump(hProcess: THANDLE; ProcessId: DWORD; hFile: THANDLE; DumpType: DWORD; ExceptionParam: pointer; UserStreamParam: pointer; CallbackParam: pointer): BOOL; stdcall;
    external 'dbghelp.dll' name 'MiniDumpWriteDump';

const
  SE_DEBUG_NAME = 'SeDebugPrivilege' ;

procedure GetDebugPrivileges;
var
  hToken: THandle;
  tkp: TTokenPrivileges;
  retval: cardinal;
begin
  if (OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or  TOKEN_QUERY, hToken)) then
  begin
    LookupPrivilegeValue(nil, SE_DEBUG_NAME  , tkp.Privileges[0].Luid);
    tkp.PrivilegeCount := 1;
    tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    AdjustTokenPrivileges(hToken, false, tkp, 0, nil, retval);
  end;
end;

const
  MiniDumpNormal         = $0000;
  MiniDumpWithDataSegs   = $0001;
  MiniDumpWithFullMemory = $0002;
  MiniDumpWithHandleData = $0004;
  MiniDumpFilterMemory   = $0008;
  MiniDumpScanMemory     = $0010;
  MiniDumpWithUnloadedModules            = $0020;
  MiniDumpWithIndirectlyReferencedMemory = $0040;
  MiniDumpFilterModulePaths              = $0080;
  MiniDumpWithProcessThreadData          = $0100;
  MiniDumpWithPrivateReadWriteMemory     = $0200;
  MiniDumpWithThreadInfo                 = $1000;

procedure MakeMinidump(aPID:integer; const aOutputFile: string = '');
var
  hProc,
  hFile: THandle;
  sFile: string;
begin
  GetDebugPrivileges;

  if aOutputFile = '' then
    sFile  := ExtractFileName(Application.ExeName) + '.dmp'
  else
    sFile  := aOutputFile;
  hProc  := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, aPID);
  hFile  := CreateFile(PChar(sFile),
                      GENERIC_WRITE,FILE_SHARE_WRITE,nil,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,0);
  try
    if not MiniDumpWriteDump(
        hProc,
        aPID,
        hFile,
        MiniDumpNormal or MiniDumpWithHandleData or
        MiniDumpWithProcessThreadData or MiniDumpWithThreadInfo,
        nil, nil ,nil) then
    begin
      if not MiniDumpWriteDump(
                 hProc, aPID, hFile,
                 MiniDumpNormal,
                 nil, nil ,nil)
      then
        RaiseLastOSError;
    end;
  finally
    FileClose(hfile);
  end;
end;

procedure TfrmMain.Button4Click(Sender: TObject);
const
  C_GUI_TIME_OUT_SECONDS: Integer = 5;
begin
  //start watchdog thread
  TThread.CreateAnonymousThread(
    procedure
    var
      iRes  : DWORD_PTR;
    begin
      repeat
        if SendMessageTimeOut(Application.MainForm.Handle,
                      WM_NULL, 0, 0,
                      SMTO_NORMAL, // or SMTO_NOTIMEOUTIFNOTHUNG or SMTO_ERRORONEXIT, // or SMTO_ABORTIFHUNG otherwise always after 5s?
                      //wait number of seconds...
                      C_GUI_TIME_OUT_SECONDS * 1000, @iRes) = 0 then
        begin
          if Application.Terminated then Exit;
          if (GetLastError = ERROR_TIMEOUT) then
          begin
            MakeMinidump(GetCurrentProcessId);
            Windows.MessageBox(0, 'Minidump made', 'Application hangs', MB_OK);
            Exit;
          end;
        end;
      until 1 = 2;
    end).Start;

  //let mainthread hang...
  Sleep(C_GUI_TIME_OUT_SECONDS * 1000 * 2);
end;

procedure TfrmMain.Button5Click(Sender: TObject);
begin
  //load dll + show GUI
  if _uAsmProfDllLoader.LoadProfilerDll then
    _uAsmProfDllLoader.ShowProfileForm;
end;

procedure TfrmMain.Button6Click(Sender: TObject);
var
  str: TStringList;
  dict: TDictionary<string,Integer>;

  procedure _HashTest1;
  var
    i: Integer;
  begin
    str := THashedStringList.Create;
    try
      //str.Sorted := True;
      for i := 0 to 10 * 1000 do
      begin
        str.AddObject(IntToStr(i), TObject(i));
        //search 1 per 1000
        if (i mod 1000) = 0 then
          str.IndexOf(IntToStr(i-1));
      end;
    finally
      str.Free;
    end;
  end;

  procedure _HashTest2;
  var
    i: Integer;
  begin
    dict := TDictionary<string,Integer>.Create;
    try
      for i := 0 to 10 * 1000 do
      begin
        dict.Add(IntToStr(i), i);
        //search 1 per 1000
        if (i mod 1000) = 0 then
          dict.ContainsKey(IntToStr(i-1));
      end;
    finally
      dict.Free;
    end;
  end;


begin
  if (Sender = nil) or not _uAsmProfDllLoader.IsProfilerDllLoaded then
  begin
    _HashTest1;
    _HashTest2;
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    Button6Click(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;
end;

procedure TfrmMain.Button7Click(Sender: TObject);
begin
(*  StringConcatBench.NB_THREADS := 2;
  Windows.SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);  //high prio for fair results

  Memo1.Lines.Add( 'UseStringBuilder: ' + StringConcatBench.MeasureThreaded( StringConcatBench.UseStringBuilder ) );
  Sleep(1000);   //insert pause in proces explorer cpu graph
  Memo1.Lines.Add( 'UseStringBuilderPreallocated: ' + StringConcatBench.MeasureThreaded( StringConcatBench.UseStringBuilderPreallocated ) );
  Sleep(1000);
  Memo1.Lines.Add( 'UseStringStream: ' + StringConcatBench.MeasureThreaded( StringConcatBench.UseStringStream ) );
  Sleep(1000);
  Memo1.Lines.Add( 'UseStringStream2: ' + StringConcatBench.MeasureThreaded( StringConcatBench.UseStringStream2 ) );
  Sleep(1000);
  Memo1.Lines.Add( 'UseConcatTrivial: ' + StringConcatBench.MeasureThreaded( StringConcatBench.UseConcatTrivial ) );
  Sleep(1000);
  Memo1.Lines.Add( 'UseConcatPreAllocated: ' + StringConcatBench.MeasureThreaded( StringConcatBench.UseConcatPreAllocated ) );
  Sleep(1000);
  Memo1.Lines.Add( 'UseConcatFormat: ' + StringConcatBench.MeasureThreaded( StringConcatBench.UseConcatFormat ) );
  Sleep(1000);
  Memo1.Lines.Add( 'UseWOBS: ' + StringConcatBench.MeasureThreaded( StringConcatBench.UseWOBS ) );
  Sleep(1000);
  Memo1.Lines.Add( 'UseTextWriter: ' + StringConcatBench.MeasureThreaded( StringConcatBench.UseTextWriter ) );
  Sleep(1000);
  Memo1.Lines.Add( 'UseStringData: ' + StringConcatBench.MeasureThreaded( StringConcatBench.UseStringData ) );
  Sleep(1000);

  Windows.SetPriorityClass(GetCurrentProcess, NORMAL_PRIORITY_CLASS);     *)
end;

procedure TfrmMain.Button8Click(Sender: TObject);
begin
(*  if Sender = nil then
  begin
    StringConcatBench.Measure( StringConcatBench.UseStringBuilder );
    StringConcatBench.Measure( StringConcatBench.UseStringBuilderPreallocated );
    StringConcatBench.Measure( StringConcatBench.UseStringStream );
    StringConcatBench.Measure( StringConcatBench.UseStringStream2 );
    StringConcatBench.Measure( StringConcatBench.UseConcatTrivial );
    StringConcatBench.Measure( StringConcatBench.UseConcatPreAllocated );
    StringConcatBench.Measure( StringConcatBench.UseConcatFormat );
    StringConcatBench.Measure( StringConcatBench.UseWOBS );
    StringConcatBench.Measure( StringConcatBench.UseTextWriter );
    StringConcatBench.Measure( StringConcatBench.UseStringData );
  end
  else
  begin
    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StartProfiler(True);

    Button8Click(nil);

    if _uAsmProfDllLoader.IsProfilerDllLoaded then
      _uAsmProfDllLoader.StopProfiler;
  end;        *)
end;

procedure TfrmMain.Button9Click(Sender: TObject);
begin
(*  //run for long time, so we can start/stop sampling profiler
  //StringConcatBench.NB := 1000;
  StringConcatBench.MEASURE_ITER := 10 * 1000 * 1000;

  //normal strings
  Memo1.Lines.Add( 'UseConcatTrivial: ' + StringConcatBench.Measure( StringConcatBench.UseConcatTrivial ) );
  Application.ProcessMessages;

  //dws
  Memo1.Lines.Add( 'UseWOBS: ' + StringConcatBench.Measure( StringConcatBench.UseWOBS ) );
  Application.ProcessMessages;

  //mormot
  Memo1.Lines.Add( 'UseTextWriter: ' + StringConcatBench.Measure( StringConcatBench.UseTextWriter ) );
  Application.ProcessMessages;  *)
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

end.
