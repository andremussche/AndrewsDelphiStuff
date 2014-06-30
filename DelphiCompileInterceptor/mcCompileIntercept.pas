unit mcCompileIntercept;

interface

uses
  Classes,
  InterceptIntf;

type
  TTestInterceptor = class(TInterfacedObject,
                           ICompileInterceptor)
  public
    {ICompileInterceptor}
      { GetOptions() returns the interceptor's options. }
    function GetOptions: TCompileInterceptOptions; stdcall;

      { GetVirtualFile() is called when the compiler wants to open a file. If
        the returned value is not NIL the compiler will operate on the virtual
        stream. In this case AlterFile is not called. CIO_VIRTUALFILE must be
        set. }
    function GetVirtualFile(Filename: PWideChar): IVirtualStream; stdcall;

      { AlterFile() is called when the file is no virtual file and CIO_ALTERFILE
        is set. FileDate is obsolete and always 0. }
    function AlterFile(Filename: PWideChar; Content: PByte; FileDate, FileSize: Integer): IVirtualStream; stdcall;

      { InspectFilename() is called when a file is opened or created and
        CIO_INSPECTFILENAMES is set. }
    procedure InspectFilename(Filename: PWideChar; FileMode: TInspectFileMode); stdcall;

       { AlterMessage() is called when the compiler wants to display a message.
         The method must return True if it has changed one of the parameters. }
    function AlterMessage(IsCompilerMessage: Boolean; var MsgKind: TMsgKind; var Code: Integer;
      const Filename: IWideString; var Line, Column: Integer; const Msg: IWideString): Boolean; stdcall;

      { CompileProject is called before the specified project is compiled. }
    procedure CompileProject(ProjectFilename, UnitPaths, SourcePaths, DcuOutputDir: PWideChar;
      IsCodeInsight: Boolean; var Cancel: Boolean); stdcall;
  end;

  TTestStream = class(TInterfacedObject,
                      IVirtualStream)
  protected
    FFileName: string;
    FFileDate: Integer;
    FStream: TMemoryStream;
    FStrData: TStrings;

    //my custom code!
    procedure ChangeCustomPascal;
  public
    constructor Create(const aFileName: string);
    destructor  Destroy; override;
    procedure   AfterConstruction;override;

    {IVirtualStream}
    function Seek(Offset: Integer; Origin: Integer): Integer; stdcall;
    function Read(var Buffer; Size: Integer): Integer; stdcall;
    procedure FileStatus(out FileDate: Integer; out FileSize: Integer); stdcall;
  end;

implementation

uses
  DbugIntf,
  SysUtils, Dialogs, TypInfo, Windows, StrUtils;

var
  CompileInterceptorServices: TGetCompileInterceptorServices;

{ TTestInterceptor }

function TTestInterceptor.AlterFile(Filename: PWideChar; Content: PByte;
  FileDate, FileSize: Integer): IVirtualStream;
begin
  Result := nil;

  try
    DbugIntf.SendDebugEx(Format('INTERCEPT: AlterFile( Filename="%s", Content=%p, FileDate=%d, FileSize=%d )',
                                [string(Filename), Content, FileDate, FileSize]),
                         mtInformation );
  except on e:Exception do
    DbugIntf.SendDebugEx(Format('INTERCEPT: AlterFile error: %s: %s',
                                [e.ClassName, e.Message]),
                         mtError );
  end;
end;

function TTestInterceptor.AlterMessage(IsCompilerMessage: Boolean;
  var MsgKind: TMsgKind; var Code: Integer; const Filename: IWideString;
  var Line, Column: Integer; const Msg: IWideString): Boolean;
begin
  Result := False;

  try
    DbugIntf.SendDebugEx(Format('INTERCEPT: AlterMessage( IsCompilerMessage=%s, MsgKind=%s, Code=%d, Filename=%s, Line=%d, Column=%d, Msg="%s" )',
                                [BoolToStr(IsCompilerMessage, True),
                                 TypInfo.GetEnumName(TypeInfo(TMsgKind), Ord(MsgKind)),
                                 Code, string(Filename.Value),
                                 Line, Column, string(Msg.Value)]),
                         mtInformation );
  except on e:Exception do
    DbugIntf.SendDebugEx(Format('INTERCEPT: AlterMessage error: %s: %s',
                                [e.ClassName, e.Message]),
                         mtError );
  end;
end;

procedure TTestInterceptor.CompileProject(ProjectFilename, UnitPaths,
  SourcePaths, DcuOutputDir: PWideChar; IsCodeInsight: Boolean;
  var Cancel: Boolean);
begin
  Cancel := False;

  try
    DbugIntf.SendDebugEx(Format('INTERCEPT: CompileProject( ProjectFilename="%s", UnitPaths="%s", SourcePaths="%s", DcuOutputDir="%s", IsCodeInsight=%s )',
                                [string(ProjectFilename), string(UnitPaths), string(SourcePaths), string(DcuOutputDir), BoolToStr(IsCodeInsight, True)]),
                         mtInformation );
  except on e:Exception do
    DbugIntf.SendDebugEx(Format('INTERCEPT: CompileProject error: %s: %s',
                                [e.ClassName, e.Message]),
                         mtError );
  end;
end;

function TTestInterceptor.GetOptions: TCompileInterceptOptions;
begin
  Result :=
    CIO_ALTERFILES       or  // The interceptor supports the AlterFile() method
    CIO_VIRTUALFILES     or  // The interceptor supports the VirtrualFile() method
    CIO_INSPECTFILENAMES or  // The interceptor supports the InspectFilename() method
    CIO_ALTERMESSAGES    or  // The interceptor supports the AlterMessage() method
    CIO_COMPILEPROJECTS;     // The interceptor supports the CompileProject() method

  DbugIntf.SendDebugEx(Format('INTERCEPT: GetOptions', []),
                       mtInformation );
end;

function TTestInterceptor.GetVirtualFile(Filename: PWideChar): IVirtualStream;
begin
  Result := nil;

  try
    DbugIntf.SendDebugEx(Format('INTERCEPT: GetVirtualFile(Filename="%s")',
                                [string(Filename)]),
                         mtInformation );

    if StartsText('my', ExtractFileName(Filename)) then
//    if ExtractFileName(Filename) = 'Unit7.pas' then
//    if Filename = 'Unit7.pas' then
    begin
      Result := TTestStream.Create(Filename);
      DbugIntf.SendDebugEx('INTERCEPT: GetVirtualFile: custom file created',
                           mtInformation );
    end;
  except on e:Exception do
    DbugIntf.SendDebugEx(Format('INTERCEPT: GetVirtualFile error: %s: %s',
                                [e.ClassName, e.Message]),
                         mtError );
  end;
end;

procedure TTestInterceptor.InspectFilename(Filename: PWideChar;
  FileMode: TInspectFileMode);
begin
  try
    DbugIntf.SendDebugEx(Format('INTERCEPT: InspectFilename(Filename="%s", FileMode=%s)',
                                [string(Filename),
                                 TypInfo.GetEnumName(TypeInfo(TInspectFileMode), Ord(FileMode))]),
                         mtInformation );
  except on e:Exception do
    DbugIntf.SendDebugEx(Format('INTERCEPT: InspectFilename error: %s: %s',
                                [e.ClassName, e.Message]),
                         mtError );
  end;
end;

{ TTestStream }

procedure TTestStream.AfterConstruction;
begin
  inherited;
end;

procedure TTestStream.ChangeCustomPascal;
var
  sData, sCustom: string;
  iPos: Integer;
begin
  sData := FStrData.Text;
  iPos  := Pos('%message%', sData);
  if iPos <= 0 then Exit;

  sCustom := 'MessageDlg(''test from file: %s'', mtInformation, [mbOK], 0);';
  sCustom := Format(sCustom, [Self.FFileName]);
  sData   := StringReplace(sData, '%message%', sCustom,[]);

  DbugIntf.SendDebugEx('INTERCEPT: ChangeCustomPascal: custom message added',
                       mtInformation );

  FStrData.Text := sData;
  FStream.Clear;
  FStrData.SaveToStream(FStream);
end;

constructor TTestStream.Create(const aFileName: string);
begin
  inherited Create;

  FFileName := aFileName;
  FFileDate := DateTimeToFileDate(Now);

  FStream   := TMemoryStream.Create;
  FStream.LoadFromFile(FFileName);

  FStrData := TStringList.Create;
  FStrData.LoadFromStream(FStream);
  ChangeCustomPascal;

  FStream.Position := 0;
end;

destructor TTestStream.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TTestStream.FileStatus(out FileDate, FileSize: Integer);
begin
  FileDate := FFileDate;
  FileSize := FStream.Size;
end;

function TTestStream.Read(var Buffer; Size: Integer): Integer;
begin
  Result := FStream.Read(Buffer, Size);
end;

function TTestStream.Seek(Offset, Origin: Integer): Integer;
begin
  Result := FStream.Seek(Offset, Origin);
end;

var
  hfile: THandle;
  iRegIdx: Integer = -1;

initialization
  try
    hfile := LoadLibrary('CompileInterceptorW.dll');//'c:\Users\Public\AppData\Roaming\DDevExtensions\CompileInterceptorW.dll');
    if hfile <> 0 then
      CompileInterceptorServices := GetProcAddress(hfile, 'GetCompileInterceptorServices')
    else
      CompileInterceptorServices := nil;

    if Assigned(CompileInterceptorServices) then
      iRegIdx := CompileInterceptorServices.RegisterInterceptor( TTestInterceptor.Create );
  except on e:Exception do
    DbugIntf.SendDebugEx(Format('INTERCEPT: initialization error: %s: %s',
                                [e.ClassName, e.Message]),
                         mtError );
  end;

finalization
  if Assigned(CompileInterceptorServices) then
    CompileInterceptorServices.UnregisterInterceptor( iRegIdx );
  CompileInterceptorServices := nil;
  FreeLibrary(hfile);

end.
