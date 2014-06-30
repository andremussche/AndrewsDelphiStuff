{******************************************************************************}
{*                                                                            *}
{* CompileInterceptor IDE Plugin                                              *}
{*                                                                            *}
{* (C) 2006-2007 Andreas Hausladen                                            *}
{*                                                                            *}
{******************************************************************************}

unit InterceptIntf;

{$IFDEF UNICODE}
  {$STRINGCHECKS OFF}
{$ENDIF UNICODE}

interface

type
  TCompileInterceptOptions = type Cardinal;

const
  CIO_ALTERFILES       = $0001;  // The interceptor supports the AlterFile() method
  CIO_VIRTUALFILES     = $0002;  // The interceptor supports the VirtrualFile() method
  CIO_INSPECTFILENAMES = $0004;  // The interceptor supports the InspectFilename() method
  CIO_ALTERMESSAGES    = $0008;  // The interceptor supports the AlterMessage() method
  CIO_COMPILEPROJECTS  = $0010;  // The interceptor supports the CompileProject() method

type
  TMsgKind = (mkHint, mkWarning, mkError, mkFatal, mkInfo);

  PUtf8Char = PAnsiChar;

  TInspectFileMode = (ifmOpen, ifmCreate);

  IWideString = interface
    ['{3B33C7A5-63F4-4700-A6D5-4072D707536C}']
    function GetValue: PWideChar;
    procedure SetValue(P: PWideChar);
    procedure SetString(P: PWideChar; Len: Integer); // Len in Bytes
    function GetLength: Integer; // Len in Bytes

    property Value: PWideChar read GetValue write SetValue;
  end;

  IVirtualStream = interface
    ['{6BBD7B93-9402-4534-ADD3-A3D287FD70E9}']
    function Seek(Offset: Integer; Origin: Integer): Integer; stdcall;
    function Read(var Buffer; Size: Integer): Integer; stdcall;
    procedure FileStatus(out FileDate: Integer; out FileSize: Integer); stdcall;
  end;

  ICompileInterceptor = interface
    ['{186D90CD-598B-4162-8E03-0BF8298A0826}']
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

  ICompileInterceptorServices = interface
    ['{CA696A1B-77EF-4EEB-9F22-9EE6E53B2B76}']
      { RegisterInterceptor() registers a compile interceptor. }
    function RegisterInterceptor(Interceptor: ICompileInterceptor): Integer; stdcall;
      { UnregisterInterceptor() removes the compile interceptor that is assigned
        to the specified ID. }
    procedure UnregisterInterceptor(Id: Integer); stdcall;

      { Returns the file content. The editor content is returned if the file is
        opened in the editor. }
    function GetFileContent(Filename: PWideChar): IVirtualStream; stdcall;
  end;

  TWideStringAdapter = class(TInterfacedObject, IWideString)
  private
    FValue: string;
  protected
    { IWideString }
    function GetLength: Integer;
    function GetValue: PWideChar;
    procedure SetString(P: PWideChar; Len: Integer);
    procedure SetValue(P: PWideChar);
  public
    constructor Create(const AValue: string);
  end;

  TGetCompileInterceptorServices = function: ICompileInterceptorServices; stdcall;
//     external 'CompileInterceptorW.dll' name 'GetCompileInterceptorServices';

const
  CompileInterceptorEntryPoint = 'CompileInterceptorEntry';

type
  TDoneProc = procedure; stdcall;
  TCompileInterceptorEntryPoint = procedure(const CompileInterceptorServices: ICompileInterceptorServices; var DoneProc: TDoneProc); stdcall;

implementation

{ TWideStringAdapter }

constructor TWideStringAdapter.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

function TWideStringAdapter.GetLength: Integer;
begin
  Result := Length(FValue);
end;

function TWideStringAdapter.GetValue: PWideChar;
begin
  Result := PWideChar(FValue);
end;

procedure TWideStringAdapter.SetString(P: PWideChar; Len: Integer);
begin
  System.SetString(FValue, P, Len);
end;

procedure TWideStringAdapter.SetValue(P: PWideChar);
begin
  FValue := P;
end;

end.
