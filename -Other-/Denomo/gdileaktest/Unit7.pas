unit Unit7;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdvOfficePager;

type
  TForm7 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

uses
  RuntimeDllMain, DenomoMemHook,
  DenomoConfig, DenomoUtils,
  DenomoHandleHook, DenomoHost, CodeHook;

{$R *.dfm}

var
  EnumHookIndexData: TEnumHookIndexData;

function HandleHookEnumCallback(AData: PEnumHookIndexData; ANode: PHandleBinaryTreeNode): Boolean;
var
  i: Integer;
  debuginfo: TDebugSourceLocationInfo;
  sUnit, sProc: string;
  p: pointer;
begin
  Form7.Memo1.Lines.Add('==========================');
  Form7.Memo1.Lines.Add(ANode.AllocatorProcName);
  Form7.Memo1.Lines.Add('--------------------------');
  for i := 0 to HandleStackTraceDepth - 1 do
  begin
    p := Pointer(ANode.StackTrace[i]);
    if p = nil then Continue;

    DenomoHost.DllService.GetDebugSourceLocationInfo( p, @debuginfo);
    sUnit := debuginfo.UnitName.Chars;
    sProc := debuginfo.ProcedureName.Chars;
    Form7.Memo1.Lines.Add( Format('%s - %s (%d)', [sUnit, sProc, debuginfo.LineNumber]) );
  end;
end;

procedure TForm7.Button1Click(Sender: TObject);
begin
  DenomoHost.DllService := GetDllService;
//  DenomoHost.DllService := CodeHook.GetCodeHookIntf;
  DenomoMemHook.InitLeakMemHook;
//  DenomoHandleHook.InitHandleHook;

  with tbitmap.Create do
  begin
    LoadFromFile('..\..\..\RES\close.bmp');
//    Free;
  end;

  ReportToLogFileWhenCheckOnExit := True;

  FillChar(EnumHookIndexData, sizeof(EnumHookIndexData), 0);
  HandleHookManager.EnumHookIndex( HandleHookEnumCallback, @EnumHookIndexData);
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
end;

end.
