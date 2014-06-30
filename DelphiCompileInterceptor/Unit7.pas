unit Unit7;

interface

uses
  InterceptIntf,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm7 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

uses
  DbugIntf, TypInfo;

{$R *.dfm}

var
 CompileInterceptorServices: TGetCompileInterceptorServices;

procedure TForm7.Button1Click(Sender: TObject);
var
  hfile: THandle;
begin
  hfile := LoadLibrary('c:\Users\Public\AppData\Roaming\DDevExtensions\CompileInterceptorW.dll');
  CompileInterceptorServices := GetProcAddress(hfile, 'GetCompileInterceptorServices');

//  CompileInterceptorServices.RegisterInterceptor( TTestInterceptor.Create );
end;

procedure TForm7.Button2Click(Sender: TObject);
begin
  %$custom$.$custom$.
  test
  %$custom$%
  Sleep(0);
end;

end.
