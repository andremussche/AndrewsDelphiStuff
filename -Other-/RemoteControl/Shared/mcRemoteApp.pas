unit mcRemoteApp;

interface

uses
  mcRemoteControlTypes;

type
  TfrmMain = class;
  TfrmTest = class;

  TApplication = class(TBaseRemoteObject)
  private
    class var FRemoteApp: TApplication;
  published
    frmMain : TfrmMain;
    frmTest : TfrmTest;
  end;

  TfrmMain = class(TForm)
  published
    btnShowModal: TButton;
    btnShowFloating: TButton;
  end;

  TfrmTestBase = class(TForm)
  published
    //ActionList1: TActionList;
    //Panel1: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    actOk: TAction;
    actCancel: TAction;
  end;

  TframTest = class;

  TfrmTest = class(TfrmTestBase)
  published
    framTest1: TframTest;
  end;

  TframBase = class(TFrame)
  end;

  TframTest = class(TframBase)
  published
    //Panel1: TPanel;
    Button1: TButton;
    Edit1: TEdit;
    //Label1: TLabel;
  end;

  function RemoteApp: TApplication;

implementation

function RemoteApp: TApplication;
begin
  if TApplication.FRemoteApp = nil then
    TApplication.FRemoteApp := TApplication.Create;
  Result := TApplication.FRemoteApp;
end;


end.
