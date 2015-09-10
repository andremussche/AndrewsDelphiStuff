unit fServerForm;

//http://wiki.remobjects.com/wiki/Calling_RemObjects_SDK_Servers_from_JavaScript#Tips_.26_Tricks

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  uROClient, uROPoweredByRemObjectsButton, uROClientIntf, uROServer,
  uROJSONMessage, uROIndyHTTPServer, uROBaseHTTPServer;

type
  TServerForm = class(TForm)
    RoPoweredByRemObjectsButton1: TRoPoweredByRemObjectsButton;
    ROMessage: TROJSONMessage;
    ROServer: TROIndyHTTPServer;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ServerForm: TServerForm;

implementation


{$R *.dfm}

procedure TServerForm.FormCreate(Sender: TObject);
begin
  ROServer.Active := true;
end;

end.
