unit fServerForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  uROClient, uROPoweredByRemObjectsButton, uROClientIntf, uROServer,
  uROJSONMessage, uROIndyHTTPServer, uROBaseHTTPServer,

  IdCustomHTTPServer, IdContext, uROServerIntf;

type
  TServerForm = class(TForm)
    RoPoweredByRemObjectsButton1: TRoPoweredByRemObjectsButton;
    ROMessage: TROJSONMessage;
    ROServer: TROIndyHTTPServer;
    procedure FormCreate(Sender: TObject);
  private
    ROHttpServerCommandGet,
    ROHttpServerCommandOther : TIdHTTPCommandEvent;
    procedure InternalServerCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure InternalServerCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
    { Public declarations }
  end;

var
  ServerForm: TServerForm;

implementation

uses uWebPages, DateUtils;


{$R *.dfm}

procedure TServerForm.FormCreate(Sender: TObject);
begin
  ROHttpServerCommandGet             := ROServer.IndyServer.OnCommandGet;
  ROServer.IndyServer.OnCommandGet   := InternalServerCommandGet;
  ROHttpServerCommandOther           := ROServer.IndyServer.OnCommandOther;
  ROServer.IndyServer.OnCommandOther := InternalServerCommandOther;

  ROServer.SendCrossOriginHeader := True;
  ROServer.Active := true;
end;

procedure TServerForm.InternalServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  AppData  : string;
  Filename : string;
begin
  // ExecuteWebPage looks to see if we have any registered event handlers that
  // wants to process the requested page.
  if not uWebPages.ExecuteWebPage(ARequestInfo.Document, AContext, ARequestInfo, AResponseInfo) then
  begin
    // There aren't any handlers registered for this request.
    // Look to see if there is a local file available that matches the
    // requested page. If so then we will serve that file out.
    AppData  := ExtractFilePath(ExtractFilePath(ParamStr(0))) + 'Project1\';
    Filename := ARequestInfo.Document;
    if Length(Filename) > 0 then
      if Filename[1] = '/' then
        Filename := Copy(Filename, 2, Length(Filename));

    FileName := AppData + Filename;
    if FileExists(Filename) then
    begin
      AResponseInfo.ContentType   := GetMimeType(Filename);
      AResponseInfo.ContentStream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);

      {set cache values}
      //<meta http-equiv="Cache-control" content="public">
      AResponseInfo.CacheControl  := 'public'; //, max-age=3600, must-revalidate';
      AResponseInfo.Expires       := IncDay(Date, 31);
      if GetFileVersion(Filename) <> Cardinal(-1) then
        AResponseInfo.LastModified  := FileDateToDateTime(GetFileVersion(Filename))
      else
        AResponseInfo.LastModified  := Now;
    end
    // Let RemObject's handle it from here
    else if Assigned(ROHttpServerCommandGet) then
      ROHttpServerCommandGet(AContext, ARequestInfo, AResponseInfo);
  end;
end;

procedure TServerForm.InternalServerCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  //original RO handling
  ROHttpServerCommandOther(AContext, ARequestInfo, AResponseInfo);

  //need to set extra allow-header
  if ROServer.SendCrossOriginHeader then
    AResponseInfo.CustomHeaders.Values['Access-Control-Allow-Headers'] := 'Origin, X-Requested-With, Content-Type';
end;

end.
