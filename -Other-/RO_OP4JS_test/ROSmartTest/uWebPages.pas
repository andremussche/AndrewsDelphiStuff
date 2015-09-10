unit uWebPages;

interface

uses
  // Indy
  IdContext, IdCustomHTTPServer, IdHttpServer;

procedure RegisterWebPage(const AFileName: string; AEvent: TIdHTTPCommandEvent);
procedure UnRegisterWebPage(const AFileName: string);
function  ExecuteWebPage(const AFileName: string; AContext: TIdContext;
                         ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;
function  GetMimeType(const FileName: string): string;

{ For copy and paste, makes adding new pages easier.
    procedure IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
}

implementation

uses
  // Codegear
  SysUtils, Registry, Windows,
  Generics.Collections;

type
  PMethod = ^TMethod;

var
  Dictionary: TDictionary<string,PMethod>;

procedure RegisterWebPage(const AFileName: string; AEvent: TIdHTTPCommandEvent);

  function Add(const AMethod): PMethod;
  var
    m: TMethod;
    p: PMethod;
  begin
    m := TMethod(AMethod);
    New(p);
    p^.Data := m.Data;
    p^.Code := m.Code;
    Result := p;
  end;

begin
  if not Dictionary.ContainsKey(AFileName) then
    Dictionary.Add(AFileName, Add(AEvent));
end;

procedure UnRegisterWebPage(const AFileName: string);
var
  p: PMethod;
begin
  if Dictionary.ContainsKey(AFileName) then
  begin
    p := Dictionary.Items[AFileName];
    Dispose(p);
    Dictionary.Remove(AFileName);
  end;
end;

function ExecuteWebPage(const AFileName: string; AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): Boolean;

  procedure CallEvent(AEvent: TIdHTTPCommandEvent);
  begin
    if Assigned(Aevent) then
    begin
      try
        AEvent(AContext, ARequestInfo, AResponseInfo);
      except
        on e: exception do
          AResponseInfo.ContentText := AResponseInfo.ContentText +
                                       Format(#13'%s: %s',[e.ClassName, e.Message]);
      end;
    end;
  end;

var
  p: PMethod;
begin
  Result := Dictionary.ContainsKey(AFileName);
  if Result then
  begin
    p := Dictionary.Items[AFileName];
    CallEvent(TIdHTTPCommandEvent(p^));
  end;
end;

var
  _MimeTypes: TDictionary<string,string>;
  _MimeLock : TMultiReadExclusiveWriteSynchronizer;

function GetMimeType(const FileName: string): string;
var
  Key: string;
begin
  Result := 'text/html';
  Key    := ExtractFileExt(FileName);

  //search in cache first:
  _MimeLock.BeginRead;
  try
    if _MimeTypes.TryGetValue(Key, Result) then
      Exit;
  finally
    _MimeLock.EndRead;
  end;

  //else search in registry and add to cache
  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    if KeyExists(Key) then
    begin
      OpenKey(Key, False);
      Result := ReadString('Content Type');

      _MimeLock.BeginWrite;
      try
        _MimeTypes.AddOrSetValue(Key, Result);
      finally
        _MimeLock.EndWrite;
      end;

      CloseKey;
    end;
  finally
    Free;
  end;
end;

initialization
  Dictionary := TDictionary<string,PMethod>.Create;
  _MimeTypes := TDictionary<string,string>.Create;
  _MimeLock  := TMultiReadExclusiveWriteSynchronizer.Create;

finalization
  _MimeLock.BeginWrite;
  _MimeLock.Free;
  _MimeTypes.Free;
  Dictionary.Free;

end.
