unit Node_Static;

interface

uses
  NodeJS.http;

type
  TNodeStaticOptions = class;

  JNodeStaticServer = class external
    procedure serve(aRequest: JServerRequest; aResponse: JServerResponse);
  end;

  TNodeStaticServer = class
  public
    class function Create(aPath: string; aOptions: TNodeStaticOptions = nil): JNodeStaticServer;
  end;

  TNodeStaticOptions = class
    cache: Integer;
  end;

implementation

uses
  NodeJS.Core;

{ TNodeStaticServer }

class function TNodeStaticServer.Create(aPath: String; aOptions: TNodeStaticOptions = nil): JNodeStaticServer;
begin
  var nodestatic := require('node-static');
  var server := null;
  asm
    @server = new ((@nodestatic).Server)(aPath, aOptions);
  end;
  Result := JNodeStaticServer(server);
end;

end.
