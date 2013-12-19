unit socket.io;

interface

uses
  NodeJS.Core, NodeJS.http,
  socket.io.Core;

type
  JSocketIO = class;
  JSocketNamespace = class;
  JSocketManager = class;

  JSocketIOSocketFunction = procedure(aSocket: JSocketIO);
  JSocketIODataFunction   = procedure(aData: variant);
  JSocketIODataFnFunction = procedure(aData: variant; fn: JSocketIODataFunction);

  JSocketIO = class external "Object"
  public
    function &in(room: string): JSocketIO;
    function &to(room: string): JSocketIO;
    function join(name: string; fn: JFunction): JSocketIO;
    function unjoin(name: string; fn: JFunction): JSocketIO;
    function &set(key: string; value: variant; fn: JFunction): JSocketIO;
    function get(key: string; value: variant; fn: JFunction): JSocketIO;
    function has(key: string; fn: JFunction): JSocketIO;
    function del(key: string; fn: JFunction): JSocketIO;
    function disconnect(): JSocketIO;
    function send(data: variant; fn: JFunction): JSocketIO;
    function emit(ev: variant; data: array of variant): JSocketIO;
    function &on(ns: string; fn: JSocketIODataFnFunction): JSocketIO;
    property json: variant;
    property log: variant;
    property volatile: variant;
    property broadcast: variant;
  end;
  JSocketNamespace = class external "Object"
  public
    function clients(room: string): array of JSocketIO;
    function &in(room: string): JSocketNamespace;
    function &on(evt: string; fn: JSocketIOSocketFunction): JSocketNamespace;
    function &to(room: string): JSocketNamespace;
    function &except(id: variant): JSocketNamespace;
    function send(data: variant): variant;
    function emit(ev: variant; data: array of variant): JSocketIO;
    function socket(sid: variant; readable: boolean): JSocketIO;
    procedure authorization(fn: JFunction);
    property log: variant;
    property store: variant;
    property json: variant;
    property volatile: variant;
  end;
  JSocketManager = class external "Object"
  public
    function get(key: variant): variant;
    function set(key: variant; value: variant): JSocketManager;
    function enable(key: variant): JSocketManager;
    function disable(key: variant): JSocketManager;
    function enabled(key: variant): boolean;
    function disabled(key: variant): boolean;
    function configure(env: string; fn: JFunction): JSocketManager;overload;
    function configure(fn: JFunction): JSocketManager;overload;
    function &of(nsp: string): JSocketNamespace;
    function &on(ns: string; fn: JFunction): JSocketManager;
    property sockets: JSocketNamespace;
  end;

  Jsocket_io_Exports = class external
  public
    //function listen(server: JServer; options: variant; fn: JFunction): JSocketManager;overload;external;
    //function listen(server: JServer; fn: JFunction): JSocketManager;overload;external;
    function listen(server: JServer): JSocketManager;overload;external;
    //function listen(port: JNumber): JSocketManager;overload;external;
  end;

  function socketio: Jsocket_io_Exports;

implementation

uses
  Nodejs.core;

function socketio: Jsocket_io_Exports;
begin
  Result := Jsocket_io_Exports( require('socket.io') );
end;

end.

