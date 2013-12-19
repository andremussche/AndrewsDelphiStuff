unit socketioclient;

interface

uses 
  w3system;

{$R 'file:socketio.js'}

type
  JSocketIO = class;
  JFunction = procedure;

  JSocketIOSocketFunction = procedure(aSocket: JSocketIO);
  JSocketIODataFunction   = procedure(aData: variant);

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
    //function emit(ev: variant; data: array of variant): JSocketIO; overload;
    function emit(ev: variant; data: array of variant; fn: JSocketIODataFunction): JSocketIO;
    function &on(ns: string; fn: JSocketIODataFunction): JSocketIO;
    property json: variant;
    property log: variant;
    property volatile: variant;
    property broadcast: variant;
  end;

  JSocketIOClient = class external "Object"
  public
    function connect(aAddress: string): JSocketIO;
  end;

  function socketio: JSocketIOClient;

  procedure alert; external;

implementation

function socketio: JSocketIOClient;
begin
  asm
    @Result = io;
  end;
end;


end.
