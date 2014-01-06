unit NodeJS.Core;

interface

{forward type declarations}
type
  //JFunction= class(JObject);
  JFunction = procedure(data: variant);
  Jchild_process= class(JObject);
  JError= class(JObject);
  JDate= class(JObject);

type
  JNodeProcess = class;
  JNodeBuffer = class;

function Process: JNodeProcess;

function Global: variant;

type
  Jconsole = class external "console"
  public
    procedure log(data: variant); overload;
    procedure log(data: array of variant); overload;
    procedure info(data: array of variant);
    procedure error(data: array of variant);
    procedure warn(data: array of variant);
    procedure dir(obj: variant);
    procedure timeEnd(label: string);
    procedure trace(label: string);
    procedure assert(expression: variant; message: array of string);
  end;

function Console: Jconsole;
function __Filename: string;
function __Dirname: string;


type
  TsetTimeout_callback_ = procedure ();

function setTimeout(callback: TsetTimeout_callback_;ms: integer): variant;external;
procedure clearTimeout(timeoutId: variant);external;

type
  TsetInterval_callback_ = procedure ();

function setInterval(callback: TsetInterval_callback_;ms: integer): variant;external;
procedure clearInterval(intervalId: variant);external;

type
  Jrequire = class external "require"
  public
    function resolve(): string;
    property cache: variant;
    property extensions: variant;
  end;

function Require(id: string): variant; external 'require';
function Require_Obj: Jrequire;


type
  Jmodule = class external "module"
  public
    function require(id: string): variant;
    property exports: variant;
    property id: string;
    property filename: string;
    property loaded: boolean;
    property parent: variant;
    property children: array of variant;
  end;

function Module_Obj: Jmodule;

function Exports: variant;


type
  JSlowBuffer = class external "SlowBuffer"
  public
    function &new(str: string; encoding: string): JNodeBuffer;overload;
    function &new(size: integer): JNodeBuffer;overload;
    function &new(&array: array of variant): JNodeBuffer;overload;
    function isBuffer(obj: variant): boolean;
    function byteLength(string_: string; encoding: string): integer;overload;
    function concat(list: array of JNodeBuffer; totalLength: integer): JNodeBuffer;overload;
    property prototype: JNodeBuffer;
  end;

function SlowBuffer_Obj: JSlowBuffer;


type
  JBuffer = class external "Buffer"
  public
    function &new(str: string; encoding: string): JNodeBuffer;overload;
    function &new(size: integer): JNodeBuffer;overload;
    function &new(&array: array of variant): JNodeBuffer;overload;
    function isBuffer(obj: variant): boolean;
    function byteLength(string_: string; encoding: string): integer;overload;
    function concat(list: array of JNodeBuffer; totalLength: integer): JNodeBuffer;overload;
    property prototype: JNodeBuffer;
  end;

function Buffer_Obj: JBuffer;


type
  TEventEmitter_listeners_result_object = class;

type
  JEventEmitter = class external "Object"
  public
    procedure addListener(event: string; listener: JFunction);
    procedure on(event: string; listener: JFunction);
    procedure once(event: string; listener: JFunction);
    procedure removeListener(event: string; listener: JFunction);
    procedure removeAllListener(event: string);
    procedure setMaxListeners(n: integer);
    function listeners(event: string): array of TEventEmitter_listeners_result_object;
    procedure emit(event: string; arg1: variant);overload;
    procedure emit(event: string; arg1: variant;arg2: variant);overload;
  end;
  TEventEmitter_listeners_result_object = class
    property &Function: variant;
  end;
type
  JWritableStream = class external(JEventEmitter)
  public
    function write(str: string; encoding: string): boolean;overload;
    function write(str: string; encoding: string;fd: string): boolean;overload;
    function write(buffer: JNodeBuffer): boolean;overload;
    procedure &end();overload;
    procedure &end(str: string; enconding: string);overload;
    procedure &end(buffer: JNodeBuffer);overload;
    procedure destroy();
    procedure destroySoon();
    property writable: boolean;
  end;

type
  TReadableStream_pipe_options_object = class;

type
  JReadableStream = class external(JEventEmitter)
  public
    procedure setEncoding(encoding: string);
    procedure pause();
    procedure resume();
    procedure destroy();
    procedure pipe(destination: JWritableStream; options: TReadableStream_pipe_options_object);overload;
    property readable: boolean;
  end;
  TReadableStream_pipe_options_object = class
    property &end: boolean;
  end;

type
  TNodeProcess_versions_object = class;
  TNodeProcess_config_object = class;
  TNodeProcess_config_object_target_defaults_object = class;
  TNodeProcess_config_object_variables_object = class;
  TNodeProcess_memoryUsage_result_object = class;

type
  JNodeProcess = class external(JEventEmitter)
  public
    procedure abort();
    procedure chdir(directory: string);
    procedure cwd();
    procedure &exit(code: integer);overload;
    function getgid(): integer;
    procedure setgid(id: integer);overload;
    procedure setgid(id: string);overload;
    function getuid(): integer;
    procedure setuid(id: integer);overload;
    procedure setuid(id: string);overload;
    procedure kill(pid: integer; signal: string);overload;
    function memoryUsage(): TNodeProcess_memoryUsage_result_object;
    procedure nextTick(callback: JFunction);
    function umask(mask: integer): integer;overload;
    function uptime(): integer;
    function hrtime(): array of integer;
    property stdout: JWritableStream;
    property stderr: JWritableStream;
    property stdin: JReadableStream;
    property argv: array of string;
    property execPath: string;
    property env: variant;
    property version: string;
    property versions: TNodeProcess_versions_object;
    property config: TNodeProcess_config_object;
    property pid: integer;
    property title: string;
    property arch: string;
    property platform: string;
  end;
  TNodeProcess_versions_object = class
    property http_parser: string;
    property node: string;
    property v8: string;
    property ares: string;
    property uv: string;
    property zlib: string;
    property openssl: string;
  end;
  TNodeProcess_config_object = class
    property target_defaults: TNodeProcess_config_object_target_defaults_object;
    property variables: TNodeProcess_config_object_variables_object;
  end;
  TNodeProcess_config_object_target_defaults_object = class
    property cflags: array of variant;
    property default_configuration: string;
    property defines: array of string;
    property include_dirs: array of string;
    property libraries: array of string;
  end;
  TNodeProcess_config_object_variables_object = class
    property clang: integer;
    property host_arch: string;
    property node_install_npm: boolean;
    property node_install_waf: boolean;
    property node_prefix: string;
    property node_shared_openssl: boolean;
    property node_shared_v8: boolean;
    property node_shared_zlib: boolean;
    property node_use_dtrace: boolean;
    property node_use_etw: boolean;
    property node_use_openssl: boolean;
    property target_arch: string;
    property v8_no_strict_aliasing: integer;
    property v8_use_snapshot: boolean;
    property visibility: string;
  end;
  TNodeProcess_memoryUsage_result_object = class
    property rss: integer;
    property heapTotal: variant;
    property number_: variant;
    property heapUsed: integer;
  end;
type
  JNodeBuffer = class external "Object"
  public
    function  GetItems(index: integer): integer; external array;
    procedure SetItems(index: integer; value: integer); external array;
    property Items[index: integer]: integer read GetItems write SetItems; default;
    function write(string_: string; offset: integer): integer;overload;
    function write(string_: string; offset: integer;length_: integer): integer;overload;
    function write(string_: string; offset: integer;length_: integer;encoding: string): integer;overload;
    function toString(encoding: string): string;overload;
    function toString(encoding: string;start: integer): string;overload;
    function toString(encoding: string;start: integer;&end: integer): string;overload;
    procedure copy(targetBuffer: JNodeBuffer; targetStart: integer);overload;
    procedure copy(targetBuffer: JNodeBuffer; targetStart: integer;sourceStart: integer);overload;
    procedure copy(targetBuffer: JNodeBuffer; targetStart: integer;sourceStart: integer;sourceEnd: integer);overload;
    function slice(start: integer): JNodeBuffer;overload;
    function slice(start: integer;&end: integer): JNodeBuffer;overload;
    function readUInt8(offset: integer; noAsset: boolean): integer;overload;
    function readUInt16LE(offset: integer; noAssert: boolean): integer;overload;
    function readUInt16BE(offset: integer; noAssert: boolean): integer;overload;
    function readUInt32LE(offset: integer; noAssert: boolean): integer;overload;
    function readUInt32BE(offset: integer; noAssert: boolean): integer;overload;
    function readInt8(offset: integer; noAssert: boolean): integer;overload;
    function readInt16LE(offset: integer; noAssert: boolean): integer;overload;
    function readInt16BE(offset: integer; noAssert: boolean): integer;overload;
    function readInt32LE(offset: integer; noAssert: boolean): integer;overload;
    function readInt32BE(offset: integer; noAssert: boolean): integer;overload;
    function readFloatLE(offset: integer; noAssert: boolean): integer;overload;
    function readFloatBE(offset: integer; noAssert: boolean): integer;overload;
    function readDoubleLE(offset: integer; noAssert: boolean): integer;overload;
    function readDoubleBE(offset: integer; noAssert: boolean): integer;overload;
    procedure writeUInt8(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeUInt16LE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeUInt16BE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeUInt32LE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeUInt32BE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeInt8(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeInt16LE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeInt16BE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeInt32LE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeInt32BE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeFloatLE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeFloatBE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeDoubleLE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure writeDoubleBE(value: integer; offset: integer; noAssert: boolean);overload;
    procedure fill(value: variant; offset: integer);overload;
    procedure fill(value: variant; offset: integer;&end: integer);overload;
    property length: integer;
    property INSPECT_MAX_BYTES: integer;
  end;


implementation

function Process: JNodeProcess;
begin
  asm @Result = process; end;
end;

function Global: variant;
begin
  asm @Result = global; end;
end;

function Console: Jconsole;
begin
  asm @Result = console; end;
end;

function __Filename: string;
begin
  asm @Result = __filename; end;
end;

function __Dirname: string;
begin
  asm @Result = __dirname; end;
end;

function Require_Obj: Jrequire;
begin
  asm @Result = require; end;
end;

function Module_Obj: Jmodule;
begin
  asm @Result = module; end;
end;

function Exports: variant;
begin
  asm @Result = exports; end;
end;

function SlowBuffer_Obj: JSlowBuffer;
begin
  asm @Result = SlowBuffer; end;
end;

function Buffer_Obj: JBuffer;
begin
  asm @Result = Buffer; end;
end;

end.

