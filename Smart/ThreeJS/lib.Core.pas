unit lib.Core;

interface

type
  JMouseEvent = class;
  JMouseWheelEvent = class;

  JRegExp = class;
  JElement = class;
  JSVGDocument = class;
  JNode = class;
  JMSScriptHost = class;
  JTextRange = class;
  JSVGAnimatedAngle = class;
  JSVGAnimatedTransformList = class;
  JHTMLTableCaptionElement = class;
  JSVGAnimatedLengthList = class;
  JWindow = class;
  JSVGAnimatedPreserveAspectRatio = class;
  JMSSiteModeEvent = class;
  JStyleSheetPageList = class;
  JHTMLCollection = class;
  JEventTarget = class;
  JSVGAngle = class;
  JKeyboardEvent = class;
  JDocument = class;
  JMessageEvent = class;
  JSVGElement = class;
  JMSCSSRuleList = class;
  JSVGTransformList = class;
  JSVGAnimatedLength = class;
  JEventListener = class;
  JFocusEvent = class;
  JRange = class;
  JSVGPoint = class;
  JSVGAnimatedNumberList = class;
  JSVGSVGElement = class;
  JStorage = class;
  JTextRangeCollection = class;
  JDocumentType = class;
  JDragEvent = class;
  JHTMLTableSectionElement = class;
  JPerformanceTiming = class;
  JSVGStringList = class;
  JSVGLength = class;
  JMSEventObj = class;
  JHTMLCanvasElement = class;
  JLocation = class;
  JSVGTransform = class;
  JUIEvent = class;
  JSVGNumber = class;
  JText = class;
  JSVGAnimatedRect = class;
  JPositionError = class;
  JMSNamespaceInfoCollection = class;
  JStyleSheetList = class;
  JGeolocation = class;
  JSVGRect = class;
  JHistory = class;
  JSVGPathSegCurvetoCubicAbs = class;
  JSVGPathSegCurvetoQuadraticAbs = class;
  JCSSRule = class;
  JSVGPathSegLinetoAbs = class;
  JBeforeUnloadEvent = class;
  JSVGMatrix = class;
  JSVGUseElement = class;
  JEvent = class;
  JImageData = class;
  JHTMLDocument = class;
  JSVGAnimatedEnumeration = class;
  JNamedNodeMap = class;
  JMediaList = class;
  JSVGPathSegCurvetoQuadraticSmoothAbs = class;
  JSVGLengthList = class;
  JSVGPathSegCurvetoCubicSmoothRel = class;
  JProcessingInstruction = class;
  JMSBehaviorUrnsCollection = class;
  JAbstractView = class;
  JDocumentFragment = class;
  JBookmarkCollection = class;
  JHTMLHeadElement = class;
  JNodeFilterCallback = class;
  JHTMLFormElement = class;
  JMSMimeTypesCollection = class;
  JStyleSheet = class;
  JNodeList = class;
  JStyleSheetPage = class;
  JMediaError = class;
  JSVGNumberList = class;
  JHTMLElement = class;
  JComment = class;
  JCanvasPattern = class;
  JPositionOptions = class;
  JStorageEvent = class;
  JSVGPathSegLinetoRel = class;
  JMSCompatibleInfoCollection = class;
  JSVGPreserveAspectRatio = class;
  JAttr = class;
  JPerformanceNavigation = class;
  JPositionCallback = class;
  JSVGElementInstanceList = class;
  JCSSRuleList = class;
  JClientRectList = class;
  JDOMError = class;
  JIDBIndex = class;
  JFileList = class;
  JFile = class;
  JMSRangeCollection = class;
  JIDBKeyRange = class;
  JIDBTransaction = class;
  JAudioTrack = class;
  JConsole = class;
  JIDBDatabase = class;
  JDOMStringList = class;
  JTextTrack = class;
  JMediaQueryListListener = class;
  JIDBRequest = class;
  JMessagePort = class;
  JBlob = class;
  JApplicationCache = class;
  JFrameRequestCallback = class;
  JPopStateEvent = class;
  JCSSKeyframeRule = class;
  JMSStream = class;
  JIDBFactory = class;
  JValidityState = class;

function NaN_var: integer;
function Infinity_var: integer;
function eval(x: string): variant;external;
function parseInt(s: string; radix: integer = 0): integer;external;
function parseFloat(string_: string): integer;external;
function isNaN(number_: integer): boolean;external;
function isFinite(number_: integer): boolean;external;
function decodeURI(encodedURI: string): string;external;
function decodeURIComponent(encodedURIComponent: string): string;external;
function encodeURI(uri: string): string;external;
function encodeURIComponent(uriComponent: string): string;external;

type
  JPropertyDescriptor = class external "Object"
    property configurable: boolean;
    property enumerable: boolean;
    property value: variant;
    property writable: boolean;
    function get(): variant;
    procedure set(v: variant);
  end;

type
  JPropertyDescriptorMap = class external "Object"
    function  GetItems(s: string): JPropertyDescriptor; external array;
    procedure SetItems(s: string; value: JPropertyDescriptor); external array;
    property Items[s: string]: JPropertyDescriptor read GetItems write SetItems; default;
  end;

type
  JObject_ = class external "Object"
    function toString(): string;
    function toLocaleString(): string;
    function valueOf(): JObject;
    function hasOwnProperty(v: string): boolean;
    function isPrototypeOf(v: JObject): boolean;
    function propertyIsEnumerable(v: string): boolean;
    function  GetItems(s: string): variant; external array;
    procedure SetItems(s: string; value: variant); external array;
    property Items[s: string]: variant read GetItems write SetItems; default;
  end;

type
  JObject1 = class external "Object"
  public
    function &new(value: variant = undefined): JObject;
    property prototype: JObject;
    function getPrototypeOf(o: variant): variant;
    function getOwnPropertyDescriptor(o: variant; p: string): JPropertyDescriptor;
    function getOwnPropertyNames(o: variant): array of string;
    function create(o: variant; properties: JPropertyDescriptorMap = nil): variant;
    function defineProperty(o: variant; p: string; attributes: JPropertyDescriptor): variant;
    function defineProperties(o: variant; properties: JPropertyDescriptorMap): variant;
    function seal(o: variant): variant;
    function freeze(o: variant): variant;
    function preventExtensions(o: variant): variant;
    function isSealed(o: variant): boolean;
    function isFrozen(o: variant): boolean;
    function isExtensible(o: variant): boolean;
    function keys(o: variant): array of string;
  end;


//function &Object(): variant;external;
function &Object(value: variant): variant;external;
function Object_var: JObject1;

type
  JFunction = class external "Object"
    function apply(thisArg: variant; argArray: variant = undefined): variant;
    function call(thisArg: variant; {many?}argArray: array of variant): variant;
    function bind(thisArg: variant; {many?}argArray: array of variant): variant;
    property prototype: variant;
    property length: integer;
    property arguments: variant;
    property caller: JFunction;
  end;

type
  JFunction1 = class external "&Function"
  public
    function &new({many?}args: array of string): JFunction;
    property prototype: JFunction;
  end;


function &Function({many?}args: array of string): JFunction;external;
function &Function_var: JFunction1;

type
  JIArguments = class external "Object"
    function  GetItems(index: integer): variant; external array;
    procedure SetItems(index: integer; value: variant); external array;
    property Items[index: integer]: variant read GetItems write SetItems; default;
    property length: integer;
    property callee: JFunction;
  end;

type
  TString_replace2_replaceValue_ = function (substring: string; {many?}args: array of variant): string;
type
  JString = class external "Object"
    function toString(): string;
    function charAt(pos: integer): string;
    function charCodeAt(index: integer): integer;
    function concat({many?}strings: array of string): string;
    function indexOf(searchString: string; position: integer = 0): integer;
    function lastIndexOf(searchString: string; position: integer = 0): integer;
    function localeCompare(that: string): integer;
    function match(regexp: string): array of string;
    function match(regexp: JRegExp): array of string;overload;
    function replace(searchValue: string; replaceValue: string): string;
    function replace(searchValue: string; replaceValue: TString_replace2_replaceValue_): string;overload;
    function replace(searchValue: JRegExp; replaceValue: string): string;overload;
    function replace(searchValue: JRegExp; replaceValue: TString_replace2_replaceValue_): string;overload;
    function search(regexp: string): integer;
    function search(regexp: JRegExp): integer;overload;
    function slice(start: integer; &end: integer = 0): string;
    function split(separator: string; limit: integer = 0): array of string;
    function split(separator: JRegExp; limit: integer = 0): array of string;overload;
    function substring(start: integer; &end: integer = 0): string;
    function toLowerCase(): string;
    function toLocaleLowerCase(): string;
    function toUpperCase(): string;
    function toLocaleUpperCase(): string;
    function trim(): string;
    property length: integer;
    function substr(from: integer; length_: integer = 0): string;
  end;

type
  JString1 = class external "String"
  public
    function &new(value: variant = undefined): JString;
    property prototype: JString;
    function fromCharCode({many?}codes: array of integer): string;
  end;


//function String(value: variant = undefined): string;external;
//function String_var: JString1;

type
  JBoolean = class external "Object"
  end;

type
  JBoolean1 = class external "Boolean"
  public
    function &new(value: variant = undefined): JBoolean;
    property prototype: JBoolean;
  end;


function Boolean_(value: variant = undefined): boolean;external;
function Boolean_var: JBoolean1;

type
  JNumber = class external "Object"
    function toString(radix: integer = 0): String;
    function toFixed(fractionDigits: integer = 0): string;
    function toExponential(fractionDigits: integer = 0): string;
    function toPrecision(precision: integer): string;
  end;

type
  JNumber1 = class external "Number"
  public
    function &new(value: variant = undefined): JNumber;
    property prototype: JNumber;
    property MAX_VALUE: integer;
    property MIN_VALUE: integer;
    property NaN: integer;
    property NEGATIVE_INFINITY: integer;
    property POSITIVE_INFINITY: integer;
  end;


function Number(value: variant = undefined): integer;external;
function Number_var: JNumber1;

type
  JMath = class external "Object"
    property E: integer;
    property LN10: integer;
    property LN2: integer;
    property LOG2E: integer;
    property LOG10E: integer;
    property PI: integer;
    property SQRT1_2: integer;
    property SQRT2: integer;
    function abs(x: integer): integer;
    function acos(x: integer): integer;
    function asin(x: integer): integer;
    function atan(x: integer): integer;
    function atan2(y: integer; x: integer): integer;
    function ceil(x: integer): integer;
    function cos(x: integer): integer;
    function exp(x: integer): integer;
    function floor(x: integer): integer;
    function log(x: integer): integer;
    function max({many?}values: array of integer): integer;
    function min({many?}values: array of integer): integer;
    function pow(x: integer; y: integer): integer;
    function random(): integer;
    function round(x: integer): integer;
    function sin(x: integer): integer;
    function sqrt(x: integer): integer;
    function tan(x: integer): integer;
  end;


function Math_var: JMath;

type
  JDate = class external "Object"
    function toString(): string;
    function toDateString(): string;
    function toTimeString(): string;
    function toLocaleString(): string;
    function toLocaleDateString(): string;
    function toLocaleTimeString(): string;
    function valueOf(): integer;
    function getTime(): integer;
    function getFullYear(): integer;
    function getUTCFullYear(): integer;
    function getMonth(): integer;
    function getUTCMonth(): integer;
    function getDate(): integer;
    function getUTCDate(): integer;
    function getDay(): integer;
    function getUTCDay(): integer;
    function getHours(): integer;
    function getUTCHours(): integer;
    function getMinutes(): integer;
    function getUTCMinutes(): integer;
    function getSeconds(): integer;
    function getUTCSeconds(): integer;
    function getMilliseconds(): integer;
    function getUTCMilliseconds(): integer;
    function getTimezoneOffset(): integer;
    procedure setTime(time: integer);
    procedure setMilliseconds(ms: integer);
    procedure setUTCMilliseconds(ms: integer);
    procedure setSeconds(sec: integer; ms: integer = 0);
    procedure setUTCSeconds(sec: integer; ms: integer = 0);
    procedure setMinutes(min: integer; sec: integer = 0; ms: integer = 0);
    procedure setUTCMinutes(min: integer; sec: integer = 0; ms: integer = 0);
    procedure setHours(hours: integer; min: integer = 0; sec: integer = 0; ms: integer = 0);
    procedure setUTCHours(hours: integer; min: integer = 0; sec: integer = 0; ms: integer = 0);
    procedure setDate(date: integer);
    procedure setUTCDate(date: integer);
    procedure setMonth(month: integer; date: integer = 0);
    procedure setUTCMonth(month: integer; date: integer = 0);
    procedure setFullYear(year: integer; month: integer = 0; date: integer = 0);
    procedure setUTCFullYear(year: integer; month: integer = 0; date: integer = 0);
    function toUTCString(): string;
    function toISOString(): string;
    function toJSON(key: variant = undefined): string;
  end;

type
  JDate1 = class external "Date"
  public
    function &new(): JDate;
    function &new(value: integer): JDate;overload;
    function &new(value: string): JDate;overload;
    function &new(year: integer; month: integer; date: integer = 0; hours: integer = 0; minutes: integer = 0; seconds: integer = 0; ms: integer = 0): JDate;overload;
    property prototype: JDate;
    function parse(s: string): integer;
    function UTC(year: integer; month: integer; date: integer = 0; hours: integer = 0; minutes: integer = 0; seconds: integer = 0; ms: integer = 0): integer;
    function now(): integer;
  end;


function Date(): string;external;
function Date_var: JDate1;

type
  TRegExpExecArray_sort_compareFn_ = function (a: string; b: string): integer;
  TRegExpExecArray_every_callbackfn_ = function (value: string; index: integer; &array: array of string): boolean;
  TRegExpExecArray_some_callbackfn_ = function (value: string; index: integer; &array: array of string): boolean;
  TRegExpExecArray_forEach_callbackfn_ = procedure (value: string; index: integer; &array: array of string);
  TRegExpExecArray_map_callbackfn_ = function (value: string; index: integer; &array: array of string): variant;
  TRegExpExecArray_filter_callbackfn_ = function (value: string; index: integer; &array: array of string): boolean;
  TRegExpExecArray_reduce_callbackfn_ = function (previousValue: variant; currentValue: variant; currentIndex: integer; &array: array of string): variant;
  TRegExpExecArray_reduceRight_callbackfn_ = function (previousValue: variant; currentValue: variant; currentIndex: integer; &array: array of string): variant;
type
  JRegExpExecArray = class external "Object"
    function  GetItems(index: integer): string; external array;
    procedure SetItems(index: integer; value: string); external array;
    property Items[index: integer]: string read GetItems write SetItems; default;
    property length: integer;
    property index: integer;
    property input: string;
    function toString(): string;
    function toLocaleString(): string;
    function concat({many?}items: array of array of string): array of string;
    function join(seperator: string = ""): string;
    function pop(): string;
    function push({many?}items: array of string): integer;
    function reverse(): array of string;
    function shift(): string;
    function slice(start: integer; &end: integer = 0): array of string;
    function sort(compareFn: TRegExpExecArray_sort_compareFn_ = nil): array of string;
    function splice(start: integer): array of string;
    function splice(start: integer; deleteCount: integer; {many?}items: array of string): array of string;overload;
    function unshift({many?}items: array of string): integer;
    function indexOf(searchElement: string; fromIndex: integer = 0): integer;
    function lastIndexOf(searchElement: string; fromIndex: integer = 0): integer;
    function every(callbackfn: TRegExpExecArray_every_callbackfn_; thisArg: variant = undefined): boolean;
    function some(callbackfn: TRegExpExecArray_some_callbackfn_; thisArg: variant = undefined): boolean;
    procedure forEach(callbackfn: TRegExpExecArray_forEach_callbackfn_; thisArg: variant = undefined);
    function map(callbackfn: TRegExpExecArray_map_callbackfn_; thisArg: variant = undefined): array of variant;
    function filter(callbackfn: TRegExpExecArray_filter_callbackfn_; thisArg: variant = undefined): array of string;
    function reduce(callbackfn: TRegExpExecArray_reduce_callbackfn_; initialValue: variant = undefined): variant;
    function reduceRight(callbackfn: TRegExpExecArray_reduceRight_callbackfn_; initialValue: variant = undefined): variant;
  end;

type
  JRegExp = class external "Object"
    function exec(string_: string): JRegExpExecArray;
    function test(string_: string): boolean;
    property source: string;
    property global: boolean;
    property ignoreCase: boolean;
    property multiline: boolean;
    property lastIndex: integer;
    function compile(): JRegExp;
  end;

type
  JRegExp1 = class external "RegExp"
  public
    function &new(pattern: string; flags: string = ""): JRegExp;
//    property $1: string;
//    property $2: string;
//    property $3: string;
//    property $4: string;
//    property $5: string;
//    property $6: string;
//    property $7: string;
//    property $8: string;
//    property $9: string;
    property lastMatch: string;
  end;


function RegExp(pattern: string; flags: string = ""): JRegExp;external;
function RegExp_var: JRegExp1;

type
  JError = class external "Object"
    property name: string;
    property message: string;
  end;

type
  JError1 = class external "Error"
  public
    function &new(message: string = ""): JError;
    property prototype: JError;
  end;


function Error(message: string = ""): JError;external;
function Error_var: JError1;

type
  JEvalError = class external "Object"(JError)
  end;

type
  JEvalError1 = class external "EvalError"
  public
    function &new(message: string = ""): JEvalError;
    property prototype: JEvalError;
  end;


function EvalError(message: string = ""): JEvalError;external;
function EvalError_var: JEvalError1;

type
  JRangeError = class external "Object"(JError)
  end;

type
  JRangeError1 = class external "RangeError"
  public
    function &new(message: string = ""): JRangeError;
    property prototype: JRangeError;
  end;


function RangeError(message: string = ""): JRangeError;external;
function RangeError_var: JRangeError1;

type
  JReferenceError = class external "Object"(JError)
  end;

type
  JReferenceError1 = class external "ReferenceError"
  public
    function &new(message: string = ""): JReferenceError;
    property prototype: JReferenceError;
  end;


function ReferenceError(message: string = ""): JReferenceError;external;
function ReferenceError_var: JReferenceError1;

type
  JSyntaxError = class external "Object"(JError)
  end;

type
  JSyntaxError1 = class external "SyntaxError"
  public
    function &new(message: string = ""): JSyntaxError;
    property prototype: JSyntaxError;
  end;


function SyntaxError(message: string = ""): JSyntaxError;external;
function SyntaxError_var: JSyntaxError1;

type
  JTypeError = class external "Object"(JError)
  end;

type
  JTypeError1 = class external "TypeError"
  public
    function &new(message: string = ""): JTypeError;
    property prototype: JTypeError;
  end;


function TypeError(message: string = ""): JTypeError;external;
function TypeError_var: JTypeError1;

type
  JURIError = class external "Object"(JError)
  end;

type
  JURIError1 = class external "URIError"
  public
    function &new(message: string = ""): JURIError;
    property prototype: JURIError;
  end;


function URIError(message: string = ""): JURIError;external;
function URIError_var: JURIError1;

type
  TJSON_parse_reviver_ = function (key: variant; value: variant): variant;
  TJSON_stringify2_replacer_ = function (key: string; value: variant): variant;
type
  JJSON = class external "Object"
    function parse(text: string; reviver: TJSON_parse_reviver_ = nil): variant;
    function stringify(value: variant): string;
    function stringify(value: variant; replacer: TJSON_stringify2_replacer_): string;overload;
    function stringify(value: variant; replacer: array of variant): string;overload;
    function stringify(value: variant; replacer: TJSON_stringify2_replacer_; space: variant): string;overload;
    function stringify(value: variant; replacer: array of variant; space: variant): string;overload;
  end;


function JSON_var: JJSON;

type
  TArray_sort_compareFn_ = function (a: Jelement; b: Jelement): integer;
  TArray_every_callbackfn_ = function (value: Jelement; index: integer; &array: array of Jelement): boolean;
  TArray_some_callbackfn_ = function (value: Jelement; index: integer; &array: array of Jelement): boolean;
  TArray_forEach_callbackfn_ = procedure (value: Jelement; index: integer; &array: array of Jelement);
  TArray_map_callbackfn_ = function (value: Jelement; index: integer; &array: array of Jelement): variant;
  TArray_filter_callbackfn_ = function (value: Jelement; index: integer; &array: array of Jelement): boolean;
  TArray_reduce_callbackfn_ = function (previousValue: variant; currentValue: variant; currentIndex: integer; &array: array of Jelement): variant;
  TArray_reduceRight_callbackfn_ = function (previousValue: variant; currentValue: variant; currentIndex: integer; &array: array of Jelement): variant;
type
  JArray = class external "Object"
    function toString(): string;
    function toLocaleString(): string;
    function concat({many?}items: array of array of Jelement): array of Jelement;
    function concat({many?}items: array of Jelement): array of Jelement;overload;
    function join(seperator: string = ""): string;
    function pop(): Jelement;
    function push({many?}items: array of Jelement): integer;
    function reverse(): array of Jelement;
    function shift(): Jelement;
    function slice(start: integer; &end: integer = 0): array of Jelement;
    function sort(compareFn: TArray_sort_compareFn_ = nil): array of Jelement;
    function splice(start: integer): array of Jelement;
    function splice(start: integer; deleteCount: integer; {many?}items: array of Jelement): array of Jelement;overload;
    function unshift({many?}items: array of Jelement): integer;
    function indexOf(searchElement: Jelement; fromIndex: integer = 0): integer;
    function lastIndexOf(searchElement: Jelement; fromIndex: integer = 0): integer;
    function every(callbackfn: TArray_every_callbackfn_; thisArg: variant = undefined): boolean;
    function some(callbackfn: TArray_some_callbackfn_; thisArg: variant = undefined): boolean;
    procedure forEach(callbackfn: TArray_forEach_callbackfn_; thisArg: variant = undefined);
    function map(callbackfn: TArray_map_callbackfn_; thisArg: variant = undefined): array of variant;
    function filter(callbackfn: TArray_filter_callbackfn_; thisArg: variant = undefined): array of Jelement;
    function reduce(callbackfn: TArray_reduce_callbackfn_; initialValue: variant = undefined): variant;
    function reduceRight(callbackfn: TArray_reduceRight_callbackfn_; initialValue: variant = undefined): variant;
    property length: integer;
  end;

type
  JArray1 = class external "&Array"
  public
    function &new({many?}items: array of variant): array of variant;
    function isArray(arg: variant): boolean;
    property prototype: JArray;
  end;


function &Array({many?}items: array of variant): array of variant;external;
function &Array_var: JArray1;

type
  JArrayBuffer = class external "Object"
    property byteLength: integer;
  end;

type
  JArrayBuffer1 = class external "ArrayBuffer"
  public
    property prototype: JArrayBuffer;
    procedure &new(byteLength: integer);
  end;


function ArrayBuffer_var: JArrayBuffer1;

type
  JArrayBufferView = class external "Object"
    property buffer: JArrayBuffer;
    property byteOffset: integer;
    property byteLength: integer;
  end;

type
  JInt8Array = class external "Object"(JArrayBufferView)
    property BYTES_PER_ELEMENT: integer;
    property length: integer;
    function  GetItems(index: integer): integer; external array;
    procedure SetItems(index: integer; value: integer); external array;
    property Items[index: integer]: integer read GetItems write SetItems; default;
    function get(index: integer): integer;
    procedure set(index: integer; value: integer);
    procedure set(&array: JInt8Array; offset: integer = 0);overload;
    procedure set(&array: array of integer; offset: integer = 0);overload;
    function subarray(&begin: integer; &end: integer = 0): JInt8Array;
  end;

type
  JInt8Array1 = class external "Int8Array"
  public
    property prototype: JInt8Array;
    function &new(length_: integer): JInt8Array;
    function &new(&array: JInt8Array): JInt8Array;overload;
    function &new(&array: array of integer): JInt8Array;overload;
    function &new(buffer: JArrayBuffer; byteOffset: integer = 0; length_: integer = 0): JInt8Array;overload;
    property BYTES_PER_ELEMENT: integer;
  end;


function Int8Array_var: JInt8Array1;

type
  JUint8Array = class external "Object"(JArrayBufferView)
    property BYTES_PER_ELEMENT: integer;
    property length: integer;
    function  GetItems(index: integer): integer; external array;
    procedure SetItems(index: integer; value: integer); external array;
    property Items[index: integer]: integer read GetItems write SetItems; default;
    function get(index: integer): integer;
    procedure set(index: integer; value: integer);
    procedure set(&array: JUint8Array; offset: integer = 0);overload;
    procedure set(&array: array of integer; offset: integer = 0);overload;
    function subarray(&begin: integer; &end: integer = 0): JUint8Array;
  end;

type
  JUint8Array1 = class external "Uint8Array"
  public
    property prototype: JUint8Array;
    function &new(length_: integer): JUint8Array;
    function &new(&array: JUint8Array): JUint8Array;overload;
    function &new(&array: array of integer): JUint8Array;overload;
    function &new(buffer: JArrayBuffer; byteOffset: integer = 0; length_: integer = 0): JUint8Array;overload;
    property BYTES_PER_ELEMENT: integer;
  end;


function Uint8Array_var: JUint8Array1;

type
  JInt16Array = class external "Object"(JArrayBufferView)
    property BYTES_PER_ELEMENT: integer;
    property length: integer;
    function  GetItems(index: integer): integer; external array;
    procedure SetItems(index: integer; value: integer); external array;
    property Items[index: integer]: integer read GetItems write SetItems; default;
    function get(index: integer): integer;
    procedure set(index: integer; value: integer);
    procedure set(&array: JInt16Array; offset: integer = 0);overload;
    procedure set(&array: array of integer; offset: integer = 0);overload;
    function subarray(&begin: integer; &end: integer = 0): JInt16Array;
  end;

type
  JInt16Array1 = class external "Int16Array"
  public
    property prototype: JInt16Array;
    function &new(length_: integer): JInt16Array;
    function &new(&array: JInt16Array): JInt16Array;overload;
    function &new(&array: array of integer): JInt16Array;overload;
    function &new(buffer: JArrayBuffer; byteOffset: integer = 0; length_: integer = 0): JInt16Array;overload;
    property BYTES_PER_ELEMENT: integer;
  end;


function Int16Array_var: JInt16Array1;

type
  JUint16Array = class external "Object"(JArrayBufferView)
    property BYTES_PER_ELEMENT: integer;
    property length: integer;
    function  GetItems(index: integer): integer; external array;
    procedure SetItems(index: integer; value: integer); external array;
    property Items[index: integer]: integer read GetItems write SetItems; default;
    function get(index: integer): integer;
    procedure set(index: integer; value: integer);
    procedure set(&array: JUint16Array; offset: integer = 0);overload;
    procedure set(&array: array of integer; offset: integer = 0);overload;
    function subarray(&begin: integer; &end: integer = 0): JUint16Array;
  end;

type
  JUint16Array1 = class external "Uint16Array"
  public
    property prototype: JUint16Array;
    function &new(length_: integer): JUint16Array;
    function &new(&array: JUint16Array): JUint16Array;overload;
    function &new(&array: array of integer): JUint16Array;overload;
    function &new(buffer: JArrayBuffer; byteOffset: integer = 0; length_: integer = 0): JUint16Array;overload;
    property BYTES_PER_ELEMENT: integer;
  end;


function Uint16Array_var: JUint16Array1;

type
  JInt32Array = class external "Object"(JArrayBufferView)
    property BYTES_PER_ELEMENT: integer;
    property length: integer;
    function  GetItems(index: integer): integer; external array;
    procedure SetItems(index: integer; value: integer); external array;
    property Items[index: integer]: integer read GetItems write SetItems; default;
    function get(index: integer): integer;
    procedure set(index: integer; value: integer);
    procedure set(&array: JInt32Array; offset: integer = 0);overload;
    procedure set(&array: array of integer; offset: integer = 0);overload;
    function subarray(&begin: integer; &end: integer = 0): JInt32Array;
  end;

type
  JInt32Array1 = class external "Int32Array"
  public
    property prototype: JInt32Array;
    function &new(length_: integer): JInt32Array;
    function &new(&array: JInt32Array): JInt32Array;overload;
    function &new(&array: array of integer): JInt32Array;overload;
    function &new(buffer: JArrayBuffer; byteOffset: integer = 0; length_: integer = 0): JInt32Array;overload;
    property BYTES_PER_ELEMENT: integer;
  end;


function Int32Array_var: JInt32Array1;

type
  JUint32Array = class external "Object"(JArrayBufferView)
    property BYTES_PER_ELEMENT: integer;
    property length: integer;
    function  GetItems(index: integer): integer; external array;
    procedure SetItems(index: integer; value: integer); external array;
    property Items[index: integer]: integer read GetItems write SetItems; default;
    function get(index: integer): integer;
    procedure set(index: integer; value: integer);
    procedure set(&array: JUint32Array; offset: integer = 0);overload;
    procedure set(&array: array of integer; offset: integer = 0);overload;
    function subarray(&begin: integer; &end: integer = 0): JUint32Array;
  end;

type
  JUint32Array1 = class external "Uint32Array"
  public
    property prototype: JUint32Array;
    function &new(length_: integer): JUint32Array;
    function &new(&array: JUint32Array): JUint32Array;overload;
    function &new(&array: array of integer): JUint32Array;overload;
    function &new(buffer: JArrayBuffer; byteOffset: integer = 0; length_: integer = 0): JUint32Array;overload;
    property BYTES_PER_ELEMENT: integer;
  end;


function Uint32Array_var: JUint32Array1;

type
  JFloat32Array = class external "Object"(JArrayBufferView)
    property BYTES_PER_ELEMENT: integer;
    property length: integer;
    function  GetItems(index: integer): integer; external array;
    procedure SetItems(index: integer; value: integer); external array;
    property Items[index: integer]: integer read GetItems write SetItems; default;
    function get(index: integer): integer;
    procedure set(index: integer; value: integer);
    procedure set(&array: JFloat32Array; offset: integer = 0);overload;
    procedure set(&array: array of integer; offset: integer = 0);overload;
    function subarray(&begin: integer; &end: integer = 0): JFloat32Array;
  end;

type
  JFloat32Array1 = class external "Float32Array"
  public
    property prototype: JFloat32Array;
    function &new(length_: integer): JFloat32Array;
    function &new(&array: JFloat32Array): JFloat32Array;overload;
    function &new(&array: array of integer): JFloat32Array;overload;
    function &new(buffer: JArrayBuffer; byteOffset: integer = 0; length_: integer = 0): JFloat32Array;overload;
    property BYTES_PER_ELEMENT: integer;
  end;


function Float32Array_var: JFloat32Array1;

type
  JFloat64Array = class external "Object"(JArrayBufferView)
    property BYTES_PER_ELEMENT: integer;
    property length: integer;
    function  GetItems(index: integer): integer; external array;
    procedure SetItems(index: integer; value: integer); external array;
    property Items[index: integer]: integer read GetItems write SetItems; default;
    function get(index: integer): integer;
    procedure set(index: integer; value: integer);
    procedure set(&array: JFloat64Array; offset: integer = 0);overload;
    procedure set(&array: array of integer; offset: integer = 0);overload;
    function subarray(&begin: integer; &end: integer = 0): JFloat64Array;
  end;

type
  JFloat64Array1 = class external "Float64Array"
  public
    property prototype: JFloat64Array;
    function &new(length_: integer): JFloat64Array;
    function &new(&array: JFloat64Array): JFloat64Array;overload;
    function &new(&array: array of integer): JFloat64Array;overload;
    function &new(buffer: JArrayBuffer; byteOffset: integer = 0; length_: integer = 0): JFloat64Array;overload;
    property BYTES_PER_ELEMENT: integer;
  end;


function Float64Array_var: JFloat64Array1;

type
  JDataView = class external "Object"(JArrayBufferView)
    function getInt8(byteOffset: integer): integer;
    function getUint8(byteOffset: integer): integer;
    function getInt16(byteOffset: integer; littleEndian: boolean = false): integer;
    function getUint16(byteOffset: integer; littleEndian: boolean = false): integer;
    function getInt32(byteOffset: integer; littleEndian: boolean = false): integer;
    function getUint32(byteOffset: integer; littleEndian: boolean = false): integer;
    function getFloat32(byteOffset: integer; littleEndian: boolean = false): integer;
    function getFloat64(byteOffset: integer; littleEndian: boolean = false): integer;
    procedure setInt8(byteOffset: integer; value: integer);
    procedure setUint8(byteOffset: integer; value: integer);
    procedure setInt16(byteOffset: integer; value: integer; littleEndian: boolean = false);
    procedure setUint16(byteOffset: integer; value: integer; littleEndian: boolean = false);
    procedure setInt32(byteOffset: integer; value: integer; littleEndian: boolean = false);
    procedure setUint32(byteOffset: integer; value: integer; littleEndian: boolean = false);
    procedure setFloat32(byteOffset: integer; value: integer; littleEndian: boolean = false);
    procedure setFloat64(byteOffset: integer; value: integer; littleEndian: boolean = false);
  end;

type
  JDataView1 = class external "DataView"
  public
    property prototype: JDataView;
    function &new(buffer: JArrayBuffer; byteOffset: integer = 0; length_: integer = 0): JDataView;
  end;


function DataView_var: JDataView1;

type
  JNavigatorID = class external "Object"
    property appVersion: string;
    property appName: string;
    property userAgent: string;
    property platform: string;
  end;

type
  JEventTarget = class external "Object"
    procedure removeEventListener(&type: string; listener: JEventListener; useCapture: boolean = false);
    procedure addEventListener(&type: string; listener: JEventListener; useCapture: boolean = false);
    function dispatchEvent(evt: JEvent): boolean;
  end;

type
  JNode = class external "Object"(JEventTarget)
    property nodeType: integer;
    property previousSibling: JNode;
    property localName: string;
    property namespaceURI: string;
    property textContent: string;
    property parentNode: JNode;
    property nextSibling: JNode;
    property nodeValue: string;
    property lastChild: JNode;
    property childNodes: JNodeList;
    property nodeName: string;
    property ownerDocument: JDocument;
    property attributes: array of JAttr;
    property firstChild: JNode;
    property prefix: string;
    function removeChild(oldChild: JNode): JNode;
    function appendChild(newChild: JNode): JNode;
    function isSupported(feature: string; version: string): boolean;
    function isEqualNode(arg: JNode): boolean;
    function lookupPrefix(namespaceURI: string): string;
    function isDefaultNamespace(namespaceURI: string): boolean;
    function compareDocumentPosition(other: JNode): integer;
    procedure normalize();
    function isSameNode(other: JNode): boolean;
    function hasAttributes(): boolean;
    function lookupNamespaceURI(prefix: string): string;
    function cloneNode(deep: boolean = false): JNode;
    function hasChildNodes(): boolean;
    function replaceChild(newChild: JNode; oldChild: JNode): JNode;
    function insertBefore(newChild: JNode; refChild: JNode = nil): JNode;
    property ENTITY_REFERENCE_NODE: integer;
    property ATTRIBUTE_NODE: integer;
    property DOCUMENT_FRAGMENT_NODE: integer;
    property TEXT_NODE: integer;
    property ELEMENT_NODE: integer;
    property COMMENT_NODE: integer;
    property DOCUMENT_POSITION_DISCONNECTED: integer;
    property DOCUMENT_POSITION_CONTAINED_BY: integer;
    property DOCUMENT_POSITION_CONTAINS: integer;
    property DOCUMENT_TYPE_NODE: integer;
    property DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: integer;
    property DOCUMENT_NODE: integer;
    property ENTITY_NODE: integer;
    property PROCESSING_INSTRUCTION_NODE: integer;
    property CDATA_SECTION_NODE: integer;
    property NOTATION_NODE: integer;
    property DOCUMENT_POSITION_FOLLOWING: integer;
    property DOCUMENT_POSITION_PRECEDING: integer;
  end;

type
  JNode1 = class external "Node"
  public
    property prototype: JNode;
    function &new(): JNode;
    property ENTITY_REFERENCE_NODE: integer;
    property ATTRIBUTE_NODE: integer;
    property DOCUMENT_FRAGMENT_NODE: integer;
    property TEXT_NODE: integer;
    property ELEMENT_NODE: integer;
    property COMMENT_NODE: integer;
    property DOCUMENT_POSITION_DISCONNECTED: integer;
    property DOCUMENT_POSITION_CONTAINED_BY: integer;
    property DOCUMENT_POSITION_CONTAINS: integer;
    property DOCUMENT_TYPE_NODE: integer;
    property DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: integer;
    property DOCUMENT_NODE: integer;
    property ENTITY_NODE: integer;
    property PROCESSING_INSTRUCTION_NODE: integer;
    property CDATA_SECTION_NODE: integer;
    property NOTATION_NODE: integer;
    property DOCUMENT_POSITION_FOLLOWING: integer;
    property DOCUMENT_POSITION_PRECEDING: integer;
  end;


function Node_var: JNode1;

type
  JClientRect = class external "Object"
    property left: integer;
    property width: integer;
    property right: integer;
    property top: integer;
    property bottom: integer;
    property height: integer;
  end;

type
  JClientRect1 = class external "ClientRect"
  public
    property prototype: JClientRect;
    function &new(): JClientRect;
  end;

function ClientRect_var: JClientRect1;

type
  JElement = class external "Object"(JNode)//, JNodeSelector)//, JElementTraversal)//, JMSElementExtensions)
    property scrollTop: integer;
    property clientLeft: integer;
    property scrollLeft: integer;
    property tagName: string;
    property clientWidth: integer;
    property scrollWidth: integer;
    property clientHeight: integer;
    property clientTop: integer;
    property scrollHeight: integer;
    function getAttribute(name: string = ""): string;
    function getElementsByTagNameNS(namespaceURI: string; localName: string): JNodeList;
    function hasAttributeNS(namespaceURI: string; localName: string): boolean;
    function getBoundingClientRect(): JClientRect;
    function getAttributeNS(namespaceURI: string; localName: string): string;
    function getAttributeNodeNS(namespaceURI: string; localName: string): JAttr;
    function setAttributeNodeNS(newAttr: JAttr): JAttr;
    function hasAttribute(name: string): boolean;
    procedure removeAttribute(name: string = "");
    procedure setAttributeNS(namespaceURI: string; qualifiedName: string; value: string);
    function getAttributeNode(name: string): JAttr;
    function getElementsByTagName(name: string): JNodeList;
    function setAttributeNode(newAttr: JAttr): JAttr;
    function getClientRects(): JClientRectList;
    function removeAttributeNode(oldAttr: JAttr): JAttr;
    procedure setAttribute(name: string = ""; value: string = "");
    procedure removeAttributeNS(namespaceURI: string; localName: string);
  end;

type
  JElement1 = class external "Element"
  public
    property prototype: JElement;
    function &new(): JElement;
  end;

function Element_var: JElement1;

type
  JCSS3Properties = class external "Object"
    property textAlignLast: string;
    property textUnderlinePosition: string;
    property wordWrap: string;
    property borderTopLeftRadius: string;
    property backgroundClip: string;
    property msTransformOrigin: string;
    property opacity: string;
    property overflowY: string;
    property boxShadow: string;
    property backgroundSize: string;
    property wordBreak: string;
    property boxSizing: string;
    property rubyOverhang: string;
    property rubyAlign: string;
    property textJustify: string;
    property borderRadius: string;
    property overflowX: string;
    property borderTopRightRadius: string;
    property msTransform: string;
    property borderBottomLeftRadius: string;
    property rubyPosition: string;
    property borderBottomRightRadius: string;
    property backgroundOrigin: string;
    property textOverflow: string;
  end;

type
  JCSSStyleDeclaration = class external "Object"(JCSS3Properties)//, JSVG1_1Properties)//, JCSS2Properties)
    property cssText: string;
    property length: integer;
    property parentRule: JCSSRule;
    function getPropertyPriority(propertyName: string): string;
    function getPropertyValue(propertyName: string): string;
    function removeProperty(propertyName: string): string;
    function item(index: integer): string;
    function  GetItems(index: integer): string; external array;
    procedure SetItems(index: integer; value: string); external array;
    property Items[index: integer]: string read GetItems write SetItems; default;
    procedure setProperty(propertyName: string; value: string; priority: string = "");
  end;

type
  JCSSStyleDeclaration1 = class external "CSSStyleDeclaration"
  public
    property prototype: JCSSStyleDeclaration;
    function &new(): JCSSStyleDeclaration;
  end;


function CSSStyleDeclaration_var: JCSSStyleDeclaration1;

type
  JMSCSSProperties = class external "Object"(JCSSStyleDeclaration)//, JMSCSSStyleDeclarationExtensions)
    property scrollbarShadowColor: string;
    property scrollbarHighlightColor: string;
    property layoutGridChar: string;
    property layoutGridType: string;
    property textAutospace: string;
    property textKashidaSpace: string;
    property writingMode: string;
    property scrollbarFaceColor: string;
    property backgroundPositionY: string;
    property lineBreak: string;
    property imeMode: string;
    property msBlockProgression: string;
    property layoutGridLine: string;
    property scrollbarBaseColor: string;
    property layoutGrid: string;
    property layoutFlow: string;
    property textKashida: string;
    property filter: string;
    property zoom: string;
    property scrollbarArrowColor: string;
    property behavior: string;
    property backgroundPositionX: string;
    property accelerator: string;
    property layoutGridMode: string;
    property textJustifyTrim: string;
    property scrollbar3dLightColor: string;
    property msInterpolationMode: string;
    property scrollbarTrackColor: string;
    property scrollbarDarkShadowColor: string;
    property styleFloat: string;
  end;

type
  JMSCSSProperties1 = class external "MSCSSProperties"
  public
    property prototype: JMSCSSProperties;
    function &new(): JMSCSSProperties;
  end;

function MSCSSProperties_var: JMSCSSProperties1;

type
  JMSStyleCSSProperties = class external "Object"(JMSCSSProperties)
    property pixelWidth: integer;
    property posHeight: integer;
    property posLeft: integer;
    property pixelTop: integer;
    property pixelBottom: integer;
    property textDecorationNone: boolean;
    property pixelLeft: integer;
    property posTop: integer;
    property posBottom: integer;
    property textDecorationOverline: boolean;
    property posWidth: integer;
    property textDecorationLineThrough: boolean;
    property pixelHeight: integer;
    property textDecorationBlink: boolean;
    property posRight: integer;
    property pixelRight: integer;
    property textDecorationUnderline: boolean;
  end;

type
  JMSStyleCSSProperties1 = class external "MSStyleCSSProperties"
  public
    property prototype: JMSStyleCSSProperties;
    function &new(): JMSStyleCSSProperties;
  end;


function MSStyleCSSProperties_var: JMSStyleCSSProperties1;

type
  THTMLElement_ondragend_ = function (ev: JDragEvent): variant;
  THTMLElement_onkeydown_ = function (ev: JKeyboardEvent): variant;
  THTMLElement_ondragover_ = function (ev: JDragEvent): variant;
  THTMLElement_onkeyup_ = function (ev: JKeyboardEvent): variant;
  THTMLElement_onreset_ = function (ev: JEvent): variant;
  THTMLElement_onmouseup_ = function (ev: JMouseEvent): variant;
  THTMLElement_ondragstart_ = function (ev: JDragEvent): variant;
  THTMLElement_ondrag_ = function (ev: JDragEvent): variant;
  THTMLElement_onmouseover_ = function (ev: JMouseEvent): variant;
  THTMLElement_ondragleave_ = function (ev: JDragEvent): variant;
  THTMLElement_onpause_ = function (ev: JEvent): variant;
  THTMLElement_onseeked_ = function (ev: JEvent): variant;
  THTMLElement_onmousedown_ = function (ev: JMouseEvent): variant;
  THTMLElement_onclick_ = function (ev: JMouseEvent): variant;
  THTMLElement_onwaiting_ = function (ev: JEvent): variant;
  THTMLElement_ondurationchange_ = function (ev: JEvent): variant;
  THTMLElement_onblur_ = function (ev: JFocusEvent): variant;
  THTMLElement_onemptied_ = function (ev: JEvent): variant;
  THTMLElement_onseeking_ = function (ev: JEvent): variant;
  THTMLElement_oncanplay_ = function (ev: JEvent): variant;
  THTMLElement_onstalled_ = function (ev: JEvent): variant;
  THTMLElement_onmousemove_ = function (ev: JMouseEvent): variant;
  THTMLElement_onratechange_ = function (ev: JEvent): variant;
  THTMLElement_onloadstart_ = function (ev: JEvent): variant;
  THTMLElement_ondragenter_ = function (ev: JDragEvent): variant;
  THTMLElement_onsubmit_ = function (ev: JEvent): variant;
  THTMLElement_onprogress_ = function (ev: variant): variant;
  THTMLElement_ondblclick_ = function (ev: JMouseEvent): variant;
  THTMLElement_oncontextmenu_ = function (ev: JMouseEvent): variant;
  THTMLElement_onchange_ = function (ev: JEvent): variant;
  THTMLElement_onloadedmetadata_ = function (ev: JEvent): variant;
  THTMLElement_onerror_ = function (ev: JEvent): variant;
  THTMLElement_onplay_ = function (ev: JEvent): variant;
  THTMLElement_onplaying_ = function (ev: JEvent): variant;
  THTMLElement_oncanplaythrough_ = function (ev: JEvent): variant;
  THTMLElement_onabort_ = function (ev: JUIEvent): variant;
  THTMLElement_onreadystatechange_ = function (ev: JEvent): variant;
  THTMLElement_onkeypress_ = function (ev: JKeyboardEvent): variant;
  THTMLElement_onloadeddata_ = function (ev: JEvent): variant;
  THTMLElement_onsuspend_ = function (ev: JEvent): variant;
  THTMLElement_onfocus_ = function (ev: JFocusEvent): variant;
  THTMLElement_ontimeupdate_ = function (ev: JEvent): variant;
  THTMLElement_onselect_ = function (ev: JUIEvent): variant;
  THTMLElement_ondrop_ = function (ev: JDragEvent): variant;
  THTMLElement_onmouseout_ = function (ev: JMouseEvent): variant;
  THTMLElement_onended_ = function (ev: JEvent): variant;
  THTMLElement_onscroll_ = function (ev: JUIEvent): variant;
  THTMLElement_onmousewheel_ = function (ev: JMouseWheelEvent): variant;
  THTMLElement_onvolumechange_ = function (ev: JEvent): variant;
  THTMLElement_onload_ = function (ev: JEvent): variant;
  THTMLElement_oninput_ = function (ev: JEvent): variant;
type
  JHTMLElement = class external "Object"(JElement)//, JMSHTMLElementRangeExtensions)//, JElementCSSInlineStyle)//, JMSEventAttachmentTarget)//, JMSHTMLElementExtensions)//, JMSNodeExtensions)
    property ondragend: THTMLElement_ondragend_;
    property onkeydown: THTMLElement_onkeydown_;
    property ondragover: THTMLElement_ondragover_;
    property onkeyup: THTMLElement_onkeyup_;
    property offsetTop: integer;
    property onreset: THTMLElement_onreset_;
    property onmouseup: THTMLElement_onmouseup_;
    property ondragstart: THTMLElement_ondragstart_;
    property ondrag: THTMLElement_ondrag_;
    property innerHTML: string;
    property onmouseover: THTMLElement_onmouseover_;
    property ondragleave: THTMLElement_ondragleave_;
    property lang: string;
    property onpause: THTMLElement_onpause_;
    property className: string;
    property onseeked: THTMLElement_onseeked_;
    property onmousedown: THTMLElement_onmousedown_;
    property title: string;
    property onclick: THTMLElement_onclick_;
    property onwaiting: THTMLElement_onwaiting_;
    property outerHTML: string;
    property offsetLeft: integer;
    property ondurationchange: THTMLElement_ondurationchange_;
    property offsetHeight: integer;
    property dir: string;
    property onblur: THTMLElement_onblur_;
    property onemptied: THTMLElement_onemptied_;
    property onseeking: THTMLElement_onseeking_;
    property oncanplay: THTMLElement_oncanplay_;
    property onstalled: THTMLElement_onstalled_;
    property onmousemove: THTMLElement_onmousemove_;
    property style: JMSStyleCSSProperties;
    property isContentEditable: boolean;
    property onratechange: THTMLElement_onratechange_;
    property onloadstart: THTMLElement_onloadstart_;
    property ondragenter: THTMLElement_ondragenter_;
    property contentEditable: string;
    property onsubmit: THTMLElement_onsubmit_;
    property tabIndex: integer;
    property onprogress: THTMLElement_onprogress_;
    property ondblclick: THTMLElement_ondblclick_;
    property oncontextmenu: THTMLElement_oncontextmenu_;
    property onchange: THTMLElement_onchange_;
    property onloadedmetadata: THTMLElement_onloadedmetadata_;
    property onerror: THTMLElement_onerror_;
    property onplay: THTMLElement_onplay_;
    property id: string;
    property onplaying: THTMLElement_onplaying_;
    property oncanplaythrough: THTMLElement_oncanplaythrough_;
    property onabort: THTMLElement_onabort_;
    property onreadystatechange: THTMLElement_onreadystatechange_;
    property onkeypress: THTMLElement_onkeypress_;
    property offsetParent: JElement;
    property onloadeddata: THTMLElement_onloadeddata_;
    property disabled: boolean;
    property onsuspend: THTMLElement_onsuspend_;
    property accessKey: string;
    property onfocus: THTMLElement_onfocus_;
    property ontimeupdate: THTMLElement_ontimeupdate_;
    property onselect: THTMLElement_onselect_;
    property ondrop: THTMLElement_ondrop_;
    property offsetWidth: integer;
    property onmouseout: THTMLElement_onmouseout_;
    property onended: THTMLElement_onended_;
    property onscroll: THTMLElement_onscroll_;
    property onmousewheel: THTMLElement_onmousewheel_;
    property onvolumechange: THTMLElement_onvolumechange_;
    property onload: THTMLElement_onload_;
    property oninput: THTMLElement_oninput_;
    procedure click();
    function getElementsByClassName(classNames: string): JNodeList;
    procedure scrollIntoView(top: boolean = false);
    procedure focus();
    procedure blur();
    procedure insertAdjacentHTML(where: string; html: string);
  end;

type
  JHTMLElement1 = class external "HTMLElement"
  public
    property prototype: JHTMLElement;
    function &new(): JHTMLElement;
  end;


function HTMLElement_var: JHTMLElement1;

type
  JHTMLTableElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedBorderStyle_HTMLTableElement)//, JDOML2DeprecatedAlignmentStyle_HTMLTableElement)//, JMSBorderColorStyle)//, JMSDataBindingExtensions)//, JMSHTMLTableElementExtensions)//, JDOML2DeprecatedBackgroundStyle)//, JMSBorderColorHighlightStyle)//, JMSDataBindingTableExtensions)//, JDOML2DeprecatedBackgroundColorStyle)
    property tBodies: JHTMLCollection;
    property width: string;
    property tHead: JHTMLTableSectionElement;
    property cellSpacing: string;
    property tFoot: JHTMLTableSectionElement;
    property frame: string;
    property rows: JHTMLCollection;
    property rules: string;
    property cellPadding: string;
    property summary: string;
    property caption: JHTMLTableCaptionElement;
    procedure deleteRow(index: integer = 0);
    function createTBody(): JHTMLElement;
    procedure deleteCaption();
    function insertRow(index: integer = 0): JHTMLElement;
    procedure deleteTFoot();
    function createTHead(): JHTMLElement;
    procedure deleteTHead();
    function createCaption(): JHTMLElement;
    function createTFoot(): JHTMLElement;
  end;

type
  JHTMLTableElement1 = class external "HTMLTableElement"
  public
    property prototype: JHTMLTableElement;
    function &new(): JHTMLTableElement;
  end;


function HTMLTableElement_var: JHTMLTableElement1;

type
  JTreeWalker = class external "Object"
    property whatToShow: integer;
    property filter: JNodeFilterCallback;
    property root: JNode;
    property currentNode: JNode;
    property expandEntityReferences: boolean;
    function previousSibling(): JNode;
    function lastChild(): JNode;
    function nextSibling(): JNode;
    function nextNode(): JNode;
    function parentNode(): JNode;
    function firstChild(): JNode;
    function previousNode(): JNode;
  end;

type
  JTreeWalker1 = class external "TreeWalker"
  public
    property prototype: JTreeWalker;
    function &new(): JTreeWalker;
  end;


function TreeWalker_var: JTreeWalker1;

type
  JGetSVGDocument = class external "Object"
    function getSVGDocument(): JSVGDocument;
  end;

type
  JHTMLHtmlElementDOML2Deprecated = class external "Object"
    property version: string;
  end;

type
  JSVGPathSeg = class external "Object"
    property pathSegType: integer;
    property pathSegTypeAsLetter: string;
    property PATHSEG_MOVETO_REL: integer;
    property PATHSEG_LINETO_VERTICAL_REL: integer;
    property PATHSEG_CURVETO_CUBIC_SMOOTH_ABS: integer;
    property PATHSEG_CURVETO_QUADRATIC_REL: integer;
    property PATHSEG_CURVETO_CUBIC_ABS: integer;
    property PATHSEG_LINETO_HORIZONTAL_ABS: integer;
    property PATHSEG_CURVETO_QUADRATIC_ABS: integer;
    property PATHSEG_LINETO_ABS: integer;
    property PATHSEG_CLOSEPATH: integer;
    property PATHSEG_LINETO_HORIZONTAL_REL: integer;
    property PATHSEG_CURVETO_CUBIC_SMOOTH_REL: integer;
    property PATHSEG_LINETO_REL: integer;
    property PATHSEG_CURVETO_QUADRATIC_SMOOTH_ABS: integer;
    property PATHSEG_ARC_REL: integer;
    property PATHSEG_CURVETO_CUBIC_REL: integer;
    property PATHSEG_UNKNOWN: integer;
    property PATHSEG_LINETO_VERTICAL_ABS: integer;
    property PATHSEG_ARC_ABS: integer;
    property PATHSEG_MOVETO_ABS: integer;
    property PATHSEG_CURVETO_QUADRATIC_SMOOTH_REL: integer;
  end;

type
  JSVGPathSeg1 = class external "SVGPathSeg"
  public
    property PATHSEG_MOVETO_REL: integer;
    property PATHSEG_LINETO_VERTICAL_REL: integer;
    property PATHSEG_CURVETO_CUBIC_SMOOTH_ABS: integer;
    property PATHSEG_CURVETO_QUADRATIC_REL: integer;
    property PATHSEG_CURVETO_CUBIC_ABS: integer;
    property PATHSEG_LINETO_HORIZONTAL_ABS: integer;
    property PATHSEG_CURVETO_QUADRATIC_ABS: integer;
    property PATHSEG_LINETO_ABS: integer;
    property PATHSEG_CLOSEPATH: integer;
    property PATHSEG_LINETO_HORIZONTAL_REL: integer;
    property PATHSEG_CURVETO_CUBIC_SMOOTH_REL: integer;
    property PATHSEG_LINETO_REL: integer;
    property PATHSEG_CURVETO_QUADRATIC_SMOOTH_ABS: integer;
    property PATHSEG_ARC_REL: integer;
    property PATHSEG_CURVETO_CUBIC_REL: integer;
    property PATHSEG_UNKNOWN: integer;
    property PATHSEG_LINETO_VERTICAL_ABS: integer;
    property PATHSEG_ARC_ABS: integer;
    property PATHSEG_MOVETO_ABS: integer;
    property PATHSEG_CURVETO_QUADRATIC_SMOOTH_REL: integer;
  end;


function SVGPathSeg_var: JSVGPathSeg1;

type
  JSVGPathSegCurvetoQuadraticRel = class external "Object"(JSVGPathSeg)
    property y: integer;
    property y1: integer;
    property x: integer;
    property x1: integer;
  end;

type
  JSVGPathSegCurvetoQuadraticRel1 = class external "SVGPathSegCurvetoQuadraticRel"
  public
    property prototype: JSVGPathSegCurvetoQuadraticRel;
    function &new(): JSVGPathSegCurvetoQuadraticRel;
  end;


function SVGPathSegCurvetoQuadraticRel_var: JSVGPathSegCurvetoQuadraticRel1;

type
  JPerformance = class external "Object"
    property navigation: JPerformanceNavigation;
    property timing: JPerformanceTiming;
    function toJSON(): variant;
  end;

type
  JPerformance1 = class external "Performance"
  public
    property prototype: JPerformance;
    function &new(): JPerformance;
  end;


function Performance_var: JPerformance1;

type
  TSVGSVGElementEventHandlers_onresize_ = function (ev: JUIEvent): variant;
  TSVGSVGElementEventHandlers_onunload_ = function (ev: JEvent): variant;
  TSVGSVGElementEventHandlers_onscroll_ = function (ev: JUIEvent): variant;
  TSVGSVGElementEventHandlers_onerror_ = function (ev: JEvent): variant;
  TSVGSVGElementEventHandlers_onzoom_ = function (ev: variant): variant;
  TSVGSVGElementEventHandlers_onabort_ = function (ev: JUIEvent): variant;
type
  JSVGSVGElementEventHandlers = class external "Object"
    property onresize: TSVGSVGElementEventHandlers_onresize_;
    property onunload: TSVGSVGElementEventHandlers_onunload_;
    property onscroll: TSVGSVGElementEventHandlers_onscroll_;
    property onerror: TSVGSVGElementEventHandlers_onerror_;
    property onzoom: TSVGSVGElementEventHandlers_onzoom_;
    property onabort: TSVGSVGElementEventHandlers_onabort_;
  end;

type
  JMSDataBindingTableExtensions = class external "Object"
    property dataPageSize: integer;
    procedure nextPage();
    procedure firstPage();
    procedure refresh();
    procedure previousPage();
    procedure lastPage();
  end;

type
  JDOML2DeprecatedAlignmentStyle_HTMLParagraphElement = class external "Object"
    property align: string;
  end;

type
  JMSEventExtensions = class external "Object"
    property cancelBubble: boolean;
    property srcElement: JElement;
  end;

type
  JEvent = class external "Object"(JMSEventExtensions)
    property timeStamp: integer;
    property defaultPrevented: boolean;
    property isTrusted: boolean;
    property currentTarget: JEventTarget;
    property target: JEventTarget;
    property eventPhase: integer;
    property &type: string;
    property cancelable: boolean;
    property bubbles: boolean;
    procedure initEvent(eventTypeArg: string; canBubbleArg: boolean; cancelableArg: boolean);
    procedure stopPropagation();
    procedure stopImmediatePropagation();
    procedure preventDefault();
    property CAPTURING_PHASE: integer;
    property AT_TARGET: integer;
    property BUBBLING_PHASE: integer;
  end;

type
  JEvent1 = class external "Event"
  public
    property prototype: JEvent;
    function &new(): JEvent;
    property CAPTURING_PHASE: integer;
    property AT_TARGET: integer;
    property BUBBLING_PHASE: integer;
  end;


function Event_var: JEvent1;

type
  JUIEvent = class external "Object"(JEvent)
    property detail: integer;
    property view: JAbstractView;
    procedure initUIEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; detailArg: integer);
  end;

type
  JUIEvent1 = class external "UIEvent"
  public
    property prototype: JUIEvent;
    function &new(): JUIEvent;
  end;


function UIEvent_var: JUIEvent1;


type
  JCompositionEvent = class external "Object"(JUIEvent)
    property data: string;
    property locale: string;
    procedure initCompositionEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; dataArg: string; locale: string);
  end;

type
  JCompositionEvent1 = class external "CompositionEvent"
  public
    property prototype: JCompositionEvent;
    function &new(): JCompositionEvent;
  end;


function CompositionEvent_var: JCompositionEvent1;

type
  JSVGElement = class external "Object"(JElement)//, JSVGElementEventHandlers)
    property xmlbase: string;
    property viewportElement: JSVGElement;
    property id: string;
    property ownerSVGElement: JSVGSVGElement;
  end;

type
  JSVGElement1 = class external "SVGElement"
  public
    property prototype: JSVGElement;
    function &new(): JSVGElement;
  end;


function SVGElement_var: JSVGElement1;

type
  JSVGMarkerElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGLangSpace)//, JSVGFitToViewBox)
    property orientType: JSVGAnimatedEnumeration;
    property markerUnits: JSVGAnimatedEnumeration;
    property markerWidth: JSVGAnimatedLength;
    property markerHeight: JSVGAnimatedLength;
    property orientAngle: JSVGAnimatedAngle;
    property refY: JSVGAnimatedLength;
    property refX: JSVGAnimatedLength;
    procedure setOrientToAngle(angle: JSVGAngle);
    procedure setOrientToAuto();
    property SVG_MARKER_ORIENT_UNKNOWN: integer;
    property SVG_MARKER_ORIENT_ANGLE: integer;
    property SVG_MARKERUNITS_UNKNOWN: integer;
    property SVG_MARKERUNITS_STROKEWIDTH: integer;
    property SVG_MARKER_ORIENT_AUTO: integer;
    property SVG_MARKERUNITS_USERSPACEONUSE: integer;
  end;

type
  JSVGMarkerElement1 = class external "SVGMarkerElement"
  public
    property prototype: JSVGMarkerElement;
    function &new(): JSVGMarkerElement;
    property SVG_MARKER_ORIENT_UNKNOWN: integer;
    property SVG_MARKER_ORIENT_ANGLE: integer;
    property SVG_MARKERUNITS_UNKNOWN: integer;
    property SVG_MARKERUNITS_STROKEWIDTH: integer;
    property SVG_MARKER_ORIENT_AUTO: integer;
    property SVG_MARKERUNITS_USERSPACEONUSE: integer;
  end;


function SVGMarkerElement_var: JSVGMarkerElement1;

type
  JWindowTimers = class external "Object"
    procedure clearTimeout(handle: integer);
    function setTimeout(expression: variant; msec: integer = 0; language: variant = undefined): integer;
    procedure clearInterval(handle: integer);
    function setInterval(expression: variant; msec: integer = 0; language: variant = undefined): integer;
  end;

type
  JSVGGElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)
  end;

type
  JSVGGElement1 = class external "SVGGElement"
  public
    property prototype: JSVGGElement;
    function &new(): JSVGGElement;
  end;


function SVGGElement_var: JSVGGElement1;

type
  JMSCSSStyleSheetExtensions = class external "Object"
    property owningElement: JElement;
    property imports: JStyleSheetList;
    property isAlternate: boolean;
    property rules: JMSCSSRuleList;
    property isPrefAlternate: boolean;
    property readOnly: boolean;
    property cssText: string;
    property href: string;
    property id: string;
    property pages: JStyleSheetPageList;
    function addImport(bstrURL: string; lIndex: integer = 0): integer;
    function addPageRule(bstrSelector: string; bstrStyle: string; lIndex: integer = 0): integer;
    procedure removeRule(lIndex: integer);
    function addRule(bstrSelector: string; bstrStyle: string = ""; lIndex: integer = 0): integer;
    procedure removeImport(lIndex: integer);
  end;

type
  JNavigator = class external "Object"(JNavigatorID)//, JNavigatorOnLine)//, JNavigatorDoNotTrack)//, JNavigatorAbilities)//, JNavigatorGeolocation)//, JMSNavigatorAbilities)
  end;

type
  JNavigator1 = class external "Navigator"
  public
    property prototype: JNavigator;
    function &new(): JNavigator;
  end;


function Navigator_var: JNavigator1;

type
  JSVGPathSegCurvetoCubicSmoothAbs = class external "Object"(JSVGPathSeg)
    property y: integer;
    property x2: integer;
    property x: integer;
    property y2: integer;
  end;

type
  JSVGPathSegCurvetoCubicSmoothAbs1 = class external "SVGPathSegCurvetoCubicSmoothAbs"
  public
    property prototype: JSVGPathSegCurvetoCubicSmoothAbs;
    function &new(): JSVGPathSegCurvetoCubicSmoothAbs;
  end;


function SVGPathSegCurvetoCubicSmoothAbs_var: JSVGPathSegCurvetoCubicSmoothAbs1;

type
  JMSBorderColorStyle_HTMLFrameSetElement = class external "Object"
    property borderColor: variant;
  end;

type
  JSVGZoomEvent = class external "Object"(JUIEvent)
    property zoomRectScreen: JSVGRect;
    property previousScale: integer;
    property newScale: integer;
    property previousTranslate: JSVGPoint;
    property newTranslate: JSVGPoint;
  end;

type
  JSVGZoomEvent1 = class external "SVGZoomEvent"
  public
    property prototype: JSVGZoomEvent;
    function &new(): JSVGZoomEvent;
  end;


function SVGZoomEvent_var: JSVGZoomEvent1;

type
  JNodeSelector = class external "Object"
    function querySelectorAll(selectors: string): JNodeList;
    function querySelector(selectors: string): JElement;
  end;

type
  JHTMLTableCellElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedTableCellHeight)//, JHTMLTableAlignment)//, JMSBorderColorHighlightStyle_HTMLTableCellElement)//, JDOML2DeprecatedWidthStyle_HTMLTableCellElement)//, JDOML2DeprecatedBackgroundStyle)//, JMSBorderColorStyle_HTMLTableCellElement)//, JMSHTMLTableCellElementExtensions)//, JDOML2DeprecatedAlignmentStyle_HTMLTableCellElement)//, JHTMLTableHeaderCellScope)//, JDOML2DeprecatedWordWrapSuppression)//, JDOML2DeprecatedBackgroundColorStyle)
    property headers: string;
    property abbr: string;
    property rowSpan: integer;
    property cellIndex: integer;
    property colSpan: integer;
    property axis: string;
  end;

type
  JHTMLTableCellElement1 = class external "HTMLTableCellElement"
  public
    property prototype: JHTMLTableCellElement;
    function &new(): JHTMLTableCellElement;
  end;


function HTMLTableCellElement_var: JHTMLTableCellElement1;

type
  JHTMLTableDataCellElement = class external "Object"(JHTMLTableCellElement)//, JMSHTMLTableDataCellElementExtensions)
  end;

type
  JHTMLTableDataCellElement1 = class external "HTMLTableDataCellElement"
  public
    property prototype: JHTMLTableDataCellElement;
    function &new(): JHTMLTableDataCellElement;
  end;


function HTMLTableDataCellElement_var: JHTMLTableDataCellElement1;

type
  JDOML2DeprecatedListNumberingAndBulletStyle = class external "Object"
    property &type: string;
  end;

type
  JMSHTMLDirectoryElementExtensions = class external "Object"(JDOML2DeprecatedListNumberingAndBulletStyle)
  end;

type
  JHTMLBaseElement = class external "Object"(JHTMLElement)
    property target: string;
    property href: string;
  end;

type
  JHTMLBaseElement1 = class external "HTMLBaseElement"
  public
    property prototype: JHTMLBaseElement;
    function &new(): JHTMLBaseElement;
  end;


function HTMLBaseElement_var: JHTMLBaseElement1;

type
  JPositionErrorCallback = class external "Object"
  end;


procedure PositionErrorCallback_(error: JPositionError);external;

type
  JDOMHTMLImplementation = class external "Object"
    function createHTMLDocument(title: string): JDocument;
  end;

type
  JDOMImplementation = class external "Object"(JDOMHTMLImplementation)
    function createDocumentType(qualifiedName: string; publicId: string; systemId: string): JDocumentType;
    function createDocument(namespaceURI: string; qualifiedName: string; doctype: JDocumentType): JDocument;
    function hasFeature(feature: string; version: string = ""): boolean;
  end;

type
  JDOMImplementation1 = class external "DOMImplementation"
  public
    property prototype: JDOMImplementation;
    function &new(): JDOMImplementation;
  end;


function DOMImplementation_var: JDOMImplementation1;

type
  JDOML2DeprecatedWidthStyle_HTMLBlockElement = class external "Object"
    property width: integer;
  end;

type
  JSVGUnitTypes = class external "Object"
    property SVG_UNIT_TYPE_UNKNOWN: integer;
    property SVG_UNIT_TYPE_OBJECTBOUNDINGBOX: integer;
    property SVG_UNIT_TYPE_USERSPACEONUSE: integer;
  end;

type
  JSVGUnitTypes1 = class external "SVGUnitTypes"
  public
    property prototype: JSVGUnitTypes;
    function &new(): JSVGUnitTypes;
    property SVG_UNIT_TYPE_UNKNOWN: integer;
    property SVG_UNIT_TYPE_OBJECTBOUNDINGBOX: integer;
    property SVG_UNIT_TYPE_USERSPACEONUSE: integer;
  end;


function SVGUnitTypes_var: JSVGUnitTypes1;

type
  JDocumentRange = class external "Object"
    function createRange(): JRange;
  end;

type
  TMSHTMLDocumentExtensions_onrowexit_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_oncontrolselect_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_onrowsinserted_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_onpropertychange_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_onafterupdate_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_onhelp_ = function (ev: JEvent): variant;
  TMSHTMLDocumentExtensions_onbeforeactivate_ = function (ev: JUIEvent): variant;
  TMSHTMLDocumentExtensions_onstoragecommit_ = function (ev: JStorageEvent): variant;
  TMSHTMLDocumentExtensions_onselectionchange_ = function (ev: JEvent): variant;
  TMSHTMLDocumentExtensions_onfocusout_ = function (ev: JFocusEvent): variant;
  TMSHTMLDocumentExtensions_ondataavailable_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_onbeforeupdate_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_onfocusin_ = function (ev: JFocusEvent): variant;
  TMSHTMLDocumentExtensions_ondatasetcomplete_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_onbeforedeactivate_ = function (ev: JUIEvent): variant;
  TMSHTMLDocumentExtensions_onstop_ = function (ev: JEvent): variant;
  TMSHTMLDocumentExtensions_onactivate_ = function (ev: JUIEvent): variant;
  TMSHTMLDocumentExtensions_onmssitemodejumplistitemremoved_ = function (ev: JMSSiteModeEvent): variant;
  TMSHTMLDocumentExtensions_onselectstart_ = function (ev: JEvent): variant;
  TMSHTMLDocumentExtensions_onerrorupdate_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_ondeactivate_ = function (ev: JUIEvent): variant;
  TMSHTMLDocumentExtensions_ondatasetchanged_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_onrowsdelete_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_onmsthumbnailclick_ = function (ev: JMSSiteModeEvent): variant;
  TMSHTMLDocumentExtensions_onrowenter_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_onbeforeeditfocus_ = function (ev: JMSEventObj): variant;
  TMSHTMLDocumentExtensions_oncellchange_ = function (ev: JMSEventObj): variant;
type
  JMSHTMLDocumentExtensions = class external "Object"
    property onrowexit: TMSHTMLDocumentExtensions_onrowexit_;
    property compatible: JMSCompatibleInfoCollection;
    property oncontrolselect: TMSHTMLDocumentExtensions_oncontrolselect_;
    property onrowsinserted: TMSHTMLDocumentExtensions_onrowsinserted_;
    property onpropertychange: TMSHTMLDocumentExtensions_onpropertychange_;
    property media: string;
    property onafterupdate: TMSHTMLDocumentExtensions_onafterupdate_;
    property onhelp: TMSHTMLDocumentExtensions_onhelp_;
    property uniqueID: string;
    property onbeforeactivate: TMSHTMLDocumentExtensions_onbeforeactivate_;
    property onstoragecommit: TMSHTMLDocumentExtensions_onstoragecommit_;
    property onselectionchange: TMSHTMLDocumentExtensions_onselectionchange_;
    property documentMode: integer;
    property onfocusout: TMSHTMLDocumentExtensions_onfocusout_;
    property ondataavailable: TMSHTMLDocumentExtensions_ondataavailable_;
    property onbeforeupdate: TMSHTMLDocumentExtensions_onbeforeupdate_;
    property onfocusin: TMSHTMLDocumentExtensions_onfocusin_;
    property security: string;
    property namespaces: JMSNamespaceInfoCollection;
    property ondatasetcomplete: TMSHTMLDocumentExtensions_ondatasetcomplete_;
    property onbeforedeactivate: TMSHTMLDocumentExtensions_onbeforedeactivate_;
    property onstop: TMSHTMLDocumentExtensions_onstop_;
    property onactivate: TMSHTMLDocumentExtensions_onactivate_;
    property onmssitemodejumplistitemremoved: TMSHTMLDocumentExtensions_onmssitemodejumplistitemremoved_;
    property frames: JWindow;
    property onselectstart: TMSHTMLDocumentExtensions_onselectstart_;
    property onerrorupdate: TMSHTMLDocumentExtensions_onerrorupdate_;
    property parentWindow: JWindow;
    property ondeactivate: TMSHTMLDocumentExtensions_ondeactivate_;
    property ondatasetchanged: TMSHTMLDocumentExtensions_ondatasetchanged_;
    property onrowsdelete: TMSHTMLDocumentExtensions_onrowsdelete_;
    property onmsthumbnailclick: TMSHTMLDocumentExtensions_onmsthumbnailclick_;
    property onrowenter: TMSHTMLDocumentExtensions_onrowenter_;
    property onbeforeeditfocus: TMSHTMLDocumentExtensions_onbeforeeditfocus_;
    property Script: JMSScriptHost;
    property oncellchange: TMSHTMLDocumentExtensions_oncellchange_;
    property URLUnencoded: string;
    procedure updateSettings();
    function execCommandShowHelp(commandId: string): boolean;
    procedure releaseCapture();
    procedure focus();
  end;

type
  JCSS2Properties = class external "Object"
    property backgroundAttachment: string;
    property visibility: string;
    property fontFamily: string;
    property borderRightStyle: string;
    property clear: string;
    property content: string;
    property counterIncrement: string;
    property orphans: string;
    property marginBottom: string;
    property borderStyle: string;
    property counterReset: string;
    property outlineWidth: string;
    property marginRight: string;
    property paddingLeft: string;
    property borderBottom: string;
    property marginTop: string;
    property borderTopColor: string;
    property top: string;
    property fontWeight: string;
    property textIndent: string;
    property borderRight: string;
    property width: string;
    property listStyleImage: string;
    property cursor: string;
    property listStylePosition: string;
    property borderTopStyle: string;
    property direction: string;
    property maxWidth: string;
    property color: string;
    property clip: string;
    property borderRightWidth: string;
    property verticalAlign: string;
    property pageBreakAfter: string;
    property overflow: string;
    property borderBottomStyle: string;
    property borderLeftStyle: string;
    property fontStretch: string;
    property emptyCells: string;
    property padding: string;
    property paddingRight: string;
    property background: string;
    property bottom: string;
    property height: string;
    property paddingTop: string;
    property right: string;
    property borderLeftWidth: string;
    property borderLeft: string;
    property backgroundPosition: string;
    property backgroundColor: string;
    property widows: string;
    property lineHeight: string;
    property pageBreakInside: string;
    property borderTopWidth: string;
    property left: string;
    property outlineStyle: string;
    property borderTop: string;
    property paddingBottom: string;
    property outlineColor: string;
    property wordSpacing: string;
    property outline: string;
    property font: string;
    property marginLeft: string;
    property display: string;
    property maxHeight: string;
    property cssFloat: string;
    property letterSpacing: string;
    property borderSpacing: string;
    property backgroundRepeat: string;
    property fontSizeAdjust: string;
    property borderLeftColor: string;
    property borderWidth: string;
    property backgroundImage: string;
    property listStyleType: string;
    property whiteSpace: string;
    property fontStyle: string;
    property borderBottomColor: string;
    property minWidth: string;
    property position: string;
    property zIndex: string;
    property borderColor: string;
    property listStyle: string;
    property captionSide: string;
    property borderCollapse: string;
    property fontVariant: string;
    property quotes: string;
    property tableLayout: string;
    property unicodeBidi: string;
    property borderBottomWidth: string;
    property minHeight: string;
    property textDecoration: string;
    property fontSize: string;
    property border: string;
    property pageBreakBefore: string;
    property textAlign: string;
    property textTransform: string;
    property margin: string;
    property borderRightColor: string;
  end;

type
  JMSImageResourceExtensions_HTMLInputElement = class external "Object"
    property dynsrc: string;
    property vrml: string;
    property lowsrc: string;
    property start: string;
    property loop: integer;
  end;

type
  JMSHTMLEmbedElementExtensions = class external "Object"
    property palette: string;
    property hidden: string;
    property pluginspage: string;
    property units: string;
  end;

type
  JMSHTMLModElementExtensions = class external "Object"
  end;

type
  JSVGDocument = class external "Object"
    property rootElement: JSVGSVGElement;
  end;

type
  JHTMLNextIdElement = class external "Object"(JHTMLElement)
    property n: string;
  end;

type
  JHTMLNextIdElement1 = class external "HTMLNextIdElement"
  public
    property prototype: JHTMLNextIdElement;
    function &new(): JHTMLNextIdElement;
  end;


function HTMLNextIdElement_var: JHTMLNextIdElement1;

type
  JSVGPathSegMovetoRel = class external "Object"(JSVGPathSeg)
    property y: integer;
    property x: integer;
  end;

type
  JSVGPathSegMovetoRel1 = class external "SVGPathSegMovetoRel"
  public
    property prototype: JSVGPathSegMovetoRel;
    function &new(): JSVGPathSegMovetoRel;
  end;


function SVGPathSegMovetoRel_var: JSVGPathSegMovetoRel1;

type
  JSVGLineElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)
    property y1: JSVGAnimatedLength;
    property x2: JSVGAnimatedLength;
    property x1: JSVGAnimatedLength;
    property y2: JSVGAnimatedLength;
  end;

type
  JSVGLineElement1 = class external "SVGLineElement"
  public
    property prototype: JSVGLineElement;
    function &new(): JSVGLineElement;
  end;


function SVGLineElement_var: JSVGLineElement1;

type
  JHTMLParagraphElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedAlignmentStyle_HTMLParagraphElement)//, JMSHTMLParagraphElementExtensions)
  end;

type
  JHTMLParagraphElement1 = class external "HTMLParagraphElement"
  public
    property prototype: JHTMLParagraphElement;
    function &new(): JHTMLParagraphElement;
  end;


function HTMLParagraphElement_var: JHTMLParagraphElement1;

type
  JMSHTMLTextAreaElementExtensions = class external "Object"
    property status: variant;
    function createTextRange(): JTextRange;
  end;

type
  JErrorFunction = class external "Object"
  end;


function ErrorFunction_(eventOrMessage: variant; source: string; fileno: integer): variant;external;

type
  JMSHTMLCollectionExtensions = class external "Object"
    function urns(urn: variant): JObject;
    function tags(tagName: variant): JObject;
  end;

type
  JHTMLCollection = class external "Object"(JMSHTMLCollectionExtensions)
    property length: integer;
    function item(nameOrIndex: variant = undefined; optionalIndex: variant = undefined): JElement;
    function namedItem(name: string): JElement;
    function  GetItems(index: integer): JElement; external array;
    procedure SetItems(index: integer; value: JElement); external array;
    property Items[index: integer]: JElement read GetItems write SetItems; default;
    //function  GetItems(name: string): JElement; external array;
    //procedure SetItems(name: string; value: JElement); external array;
    //property Items[name: string]: JElement read GetItems write SetItems; default;
  end;


function HTMLCollection_(nameOrIndex: variant; optionalIndex: variant): JElement;external;
//function HTMLCollection_(name: string): JElement;external;

type
  JHTMLCollection1 = class external "HTMLCollection"
  public
    property prototype: JHTMLCollection;
    function &new(): JHTMLCollection;
  end;


function HTMLCollection_var: JHTMLCollection1;

type
  JHTMLAreasCollection = class external "Object"(JHTMLCollection)
    procedure remove(index: integer = 0);
    procedure add(element: JHTMLElement; before: variant = undefined);
  end;

type
  JHTMLAreasCollection1 = class external "HTMLAreasCollection"
  public
    property prototype: JHTMLAreasCollection;
    function &new(): JHTMLAreasCollection;
  end;


function HTMLAreasCollection_var: JHTMLAreasCollection1;

type
  JSVGDescElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGLangSpace)
  end;

type
  JSVGDescElement1 = class external "SVGDescElement"
  public
    property prototype: JSVGDescElement;
    function &new(): JSVGDescElement;
  end;


function SVGDescElement_var: JSVGDescElement1;

type
  JMSHTMLLegendElementExtensions = class external "Object"
  end;

type
  JMSCSSStyleDeclarationExtensions = class external "Object"
    function getAttribute(attributeName: string; flags: integer = 0): variant;
    procedure setAttribute(attributeName: string; AttributeValue: variant; flags: integer = 0);
    function removeAttribute(attributeName: string; flags: integer = 0): boolean;
  end;

type
  JSVGPathSegCurvetoQuadraticSmoothRel = class external "Object"(JSVGPathSeg)
    property y: integer;
    property x: integer;
  end;

type
  JSVGPathSegCurvetoQuadraticSmoothRel1 = class external "SVGPathSegCurvetoQuadraticSmoothRel"
  public
    property prototype: JSVGPathSegCurvetoQuadraticSmoothRel;
    function &new(): JSVGPathSegCurvetoQuadraticSmoothRel;
  end;


function SVGPathSegCurvetoQuadraticSmoothRel_var: JSVGPathSegCurvetoQuadraticSmoothRel1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLTableRowElement = class external "Object"
    property align: string;
  end;

type
  JDOML2DeprecatedBorderStyle_HTMLObjectElement = class external "Object"
    property border: string;
  end;

type
  JMSHTMLSpanElementExtensions = class external "Object"
  end;

type
  JMSHTMLObjectElementExtensions = class external "Object"
    property &object: JObject;
    property alt: string;
    property classid: string;
    property altHtml: string;
    property BaseHref: string;
  end;

type
  JDOML2DeprecatedListSpaceReduction = class external "Object"
    property compact: boolean;
  end;

type
  JMSScriptHost = class external "Object"
  end;

type
  JMSScriptHost1 = class external "MSScriptHost"
  public
    property prototype: JMSScriptHost;
    function &new(): JMSScriptHost;
  end;


function MSScriptHost_var: JMSScriptHost1;

type
  JSVGClipPathElement = class external "Object"(JSVGElement)//, JSVGUnitTypes)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)
    property clipPathUnits: JSVGAnimatedEnumeration;
  end;

type
  JSVGClipPathElement1 = class external "SVGClipPathElement"
  public
    property prototype: JSVGClipPathElement;
    function &new(): JSVGClipPathElement;
  end;


function SVGClipPathElement_var: JSVGClipPathElement1;

type
  JMouseEvent = class external "Object"(JUIEvent)//, JMSMouseEventExtensions)
    property pageX: integer;
    property offsetY: integer;
    property x: integer;
    property y: integer;
    property altKey: boolean;
    property metaKey: boolean;
    property ctrlKey: boolean;
    property offsetX: integer;
    property screenX: integer;
    property clientY: integer;
    property shiftKey: boolean;
    property screenY: integer;
    property relatedTarget: JEventTarget;
    property button: integer;
    property pageY: integer;
    property buttons: integer;
    property clientX: integer;
    procedure initMouseEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; detailArg: integer; screenXArg: integer; screenYArg: integer; clientXArg: integer; clientYArg: integer; ctrlKeyArg: boolean; altKeyArg: boolean; shiftKeyArg: boolean; metaKeyArg: boolean; buttonArg: integer; relatedTargetArg: JEventTarget);
    function getModifierState(keyArg: string): boolean;
  end;

type
  JMouseEvent1 = class external "MouseEvent"
  public
    property prototype: JMouseEvent;
    function &new(): JMouseEvent;
  end;


function MouseEvent_var: JMouseEvent1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLTableElement = class external "Object"
    property align: string;
  end;

type
  JRangeException = class external "Object"
    property code: integer;
    property message: string;
    function toString(): string;
    property INVALID_NODE_TYPE_ERR: integer;
    property BAD_BOUNDARYPOINTS_ERR: integer;
  end;

type
  JRangeException1 = class external "RangeException"
  public
    property prototype: JRangeException;
    function &new(): JRangeException;
    property INVALID_NODE_TYPE_ERR: integer;
    property BAD_BOUNDARYPOINTS_ERR: integer;
  end;


function RangeException_var: JRangeException1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLHRElement = class external "Object"
    property align: string;
  end;

type
  JSVGTextContentElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGLangSpace)//, JSVGTests)
    property textLength: JSVGAnimatedLength;
    property lengthAdjust: JSVGAnimatedEnumeration;
    function getCharNumAtPosition(point: JSVGPoint): integer;
    function getStartPositionOfChar(charnum: integer): JSVGPoint;
    function getExtentOfChar(charnum: integer): JSVGRect;
    function getComputedTextLength(): integer;
    function getSubStringLength(charnum: integer; nchars: integer): integer;
    procedure selectSubString(charnum: integer; nchars: integer);
    function getNumberOfChars(): integer;
    function getRotationOfChar(charnum: integer): integer;
    function getEndPositionOfChar(charnum: integer): JSVGPoint;
    property LENGTHADJUST_SPACING: integer;
    property LENGTHADJUST_SPACINGANDGLYPHS: integer;
    property LENGTHADJUST_UNKNOWN: integer;
  end;

type
  JSVGTextContentElement1 = class external "SVGTextContentElement"
  public
    property prototype: JSVGTextContentElement;
    function &new(): JSVGTextContentElement;
    property LENGTHADJUST_SPACING: integer;
    property LENGTHADJUST_SPACINGANDGLYPHS: integer;
    property LENGTHADJUST_UNKNOWN: integer;
  end;


function SVGTextContentElement_var: JSVGTextContentElement1;

type
  JSVGTextPositioningElement = class external "Object"(JSVGTextContentElement)
    property y: JSVGAnimatedLengthList;
    property rotate: JSVGAnimatedNumberList;
    property dy: JSVGAnimatedLengthList;
    property x: JSVGAnimatedLengthList;
    property dx: JSVGAnimatedLengthList;
  end;

type
  JSVGTextPositioningElement1 = class external "SVGTextPositioningElement"
  public
    property prototype: JSVGTextPositioningElement;
    function &new(): JSVGTextPositioningElement;
  end;


function SVGTextPositioningElement_var: JSVGTextPositioningElement1;

type
  JHTMLAppletElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedWidthStyle_HTMLAppletElement)//, JDOML2DeprecatedMarginStyle_HTMLObjectElement)//, JMSHTMLAppletElementExtensions)//, JMSDataBindingExtensions)//, JMSDataBindingRecordSetExtensions)//, JDOML2DeprecatedAlignmentStyle_HTMLObjectElement)
    property &object: string;
    property archive: string;
    property codeBase: string;
    property alt: string;
    property name: string;
    property height: string;
    property code: string;
  end;

type
  JHTMLAppletElement1 = class external "HTMLAppletElement"
  public
    property prototype: JHTMLAppletElement;
    function &new(): JHTMLAppletElement;
  end;


function HTMLAppletElement_var: JHTMLAppletElement1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLFieldSetElement = class external "Object"
    property align: string;
  end;

type
  JMSHTMLFieldSetElementExtensions = class external "Object"(JDOML2DeprecatedAlignmentStyle_HTMLFieldSetElement)
  end;

type
  JDocumentEvent = class external "Object"
    function createEvent(eventInterface: string): JEvent;
  end;

type
  JMSHTMLUnknownElementExtensions = class external "Object"
  end;

type
  JTextMetrics = class external "Object"
    property width: integer;
  end;

type
  JTextMetrics1 = class external "TextMetrics"
  public
    property prototype: JTextMetrics;
    function &new(): JTextMetrics;
  end;


function TextMetrics_var: JTextMetrics1;

type
  JDOML2DeprecatedWordWrapSuppression_HTMLBodyElement = class external "Object"
    property noWrap: boolean;
  end;

type
  JHTMLOListElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedListNumberingAndBulletStyle)//, JDOML2DeprecatedListSpaceReduction)//, JMSHTMLOListElementExtensions)
    property start: integer;
  end;

type
  JHTMLOListElement1 = class external "HTMLOListElement"
  public
    property prototype: JHTMLOListElement;
    function &new(): JHTMLOListElement;
  end;


function HTMLOListElement_var: JHTMLOListElement1;

type
  JMSHTMLTableCaptionElementExtensions = class external "Object"
    property vAlign: string;
  end;

type
  JSVGAnimatedString = class external "Object"
    property animVal: string;
    property baseVal: string;
  end;

type
  JSVGAnimatedString1 = class external "SVGAnimatedString"
  public
    property prototype: JSVGAnimatedString;
    function &new(): JSVGAnimatedString;
  end;


function SVGAnimatedString_var: JSVGAnimatedString1;

type
  JSVGPathSegLinetoVerticalRel = class external "Object"(JSVGPathSeg)
    property y: integer;
  end;

type
  JSVGPathSegLinetoVerticalRel1 = class external "SVGPathSegLinetoVerticalRel"
  public
    property prototype: JSVGPathSegLinetoVerticalRel;
    function &new(): JSVGPathSegLinetoVerticalRel;
  end;


function SVGPathSegLinetoVerticalRel_var: JSVGPathSegLinetoVerticalRel1;

type
  JCharacterData = class external "Object"(JNode)
    property length: integer;
    property data: string;
    procedure deleteData(offset: integer; count: integer);
    procedure replaceData(offset: integer; count: integer; arg: string);
    procedure appendData(arg: string);
    procedure insertData(offset: integer; arg: string);
    function substringData(offset: integer; count: integer): string;
  end;

type
  JCharacterData1 = class external "CharacterData"
  public
    property prototype: JCharacterData;
    function &new(): JCharacterData;
  end;


function CharacterData_var: JCharacterData1;

type
  JText = class external "Object"(JCharacterData)//, JMSNodeExtensions)
    property wholeText: string;
    function splitText(offset: integer): JText;
    function replaceWholeText(content: string): JText;
  end;

type
  JText1 = class external "Text"
  public
    property prototype: JText;
    function &new(): JText;
  end;

function Text_var: JText1;

type
  JCDATASection = class external "Object"(JText)
  end;

type
  JCDATASection1 = class external "CDATASection"
  public
    property prototype: JCDATASection;
    function &new(): JCDATASection;
  end;


function CDATASection_var: JCDATASection1;

type
  JStyleMedia = class external "Object"
    property &type: string;
    function matchMedium(mediaquery: string): boolean;
  end;

type
  JStyleMedia1 = class external "StyleMedia"
  public
    property prototype: JStyleMedia;
    function &new(): JStyleMedia;
  end;


function StyleMedia_var: JStyleMedia1;

type
  JTextRange = class external "Object"
    property boundingLeft: integer;
    property htmlText: string;
    property offsetLeft: integer;
    property boundingWidth: integer;
    property boundingHeight: integer;
    property boundingTop: integer;
    property text: string;
    property offsetTop: integer;
    procedure moveToPoint(x: integer; y: integer);
    function queryCommandValue(cmdID: string): variant;
    function getBookmark(): string;
    function move(&Unit: string; Count: integer = 0): integer;
    function queryCommandIndeterm(cmdID: string): boolean;
    procedure scrollIntoView(fStart: boolean = false);
    function findText(string_: string; count: integer = 0; flags: integer = 0): boolean;
    function execCommand(cmdID: string; showUI: boolean = false; value: variant = undefined): boolean;
    function getBoundingClientRect(): JClientRect;
    function moveToBookmark(Bookmark: string): boolean;
    function isEqual(range: JTextRange): boolean;
    function duplicate(): JTextRange;
    procedure collapse(Start: boolean = false);
    function queryCommandText(cmdID: string): string;
    procedure select();
    procedure pasteHTML(html: string);
    function inRange(range: JTextRange): boolean;
    function moveEnd(&Unit: string; Count: integer = 0): integer;
    function getClientRects(): JClientRectList;
    function moveStart(&Unit: string; Count: integer = 0): integer;
    function parentElement(): JElement;
    function queryCommandState(cmdID: string): boolean;
    function compareEndPoints(how: string; sourceRange: JTextRange): integer;
    function execCommandShowHelp(cmdID: string): boolean;
    procedure moveToElementText(element: JElement);
    function expand(&Unit: string): boolean;
    function queryCommandSupported(cmdID: string): boolean;
    procedure setEndPoint(how: string; SourceRange: JTextRange);
    function queryCommandEnabled(cmdID: string): boolean;
  end;

type
  JTextRange1 = class external "TextRange"
  public
    property prototype: JTextRange;
    function &new(): JTextRange;
  end;


function TextRange_var: JTextRange1;

type
  JHTMLSelectElement = class external "Object"(JHTMLElement)//, JMSHTMLCollectionExtensions)//, JMSDataBindingExtensions)//, JMSHTMLSelectElementExtensions)
    property options: JHTMLSelectElement;
    property value: string;
    property form: JHTMLFormElement;
    property name: string;
    property size: integer;
    property length: integer;
    property selectedIndex: integer;
    property multiple: boolean;
    property &type: string;
    procedure remove(index: integer = 0);
    procedure add(element: JHTMLElement; before: variant = undefined);
    function item(name: variant = undefined; index: variant = undefined): variant;
    function namedItem(name: string): variant;
    function  GetItems(name: string): variant; external array;
    procedure SetItems(name: string; value: variant); external array;
    property Items[name: string]: variant read GetItems write SetItems; default;
  end;


function HTMLSelectElement_(name: variant; index: variant): variant;external;
//function HTMLSelectElement_(name: string): variant;external;

type
  JHTMLSelectElement1 = class external "HTMLSelectElement"
  public
    property prototype: JHTMLSelectElement;
    function &new(): JHTMLSelectElement;
  end;


function HTMLSelectElement_var: JHTMLSelectElement1;

type
  JStyleSheet = class external "Object"
    property disabled: boolean;
    property ownerNode: JNode;
    property parentStyleSheet: JStyleSheet;
    property href: string;
    property media: JMediaList;
    property &type: string;
    property title: string;
  end;

type
  JStyleSheet1 = class external "StyleSheet"
  public
    property prototype: JStyleSheet;
    function &new(): JStyleSheet;
  end;


function StyleSheet_var: JStyleSheet1;

type
  JCSSStyleSheet = class external "Object"(JStyleSheet)//, JMSCSSStyleSheetExtensions)
    property ownerRule: JCSSRule;
    property cssRules: JCSSRuleList;
    function insertRule(rule: string; index: integer = 0): integer;
    procedure deleteRule(index: integer = 0);
  end;

type
  JCSSStyleSheet1 = class external "CSSStyleSheet"
  public
    property prototype: JCSSStyleSheet;
    function &new(): JCSSStyleSheet;
  end;


function CSSStyleSheet_var: JCSSStyleSheet1;

type
  JHTMLBlockElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedTextFlowControl_HTMLBlockElement)//, JDOML2DeprecatedWidthStyle_HTMLBlockElement)
    property cite: string;
  end;

type
  JHTMLBlockElement1 = class external "HTMLBlockElement"
  public
    property prototype: JHTMLBlockElement;
    function &new(): JHTMLBlockElement;
  end;


function HTMLBlockElement_var: JHTMLBlockElement1;

type
  JSVGTests = class external "Object"
    property requiredFeatures: JSVGStringList;
    property requiredExtensions: JSVGStringList;
    property systemLanguage: JSVGStringList;
    function hasExtension(extension: string): boolean;
  end;

type
  JMSSelection = class external "Object"
    property &type: string;
    property typeDetail: string;
    function createRange(): JTextRange;
    procedure clear();
    function createRangeCollection(): JTextRangeCollection;
    procedure empty();
  end;

type
  JMSSelection1 = class external "MSSelection"
  public
    property prototype: JMSSelection;
    function &new(): JMSSelection;
  end;


function MSSelection_var: JMSSelection1;

type
  JMSHTMLDListElementExtensions = class external "Object"
  end;

type
  JHTMLMetaElement = class external "Object"(JHTMLElement)//, JMSHTMLMetaElementExtensions)
    property httpEquiv: string;
    property name: string;
    property content: string;
    property scheme: string;
  end;

type
  JHTMLMetaElement1 = class external "HTMLMetaElement"
  public
    property prototype: JHTMLMetaElement;
    function &new(): JHTMLMetaElement;
  end;


function HTMLMetaElement_var: JHTMLMetaElement1;

type
  JSelection = class external "Object"
    property isCollapsed: boolean;
    property anchorNode: JNode;
    property focusNode: JNode;
    property anchorOffset: integer;
    property focusOffset: integer;
    property rangeCount: integer;
    procedure addRange(range: JRange);
    procedure collapseToEnd();
    function toString(): string;
    procedure selectAllChildren(parentNode: JNode);
    function getRangeAt(index: integer): JRange;
    procedure collapse(parentNode: JNode; offset: integer);
    procedure removeAllRanges();
    procedure collapseToStart();
    procedure deleteFromDocument();
    procedure removeRange(range: JRange);
  end;

type
  JSelection1 = class external "Selection"
  public
    property prototype: JSelection;
    function &new(): JSelection;
  end;


function Selection_var: JSelection1;

type
  JSVGAnimatedAngle = class external "Object"
    property animVal: JSVGAngle;
    property baseVal: JSVGAngle;
  end;

type
  JSVGAnimatedAngle1 = class external "SVGAnimatedAngle"
  public
    property prototype: JSVGAnimatedAngle;
    function &new(): JSVGAnimatedAngle;
  end;


function SVGAnimatedAngle_var: JSVGAnimatedAngle1;

type
  JSVGPatternElement = class external "Object"(JSVGElement)//, JSVGUnitTypes)//, JSVGStylable)//, JSVGLangSpace)//, JSVGTests)//, JSVGFitToViewBox)//, JSVGURIReference)
    property patternUnits: JSVGAnimatedEnumeration;
    property y: JSVGAnimatedLength;
    property width: JSVGAnimatedLength;
    property x: JSVGAnimatedLength;
    property patternContentUnits: JSVGAnimatedEnumeration;
    property patternTransform: JSVGAnimatedTransformList;
    property height: JSVGAnimatedLength;
  end;

type
  JSVGPatternElement1 = class external "SVGPatternElement"
  public
    property prototype: JSVGPatternElement;
    function &new(): JSVGPatternElement;
  end;


function SVGPatternElement_var: JSVGPatternElement1;

type
  JSVGScriptElement = class external "Object"(JSVGElement)//, JSVGURIReference)
    property &type: string;
  end;

type
  JSVGScriptElement1 = class external "SVGScriptElement"
  public
    property prototype: JSVGScriptElement;
    function &new(): JSVGScriptElement;
  end;


function SVGScriptElement_var: JSVGScriptElement1;

type
  JHTMLDDElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedWordWrapSuppression_HTMLDDElement)
  end;

type
  JHTMLDDElement1 = class external "HTMLDDElement"
  public
    property prototype: JHTMLDDElement;
    function &new(): JHTMLDDElement;
  end;


function HTMLDDElement_var: JHTMLDDElement1;

type
  JNodeIterator = class external "Object"
    property whatToShow: integer;
    property filter: JNodeFilterCallback;
    property root: JNode;
    property expandEntityReferences: boolean;
    function nextNode(): JNode;
    procedure detach();
    function previousNode(): JNode;
  end;

type
  JNodeIterator1 = class external "NodeIterator"
  public
    property prototype: JNodeIterator;
    function &new(): JNodeIterator;
  end;


function NodeIterator_var: JNodeIterator1;

type
  JCSSRule = class external "Object"
    property cssText: string;
    property parentStyleSheet: JCSSStyleSheet;
    property parentRule: JCSSRule;
    property &type: integer;
    property IMPORT_RULE: integer;
    property MEDIA_RULE: integer;
    property STYLE_RULE: integer;
    property NAMESPACE_RULE: integer;
    property PAGE_RULE: integer;
    property UNKNOWN_RULE: integer;
    property FONT_FACE_RULE: integer;
    property CHARSET_RULE: integer;
  end;

type
  JCSSRule1 = class external "CSSRule"
  public
    property prototype: JCSSRule;
    function &new(): JCSSRule;
    property IMPORT_RULE: integer;
    property MEDIA_RULE: integer;
    property STYLE_RULE: integer;
    property NAMESPACE_RULE: integer;
    property PAGE_RULE: integer;
    property UNKNOWN_RULE: integer;
    property FONT_FACE_RULE: integer;
    property CHARSET_RULE: integer;
  end;


function CSSRule_var: JCSSRule1;

type
  JCSSStyleRule = class external "Object"(JCSSRule)//, JMSCSSStyleRuleExtensions)
    property selectorText: string;
    property style: JMSStyleCSSProperties;
  end;

type
  JCSSStyleRule1 = class external "CSSStyleRule"
  public
    property prototype: JCSSStyleRule;
    function &new(): JCSSStyleRule;
  end;


function CSSStyleRule_var: JCSSStyleRule1;

type
  JMSDataBindingRecordSetReadonlyExtensions = class external "Object"
    property recordset: JObject;
    function namedRecordset(dataMember: string; hierarchy: variant = undefined): JObject;
  end;

type
  JHTMLLinkElement = class external "Object"(JHTMLElement)//, JMSLinkStyleExtensions)//, JLinkStyle)
    property rel: string;
    property target: string;
    property href: string;
    property media: string;
    property rev: string;
    property &type: string;
    property charset: string;
    property hreflang: string;
  end;

type
  JHTMLLinkElement1 = class external "HTMLLinkElement"
  public
    property prototype: JHTMLLinkElement;
    function &new(): JHTMLLinkElement;
  end;


function HTMLLinkElement_var: JHTMLLinkElement1;

type
  JSVGViewElement = class external "Object"(JSVGElement)//, JSVGZoomAndPan)//, JSVGFitToViewBox)
    property viewTarget: JSVGStringList;
  end;

type
  JSVGViewElement1 = class external "SVGViewElement"
  public
    property prototype: JSVGViewElement;
    function &new(): JSVGViewElement;
  end;


function SVGViewElement_var: JSVGViewElement1;

type
  JMSHTMLAppletElementExtensions = class external "Object"(JDOML2DeprecatedBorderStyle_HTMLObjectElement)
    property codeType: string;
    property standby: string;
    property classid: string;
    property useMap: string;
    property form: JHTMLFormElement;
    property data: string;
    property contentDocument: JDocument;
    property altHtml: string;
    property declare: boolean;
    property &type: string;
    property BaseHref: string;
  end;

type
  JSVGLocatable = class external "Object"
    property farthestViewportElement: JSVGElement;
    property nearestViewportElement: JSVGElement;
    function getBBox(): JSVGRect;
    function getTransformToElement(element: JSVGElement): JSVGMatrix;
    function getCTM(): JSVGMatrix;
    function getScreenCTM(): JSVGMatrix;
  end;

type
  JHTMLFontElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedColorProperty)//, JMSHTMLFontElementExtensions)//, JDOML2DeprecatedSizeProperty)
    property face: string;
  end;

type
  JHTMLFontElement1 = class external "HTMLFontElement"
  public
    property prototype: JHTMLFontElement;
    function &new(): JHTMLFontElement;
  end;


function HTMLFontElement_var: JHTMLFontElement1;

type
  JMSHTMLTableElementExtensions = class external "Object"
    property cells: JHTMLCollection;
    property height: variant;
    property cols: integer;
    function moveRow(indexFrom: integer = 0; indexTo: integer = 0): JObject;
  end;

type
  JSVGTitleElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGLangSpace)
  end;

type
  JSVGTitleElement1 = class external "SVGTitleElement"
  public
    property prototype: JSVGTitleElement;
    function &new(): JSVGTitleElement;
  end;


function SVGTitleElement_var: JSVGTitleElement1;

type
  JControlRangeCollection = class external "Object"
    property length: integer;
    function queryCommandValue(cmdID: string): variant;
    procedure remove(index: integer);
    procedure add(item: JElement);
    function queryCommandIndeterm(cmdID: string): boolean;
    procedure scrollIntoView(varargStart: variant = undefined);
    function item(index: integer): JElement;
    function  GetItems(index: integer): JElement; external array;
    procedure SetItems(index: integer; value: JElement); external array;
    property Items[index: integer]: JElement read GetItems write SetItems; default;
    function execCommand(cmdID: string; showUI: boolean = false; value: variant = undefined): boolean;
    procedure addElement(item: JElement);
    function queryCommandState(cmdID: string): boolean;
    function queryCommandSupported(cmdID: string): boolean;
    function queryCommandEnabled(cmdID: string): boolean;
    function queryCommandText(cmdID: string): string;
    procedure select();
  end;

type
  JControlRangeCollection1 = class external "ControlRangeCollection"
  public
    property prototype: JControlRangeCollection;
    function &new(): JControlRangeCollection;
  end;


function ControlRangeCollection_var: JControlRangeCollection1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLImageElement = class external "Object"
    property align: string;
  end;

type
  TMSHTMLFrameElementExtensions_onload_ = function (ev: JEvent): variant;
type
  JMSHTMLFrameElementExtensions = class external "Object"
    property width: variant;
    property contentWindow: JWindow;
    property onload: TMSHTMLFrameElementExtensions_onload_;
    property frameBorder: string;
    property height: variant;
    property border: string;
    property frameSpacing: variant;
  end;

type
  JMSEventAttachmentTarget = class external "Object"
    function attachEvent(event: string; listener: JEventListener): boolean;
    procedure detachEvent(event: string; listener: JEventListener);
  end;

type
  TMSNamespaceInfo_onreadystatechange_ = function (ev: JEvent): variant;
type
  JMSNamespaceInfo = class external "Object"(JMSEventAttachmentTarget)
    property urn: string;
    property onreadystatechange: TMSNamespaceInfo_onreadystatechange_;
    property name: string;
    property readyState: string;
    procedure doImport(implementationUrl: string);
  end;

type
  JMSNamespaceInfo1 = class external "MSNamespaceInfo"
  public
    property prototype: JMSNamespaceInfo;
    function &new(): JMSNamespaceInfo;
  end;


function MSNamespaceInfo_var: JMSNamespaceInfo1;

type
  JWindowSessionStorage = class external "Object"
    property sessionStorage: JStorage;
  end;

type
  JSVGAnimatedTransformList = class external "Object"
    property animVal: JSVGTransformList;
    property baseVal: JSVGTransformList;
  end;

type
  JSVGAnimatedTransformList1 = class external "SVGAnimatedTransformList"
  public
    property prototype: JSVGAnimatedTransformList;
    function &new(): JSVGAnimatedTransformList;
  end;


function SVGAnimatedTransformList_var: JSVGAnimatedTransformList1;

type
  JHTMLTableCaptionElement = class external "Object"(JHTMLElement)//, JMSHTMLTableCaptionElementExtensions)//, JDOML2DeprecatedAlignmentStyle_HTMLTableCaptionElement)
  end;

type
  JHTMLTableCaptionElement1 = class external "HTMLTableCaptionElement"
  public
    property prototype: JHTMLTableCaptionElement;
    function &new(): JHTMLTableCaptionElement;
  end;


function HTMLTableCaptionElement_var: JHTMLTableCaptionElement1;

type
  JHTMLOptionElement = class external "Object"(JHTMLElement)//, JMSDataBindingExtensions)
    property index: integer;
    property defaultSelected: boolean;
    property value: string;
    property text: string;
    property form: JHTMLFormElement;
    property label: string;
    property selected: boolean;
  end;

type
  JHTMLOptionElement1 = class external "HTMLOptionElement"
  public
    property prototype: JHTMLOptionElement;
    function &new(): JHTMLOptionElement;
  end;


function HTMLOptionElement_var: JHTMLOptionElement1;

type
  JHTMLMapElement = class external "Object"(JHTMLElement)
    property name: string;
    property areas: JHTMLAreasCollection;
  end;

type
  JHTMLMapElement1 = class external "HTMLMapElement"
  public
    property prototype: JHTMLMapElement;
    function &new(): JHTMLMapElement;
  end;


function HTMLMapElement_var: JHTMLMapElement1;

type
  JHTMLMenuElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedListSpaceReduction)//, JMSHTMLMenuElementExtensions)
    property &type: string;
  end;

type
  JHTMLMenuElement1 = class external "HTMLMenuElement"
  public
    property prototype: JHTMLMenuElement;
    function &new(): JHTMLMenuElement;
  end;


function HTMLMenuElement_var: JHTMLMenuElement1;

type
  JMouseWheelEvent = class external "Object"(JMouseEvent)
    property wheelDelta: integer;
    procedure initMouseWheelEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; detailArg: integer; screenXArg: integer; screenYArg: integer; clientXArg: integer; clientYArg: integer; buttonArg: integer; relatedTargetArg: JEventTarget; modifiersListArg: string; wheelDeltaArg: integer);
  end;

type
  JMouseWheelEvent1 = class external "MouseWheelEvent"
  public
    property prototype: JMouseWheelEvent;
    function &new(): JMouseWheelEvent;
  end;


function MouseWheelEvent_var: JMouseWheelEvent1;

type
  JSVGFitToViewBox = class external "Object"
    property viewBox: JSVGAnimatedRect;
    property preserveAspectRatio: JSVGAnimatedPreserveAspectRatio;
  end;

type
  JMSHTMLAnchorElementExtensions = class external "Object"
    property nameProp: string;
    property protocolLong: string;
    property urn: string;
    property mimeType: string;
    property Methods: string;
  end;

type
  JSVGPointList = class external "Object"
    property numberOfItems: integer;
    function replaceItem(newItem: JSVGPoint; index: integer): JSVGPoint;
    function getItem(index: integer): JSVGPoint;
    procedure clear();
    function appendItem(newItem: JSVGPoint): JSVGPoint;
    function initialize(newItem: JSVGPoint): JSVGPoint;
    function removeItem(index: integer): JSVGPoint;
    function insertItemBefore(newItem: JSVGPoint; index: integer): JSVGPoint;
  end;

type
  JSVGPointList1 = class external "SVGPointList"
  public
    property prototype: JSVGPointList;
    function &new(): JSVGPointList;
  end;


function SVGPointList_var: JSVGPointList1;

type
  JMSElementCSSInlineStyleExtensions = class external "Object"
    procedure doScroll(component: variant = undefined);
    function componentFromPoint(x: integer; y: integer): string;
  end;

type
  JSVGAnimatedLengthList = class external "Object"
    property animVal: JSVGLengthList;
    property baseVal: JSVGLengthList;
  end;

type
  JSVGAnimatedLengthList1 = class external "SVGAnimatedLengthList"
  public
    property prototype: JSVGAnimatedLengthList;
    function &new(): JSVGAnimatedLengthList;
  end;


function SVGAnimatedLengthList_var: JSVGAnimatedLengthList1;

type
  JMSHTMLTableDataCellElementExtensions = class external "Object"
  end;

type
  TWindow_ondragend_ = function (ev: JDragEvent): variant;
  TWindow_onkeydown_ = function (ev: JKeyboardEvent): variant;
  TWindow_ondragover_ = function (ev: JDragEvent): variant;
  TWindow_onkeyup_ = function (ev: JKeyboardEvent): variant;
  TWindow_onreset_ = function (ev: JEvent): variant;
  TWindow_onmouseup_ = function (ev: JMouseEvent): variant;
  TWindow_ondragstart_ = function (ev: JDragEvent): variant;
  TWindow_ondrag_ = function (ev: JDragEvent): variant;
  TWindow_onmouseover_ = function (ev: JMouseEvent): variant;
  TWindow_ondragleave_ = function (ev: JDragEvent): variant;
  TWindow_onafterprint_ = function (ev: JEvent): variant;
  TWindow_onpause_ = function (ev: JEvent): variant;
  TWindow_onbeforeprint_ = function (ev: JEvent): variant;
  TWindow_onmousedown_ = function (ev: JMouseEvent): variant;
  TWindow_onseeked_ = function (ev: JEvent): variant;
  TWindow_onclick_ = function (ev: JMouseEvent): variant;
  TWindow_onwaiting_ = function (ev: JEvent): variant;
  TWindow_ononline_ = function (ev: JEvent): variant;
  TWindow_ondurationchange_ = function (ev: JEvent): variant;
  TWindow_onblur_ = function (ev: JFocusEvent): variant;
  TWindow_onemptied_ = function (ev: JEvent): variant;
  TWindow_onseeking_ = function (ev: JEvent): variant;
  TWindow_oncanplay_ = function (ev: JEvent): variant;
  TWindow_onstalled_ = function (ev: JEvent): variant;
  TWindow_onmousemove_ = function (ev: JMouseEvent): variant;
  TWindow_onoffline_ = function (ev: JEvent): variant;
  TWindow_onbeforeunload_ = function (ev: JBeforeUnloadEvent): variant;
  TWindow_onratechange_ = function (ev: JEvent): variant;
  TWindow_onstorage_ = function (ev: JStorageEvent): variant;
  TWindow_onloadstart_ = function (ev: JEvent): variant;
  TWindow_ondragenter_ = function (ev: JDragEvent): variant;
  TWindow_onsubmit_ = function (ev: JEvent): variant;
  TWindow_onprogress_ = function (ev: variant): variant;
  TWindow_ondblclick_ = function (ev: JMouseEvent): variant;
  TWindow_oncontextmenu_ = function (ev: JMouseEvent): variant;
  TWindow_onchange_ = function (ev: JEvent): variant;
  TWindow_onloadedmetadata_ = function (ev: JEvent): variant;
  TWindow_onplay_ = function (ev: JEvent): variant;
  TWindow_onplaying_ = function (ev: JEvent): variant;
  TWindow_oncanplaythrough_ = function (ev: JEvent): variant;
  TWindow_onabort_ = function (ev: JUIEvent): variant;
  TWindow_onreadystatechange_ = function (ev: JEvent): variant;
  TWindow_onkeypress_ = function (ev: JKeyboardEvent): variant;
  TWindow_onloadeddata_ = function (ev: JEvent): variant;
  TWindow_onsuspend_ = function (ev: JEvent): variant;
  TWindow_onfocus_ = function (ev: JFocusEvent): variant;
  TWindow_onmessage_ = function (ev: JMessageEvent): variant;
  TWindow_ontimeupdate_ = function (ev: JEvent): variant;
  TWindow_onresize_ = function (ev: JUIEvent): variant;
  TWindow_onselect_ = function (ev: JUIEvent): variant;
  TWindow_ondrop_ = function (ev: JDragEvent): variant;
  TWindow_onmouseout_ = function (ev: JMouseEvent): variant;
  TWindow_onended_ = function (ev: JEvent): variant;
  TWindow_onhashchange_ = function (ev: JEvent): variant;
  TWindow_onunload_ = function (ev: JEvent): variant;
  TWindow_onscroll_ = function (ev: JUIEvent): variant;
  TWindow_onmousewheel_ = function (ev: JMouseWheelEvent): variant;
  TWindow_onload_ = function (ev: JEvent): variant;
  TWindow_onvolumechange_ = function (ev: JEvent): variant;
  TWindow_oninput_ = function (ev: JEvent): variant;

type
  JViewCSS = class external "Object"
    function getComputedStyle(elt: JElement; pseudoElt: string = ""): JCSSStyleDeclaration;
  end;

type
  JWindow = class external "Object"(JViewCSS)//, JMSEventAttachmentTarget)//, JMSWindowExtensions)//, JWindowPerformance)//, JScreenView)//, JEventTarget)//, JWindowLocalStorage)//, JWindowSessionStorage)//, JWindowTimers)
    property ondragend: TWindow_ondragend_;
    property onkeydown: TWindow_onkeydown_;
    property ondragover: TWindow_ondragover_;
    property onkeyup: TWindow_onkeyup_;
    property onreset: TWindow_onreset_;
    property onmouseup: TWindow_onmouseup_;
    property ondragstart: TWindow_ondragstart_;
    property ondrag: TWindow_ondrag_;
    property onmouseover: TWindow_onmouseover_;
    property ondragleave: TWindow_ondragleave_;
    property history: JHistory;
    property name: string;
    property onafterprint: TWindow_onafterprint_;
    property onpause: TWindow_onpause_;
    property onbeforeprint: TWindow_onbeforeprint_;
    property top: JWindow;
    property onmousedown: TWindow_onmousedown_;
    property onseeked: TWindow_onseeked_;
    property opener: JWindow;
    property onclick: TWindow_onclick_;
    property onwaiting: TWindow_onwaiting_;
    property ononline: TWindow_ononline_;
    property ondurationchange: TWindow_ondurationchange_;
    property frames: JWindow;
    property onblur: TWindow_onblur_;
    property onemptied: TWindow_onemptied_;
    property onseeking: TWindow_onseeking_;
    property oncanplay: TWindow_oncanplay_;
    property onstalled: TWindow_onstalled_;
    property onmousemove: TWindow_onmousemove_;
    property onoffline: TWindow_onoffline_;
    property length: integer;
    property onbeforeunload: TWindow_onbeforeunload_;
    property onratechange: TWindow_onratechange_;
    property onstorage: TWindow_onstorage_;
    property onloadstart: TWindow_onloadstart_;
    property ondragenter: TWindow_ondragenter_;
    property onsubmit: TWindow_onsubmit_;
    property self: JWindow;
    property onprogress: TWindow_onprogress_;
    property ondblclick: TWindow_ondblclick_;
    property oncontextmenu: TWindow_oncontextmenu_;
    property onchange: TWindow_onchange_;
    property onloadedmetadata: TWindow_onloadedmetadata_;
    property onplay: TWindow_onplay_;
    property onerror: JErrorFunction;
    property onplaying: TWindow_onplaying_;
    property parent: JWindow;
    property location: JLocation;
    property oncanplaythrough: TWindow_oncanplaythrough_;
    property onabort: TWindow_onabort_;
    property onreadystatechange: TWindow_onreadystatechange_;
    property onkeypress: TWindow_onkeypress_;
    property frameElement: JElement;
    property onloadeddata: TWindow_onloadeddata_;
    property onsuspend: TWindow_onsuspend_;
    property window: JWindow;
    property onfocus: TWindow_onfocus_;
    property onmessage: TWindow_onmessage_;
    property ontimeupdate: TWindow_ontimeupdate_;
    property onresize: TWindow_onresize_;
    property navigator: JNavigator;
    property onselect: TWindow_onselect_;
    property ondrop: TWindow_ondrop_;
    property onmouseout: TWindow_onmouseout_;
    property onended: TWindow_onended_;
    property onhashchange: TWindow_onhashchange_;
    property onunload: TWindow_onunload_;
    property onscroll: TWindow_onscroll_;
    property onmousewheel: TWindow_onmousewheel_;
    property onload: TWindow_onload_;
    property onvolumechange: TWindow_onvolumechange_;
    property oninput: TWindow_oninput_;
    procedure alert(message: string = "");
    procedure focus();
    procedure print();
    function prompt(message: string = ""; defaul: string = ""): string;
    function toString(): string;
    function open(url: string = ""; target: string = ""; features: string = ""; replace: boolean = false): JWindow;
    procedure close();
    function confirm(message: string = ""): boolean;
    procedure postMessage(message: variant; targetOrigin: string; ports: variant = undefined);
    function showModalDialog(url: string = ""; argument: variant = undefined; options: variant = undefined): variant;
    procedure blur();
    function getSelection(): JSelection;
  end;

type
  JWindow1 = class external "Window"
  public
    property prototype: JWindow;
    function &new(): JWindow;
  end;


function Window_var: JWindow1;

type
  JSVGAnimatedPreserveAspectRatio = class external "Object"
    property animVal: JSVGPreserveAspectRatio;
    property baseVal: JSVGPreserveAspectRatio;
  end;

type
  JSVGAnimatedPreserveAspectRatio1 = class external "SVGAnimatedPreserveAspectRatio"
  public
    property prototype: JSVGAnimatedPreserveAspectRatio;
    function &new(): JSVGAnimatedPreserveAspectRatio;
  end;


function SVGAnimatedPreserveAspectRatio_var: JSVGAnimatedPreserveAspectRatio1;

type
  JMSSiteModeEvent = class external "Object"(JEvent)
    property buttonID: integer;
    property actionURL: string;
  end;

type
  JMSSiteModeEvent1 = class external "MSSiteModeEvent"
  public
    property prototype: JMSSiteModeEvent;
    function &new(): JMSSiteModeEvent;
  end;


function MSSiteModeEvent_var: JMSSiteModeEvent1;

type
  JMSCSSStyleRuleExtensions = class external "Object"
    property readOnly: boolean;
  end;

type
  JStyleSheetPageList = class external "Object"
    property length: integer;
    function item(index: integer): JStyleSheetPage;
    function  GetItems(index: integer): JStyleSheetPage; external array;
    procedure SetItems(index: integer; value: JStyleSheetPage); external array;
    property Items[index: integer]: JStyleSheetPage read GetItems write SetItems; default;
  end;

type
  JStyleSheetPageList1 = class external "StyleSheetPageList"
  public
    property prototype: JStyleSheetPageList;
    function &new(): JStyleSheetPageList;
  end;


function StyleSheetPageList_var: JStyleSheetPageList1;

type
  JHTMLImageElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedMarginStyle)//, JDOML2DeprecatedBorderStyle)//, JDOML2DeprecatedAlignmentStyle_HTMLImageElement)//, JMSImageResourceExtensions)//, JMSHTMLImageElementExtensions)//, JMSDataBindingExtensions)//, JMSResourceMetadata)
    property width: integer;
    property naturalHeight: integer;
    property alt: string;
    property src: string;
    property useMap: string;
    property naturalWidth: integer;
    property name: string;
    property height: integer;
    property longDesc: string;
    property isMap: boolean;
    property complete: boolean;
  end;

type
  JHTMLImageElement1 = class external "HTMLImageElement"
  public
    property prototype: JHTMLImageElement;
    function &new(): JHTMLImageElement;
  end;


function HTMLImageElement_var: JHTMLImageElement1;

type
  JHTMLAreaElement = class external "Object"(JHTMLElement)//, JMSHTMLAreaElementExtensions)
    property protocol: string;
    property search: string;
    property alt: string;
    property coords: string;
    property hostname: string;
    property port: string;
    property pathname: string;
    property host: string;
    property hash: string;
    property target: string;
    property href: string;
    property noHref: boolean;
    property shape: string;
    function toString(): string;
  end;

type
  JHTMLAreaElement1 = class external "HTMLAreaElement"
  public
    property prototype: JHTMLAreaElement;
    function &new(): JHTMLAreaElement;
  end;


function HTMLAreaElement_var: JHTMLAreaElement1;

type
  JSVGAngle = class external "Object"
    property valueAsString: string;
    property valueInSpecifiedUnits: integer;
    property value: integer;
    property unitType: integer;
    procedure newValueSpecifiedUnits(unitType: integer; valueInSpecifiedUnits: integer);
    procedure convertToSpecifiedUnits(unitType: integer);
    property SVG_ANGLETYPE_RAD: integer;
    property SVG_ANGLETYPE_UNKNOWN: integer;
    property SVG_ANGLETYPE_UNSPECIFIED: integer;
    property SVG_ANGLETYPE_DEG: integer;
    property SVG_ANGLETYPE_GRAD: integer;
  end;

type
  JSVGAngle1 = class external "SVGAngle"
  public
    property prototype: JSVGAngle;
    function &new(): JSVGAngle;
    property SVG_ANGLETYPE_RAD: integer;
    property SVG_ANGLETYPE_UNKNOWN: integer;
    property SVG_ANGLETYPE_UNSPECIFIED: integer;
    property SVG_ANGLETYPE_DEG: integer;
    property SVG_ANGLETYPE_GRAD: integer;
  end;


function SVGAngle_var: JSVGAngle1;

type
  JHTMLButtonElement = class external "Object"(JHTMLElement)//, JMSHTMLButtonElementExtensions)//, JMSDataBindingExtensions)
    property value: string;
    property form: JHTMLFormElement;
    property name: string;
    property &type: string;
  end;

type
  JHTMLButtonElement1 = class external "HTMLButtonElement"
  public
    property prototype: JHTMLButtonElement;
    function &new(): JHTMLButtonElement;
  end;


function HTMLButtonElement_var: JHTMLButtonElement1;

type
  JMSHTMLLabelElementExtensions = class external "Object"
  end;

type
  JHTMLSourceElement = class external "Object"(JHTMLElement)
    property src: string;
    property media: string;
    property &type: string;
  end;

type
  JHTMLSourceElement1 = class external "HTMLSourceElement"
  public
    property prototype: JHTMLSourceElement;
    function &new(): JHTMLSourceElement;
  end;


function HTMLSourceElement_var: JHTMLSourceElement1;

type
  JCanvasGradient = class external "Object"
    procedure addColorStop(offset: integer; color: string);
  end;

type
  JCanvasGradient1 = class external "CanvasGradient"
  public
    property prototype: JCanvasGradient;
    function &new(): JCanvasGradient;
  end;


function CanvasGradient_var: JCanvasGradient1;

type
  JKeyboardEvent = class external "Object"(JUIEvent)//, JKeyboardEventExtensions)
    property location: integer;
    property shiftKey: boolean;
    property locale: string;
    property key: string;
    property altKey: boolean;
    property metaKey: boolean;
    property char: string;
    property ctrlKey: boolean;
    property &repeat: boolean;
    function getModifierState(keyArg: string): boolean;
    procedure initKeyboardEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; keyArg: string; locationArg: integer; modifiersListArg: string; &repeat: boolean; locale: string);
    property DOM_KEY_LOCATION_RIGHT: integer;
    property DOM_KEY_LOCATION_STANDARD: integer;
    property DOM_KEY_LOCATION_LEFT: integer;
    property DOM_KEY_LOCATION_NUMPAD: integer;
    property DOM_KEY_LOCATION_JOYSTICK: integer;
    property DOM_KEY_LOCATION_MOBILE: integer;
  end;

type
  JKeyboardEvent1 = class external "KeyboardEvent"
  public
    property prototype: JKeyboardEvent;
    function &new(): JKeyboardEvent;
    property DOM_KEY_LOCATION_RIGHT: integer;
    property DOM_KEY_LOCATION_STANDARD: integer;
    property DOM_KEY_LOCATION_LEFT: integer;
    property DOM_KEY_LOCATION_NUMPAD: integer;
    property DOM_KEY_LOCATION_JOYSTICK: integer;
    property DOM_KEY_LOCATION_MOBILE: integer;
  end;


function KeyboardEvent_var: JKeyboardEvent1;

type
  JDocument = class external "Object"(JNode)//, JDocumentStyle)//, JDocumentRange)//, JHTMLDocument)//, JNodeSelector)//, JDocumentEvent)//, JDocumentTraversal)//, JDocumentView)//, JSVGDocument)
    property doctype: JDocumentType;
    property xmlVersion: string;
    property &implementation: JDOMImplementation;
    property xmlEncoding: string;
    property xmlStandalone: boolean;
    property documentElement: JHTMLElement;
    property inputEncoding: string;
    function createElement(tagName: string): JHTMLElement;
    function adoptNode(source: JNode): JNode;
    function createComment(data: string): JComment;
    function createDocumentFragment(): JDocumentFragment;
    function getElementsByTagName(tagname: string): JNodeList;
    function getElementsByTagNameNS(namespaceURI: string; localName: string): JNodeList;
    function createProcessingInstruction(target: string; data: string): JProcessingInstruction;
    function createElementNS(namespaceURI: string; qualifiedName: string): JElement;
    function createAttribute(name: string): JAttr;
    function createTextNode(data: string): JText;
    function importNode(importedNode: JNode; deep: boolean): JNode;
    function createCDATASection(data: string): JCDATASection;
    function createAttributeNS(namespaceURI: string; qualifiedName: string): JAttr;
    function getElementById(elementId: string): JHTMLElement;
  end;

type
  JDocument1 = class external "Document"
  public
    property prototype: JDocument;
    function &new(): JDocument;
  end;


function Document_var: JDocument1;

type
  JMessageEvent = class external "Object"(JEvent)
    property source: JWindow;
    property origin: string;
    property data: variant;
    procedure initMessageEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; dataArg: variant; originArg: string; lastEventIdArg: string; sourceArg: JWindow);
  end;

type
  JMessageEvent1 = class external "MessageEvent"
  public
    property prototype: JMessageEvent;
    function &new(): JMessageEvent;
  end;


function MessageEvent_var: JMessageEvent1;

type
  JHTMLScriptElement = class external "Object"(JHTMLElement)
    property defer: boolean;
    property text: string;
    property src: string;
    property htmlFor: string;
    property charset: string;
    property &type: string;
    property event: string;
  end;

type
  JHTMLScriptElement1 = class external "HTMLScriptElement"
  public
    property prototype: JHTMLScriptElement;
    function &new(): JHTMLScriptElement;
  end;


function HTMLScriptElement_var: JHTMLScriptElement1;

type
  JMSHTMLBodyElementExtensions = class external "Object"(JDOML2DeprecatedWordWrapSuppression_HTMLBodyElement)
    property scroll: string;
    property bottomMargin: variant;
    property topMargin: variant;
    property rightMargin: variant;
    property bgProperties: string;
    property leftMargin: variant;
    function createTextRange(): JTextRange;
  end;

type
  JHTMLTableRowElement = class external "Object"(JHTMLElement)//, JMSBorderColorHighlightStyle_HTMLTableRowElement)//, JHTMLTableAlignment)//, JMSBorderColorStyle_HTMLTableRowElement)//, JDOML2DeprecatedAlignmentStyle_HTMLTableRowElement)//, JDOML2DeprecatedBackgroundColorStyle)//, JMSHTMLTableRowElementExtensions)
    property rowIndex: integer;
    property cells: JHTMLCollection;
    property sectionRowIndex: integer;
    procedure deleteCell(index: integer = 0);
    function insertCell(index: integer = 0): JHTMLElement;
  end;

type
  JHTMLTableRowElement1 = class external "HTMLTableRowElement"
  public
    property prototype: JHTMLTableRowElement;
    function &new(): JHTMLTableRowElement;
  end;


function HTMLTableRowElement_var: JHTMLTableRowElement1;

type
  JMSCommentExtensions = class external "Object"
    property text: string;
  end;

type
  JDOML2DeprecatedMarginStyle_HTMLMarqueeElement = class external "Object"
    property vspace: integer;
    property hspace: integer;
  end;

type
  JMSCSSRuleList = class external "Object"
    property length: integer;
    function item(index: integer = 0): JCSSStyleRule;
    function  GetItems(index: integer): JCSSStyleRule; external array;
    procedure SetItems(index: integer; value: JCSSStyleRule); external array;
    property Items[index: integer]: JCSSStyleRule read GetItems write SetItems; default;
  end;

type
  JMSCSSRuleList1 = class external "MSCSSRuleList"
  public
    property prototype: JMSCSSRuleList;
    function &new(): JMSCSSRuleList;
  end;


function MSCSSRuleList_var: JMSCSSRuleList1;

type
  JCanvasRenderingContext2D = class external "Object"
    property shadowOffsetX: integer;
    property lineWidth: integer;
    property miterLimit: integer;
    property canvas: JHTMLCanvasElement;
    property strokeStyle: variant;
    property font: string;
    property globalAlpha: integer;
    property globalCompositeOperation: string;
    property shadowOffsetY: integer;
    property fillStyle: variant;
    property lineCap: string;
    property shadowBlur: integer;
    property textAlign: string;
    property textBaseline: string;
    property shadowColor: string;
    property lineJoin: string;
    procedure restore();
    procedure setTransform(m11: integer; m12: integer; m21: integer; m22: integer; dx: integer; dy: integer);
    procedure save();
    procedure arc(x: integer; y: integer; radius: integer; startAngle: integer; endAngle: integer; anticlockwise: boolean = false);
    function measureText(text: string): JTextMetrics;
    function isPointInPath(x: integer; y: integer): boolean;
    procedure quadraticCurveTo(cpx: integer; cpy: integer; x: integer; y: integer);
    procedure putImageData(imagedata: JImageData; dx: integer; dy: integer; dirtyX: integer = 0; dirtyY: integer = 0; dirtyWidth: integer = 0; dirtyHeight: integer = 0);
    procedure rotate(angle: integer);
    procedure fillText(text: string; x: integer; y: integer; maxWidth: integer = 0);
    procedure translate(x: integer; y: integer);
    procedure scale(x: integer; y: integer);
    function createRadialGradient(x0: integer; y0: integer; r0: integer; x1: integer; y1: integer; r1: integer): JCanvasGradient;
    procedure lineTo(x: integer; y: integer);
    procedure fill();
    function createPattern(image: JHTMLElement; repetition: string): JCanvasPattern;
    procedure closePath();
    procedure rect(x: integer; y: integer; w: integer; h: integer);
    procedure clip();
    function createImageData(imageDataOrSw: variant; sh: integer = 0): JImageData;
    procedure clearRect(x: integer; y: integer; w: integer; h: integer);
    procedure moveTo(x: integer; y: integer);
    function getImageData(sx: integer; sy: integer; sw: integer; sh: integer): JImageData;
    procedure fillRect(x: integer; y: integer; w: integer; h: integer);
    procedure bezierCurveTo(cp1x: integer; cp1y: integer; cp2x: integer; cp2y: integer; x: integer; y: integer);
    procedure drawImage(image: JHTMLElement; offsetX: integer; offsetY: integer; width: integer = 0; height: integer = 0; canvasOffsetX: integer = 0; canvasOffsetY: integer = 0; canvasImageWidth: integer = 0; canvasImageHeight: integer = 0);
    procedure transform(m11: integer; m12: integer; m21: integer; m22: integer; dx: integer; dy: integer);
    procedure stroke();
    procedure strokeRect(x: integer; y: integer; w: integer; h: integer);
    procedure strokeText(text: string; x: integer; y: integer; maxWidth: integer = 0);
    procedure beginPath();
    procedure arcTo(x1: integer; y1: integer; x2: integer; y2: integer; radius: integer);
    function createLinearGradient(x0: integer; y0: integer; x1: integer; y1: integer): JCanvasGradient;
  end;

type
  JCanvasRenderingContext2D1 = class external "CanvasRenderingContext2D"
  public
    property prototype: JCanvasRenderingContext2D;
    function &new(): JCanvasRenderingContext2D;
  end;


function CanvasRenderingContext2D_var: JCanvasRenderingContext2D1;

type
  JSVGPathSegLinetoHorizontalAbs = class external "Object"(JSVGPathSeg)
    property x: integer;
  end;

type
  JSVGPathSegLinetoHorizontalAbs1 = class external "SVGPathSegLinetoHorizontalAbs"
  public
    property prototype: JSVGPathSegLinetoHorizontalAbs;
    function &new(): JSVGPathSegLinetoHorizontalAbs;
  end;


function SVGPathSegLinetoHorizontalAbs_var: JSVGPathSegLinetoHorizontalAbs1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLObjectElement = class external "Object"
    property align: string;
  end;

type
  JDOML2DeprecatedBorderStyle_MSHTMLIFrameElementExtensions = class external "Object"
    property border: string;
  end;

type
  JMSHTMLElementRangeExtensions = class external "Object"
    function createControlRange(): JControlRangeCollection;
  end;

type
  JSVGPathSegArcAbs = class external "Object"(JSVGPathSeg)
    property y: integer;
    property sweepFlag: boolean;
    property r2: integer;
    property x: integer;
    property angle: integer;
    property r1: integer;
    property largeArcFlag: boolean;
  end;

type
  JSVGPathSegArcAbs1 = class external "SVGPathSegArcAbs"
  public
    property prototype: JSVGPathSegArcAbs;
    function &new(): JSVGPathSegArcAbs;
  end;


function SVGPathSegArcAbs_var: JSVGPathSegArcAbs1;

type
  JMSScreenExtensions = class external "Object"
    property deviceXDPI: integer;
    property fontSmoothingEnabled: boolean;
    property bufferDepth: integer;
    property logicalXDPI: integer;
    property systemXDPI: integer;
    property logicalYDPI: integer;
    property systemYDPI: integer;
    property updateInterval: integer;
    property deviceYDPI: integer;
  end;

type
  JHTMLHtmlElement = class external "Object"(JHTMLElement)//, JHTMLHtmlElementDOML2Deprecated)
  end;

type
  JHTMLHtmlElement1 = class external "HTMLHtmlElement"
  public
    property prototype: JHTMLHtmlElement;
    function &new(): JHTMLHtmlElement;
  end;


function HTMLHtmlElement_var: JHTMLHtmlElement1;

type
  JMSBorderColorStyle = class external "Object"
    property borderColor: variant;
  end;

type
  JSVGTransformList = class external "Object"
    property numberOfItems: integer;
    function getItem(index: integer): JSVGTransform;
    function consolidate(): JSVGTransform;
    procedure clear();
    function appendItem(newItem: JSVGTransform): JSVGTransform;
    function initialize(newItem: JSVGTransform): JSVGTransform;
    function removeItem(index: integer): JSVGTransform;
    function insertItemBefore(newItem: JSVGTransform; index: integer): JSVGTransform;
    function replaceItem(newItem: JSVGTransform; index: integer): JSVGTransform;
    function createSVGTransformFromMatrix(matrix: JSVGMatrix): JSVGTransform;
  end;

type
  JSVGTransformList1 = class external "SVGTransformList"
  public
    property prototype: JSVGTransformList;
    function &new(): JSVGTransformList;
  end;


function SVGTransformList_var: JSVGTransformList1;

type
  JSVGPathSegClosePath = class external "Object"(JSVGPathSeg)
  end;

type
  JSVGPathSegClosePath1 = class external "SVGPathSegClosePath"
  public
    property prototype: JSVGPathSegClosePath;
    function &new(): JSVGPathSegClosePath;
  end;


function SVGPathSegClosePath_var: JSVGPathSegClosePath1;

type
  JDOML2DeprecatedMarginStyle_MSHTMLIFrameElementExtensions = class external "Object"
    property vspace: integer;
    property hspace: integer;
  end;

type
  JHTMLFrameElement = class external "Object"(JHTMLElement)//, JGetSVGDocument)//, JMSHTMLFrameElementExtensions)//, JMSDataBindingExtensions)//, JMSBorderColorStyle_HTMLFrameElement)
    property scrolling: string;
    property marginHeight: string;
    property src: string;
    property name: string;
    property marginWidth: string;
    property contentDocument: JDocument;
    property longDesc: string;
    property noResize: boolean;
  end;

type
  JHTMLFrameElement1 = class external "HTMLFrameElement"
  public
    property prototype: JHTMLFrameElement;
    function &new(): JHTMLFrameElement;
  end;


function HTMLFrameElement_var: JHTMLFrameElement1;

type
  JSVGAnimatedLength = class external "Object"
    property animVal: JSVGLength;
    property baseVal: JSVGLength;
  end;

type
  JSVGAnimatedLength1 = class external "SVGAnimatedLength"
  public
    property prototype: JSVGAnimatedLength;
    function &new(): JSVGAnimatedLength;
  end;


function SVGAnimatedLength_var: JSVGAnimatedLength1;

type
  JCSSMediaRule = class external "Object"(JCSSRule)
    property media: JMediaList;
    property cssRules: JCSSRuleList;
    function insertRule(rule: string; index: integer = 0): integer;
    procedure deleteRule(index: integer = 0);
  end;

type
  JCSSMediaRule1 = class external "CSSMediaRule"
  public
    property prototype: JCSSMediaRule;
    function &new(): JCSSMediaRule;
  end;


function CSSMediaRule_var: JCSSMediaRule1;

type
  JHTMLQuoteElement = class external "Object"(JHTMLElement)//, JMSHTMLQuoteElementExtensions)
    property cite: string;
  end;

type
  JHTMLQuoteElement1 = class external "HTMLQuoteElement"
  public
    property prototype: JHTMLQuoteElement;
    function &new(): JHTMLQuoteElement;
  end;


function HTMLQuoteElement_var: JHTMLQuoteElement1;

type
  JSVGDefsElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)
  end;

type
  JSVGDefsElement1 = class external "SVGDefsElement"
  public
    property prototype: JSVGDefsElement;
    function &new(): JSVGDefsElement;
  end;


function SVGDefsElement_var: JSVGDefsElement1;

type
  JSVGAnimatedPoints = class external "Object"
    property points: JSVGPointList;
    property animatedPoints: JSVGPointList;
  end;

type
  JWindowModal = class external "Object"
    property dialogArguments: variant;
    property returnValue: variant;
  end;

type
  JMSHTMLButtonElementExtensions = class external "Object"
    property status: variant;
    function createTextRange(): JTextRange;
  end;

type
  TXMLHttpRequest_onreadystatechange_ = function (ev: JEvent): variant;
  TXMLHttpRequest_onload_ = function (ev: JEvent): variant;
type
  JXMLHttpRequest = class external "Object"(JEventTarget)//, JMSXMLHttpRequestExtensions)
    property onreadystatechange: TXMLHttpRequest_onreadystatechange_;
    property status: integer;
    property onload: TXMLHttpRequest_onload_;
    property readyState: integer;
    property responseText: string;
    property responseXML: JDocument;
    property statusText: string;
    procedure open(method: string; url: string; async: boolean = false; user: string = ""; password: string = "");
    procedure send(data: variant = undefined);
    procedure abort();
    function getAllResponseHeaders(): string;
    procedure setRequestHeader(header: string; value: string);
    function getResponseHeader(header: string): string;
    property LOADING: integer;
    property DONE: integer;
    property UNSENT: integer;
    property OPENED: integer;
    property HEADERS_RECEIVED: integer;
  end;

type
  JXMLHttpRequest1 = class external "XMLHttpRequest"
  public
    property prototype: JXMLHttpRequest;
    function &new(): JXMLHttpRequest;
    property LOADING: integer;
    property DONE: integer;
    property UNSENT: integer;
    property OPENED: integer;
    property HEADERS_RECEIVED: integer;
  end;


function XMLHttpRequest_var: JXMLHttpRequest1;

type
  JHTMLTableHeaderCellElement = class external "Object"(JHTMLTableCellElement)//, JHTMLTableHeaderCellScope)
  end;

type
  JHTMLTableHeaderCellElement1 = class external "HTMLTableHeaderCellElement"
  public
    property prototype: JHTMLTableHeaderCellElement;
    function &new(): JHTMLTableHeaderCellElement;
  end;


function HTMLTableHeaderCellElement_var: JHTMLTableHeaderCellElement1;

type
  JHTMLDListElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedListSpaceReduction)//, JMSHTMLDListElementExtensions)
  end;

type
  JHTMLDListElement1 = class external "HTMLDListElement"
  public
    property prototype: JHTMLDListElement;
    function &new(): JHTMLDListElement;
  end;


function HTMLDListElement_var: JHTMLDListElement1;

type
  JMSDataBindingExtensions = class external "Object"
    property dataSrc: string;
    property dataFormatAs: string;
    property dataFld: string;
  end;

type
  JSVGEllipseElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)
    property ry: JSVGAnimatedLength;
    property cx: JSVGAnimatedLength;
    property rx: JSVGAnimatedLength;
    property cy: JSVGAnimatedLength;
  end;

type
  JSVGEllipseElement1 = class external "SVGEllipseElement"
  public
    property prototype: JSVGEllipseElement;
    function &new(): JSVGEllipseElement;
  end;


function SVGEllipseElement_var: JSVGEllipseElement1;

type
  JSVGPathSegLinetoHorizontalRel = class external "Object"(JSVGPathSeg)
    property x: integer;
  end;

type
  JSVGPathSegLinetoHorizontalRel1 = class external "SVGPathSegLinetoHorizontalRel"
  public
    property prototype: JSVGPathSegLinetoHorizontalRel;
    function &new(): JSVGPathSegLinetoHorizontalRel;
  end;


function SVGPathSegLinetoHorizontalRel_var: JSVGPathSegLinetoHorizontalRel1;

type
  JSVGAElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)//, JSVGURIReference)
    property target: JSVGAnimatedString;
  end;

type
  JSVGAElement1 = class external "SVGAElement"
  public
    property prototype: JSVGAElement;
    function &new(): JSVGAElement;
  end;


function SVGAElement_var: JSVGAElement1;

type
  JMSHTMLMetaElementExtensions = class external "Object"
    property url: string;
    property charset: string;
  end;

type
  JSVGStylable = class external "Object"
    property className: JSVGAnimatedString;
    property style: JCSSStyleDeclaration;
  end;

type
  JMSHTMLTableCellElementExtensions = class external "Object"
  end;

type
  THTMLFrameSetElement_onresize_ = function (ev: JUIEvent): variant;
  THTMLFrameSetElement_ononline_ = function (ev: JEvent): variant;
  THTMLFrameSetElement_onafterprint_ = function (ev: JEvent): variant;
  THTMLFrameSetElement_onbeforeprint_ = function (ev: JEvent): variant;
  THTMLFrameSetElement_onoffline_ = function (ev: JEvent): variant;
  THTMLFrameSetElement_onblur_ = function (ev: JFocusEvent): variant;
  THTMLFrameSetElement_onunload_ = function (ev: JEvent): variant;
  THTMLFrameSetElement_onhashchange_ = function (ev: JEvent): variant;
  THTMLFrameSetElement_onfocus_ = function (ev: JFocusEvent): variant;
  THTMLFrameSetElement_onmessage_ = function (ev: JMessageEvent): variant;
  THTMLFrameSetElement_onload_ = function (ev: JEvent): variant;
  THTMLFrameSetElement_onerror_ = function (ev: JEvent): variant;
  THTMLFrameSetElement_onbeforeunload_ = function (ev: JBeforeUnloadEvent): variant;
  THTMLFrameSetElement_onstorage_ = function (ev: JStorageEvent): variant;
type
  JHTMLFrameSetElement = class external "Object"(JHTMLElement)//, JMSHTMLFrameSetElementExtensions)//, JMSBorderColorStyle_HTMLFrameSetElement)
    property onresize: THTMLFrameSetElement_onresize_;
    property ononline: THTMLFrameSetElement_ononline_;
    property onafterprint: THTMLFrameSetElement_onafterprint_;
    property onbeforeprint: THTMLFrameSetElement_onbeforeprint_;
    property onoffline: THTMLFrameSetElement_onoffline_;
    property rows: string;
    property cols: string;
    property onblur: THTMLFrameSetElement_onblur_;
    property onunload: THTMLFrameSetElement_onunload_;
    property onhashchange: THTMLFrameSetElement_onhashchange_;
    property onfocus: THTMLFrameSetElement_onfocus_;
    property onmessage: THTMLFrameSetElement_onmessage_;
    property onload: THTMLFrameSetElement_onload_;
    property onerror: THTMLFrameSetElement_onerror_;
    property onbeforeunload: THTMLFrameSetElement_onbeforeunload_;
    property onstorage: THTMLFrameSetElement_onstorage_;
  end;

type
  JHTMLFrameSetElement1 = class external "HTMLFrameSetElement"
  public
    property prototype: JHTMLFrameSetElement;
    function &new(): JHTMLFrameSetElement;
  end;


function HTMLFrameSetElement_var: JHTMLFrameSetElement1;

type
  JSVGTransformable = class external "Object"(JSVGLocatable)
    property transform: JSVGAnimatedTransformList;
  end;

type
  JScreen = class external "Object"(JMSScreenExtensions)
    property width: integer;
    property colorDepth: integer;
    property availWidth: integer;
    property pixelDepth: integer;
    property availHeight: integer;
    property height: integer;
  end;

type
  JScreen1 = class external "Screen"
  public
    property prototype: JScreen;
    function &new(): JScreen;
  end;


function Screen_var: JScreen1;

type
  JNavigatorGeolocation = class external "Object"
    property geolocation: JGeolocation;
  end;

type
  JCoordinates = class external "Object"
    property altitudeAccuracy: integer;
    property longitude: integer;
    property latitude: integer;
    property speed: integer;
    property heading: integer;
    property altitude: integer;
    property accuracy: integer;
  end;

type
  JCoordinates1 = class external "Coordinates"
  public
    property prototype: JCoordinates;
    function &new(): JCoordinates;
  end;


function Coordinates_var: JCoordinates1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLTableColElement = class external "Object"
    property align: string;
  end;

type
  JEventListener = class external "Object"
  end;


procedure EventListener_(evt: JEvent);external;

type
  JSVGLangSpace = class external "Object"
    property xmllang: string;
    property xmlspace: string;
  end;

type
  JDataTransfer = class external "Object"
    property effectAllowed: string;
    property dropEffect: string;
    function clearData(format: string = ""): boolean;
    function setData(format: string; data: string): boolean;
    function getData(format: string): string;
  end;

type
  JDataTransfer1 = class external "DataTransfer"
  public
    property prototype: JDataTransfer;
    function &new(): JDataTransfer;
  end;


function DataTransfer_var: JDataTransfer1;

type
  JFocusEvent = class external "Object"(JUIEvent)
    property relatedTarget: JEventTarget;
    procedure initFocusEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; detailArg: integer; relatedTargetArg: JEventTarget);
  end;

type
  JFocusEvent1 = class external "FocusEvent"
  public
    property prototype: JFocusEvent;
    function &new(): JFocusEvent;
  end;


function FocusEvent_var: JFocusEvent1;

type
  JRange = class external "Object"
    property startOffset: integer;
    property collapsed: boolean;
    property endOffset: integer;
    property startContainer: JNode;
    property endContainer: JNode;
    property commonAncestorContainer: JNode;
    procedure setStart(refNode: JNode; offset: integer);
    procedure setEndBefore(refNode: JNode);
    procedure setStartBefore(refNode: JNode);
    procedure selectNode(refNode: JNode);
    procedure detach();
    function getBoundingClientRect(): JClientRect;
    function toString(): string;
    function compareBoundaryPoints(how: integer; sourceRange: JRange): integer;
    procedure insertNode(newNode: JNode);
    procedure collapse(toStart: boolean);
    procedure selectNodeContents(refNode: JNode);
    function cloneContents(): JDocumentFragment;
    procedure setEnd(refNode: JNode; offset: integer);
    function cloneRange(): JRange;
    function getClientRects(): JClientRectList;
    procedure surroundContents(newParent: JNode);
    procedure deleteContents();
    procedure setStartAfter(refNode: JNode);
    function extractContents(): JDocumentFragment;
    procedure setEndAfter(refNode: JNode);
    property END_TO_END: integer;
    property START_TO_START: integer;
    property START_TO_END: integer;
    property END_TO_START: integer;
  end;

type
  JRange1 = class external "Range"
  public
    property prototype: JRange;
    function &new(): JRange;
    property END_TO_END: integer;
    property START_TO_START: integer;
    property START_TO_END: integer;
    property END_TO_START: integer;
  end;


function Range_var: JRange1;

type
  JDOML2DeprecatedTextFlowControl_HTMLBlockElement = class external "Object"
    property clear: string;
  end;

type
  JMSHTMLPreElementExtensions = class external "Object"(JDOML2DeprecatedTextFlowControl_HTMLBlockElement)
    property cite: string;
  end;

type
  JSVGPoint = class external "Object"
    property y: integer;
    property x: integer;
    function matrixTransform(matrix: JSVGMatrix): JSVGPoint;
  end;

type
  JSVGPoint1 = class external "SVGPoint"
  public
    property prototype: JSVGPoint;
    function &new(): JSVGPoint;
  end;


function SVGPoint_var: JSVGPoint1;

type
  JMSPluginsCollection = class external "Object"
    property length: integer;
    procedure refresh(reload: boolean = false);
  end;

type
  JMSPluginsCollection1 = class external "MSPluginsCollection"
  public
    property prototype: JMSPluginsCollection;
    function &new(): JMSPluginsCollection;
  end;


function MSPluginsCollection_var: JMSPluginsCollection1;

type
  JMSHTMLFontElementExtensions = class external "Object"
  end;

type
  JSVGAnimatedNumberList = class external "Object"
    property animVal: JSVGNumberList;
    property baseVal: JSVGNumberList;
  end;

type
  JSVGAnimatedNumberList1 = class external "SVGAnimatedNumberList"
  public
    property prototype: JSVGAnimatedNumberList;
    function &new(): JSVGAnimatedNumberList;
  end;


function SVGAnimatedNumberList_var: JSVGAnimatedNumberList1;

type
  JSVGSVGElement = class external "Object"(JSVGElement)//, JSVGZoomAndPan)//, JSVGLangSpace)//, JSVGLocatable)//, JSVGTests)//, JSVGFitToViewBox)//, JSVGSVGElementEventHandlers)//, JSVGStylable)//, JDocumentEvent)//, JViewCSS_SVGSVGElement)
    property width: JSVGAnimatedLength;
    property x: JSVGAnimatedLength;
    property contentStyleType: string;
    property screenPixelToMillimeterY: integer;
    property height: JSVGAnimatedLength;
    property contentScriptType: string;
    property pixelUnitToMillimeterX: integer;
    property currentTranslate: JSVGPoint;
    property y: JSVGAnimatedLength;
    property viewport: JSVGRect;
    property currentScale: integer;
    property screenPixelToMillimeterX: integer;
    property pixelUnitToMillimeterY: integer;
    procedure setCurrentTime(seconds: integer);
    function createSVGLength(): JSVGLength;
    function getIntersectionList(rect: JSVGRect; referenceElement: JSVGElement): JNodeList;
    procedure unpauseAnimations();
    function createSVGRect(): JSVGRect;
    function checkIntersection(element: JSVGElement; rect: JSVGRect): boolean;
    procedure unsuspendRedrawAll();
    procedure pauseAnimations();
    function suspendRedraw(maxWaitMilliseconds: integer): integer;
    procedure deselectAll();
    function createSVGAngle(): JSVGAngle;
    function getEnclosureList(rect: JSVGRect; referenceElement: JSVGElement): JNodeList;
    function createSVGTransform(): JSVGTransform;
    procedure unsuspendRedraw(suspendHandleID: integer);
    procedure forceRedraw();
    function getCurrentTime(): integer;
    function checkEnclosure(element: JSVGElement; rect: JSVGRect): boolean;
    function createSVGMatrix(): JSVGMatrix;
    function createSVGPoint(): JSVGPoint;
    function createSVGNumber(): JSVGNumber;
    function createSVGTransformFromMatrix(matrix: JSVGMatrix): JSVGTransform;
    function getElementById(elementId: string): JElement;
  end;

type
  JSVGSVGElement1 = class external "SVGSVGElement"
  public
    property prototype: JSVGSVGElement;
    function &new(): JSVGSVGElement;
  end;


function SVGSVGElement_var: JSVGSVGElement1;

type
  JHTMLLabelElement = class external "Object"(JHTMLElement)//, JMSDataBindingExtensions)//, JMSHTMLLabelElementExtensions)
    property htmlFor: string;
    property form: JHTMLFormElement;
  end;

type
  JHTMLLabelElement1 = class external "HTMLLabelElement"
  public
    property prototype: JHTMLLabelElement;
    function &new(): JHTMLLabelElement;
  end;


function HTMLLabelElement_var: JHTMLLabelElement1;

type
  JMSResourceMetadata = class external "Object"
    property protocol: string;
    property fileSize: string;
    property fileUpdatedDate: string;
    property nameProp: string;
    property fileCreatedDate: string;
    property fileModifiedDate: string;
    property mimeType: string;
  end;

type
  JMSHTMLQuoteElementExtensions = class external "Object"
    property dateTime: string;
  end;

type
  JDOML2DeprecatedAlignmentStyle_HTMLIFrameElement = class external "Object"
    property align: string;
  end;

type
  JHTMLLegendElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedAlignmentStyle_HTMLLegendElement)//, JMSDataBindingExtensions)//, JMSHTMLLegendElementExtensions)
    property form: JHTMLFormElement;
  end;

type
  JHTMLLegendElement1 = class external "HTMLLegendElement"
  public
    property prototype: JHTMLLegendElement;
    function &new(): JHTMLLegendElement;
  end;


function HTMLLegendElement_var: JHTMLLegendElement1;

type
  JHTMLDirectoryElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedListSpaceReduction)//, JMSHTMLDirectoryElementExtensions)
  end;

type
  JHTMLDirectoryElement1 = class external "HTMLDirectoryElement"
  public
    property prototype: JHTMLDirectoryElement;
    function &new(): JHTMLDirectoryElement;
  end;


function HTMLDirectoryElement_var: JHTMLDirectoryElement1;

type
  JNavigatorAbilities = class external "Object"
  end;

type
  JMSHTMLImageElementExtensions = class external "Object"
    property href: string;
  end;

type
  JSVGAnimatedInteger = class external "Object"
    property animVal: integer;
    property baseVal: integer;
  end;

type
  JSVGAnimatedInteger1 = class external "SVGAnimatedInteger"
  public
    property prototype: JSVGAnimatedInteger;
    function &new(): JSVGAnimatedInteger;
  end;


function SVGAnimatedInteger_var: JSVGAnimatedInteger1;

type
  JSVGTextElement = class external "Object"(JSVGTextPositioningElement)//, JSVGTransformable)
  end;

type
  JSVGTextElement1 = class external "SVGTextElement"
  public
    property prototype: JSVGTextElement;
    function &new(): JSVGTextElement;
  end;


function SVGTextElement_var: JSVGTextElement1;

type
  JSVGTSpanElement = class external "Object"(JSVGTextPositioningElement)
  end;

type
  JSVGTSpanElement1 = class external "SVGTSpanElement"
  public
    property prototype: JSVGTSpanElement;
    function &new(): JSVGTSpanElement;
  end;


function SVGTSpanElement_var: JSVGTSpanElement1;

type
  JHTMLLIElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedListNumberingAndBulletStyle)//, JMSHTMLLIElementExtensions)
    property value: integer;
  end;

type
  JHTMLLIElement1 = class external "HTMLLIElement"
  public
    property prototype: JHTMLLIElement;
    function &new(): JHTMLLIElement;
  end;


function HTMLLIElement_var: JHTMLLIElement1;

type
  JSVGPathSegLinetoVerticalAbs = class external "Object"(JSVGPathSeg)
    property y: integer;
  end;

type
  JSVGPathSegLinetoVerticalAbs1 = class external "SVGPathSegLinetoVerticalAbs"
  public
    property prototype: JSVGPathSegLinetoVerticalAbs;
    function &new(): JSVGPathSegLinetoVerticalAbs;
  end;


function SVGPathSegLinetoVerticalAbs_var: JSVGPathSegLinetoVerticalAbs1;

type
  JMSAttrExtensions = class external "Object"
    property expando: boolean;
  end;

type
  JMSStorageExtensions = class external "Object"
    property remainingSpace: integer;
  end;

type
  JSVGStyleElement = class external "Object"(JSVGElement)//, JSVGLangSpace)
    property media: string;
    property &type: string;
    property title: string;
  end;

type
  JSVGStyleElement1 = class external "SVGStyleElement"
  public
    property prototype: JSVGStyleElement;
    function &new(): JSVGStyleElement;
  end;


function SVGStyleElement_var: JSVGStyleElement1;

type
  JMSCurrentStyleCSSProperties = class external "Object"(JMSCSSProperties)
    property blockDirection: string;
    property clipBottom: string;
    property clipLeft: string;
    property clipRight: string;
    property clipTop: string;
    property hasLayout: string;
  end;

type
  JMSCurrentStyleCSSProperties1 = class external "MSCurrentStyleCSSProperties"
  public
    property prototype: JMSCurrentStyleCSSProperties;
    function &new(): JMSCurrentStyleCSSProperties;
  end;


function MSCurrentStyleCSSProperties_var: JMSCurrentStyleCSSProperties1;

type
  JMSLinkStyleExtensions = class external "Object"
    property styleSheet: JStyleSheet;
  end;

type
  JDOML2DeprecatedWordWrapSuppression_HTMLDivElement = class external "Object"
    property noWrap: boolean;
  end;

type
  JDocumentTraversal = class external "Object"
    function createNodeIterator(root: JNode; whatToShow: integer; filter: JNodeFilterCallback; entityReferenceExpansion: boolean): JNodeIterator;
    function createTreeWalker(root: JNode; whatToShow: integer; filter: JNodeFilterCallback; entityReferenceExpansion: boolean): JTreeWalker;
  end;

type
  JStorage = class external "Object"(JMSStorageExtensions)
    property length: integer;
    function getItem(key: string): variant;
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
    procedure setItem(key: string; data: string);
    procedure clear();
    procedure removeItem(key: string);
    function key(index: integer): string;
//    function  GetItems(index: integer): variant; external array;
//    procedure SetItems(index: integer; value: variant); external array;
//    property Items[index: integer]: variant read GetItems write SetItems; default;
  end;

type
  JStorage1 = class external "Storage"
  public
    property prototype: JStorage;
    function &new(): JStorage;
  end;


function Storage_var: JStorage1;

type
  JHTMLTableHeaderCellScope = class external "Object"
    property scope: string;
  end;

type
  JHTMLIFrameElement = class external "Object"(JHTMLElement)//, JGetSVGDocument)//, JMSHTMLIFrameElementExtensions)//, JMSDataBindingExtensions)//, JDOML2DeprecatedAlignmentStyle_HTMLIFrameElement)
    property width: string;
    property contentWindow: JWindow;
    property scrolling: string;
    property src: string;
    property marginHeight: string;
    property name: string;
    property marginWidth: string;
    property height: string;
    property contentDocument: JDocument;
    property longDesc: string;
    property frameBorder: string;
  end;

type
  JHTMLIFrameElement1 = class external "HTMLIFrameElement"
  public
    property prototype: JHTMLIFrameElement;
    function &new(): JHTMLIFrameElement;
  end;


function HTMLIFrameElement_var: JHTMLIFrameElement1;

type
  JMSNavigatorAbilities = class external "Object"
    property userLanguage: string;
    property plugins: JMSPluginsCollection;
    property cookieEnabled: boolean;
    property appCodeName: string;
    property cpuClass: string;
    property appMinorVersion: string;
    property connectionSpeed: integer;
    property browserLanguage: string;
    property mimeTypes: JMSMimeTypesCollection;
    property product: string;
    property systemLanguage: string;
    function javaEnabled(): boolean;
    function taintEnabled(): boolean;
  end;

type
  JTextRangeCollection = class external "Object"
    property length: integer;
    function item(index: integer): JTextRange;
    function  GetItems(index: integer): JTextRange; external array;
    procedure SetItems(index: integer; value: JTextRange); external array;
    property Items[index: integer]: JTextRange read GetItems write SetItems; default;
  end;

type
  JTextRangeCollection1 = class external "TextRangeCollection"
  public
    property prototype: JTextRangeCollection;
    function &new(): JTextRangeCollection;
  end;


function TextRangeCollection_var: JTextRangeCollection1;

type
  THTMLBodyElement_onresize_ = function (ev: JUIEvent): variant;
  THTMLBodyElement_ononline_ = function (ev: JEvent): variant;
  THTMLBodyElement_onafterprint_ = function (ev: JEvent): variant;
  THTMLBodyElement_onbeforeprint_ = function (ev: JEvent): variant;
  THTMLBodyElement_onoffline_ = function (ev: JEvent): variant;
  THTMLBodyElement_onblur_ = function (ev: JFocusEvent): variant;
  THTMLBodyElement_onhashchange_ = function (ev: JEvent): variant;
  THTMLBodyElement_onunload_ = function (ev: JEvent): variant;
  THTMLBodyElement_onfocus_ = function (ev: JFocusEvent): variant;
  THTMLBodyElement_onmessage_ = function (ev: JMessageEvent): variant;
  THTMLBodyElement_onload_ = function (ev: JEvent): variant;
  THTMLBodyElement_onerror_ = function (ev: JEvent): variant;
  THTMLBodyElement_onbeforeunload_ = function (ev: JBeforeUnloadEvent): variant;
  THTMLBodyElement_onstorage_ = function (ev: JStorageEvent): variant;
type
  JHTMLBodyElement = class external "Object"(JHTMLElement)//, JHTMLBodyElementDOML2Deprecated)//, JMSHTMLBodyElementExtensions)//, JDOML2DeprecatedBackgroundStyle)//, JDOML2DeprecatedBackgroundColorStyle)
    property onresize: THTMLBodyElement_onresize_;
    property ononline: THTMLBodyElement_ononline_;
    property onafterprint: THTMLBodyElement_onafterprint_;
    property onbeforeprint: THTMLBodyElement_onbeforeprint_;
    property onoffline: THTMLBodyElement_onoffline_;
    property onblur: THTMLBodyElement_onblur_;
    property onhashchange: THTMLBodyElement_onhashchange_;
    property onunload: THTMLBodyElement_onunload_;
    property onfocus: THTMLBodyElement_onfocus_;
    property onmessage: THTMLBodyElement_onmessage_;
    property onload: THTMLBodyElement_onload_;
    property onerror: THTMLBodyElement_onerror_;
    property onbeforeunload: THTMLBodyElement_onbeforeunload_;
    property onstorage: THTMLBodyElement_onstorage_;
  end;

type
  JHTMLBodyElement1 = class external "HTMLBodyElement"
  public
    property prototype: JHTMLBodyElement;
    function &new(): JHTMLBodyElement;
  end;


function HTMLBodyElement_var: JHTMLBodyElement1;

type
  JDocumentType = class external "Object"(JNode)
    property name: string;
    property notations: JNamedNodeMap;
    property systemId: string;
    property internalSubset: string;
    property entities: JNamedNodeMap;
    property publicId: string;
  end;

type
  JDocumentType1 = class external "DocumentType"
  public
    property prototype: JDocumentType;
    function &new(): JDocumentType;
  end;


function DocumentType_var: JDocumentType1;

type
  JDOML2DeprecatedMarginStyle_HTMLInputElement = class external "Object"
    property vspace: integer;
    property hspace: integer;
  end;

type
  JMSHTMLInputElementExtensions = class external "Object"(JDOML2DeprecatedMarginStyle_HTMLInputElement)//, JDOML2DeprecatedBorderStyle_HTMLInputElement)
    property status: boolean;
    property complete: boolean;
    function createTextRange(): JTextRange;
  end;

type
  JDOML2DeprecatedAlignmentStyle_HTMLLegendElement = class external "Object"
    property align: string;
  end;

type
  JSVGGradientElement = class external "Object"(JSVGElement)//, JSVGUnitTypes)//, JSVGStylable)//, JSVGURIReference)
    property spreadMethod: JSVGAnimatedEnumeration;
    property gradientTransform: JSVGAnimatedTransformList;
    property gradientUnits: JSVGAnimatedEnumeration;
    property SVG_SPREADMETHOD_REFLECT: integer;
    property SVG_SPREADMETHOD_PAD: integer;
    property SVG_SPREADMETHOD_UNKNOWN: integer;
    property SVG_SPREADMETHOD_REPEAT: integer;
  end;

type
  JSVGGradientElement1 = class external "SVGGradientElement"
  public
    property prototype: JSVGGradientElement;
    function &new(): JSVGGradientElement;
    property SVG_SPREADMETHOD_REFLECT: integer;
    property SVG_SPREADMETHOD_PAD: integer;
    property SVG_SPREADMETHOD_UNKNOWN: integer;
    property SVG_SPREADMETHOD_REPEAT: integer;
  end;


function SVGGradientElement_var: JSVGGradientElement1;

type
  JSVGRadialGradientElement = class external "Object"(JSVGGradientElement)
    property cx: JSVGAnimatedLength;
    property r: JSVGAnimatedLength;
    property cy: JSVGAnimatedLength;
    property fx: JSVGAnimatedLength;
    property fy: JSVGAnimatedLength;
  end;

type
  JSVGRadialGradientElement1 = class external "SVGRadialGradientElement"
  public
    property prototype: JSVGRadialGradientElement;
    function &new(): JSVGRadialGradientElement;
  end;


function SVGRadialGradientElement_var: JSVGRadialGradientElement1;

type
  JMutationEvent = class external "Object"(JEvent)
    property newValue: string;
    property attrChange: integer;
    property attrName: string;
    property prevValue: string;
    property relatedNode: JNode;
    procedure initMutationEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; relatedNodeArg: JNode; prevValueArg: string; newValueArg: string; attrNameArg: string; attrChangeArg: integer);
    property MODIFICATION: integer;
    property REMOVAL: integer;
    property ADDITION: integer;
  end;

type
  JMutationEvent1 = class external "MutationEvent"
  public
    property prototype: JMutationEvent;
    function &new(): JMutationEvent;
    property MODIFICATION: integer;
    property REMOVAL: integer;
    property ADDITION: integer;
  end;


function MutationEvent_var: JMutationEvent1;

type
  JDragEvent = class external "Object"(JMouseEvent)
    property dataTransfer: JDataTransfer;
    procedure initDragEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; detailArg: integer; screenXArg: integer; screenYArg: integer; clientXArg: integer; clientYArg: integer; ctrlKeyArg: boolean; altKeyArg: boolean; shiftKeyArg: boolean; metaKeyArg: boolean; buttonArg: integer; relatedTargetArg: JEventTarget; dataTransferArg: JDataTransfer);
  end;

type
  JDragEvent1 = class external "DragEvent"
  public
    property prototype: JDragEvent;
    function &new(): JDragEvent;
  end;


function DragEvent_var: JDragEvent1;

type
  JDOML2DeprecatedWidthStyle_HTMLTableCellElement = class external "Object"
    property width: integer;
  end;

type
  JHTMLTableSectionElement = class external "Object"(JHTMLElement)//, JMSHTMLTableSectionElementExtensions)//, JDOML2DeprecatedAlignmentStyle_HTMLTableSectionElement)//, JHTMLTableAlignment)
    property rows: JHTMLCollection;
    procedure deleteRow(index: integer = 0);
    function insertRow(index: integer = 0): JHTMLElement;
  end;

type
  JHTMLTableSectionElement1 = class external "HTMLTableSectionElement"
  public
    property prototype: JHTMLTableSectionElement;
    function &new(): JHTMLTableSectionElement;
  end;


function HTMLTableSectionElement_var: JHTMLTableSectionElement1;

type
  JHTMLInputElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedAlignmentStyle_HTMLInputElement)//, JMSImageResourceExtensions_HTMLInputElement)//, JMSHTMLInputElementExtensions)//, JMSDataBindingExtensions)
    property width: string;
    property defaultChecked: boolean;
    property alt: string;
    property accept: string;
    property value: string;
    property src: string;
    property useMap: string;
    property name: string;
    property form: JHTMLFormElement;
    property selectionStart: integer;
    property height: string;
    property indeterminate: boolean;
    property readOnly: boolean;
    property size: integer;
    property checked: boolean;
    property maxLength: integer;
    property selectionEnd: integer;
    property &type: string;
    property defaultValue: string;
    procedure setSelectionRange(start: integer; &end: integer);
    procedure select();
  end;

type
  JHTMLInputElement1 = class external "HTMLInputElement"
  public
    property prototype: JHTMLInputElement;
    function &new(): JHTMLInputElement;
  end;


function HTMLInputElement_var: JHTMLInputElement1;

type
  JHTMLAnchorElement = class external "Object"(JHTMLElement)//, JMSHTMLAnchorElementExtensions)//, JMSDataBindingExtensions)
    property rel: string;
    property protocol: string;
    property search: string;
    property coords: string;
    property hostname: string;
    property pathname: string;
    property target: string;
    property href: string;
    property name: string;
    property charset: string;
    property hreflang: string;
    property port: string;
    property host: string;
    property hash: string;
    property rev: string;
    property &type: string;
    property shape: string;
    function toString(): string;
  end;

type
  JHTMLAnchorElement1 = class external "HTMLAnchorElement"
  public
    property prototype: JHTMLAnchorElement;
    function &new(): JHTMLAnchorElement;
  end;


function HTMLAnchorElement_var: JHTMLAnchorElement1;

type
  JSVGImageElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)//, JSVGURIReference)
    property y: JSVGAnimatedLength;
    property width: JSVGAnimatedLength;
    property preserveAspectRatio: JSVGAnimatedPreserveAspectRatio;
    property x: JSVGAnimatedLength;
    property height: JSVGAnimatedLength;
  end;

type
  JSVGImageElement1 = class external "SVGImageElement"
  public
    property prototype: JSVGImageElement;
    function &new(): JSVGImageElement;
  end;


function SVGImageElement_var: JSVGImageElement1;

type
  JMSElementExtensions = class external "Object"
    function msMatchesSelector(selectors: string): boolean;
    function fireEvent(eventName: string; eventObj: variant = undefined): boolean;
  end;

type
  JHTMLParamElement = class external "Object"(JHTMLElement)
    property value: string;
    property name: string;
    property &type: string;
    property valueType: string;
  end;

type
  JHTMLParamElement1 = class external "HTMLParamElement"
  public
    property prototype: JHTMLParamElement;
    function &new(): JHTMLParamElement;
  end;


function HTMLParamElement_var: JHTMLParamElement1;

type
  JMSHTMLDocumentViewExtensions = class external "Object"
    function createStyleSheet(href: string = ""; index: integer = 0): JCSSStyleSheet;
  end;

type
  JSVGAnimatedNumber = class external "Object"
    property animVal: integer;
    property baseVal: integer;
  end;

type
  JSVGAnimatedNumber1 = class external "SVGAnimatedNumber"
  public
    property prototype: JSVGAnimatedNumber;
    function &new(): JSVGAnimatedNumber;
  end;


function SVGAnimatedNumber_var: JSVGAnimatedNumber1;

type
  JPerformanceTiming = class external "Object"
    property redirectStart: integer;
    property domainLookupEnd: integer;
    property responseStart: integer;
    property domComplete: integer;
    property domainLookupStart: integer;
    property loadEventStart: integer;
    property msFirstPaint: integer;
    property unloadEventEnd: integer;
    property fetchStart: integer;
    property requestStart: integer;
    property domInteractive: integer;
    property navigationStart: integer;
    property connectEnd: integer;
    property loadEventEnd: integer;
    property connectStart: integer;
    property responseEnd: integer;
    property domLoading: integer;
    property redirectEnd: integer;
    property unloadEventStart: integer;
    property domContentLoadedEventStart: integer;
    property domContentLoadedEventEnd: integer;
    function toJSON(): variant;
  end;

type
  JPerformanceTiming1 = class external "PerformanceTiming"
  public
    property prototype: JPerformanceTiming;
    function &new(): JPerformanceTiming;
  end;


function PerformanceTiming_var: JPerformanceTiming1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLInputElement = class external "Object"
    property align: string;
  end;

type
  JHTMLPreElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedWidthStyle)//, JMSHTMLPreElementExtensions)
  end;

type
  JHTMLPreElement1 = class external "HTMLPreElement"
  public
    property prototype: JHTMLPreElement;
    function &new(): JHTMLPreElement;
  end;


function HTMLPreElement_var: JHTMLPreElement1;

type
  JEventException = class external "Object"
    property code: integer;
    property message: string;
    function toString(): string;
    property DISPATCH_REQUEST_ERR: integer;
    property UNSPECIFIED_EVENT_TYPE_ERR: integer;
  end;

type
  JEventException1 = class external "EventException"
  public
    property prototype: JEventException;
    function &new(): JEventException;
    property DISPATCH_REQUEST_ERR: integer;
    property UNSPECIFIED_EVENT_TYPE_ERR: integer;
  end;


function EventException_var: JEventException1;

type
  JMSBorderColorHighlightStyle_HTMLTableCellElement = class external "Object"
    property borderColorLight: variant;
    property borderColorDark: variant;
  end;

type
  JNavigatorOnLine = class external "Object"
    property onLine: boolean;
  end;

type
  TSVGElementEventHandlers_onmouseover_ = function (ev: JMouseEvent): variant;
  TSVGElementEventHandlers_onmousemove_ = function (ev: JMouseEvent): variant;
  TSVGElementEventHandlers_onmouseout_ = function (ev: JMouseEvent): variant;
  TSVGElementEventHandlers_ondblclick_ = function (ev: JMouseEvent): variant;
  TSVGElementEventHandlers_onfocusout_ = function (ev: JFocusEvent): variant;
  TSVGElementEventHandlers_onfocusin_ = function (ev: JFocusEvent): variant;
  TSVGElementEventHandlers_onmousedown_ = function (ev: JMouseEvent): variant;
  TSVGElementEventHandlers_onmouseup_ = function (ev: JMouseEvent): variant;
  TSVGElementEventHandlers_onload_ = function (ev: JEvent): variant;
  TSVGElementEventHandlers_onclick_ = function (ev: JMouseEvent): variant;
type
  JSVGElementEventHandlers = class external "Object"
    property onmouseover: TSVGElementEventHandlers_onmouseover_;
    property onmousemove: TSVGElementEventHandlers_onmousemove_;
    property onmouseout: TSVGElementEventHandlers_onmouseout_;
    property ondblclick: TSVGElementEventHandlers_ondblclick_;
    property onfocusout: TSVGElementEventHandlers_onfocusout_;
    property onfocusin: TSVGElementEventHandlers_onfocusin_;
    property onmousedown: TSVGElementEventHandlers_onmousedown_;
    property onmouseup: TSVGElementEventHandlers_onmouseup_;
    property onload: TSVGElementEventHandlers_onload_;
    property onclick: TSVGElementEventHandlers_onclick_;
  end;

type
  JWindowLocalStorage = class external "Object"
    property localStorage: JStorage;
  end;

type
  JSVGMetadataElement = class external "Object"(JSVGElement)
  end;

type
  JSVGMetadataElement1 = class external "SVGMetadataElement"
  public
    property prototype: JSVGMetadataElement;
    function &new(): JSVGMetadataElement;
  end;


function SVGMetadataElement_var: JSVGMetadataElement1;

type
  JSVGPathSegArcRel = class external "Object"(JSVGPathSeg)
    property y: integer;
    property sweepFlag: boolean;
    property r2: integer;
    property x: integer;
    property angle: integer;
    property r1: integer;
    property largeArcFlag: boolean;
  end;

type
  JSVGPathSegArcRel1 = class external "SVGPathSegArcRel"
  public
    property prototype: JSVGPathSegArcRel;
    function &new(): JSVGPathSegArcRel;
  end;


function SVGPathSegArcRel_var: JSVGPathSegArcRel1;

type
  JSVGPathSegMovetoAbs = class external "Object"(JSVGPathSeg)
    property y: integer;
    property x: integer;
  end;

type
  JSVGPathSegMovetoAbs1 = class external "SVGPathSegMovetoAbs"
  public
    property prototype: JSVGPathSegMovetoAbs;
    function &new(): JSVGPathSegMovetoAbs;
  end;


function SVGPathSegMovetoAbs_var: JSVGPathSegMovetoAbs1;

type
  JSVGStringList = class external "Object"
    property numberOfItems: integer;
    function replaceItem(newItem: string; index: integer): string;
    function getItem(index: integer): string;
    procedure clear();
    function appendItem(newItem: string): string;
    function initialize(newItem: string): string;
    function removeItem(index: integer): string;
    function insertItemBefore(newItem: string; index: integer): string;
  end;

type
  JSVGStringList1 = class external "SVGStringList"
  public
    property prototype: JSVGStringList;
    function &new(): JSVGStringList;
  end;


function SVGStringList_var: JSVGStringList1;

type
  TXDomainRequest_onerror_ = function (ev: JEvent): variant;
  TXDomainRequest_onload_ = function (ev: JEvent): variant;
  TXDomainRequest_onprogress_ = function (ev: variant): variant;
  TXDomainRequest_ontimeout_ = function (ev: JEvent): variant;
type
  JXDomainRequest = class external "Object"
    property timeout: integer;
    property onerror: TXDomainRequest_onerror_;
    property onload: TXDomainRequest_onload_;
    property onprogress: TXDomainRequest_onprogress_;
    property ontimeout: TXDomainRequest_ontimeout_;
    property responseText: string;
    property contentType: string;
    procedure open(method: string; url: string);
    procedure abort();
    procedure send(data: variant = undefined);
  end;

type
  JXDomainRequest1 = class external "XDomainRequest"
  public
    property prototype: JXDomainRequest;
    function &new(): JXDomainRequest;
  end;


function XDomainRequest_var: JXDomainRequest1;

type
  JDOML2DeprecatedBackgroundColorStyle = class external "Object"
    property bgColor: variant;
  end;

type
  JElementTraversal = class external "Object"
    property childElementCount: integer;
    property previousElementSibling: JElement;
    property lastElementChild: JElement;
    property nextElementSibling: JElement;
    property firstElementChild: JElement;
  end;

type
  JSVGLength = class external "Object"
    property valueAsString: string;
    property valueInSpecifiedUnits: integer;
    property value: integer;
    property unitType: integer;
    procedure newValueSpecifiedUnits(unitType: integer; valueInSpecifiedUnits: integer);
    procedure convertToSpecifiedUnits(unitType: integer);
    property SVG_LENGTHTYPE_NUMBER: integer;
    property SVG_LENGTHTYPE_CM: integer;
    property SVG_LENGTHTYPE_PC: integer;
    property SVG_LENGTHTYPE_PERCENTAGE: integer;
    property SVG_LENGTHTYPE_MM: integer;
    property SVG_LENGTHTYPE_PT: integer;
    property SVG_LENGTHTYPE_IN: integer;
    property SVG_LENGTHTYPE_EMS: integer;
    property SVG_LENGTHTYPE_PX: integer;
    property SVG_LENGTHTYPE_UNKNOWN: integer;
    property SVG_LENGTHTYPE_EXS: integer;
  end;

type
  JSVGLength1 = class external "SVGLength"
  public
    property prototype: JSVGLength;
    function &new(): JSVGLength;
    property SVG_LENGTHTYPE_NUMBER: integer;
    property SVG_LENGTHTYPE_CM: integer;
    property SVG_LENGTHTYPE_PC: integer;
    property SVG_LENGTHTYPE_PERCENTAGE: integer;
    property SVG_LENGTHTYPE_MM: integer;
    property SVG_LENGTHTYPE_PT: integer;
    property SVG_LENGTHTYPE_IN: integer;
    property SVG_LENGTHTYPE_EMS: integer;
    property SVG_LENGTHTYPE_PX: integer;
    property SVG_LENGTHTYPE_UNKNOWN: integer;
    property SVG_LENGTHTYPE_EXS: integer;
  end;


function SVGLength_var: JSVGLength1;

type
  JSVGPolygonElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGAnimatedPoints)//, JSVGTests)
  end;

type
  JSVGPolygonElement1 = class external "SVGPolygonElement"
  public
    property prototype: JSVGPolygonElement;
    function &new(): JSVGPolygonElement;
  end;


function SVGPolygonElement_var: JSVGPolygonElement1;

type
  JHTMLPhraseElement = class external "Object"(JHTMLElement)
    property dateTime: string;
    property cite: string;
  end;

type
  JHTMLPhraseElement1 = class external "HTMLPhraseElement"
  public
    property prototype: JHTMLPhraseElement;
    function &new(): JHTMLPhraseElement;
  end;


function HTMLPhraseElement_var: JHTMLPhraseElement1;

type
  JMSHTMLAreaElementExtensions = class external "Object"
  end;

type
  JSVGPathSegCurvetoCubicRel = class external "Object"(JSVGPathSeg)
    property y: integer;
    property y1: integer;
    property x2: integer;
    property x: integer;
    property x1: integer;
    property y2: integer;
  end;

type
  JSVGPathSegCurvetoCubicRel1 = class external "SVGPathSegCurvetoCubicRel"
  public
    property prototype: JSVGPathSegCurvetoCubicRel;
    function &new(): JSVGPathSegCurvetoCubicRel;
  end;


function SVGPathSegCurvetoCubicRel_var: JSVGPathSegCurvetoCubicRel1;

type
  JMSEventObj = class external "Object"
    property nextPage: string;
    property keyCode: integer;
    property toElement: JElement;
    property returnValue: variant;
    property dataFld: string;
    property y: integer;
    property dataTransfer: JDataTransfer;
    property propertyName: string;
    property url: string;
    property offsetX: integer;
    property recordset: JObject;
    property screenX: integer;
    property buttonID: integer;
    property wheelDelta: integer;
    property reason: integer;
    property origin: string;
    property data: string;
    property srcFilter: JObject;
    property boundElements: JHTMLCollection;
    property cancelBubble: boolean;
    property altLeft: boolean;
    property behaviorCookie: integer;
    property bookmarks: JBookmarkCollection;
    property &type: string;
    property &repeat: boolean;
    property srcElement: JElement;
    property source: JWindow;
    property fromElement: JElement;
    property offsetY: integer;
    property x: integer;
    property behaviorPart: integer;
    property qualifier: string;
    property altKey: boolean;
    property ctrlKey: boolean;
    property clientY: integer;
    property shiftKey: boolean;
    property shiftLeft: boolean;
    property contentOverflow: boolean;
    property screenY: integer;
    property ctrlLeft: boolean;
    property button: integer;
    property srcUrn: string;
    property clientX: integer;
    property actionURL: string;
    function getAttribute(strAttributeName: string; lFlags: integer = 0): variant;
    procedure setAttribute(strAttributeName: string; AttributeValue: variant; lFlags: integer = 0);
    function removeAttribute(strAttributeName: string; lFlags: integer = 0): boolean;
  end;

type
  JMSEventObj1 = class external "MSEventObj"
  public
    property prototype: JMSEventObj;
    function &new(): JMSEventObj;
  end;


function MSEventObj_var: JMSEventObj1;

type
  JDOML2DeprecatedColorProperty = class external "Object"
    property color: string;
  end;

type
  JMSHTMLLIElementExtensions = class external "Object"
  end;

type
  JHTMLCanvasElement = class external "Object"(JHTMLElement)
    property width: integer;
    property height: integer;
    function toDataURL(): string;
    function toDataURL(&type: string; {many?}args: array of variant): string;overload;
    function getContext(contextId: string): JCanvasRenderingContext2D;
  end;

type
  JHTMLCanvasElement1 = class external "HTMLCanvasElement"
  public
    property prototype: JHTMLCanvasElement;
    function &new(): JHTMLCanvasElement;
  end;


function HTMLCanvasElement_var: JHTMLCanvasElement1;

type
  JHTMLTitleElement = class external "Object"(JHTMLElement)
    property text: string;
  end;

type
  JHTMLTitleElement1 = class external "HTMLTitleElement"
  public
    property prototype: JHTMLTitleElement;
    function &new(): JHTMLTitleElement;
  end;


function HTMLTitleElement_var: JHTMLTitleElement1;

type
  JLocation = class external "Object"
    property hash: string;
    property protocol: string;
    property search: string;
    property href: string;
    property hostname: string;
    property port: string;
    property pathname: string;
    property host: string;
    procedure reload(flag: boolean = false);
    procedure replace(url: string);
    procedure assign(url: string);
    function toString(): string;
  end;

type
  JLocation1 = class external "Location"
  public
    property prototype: JLocation;
    function &new(): JLocation;
  end;


function Location_var: JLocation1;

type
  JHTMLStyleElement = class external "Object"(JHTMLElement)//, JMSLinkStyleExtensions)//, JLinkStyle)
    property media: string;
    property &type: string;
  end;

type
  JHTMLStyleElement1 = class external "HTMLStyleElement"
  public
    property prototype: JHTMLStyleElement;
    function &new(): JHTMLStyleElement;
  end;


function HTMLStyleElement_var: JHTMLStyleElement1;

type
  JMSHTMLOptGroupElementExtensions = class external "Object"
    property index: integer;
    property defaultSelected: boolean;
    property text: string;
    property value: string;
    property form: JHTMLFormElement;
    property selected: boolean;
  end;

type
  JMSBorderColorHighlightStyle = class external "Object"
    property borderColorLight: variant;
    property borderColorDark: variant;
  end;

type
  JDOML2DeprecatedSizeProperty_HTMLBaseFontElement = class external "Object"
    property size: integer;
  end;

type
  JSVGTransform = class external "Object"
    property &type: integer;
    property angle: integer;
    property matrix: JSVGMatrix;
    procedure setTranslate(tx: integer; ty: integer);
    procedure setScale(sx: integer; sy: integer);
    procedure setMatrix(matrix: JSVGMatrix);
    procedure setSkewY(angle: integer);
    procedure setRotate(angle: integer; cx: integer; cy: integer);
    procedure setSkewX(angle: integer);
    property SVG_TRANSFORM_SKEWX: integer;
    property SVG_TRANSFORM_UNKNOWN: integer;
    property SVG_TRANSFORM_SCALE: integer;
    property SVG_TRANSFORM_TRANSLATE: integer;
    property SVG_TRANSFORM_MATRIX: integer;
    property SVG_TRANSFORM_ROTATE: integer;
    property SVG_TRANSFORM_SKEWY: integer;
  end;

type
  JSVGTransform1 = class external "SVGTransform"
  public
    property prototype: JSVGTransform;
    function &new(): JSVGTransform;
    property SVG_TRANSFORM_SKEWX: integer;
    property SVG_TRANSFORM_UNKNOWN: integer;
    property SVG_TRANSFORM_SCALE: integer;
    property SVG_TRANSFORM_TRANSLATE: integer;
    property SVG_TRANSFORM_MATRIX: integer;
    property SVG_TRANSFORM_ROTATE: integer;
    property SVG_TRANSFORM_SKEWY: integer;
  end;


function SVGTransform_var: JSVGTransform1;

type
  JMSCSSFilter = class external "Object"
    property Percent: integer;
    property Enabled: boolean;
    property Duration: integer;
    procedure Play(Duration: integer);
    procedure Apply();
    procedure Stop();
  end;

type
  JMSCSSFilter1 = class external "MSCSSFilter"
  public
    property prototype: JMSCSSFilter;
    function &new(): JMSCSSFilter;
  end;


function MSCSSFilter_var: JMSCSSFilter1;

type
  JViewCSS_SVGSVGElement = class external "Object"
    function getComputedStyle(elt: JElement; pseudoElt: string = ""): JCSSStyleDeclaration;
  end;

type
  JSVGURIReference = class external "Object"
    property href: JSVGAnimatedString;
  end;

type
  JWheelEvent = class external "Object"(JMouseEvent)
    property deltaZ: integer;
    property deltaX: integer;
    property deltaMode: integer;
    property deltaY: integer;
    procedure initWheelEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; detailArg: integer; screenXArg: integer; screenYArg: integer; clientXArg: integer; clientYArg: integer; buttonArg: integer; relatedTargetArg: JEventTarget; modifiersListArg: string; deltaXArg: integer; deltaYArg: integer; deltaZArg: integer; deltaMode: integer);
    property DOM_DELTA_PIXEL: integer;
    property DOM_DELTA_LINE: integer;
    property DOM_DELTA_PAGE: integer;
  end;

type
  JWheelEvent1 = class external "WheelEvent"
  public
    property prototype: JWheelEvent;
    function &new(): JWheelEvent;
    property DOM_DELTA_PIXEL: integer;
    property DOM_DELTA_LINE: integer;
    property DOM_DELTA_PAGE: integer;
  end;


function WheelEvent_var: JWheelEvent1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLDivElement = class external "Object"
    property align: string;
  end;

type
  JSVGNumber = class external "Object"
    property value: integer;
  end;

type
  JSVGNumber1 = class external "SVGNumber"
  public
    property prototype: JSVGNumber;
    function &new(): JSVGNumber;
  end;


function SVGNumber_var: JSVGNumber1;

type
  JSVGPathElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGAnimatedPathData)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)
    function getPathSegAtLength(distance: integer): integer;
    function getPointAtLength(distance: integer): JSVGPoint;
    function createSVGPathSegCurvetoQuadraticAbs(x: integer; y: integer; x1: integer; y1: integer): JSVGPathSegCurvetoQuadraticAbs;
    function createSVGPathSegLinetoRel(x: integer; y: integer): JSVGPathSegLinetoRel;
    function createSVGPathSegCurvetoQuadraticRel(x: integer; y: integer; x1: integer; y1: integer): JSVGPathSegCurvetoQuadraticRel;
    function createSVGPathSegCurvetoCubicAbs(x: integer; y: integer; x1: integer; y1: integer; x2: integer; y2: integer): JSVGPathSegCurvetoCubicAbs;
    function createSVGPathSegLinetoAbs(x: integer; y: integer): JSVGPathSegLinetoAbs;
    function createSVGPathSegClosePath(): JSVGPathSegClosePath;
    function createSVGPathSegCurvetoCubicRel(x: integer; y: integer; x1: integer; y1: integer; x2: integer; y2: integer): JSVGPathSegCurvetoCubicRel;
    function createSVGPathSegCurvetoQuadraticSmoothRel(x: integer; y: integer): JSVGPathSegCurvetoQuadraticSmoothRel;
    function createSVGPathSegMovetoRel(x: integer; y: integer): JSVGPathSegMovetoRel;
    function createSVGPathSegCurvetoCubicSmoothAbs(x: integer; y: integer; x2: integer; y2: integer): JSVGPathSegCurvetoCubicSmoothAbs;
    function createSVGPathSegMovetoAbs(x: integer; y: integer): JSVGPathSegMovetoAbs;
    function createSVGPathSegLinetoVerticalRel(y: integer): JSVGPathSegLinetoVerticalRel;
    function createSVGPathSegArcRel(x: integer; y: integer; r1: integer; r2: integer; angle: integer; largeArcFlag: boolean; sweepFlag: boolean): JSVGPathSegArcRel;
    function createSVGPathSegCurvetoQuadraticSmoothAbs(x: integer; y: integer): JSVGPathSegCurvetoQuadraticSmoothAbs;
    function createSVGPathSegLinetoHorizontalRel(x: integer): JSVGPathSegLinetoHorizontalRel;
    function getTotalLength(): integer;
    function createSVGPathSegCurvetoCubicSmoothRel(x: integer; y: integer; x2: integer; y2: integer): JSVGPathSegCurvetoCubicSmoothRel;
    function createSVGPathSegLinetoHorizontalAbs(x: integer): JSVGPathSegLinetoHorizontalAbs;
    function createSVGPathSegLinetoVerticalAbs(y: integer): JSVGPathSegLinetoVerticalAbs;
    function createSVGPathSegArcAbs(x: integer; y: integer; r1: integer; r2: integer; angle: integer; largeArcFlag: boolean; sweepFlag: boolean): JSVGPathSegArcAbs;
  end;

type
  JSVGPathElement1 = class external "SVGPathElement"
  public
    property prototype: JSVGPathElement;
    function &new(): JSVGPathElement;
  end;


function SVGPathElement_var: JSVGPathElement1;

type
  JMSCompatibleInfo = class external "Object"
    property version: string;
    property userAgent: string;
  end;

type
  JMSCompatibleInfo1 = class external "MSCompatibleInfo"
  public
    property prototype: JMSCompatibleInfo;
    function &new(): JMSCompatibleInfo;
  end;


function MSCompatibleInfo_var: JMSCompatibleInfo1;

type
  JMSHTMLDocumentEventExtensions = class external "Object"
    function createEventObject(eventObj: variant = undefined): JMSEventObj;
    function fireEvent(eventName: string; eventObj: variant = undefined): boolean;
  end;

type
  JSVGAnimatedRect = class external "Object"
    property animVal: JSVGRect;
    property baseVal: JSVGRect;
  end;

type
  JSVGAnimatedRect1 = class external "SVGAnimatedRect"
  public
    property prototype: JSVGAnimatedRect;
    function &new(): JSVGAnimatedRect;
  end;


function SVGAnimatedRect_var: JSVGAnimatedRect1;

type
  JCSSNamespaceRule = class external "Object"(JCSSRule)
    property namespaceURI: string;
    property prefix: string;
  end;

type
  JCSSNamespaceRule1 = class external "CSSNamespaceRule"
  public
    property prototype: JCSSNamespaceRule;
    function &new(): JCSSNamespaceRule;
  end;


function CSSNamespaceRule_var: JCSSNamespaceRule1;

type
  JHTMLUnknownElement = class external "Object"(JHTMLElement)//, JMSDataBindingRecordSetReadonlyExtensions)//, JMSHTMLUnknownElementExtensions)
  end;

type
  JHTMLUnknownElement1 = class external "HTMLUnknownElement"
  public
    property prototype: JHTMLUnknownElement;
    function &new(): JHTMLUnknownElement;
  end;


function HTMLUnknownElement_var: JHTMLUnknownElement1;

type
  JSVGPathSegList = class external "Object"
    property numberOfItems: integer;
    function replaceItem(newItem: JSVGPathSeg; index: integer): JSVGPathSeg;
    function getItem(index: integer): JSVGPathSeg;
    procedure clear();
    function appendItem(newItem: JSVGPathSeg): JSVGPathSeg;
    function initialize(newItem: JSVGPathSeg): JSVGPathSeg;
    function removeItem(index: integer): JSVGPathSeg;
    function insertItemBefore(newItem: JSVGPathSeg; index: integer): JSVGPathSeg;
  end;

type
  JSVGPathSegList1 = class external "SVGPathSegList"
  public
    property prototype: JSVGPathSegList;
    function &new(): JSVGPathSegList;
  end;


function SVGPathSegList_var: JSVGPathSegList1;

type
  JHTMLMediaElement = class external "Object"(JHTMLElement)
    property initialTime: integer;
    property played: JTimeRanges;
    property currentSrc: string;
    property readyState: string;
    property autobuffer: boolean;
    property loop: boolean;
    property ended: boolean;
    property buffered: JTimeRanges;
    property error: JMediaError;
    property seekable: JTimeRanges;
    property autoplay: boolean;
    property controls: boolean;
    property volume: integer;
    property src: string;
    property playbackRate: integer;
    property duration: integer;
    property muted: boolean;
    property defaultPlaybackRate: integer;
    property paused: boolean;
    property seeking: boolean;
    property currentTime: integer;
    property preload: string;
    property networkState: integer;
    procedure pause();
    procedure play();
    procedure load();
    function canPlayType(&type: string): string;
    property HAVE_METADATA: integer;
    property HAVE_CURRENT_DATA: integer;
    property HAVE_NOTHING: integer;
    property NETWORK_NO_SOURCE: integer;
    property HAVE_ENOUGH_DATA: integer;
    property NETWORK_EMPTY: integer;
    property NETWORK_LOADING: integer;
    property NETWORK_IDLE: integer;
    property HAVE_FUTURE_DATA: integer;
  end;

type
  JHTMLMediaElement1 = class external "HTMLMediaElement"
  public
    property prototype: JHTMLMediaElement;
    function &new(): JHTMLMediaElement;
    property HAVE_METADATA: integer;
    property HAVE_CURRENT_DATA: integer;
    property HAVE_NOTHING: integer;
    property NETWORK_NO_SOURCE: integer;
    property HAVE_ENOUGH_DATA: integer;
    property NETWORK_EMPTY: integer;
    property NETWORK_LOADING: integer;
    property NETWORK_IDLE: integer;
    property HAVE_FUTURE_DATA: integer;
  end;


function HTMLMediaElement_var: JHTMLMediaElement1;

type
  JHTMLAudioElement = class external "Object"(JHTMLMediaElement)
  end;

type
  JHTMLAudioElement1 = class external "HTMLAudioElement"
  public
    property prototype: JHTMLAudioElement;
    function &new(): JHTMLAudioElement;
  end;


function HTMLAudioElement_var: JHTMLAudioElement1;

type
  JMSImageResourceExtensions = class external "Object"
    property dynsrc: string;
    property vrml: string;
    property lowsrc: string;
    property start: string;
    property loop: integer;
  end;

type
  JMSBorderColorHighlightStyle_HTMLTableRowElement = class external "Object"
    property borderColorLight: variant;
    property borderColorDark: variant;
  end;

type
  JPositionError = class external "Object"
    property code: integer;
    property message: string;
    function toString(): string;
    property POSITION_UNAVAILABLE: integer;
    property PERMISSION_DENIED: integer;
    property TIMEOUT: integer;
  end;

type
  JPositionError1 = class external "PositionError"
  public
    property POSITION_UNAVAILABLE: integer;
    property PERMISSION_DENIED: integer;
    property TIMEOUT: integer;
  end;


function PositionError_var: JPositionError1;

type
  JBrowserPublic = class external "Object"
  end;

type
  JBrowserPublic1 = class external "BrowserPublic"
  public
    property prototype: JBrowserPublic;
    function &new(): JBrowserPublic;
  end;


function BrowserPublic_var: JBrowserPublic1;

type
  JMSNamespaceInfoCollection = class external "Object"
    property length: integer;
    function add(namespace: string = ""; urn: string = ""; implementationUrl: variant = undefined): JObject;
    function item(index: variant): JObject;
    function  GetItems(index: string): JObject; external array;
    procedure SetItems(index: string; value: JObject); external array;
    property Items[index: string]: JObject read GetItems write SetItems; default;
  end;


function MSNamespaceInfoCollection_(index: variant): JObject;external;

type
  JMSNamespaceInfoCollection1 = class external "MSNamespaceInfoCollection"
  public
    property prototype: JMSNamespaceInfoCollection;
    function &new(): JMSNamespaceInfoCollection;
  end;


function MSNamespaceInfoCollection_var: JMSNamespaceInfoCollection1;

type
  JSVGElementInstance = class external "Object"(JEventTarget)
    property previousSibling: JSVGElementInstance;
    property parentNode: JSVGElementInstance;
    property lastChild: JSVGElementInstance;
    property nextSibling: JSVGElementInstance;
    property childNodes: JSVGElementInstanceList;
    property correspondingUseElement: JSVGUseElement;
    property correspondingElement: JSVGElement;
    property firstChild: JSVGElementInstance;
  end;

type
  JSVGElementInstance1 = class external "SVGElementInstance"
  public
    property prototype: JSVGElementInstance;
    function &new(): JSVGElementInstance;
  end;


function SVGElementInstance_var: JSVGElementInstance1;

type
  JMSHTMLUListElementExtensions = class external "Object"
  end;

type
  JSVGCircleElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)
    property cx: JSVGAnimatedLength;
    property r: JSVGAnimatedLength;
    property cy: JSVGAnimatedLength;
  end;

type
  JSVGCircleElement1 = class external "SVGCircleElement"
  public
    property prototype: JSVGCircleElement;
    function &new(): JSVGCircleElement;
  end;


function SVGCircleElement_var: JSVGCircleElement1;

type
  JHTMLBaseFontElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedSizeProperty_HTMLBaseFontElement)//, JDOML2DeprecatedColorProperty)
    property face: string;
  end;

type
  JHTMLBaseFontElement1 = class external "HTMLBaseFontElement"
  public
    property prototype: JHTMLBaseFontElement;
    function &new(): JHTMLBaseFontElement;
  end;


function HTMLBaseFontElement_var: JHTMLBaseFontElement1;

type
  JCustomEvent = class external "Object"(JEvent)
    property detail: JObject;
    procedure initCustomEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; detailArg: JObject);
  end;

type
  JCustomEvent1 = class external "CustomEvent"
  public
    property prototype: JCustomEvent;
    function &new(): JCustomEvent;
  end;


function CustomEvent_var: JCustomEvent1;

type
  JCSSImportRule = class external "Object"(JCSSRule)
    property styleSheet: JCSSStyleSheet;
    property href: string;
    property media: JMediaList;
  end;

type
  JCSSImportRule1 = class external "CSSImportRule"
  public
    property prototype: JCSSImportRule;
    function &new(): JCSSImportRule;
  end;


function CSSImportRule_var: JCSSImportRule1;

type
  JStyleSheetList = class external "Object"
    property length: integer;
    function item(index: integer = 0): JStyleSheet;
    function  GetItems(index: integer): JStyleSheet; external array;
    procedure SetItems(index: integer; value: JStyleSheet); external array;
    property Items[index: integer]: JStyleSheet read GetItems write SetItems; default;
  end;

type
  JStyleSheetList1 = class external "StyleSheetList"
  public
    property prototype: JStyleSheetList;
    function &new(): JStyleSheetList;
  end;


function StyleSheetList_var: JStyleSheetList1;

type
  JHTMLTextAreaElement = class external "Object"(JHTMLElement)//, JMSDataBindingExtensions)//, JMSHTMLTextAreaElementExtensions)
    property value: string;
    property form: JHTMLFormElement;
    property name: string;
    property selectionStart: integer;
    property rows: integer;
    property cols: integer;
    property readOnly: boolean;
    property wrap: string;
    property selectionEnd: integer;
    property &type: string;
    property defaultValue: string;
    procedure setSelectionRange(start: integer; &end: integer);
    procedure select();
  end;

type
  JHTMLTextAreaElement1 = class external "HTMLTextAreaElement"
  public
    property prototype: JHTMLTextAreaElement;
    function &new(): JHTMLTextAreaElement;
  end;


function HTMLTextAreaElement_var: JHTMLTextAreaElement1;

type
  JMSHTMLFormElementExtensions = class external "Object"
    property encoding: string;
  end;

type
  JDOML2DeprecatedMarginStyle = class external "Object"
    property vspace: integer;
    property hspace: integer;
  end;

type
  JGeolocation = class external "Object"
    procedure clearWatch(watchId: integer);
    procedure getCurrentPosition(successCallback: JPositionCallback; errorCallback: JPositionErrorCallback = nil; options: JPositionOptions = nil);
    function watchPosition(successCallback: JPositionCallback; errorCallback: JPositionErrorCallback = nil; options: JPositionOptions = nil): integer;
  end;

type
  JGeolocation1 = class external "Geolocation"
  public
    property prototype: JGeolocation;
    function &new(): JGeolocation;
  end;


function Geolocation_var: JGeolocation1;

type
  JMSWindowModeless = class external "Object"
    property dialogTop: variant;
    property dialogLeft: variant;
    property dialogWidth: variant;
    property dialogHeight: variant;
    property menuArguments: variant;
  end;

type
  THTMLMarqueeElement_onbounce_ = function (ev: JEvent): variant;
  THTMLMarqueeElement_onstart_ = function (ev: JEvent): variant;
  THTMLMarqueeElement_onfinish_ = function (ev: JEvent): variant;
type
  JHTMLMarqueeElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedMarginStyle_HTMLMarqueeElement)//, JMSDataBindingExtensions)//, JMSHTMLMarqueeElementExtensions)//, JDOML2DeprecatedBackgroundColorStyle)
    property width: string;
    property onbounce: THTMLMarqueeElement_onbounce_;
    property trueSpeed: boolean;
    property scrollAmount: integer;
    property scrollDelay: integer;
    property behavior: string;
    property height: string;
    property loop: integer;
    property direction: string;
    property onstart: THTMLMarqueeElement_onstart_;
    property onfinish: THTMLMarqueeElement_onfinish_;
    procedure stop();
    procedure start();
  end;

type
  JHTMLMarqueeElement1 = class external "HTMLMarqueeElement"
  public
    property prototype: JHTMLMarqueeElement;
    function &new(): JHTMLMarqueeElement;
  end;


function HTMLMarqueeElement_var: JHTMLMarqueeElement1;

type
  JSVGRect = class external "Object"
    property y: integer;
    property width: integer;
    property x: integer;
    property height: integer;
  end;

type
  JSVGRect1 = class external "SVGRect"
  public
    property prototype: JSVGRect;
    function &new(): JSVGRect;
  end;


function SVGRect_var: JSVGRect1;

type
  JMSNodeExtensions = class external "Object"
    function swapNode(otherNode: JNode): JNode;
    function removeNode(deep: boolean = false): JNode;
    function replaceNode(replacement: JNode): JNode;
  end;

type
  JKeyboardEventExtensions = class external "Object"
    property keyCode: integer;
    property which: integer;
    property charCode: integer;
  end;

type
  JHistory = class external "Object"
    property length: integer;
    procedure back(distance: variant = undefined);
    procedure forward(distance: variant = undefined);
    procedure go(delta: variant = undefined);
  end;

type
  JHistory1 = class external "History"
  public
    property prototype: JHistory;
    function &new(): JHistory;
  end;


function History_var: JHistory1;

type
  JDocumentStyle = class external "Object"
    property styleSheets: JStyleSheetList;
  end;

type
  JSVGPathSegCurvetoCubicAbs = class external "Object"(JSVGPathSeg)
    property y: integer;
    property y1: integer;
    property x2: integer;
    property x: integer;
    property x1: integer;
    property y2: integer;
  end;

type
  JSVGPathSegCurvetoCubicAbs1 = class external "SVGPathSegCurvetoCubicAbs"
  public
    property prototype: JSVGPathSegCurvetoCubicAbs;
    function &new(): JSVGPathSegCurvetoCubicAbs;
  end;


function SVGPathSegCurvetoCubicAbs_var: JSVGPathSegCurvetoCubicAbs1;

type
  JTimeRanges = class external "Object"
    property length: integer;
    function start(index: integer): integer;
    function &end(index: integer): integer;
  end;

type
  JTimeRanges1 = class external "TimeRanges"
  public
    property prototype: JTimeRanges;
    function &new(): JTimeRanges;
  end;


function TimeRanges_var: JTimeRanges1;

type
  JSVGPathSegCurvetoQuadraticAbs = class external "Object"(JSVGPathSeg)
    property y: integer;
    property y1: integer;
    property x: integer;
    property x1: integer;
  end;

type
  JSVGPathSegCurvetoQuadraticAbs1 = class external "SVGPathSegCurvetoQuadraticAbs"
  public
    property prototype: JSVGPathSegCurvetoQuadraticAbs;
    function &new(): JSVGPathSegCurvetoQuadraticAbs;
  end;


function SVGPathSegCurvetoQuadraticAbs_var: JSVGPathSegCurvetoQuadraticAbs1;

type
  JMSHTMLSelectElementExtensions = class external "Object"
  end;

type
  JSVGPathSegLinetoAbs = class external "Object"(JSVGPathSeg)
    property y: integer;
    property x: integer;
  end;

type
  JSVGPathSegLinetoAbs1 = class external "SVGPathSegLinetoAbs"
  public
    property prototype: JSVGPathSegLinetoAbs;
    function &new(): JSVGPathSegLinetoAbs;
  end;


function SVGPathSegLinetoAbs_var: JSVGPathSegLinetoAbs1;

type
  JMSMouseEventExtensions = class external "Object"
    property toElement: JElement;
    property layerY: integer;
    property fromElement: JElement;
    property which: integer;
    property layerX: integer;
  end;

type
  JHTMLModElement = class external "Object"(JHTMLElement)//, JMSHTMLModElementExtensions)
    property dateTime: string;
    property cite: string;
  end;

type
  JHTMLModElement1 = class external "HTMLModElement"
  public
    property prototype: JHTMLModElement;
    function &new(): JHTMLModElement;
  end;


function HTMLModElement_var: JHTMLModElement1;

type
  JDOML2DeprecatedWordWrapSuppression = class external "Object"
    property noWrap: boolean;
  end;

type
  JBeforeUnloadEvent = class external "Object"(JEvent)
    property returnValue: string;
  end;

type
  JBeforeUnloadEvent1 = class external "BeforeUnloadEvent"
  public
    property prototype: JBeforeUnloadEvent;
    function &new(): JBeforeUnloadEvent;
  end;


function BeforeUnloadEvent_var: JBeforeUnloadEvent1;

type
  JMSPopupWindow = class external "Object"
    property document: JHTMLDocument;
    property isOpen: boolean;
    procedure show(x: integer; y: integer; w: integer; h: integer; element: variant = undefined);
    procedure hide();
  end;

type
  JMSPopupWindow1 = class external "MSPopupWindow"
  public
    property prototype: JMSPopupWindow;
    function &new(): JMSPopupWindow;
  end;


function MSPopupWindow_var: JMSPopupWindow1;

type
  JSVGMatrix = class external "Object"
    property e: integer;
    property c: integer;
    property a: integer;
    property b: integer;
    property d: integer;
    property f: integer;
    function multiply(secondMatrix: JSVGMatrix): JSVGMatrix;
    function flipY(): JSVGMatrix;
    function skewY(angle: integer): JSVGMatrix;
    function inverse(): JSVGMatrix;
    function scaleNonUniform(scaleFactorX: integer; scaleFactorY: integer): JSVGMatrix;
    function rotate(angle: integer): JSVGMatrix;
    function flipX(): JSVGMatrix;
    function translate(x: integer; y: integer): JSVGMatrix;
    function scale(scaleFactor: integer): JSVGMatrix;
    function rotateFromVector(x: integer; y: integer): JSVGMatrix;
    function skewX(angle: integer): JSVGMatrix;
  end;

type
  JSVGMatrix1 = class external "SVGMatrix"
  public
    property prototype: JSVGMatrix;
    function &new(): JSVGMatrix;
  end;


function SVGMatrix_var: JSVGMatrix1;

type
  JSVGUseElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)//, JSVGURIReference)
    property y: JSVGAnimatedLength;
    property width: JSVGAnimatedLength;
    property animatedInstanceRoot: JSVGElementInstance;
    property instanceRoot: JSVGElementInstance;
    property x: JSVGAnimatedLength;
    property height: JSVGAnimatedLength;
  end;

type
  JSVGUseElement1 = class external "SVGUseElement"
  public
    property prototype: JSVGUseElement;
    function &new(): JSVGUseElement;
  end;


function SVGUseElement_var: JSVGUseElement1;

type
  JImageData = class external "Object"
    property width: integer;
    property data: array of integer;
    property height: integer;
  end;

type
  JImageData1 = class external "ImageData"
  public
    property prototype: JImageData;
    function &new(): JImageData;
  end;


function ImageData_var: JImageData1;

type
  TMSHTMLElementExtensions_onlosecapture_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onrowexit_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_oncontrolselect_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onrowsinserted_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onmouseleave_ = function (ev: JMouseEvent): variant;
  TMSHTMLElementExtensions_onpropertychange_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onbeforecut_ = function (ev: JDragEvent): variant;
  TMSHTMLElementExtensions_onbeforepaste_ = function (ev: JDragEvent): variant;
  TMSHTMLElementExtensions_onmove_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onafterupdate_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onbeforecopy_ = function (ev: JDragEvent): variant;
  TMSHTMLElementExtensions_onlayoutcomplete_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onresizeend_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onhelp_ = function (ev: JEvent): variant;
  TMSHTMLElementExtensions_onbeforeactivate_ = function (ev: JUIEvent): variant;
  TMSHTMLElementExtensions_onfocusout_ = function (ev: JFocusEvent): variant;
  TMSHTMLElementExtensions_ondataavailable_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onbeforeupdate_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onfilterchange_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onfocusin_ = function (ev: JFocusEvent): variant;
  TMSHTMLElementExtensions_ondatasetcomplete_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onbeforedeactivate_ = function (ev: JUIEvent): variant;
  TMSHTMLElementExtensions_onresizestart_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onactivate_ = function (ev: JUIEvent): variant;
  TMSHTMLElementExtensions_onmouseenter_ = function (ev: JMouseEvent): variant;
  TMSHTMLElementExtensions_onmovestart_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onselectstart_ = function (ev: JEvent): variant;
  TMSHTMLElementExtensions_onpaste_ = function (ev: JDragEvent): variant;
  TMSHTMLElementExtensions_onerrorupdate_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_ondeactivate_ = function (ev: JUIEvent): variant;
  TMSHTMLElementExtensions_oncut_ = function (ev: JDragEvent): variant;
  TMSHTMLElementExtensions_onmoveend_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onresize_ = function (ev: JUIEvent): variant;
  TMSHTMLElementExtensions_ondatasetchanged_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_oncopy_ = function (ev: JDragEvent): variant;
  TMSHTMLElementExtensions_onrowsdelete_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onrowenter_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_onbeforeeditfocus_ = function (ev: JMSEventObj): variant;
  TMSHTMLElementExtensions_oncellchange_ = function (ev: JMSEventObj): variant;
type
  JMSHTMLElementExtensions = class external "Object"
    property onlosecapture: TMSHTMLElementExtensions_onlosecapture_;
    property onrowexit: TMSHTMLElementExtensions_onrowexit_;
    property oncontrolselect: TMSHTMLElementExtensions_oncontrolselect_;
    property onrowsinserted: TMSHTMLElementExtensions_onrowsinserted_;
    property onmouseleave: TMSHTMLElementExtensions_onmouseleave_;
    property document: JHTMLDocument;
    property behaviorUrns: JMSBehaviorUrnsCollection;
    property onpropertychange: TMSHTMLElementExtensions_onpropertychange_;
    property children: JHTMLCollection;
    property filters: JObject;
    property onbeforecut: TMSHTMLElementExtensions_onbeforecut_;
    property scopeName: string;
    property onbeforepaste: TMSHTMLElementExtensions_onbeforepaste_;
    property onmove: TMSHTMLElementExtensions_onmove_;
    property onafterupdate: TMSHTMLElementExtensions_onafterupdate_;
    property onbeforecopy: TMSHTMLElementExtensions_onbeforecopy_;
    property onlayoutcomplete: TMSHTMLElementExtensions_onlayoutcomplete_;
    property onresizeend: TMSHTMLElementExtensions_onresizeend_;
    property uniqueID: string;
    property onhelp: TMSHTMLElementExtensions_onhelp_;
    property onbeforeactivate: TMSHTMLElementExtensions_onbeforeactivate_;
    property isMultiLine: boolean;
    property uniqueNumber: integer;
    property tagUrn: string;
    property onfocusout: TMSHTMLElementExtensions_onfocusout_;
    property ondataavailable: TMSHTMLElementExtensions_ondataavailable_;
    property hideFocus: boolean;
    property onbeforeupdate: TMSHTMLElementExtensions_onbeforeupdate_;
    property onfilterchange: TMSHTMLElementExtensions_onfilterchange_;
    property onfocusin: TMSHTMLElementExtensions_onfocusin_;
    property recordNumber: variant;
    property parentTextEdit: JElement;
    property ondatasetcomplete: TMSHTMLElementExtensions_ondatasetcomplete_;
    property onbeforedeactivate: TMSHTMLElementExtensions_onbeforedeactivate_;
    property outerText: string;
    property onresizestart: TMSHTMLElementExtensions_onresizestart_;
    property onactivate: TMSHTMLElementExtensions_onactivate_;
    property isTextEdit: boolean;
    property isDisabled: boolean;
    property readyState: string;
    property all: JHTMLCollection;
    property onmouseenter: TMSHTMLElementExtensions_onmouseenter_;
    property onmovestart: TMSHTMLElementExtensions_onmovestart_;
    property onselectstart: TMSHTMLElementExtensions_onselectstart_;
    property onpaste: TMSHTMLElementExtensions_onpaste_;
    property canHaveHTML: boolean;
    property innerText: string;
    property onerrorupdate: TMSHTMLElementExtensions_onerrorupdate_;
    property ondeactivate: TMSHTMLElementExtensions_ondeactivate_;
    property oncut: TMSHTMLElementExtensions_oncut_;
    property onmoveend: TMSHTMLElementExtensions_onmoveend_;
    property onresize: TMSHTMLElementExtensions_onresize_;
    property language: string;
    property ondatasetchanged: TMSHTMLElementExtensions_ondatasetchanged_;
    property oncopy: TMSHTMLElementExtensions_oncopy_;
    property onrowsdelete: TMSHTMLElementExtensions_onrowsdelete_;
    property parentElement: JHTMLElement;
    property onrowenter: TMSHTMLElementExtensions_onrowenter_;
    property onbeforeeditfocus: TMSHTMLElementExtensions_onbeforeeditfocus_;
    property canHaveChildren: boolean;
    property sourceIndex: integer;
    property oncellchange: TMSHTMLElementExtensions_oncellchange_;
    function dragDrop(): boolean;
    procedure releaseCapture();
    procedure addFilter(filter: JObject);
    procedure setCapture(containerCapture: boolean = false);
    function removeBehavior(cookie: integer): boolean;
    function contains(child: JHTMLElement): boolean;
    function applyElement(apply: JElement; where: string = ""): JElement;
    function replaceAdjacentText(where: string; newText: string): string;
    procedure mergeAttributes(source: JHTMLElement; preserveIdentity: boolean = false);
    function insertAdjacentElement(position: string; insertedElement: JElement): JElement;
    procedure insertAdjacentText(where: string; text: string);
    function getAdjacentText(where: string): string;
    procedure removeFilter(filter: JObject);
    procedure setActive();
    function addBehavior(bstrUrl: string; factory: variant = undefined): integer;
    procedure clearAttributes();
  end;

type
  JHTMLTableColElement = class external "Object"(JHTMLElement)//, JMSHTMLTableColElementExtensions)//, JHTMLTableAlignment)//, JDOML2DeprecatedAlignmentStyle_HTMLTableColElement)
    property width: variant;
    property span: integer;
  end;

type
  JHTMLTableColElement1 = class external "HTMLTableColElement"
  public
    property prototype: JHTMLTableColElement;
    function &new(): JHTMLTableColElement;
  end;


function HTMLTableColElement_var: JHTMLTableColElement1;

type
  THTMLDocument_ondragend_ = function (ev: JDragEvent): variant;
  THTMLDocument_ondragover_ = function (ev: JDragEvent): variant;
  THTMLDocument_onkeydown_ = function (ev: JKeyboardEvent): variant;
  THTMLDocument_onkeyup_ = function (ev: JKeyboardEvent): variant;
  THTMLDocument_onreset_ = function (ev: JEvent): variant;
  THTMLDocument_onmouseup_ = function (ev: JMouseEvent): variant;
  THTMLDocument_ondragstart_ = function (ev: JDragEvent): variant;
  THTMLDocument_ondrag_ = function (ev: JDragEvent): variant;
  THTMLDocument_ondragleave_ = function (ev: JDragEvent): variant;
  THTMLDocument_onmouseover_ = function (ev: JMouseEvent): variant;
  THTMLDocument_onpause_ = function (ev: JEvent): variant;
  THTMLDocument_onmousedown_ = function (ev: JMouseEvent): variant;
  THTMLDocument_onseeked_ = function (ev: JEvent): variant;
  THTMLDocument_onclick_ = function (ev: JMouseEvent): variant;
  THTMLDocument_onwaiting_ = function (ev: JEvent): variant;
  THTMLDocument_ondurationchange_ = function (ev: JEvent): variant;
  THTMLDocument_onblur_ = function (ev: JFocusEvent): variant;
  THTMLDocument_onemptied_ = function (ev: JEvent): variant;
  THTMLDocument_onseeking_ = function (ev: JEvent): variant;
  THTMLDocument_oncanplay_ = function (ev: JEvent): variant;
  THTMLDocument_onstalled_ = function (ev: JEvent): variant;
  THTMLDocument_onmousemove_ = function (ev: JMouseEvent): variant;
  THTMLDocument_onratechange_ = function (ev: JEvent): variant;
  THTMLDocument_onloadstart_ = function (ev: JEvent): variant;
  THTMLDocument_ondragenter_ = function (ev: JDragEvent): variant;
  THTMLDocument_onsubmit_ = function (ev: JEvent): variant;
  THTMLDocument_onprogress_ = function (ev: variant): variant;
  THTMLDocument_ondblclick_ = function (ev: JMouseEvent): variant;
  THTMLDocument_oncontextmenu_ = function (ev: JMouseEvent): variant;
  THTMLDocument_onchange_ = function (ev: JEvent): variant;
  THTMLDocument_onloadedmetadata_ = function (ev: JEvent): variant;
  THTMLDocument_onerror_ = function (ev: JEvent): variant;
  THTMLDocument_onplay_ = function (ev: JEvent): variant;
  THTMLDocument_onplaying_ = function (ev: JEvent): variant;
  THTMLDocument_oncanplaythrough_ = function (ev: JEvent): variant;
  THTMLDocument_onabort_ = function (ev: JUIEvent): variant;
  THTMLDocument_onreadystatechange_ = function (ev: JEvent): variant;
  THTMLDocument_onkeypress_ = function (ev: JKeyboardEvent): variant;
  THTMLDocument_onloadeddata_ = function (ev: JEvent): variant;
  THTMLDocument_onsuspend_ = function (ev: JEvent): variant;
  THTMLDocument_onfocus_ = function (ev: JFocusEvent): variant;
  THTMLDocument_ontimeupdate_ = function (ev: JEvent): variant;
  THTMLDocument_onselect_ = function (ev: JUIEvent): variant;
  THTMLDocument_ondrop_ = function (ev: JDragEvent): variant;
  THTMLDocument_onmouseout_ = function (ev: JMouseEvent): variant;
  THTMLDocument_onended_ = function (ev: JEvent): variant;
  THTMLDocument_onscroll_ = function (ev: JUIEvent): variant;
  THTMLDocument_onmousewheel_ = function (ev: JMouseWheelEvent): variant;
  THTMLDocument_onload_ = function (ev: JEvent): variant;
  THTMLDocument_onvolumechange_ = function (ev: JEvent): variant;
  THTMLDocument_oninput_ = function (ev: JEvent): variant;
type
  JHTMLDocument = class external "Object"(JMSEventAttachmentTarget)//, JMSHTMLDocumentSelection)//, JMSHTMLDocumentExtensions)//, JMSNodeExtensions)//, JMSResourceMetadata)//, JMSHTMLDocumentEventExtensions)//, JMSHTMLDocumentViewExtensions)
    property ondragend: THTMLDocument_ondragend_;
    property ondragover: THTMLDocument_ondragover_;
    property onkeydown: THTMLDocument_onkeydown_;
    property bgColor: string;
    property onkeyup: THTMLDocument_onkeyup_;
    property onreset: THTMLDocument_onreset_;
    property onmouseup: THTMLDocument_onmouseup_;
    property ondragstart: THTMLDocument_ondragstart_;
    property scripts: JHTMLCollection;
    property ondrag: THTMLDocument_ondrag_;
    property linkColor: string;
    property ondragleave: THTMLDocument_ondragleave_;
    property onmouseover: THTMLDocument_onmouseover_;
    property onpause: THTMLDocument_onpause_;
    property charset: string;
    property vlinkColor: string;
    property onmousedown: THTMLDocument_onmousedown_;
    property onseeked: THTMLDocument_onseeked_;
    property title: string;
    property onclick: THTMLDocument_onclick_;
    property onwaiting: THTMLDocument_onwaiting_;
    property defaultCharset: string;
    property embeds: JHTMLCollection;
    property ondurationchange: THTMLDocument_ondurationchange_;
    property all: JHTMLCollection;
    property applets: JHTMLCollection;
    property forms: JHTMLCollection;
    property onblur: THTMLDocument_onblur_;
    property dir: string;
    property body: JHTMLElement;
    property designMode: string;
    property onemptied: THTMLDocument_onemptied_;
    property domain: string;
    property onseeking: THTMLDocument_onseeking_;
    property oncanplay: THTMLDocument_oncanplay_;
    property onstalled: THTMLDocument_onstalled_;
    property onmousemove: THTMLDocument_onmousemove_;
    property onratechange: THTMLDocument_onratechange_;
    property onloadstart: THTMLDocument_onloadstart_;
    property ondragenter: THTMLDocument_ondragenter_;
    property onsubmit: THTMLDocument_onsubmit_;
    property onprogress: THTMLDocument_onprogress_;
    property ondblclick: THTMLDocument_ondblclick_;
    property oncontextmenu: THTMLDocument_oncontextmenu_;
    property activeElement: JElement;
    property onchange: THTMLDocument_onchange_;
    property onloadedmetadata: THTMLDocument_onloadedmetadata_;
    property onerror: THTMLDocument_onerror_;
    property onplay: THTMLDocument_onplay_;
    property links: JHTMLCollection;
    property onplaying: THTMLDocument_onplaying_;
    property URL: string;
    property images: JHTMLCollection;
    property head: JHTMLHeadElement;
    property location: JLocation;
    property cookie: string;
    property oncanplaythrough: THTMLDocument_oncanplaythrough_;
    property onabort: THTMLDocument_onabort_;
    property characterSet: string;
    property anchors: JHTMLCollection;
    property lastModified: string;
    property onreadystatechange: THTMLDocument_onreadystatechange_;
    property onkeypress: THTMLDocument_onkeypress_;
    property onloadeddata: THTMLDocument_onloadeddata_;
    property plugins: JHTMLCollection;
    property onsuspend: THTMLDocument_onsuspend_;
    property referrer: string;
    property readyState: string;
    property alinkColor: string;
    property onfocus: THTMLDocument_onfocus_;
    property fgColor: string;
    property ontimeupdate: THTMLDocument_ontimeupdate_;
    property onselect: THTMLDocument_onselect_;
    property ondrop: THTMLDocument_ondrop_;
    property onmouseout: THTMLDocument_onmouseout_;
    property onended: THTMLDocument_onended_;
    property compatMode: string;
    property onscroll: THTMLDocument_onscroll_;
    property onmousewheel: THTMLDocument_onmousewheel_;
    property onload: THTMLDocument_onload_;
    property onvolumechange: THTMLDocument_onvolumechange_;
    property oninput: THTMLDocument_oninput_;
    function queryCommandValue(commandId: string): string;
    function queryCommandIndeterm(commandId: string): boolean;
    function execCommand(commandId: string; showUI: boolean = false; value: variant = undefined): boolean;
    function getElementsByName(elementName: string): JNodeList;
    procedure writeln({many?}content: array of string);
    function open(url: string = ""; name: string = ""; features: string = ""; replace: boolean = false): variant;
    function queryCommandState(commandId: string): boolean;
    procedure close();
    function hasFocus(): boolean;
    function getElementsByClassName(classNames: string): JNodeList;
    function queryCommandSupported(commandId: string): boolean;
    function getSelection(): JSelection;
    function queryCommandEnabled(commandId: string): boolean;
    procedure write({many?}content: array of string);
    function queryCommandText(commandId: string): string;
  end;

type
  JSVGException = class external "Object"
    property code: integer;
    property message: string;
    function toString(): string;
    property SVG_MATRIX_NOT_INVERTABLE: integer;
    property SVG_WRONG_TYPE_ERR: integer;
    property SVG_INVALID_VALUE_ERR: integer;
  end;

type
  JSVGException1 = class external "SVGException"
  public
    property prototype: JSVGException;
    function &new(): JSVGException;
    property SVG_MATRIX_NOT_INVERTABLE: integer;
    property SVG_WRONG_TYPE_ERR: integer;
    property SVG_INVALID_VALUE_ERR: integer;
  end;


function SVGException_var: JSVGException1;

type
  JDOML2DeprecatedTableCellHeight = class external "Object"
    property height: variant;
  end;

type
  JHTMLTableAlignment = class external "Object"
    property ch: string;
    property vAlign: string;
    property chOff: string;
  end;

type
  JSVGAnimatedEnumeration = class external "Object"
    property animVal: integer;
    property baseVal: integer;
  end;

type
  JSVGAnimatedEnumeration1 = class external "SVGAnimatedEnumeration"
  public
    property prototype: JSVGAnimatedEnumeration;
    function &new(): JSVGAnimatedEnumeration;
  end;


function SVGAnimatedEnumeration_var: JSVGAnimatedEnumeration1;

type
  JSVGLinearGradientElement = class external "Object"(JSVGGradientElement)
    property y1: JSVGAnimatedLength;
    property x2: JSVGAnimatedLength;
    property x1: JSVGAnimatedLength;
    property y2: JSVGAnimatedLength;
  end;

type
  JSVGLinearGradientElement1 = class external "SVGLinearGradientElement"
  public
    property prototype: JSVGLinearGradientElement;
    function &new(): JSVGLinearGradientElement;
  end;


function SVGLinearGradientElement_var: JSVGLinearGradientElement1;

type
  JDOML2DeprecatedSizeProperty = class external "Object"
    property size: integer;
  end;

type
  JMSHTMLHeadingElementExtensions = class external "Object"(JDOML2DeprecatedTextFlowControl_HTMLBlockElement)
  end;

type
  JMSBorderColorStyle_HTMLTableCellElement = class external "Object"
    property borderColor: variant;
  end;

type
  JDOML2DeprecatedWidthStyle_HTMLHRElement = class external "Object"
    property width: integer;
  end;

type
  JHTMLUListElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedListSpaceReduction)//, JDOML2DeprecatedListNumberingAndBulletStyle)//, JMSHTMLUListElementExtensions)
  end;

type
  JHTMLUListElement1 = class external "HTMLUListElement"
  public
    property prototype: JHTMLUListElement;
    function &new(): JHTMLUListElement;
  end;


function HTMLUListElement_var: JHTMLUListElement1;

type
  JSVGRectElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)
    property y: JSVGAnimatedLength;
    property width: JSVGAnimatedLength;
    property ry: JSVGAnimatedLength;
    property rx: JSVGAnimatedLength;
    property x: JSVGAnimatedLength;
    property height: JSVGAnimatedLength;
  end;

type
  JSVGRectElement1 = class external "SVGRectElement"
  public
    property prototype: JSVGRectElement;
    function &new(): JSVGRectElement;
  end;


function SVGRectElement_var: JSVGRectElement1;

type
  JDOML2DeprecatedBorderStyle = class external "Object"
    property border: string;
  end;

type
  JHTMLDivElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedAlignmentStyle_HTMLDivElement)//, JMSHTMLDivElementExtensions)//, JMSDataBindingExtensions)
  end;

type
  JHTMLDivElement1 = class external "HTMLDivElement"
  public
    property prototype: JHTMLDivElement;
    function &new(): JHTMLDivElement;
  end;


function HTMLDivElement_var: JHTMLDivElement1;

type
  JNavigatorDoNotTrack = class external "Object"
    property msDoNotTrack: string;
  end;

type
  JSVG1_1Properties = class external "Object"
    property fillRule: string;
    property strokeLinecap: string;
    property stopColor: string;
    property glyphOrientationHorizontal: string;
    property kerning: string;
    property alignmentBaseline: string;
    property dominantBaseline: string;
    property fill: string;
    property strokeMiterlimit: string;
    property marker: string;
    property glyphOrientationVertical: string;
    property markerMid: string;
    property textAnchor: string;
    property fillOpacity: string;
    property strokeDasharray: string;
    property mask: string;
    property stopOpacity: string;
    property stroke: string;
    property strokeDashoffset: string;
    property strokeOpacity: string;
    property markerStart: string;
    property pointerEvents: string;
    property baselineShift: string;
    property markerEnd: string;
    property clipRule: string;
    property strokeLinejoin: string;
    property clipPath: string;
    property strokeWidth: string;
  end;

type
  JNamedNodeMap = class external "Object"
    property length: integer;
    function removeNamedItemNS(namespaceURI: string; localName: string): JNode;
    function item(index: integer): JNode;
    function  GetItems(index: integer): JNode; external array;
    procedure SetItems(index: integer; value: JNode); external array;
    property Items[index: integer]: JNode read GetItems write SetItems; default;
    function removeNamedItem(name: string): JNode;
    function getNamedItem(name: string): JNode;
    function  GetItems(name: string): JNode; external array;
    procedure SetItems(name: string; value: JNode); external array;
    property Items[name: string]: JNode read GetItems write SetItems; default;
    function setNamedItem(arg: JNode): JNode;
    function getNamedItemNS(namespaceURI: string; localName: string): JNode;
    function setNamedItemNS(arg: JNode): JNode;
  end;

type
  JNamedNodeMap1 = class external "NamedNodeMap"
  public
    property prototype: JNamedNodeMap;
    function &new(): JNamedNodeMap;
  end;


function NamedNodeMap_var: JNamedNodeMap1;

type
  JMediaList = class external "Object"
    property length: integer;
    property mediaText: string;
    procedure deleteMedium(oldMedium: string);
    procedure appendMedium(newMedium: string);
    function item(index: integer): string;
    function  GetItems(index: integer): string; external array;
    procedure SetItems(index: integer; value: string); external array;
    property Items[index: integer]: string read GetItems write SetItems; default;
    function toString(): string;
  end;

type
  JMediaList1 = class external "MediaList"
  public
    property prototype: JMediaList;
    function &new(): JMediaList;
  end;


function MediaList_var: JMediaList1;

type
  JSVGPathSegCurvetoQuadraticSmoothAbs = class external "Object"(JSVGPathSeg)
    property y: integer;
    property x: integer;
  end;

type
  JSVGPathSegCurvetoQuadraticSmoothAbs1 = class external "SVGPathSegCurvetoQuadraticSmoothAbs"
  public
    property prototype: JSVGPathSegCurvetoQuadraticSmoothAbs;
    function &new(): JSVGPathSegCurvetoQuadraticSmoothAbs;
  end;


function SVGPathSegCurvetoQuadraticSmoothAbs_var: JSVGPathSegCurvetoQuadraticSmoothAbs1;

type
  JSVGLengthList = class external "Object"
    property numberOfItems: integer;
    function replaceItem(newItem: JSVGLength; index: integer): JSVGLength;
    function getItem(index: integer): JSVGLength;
    procedure clear();
    function appendItem(newItem: JSVGLength): JSVGLength;
    function initialize(newItem: JSVGLength): JSVGLength;
    function removeItem(index: integer): JSVGLength;
    function insertItemBefore(newItem: JSVGLength; index: integer): JSVGLength;
  end;

type
  JSVGLengthList1 = class external "SVGLengthList"
  public
    property prototype: JSVGLengthList;
    function &new(): JSVGLengthList;
  end;


function SVGLengthList_var: JSVGLengthList1;

type
  JSVGPathSegCurvetoCubicSmoothRel = class external "Object"(JSVGPathSeg)
    property y: integer;
    property x2: integer;
    property x: integer;
    property y2: integer;
  end;

type
  JSVGPathSegCurvetoCubicSmoothRel1 = class external "SVGPathSegCurvetoCubicSmoothRel"
  public
    property prototype: JSVGPathSegCurvetoCubicSmoothRel;
    function &new(): JSVGPathSegCurvetoCubicSmoothRel;
  end;


function SVGPathSegCurvetoCubicSmoothRel_var: JSVGPathSegCurvetoCubicSmoothRel1;

type
  TMSWindowExtensions_onmouseleave_ = function (ev: JMouseEvent): variant;
  TMSWindowExtensions_onmouseenter_ = function (ev: JMouseEvent): variant;
  TMSWindowExtensions_onhelp_ = function (ev: JEvent): variant;
  TMSWindowExtensions_onfocusout_ = function (ev: JFocusEvent): variant;
  TMSWindowExtensions_onfocusin_ = function (ev: JFocusEvent): variant;
type
  JMSWindowExtensions = class external "Object"
    property status: string;
    property onmouseleave: TMSWindowExtensions_onmouseleave_;
    property screenLeft: integer;
    property offscreenBuffering: variant;
    property maxConnectionsPerServer: integer;
    property onmouseenter: TMSWindowExtensions_onmouseenter_;
    property clipboardData: JDataTransfer;
    property defaultStatus: string;
    property clientInformation: JNavigator;
    property closed: boolean;
    property onhelp: TMSWindowExtensions_onhelp_;
    property external: JBrowserPublic;
    property event: JMSEventObj;
    property onfocusout: TMSWindowExtensions_onfocusout_;
    property screenTop: integer;
    property onfocusin: TMSWindowExtensions_onfocusin_;
    function showModelessDialog(url: string = ""; argument: variant = undefined; options: variant = undefined): JWindow;
    procedure navigate(url: string);
    procedure resizeBy(x: integer = 0; y: integer = 0);
    function item(index: variant): variant;
    procedure resizeTo(x: integer = 0; y: integer = 0);
    function createPopup(arguments: variant = undefined): JMSPopupWindow;
    function toStaticHTML(html: string): string;
    function execScript(code: string; language: string = ""): variant;
    procedure msWriteProfilerMark(profilerMarkName: string);
    procedure moveTo(x: integer = 0; y: integer = 0);
    procedure moveBy(x: integer = 0; y: integer = 0);
    procedure showHelp(url: string; helpArg: variant = undefined; features: string = "");
  end;

type
  JProcessingInstruction = class external "Object"(JNode)
    property target: string;
    property data: string;
  end;

type
  JProcessingInstruction1 = class external "ProcessingInstruction"
  public
    property prototype: JProcessingInstruction;
    function &new(): JProcessingInstruction;
  end;


function ProcessingInstruction_var: JProcessingInstruction1;

type
  JMSBehaviorUrnsCollection = class external "Object"
    property length: integer;
    function item(index: integer): string;
  end;

type
  JMSBehaviorUrnsCollection1 = class external "MSBehaviorUrnsCollection"
  public
    property prototype: JMSBehaviorUrnsCollection;
    function &new(): JMSBehaviorUrnsCollection;
  end;


function MSBehaviorUrnsCollection_var: JMSBehaviorUrnsCollection1;

type
  JCSSFontFaceRule = class external "Object"(JCSSRule)
    property style: JCSSStyleDeclaration;
  end;

type
  JCSSFontFaceRule1 = class external "CSSFontFaceRule"
  public
    property prototype: JCSSFontFaceRule;
    function &new(): JCSSFontFaceRule;
  end;


function CSSFontFaceRule_var: JCSSFontFaceRule1;

type
  JDOML2DeprecatedBackgroundStyle = class external "Object"
    property background: string;
  end;

type
  JTextEvent = class external "Object"(JUIEvent)
    property inputMethod: integer;
    property data: string;
    property locale: string;
    procedure initTextEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; dataArg: string; inputMethod: integer; locale: string);
    property DOM_INPUT_METHOD_KEYBOARD: integer;
    property DOM_INPUT_METHOD_DROP: integer;
    property DOM_INPUT_METHOD_IME: integer;
    property DOM_INPUT_METHOD_SCRIPT: integer;
    property DOM_INPUT_METHOD_VOICE: integer;
    property DOM_INPUT_METHOD_UNKNOWN: integer;
    property DOM_INPUT_METHOD_PASTE: integer;
    property DOM_INPUT_METHOD_HANDWRITING: integer;
    property DOM_INPUT_METHOD_OPTION: integer;
    property DOM_INPUT_METHOD_MULTIMODAL: integer;
  end;

type
  JTextEvent1 = class external "TextEvent"
  public
    property prototype: JTextEvent;
    function &new(): JTextEvent;
    property DOM_INPUT_METHOD_KEYBOARD: integer;
    property DOM_INPUT_METHOD_DROP: integer;
    property DOM_INPUT_METHOD_IME: integer;
    property DOM_INPUT_METHOD_SCRIPT: integer;
    property DOM_INPUT_METHOD_VOICE: integer;
    property DOM_INPUT_METHOD_UNKNOWN: integer;
    property DOM_INPUT_METHOD_PASTE: integer;
    property DOM_INPUT_METHOD_HANDWRITING: integer;
    property DOM_INPUT_METHOD_OPTION: integer;
    property DOM_INPUT_METHOD_MULTIMODAL: integer;
  end;


function TextEvent_var: JTextEvent1;

type
  JMSHTMLHRElementExtensions = class external "Object"(JDOML2DeprecatedColorProperty)
  end;

type
  JAbstractView = class external "Object"
    property styleMedia: JStyleMedia;
    property document: JDocument;
  end;

type
  JDocumentFragment = class external "Object"(JNode)//, JNodeSelector)//, JMSEventAttachmentTarget)//, JMSNodeExtensions)
  end;

type
  JDocumentFragment1 = class external "DocumentFragment"
  public
    property prototype: JDocumentFragment;
    function &new(): JDocumentFragment;
  end;


function DocumentFragment_var: JDocumentFragment1;

type
  JSVGPolylineElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGAnimatedPoints)//, JSVGTests)
  end;

type
  JSVGPolylineElement1 = class external "SVGPolylineElement"
  public
    property prototype: JSVGPolylineElement;
    function &new(): JSVGPolylineElement;
  end;


function SVGPolylineElement_var: JSVGPolylineElement1;

type
  JDOML2DeprecatedWidthStyle = class external "Object"
    property width: integer;
  end;

type
  JDOML2DeprecatedAlignmentStyle_HTMLHeadingElement = class external "Object"
    property align: string;
  end;

type
  JSVGAnimatedPathData = class external "Object"
    property pathSegList: JSVGPathSegList;
  end;

type
  JPosition = class external "Object"
    property timestamp: JDate;
    property coords: JCoordinates;
  end;

type
  JPosition1 = class external "Position"
  public
    property prototype: JPosition;
    function &new(): JPosition;
  end;


function Position_var: JPosition1;

type
  JBookmarkCollection = class external "Object"
    property length: integer;
    function item(index: integer): variant;
    function  GetItems(index: integer): variant; external array;
    procedure SetItems(index: integer; value: variant); external array;
    property Items[index: integer]: variant read GetItems write SetItems; default;
  end;

type
  JBookmarkCollection1 = class external "BookmarkCollection"
  public
    property prototype: JBookmarkCollection;
    function &new(): JBookmarkCollection;
  end;


function BookmarkCollection_var: JBookmarkCollection1;

type
  JCSSPageRule = class external "Object"(JCSSRule)//, JStyleSheetPage)
    property selectorText: string;
    property style: JCSSStyleDeclaration;
  end;

type
  JCSSPageRule1 = class external "CSSPageRule"
  public
    property prototype: JCSSPageRule;
    function &new(): JCSSPageRule;
  end;


function CSSPageRule_var: JCSSPageRule1;

type
  JWindowPerformance = class external "Object"
    property performance: variant;
  end;

type
  JHTMLBRElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedTextFlowControl_HTMLBRElement)
  end;

type
  JHTMLBRElement1 = class external "HTMLBRElement"
  public
    property prototype: JHTMLBRElement;
    function &new(): JHTMLBRElement;
  end;


function HTMLBRElement_var: JHTMLBRElement1;

type
  JMSHTMLDivElementExtensions = class external "Object"(JDOML2DeprecatedWordWrapSuppression_HTMLDivElement)
  end;

type
  JDOML2DeprecatedBorderStyle_HTMLInputElement = class external "Object"
    property border: string;
  end;

type
  JHTMLSpanElement = class external "Object"(JHTMLElement)//, JMSHTMLSpanElementExtensions)//, JMSDataBindingExtensions)
  end;

type
  JHTMLSpanElement1 = class external "HTMLSpanElement"
  public
    property prototype: JHTMLSpanElement;
    function &new(): JHTMLSpanElement;
  end;


function HTMLSpanElement_var: JHTMLSpanElement1;

type
  JHTMLHRElementDOML2Deprecated = class external "Object"
    property noShade: boolean;
  end;

type
  JHTMLHeadElement = class external "Object"(JHTMLElement)
    property profile: string;
  end;

type
  JHTMLHeadElement1 = class external "HTMLHeadElement"
  public
    property prototype: JHTMLHeadElement;
    function &new(): JHTMLHeadElement;
  end;


function HTMLHeadElement_var: JHTMLHeadElement1;

type
  JNodeFilterCallback = class external "Object"
  end;


function NodeFilterCallback_({many?}args: array of variant): variant;external;

type
  JHTMLHeadingElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedAlignmentStyle_HTMLHeadingElement)//, JMSHTMLHeadingElementExtensions)
  end;

type
  JHTMLHeadingElement1 = class external "HTMLHeadingElement"
  public
    property prototype: JHTMLHeadingElement;
    function &new(): JHTMLHeadingElement;
  end;


function HTMLHeadingElement_var: JHTMLHeadingElement1;

type
  JHTMLFormElement = class external "Object"(JHTMLElement)//, JMSHTMLFormElementExtensions)//, JMSHTMLCollectionExtensions)
    property length: integer;
    property target: string;
    property acceptCharset: string;
    property enctype: string;
    property elements: JHTMLCollection;
    property action: string;
    property name: string;
    property method: string;
    procedure reset();
    function item(name: variant = undefined; index: variant = undefined): variant;
    procedure submit();
    function namedItem(name: string): variant;
    function  GetItems(name: string): variant; external array;
    procedure SetItems(name: string; value: variant); external array;
    property Items[name: string]: variant read GetItems write SetItems; default;
  end;


function HTMLFormElement_(name: variant; index: variant): variant;external;
function HTMLFormElement_(name: string): variant;external;

type
  JHTMLFormElement1 = class external "HTMLFormElement"
  public
    property prototype: JHTMLFormElement;
    function &new(): JHTMLFormElement;
  end;


function HTMLFormElement_var: JHTMLFormElement1;

type
  JSVGZoomAndPan = class external "Object"
    property zoomAndPan: integer;
    property SVG_ZOOMANDPAN_MAGNIFY: integer;
    property SVG_ZOOMANDPAN_UNKNOWN: integer;
    property SVG_ZOOMANDPAN_DISABLE: integer;
  end;

type
  JSVGZoomAndPan1 = class external "SVGZoomAndPan"
  public
    property prototype: JSVGZoomAndPan;
    function &new(): JSVGZoomAndPan;
    property SVG_ZOOMANDPAN_MAGNIFY: integer;
    property SVG_ZOOMANDPAN_UNKNOWN: integer;
    property SVG_ZOOMANDPAN_DISABLE: integer;
  end;


function SVGZoomAndPan_var: JSVGZoomAndPan1;

type
  JElementCSSInlineStyle = class external "Object"(JMSElementCSSInlineStyleExtensions)
    property runtimeStyle: JMSStyleCSSProperties;
    property currentStyle: JMSCurrentStyleCSSProperties;
  end;

type
  JDOMParser = class external "Object"
    function parseFromString(source: string; mimeType: string): JDocument;
  end;

type
  JDOMParser1 = class external "DOMParser"
  public
    property prototype: JDOMParser;
    function &new(): JDOMParser;
  end;


function DOMParser_var: JDOMParser1;

type
  JMSMimeTypesCollection = class external "Object"
    property length: integer;
  end;

type
  JMSMimeTypesCollection1 = class external "MSMimeTypesCollection"
  public
    property prototype: JMSMimeTypesCollection;
    function &new(): JMSMimeTypesCollection;
  end;


function MSMimeTypesCollection_var: JMSMimeTypesCollection1;

type
  JDOML2DeprecatedBorderStyle_HTMLTableElement = class external "Object"
    property border: string;
  end;

type
  JDOML2DeprecatedWidthStyle_HTMLAppletElement = class external "Object"
    property width: integer;
  end;

type
  JSVGTextPathElement = class external "Object"(JSVGTextContentElement)//, JSVGURIReference)
    property startOffset: JSVGAnimatedLength;
    property method: JSVGAnimatedEnumeration;
    property spacing: JSVGAnimatedEnumeration;
    property TEXTPATH_SPACINGTYPE_EXACT: integer;
    property TEXTPATH_METHODTYPE_STRETCH: integer;
    property TEXTPATH_SPACINGTYPE_AUTO: integer;
    property TEXTPATH_SPACINGTYPE_UNKNOWN: integer;
    property TEXTPATH_METHODTYPE_UNKNOWN: integer;
    property TEXTPATH_METHODTYPE_ALIGN: integer;
  end;

type
  JSVGTextPathElement1 = class external "SVGTextPathElement"
  public
    property prototype: JSVGTextPathElement;
    function &new(): JSVGTextPathElement;
    property TEXTPATH_SPACINGTYPE_EXACT: integer;
    property TEXTPATH_METHODTYPE_STRETCH: integer;
    property TEXTPATH_SPACINGTYPE_AUTO: integer;
    property TEXTPATH_SPACINGTYPE_UNKNOWN: integer;
    property TEXTPATH_METHODTYPE_UNKNOWN: integer;
    property TEXTPATH_METHODTYPE_ALIGN: integer;
  end;


function SVGTextPathElement_var: JSVGTextPathElement1;

type
  JNodeList = class external "Object"
    property length: integer;
    function item(index: integer): JNode;
    function  GetItems(index: integer): JNode; external array;
    procedure SetItems(index: integer; value: JNode); external array;
    property Items[index: integer]: JNode read GetItems write SetItems; default;
  end;

type
  JNodeList1 = class external "NodeList"
  public
    property prototype: JNodeList;
    function &new(): JNodeList;
  end;


function NodeList_var: JNodeList1;

type
  JHTMLDTElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedWordWrapSuppression_HTMLDTElement)
  end;

type
  JHTMLDTElement1 = class external "HTMLDTElement"
  public
    property prototype: JHTMLDTElement;
    function &new(): JHTMLDTElement;
  end;


function HTMLDTElement_var: JHTMLDTElement1;

type
  JXMLSerializer = class external "Object"
    function serializeToString(target: JNode): string;
  end;

type
  JXMLSerializer1 = class external "XMLSerializer"
  public
    property prototype: JXMLSerializer;
    function &new(): JXMLSerializer;
  end;


function XMLSerializer_var: JXMLSerializer1;

type
  JStyleSheetPage = class external "Object"
    property pseudoClass: string;
    property selector: string;
  end;

type
  JDOML2DeprecatedWordWrapSuppression_HTMLDDElement = class external "Object"
    property noWrap: boolean;
  end;

type
  JMSHTMLTableRowElementExtensions = class external "Object"
    property height: variant;
  end;

type
  JDOML2DeprecatedTextFlowControl_HTMLBRElement = class external "Object"
    property clear: string;
  end;

type
  JMSHTMLParagraphElementExtensions = class external "Object"(JDOML2DeprecatedTextFlowControl_HTMLBlockElement)
  end;

type
  JNodeFilter = class external "Object"
    function acceptNode(n: JNode): integer;
    property SHOW_ENTITY_REFERENCE: integer;
    property SHOW_NOTATION: integer;
    property SHOW_ENTITY: integer;
    property SHOW_DOCUMENT: integer;
    property SHOW_PROCESSING_INSTRUCTION: integer;
    property FILTER_REJECT: integer;
    property SHOW_CDATA_SECTION: integer;
    property FILTER_ACCEPT: integer;
    property SHOW_ALL: integer;
    property SHOW_DOCUMENT_TYPE: integer;
    property SHOW_TEXT: integer;
    property SHOW_ELEMENT: integer;
    property SHOW_COMMENT: integer;
    property FILTER_SKIP: integer;
    property SHOW_ATTRIBUTE: integer;
    property SHOW_DOCUMENT_FRAGMENT: integer;
  end;

type
  JNodeFilter1 = class external "NodeFilter"
  public
    property prototype: JNodeFilter;
    function &new(): JNodeFilter;
    property SHOW_ENTITY_REFERENCE: integer;
    property SHOW_NOTATION: integer;
    property SHOW_ENTITY: integer;
    property SHOW_DOCUMENT: integer;
    property SHOW_PROCESSING_INSTRUCTION: integer;
    property FILTER_REJECT: integer;
    property SHOW_CDATA_SECTION: integer;
    property FILTER_ACCEPT: integer;
    property SHOW_ALL: integer;
    property SHOW_DOCUMENT_TYPE: integer;
    property SHOW_TEXT: integer;
    property SHOW_ELEMENT: integer;
    property SHOW_COMMENT: integer;
    property FILTER_SKIP: integer;
    property SHOW_ATTRIBUTE: integer;
    property SHOW_DOCUMENT_FRAGMENT: integer;
  end;


function NodeFilter_var: JNodeFilter1;

type
  JMSBorderColorStyle_HTMLFrameElement = class external "Object"
    property borderColor: variant;
  end;

type
  JMSHTMLOListElementExtensions = class external "Object"
  end;

type
  JDOML2DeprecatedWordWrapSuppression_HTMLDTElement = class external "Object"
    property noWrap: boolean;
  end;

type
  JScreenView = class external "Object"(JAbstractView)
    property outerWidth: integer;
    property pageXOffset: integer;
    property innerWidth: integer;
    property pageYOffset: integer;
    property screenY: integer;
    property outerHeight: integer;
    property screen: JScreen;
    property innerHeight: integer;
    property screenX: integer;
    procedure scroll(x: integer = 0; y: integer = 0);
    procedure scrollBy(x: integer = 0; y: integer = 0);
    procedure scrollTo(x: integer = 0; y: integer = 0);
  end;

type
  JDOML2DeprecatedMarginStyle_HTMLObjectElement = class external "Object"
    property vspace: integer;
    property hspace: integer;
  end;

type
  JMSHTMLTableSectionElementExtensions = class external "Object"(JDOML2DeprecatedBackgroundColorStyle)
    function moveRow(indexFrom: integer = 0; indexTo: integer = 0): JObject;
  end;

type
  JHTMLFieldSetElement = class external "Object"(JHTMLElement)//, JMSHTMLFieldSetElementExtensions)
    property form: JHTMLFormElement;
  end;

type
  JHTMLFieldSetElement1 = class external "HTMLFieldSetElement"
  public
    property prototype: JHTMLFieldSetElement;
    function &new(): JHTMLFieldSetElement;
  end;


function HTMLFieldSetElement_var: JHTMLFieldSetElement1;

type
  JMediaError = class external "Object"
    property code: integer;
    property MEDIA_ERR_ABORTED: integer;
    property MEDIA_ERR_NETWORK: integer;
    property MEDIA_ERR_SRC_NOT_SUPPORTED: integer;
    property MEDIA_ERR_DECODE: integer;
  end;

type
  JMediaError1 = class external "MediaError"
  public
    property prototype: JMediaError;
    function &new(): JMediaError;
    property MEDIA_ERR_ABORTED: integer;
    property MEDIA_ERR_NETWORK: integer;
    property MEDIA_ERR_SRC_NOT_SUPPORTED: integer;
    property MEDIA_ERR_DECODE: integer;
  end;


function MediaError_var: JMediaError1;

type
  JSVGNumberList = class external "Object"
    property numberOfItems: integer;
    function replaceItem(newItem: JSVGNumber; index: integer): JSVGNumber;
    function getItem(index: integer): JSVGNumber;
    procedure clear();
    function appendItem(newItem: JSVGNumber): JSVGNumber;
    function initialize(newItem: JSVGNumber): JSVGNumber;
    function removeItem(index: integer): JSVGNumber;
    function insertItemBefore(newItem: JSVGNumber; index: integer): JSVGNumber;
  end;

type
  JSVGNumberList1 = class external "SVGNumberList"
  public
    property prototype: JSVGNumberList;
    function &new(): JSVGNumberList;
  end;


function SVGNumberList_var: JSVGNumberList1;

type
  JHTMLBGSoundElement = class external "Object"(JHTMLElement)
    property balance: variant;
    property volume: variant;
    property src: string;
    property loop: integer;
  end;

type
  JHTMLBGSoundElement1 = class external "HTMLBGSoundElement"
  public
    property prototype: JHTMLBGSoundElement;
    function &new(): JHTMLBGSoundElement;
  end;


function HTMLBGSoundElement_var: JHTMLBGSoundElement1;

type
  JComment = class external "Object"(JCharacterData)//, JMSCommentExtensions)
  end;

type
  JComment1 = class external "Comment"
  public
    property prototype: JComment;
    function &new(): JComment;
  end;


function Comment_var: JComment1;

type
  JCanvasPattern = class external "Object"
  end;

type
  JCanvasPattern1 = class external "CanvasPattern"
  public
    property prototype: JCanvasPattern;
    function &new(): JCanvasPattern;
  end;


function CanvasPattern_var: JCanvasPattern1;

type
  JHTMLHRElement = class external "Object"(JHTMLElement)//, JDOML2DeprecatedWidthStyle_HTMLHRElement)//, JMSHTMLHRElementExtensions)//, JHTMLHRElementDOML2Deprecated)//, JDOML2DeprecatedAlignmentStyle_HTMLHRElement)//, JDOML2DeprecatedSizeProperty)
  end;

type
  JHTMLHRElement1 = class external "HTMLHRElement"
  public
    property prototype: JHTMLHRElement;
    function &new(): JHTMLHRElement;
  end;


function HTMLHRElement_var: JHTMLHRElement1;

type
  JMSHTMLFrameSetElementExtensions = class external "Object"
    property name: string;
    property frameBorder: string;
    property border: string;
    property frameSpacing: variant;
  end;

type
  JPositionOptions = class external "Object"
    property enableHighAccuracy: boolean;
    property timeout: integer;
    property maximumAge: integer;
  end;

type
  JHTMLObjectElement = class external "Object"(JHTMLElement)//, JMSHTMLObjectElementExtensions)//, JGetSVGDocument)//, JDOML2DeprecatedMarginStyle_HTMLObjectElement)//, JMSDataBindingExtensions)//, JMSDataBindingRecordSetExtensions)//, JDOML2DeprecatedAlignmentStyle_HTMLObjectElement)//, JDOML2DeprecatedBorderStyle_HTMLObjectElement)
    property width: string;
    property codeType: string;
    property archive: string;
    property standby: string;
    property name: string;
    property useMap: string;
    property form: JHTMLFormElement;
    property data: string;
    property height: string;
    property contentDocument: JDocument;
    property codeBase: string;
    property declare: boolean;
    property &type: string;
    property code: string;
  end;

type
  JHTMLObjectElement1 = class external "HTMLObjectElement"
  public
    property prototype: JHTMLObjectElement;
    function &new(): JHTMLObjectElement;
  end;


function HTMLObjectElement_var: JHTMLObjectElement1;

type
  JMSHTMLMenuElementExtensions = class external "Object"
  end;

type
  JDocumentView = class external "Object"
    property defaultView: JAbstractView;
    function elementFromPoint(x: integer; y: integer): JElement;
  end;

type
  JStorageEvent = class external "Object"(JEvent)
    property oldValue: variant;
    property newValue: variant;
    property url: string;
    property storageArea: JStorage;
    property key: string;
    procedure initStorageEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; keyArg: string; oldValueArg: variant; newValueArg: variant; urlArg: string; storageAreaArg: JStorage);
  end;

type
  JStorageEvent1 = class external "StorageEvent"
  public
    property prototype: JStorageEvent;
    function &new(): JStorageEvent;
  end;


function StorageEvent_var: JStorageEvent1;

type
  JHTMLEmbedElement = class external "Object"(JHTMLElement)//, JGetSVGDocument)//, JMSHTMLEmbedElementExtensions)
    property width: string;
    property src: string;
    property name: string;
    property height: string;
  end;

type
  JHTMLEmbedElement1 = class external "HTMLEmbedElement"
  public
    property prototype: JHTMLEmbedElement;
    function &new(): JHTMLEmbedElement;
  end;


function HTMLEmbedElement_var: JHTMLEmbedElement1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLTableSectionElement = class external "Object"
    property align: string;
  end;

type
  JHTMLOptGroupElement = class external "Object"(JHTMLElement)//, JMSDataBindingExtensions)//, JMSHTMLOptGroupElementExtensions)
    property label: string;
  end;

type
  JHTMLOptGroupElement1 = class external "HTMLOptGroupElement"
  public
    property prototype: JHTMLOptGroupElement;
    function &new(): JHTMLOptGroupElement;
  end;


function HTMLOptGroupElement_var: JHTMLOptGroupElement1;

type
  JHTMLIsIndexElement = class external "Object"(JHTMLElement)//, JMSHTMLIsIndexElementExtensions)
    property form: JHTMLFormElement;
    property prompt: string;
  end;

type
  JHTMLIsIndexElement1 = class external "HTMLIsIndexElement"
  public
    property prototype: JHTMLIsIndexElement;
    function &new(): JHTMLIsIndexElement;
  end;


function HTMLIsIndexElement_var: JHTMLIsIndexElement1;

type
  JSVGPathSegLinetoRel = class external "Object"(JSVGPathSeg)
    property y: integer;
    property x: integer;
  end;

type
  JSVGPathSegLinetoRel1 = class external "SVGPathSegLinetoRel"
  public
    property prototype: JSVGPathSegLinetoRel;
    function &new(): JSVGPathSegLinetoRel;
  end;


function SVGPathSegLinetoRel_var: JSVGPathSegLinetoRel1;

type
  JMSHTMLDocumentSelection = class external "Object"
    property selection: JMSSelection;
  end;

type
  JDOMException = class external "Object"
    property code: integer;
    property message: string;
    function toString(): string;
    property HIERARCHY_REQUEST_ERR: integer;
    property NO_MODIFICATION_ALLOWED_ERR: integer;
    property INVALID_MODIFICATION_ERR: integer;
    property NAMESPACE_ERR: integer;
    property INVALID_CHARACTER_ERR: integer;
    property TYPE_MISMATCH_ERR: integer;
    property ABORT_ERR: integer;
    property INVALID_STATE_ERR: integer;
    property SECURITY_ERR: integer;
    property NETWORK_ERR: integer;
    property WRONG_DOCUMENT_ERR: integer;
    property QUOTA_EXCEEDED_ERR: integer;
    property INDEX_SIZE_ERR: integer;
    property DOMSTRING_SIZE_ERR: integer;
    property SYNTAX_ERR: integer;
    property SERIALIZE_ERR: integer;
    property VALIDATION_ERR: integer;
    property NOT_FOUND_ERR: integer;
    property URL_MISMATCH_ERR: integer;
    property PARSE_ERR: integer;
    property NO_DATA_ALLOWED_ERR: integer;
    property NOT_SUPPORTED_ERR: integer;
    property INVALID_ACCESS_ERR: integer;
    property INUSE_ATTRIBUTE_ERR: integer;
  end;

type
  JDOMException1 = class external "DOMException"
  public
    property prototype: JDOMException;
    function &new(): JDOMException;
    property HIERARCHY_REQUEST_ERR: integer;
    property NO_MODIFICATION_ALLOWED_ERR: integer;
    property INVALID_MODIFICATION_ERR: integer;
    property NAMESPACE_ERR: integer;
    property INVALID_CHARACTER_ERR: integer;
    property TYPE_MISMATCH_ERR: integer;
    property ABORT_ERR: integer;
    property INVALID_STATE_ERR: integer;
    property SECURITY_ERR: integer;
    property NETWORK_ERR: integer;
    property WRONG_DOCUMENT_ERR: integer;
    property QUOTA_EXCEEDED_ERR: integer;
    property INDEX_SIZE_ERR: integer;
    property DOMSTRING_SIZE_ERR: integer;
    property SYNTAX_ERR: integer;
    property SERIALIZE_ERR: integer;
    property VALIDATION_ERR: integer;
    property NOT_FOUND_ERR: integer;
    property URL_MISMATCH_ERR: integer;
    property PARSE_ERR: integer;
    property NO_DATA_ALLOWED_ERR: integer;
    property NOT_SUPPORTED_ERR: integer;
    property INVALID_ACCESS_ERR: integer;
    property INUSE_ATTRIBUTE_ERR: integer;
  end;


function DOMException_var: JDOMException1;

type
  JMSCompatibleInfoCollection = class external "Object"
    property length: integer;
    function item(index: integer): JMSCompatibleInfo;
  end;

type
  JMSCompatibleInfoCollection1 = class external "MSCompatibleInfoCollection"
  public
    property prototype: JMSCompatibleInfoCollection;
    function &new(): JMSCompatibleInfoCollection;
  end;


function MSCompatibleInfoCollection_var: JMSCompatibleInfoCollection1;

type
  JMSHTMLIsIndexElementExtensions = class external "Object"
    property action: string;
  end;

type
  JSVGAnimatedBoolean = class external "Object"
    property animVal: boolean;
    property baseVal: boolean;
  end;

type
  JSVGAnimatedBoolean1 = class external "SVGAnimatedBoolean"
  public
    property prototype: JSVGAnimatedBoolean;
    function &new(): JSVGAnimatedBoolean;
  end;


function SVGAnimatedBoolean_var: JSVGAnimatedBoolean1;

type
  JSVGSwitchElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGTransformable)//, JSVGLangSpace)//, JSVGTests)
  end;

type
  JSVGSwitchElement1 = class external "SVGSwitchElement"
  public
    property prototype: JSVGSwitchElement;
    function &new(): JSVGSwitchElement;
  end;


function SVGSwitchElement_var: JSVGSwitchElement1;

type
  TMSHTMLIFrameElementExtensions_onload_ = function (ev: JEvent): variant;
type
  JMSHTMLIFrameElementExtensions = class external "Object"(JDOML2DeprecatedMarginStyle_MSHTMLIFrameElementExtensions)//, JDOML2DeprecatedBorderStyle_MSHTMLIFrameElementExtensions)
    property onload: TMSHTMLIFrameElementExtensions_onload_;
    property frameSpacing: variant;
    property noResize: boolean;
  end;

type
  JSVGPreserveAspectRatio = class external "Object"
    property align: integer;
    property meetOrSlice: integer;
    property SVG_PRESERVEASPECTRATIO_NONE: integer;
    property SVG_PRESERVEASPECTRATIO_XMINYMID: integer;
    property SVG_PRESERVEASPECTRATIO_XMAXYMIN: integer;
    property SVG_PRESERVEASPECTRATIO_XMINYMAX: integer;
    property SVG_PRESERVEASPECTRATIO_XMAXYMAX: integer;
    property SVG_MEETORSLICE_UNKNOWN: integer;
    property SVG_PRESERVEASPECTRATIO_XMAXYMID: integer;
    property SVG_PRESERVEASPECTRATIO_XMIDYMAX: integer;
    property SVG_PRESERVEASPECTRATIO_XMINYMIN: integer;
    property SVG_MEETORSLICE_MEET: integer;
    property SVG_PRESERVEASPECTRATIO_XMIDYMID: integer;
    property SVG_PRESERVEASPECTRATIO_XMIDYMIN: integer;
    property SVG_MEETORSLICE_SLICE: integer;
    property SVG_PRESERVEASPECTRATIO_UNKNOWN: integer;
  end;

type
  JSVGPreserveAspectRatio1 = class external "SVGPreserveAspectRatio"
  public
    property prototype: JSVGPreserveAspectRatio;
    function &new(): JSVGPreserveAspectRatio;
    property SVG_PRESERVEASPECTRATIO_NONE: integer;
    property SVG_PRESERVEASPECTRATIO_XMINYMID: integer;
    property SVG_PRESERVEASPECTRATIO_XMAXYMIN: integer;
    property SVG_PRESERVEASPECTRATIO_XMINYMAX: integer;
    property SVG_PRESERVEASPECTRATIO_XMAXYMAX: integer;
    property SVG_MEETORSLICE_UNKNOWN: integer;
    property SVG_PRESERVEASPECTRATIO_XMAXYMID: integer;
    property SVG_PRESERVEASPECTRATIO_XMIDYMAX: integer;
    property SVG_PRESERVEASPECTRATIO_XMINYMIN: integer;
    property SVG_MEETORSLICE_MEET: integer;
    property SVG_PRESERVEASPECTRATIO_XMIDYMID: integer;
    property SVG_PRESERVEASPECTRATIO_XMIDYMIN: integer;
    property SVG_MEETORSLICE_SLICE: integer;
    property SVG_PRESERVEASPECTRATIO_UNKNOWN: integer;
  end;


function SVGPreserveAspectRatio_var: JSVGPreserveAspectRatio1;

type
  JAttr = class external "Object"(JNode)//, JMSAttrExtensions)
    property specified: boolean;
    property ownerElement: JElement;
    property value: string;
    property name: string;
  end;

type
  JAttr1 = class external "Attr"
  public
    property prototype: JAttr;
    function &new(): JAttr;
  end;


function Attr_var: JAttr1;

type
  JMSBorderColorStyle_HTMLTableRowElement = class external "Object"
    property borderColor: variant;
  end;

type
  JDOML2DeprecatedAlignmentStyle_HTMLTableCaptionElement = class external "Object"
    property align: string;
  end;

type
  JPerformanceNavigation = class external "Object"
    property redirectCount: integer;
    property &type: integer;
    function toJSON(): variant;
    property TYPE_RELOAD: integer;
    property TYPE_RESERVED: integer;
    property TYPE_BACK_FORWARD: integer;
    property TYPE_NAVIGATE: integer;
  end;

type
  JPerformanceNavigation1 = class external "PerformanceNavigation"
  public
    property prototype: JPerformanceNavigation;
    function &new(): JPerformanceNavigation;
    property TYPE_RELOAD: integer;
    property TYPE_RESERVED: integer;
    property TYPE_BACK_FORWARD: integer;
    property TYPE_NAVIGATE: integer;
  end;


function PerformanceNavigation_var: JPerformanceNavigation1;

type
  JHTMLBodyElementDOML2Deprecated = class external "Object"
    property link: variant;
    property aLink: variant;
    property text: variant;
    property vLink: variant;
  end;

type
  JSVGStopElement = class external "Object"(JSVGElement)//, JSVGStylable)
    property offset: JSVGAnimatedNumber;
  end;

type
  JSVGStopElement1 = class external "SVGStopElement"
  public
    property prototype: JSVGStopElement;
    function &new(): JSVGStopElement;
  end;


function SVGStopElement_var: JSVGStopElement1;

type
  JPositionCallback = class external "Object"
  end;


procedure PositionCallback_(position: JPosition);external;

type
  JSVGSymbolElement = class external "Object"(JSVGElement)//, JSVGStylable)//, JSVGLangSpace)//, JSVGFitToViewBox)
  end;

type
  JSVGSymbolElement1 = class external "SVGSymbolElement"
  public
    property prototype: JSVGSymbolElement;
    function &new(): JSVGSymbolElement;
  end;


function SVGSymbolElement_var: JSVGSymbolElement1;

type
  JSVGElementInstanceList = class external "Object"
    property length: integer;
    function item(index: integer): JSVGElementInstance;
  end;

type
  JSVGElementInstanceList1 = class external "SVGElementInstanceList"
  public
    property prototype: JSVGElementInstanceList;
    function &new(): JSVGElementInstanceList;
  end;


function SVGElementInstanceList_var: JSVGElementInstanceList1;

type
  JMSDataBindingRecordSetExtensions = class external "Object"
    property recordset: JObject;
    function namedRecordset(dataMember: string; hierarchy: variant = undefined): JObject;
  end;

type
  JCSSRuleList = class external "Object"
    property length: integer;
    function item(index: integer): JCSSRule;
    function  GetItems(index: integer): JCSSRule; external array;
    procedure SetItems(index: integer; value: JCSSRule); external array;
    property Items[index: integer]: JCSSRule read GetItems write SetItems; default;
  end;

type
  JCSSRuleList1 = class external "CSSRuleList"
  public
    property prototype: JCSSRuleList;
    function &new(): JCSSRuleList;
  end;


function CSSRuleList_var: JCSSRuleList1;

type
  JMSHTMLTableColElementExtensions = class external "Object"
  end;

type
  JLinkStyle = class external "Object"
    property sheet: JStyleSheet;
  end;

type
  JMSHTMLMarqueeElementExtensions = class external "Object"
  end;

type
  JHTMLVideoElement = class external "Object"(JHTMLMediaElement)
    property width: integer;
    property videoWidth: integer;
    property videoHeight: integer;
    property height: integer;
    property poster: string;
  end;

type
  JHTMLVideoElement1 = class external "HTMLVideoElement"
  public
    property prototype: JHTMLVideoElement;
    function &new(): JHTMLVideoElement;
  end;


function HTMLVideoElement_var: JHTMLVideoElement1;

type
  TMSXMLHttpRequestExtensions_ontimeout_ = function (ev: JEvent): variant;
type
  JMSXMLHttpRequestExtensions = class external "Object"
    property responseBody: variant;
    property timeout: integer;
    property ontimeout: TMSXMLHttpRequestExtensions_ontimeout_;
  end;

type
  JClientRectList = class external "Object"
    property length: integer;
    function item(index: integer): JClientRect;
    function  GetItems(index: integer): JClientRect; external array;
    procedure SetItems(index: integer; value: JClientRect); external array;
    property Items[index: integer]: JClientRect read GetItems write SetItems; default;
  end;

type
  JClientRectList1 = class external "ClientRectList"
  public
    property prototype: JClientRectList;
    function &new(): JClientRectList;
  end;


function ClientRectList_var: JClientRectList1;

type
  JDOML2DeprecatedAlignmentStyle_HTMLTableCellElement = class external "Object"
    property align: string;
  end;

type
  JSVGMaskElement = class external "Object"(JSVGElement)//, JSVGUnitTypes)//, JSVGStylable)//, JSVGLangSpace)//, JSVGTests)
    property y: JSVGAnimatedLength;
    property width: JSVGAnimatedLength;
    property maskUnits: JSVGAnimatedEnumeration;
    property maskContentUnits: JSVGAnimatedEnumeration;
    property x: JSVGAnimatedLength;
    property height: JSVGAnimatedLength;
  end;

type
  JSVGMaskElement1 = class external "SVGMaskElement"
  public
    property prototype: JSVGMaskElement;
    function &new(): JSVGMaskElement;
  end;


function SVGMaskElement_var: JSVGMaskElement1;

type
  JAudio = class external "Audio"
  public
    function &new(src: string = ""): JHTMLAudioElement;
  end;


function Audio_var: JAudio;

type
  JOption = class external "Option"
  public
    function &new(text: string = ""; value: string = ""; defaultSelected: boolean = false; selected: boolean = false): JHTMLOptionElement;
  end;


function Option_var: JOption;

type
  JImage = class external "Image"
  public
    function &new(width: integer = 0; height: integer = 0): JHTMLImageElement;
  end;


function Image_var: JImage;

type
  Tondragend_ = function (ev: JDragEvent): variant;

function ondragend_var: Tondragend_;

type
  Tonkeydown_ = function (ev: JKeyboardEvent): variant;

function onkeydown_var: Tonkeydown_;

type
  Tondragover_ = function (ev: JDragEvent): variant;

function ondragover_var: Tondragover_;

type
  Tonkeyup_ = function (ev: JKeyboardEvent): variant;

function onkeyup_var: Tonkeyup_;

type
  Tonreset_ = function (ev: JEvent): variant;

function onreset_var: Tonreset_;

type
  Tonmouseup_ = function (ev: JMouseEvent): variant;

function onmouseup_var: Tonmouseup_;

type
  Tondragstart_ = function (ev: JDragEvent): variant;

function ondragstart_var: Tondragstart_;

type
  Tondrag_ = function (ev: JDragEvent): variant;

function ondrag_var: Tondrag_;

type
  Tonmouseover_ = function (ev: JMouseEvent): variant;

function onmouseover_var: Tonmouseover_;

type
  Tondragleave_ = function (ev: JDragEvent): variant;

function ondragleave_var: Tondragleave_;
function history_var: JHistory;
function name_var: string;

type
  Tonafterprint_ = function (ev: JEvent): variant;

function onafterprint_var: Tonafterprint_;

type
  Tonpause_ = function (ev: JEvent): variant;

function onpause_var: Tonpause_;

type
  Tonbeforeprint_ = function (ev: JEvent): variant;

function onbeforeprint_var: Tonbeforeprint_;
function top_var: JWindow;

type
  Tonmousedown_ = function (ev: JMouseEvent): variant;

function onmousedown_var: Tonmousedown_;

type
  Tonseeked_ = function (ev: JEvent): variant;

function onseeked_var: Tonseeked_;
function opener_var: JWindow;

type
  Tonclick_ = function (ev: JMouseEvent): variant;

function onclick_var: Tonclick_;

type
  Tonwaiting_ = function (ev: JEvent): variant;

function onwaiting_var: Tonwaiting_;

type
  Tononline_ = function (ev: JEvent): variant;

function ononline_var: Tononline_;

type
  Tondurationchange_ = function (ev: JEvent): variant;

function ondurationchange_var: Tondurationchange_;
function frames_var: JWindow;

type
  Tonblur_ = function (ev: JFocusEvent): variant;

function onblur_var: Tonblur_;

type
  Tonemptied_ = function (ev: JEvent): variant;

function onemptied_var: Tonemptied_;

type
  Tonseeking_ = function (ev: JEvent): variant;

function onseeking_var: Tonseeking_;

type
  Toncanplay_ = function (ev: JEvent): variant;

function oncanplay_var: Toncanplay_;

type
  Tonstalled_ = function (ev: JEvent): variant;

function onstalled_var: Tonstalled_;

type
  Tonmousemove_ = function (ev: JMouseEvent): variant;

function onmousemove_var: Tonmousemove_;

type
  Tonoffline_ = function (ev: JEvent): variant;

function onoffline_var: Tonoffline_;
function length_var: integer;

type
  Tonbeforeunload_ = function (ev: JBeforeUnloadEvent): variant;

function onbeforeunload_var: Tonbeforeunload_;

type
  Tonratechange_ = function (ev: JEvent): variant;

function onratechange_var: Tonratechange_;

type
  Tonstorage_ = function (ev: JStorageEvent): variant;

function onstorage_var: Tonstorage_;

type
  Tonloadstart_ = function (ev: JEvent): variant;

function onloadstart_var: Tonloadstart_;

type
  Tondragenter_ = function (ev: JDragEvent): variant;

function ondragenter_var: Tondragenter_;

type
  Tonsubmit_ = function (ev: JEvent): variant;

function onsubmit_var: Tonsubmit_;
function self_var: JWindow;

type
  Tonprogress_ = function (ev: variant): variant;

function onprogress_var: Tonprogress_;

type
  Tondblclick_ = function (ev: JMouseEvent): variant;

function ondblclick_var: Tondblclick_;

type
  Toncontextmenu_ = function (ev: JMouseEvent): variant;

function oncontextmenu_var: Toncontextmenu_;

type
  Tonchange_ = function (ev: JEvent): variant;

function onchange_var: Tonchange_;

type
  Tonloadedmetadata_ = function (ev: JEvent): variant;

function onloadedmetadata_var: Tonloadedmetadata_;

type
  Tonplay_ = function (ev: JEvent): variant;

function onplay_var: Tonplay_;
function onerror_var: JErrorFunction;

type
  Tonplaying_ = function (ev: JEvent): variant;

function onplaying_var: Tonplaying_;
function parent_var: JWindow;
function location_var: JLocation;

type
  Toncanplaythrough_ = function (ev: JEvent): variant;

function oncanplaythrough_var: Toncanplaythrough_;

type
  Tonabort_ = function (ev: JUIEvent): variant;

function onabort_var: Tonabort_;

type
  Tonreadystatechange_ = function (ev: JEvent): variant;

function onreadystatechange_var: Tonreadystatechange_;

type
  Tonkeypress_ = function (ev: JKeyboardEvent): variant;

function onkeypress_var: Tonkeypress_;
function frameElement_var: JElement;

type
  Tonloadeddata_ = function (ev: JEvent): variant;

function onloadeddata_var: Tonloadeddata_;

type
  Tonsuspend_ = function (ev: JEvent): variant;

function onsuspend_var: Tonsuspend_;
function window_var: JWindow;

type
  Tonfocus_ = function (ev: JFocusEvent): variant;

function onfocus_var: Tonfocus_;

type
  Tonmessage_ = function (ev: JMessageEvent): variant;

function onmessage_var: Tonmessage_;

type
  Tontimeupdate_ = function (ev: JEvent): variant;

function ontimeupdate_var: Tontimeupdate_;

type
  Tonresize_ = function (ev: JUIEvent): variant;

function onresize_var: Tonresize_;
function navigator_var: JNavigator;

type
  Tonselect_ = function (ev: JUIEvent): variant;

function onselect_var: Tonselect_;

type
  Tondrop_ = function (ev: JDragEvent): variant;

function ondrop_var: Tondrop_;

type
  Tonmouseout_ = function (ev: JMouseEvent): variant;

function onmouseout_var: Tonmouseout_;

type
  Tonended_ = function (ev: JEvent): variant;

function onended_var: Tonended_;

type
  Tonhashchange_ = function (ev: JEvent): variant;

function onhashchange_var: Tonhashchange_;

type
  Tonunload_ = function (ev: JEvent): variant;

function onunload_var: Tonunload_;

type
  Tonscroll_ = function (ev: JUIEvent): variant;

function onscroll_var: Tonscroll_;

type
  Tonmousewheel_ = function (ev: JMouseWheelEvent): variant;

function onmousewheel_var: Tonmousewheel_;

type
  Tonload_ = function (ev: JEvent): variant;

function onload_var: Tonload_;

type
  Tonvolumechange_ = function (ev: JEvent): variant;

function onvolumechange_var: Tonvolumechange_;

type
  Toninput_ = function (ev: JEvent): variant;

function oninput_var: Toninput_;
procedure alert(message: string = "");external;
procedure focus();external;
procedure print();external;
function prompt(message: string = ""; defaul: string = ""): string;external;
function toString(): string;external;
function open(url: string = ""; target: string = ""; features: string = ""; replace: boolean = false): JWindow;external;
procedure close();external;
function confirm(message: string = ""): boolean;external;
procedure postMessage(message: variant; targetOrigin: string; ports: variant = undefined);external;
function showModalDialog(url: string = ""; argument: variant = undefined; options: variant = undefined): variant;external;
procedure blur();external;
function getSelection(): JSelection;external;
function getComputedStyle(elt: JElement; pseudoElt: string = ""): JCSSStyleDeclaration;external;
function attachEvent(event: string; listener: JEventListener): boolean;external;
procedure detachEvent(event: string; listener: JEventListener);external;
function status_var: string;

type
  Tonmouseleave_ = function (ev: JMouseEvent): variant;

function onmouseleave_var: Tonmouseleave_;
function screenLeft_var: integer;
function offscreenBuffering_var: variant;
function maxConnectionsPerServer_var: integer;

type
  Tonmouseenter_ = function (ev: JMouseEvent): variant;

function onmouseenter_var: Tonmouseenter_;
function clipboardData_var: JDataTransfer;
function defaultStatus_var: string;
function clientInformation_var: JNavigator;
function closed_var: boolean;

type
  Tonhelp_ = function (ev: JEvent): variant;

function onhelp_var: Tonhelp_;
function external_var: JBrowserPublic;
function event_var: JMSEventObj;

type
  Tonfocusout_ = function (ev: JFocusEvent): variant;

function onfocusout_var: Tonfocusout_;
function screenTop_var: integer;

type
  Tonfocusin_ = function (ev: JFocusEvent): variant;

function onfocusin_var: Tonfocusin_;
function showModelessDialog(url: string = ""; argument: variant = undefined; options: variant = undefined): JWindow;external;
procedure navigate(url: string);external;
procedure resizeBy(x: integer = 0; y: integer = 0);external;
function item(index: variant): variant;external;
procedure resizeTo(x: integer = 0; y: integer = 0);external;
function createPopup(arguments: variant = undefined): JMSPopupWindow;external;
function toStaticHTML(html: string): string;external;
function execScript(code: string; language: string = ""): variant;external;
procedure msWriteProfilerMark(profilerMarkName: string);external;
procedure moveTo(x: integer = 0; y: integer = 0);external;
procedure moveBy(x: integer = 0; y: integer = 0);external;
procedure showHelp(url: string; helpArg: variant = undefined; features: string = "");external;
function performance_var: variant;
function outerWidth_var: integer;
function pageXOffset_var: integer;
function innerWidth_var: integer;
function pageYOffset_var: integer;
function screenY_var: integer;
function outerHeight_var: integer;
function screen_var: JScreen;
function innerHeight_var: integer;
function screenX_var: integer;
procedure scroll(x: integer = 0; y: integer = 0);external;
procedure scrollBy(x: integer = 0; y: integer = 0);external;
procedure scrollTo(x: integer = 0; y: integer = 0);external;
function styleMedia_var: JStyleMedia;
function document_var: JDocument;
procedure removeEventListener(&type: string; listener: JEventListener; useCapture: boolean = false);external;
procedure addEventListener(&type: string; listener: JEventListener; useCapture: boolean = false);external;
function dispatchEvent(evt: JEvent): boolean;external;
function localStorage_var: JStorage;
function sessionStorage_var: JStorage;
procedure clearTimeout(handle: integer);external;
function setTimeout(expression: variant; msec: integer = 0; language: variant = undefined): integer;external;
procedure clearInterval(handle: integer);external;
function setInterval(expression: variant; msec: integer = 0; language: variant = undefined): integer;external;

type
  THTMLBodyElement_onpopstate_ = function (ev: JPopStateEvent): variant;
type
  JHTMLBodyElement2 = class external "Object"
    property onpopstate: THTMLBodyElement_onpopstate_;
  end;

type
  JMSGestureEvent = class external "Object"(JUIEvent)
    property offsetY: integer;
    property translationY: integer;
    property velocityExpansion: integer;
    property velocityY: integer;
    property velocityAngular: integer;
    property translationX: integer;
    property velocityX: integer;
    property hwTimestamp: integer;
    property offsetX: integer;
    property screenX: integer;
    property rotation: integer;
    property expansion: integer;
    property clientY: integer;
    property screenY: integer;
    property scale: integer;
    property gestureObject: variant;
    property clientX: integer;
    procedure initGestureEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; detailArg: integer; screenXArg: integer; screenYArg: integer; clientXArg: integer; clientYArg: integer; offsetXArg: integer; offsetYArg: integer; translationXArg: integer; translationYArg: integer; scaleArg: integer; expansionArg: integer; rotationArg: integer; velocityXArg: integer; velocityYArg: integer; velocityExpansionArg: integer; velocityAngularArg: integer; hwTimestampArg: integer);
    property MSGESTURE_FLAG_BEGIN: integer;
    property MSGESTURE_FLAG_END: integer;
    property MSGESTURE_FLAG_CANCEL: integer;
    property MSGESTURE_FLAG_INERTIA: integer;
    property MSGESTURE_FLAG_NONE: integer;
  end;

type
  JMSGestureEvent1 = class external "MSGestureEvent"
  public
    property prototype: JMSGestureEvent;
    function &new(): JMSGestureEvent;
    property MSGESTURE_FLAG_BEGIN: integer;
    property MSGESTURE_FLAG_END: integer;
    property MSGESTURE_FLAG_CANCEL: integer;
    property MSGESTURE_FLAG_INERTIA: integer;
    property MSGESTURE_FLAG_NONE: integer;
  end;


function MSGestureEvent_var: JMSGestureEvent1;

type
  JHTMLAnchorElement2 = class external "Object"
    property text: string;
  end;

type
  JHTMLInputElement2 = class external "Object"
    property validationMessage: string;
    property files: JFileList;
    property max: string;
    property formTarget: string;
    property willValidate: boolean;
    property step: string;
    property autofocus: boolean;
    property required: boolean;
    property formEnctype: string;
    property valueAsNumber: integer;
    property placeholder: string;
    property formMethod: string;
    property list: JHTMLElement;
    property autocomplete: string;
    property min: string;
    property formAction: string;
    property pattern: string;
    property validity: JValidityState;
    property formNoValidate: string;
    property multiple: boolean;
    function checkValidity(): boolean;
    procedure stepDown(n: integer = 0);
    procedure stepUp(n: integer = 0);
    procedure setCustomValidity(error: string);
  end;

type
  JErrorEvent = class external "Object"(JEvent)
    property colno: integer;
    property filename: string;
    property lineno: integer;
    property message: string;
    procedure initErrorEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; messageArg: string; filenameArg: string; linenoArg: integer);
  end;

type
  JErrorEvent1 = class external "ErrorEvent"
  public
    property prototype: JErrorEvent;
    function &new(): JErrorEvent;
  end;


function ErrorEvent_var: JErrorEvent1;

type
  JSVGFilterElement = class external "Object"(JSVGElement)//, JSVGUnitTypes)//, JSVGStylable)//, JSVGLangSpace)//, JSVGURIReference)
    property y: JSVGAnimatedLength;
    property width: JSVGAnimatedLength;
    property filterResX: JSVGAnimatedInteger;
    property filterUnits: JSVGAnimatedEnumeration;
    property primitiveUnits: JSVGAnimatedEnumeration;
    property x: JSVGAnimatedLength;
    property height: JSVGAnimatedLength;
    property filterResY: JSVGAnimatedInteger;
    procedure setFilterRes(filterResX: integer; filterResY: integer);
  end;

type
  JSVGFilterElement1 = class external "SVGFilterElement"
  public
    property prototype: JSVGFilterElement;
    function &new(): JSVGFilterElement;
  end;


function SVGFilterElement_var: JSVGFilterElement1;

type
  JTrackEvent = class external "Object"(JEvent)
    property track: variant;
  end;

type
  JTrackEvent1 = class external "TrackEvent"
  public
    property prototype: JTrackEvent;
    function &new(): JTrackEvent;
  end;


function TrackEvent_var: JTrackEvent1;

type
  JSVGFEMergeNodeElement = class external "Object"(JSVGElement)
    property in1: JSVGAnimatedString;
  end;

type
  JSVGFEMergeNodeElement1 = class external "SVGFEMergeNodeElement"
  public
    property prototype: JSVGFEMergeNodeElement;
    function &new(): JSVGFEMergeNodeElement;
  end;


function SVGFEMergeNodeElement_var: JSVGFEMergeNodeElement1;

type
  JSVGFEFloodElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
  end;

type
  JSVGFEFloodElement1 = class external "SVGFEFloodElement"
  public
    property prototype: JSVGFEFloodElement;
    function &new(): JSVGFEFloodElement;
  end;


function SVGFEFloodElement_var: JSVGFEFloodElement1;

type
  TMSElementExtensions_onmspointerdown_ = function (ev: variant): variant;
  TMSElementExtensions_onmsgotpointercapture_ = function (ev: variant): variant;
  TMSElementExtensions_onmsgesturedoubletap_ = function (ev: variant): variant;
  TMSElementExtensions_onmspointerhover_ = function (ev: variant): variant;
  TMSElementExtensions_onmsgesturehold_ = function (ev: variant): variant;
  TMSElementExtensions_onmspointermove_ = function (ev: variant): variant;
  TMSElementExtensions_onmsgesturechange_ = function (ev: variant): variant;
  TMSElementExtensions_onmsgesturestart_ = function (ev: variant): variant;
  TMSElementExtensions_onmspointercancel_ = function (ev: variant): variant;
  TMSElementExtensions_onmsgestureend_ = function (ev: variant): variant;
  TMSElementExtensions_onmsgesturetap_ = function (ev: variant): variant;
  TMSElementExtensions_onmspointerout_ = function (ev: variant): variant;
  TMSElementExtensions_onmsinertiastart_ = function (ev: variant): variant;
  TMSElementExtensions_onmslostpointercapture_ = function (ev: variant): variant;
  TMSElementExtensions_onmspointerover_ = function (ev: variant): variant;
  TMSElementExtensions_onmspointerup_ = function (ev: variant): variant;
type
  JMSElementExtensions1 = class external "Object"
    property msRegionOverflow: string;
    property onmspointerdown: TMSElementExtensions_onmspointerdown_;
    property onmsgotpointercapture: TMSElementExtensions_onmsgotpointercapture_;
    property onmsgesturedoubletap: TMSElementExtensions_onmsgesturedoubletap_;
    property onmspointerhover: TMSElementExtensions_onmspointerhover_;
    property onmsgesturehold: TMSElementExtensions_onmsgesturehold_;
    property onmspointermove: TMSElementExtensions_onmspointermove_;
    property onmsgesturechange: TMSElementExtensions_onmsgesturechange_;
    property onmsgesturestart: TMSElementExtensions_onmsgesturestart_;
    property onmspointercancel: TMSElementExtensions_onmspointercancel_;
    property onmsgestureend: TMSElementExtensions_onmsgestureend_;
    property onmsgesturetap: TMSElementExtensions_onmsgesturetap_;
    property onmspointerout: TMSElementExtensions_onmspointerout_;
    property onmsinertiastart: TMSElementExtensions_onmsinertiastart_;
    property onmslostpointercapture: TMSElementExtensions_onmslostpointercapture_;
    property onmspointerover: TMSElementExtensions_onmspointerover_;
    property msContentZoomFactor: integer;
    property onmspointerup: TMSElementExtensions_onmspointerup_;
    function msGetRegionContent(): JMSRangeCollection;
    procedure msReleasePointerCapture(pointerId: integer);
    procedure msSetPointerCapture(pointerId: integer);
  end;

type
  JMSElementExtensions2 = class external "MSElementExtensions"
  public
    property prototype: JMSElementExtensions;
    function &new(): JMSElementExtensions;
  end;


function MSElementExtensions_var: JMSElementExtensions2;

type
  JMSCSSScrollTranslationProperties = class external "Object"
    property msScrollTranslation: string;
  end;

type
  JMSGesture = class external "Object"
    property target: JElement;
    procedure addPointer(pointerId: integer);
    procedure stop();
  end;

type
  JMSGesture1 = class external "MSGesture"
  public
    property prototype: JMSGesture;
    function &new(): JMSGesture;
  end;


function MSGesture_var: JMSGesture1;

type
  TTextTrackCue_onenter_ = function (ev: JEvent): variant;
  TTextTrackCue_onexit_ = function (ev: JEvent): variant;
type
  JTextTrackCue = class external "Object"(JEventTarget)
    property onenter: TTextTrackCue_onenter_;
    property track: JTextTrack;
    property endTime: integer;
    property text: string;
    property pauseOnExit: boolean;
    property id: string;
    property startTime: integer;
    property onexit: TTextTrackCue_onexit_;
    function getCueAsHTML(): JDocumentFragment;
  end;

type
  JTextTrackCue1 = class external "TextTrackCue"
  public
    property prototype: JTextTrackCue;
    function &new(): JTextTrackCue;
  end;


function TextTrackCue_var: JTextTrackCue1;

type
  JMSHTMLDocumentViewExtensions1 = class external "Object"
    property msCSSOMElementFloatMetrics: boolean;
    function msElementsFromPoint(x: integer; y: integer): JNodeList;
    function msElementsFromRect(left: integer; top: integer; width: integer; height: integer): JNodeList;
  end;

type
  JMSHTMLDocumentViewExtensions2 = class external "MSHTMLDocumentViewExtensions"
  public
    property prototype: JMSHTMLDocumentViewExtensions;
    function &new(): JMSHTMLDocumentViewExtensions;
  end;


function MSHTMLDocumentViewExtensions_var: JMSHTMLDocumentViewExtensions2;

type
  JMSStreamReader = class external "Object"(JMSBaseReader)
    property error: JDOMError;
    procedure readAsArrayBuffer(stream: JMSStream; size: integer = 0);
    procedure readAsBlob(stream: JMSStream; size: integer = 0);
    procedure readAsDataURL(stream: JMSStream; size: integer = 0);
    procedure readAsText(stream: JMSStream; encoding: string = ""; size: integer = 0);
  end;

type
  JMSStreamReader1 = class external "MSStreamReader"
  public
    property prototype: JMSStreamReader;
    function &new(): JMSStreamReader;
  end;


function MSStreamReader_var: JMSStreamReader1;

type
  JCSSFlexibleBoxProperties = class external "Object"
    property msFlex: string;
    property msFlexDirection: string;
    property msFlexNegative: string;
    property msFlexPack: string;
    property msFlexWrap: string;
    property msFlexItemAlign: string;
    property msFlexOrder: string;
    property msFlexPositive: string;
    property msFlexAlign: string;
    property msFlexFlow: string;
    property msFlexPreferredSize: string;
    property msFlexLinePack: string;
  end;

type
  JDOMTokenList = class external "Object"
    property length: integer;
    function contains(token: string): boolean;
    procedure remove(token: string);
    function toggle(token: string): boolean;
    procedure add(token: string);
    function item(index: integer): string;
    function  GetItems(index: integer): string; external array;
    procedure SetItems(index: integer; value: string); external array;
    property Items[index: integer]: string read GetItems write SetItems; default;
    function toString(): string;
  end;

type
  JDOMTokenList1 = class external "DOMTokenList"
  public
    property prototype: JDOMTokenList;
    function &new(): JDOMTokenList;
  end;


function DOMTokenList_var: JDOMTokenList1;

type
  JEventException2 = class external "Object"
    property name: string;
  end;

type
  JSVGFEFuncAElement = class external "Object"(JSVGComponentTransferFunctionElement)
  end;

type
  JSVGFEFuncAElement1 = class external "SVGFEFuncAElement"
  public
    property prototype: JSVGFEFuncAElement;
    function &new(): JSVGFEFuncAElement;
  end;


function SVGFEFuncAElement_var: JSVGFEFuncAElement1;

type
  JPerformance2 = class external "Object"
    function now(): integer;
  end;

type
  JSVGFETileElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property in1: JSVGAnimatedString;
  end;

type
  JSVGFETileElement1 = class external "SVGFETileElement"
  public
    property prototype: JSVGFETileElement;
    function &new(): JSVGFETileElement;
  end;


function SVGFETileElement_var: JSVGFETileElement1;

type
  JSVGFEBlendElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property in2: JSVGAnimatedString;
    property mode: JSVGAnimatedEnumeration;
    property in1: JSVGAnimatedString;
    property SVG_FEBLEND_MODE_DARKEN: integer;
    property SVG_FEBLEND_MODE_UNKNOWN: integer;
    property SVG_FEBLEND_MODE_MULTIPLY: integer;
    property SVG_FEBLEND_MODE_NORMAL: integer;
    property SVG_FEBLEND_MODE_SCREEN: integer;
    property SVG_FEBLEND_MODE_LIGHTEN: integer;
  end;

type
  JSVGFEBlendElement1 = class external "SVGFEBlendElement"
  public
    property prototype: JSVGFEBlendElement;
    function &new(): JSVGFEBlendElement;
    property SVG_FEBLEND_MODE_DARKEN: integer;
    property SVG_FEBLEND_MODE_UNKNOWN: integer;
    property SVG_FEBLEND_MODE_MULTIPLY: integer;
    property SVG_FEBLEND_MODE_NORMAL: integer;
    property SVG_FEBLEND_MODE_SCREEN: integer;
    property SVG_FEBLEND_MODE_LIGHTEN: integer;
  end;


function SVGFEBlendElement_var: JSVGFEBlendElement1;

type
  JWindowTimers1 = class external "Object"(JWindowTimersExtension)
  end;

type
  JWindowTimers2 = class external "WindowTimers"
  public
    property prototype: JWindowTimers;
    function &new(): JWindowTimers;
  end;


function WindowTimers_var: JWindowTimers2;

type
  JCSSStyleDeclaration2 = class external "Object"(JCSS2DTransformsProperties)//, JCSSTransitionsProperties)//, JCSSFontsProperties)//, JMSCSSHighContrastProperties)//, JCSSGridProperties)//, JCSSAnimationsProperties)//, JMSCSSContentZoomProperties)//, JMSCSSScrollTranslationProperties)//, JMSCSSTouchManipulationProperties)//, JCSSFlexibleBoxProperties)//, JMSCSSPositionedFloatsProperties)//, JMSCSSRegionProperties)//, JMSCSSSelectionBoundaryProperties)//, JCSSMultiColumnProperties)//, JCSSTextProperties)//, JCSS3DTransformsProperties)
  end;

type
  JMessageChannel = class external "Object"
    property port2: JMessagePort;
    property port1: JMessagePort;
  end;

type
  JMessageChannel1 = class external "MessageChannel"
  public
    property prototype: JMessageChannel;
    function &new(): JMessageChannel;
  end;


function MessageChannel_var: JMessageChannel1;

type
  JSVGFEMergeElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
  end;

type
  JSVGFEMergeElement1 = class external "SVGFEMergeElement"
  public
    property prototype: JSVGFEMergeElement;
    function &new(): JSVGFEMergeElement;
  end;


function SVGFEMergeElement_var: JSVGFEMergeElement1;

type
  JNavigator2 = class external "Object"(JMSFileSaver)
  end;

type
  JTransitionEvent = class external "Object"(JEvent)
    property propertyName: string;
    property elapsedTime: integer;
    procedure initTransitionEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; propertyNameArg: string; elapsedTimeArg: integer);
  end;

type
  JTransitionEvent1 = class external "TransitionEvent"
  public
    property prototype: JTransitionEvent;
    function &new(): JTransitionEvent;
  end;


function TransitionEvent_var: JTransitionEvent1;

type
  JMediaQueryList = class external "Object"
    property matches: boolean;
    property media: string;
    procedure addListener(listener: JMediaQueryListListener);
    procedure removeListener(listener: JMediaQueryListListener);
  end;

type
  JMediaQueryList1 = class external "MediaQueryList"
  public
    property prototype: JMediaQueryList;
    function &new(): JMediaQueryList;
  end;


function MediaQueryList_var: JMediaQueryList1;

type
  JDOMError = class external "Object"
    property name: string;
    function toString(): string;
  end;

type
  JDOMError1 = class external "DOMError"
  public
    property prototype: JDOMError;
    function &new(): JDOMError;
  end;


function DOMError_var: JDOMError1;

type
  JSVGFEPointLightElement = class external "Object"(JSVGElement)
    property y: JSVGAnimatedNumber;
    property x: JSVGAnimatedNumber;
    property z: JSVGAnimatedNumber;
  end;

type
  JSVGFEPointLightElement1 = class external "SVGFEPointLightElement"
  public
    property prototype: JSVGFEPointLightElement;
    function &new(): JSVGFEPointLightElement;
  end;


function SVGFEPointLightElement_var: JSVGFEPointLightElement1;

type
  JCSSFontsProperties = class external "Object"
    property msFontFeatureSettings: string;
    property fontFeatureSettings: string;
  end;

type
  JCloseEvent = class external "Object"(JEvent)
    property wasClean: boolean;
    property reason: string;
    property code: integer;
    procedure initCloseEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; wasCleanArg: boolean; codeArg: integer; reasonArg: string);
  end;

type
  JCloseEvent1 = class external "CloseEvent"
  public
    property prototype: JCloseEvent;
    function &new(): JCloseEvent;
  end;


function CloseEvent_var: JCloseEvent1;

type
  TWebSocket_onopen_ = function (ev: JEvent): variant;
  TWebSocket_onmessage_ = function (ev: variant): variant;
  TWebSocket_onclose_ = function (ev: JCloseEvent): variant;
  TWebSocket_onerror_ = function (ev: JErrorEvent): variant;
type
  JWebSocket = class external "Object"(JEventTarget)
    property protocol: string;
    property readyState: integer;
    property bufferedAmount: integer;
    property onopen: TWebSocket_onopen_;
    property extensions: string;
    property onmessage: TWebSocket_onmessage_;
    property onclose: TWebSocket_onclose_;
    property onerror: TWebSocket_onerror_;
    property binaryType: string;
    property url: string;
    procedure close(code: integer = 0; reason: string = "");
    procedure send(data: variant);
    property OPEN: integer;
    property CLOSING: integer;
    property CONNECTING: integer;
    property CLOSED: integer;
  end;

type
  JWebSocket1 = class external "WebSocket"
  public
    property prototype: JWebSocket;
    function &new(url: string): JWebSocket;
    function &new(url: string; prototcol: string): JWebSocket;overload;
    function &new(url: string; prototcol: array of string): JWebSocket;overload;
    property OPEN: integer;
    property CLOSING: integer;
    property CONNECTING: integer;
    property CLOSED: integer;
  end;


function WebSocket_var: JWebSocket1;

type
  JProgressEvent = class external "Object"(JEvent)
    property loaded: integer;
    property lengthComputable: boolean;
    property total: integer;
    procedure initProgressEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; lengthComputableArg: boolean; loadedArg: integer; totalArg: integer);
  end;

type
  JProgressEvent1 = class external "ProgressEvent"
  public
    property prototype: JProgressEvent;
    function &new(): JProgressEvent;
  end;


function ProgressEvent_var: JProgressEvent1;

type
  JHTMLCanvasElement2 = class external "Object"
    function msToBlob(): JBlob;
  end;

type
  JIDBObjectStore = class external "Object"
    property indexNames: JDOMStringList;
    property name: string;
    property transaction: JIDBTransaction;
    property keyPath: string;
    function count(key: variant = undefined): JIDBRequest;
    function add(value: variant; key: variant = undefined): JIDBRequest;
    function clear(): JIDBRequest;
    function createIndex(name: string; keyPath: string; optionalParameters: variant = undefined): JIDBIndex;
    function put(value: variant; key: variant = undefined): JIDBRequest;
    function openCursor(range: variant = undefined; direction: string = ""): JIDBRequest;
    procedure deleteIndex(indexName: string);
    function index(name: string): JIDBIndex;
    function get(key: variant): JIDBRequest;
    function delet(key: variant): JIDBRequest;
  end;

type
  JIDBObjectStore1 = class external "IDBObjectStore"
  public
    property prototype: JIDBObjectStore;
    function &new(): JIDBObjectStore;
  end;


function IDBObjectStore_var: JIDBObjectStore1;

type
  JObjectURLOptions = class external "Object"
    property oneTimeOnly: boolean;
  end;

type
  JSVGFEGaussianBlurElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property stdDeviationX: JSVGAnimatedNumber;
    property in1: JSVGAnimatedString;
    property stdDeviationY: JSVGAnimatedNumber;
    procedure setStdDeviation(stdDeviationX: integer; stdDeviationY: integer);
  end;

type
  JSVGFEGaussianBlurElement1 = class external "SVGFEGaussianBlurElement"
  public
    property prototype: JSVGFEGaussianBlurElement;
    function &new(): JSVGFEGaussianBlurElement;
  end;


function SVGFEGaussianBlurElement_var: JSVGFEGaussianBlurElement1;

type
  TMSHTMLDocumentExtensions_onmspointerdown_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmspointercancel_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmsgesturedoubletap_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmsgesturetap_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmsgestureend_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmspointerout_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmsmanipulationstatechanged_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmsinertiastart_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmspointerhover_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmscontentzoom_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmsgesturehold_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmspointermove_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmspointerover_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmsgesturechange_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmsgesturestart_ = function (ev: variant): variant;
  TMSHTMLDocumentExtensions_onmspointerup_ = function (ev: variant): variant;
type
  JMSHTMLDocumentExtensions1 = class external "Object"
    property onmspointerdown: TMSHTMLDocumentExtensions_onmspointerdown_;
    property onmspointercancel: TMSHTMLDocumentExtensions_onmspointercancel_;
    property onmsgesturedoubletap: TMSHTMLDocumentExtensions_onmsgesturedoubletap_;
    property onmsgesturetap: TMSHTMLDocumentExtensions_onmsgesturetap_;
    property onmsgestureend: TMSHTMLDocumentExtensions_onmsgestureend_;
    property onmspointerout: TMSHTMLDocumentExtensions_onmspointerout_;
    property onmsmanipulationstatechanged: TMSHTMLDocumentExtensions_onmsmanipulationstatechanged_;
    property onmsinertiastart: TMSHTMLDocumentExtensions_onmsinertiastart_;
    property onmspointerhover: TMSHTMLDocumentExtensions_onmspointerhover_;
    property onmscontentzoom: TMSHTMLDocumentExtensions_onmscontentzoom_;
    property onmsgesturehold: TMSHTMLDocumentExtensions_onmsgesturehold_;
    property onmspointermove: TMSHTMLDocumentExtensions_onmspointermove_;
    property onmspointerover: TMSHTMLDocumentExtensions_onmspointerover_;
    property onmsgesturechange: TMSHTMLDocumentExtensions_onmsgesturechange_;
    property onmsgesturestart: TMSHTMLDocumentExtensions_onmsgesturestart_;
    property onmspointerup: TMSHTMLDocumentExtensions_onmspointerup_;
  end;

type
  JMSHTMLDocumentExtensions2 = class external "MSHTMLDocumentExtensions"
  public
    property prototype: JMSHTMLDocumentExtensions;
    function &new(): JMSHTMLDocumentExtensions;
  end;


function MSHTMLDocumentExtensions_var: JMSHTMLDocumentExtensions2;

type
  JMSCSSSelectionBoundaryProperties = class external "Object"
    property msUserSelect: string;
  end;

type
  JSVGFilterPrimitiveStandardAttributes = class external "Object"(JSVGStylable)
    property y: JSVGAnimatedLength;
    property width: JSVGAnimatedLength;
    property x: JSVGAnimatedLength;
    property height: JSVGAnimatedLength;
    property result: JSVGAnimatedString;
  end;

type
  JIDBVersionChangeEvent = class external "Object"(JEvent)
    property newVersion: integer;
    property oldVersion: integer;
  end;

type
  JIDBVersionChangeEvent1 = class external "IDBVersionChangeEvent"
  public
    property prototype: JIDBVersionChangeEvent;
    function &new(): JIDBVersionChangeEvent;
  end;


function IDBVersionChangeEvent_var: JIDBVersionChangeEvent1;

type
  JIDBIndex = class external "Object"
    property unique: boolean;
    property name: string;
    property keyPath: string;
    property objectStore: JIDBObjectStore;
    function count(key: variant = undefined): JIDBRequest;
    function getKey(key: variant): JIDBRequest;
    function openKeyCursor(range: JIDBKeyRange = nil; direction: string = ""): JIDBRequest;
    function get(key: variant): JIDBRequest;
    function openCursor(range: JIDBKeyRange = nil; direction: string = ""): JIDBRequest;
  end;

type
  JIDBIndex1 = class external "IDBIndex"
  public
    property prototype: JIDBIndex;
    function &new(): JIDBIndex;
  end;


function IDBIndex_var: JIDBIndex1;

type
  JFileList = class external "Object"
    property length: integer;
    function item(index: integer): JFile;
    function  GetItems(index: integer): JFile; external array;
    procedure SetItems(index: integer; value: JFile); external array;
    property Items[index: integer]: JFile read GetItems write SetItems; default;
  end;

type
  JFileList1 = class external "FileList"
  public
    property prototype: JFileList;
    function &new(): JFileList;
  end;


function FileList_var: JFileList1;

type
  JIDBCursor = class external "Object"
    property source: variant;
    property direction: string;
    property key: variant;
    property primaryKey: variant;
    procedure advance(count: integer);
    function delet(): JIDBRequest;
    procedure continu(key: variant = undefined);
    function update(value: variant): JIDBRequest;
  end;

type
  JIDBCursor1 = class external "IDBCursor"
  public
    property prototype: JIDBCursor;
    function &new(): JIDBCursor;
  end;


function IDBCursor_var: JIDBCursor1;

type
  JCSSAnimationsProperties = class external "Object"
    property animationFillMode: string;
    property msAnimationDirection: string;
    property msAnimationDelay: string;
    property msAnimationFillMode: string;
    property animationIterationCount: string;
    property msAnimationPlayState: string;
    property msAnimationIterationCount: string;
    property animationDelay: string;
    property animationTimingFunction: string;
    property msAnimation: string;
    property animation: string;
    property animationDirection: string;
    property animationDuration: string;
    property animationName: string;
    property animationPlayState: string;
    property msAnimationTimingFunction: string;
    property msAnimationName: string;
    property msAnimationDuration: string;
  end;

type
  JSVGFESpecularLightingElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property kernelUnitLengthY: JSVGAnimatedNumber;
    property surfaceScale: JSVGAnimatedNumber;
    property specularExponent: JSVGAnimatedNumber;
    property in1: JSVGAnimatedString;
    property kernelUnitLengthX: JSVGAnimatedNumber;
    property specularConstant: JSVGAnimatedNumber;
  end;

type
  JSVGFESpecularLightingElement1 = class external "SVGFESpecularLightingElement"
  public
    property prototype: JSVGFESpecularLightingElement;
    function &new(): JSVGFESpecularLightingElement;
  end;


function SVGFESpecularLightingElement_var: JSVGFESpecularLightingElement1;

type
  JFile = class external "Object"(JBlob)
    property lastModifiedDate: variant;
    property name: string;
  end;

type
  JFile1 = class external "File"
  public
    property prototype: JFile;
    function &new(): JFile;
  end;


function File_var: JFile1;

type
  JURL = class external "Object"
    procedure revokeObjectURL(url: string);
    function createObjectURL(object_: variant; options: JObjectURLOptions = nil): string;
  end;


function URL_var: JURL;

type
  JRangeException2 = class external "Object"
    property name: string;
  end;

type
  JIDBCursorWithValue = class external "Object"(JIDBCursor)
    property value: variant;
  end;

type
  JIDBCursorWithValue1 = class external "IDBCursorWithValue"
  public
    property prototype: JIDBCursorWithValue;
    function &new(): JIDBCursorWithValue;
  end;


function IDBCursorWithValue_var: JIDBCursorWithValue1;

type
  JHTMLTextAreaElement2 = class external "Object"
    property validationMessage: string;
    property autofocus: boolean;
    property validity: JValidityState;
    property required: boolean;
    property maxLength: integer;
    property willValidate: boolean;
    property placeholder: string;
    function checkValidity(): boolean;
    procedure setCustomValidity(error: string);
  end;

type
  TXMLHttpRequestEventTarget_onprogress_ = function (ev: JProgressEvent): variant;
  TXMLHttpRequestEventTarget_onerror_ = function (ev: JErrorEvent): variant;
  TXMLHttpRequestEventTarget_onload_ = function (ev: variant): variant;
  TXMLHttpRequestEventTarget_ontimeout_ = function (ev: variant): variant;
  TXMLHttpRequestEventTarget_onabort_ = function (ev: variant): variant;
  TXMLHttpRequestEventTarget_onloadstart_ = function (ev: variant): variant;
  TXMLHttpRequestEventTarget_onloadend_ = function (ev: JProgressEvent): variant;
type
  JXMLHttpRequestEventTarget = class external "Object"(JEventTarget)
    property onprogress: TXMLHttpRequestEventTarget_onprogress_;
    property onerror: TXMLHttpRequestEventTarget_onerror_;
    property onload: TXMLHttpRequestEventTarget_onload_;
    property ontimeout: TXMLHttpRequestEventTarget_ontimeout_;
    property onabort: TXMLHttpRequestEventTarget_onabort_;
    property onloadstart: TXMLHttpRequestEventTarget_onloadstart_;
    property onloadend: TXMLHttpRequestEventTarget_onloadend_;
  end;

type
  JXMLHttpRequestEventTarget1 = class external "XMLHttpRequestEventTarget"
  public
    property prototype: JXMLHttpRequestEventTarget;
    function &new(): JXMLHttpRequestEventTarget;
  end;


function XMLHttpRequestEventTarget_var: JXMLHttpRequestEventTarget1;

type
  JIDBEnvironment = class external "Object"
    property msIndexedDB: JIDBFactory;
    property indexedDB: JIDBFactory;
  end;

type
  TAudioTrackList_onchange_ = function (ev: variant): variant;
  TAudioTrackList_onaddtrack_ = function (ev: JTrackEvent): variant;
type
  JAudioTrackList = class external "Object"(JEventTarget)
    property length: integer;
    property onchange: TAudioTrackList_onchange_;
    property onaddtrack: TAudioTrackList_onaddtrack_;
    function getTrackById(id: string): JAudioTrack;
    function item(index: integer): JAudioTrack;
    function  GetItems(index: integer): JAudioTrack; external array;
    procedure SetItems(index: integer; value: JAudioTrack); external array;
    property Items[index: integer]: JAudioTrack read GetItems write SetItems; default;
  end;

type
  JAudioTrackList1 = class external "AudioTrackList"
  public
    property prototype: JAudioTrackList;
    function &new(): JAudioTrackList;
  end;


function AudioTrackList_var: JAudioTrackList1;

type
  TMSBaseReader_onprogress_ = function (ev: JProgressEvent): variant;
  TMSBaseReader_onabort_ = function (ev: variant): variant;
  TMSBaseReader_onloadend_ = function (ev: JProgressEvent): variant;
  TMSBaseReader_onerror_ = function (ev: JErrorEvent): variant;
  TMSBaseReader_onload_ = function (ev: variant): variant;
  TMSBaseReader_onloadstart_ = function (ev: variant): variant;
type
  JMSBaseReader = class external "Object"(JEventTarget)
    property onprogress: TMSBaseReader_onprogress_;
    property readyState: integer;
    property onabort: TMSBaseReader_onabort_;
    property onloadend: TMSBaseReader_onloadend_;
    property onerror: TMSBaseReader_onerror_;
    property onload: TMSBaseReader_onload_;
    property onloadstart: TMSBaseReader_onloadstart_;
    property result: variant;
    procedure abort();
    property LOADING: integer;
    property EMPTY: integer;
    property DONE: integer;
  end;

type
  JHistory2 = class external "Object"
    property state: variant;
    procedure replaceState(statedata: variant; title: string; url: string = "");
    procedure pushState(statedata: variant; title: string; url: string = "");
  end;

type
  JMSProtocol = class external "Object"
    property protocol: string;
  end;

type
  JMSProtocol1 = class external "MSProtocol"
  public
    property prototype: JMSProtocol;
    function &new(): JMSProtocol;
  end;


function MSProtocol_var: JMSProtocol1;

type
  JSVGFEMorphologyElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property operator: JSVGAnimatedEnumeration;
    property radiusX: JSVGAnimatedNumber;
    property radiusY: JSVGAnimatedNumber;
    property in1: JSVGAnimatedString;
    property SVG_MORPHOLOGY_OPERATOR_UNKNOWN: integer;
    property SVG_MORPHOLOGY_OPERATOR_ERODE: integer;
    property SVG_MORPHOLOGY_OPERATOR_DILATE: integer;
  end;

type
  JSVGFEMorphologyElement1 = class external "SVGFEMorphologyElement"
  public
    property prototype: JSVGFEMorphologyElement;
    function &new(): JSVGFEMorphologyElement;
    property SVG_MORPHOLOGY_OPERATOR_UNKNOWN: integer;
    property SVG_MORPHOLOGY_OPERATOR_ERODE: integer;
    property SVG_MORPHOLOGY_OPERATOR_DILATE: integer;
  end;


function SVGFEMorphologyElement_var: JSVGFEMorphologyElement1;

type
  JHTMLSelectElement2 = class external "Object"
    property validationMessage: string;
    property autofocus: boolean;
    property validity: JValidityState;
    property required: boolean;
    property willValidate: boolean;
    function checkValidity(): boolean;
    procedure setCustomValidity(error: string);
  end;

type
  JCSSTransitionsProperties = class external "Object"
    property transition: string;
    property transitionDelay: string;
    property transitionDuration: string;
    property msTransitionTimingFunction: string;
    property msTransition: string;
    property msTransitionDuration: string;
    property transitionTimingFunction: string;
    property msTransitionDelay: string;
    property transitionProperty: string;
    property msTransitionProperty: string;
  end;

type
  JSVGFEFuncRElement = class external "Object"(JSVGComponentTransferFunctionElement)
  end;

type
  JSVGFEFuncRElement1 = class external "SVGFEFuncRElement"
  public
    property prototype: JSVGFEFuncRElement;
    function &new(): JSVGFEFuncRElement;
  end;


function SVGFEFuncRElement_var: JSVGFEFuncRElement1;

type
  JCSSRule2 = class external "Object"
    property KEYFRAMES_RULE: integer;
    property KEYFRAME_RULE: integer;
    property VIEWPORT_RULE: integer;
  end;

type
  JWindowTimersExtension = class external "Object"
    function msSetImmediate(expression: variant; {many?}args: array of variant): integer;
    procedure clearImmediate(handle: integer);
    procedure msClearImmediate(handle: integer);
    function setImmediate(expression: variant; {many?}args: array of variant): integer;
  end;

type
  JSVGFEDisplacementMapElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property in2: JSVGAnimatedString;
    property xChannelSelector: JSVGAnimatedEnumeration;
    property yChannelSelector: JSVGAnimatedEnumeration;
    property scale: JSVGAnimatedNumber;
    property in1: JSVGAnimatedString;
    property SVG_CHANNEL_B: integer;
    property SVG_CHANNEL_R: integer;
    property SVG_CHANNEL_G: integer;
    property SVG_CHANNEL_UNKNOWN: integer;
    property SVG_CHANNEL_A: integer;
  end;

type
  JSVGFEDisplacementMapElement1 = class external "SVGFEDisplacementMapElement"
  public
    property prototype: JSVGFEDisplacementMapElement;
    function &new(): JSVGFEDisplacementMapElement;
    property SVG_CHANNEL_B: integer;
    property SVG_CHANNEL_R: integer;
    property SVG_CHANNEL_G: integer;
    property SVG_CHANNEL_UNKNOWN: integer;
    property SVG_CHANNEL_A: integer;
  end;


function SVGFEDisplacementMapElement_var: JSVGFEDisplacementMapElement1;

type
  JMSCSSContentZoomProperties = class external "Object"
    property msContentZoomLimit: string;
    property msContentZooming: string;
    property msContentZoomSnapType: string;
    property msContentZoomLimitMax: variant;
    property msContentZoomSnapPoints: string;
    property msContentZoomSnap: string;
    property msContentZoomLimitMin: variant;
    property msContentZoomChaining: string;
  end;

type
  JAnimationEvent = class external "Object"(JEvent)
    property animationName: string;
    property elapsedTime: integer;
    procedure initAnimationEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; animationNameArg: string; elapsedTimeArg: integer);
  end;

type
  JAnimationEvent1 = class external "AnimationEvent"
  public
    property prototype: JAnimationEvent;
    function &new(): JAnimationEvent;
  end;


function AnimationEvent_var: JAnimationEvent1;

type
  JSVGComponentTransferFunctionElement = class external "Object"(JSVGElement)
    property tableValues: JSVGAnimatedNumberList;
    property slope: JSVGAnimatedNumber;
    property &type: JSVGAnimatedEnumeration;
    property exponent: JSVGAnimatedNumber;
    property amplitude: JSVGAnimatedNumber;
    property intercept: JSVGAnimatedNumber;
    property offset: JSVGAnimatedNumber;
    property SVG_FECOMPONENTTRANSFER_TYPE_UNKNOWN: integer;
    property SVG_FECOMPONENTTRANSFER_TYPE_TABLE: integer;
    property SVG_FECOMPONENTTRANSFER_TYPE_IDENTITY: integer;
    property SVG_FECOMPONENTTRANSFER_TYPE_GAMMA: integer;
    property SVG_FECOMPONENTTRANSFER_TYPE_DISCRETE: integer;
    property SVG_FECOMPONENTTRANSFER_TYPE_LINEAR: integer;
  end;

type
  JSVGComponentTransferFunctionElement1 = class external "SVGComponentTransferFunctionElement"
  public
    property prototype: JSVGComponentTransferFunctionElement;
    function &new(): JSVGComponentTransferFunctionElement;
    property SVG_FECOMPONENTTRANSFER_TYPE_UNKNOWN: integer;
    property SVG_FECOMPONENTTRANSFER_TYPE_TABLE: integer;
    property SVG_FECOMPONENTTRANSFER_TYPE_IDENTITY: integer;
    property SVG_FECOMPONENTTRANSFER_TYPE_GAMMA: integer;
    property SVG_FECOMPONENTTRANSFER_TYPE_DISCRETE: integer;
    property SVG_FECOMPONENTTRANSFER_TYPE_LINEAR: integer;
  end;


function SVGComponentTransferFunctionElement_var: JSVGComponentTransferFunctionElement1;

type
  JMSRangeCollection = class external "Object"
    property length: integer;
    function item(index: integer): JRange;
    function  GetItems(index: integer): JRange; external array;
    procedure SetItems(index: integer; value: JRange); external array;
    property Items[index: integer]: JRange read GetItems write SetItems; default;
  end;

type
  JMSRangeCollection1 = class external "MSRangeCollection"
  public
    property prototype: JMSRangeCollection;
    function &new(): JMSRangeCollection;
  end;


function MSRangeCollection_var: JMSRangeCollection1;

type
  TMSHTMLElementExtensions_onmscontentzoom_ = function (ev: variant): variant;
  TMSHTMLElementExtensions_onmsmanipulationstatechanged_ = function (ev: variant): variant;
type
  JMSHTMLElementExtensions1 = class external "Object"
    property onmscontentzoom: TMSHTMLElementExtensions_onmscontentzoom_;
    property onmsmanipulationstatechanged: TMSHTMLElementExtensions_onmsmanipulationstatechanged_;
  end;

type
  JMSHTMLElementExtensions2 = class external "MSHTMLElementExtensions"
  public
    property prototype: JMSHTMLElementExtensions;
    function &new(): JMSHTMLElementExtensions;
  end;


function MSHTMLElementExtensions_var: JMSHTMLElementExtensions2;

type
  JMSCSSPositionedFloatsProperties = class external "Object"
    property msWrapMargin: variant;
    property msWrapFlow: string;
  end;

type
  JSVGException2 = class external "Object"
    property name: string;
  end;

type
  JSVGFEDistantLightElement = class external "Object"(JSVGElement)
    property azimuth: JSVGAnimatedNumber;
    property elevation: JSVGAnimatedNumber;
  end;

type
  JSVGFEDistantLightElement1 = class external "SVGFEDistantLightElement"
  public
    property prototype: JSVGFEDistantLightElement;
    function &new(): JSVGFEDistantLightElement;
  end;


function SVGFEDistantLightElement_var: JSVGFEDistantLightElement1;

type
  JMSCSSRegionProperties = class external "Object"
    property msFlowFrom: string;
    property msFlowInto: string;
    property msWrapThrough: string;
  end;

type
  JSVGFEFuncBElement = class external "Object"(JSVGComponentTransferFunctionElement)
  end;

type
  JSVGFEFuncBElement1 = class external "SVGFEFuncBElement"
  public
    property prototype: JSVGFEFuncBElement;
    function &new(): JSVGFEFuncBElement;
  end;


function SVGFEFuncBElement_var: JSVGFEFuncBElement1;

type
  JIDBKeyRange = class external "Object"
    property upper: variant;
    property upperOpen: boolean;
    property lower: variant;
    property lowerOpen: boolean;
    function bound(lower: variant; upper: variant; lowerOpen: boolean = false; upperOpen: boolean = false): JIDBKeyRange;
    function only(value: variant): JIDBKeyRange;
    function lowerBound(bound: variant; open: boolean = false): JIDBKeyRange;
    function upperBound(bound: variant; open: boolean = false): JIDBKeyRange;
  end;

type
  JIDBKeyRange1 = class external "IDBKeyRange"
  public
    property prototype: JIDBKeyRange;
    function &new(): JIDBKeyRange;
  end;


function IDBKeyRange_var: JIDBKeyRange1;

type
  JWindowConsole = class external "Object"
    property console: JConsole;
  end;

type
  JSVG1_1Properties1 = class external "Object"
    property floodOpacity: string;
    property floodColor: string;
    property filter: string;
    property lightingColor: string;
    property enableBackground: string;
    property colorInterpolationFilters: string;
  end;

type
  JSVG1_1Properties2 = class external "SVG1_1Properties"
  public
    property prototype: JSVG1_1Properties;
    function &new(): JSVG1_1Properties;
  end;


function SVG1_1Properties_var: JSVG1_1Properties2;

type
  TIDBTransaction_oncomplete_ = function (ev: JEvent): variant;
  TIDBTransaction_onerror_ = function (ev: JErrorEvent): variant;
  TIDBTransaction_onabort_ = function (ev: variant): variant;
type
  JIDBTransaction = class external "Object"(JEventTarget)
    property oncomplete: TIDBTransaction_oncomplete_;
    property db: JIDBDatabase;
    property mode: string;
    property error: JDOMError;
    property onerror: TIDBTransaction_onerror_;
    property onabort: TIDBTransaction_onabort_;
    procedure abort();
    function objectStore(name: string): JIDBObjectStore;
  end;

type
  JIDBTransaction1 = class external "IDBTransaction"
  public
    property prototype: JIDBTransaction;
    function &new(): JIDBTransaction;
  end;


function IDBTransaction_var: JIDBTransaction1;

type
  TMSWindowExtensions_onmspointerdown_ = function (ev: variant): variant;
  TMSWindowExtensions_onmspointercancel_ = function (ev: variant): variant;
  TMSWindowExtensions_onmsgesturedoubletap_ = function (ev: variant): variant;
  TMSWindowExtensions_onmsgestureend_ = function (ev: variant): variant;
  TMSWindowExtensions_onmsgesturetap_ = function (ev: variant): variant;
  TMSWindowExtensions_onmspointerout_ = function (ev: variant): variant;
  TMSWindowExtensions_onmspointerhover_ = function (ev: variant): variant;
  TMSWindowExtensions_onmsinertiastart_ = function (ev: variant): variant;
  TMSWindowExtensions_onmspointermove_ = function (ev: variant): variant;
  TMSWindowExtensions_onmsgesturehold_ = function (ev: variant): variant;
  TMSWindowExtensions_onmspointerover_ = function (ev: variant): variant;
  TMSWindowExtensions_onmsgesturechange_ = function (ev: variant): variant;
  TMSWindowExtensions_onmsgesturestart_ = function (ev: variant): variant;
  TMSWindowExtensions_onmspointerup_ = function (ev: variant): variant;
type
  JMSWindowExtensions1 = class external "Object"
    property onmspointerdown: TMSWindowExtensions_onmspointerdown_;
    property onmspointercancel: TMSWindowExtensions_onmspointercancel_;
    property onmsgesturedoubletap: TMSWindowExtensions_onmsgesturedoubletap_;
    property onmsgestureend: TMSWindowExtensions_onmsgestureend_;
    property onmsgesturetap: TMSWindowExtensions_onmsgesturetap_;
    property onmspointerout: TMSWindowExtensions_onmspointerout_;
    property onmspointerhover: TMSWindowExtensions_onmspointerhover_;
    property onmsinertiastart: TMSWindowExtensions_onmsinertiastart_;
    property onmspointermove: TMSWindowExtensions_onmspointermove_;
    property onmsgesturehold: TMSWindowExtensions_onmsgesturehold_;
    property onmspointerover: TMSWindowExtensions_onmspointerover_;
    property onmsgesturechange: TMSWindowExtensions_onmsgesturechange_;
    property onmsgesturestart: TMSWindowExtensions_onmsgesturestart_;
    property onmspointerup: TMSWindowExtensions_onmspointerup_;
    function msIsStaticHTML(html: string): boolean;
  end;

type
  JMSWindowExtensions2 = class external "MSWindowExtensions"
  public
    property prototype: JMSWindowExtensions;
    function &new(): JMSWindowExtensions;
  end;


function MSWindowExtensions_var: JMSWindowExtensions2;

type
  JAudioTrack = class external "Object"
    property kind: string;
    property language: string;
    property id: string;
    property label: string;
    property enabled: boolean;
  end;

type
  JAudioTrack1 = class external "AudioTrack"
  public
    property prototype: JAudioTrack;
    function &new(): JAudioTrack;
  end;


function AudioTrack_var: JAudioTrack1;

type
  JSVGFEConvolveMatrixElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property orderY: JSVGAnimatedInteger;
    property kernelUnitLengthY: JSVGAnimatedNumber;
    property orderX: JSVGAnimatedInteger;
    property preserveAlpha: JSVGAnimatedBoolean;
    property kernelMatrix: JSVGAnimatedNumberList;
    property edgeMode: JSVGAnimatedEnumeration;
    property kernelUnitLengthX: JSVGAnimatedNumber;
    property bias: JSVGAnimatedNumber;
    property targetX: JSVGAnimatedInteger;
    property targetY: JSVGAnimatedInteger;
    property divisor: JSVGAnimatedNumber;
    property in1: JSVGAnimatedString;
    property SVG_EDGEMODE_WRAP: integer;
    property SVG_EDGEMODE_DUPLICATE: integer;
    property SVG_EDGEMODE_UNKNOWN: integer;
    property SVG_EDGEMODE_NONE: integer;
  end;

type
  JSVGFEConvolveMatrixElement1 = class external "SVGFEConvolveMatrixElement"
  public
    property prototype: JSVGFEConvolveMatrixElement;
    function &new(): JSVGFEConvolveMatrixElement;
    property SVG_EDGEMODE_WRAP: integer;
    property SVG_EDGEMODE_DUPLICATE: integer;
    property SVG_EDGEMODE_UNKNOWN: integer;
    property SVG_EDGEMODE_NONE: integer;
  end;


function SVGFEConvolveMatrixElement_var: JSVGFEConvolveMatrixElement1;

type
  JTextTrackCueList = class external "Object"
    property length: integer;
    function item(index: integer): JTextTrackCue;
    function  GetItems(index: integer): JTextTrackCue; external array;
    procedure SetItems(index: integer; value: JTextTrackCue); external array;
    property Items[index: integer]: JTextTrackCue read GetItems write SetItems; default;
    function getCueById(id: string): JTextTrackCue;
  end;

type
  JTextTrackCueList1 = class external "TextTrackCueList"
  public
    property prototype: JTextTrackCueList;
    function &new(): JTextTrackCueList;
  end;


function TextTrackCueList_var: JTextTrackCueList1;

type
  JCSSKeyframesRule = class external "Object"(JCSSRule)
    property name: string;
    property cssRules: JCSSRuleList;
    function findRule(rule: string): JCSSKeyframeRule;
    procedure deleteRule(rule: string);
    procedure appendRule(rule: string);
  end;

type
  JCSSKeyframesRule1 = class external "CSSKeyframesRule"
  public
    property prototype: JCSSKeyframesRule;
    function &new(): JCSSKeyframesRule;
  end;


function CSSKeyframesRule_var: JCSSKeyframesRule1;

type
  JMSCSSTouchManipulationProperties = class external "Object"
    property msScrollSnapPointsY: string;
    property msOverflowStyle: string;
    property msScrollLimitXMax: variant;
    property msScrollSnapType: string;
    property msScrollSnapPointsX: string;
    property msScrollLimitYMax: variant;
    property msScrollSnapY: string;
    property msScrollLimitXMin: variant;
    property msScrollLimitYMin: variant;
    property msScrollChaining: string;
    property msTouchAction: string;
    property msScrollSnapX: string;
    property msScrollLimit: string;
    property msScrollRails: string;
    property msTouchSelect: string;
  end;

type
  TWindow_onpopstate_ = function (ev: JPopStateEvent): variant;
type
  JWindow2 = class external "Object"(JWindowAnimationTiming)//, JWindowBase64)//, JIDBEnvironment)//, JWindowConsole)
    property onpopstate: TWindow_onpopstate_;
    property applicationCache: JApplicationCache;
    function matchMedia(mediaQuery: string): JMediaQueryList;
    function msMatchMedia(mediaQuery: string): JMediaQueryList;
  end;

type
  JSVGFETurbulenceElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property baseFrequencyX: JSVGAnimatedNumber;
    property numOctaves: JSVGAnimatedInteger;
    property &type: JSVGAnimatedEnumeration;
    property baseFrequencyY: JSVGAnimatedNumber;
    property stitchTiles: JSVGAnimatedEnumeration;
    property seed: JSVGAnimatedNumber;
    property SVG_STITCHTYPE_UNKNOWN: integer;
    property SVG_STITCHTYPE_NOSTITCH: integer;
    property SVG_TURBULENCE_TYPE_UNKNOWN: integer;
    property SVG_TURBULENCE_TYPE_TURBULENCE: integer;
    property SVG_TURBULENCE_TYPE_FRACTALNOISE: integer;
    property SVG_STITCHTYPE_STITCH: integer;
  end;

type
  JSVGFETurbulenceElement1 = class external "SVGFETurbulenceElement"
  public
    property prototype: JSVGFETurbulenceElement;
    function &new(): JSVGFETurbulenceElement;
    property SVG_STITCHTYPE_UNKNOWN: integer;
    property SVG_STITCHTYPE_NOSTITCH: integer;
    property SVG_TURBULENCE_TYPE_UNKNOWN: integer;
    property SVG_TURBULENCE_TYPE_TURBULENCE: integer;
    property SVG_TURBULENCE_TYPE_FRACTALNOISE: integer;
    property SVG_STITCHTYPE_STITCH: integer;
  end;


function SVGFETurbulenceElement_var: JSVGFETurbulenceElement1;

type
  JTextTrackList = class external "Object"
    property length: integer;
    function item(index: integer): JTextTrack;
    function  GetItems(index: integer): JTextTrack; external array;
    procedure SetItems(index: integer; value: JTextTrack); external array;
    property Items[index: integer]: JTextTrack read GetItems write SetItems; default;
  end;

type
  JTextTrackList1 = class external "TextTrackList"
  public
    property prototype: JTextTrackList;
    function &new(): JTextTrackList;
  end;


function TextTrackList_var: JTextTrackList1;

type
  JWindowAnimationTiming = class external "Object"
    property animationStartTime: integer;
    property msAnimationStartTime: integer;
    procedure msCancelRequestAnimationFrame(handle: integer);
    procedure cancelAnimationFrame(handle: integer);
    function requestAnimationFrame(callback: JFrameRequestCallback): integer;
    function msRequestAnimationFrame(callback: JFrameRequestCallback): integer;
  end;

type
  JSVGFEFuncGElement = class external "Object"(JSVGComponentTransferFunctionElement)
  end;

type
  JSVGFEFuncGElement1 = class external "SVGFEFuncGElement"
  public
    property prototype: JSVGFEFuncGElement;
    function &new(): JSVGFEFuncGElement;
  end;


function SVGFEFuncGElement_var: JSVGFEFuncGElement1;

type
  JSVGFEColorMatrixElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property in1: JSVGAnimatedString;
    property &type: JSVGAnimatedEnumeration;
    property values: JSVGAnimatedNumberList;
    property SVG_FECOLORMATRIX_TYPE_SATURATE: integer;
    property SVG_FECOLORMATRIX_TYPE_UNKNOWN: integer;
    property SVG_FECOLORMATRIX_TYPE_MATRIX: integer;
    property SVG_FECOLORMATRIX_TYPE_HUEROTATE: integer;
    property SVG_FECOLORMATRIX_TYPE_LUMINANCETOALPHA: integer;
  end;

type
  JSVGFEColorMatrixElement1 = class external "SVGFEColorMatrixElement"
  public
    property prototype: JSVGFEColorMatrixElement;
    function &new(): JSVGFEColorMatrixElement;
    property SVG_FECOLORMATRIX_TYPE_SATURATE: integer;
    property SVG_FECOLORMATRIX_TYPE_UNKNOWN: integer;
    property SVG_FECOLORMATRIX_TYPE_MATRIX: integer;
    property SVG_FECOLORMATRIX_TYPE_HUEROTATE: integer;
    property SVG_FECOLORMATRIX_TYPE_LUMINANCETOALPHA: integer;
  end;


function SVGFEColorMatrixElement_var: JSVGFEColorMatrixElement1;

type
  JConsole = class external "Object"
    procedure info();
    procedure info(message: variant; {many?}optionalParams: array of variant);overload;
    function profile(reportName: string = ""): boolean;
    procedure assert();
    procedure assert(test: boolean);overload;
    procedure assert(test: boolean; message: variant; {many?}optionalParams: array of variant);overload;
    function msIsIndependentlyComposed(element: JElement): boolean;
    function clear(): boolean;
    function dir(): boolean;
    function dir(value: variant; {many?}optionalParams: array of variant): boolean;overload;
    procedure warn();
    procedure warn(message: variant; {many?}optionalParams: array of variant);overload;
    procedure error();
    procedure error(message: variant; {many?}optionalParams: array of variant);overload;
    procedure log();
    procedure log(message: variant; {many?}optionalParams: array of variant);overload;
    function profileEnd(): boolean;
  end;

type
  JConsole1 = class external "Console"
  public
    property prototype: JConsole;
    function &new(): JConsole;
  end;


function Console_var: JConsole1;

type
  JSVGFESpotLightElement = class external "Object"(JSVGElement)
    property pointsAtY: JSVGAnimatedNumber;
    property y: JSVGAnimatedNumber;
    property limitingConeAngle: JSVGAnimatedNumber;
    property specularExponent: JSVGAnimatedNumber;
    property x: JSVGAnimatedNumber;
    property pointsAtZ: JSVGAnimatedNumber;
    property z: JSVGAnimatedNumber;
    property pointsAtX: JSVGAnimatedNumber;
  end;

type
  JSVGFESpotLightElement1 = class external "SVGFESpotLightElement"
  public
    property prototype: JSVGFESpotLightElement;
    function &new(): JSVGFESpotLightElement;
  end;


function SVGFESpotLightElement_var: JSVGFESpotLightElement1;

type
  JDocumentVisibility = class external "Object"
    property msHidden: boolean;
    property msVisibilityState: string;
    property visibilityState: string;
    property hidden: boolean;
  end;

type
  JWindowBase64 = class external "Object"
    function btoa(rawString: string): string;
    function atob(encodedString: string): string;
  end;

type
  TIDBDatabase_onerror_ = function (ev: JErrorEvent): variant;
  TIDBDatabase_onabort_ = function (ev: variant): variant;
type
  JIDBDatabase = class external "Object"(JEventTarget)
    property version: string;
    property name: string;
    property objectStoreNames: JDOMStringList;
    property onerror: TIDBDatabase_onerror_;
    property onabort: TIDBDatabase_onabort_;
    function createObjectStore(name: string; optionalParameters: variant = undefined): JIDBObjectStore;
    procedure close();
    function transaction(storeNames: variant; mode: string = ""): JIDBTransaction;
    procedure deleteObjectStore(name: string);
  end;

type
  JIDBDatabase1 = class external "IDBDatabase"
  public
    property prototype: JIDBDatabase;
    function &new(): JIDBDatabase;
  end;


function IDBDatabase_var: JIDBDatabase1;

type
  JMSProtocolsCollection = class external "Object"
  end;

type
  JMSProtocolsCollection1 = class external "MSProtocolsCollection"
  public
    property prototype: JMSProtocolsCollection;
    function &new(): JMSProtocolsCollection;
  end;


function MSProtocolsCollection_var: JMSProtocolsCollection1;

type
  JDOMStringList = class external "Object"
    property length: integer;
    function contains(str: string): boolean;
    function item(index: integer): string;
    function  GetItems(index: integer): string; external array;
    procedure SetItems(index: integer; value: string); external array;
    property Items[index: integer]: string read GetItems write SetItems; default;
  end;

type
  JDOMStringList1 = class external "DOMStringList"
  public
    property prototype: JDOMStringList;
    function &new(): JDOMStringList;
  end;


function DOMStringList_var: JDOMStringList1;

type
  JCSSMultiColumnProperties = class external "Object"
    property breakAfter: string;
    property columnSpan: string;
    property columnRule: string;
    property columnFill: string;
    property columnRuleStyle: string;
    property breakBefore: string;
    property columnCount: variant;
    property breakInside: string;
    property columnWidth: variant;
    property columns: string;
    property columnRuleColor: variant;
    property columnGap: variant;
    property columnRuleWidth: variant;
  end;

type
  TIDBOpenDBRequest_onupgradeneeded_ = function (ev: JIDBVersionChangeEvent): variant;
  TIDBOpenDBRequest_onblocked_ = function (ev: JEvent): variant;
type
  JIDBOpenDBRequest = class external "Object"(JIDBRequest)
    property onupgradeneeded: TIDBOpenDBRequest_onupgradeneeded_;
    property onblocked: TIDBOpenDBRequest_onblocked_;
  end;

type
  JIDBOpenDBRequest1 = class external "IDBOpenDBRequest"
  public
    property prototype: JIDBOpenDBRequest;
    function &new(): JIDBOpenDBRequest;
  end;


function IDBOpenDBRequest_var: JIDBOpenDBRequest1;

type
  JHTMLButtonElement2 = class external "Object"
    property validationMessage: string;
    property formTarget: string;
    property willValidate: boolean;
    property formAction: string;
    property autofocus: boolean;
    property validity: JValidityState;
    property formNoValidate: string;
    property formEnctype: string;
    property formMethod: string;
    function checkValidity(): boolean;
    procedure setCustomValidity(error: string);
  end;

type
  JHTMLProgressElement = class external "Object"(JHTMLElement)
    property value: integer;
    property max: integer;
    property position: integer;
    property form: JHTMLFormElement;
  end;

type
  JHTMLProgressElement1 = class external "HTMLProgressElement"
  public
    property prototype: JHTMLProgressElement;
    function &new(): JHTMLProgressElement;
  end;


function HTMLProgressElement_var: JHTMLProgressElement1;

type
  JSVGFEOffsetElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property dy: JSVGAnimatedNumber;
    property in1: JSVGAnimatedString;
    property dx: JSVGAnimatedNumber;
  end;

type
  JSVGFEOffsetElement1 = class external "SVGFEOffsetElement"
  public
    property prototype: JSVGFEOffsetElement;
    function &new(): JSVGFEOffsetElement;
  end;


function SVGFEOffsetElement_var: JSVGFEOffsetElement1;

type
  JHTMLFormElement2 = class external "Object"
    property autocomplete: string;
    property noValidate: boolean;
    function checkValidity(): boolean;
  end;

type
  JMSUnsafeFunctionCallback = class external "Object"
  end;


function MSUnsafeFunctionCallback_(): variant;external;

type
  JDocument2 = class external "Object"(JDocumentVisibility)
  end;

type
  JMessageEvent2 = class external "Object"(JEvent)
    property ports: variant;
  end;

type
  JHTMLScriptElement2 = class external "Object"
    property async: boolean;
  end;

type
  JHTMLMediaElement2 = class external "Object"(JMSHTMLMediaElementExtensions)
    property textTracks: JTextTrackList;
    property audioTracks: JAudioTrackList;
  end;

type
  TTextTrack_oncuechange_ = function (ev: JEvent): variant;
  TTextTrack_onload_ = function (ev: variant): variant;
  TTextTrack_onerror_ = function (ev: JErrorEvent): variant;
type
  JTextTrack = class external "Object"(JEventTarget)
    property language: string;
    property mode: integer;
    property readyState: string;
    property activeCues: JTextTrackCueList;
    property cues: JTextTrackCueList;
    property oncuechange: TTextTrack_oncuechange_;
    property kind: string;
    property onload: TTextTrack_onload_;
    property onerror: TTextTrack_onerror_;
    property label: string;
    property ERROR: integer;
    property SHOWING: integer;
    property LOADING: integer;
    property LOADED: integer;
    property NONE: integer;
    property HIDDEN: integer;
    property DISABLED: integer;
  end;

type
  JTextTrack1 = class external "TextTrack"
  public
    property prototype: JTextTrack;
    function &new(): JTextTrack;
    property ERROR: integer;
    property SHOWING: integer;
    property LOADING: integer;
    property LOADED: integer;
    property NONE: integer;
    property HIDDEN: integer;
    property DISABLED: integer;
  end;


function TextTrack_var: JTextTrack1;

type
  JMediaQueryListListener = class external "Object"
  end;


procedure MediaQueryListListener_(mql: JMediaQueryList);external;

type
  TIDBRequest_onsuccess_ = function (ev: JEvent): variant;
  TIDBRequest_onerror_ = function (ev: JErrorEvent): variant;
type
  JIDBRequest = class external "Object"(JEventTarget)
    property source: variant;
    property onsuccess: TIDBRequest_onsuccess_;
    property error: JDOMError;
    property transaction: JIDBTransaction;
    property onerror: TIDBRequest_onerror_;
    property readyState: string;
    property result: variant;
  end;

type
  JIDBRequest1 = class external "IDBRequest"
  public
    property prototype: JIDBRequest;
    function &new(): JIDBRequest;
  end;


function IDBRequest_var: JIDBRequest1;

type
  TMessagePort_onmessage_ = function (ev: variant): variant;
type
  JMessagePort = class external "Object"(JEventTarget)
    property onmessage: TMessagePort_onmessage_;
    procedure close();
    procedure postMessage(message: variant; ports: variant = undefined);
    procedure start();
  end;

type
  JMessagePort1 = class external "MessagePort"
  public
    property prototype: JMessagePort;
    function &new(): JMessagePort;
  end;


function MessagePort_var: JMessagePort1;

type
  JFileReader = class external "Object"(JMSBaseReader)
    property error: JDOMError;
    procedure readAsArrayBuffer(blob: JBlob);
    procedure readAsDataURL(blob: JBlob);
    procedure readAsText(blob: JBlob; encoding: string = "");
  end;

type
  JFileReader1 = class external "FileReader"
  public
    property prototype: JFileReader;
    function &new(): JFileReader;
  end;


function FileReader_var: JFileReader1;

type
  JBlob = class external "Object"
    property &type: string;
    property size: integer;
    function msDetachStream(): variant;
    function slice(start: integer = 0; &end: integer = 0; contentType: string = ""): JBlob;
    procedure close();
    procedure msClose();
  end;

type
  JBlobPropertyBag = class external "Object"
    property &type: string;
    property endings: string;
  end;

type
  JBlob1 = class external "Blob"
  public
    property prototype: JBlob;
    function &new(blobParts: array of variant = []; options: JBlobPropertyBag = nil): JBlob;
  end;


function Blob_var: JBlob1;

type
  TApplicationCache_ondownloading_ = function (ev: JEvent): variant;
  TApplicationCache_onprogress_ = function (ev: JProgressEvent): variant;
  TApplicationCache_onupdateready_ = function (ev: JEvent): variant;
  TApplicationCache_oncached_ = function (ev: JEvent): variant;
  TApplicationCache_onobsolete_ = function (ev: JEvent): variant;
  TApplicationCache_onerror_ = function (ev: JErrorEvent): variant;
  TApplicationCache_onchecking_ = function (ev: JEvent): variant;
  TApplicationCache_onnoupdate_ = function (ev: JEvent): variant;
type
  JApplicationCache = class external "Object"(JEventTarget)
    property status: integer;
    property ondownloading: TApplicationCache_ondownloading_;
    property onprogress: TApplicationCache_onprogress_;
    property onupdateready: TApplicationCache_onupdateready_;
    property oncached: TApplicationCache_oncached_;
    property onobsolete: TApplicationCache_onobsolete_;
    property onerror: TApplicationCache_onerror_;
    property onchecking: TApplicationCache_onchecking_;
    property onnoupdate: TApplicationCache_onnoupdate_;
    procedure swapCache();
    procedure abort();
    procedure update();
    property CHECKING: integer;
    property UNCACHED: integer;
    property UPDATEREADY: integer;
    property DOWNLOADING: integer;
    property IDLE: integer;
    property OBSOLETE: integer;
  end;

type
  JApplicationCache1 = class external "ApplicationCache"
  public
    property prototype: JApplicationCache;
    function &new(): JApplicationCache;
    property CHECKING: integer;
    property UNCACHED: integer;
    property UPDATEREADY: integer;
    property DOWNLOADING: integer;
    property IDLE: integer;
    property OBSOLETE: integer;
  end;


function ApplicationCache_var: JApplicationCache1;

type
  TMSHTMLVideoElementExtensions_onMSVideoOptimalLayoutChanged_ = function (ev: variant): variant;
  TMSHTMLVideoElementExtensions_onMSVideoFrameStepCompleted_ = function (ev: variant): variant;
  TMSHTMLVideoElementExtensions_onMSVideoFormatChanged_ = function (ev: variant): variant;
type
  JMSHTMLVideoElementExtensions = class external "Object"
    property msIsStereo3D: boolean;
    property msStereo3DPackingMode: string;
    property onMSVideoOptimalLayoutChanged: TMSHTMLVideoElementExtensions_onMSVideoOptimalLayoutChanged_;
    property onMSVideoFrameStepCompleted: TMSHTMLVideoElementExtensions_onMSVideoFrameStepCompleted_;
    property msStereo3DRenderMode: string;
    property msIsLayoutOptimalForPlayback: boolean;
    property msHorizontalMirror: boolean;
    property onMSVideoFormatChanged: TMSHTMLVideoElementExtensions_onMSVideoFormatChanged_;
    property msZoom: boolean;
    procedure msInsertVideoEffect(activatableClassId: string; effectRequired: boolean; config: variant = undefined);
    procedure msSetVideoRectangle(left: integer; top: integer; right: integer; bottom: integer);
    procedure msFrameStep(forward: boolean);
  end;

type
  JFrameRequestCallback = class external "Object"
  end;


procedure FrameRequestCallback_(time: integer);external;

type
  JCSS3DTransformsProperties = class external "Object"
    property perspective: string;
    property msBackfaceVisibility: string;
    property perspectiveOrigin: string;
    property transformStyle: string;
    property backfaceVisibility: string;
    property msPerspectiveOrigin: string;
    property msTransformStyle: string;
    property msPerspective: string;
  end;

type
  JXMLHttpRequest2 = class external "Object"
    property withCredentials: boolean;
  end;

type
  JPopStateEvent = class external "Object"(JEvent)
    property state: variant;
    procedure initPopStateEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; stateArg: variant);
  end;

type
  JPopStateEvent1 = class external "PopStateEvent"
  public
    property prototype: JPopStateEvent;
    function &new(): JPopStateEvent;
  end;


function PopStateEvent_var: JPopStateEvent1;

type
  JCSSKeyframeRule = class external "Object"(JCSSRule)
    property keyText: string;
    property style: JCSSStyleDeclaration;
  end;

type
  JCSSKeyframeRule1 = class external "CSSKeyframeRule"
  public
    property prototype: JCSSKeyframeRule;
    function &new(): JCSSKeyframeRule;
  end;


function CSSKeyframeRule_var: JCSSKeyframeRule1;

type
  JCSSGridProperties = class external "Object"
    property msGridRows: string;
    property msGridColumnSpan: variant;
    property msGridRow: variant;
    property msGridRowSpan: variant;
    property msGridColumns: string;
    property msGridColumnAlign: string;
    property msGridRowAlign: string;
    property msGridColumn: variant;
  end;

type
  JMSFileSaver = class external "Object"
    function msSaveBlob(blob: variant; defaultName: string = ""): boolean;
    function msSaveOrOpenBlob(blob: variant; defaultName: string = ""): boolean;
  end;

type
  JMSStream = class external "Object"
    property &type: string;
    function msDetachStream(): variant;
    procedure msClose();
  end;

type
  JMSStream1 = class external "MSStream"
  public
    property prototype: JMSStream;
    function &new(): JMSStream;
  end;


function MSStream_var: JMSStream1;

type
  JMediaError2 = class external "Object"(JMSMediaErrorExtensions)
  end;

type
  JHTMLFieldSetElement2 = class external "Object"
    property validationMessage: string;
    property validity: JValidityState;
    property willValidate: boolean;
    function checkValidity(): boolean;
    procedure setCustomValidity(error: string);
  end;

type
  JMSBlobBuilder = class external "Object"
    procedure append(data: variant; endings: string = "");
    function getBlob(contentType: string = ""): JBlob;
  end;

type
  JMSBlobBuilder1 = class external "MSBlobBuilder"
  public
    property prototype: JMSBlobBuilder;
    function &new(): JMSBlobBuilder;
  end;


function MSBlobBuilder_var: JMSBlobBuilder1;

type
  JMSRangeExtensions = class external "Object"
    function createContextualFragment(fragment: string): JDocumentFragment;
  end;

type
  THTMLElement_oncuechange_ = function (ev: JEvent): variant;
type
  JHTMLElement2 = class external "Object"
    property oncuechange: THTMLElement_oncuechange_;
    property spellcheck: boolean;
    property classList: JDOMTokenList;
    property draggable: boolean;
  end;

type
  JDataTransfer2 = class external "Object"
    property types: JDOMStringList;
    property files: JFileList;
  end;

type
  JDOMSettableTokenList = class external "Object"(JDOMTokenList)
    property value: string;
  end;

type
  JDOMSettableTokenList1 = class external "DOMSettableTokenList"
  public
    property prototype: JDOMSettableTokenList;
    function &new(): JDOMSettableTokenList;
  end;


function DOMSettableTokenList_var: JDOMSettableTokenList1;

type
  JIDBFactory = class external "Object"
    function open(name: string; version: integer = 0): JIDBOpenDBRequest;
    function cmp(first: variant; second: variant): integer;
    function deleteDatabase(name: string): JIDBOpenDBRequest;
  end;

type
  JIDBFactory1 = class external "IDBFactory"
  public
    property prototype: JIDBFactory;
    function &new(): JIDBFactory;
  end;


function IDBFactory_var: JIDBFactory1;

type
  JRange2 = class external "Object"(JMSRangeExtensions)
  end;

type
  JHTMLObjectElement2 = class external "Object"
    property validationMessage: string;
    property validity: JValidityState;
    property willValidate: boolean;
    function checkValidity(): boolean;
    procedure setCustomValidity(error: string);
  end;

type
  JMSPointerEvent = class external "Object"(JMouseEvent)
    property width: integer;
    property rotation: integer;
    property pressure: integer;
    property pointerType: integer;
    property isPrimary: boolean;
    property tiltY: integer;
    property height: integer;
    property intermediatePoints: variant;
    property currentPoint: variant;
    property tiltX: integer;
    property hwTimestamp: integer;
    property pointerId: integer;
    procedure initPointerEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; detailArg: integer; screenXArg: integer; screenYArg: integer; clientXArg: integer; clientYArg: integer; ctrlKeyArg: boolean; altKeyArg: boolean; shiftKeyArg: boolean; metaKeyArg: boolean; buttonArg: integer; relatedTargetArg: JEventTarget; offsetXArg: integer; offsetYArg: integer; widthArg: integer; heightArg: integer; pressure: integer; rotation: integer; tiltX: integer; tiltY: integer; pointerIdArg: integer; pointerType: integer; hwTimestampArg: integer; isPrimary: boolean);
    procedure getCurrentPoint(element: JElement);
    procedure getIntermediatePoints(element: JElement);
    property MSPOINTER_TYPE_PEN: integer;
    property MSPOINTER_TYPE_MOUSE: integer;
    property MSPOINTER_TYPE_TOUCH: integer;
  end;

type
  JMSPointerEvent1 = class external "MSPointerEvent"
  public
    property prototype: JMSPointerEvent;
    function &new(): JMSPointerEvent;
    property MSPOINTER_TYPE_PEN: integer;
    property MSPOINTER_TYPE_MOUSE: integer;
    property MSPOINTER_TYPE_TOUCH: integer;
  end;


function MSPointerEvent_var: JMSPointerEvent1;

type
  JCSSTextProperties = class external "Object"
    property textShadow: string;
    property msHyphenateLimitLines: variant;
    property msHyphens: string;
    property msHyphenateLimitChars: string;
    property msHyphenateLimitZone: variant;
  end;

type
  JCSS2DTransformsProperties = class external "Object"
    property transform: string;
    property transformOrigin: string;
  end;

type
  JDOMException2 = class external "Object"
    property name: string;
    property INVALID_NODE_TYPE_ERR: integer;
    property DATA_CLONE_ERR: integer;
    property TIMEOUT_ERR: integer;
  end;

type
  JMSCSSHighContrastProperties = class external "Object"
    property msHighContrastAdjust: string;
  end;

type
  JMSManipulationEvent = class external "Object"(JUIEvent)
    property lastState: integer;
    property currentState: integer;
    procedure initMSManipulationEvent(typeArg: string; canBubbleArg: boolean; cancelableArg: boolean; viewArg: JAbstractView; detailArg: integer; lastState: integer; currentState: integer);
    property MS_MANIPULATION_STATE_STOPPED: integer;
    property MS_MANIPULATION_STATE_ACTIVE: integer;
    property MS_MANIPULATION_STATE_INERTIA: integer;
  end;

type
  JMSManipulationEvent1 = class external "MSManipulationEvent"
  public
    property prototype: JMSManipulationEvent;
    function &new(): JMSManipulationEvent;
    property MS_MANIPULATION_STATE_STOPPED: integer;
    property MS_MANIPULATION_STATE_ACTIVE: integer;
    property MS_MANIPULATION_STATE_INERTIA: integer;
  end;


function MSManipulationEvent_var: JMSManipulationEvent1;

type
  JFormData = class external "Object"
    procedure append(name: variant; value: variant; blobName: string = "");
  end;

type
  JFormData1 = class external "FormData"
  public
    property prototype: JFormData;
    function &new(): JFormData;
    function &new(form: JHTMLFormElement): JFormData;overload;
  end;


function FormData_var: JFormData1;

type
  JMSHTMLImageElementExtensions1 = class external "Object"
    property msPlayToPrimary: boolean;
    property msPlayToDisabled: boolean;
    property msPlayToSource: variant;
  end;

type
  JMSHTMLImageElementExtensions2 = class external "MSHTMLImageElementExtensions"
  public
    property prototype: JMSHTMLImageElementExtensions;
    function &new(): JMSHTMLImageElementExtensions;
  end;


function MSHTMLImageElementExtensions_var: JMSHTMLImageElementExtensions2;

type
  JMSHTMLMediaElementExtensions = class external "Object"
    property msAudioCategory: string;
    property msRealTime: boolean;
    property msPlayToPrimary: boolean;
    property msPlayToDisabled: boolean;
    property msPlayToSource: variant;
    property msAudioDeviceType: string;
    procedure msClearEffects();
    procedure msSetMediaProtectionManager(mediaProtectionManager: variant = undefined);
    procedure msInsertAudioEffect(activatableClassId: string; effectRequired: boolean; config: variant = undefined);
  end;

type
  JSVGFEImageElement = class external "Object"(JSVGElement)//, JSVGLangSpace)//, JSVGFilterPrimitiveStandardAttributes)//, JSVGURIReference)
    property preserveAspectRatio: JSVGAnimatedPreserveAspectRatio;
  end;

type
  JSVGFEImageElement1 = class external "SVGFEImageElement"
  public
    property prototype: JSVGFEImageElement;
    function &new(): JSVGFEImageElement;
  end;


function SVGFEImageElement_var: JSVGFEImageElement1;

type
  JHTMLDataListElement = class external "Object"(JHTMLElement)
    property options: JHTMLCollection;
  end;

type
  JHTMLDataListElement1 = class external "HTMLDataListElement"
  public
    property prototype: JHTMLDataListElement;
    function &new(): JHTMLDataListElement;
  end;


function HTMLDataListElement_var: JHTMLDataListElement1;

type
  TAbstractWorker_onerror_ = function (ev: JErrorEvent): variant;
type
  JAbstractWorker = class external "Object"(JEventTarget)
    property onerror: TAbstractWorker_onerror_;
  end;

type
  JSVGFECompositeElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property operator: JSVGAnimatedEnumeration;
    property in2: JSVGAnimatedString;
    property k2: JSVGAnimatedNumber;
    property k1: JSVGAnimatedNumber;
    property k3: JSVGAnimatedNumber;
    property in1: JSVGAnimatedString;
    property k4: JSVGAnimatedNumber;
    property SVG_FECOMPOSITE_OPERATOR_OUT: integer;
    property SVG_FECOMPOSITE_OPERATOR_OVER: integer;
    property SVG_FECOMPOSITE_OPERATOR_XOR: integer;
    property SVG_FECOMPOSITE_OPERATOR_ARITHMETIC: integer;
    property SVG_FECOMPOSITE_OPERATOR_UNKNOWN: integer;
    property SVG_FECOMPOSITE_OPERATOR_IN: integer;
    property SVG_FECOMPOSITE_OPERATOR_ATOP: integer;
  end;

type
  JSVGFECompositeElement1 = class external "SVGFECompositeElement"
  public
    property prototype: JSVGFECompositeElement;
    function &new(): JSVGFECompositeElement;
    property SVG_FECOMPOSITE_OPERATOR_OUT: integer;
    property SVG_FECOMPOSITE_OPERATOR_OVER: integer;
    property SVG_FECOMPOSITE_OPERATOR_XOR: integer;
    property SVG_FECOMPOSITE_OPERATOR_ARITHMETIC: integer;
    property SVG_FECOMPOSITE_OPERATOR_UNKNOWN: integer;
    property SVG_FECOMPOSITE_OPERATOR_IN: integer;
    property SVG_FECOMPOSITE_OPERATOR_ATOP: integer;
  end;


function SVGFECompositeElement_var: JSVGFECompositeElement1;

type
  JValidityState = class external "Object"
    property customError: boolean;
    property valueMissing: boolean;
    property stepMismatch: boolean;
    property rangeUnderflow: boolean;
    property rangeOverflow: boolean;
    property typeMismatch: boolean;
    property patternMismatch: boolean;
    property tooLong: boolean;
    property valid: boolean;
  end;

type
  JValidityState1 = class external "ValidityState"
  public
    property prototype: JValidityState;
    function &new(): JValidityState;
  end;


function ValidityState_var: JValidityState1;

type
  JHTMLVideoElement2 = class external "Object"(JMSHTMLVideoElementExtensions)
  end;

type
  JHTMLTrackElement = class external "Object"(JHTMLElement)
    property kind: string;
    property src: string;
    property srclang: string;
    property track: JTextTrack;
    property label: string;
    property defaul: boolean;
  end;

type
  JHTMLTrackElement1 = class external "HTMLTrackElement"
  public
    property prototype: JHTMLTrackElement;
    function &new(): JHTMLTrackElement;
  end;


function HTMLTrackElement_var: JHTMLTrackElement1;

type
  JMSApp = class external "Object"
    function createFileFromStorageFile(storageFile: variant): JFile;
    function createBlobFromRandomAccessStream(&type: string; seeker: variant): JBlob;
    function createStreamFromInputStream(&type: string; inputStream: variant): JMSStream;
    procedure terminateApp(exceptionObject: variant);
    function createDataPackage(object_: variant): variant;
    function execUnsafeLocalFunction(unsafeFunction: JMSUnsafeFunctionCallback): variant;
    function getHtmlPrintDocumentSource(htmlDoc: variant; printTemplate: string = ""): variant;
    procedure addPublicLocalApplicationUri(uri: string);
    function createDataPackageFromSelection(): variant;
  end;


function MSApp_var: JMSApp;

type
  TMSXMLHttpRequestExtensions_onprogress_ = function (ev: JProgressEvent): variant;
  TMSXMLHttpRequestExtensions_onabort_ = function (ev: variant): variant;
  TMSXMLHttpRequestExtensions_onloadend_ = function (ev: JProgressEvent): variant;
  TMSXMLHttpRequestExtensions_onerror_ = function (ev: JErrorEvent): variant;
  TMSXMLHttpRequestExtensions_onloadstart_ = function (ev: variant): variant;
type
  JMSXMLHttpRequestExtensions1 = class external "Object"
    property response: variant;
    property onprogress: TMSXMLHttpRequestExtensions_onprogress_;
    property onabort: TMSXMLHttpRequestExtensions_onabort_;
    property responseType: string;
    property onloadend: TMSXMLHttpRequestExtensions_onloadend_;
    property upload: JXMLHttpRequestEventTarget;
    property onerror: TMSXMLHttpRequestExtensions_onerror_;
    property onloadstart: TMSXMLHttpRequestExtensions_onloadstart_;
  end;

type
  JMSXMLHttpRequestExtensions2 = class external "MSXMLHttpRequestExtensions"
  public
    property prototype: JMSXMLHttpRequestExtensions;
    function &new(): JMSXMLHttpRequestExtensions;
  end;


function MSXMLHttpRequestExtensions_var: JMSXMLHttpRequestExtensions2;

type
  JSVGFEDiffuseLightingElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property kernelUnitLengthY: JSVGAnimatedNumber;
    property surfaceScale: JSVGAnimatedNumber;
    property in1: JSVGAnimatedString;
    property kernelUnitLengthX: JSVGAnimatedNumber;
    property diffuseConstant: JSVGAnimatedNumber;
  end;

type
  JSVGFEDiffuseLightingElement1 = class external "SVGFEDiffuseLightingElement"
  public
    property prototype: JSVGFEDiffuseLightingElement;
    function &new(): JSVGFEDiffuseLightingElement;
  end;


function SVGFEDiffuseLightingElement_var: JSVGFEDiffuseLightingElement1;

type
  JSVGFEComponentTransferElement = class external "Object"(JSVGElement)//, JSVGFilterPrimitiveStandardAttributes)
    property in1: JSVGAnimatedString;
  end;

type
  JSVGFEComponentTransferElement1 = class external "SVGFEComponentTransferElement"
  public
    property prototype: JSVGFEComponentTransferElement;
    function &new(): JSVGFEComponentTransferElement;
  end;


function SVGFEComponentTransferElement_var: JSVGFEComponentTransferElement1;

type
  JMSCSSMatrix = class external "Object"
    property m24: integer;
    property m34: integer;
    property a: integer;
    property d: integer;
    property m32: integer;
    property m41: integer;
    property m11: integer;
    property f: integer;
    property e: integer;
    property m23: integer;
    property m14: integer;
    property m33: integer;
    property m22: integer;
    property m21: integer;
    property c: integer;
    property m12: integer;
    property b: integer;
    property m42: integer;
    property m31: integer;
    property m43: integer;
    property m13: integer;
    property m44: integer;
    function multiply(secondMatrix: JMSCSSMatrix): JMSCSSMatrix;
    function skewY(angle: integer): JMSCSSMatrix;
    procedure setMatrixValue(value: string);
    function inverse(): JMSCSSMatrix;
    function rotateAxisAngle(x: integer; y: integer; z: integer; angle: integer): JMSCSSMatrix;
    function toString(): string;
    function rotate(angleX: integer; angleY: integer = 0; angleZ: integer = 0): JMSCSSMatrix;
    function translate(x: integer; y: integer; z: integer = 0): JMSCSSMatrix;
    function scale(scaleX: integer; scaleY: integer = 0; scaleZ: integer = 0): JMSCSSMatrix;
    function skewX(angle: integer): JMSCSSMatrix;
  end;

type
  JMSCSSMatrix1 = class external "MSCSSMatrix"
  public
    property prototype: JMSCSSMatrix;
    function &new(text: string = ""): JMSCSSMatrix;
  end;


function MSCSSMatrix_var: JMSCSSMatrix1;

type
  TWorker_onmessage_ = function (ev: variant): variant;
type
  JWorker = class external "Object"(JAbstractWorker)
    property onmessage: TWorker_onmessage_;
    procedure postMessage(message: variant; ports: variant = undefined);
    procedure terminate();
  end;

type
  JWorker1 = class external "Worker"
  public
    property prototype: JWorker;
    function &new(stringUrl: string): JWorker;
  end;


function Worker_var: JWorker1;

type
  JHTMLIFrameElement2 = class external "Object"
    property sandbox: JDOMSettableTokenList;
  end;

type
  JMSMediaErrorExtensions = class external "Object"
    property msExtendedCode: integer;
  end;

type
  JMSNavigatorAbilities1 = class external "Object"
    property msProtocols: JMSProtocolsCollection;
    property msMaxTouchPoints: integer;
    property msPointerEnabled: boolean;
    property msManipulationViewsEnabled: boolean;
  end;

type
  JMSNavigatorAbilities2 = class external "MSNavigatorAbilities"
  public
    property prototype: JMSNavigatorAbilities;
    function &new(): JMSNavigatorAbilities;
  end;


function MSNavigatorAbilities_var: JMSNavigatorAbilities2;

type
  Tonpopstate_ = function (ev: JPopStateEvent): variant;

function onpopstate_var: Tonpopstate_;
function applicationCache_var: JApplicationCache;
function matchMedia(mediaQuery: string): JMediaQueryList;external;
function msMatchMedia(mediaQuery: string): JMediaQueryList;external;
function animationStartTime_var: integer;
function msAnimationStartTime_var: integer;
procedure msCancelRequestAnimationFrame(handle: integer);external;
procedure cancelAnimationFrame(handle: integer);external;
function requestAnimationFrame(callback: JFrameRequestCallback): integer;external;
function msRequestAnimationFrame(callback: JFrameRequestCallback): integer;external;
function btoa(rawString: string): string;external;
function atob(encodedString: string): string;external;
function msIndexedDB_var: JIDBFactory;
function indexedDB_var: JIDBFactory;
function console_var: JConsole;
procedure importScripts({many?}urls: array of string);external;

type
  JActiveXObject = class external "ActiveXObject"
  public
    function &new(s: string): variant;
  end;


function ActiveXObject_var: JActiveXObject;

type
  JITextWriter = class external "Object"
    procedure Write(s: string);
    procedure WriteLine(s: string);
    procedure Close();
  end;

type
  TJWScript_Arguments_object_ = class;
type
  TJWScript_Arguments_object_ = class 
  public
    property length: integer;
    function Item(n: integer): string;
  end;

  JWScript = class external "WScript"
  public
    procedure Echo(s: Variant);
    property StdErr: JITextWriter;
    property StdOut: JITextWriter;
    property Arguments: TJWScript_Arguments_object_;
    property ScriptFullName: string;
    procedure Quit(exitCode: integer = 0);
  end;


function WScript_var: JWScript;


implementation

function NaN_var: integer;
begin
  asm @Result = NaN; end;
end;

function Infinity_var: integer;
begin
  asm @Result = Infinity; end;
end;

function Object_var: JObject1;
begin
  asm @Result = Object; end;
end;

function &Function_var: J&Function1;
begin
  asm @Result = &Function; end;
end;

function String_var: JString1;
begin
  asm @Result = String; end;
end;

function Boolean_var: JBoolean1;
begin
  asm @Result = Boolean; end;
end;

function Number_var: JNumber1;
begin
  asm @Result = Number; end;
end;

function Math_var: JMath;
begin
  asm @Result = Math; end;
end;

function Date_var: JDate1;
begin
  asm @Result = Date; end;
end;

function RegExp_var: JRegExp1;
begin
  asm @Result = RegExp; end;
end;

function Error_var: JError1;
begin
  asm @Result = Error; end;
end;

function EvalError_var: JEvalError1;
begin
  asm @Result = EvalError; end;
end;

function RangeError_var: JRangeError1;
begin
  asm @Result = RangeError; end;
end;

function ReferenceError_var: JReferenceError1;
begin
  asm @Result = ReferenceError; end;
end;

function SyntaxError_var: JSyntaxError1;
begin
  asm @Result = SyntaxError; end;
end;

function TypeError_var: JTypeError1;
begin
  asm @Result = TypeError; end;
end;

function URIError_var: JURIError1;
begin
  asm @Result = URIError; end;
end;

function JSON_var: JJSON;
begin
  asm @Result = JSON; end;
end;

function &Array_var: J&Array1;
begin
  asm @Result = &Array; end;
end;

function ArrayBuffer_var: JArrayBuffer1;
begin
  asm @Result = ArrayBuffer; end;
end;

function Int8Array_var: JInt8Array1;
begin
  asm @Result = Int8Array; end;
end;

function Uint8Array_var: JUint8Array1;
begin
  asm @Result = Uint8Array; end;
end;

function Int16Array_var: JInt16Array1;
begin
  asm @Result = Int16Array; end;
end;

function Uint16Array_var: JUint16Array1;
begin
  asm @Result = Uint16Array; end;
end;

function Int32Array_var: JInt32Array1;
begin
  asm @Result = Int32Array; end;
end;

function Uint32Array_var: JUint32Array1;
begin
  asm @Result = Uint32Array; end;
end;

function Float32Array_var: JFloat32Array1;
begin
  asm @Result = Float32Array; end;
end;

function Float64Array_var: JFloat64Array1;
begin
  asm @Result = Float64Array; end;
end;

function DataView_var: JDataView1;
begin
  asm @Result = DataView; end;
end;

function HTMLTableElement_var: JHTMLTableElement1;
begin
  asm @Result = HTMLTableElement; end;
end;

function TreeWalker_var: JTreeWalker1;
begin
  asm @Result = TreeWalker; end;
end;

function SVGPathSegCurvetoQuadraticRel_var: JSVGPathSegCurvetoQuadraticRel1;
begin
  asm @Result = SVGPathSegCurvetoQuadraticRel; end;
end;

function Performance_var: JPerformance1;
begin
  asm @Result = Performance; end;
end;

function CompositionEvent_var: JCompositionEvent1;
begin
  asm @Result = CompositionEvent; end;
end;

function SVGMarkerElement_var: JSVGMarkerElement1;
begin
  asm @Result = SVGMarkerElement; end;
end;

function CSSStyleDeclaration_var: JCSSStyleDeclaration1;
begin
  asm @Result = CSSStyleDeclaration; end;
end;

function SVGGElement_var: JSVGGElement1;
begin
  asm @Result = SVGGElement; end;
end;

function MSStyleCSSProperties_var: JMSStyleCSSProperties1;
begin
  asm @Result = MSStyleCSSProperties; end;
end;

function Navigator_var: JNavigator1;
begin
  asm @Result = Navigator; end;
end;

function SVGPathSegCurvetoCubicSmoothAbs_var: JSVGPathSegCurvetoCubicSmoothAbs1;
begin
  asm @Result = SVGPathSegCurvetoCubicSmoothAbs; end;
end;

function SVGZoomEvent_var: JSVGZoomEvent1;
begin
  asm @Result = SVGZoomEvent; end;
end;

function HTMLTableDataCellElement_var: JHTMLTableDataCellElement1;
begin
  asm @Result = HTMLTableDataCellElement; end;
end;

function HTMLBaseElement_var: JHTMLBaseElement1;
begin
  asm @Result = HTMLBaseElement; end;
end;

function ClientRect_var: JClientRect1;
begin
  asm @Result = ClientRect; end;
end;

function DOMImplementation_var: JDOMImplementation1;
begin
  asm @Result = DOMImplementation; end;
end;

function SVGUnitTypes_var: JSVGUnitTypes1;
begin
  asm @Result = SVGUnitTypes; end;
end;

function Element_var: JElement1;
begin
  asm @Result = Element; end;
end;

function HTMLNextIdElement_var: JHTMLNextIdElement1;
begin
  asm @Result = HTMLNextIdElement; end;
end;

function SVGPathSegMovetoRel_var: JSVGPathSegMovetoRel1;
begin
  asm @Result = SVGPathSegMovetoRel; end;
end;

function SVGLineElement_var: JSVGLineElement1;
begin
  asm @Result = SVGLineElement; end;
end;

function HTMLParagraphElement_var: JHTMLParagraphElement1;
begin
  asm @Result = HTMLParagraphElement; end;
end;

function HTMLAreasCollection_var: JHTMLAreasCollection1;
begin
  asm @Result = HTMLAreasCollection; end;
end;

function SVGDescElement_var: JSVGDescElement1;
begin
  asm @Result = SVGDescElement; end;
end;

function Node_var: JNode1;
begin
  asm @Result = Node; end;
end;

function SVGPathSegCurvetoQuadraticSmoothRel_var: JSVGPathSegCurvetoQuadraticSmoothRel1;
begin
  asm @Result = SVGPathSegCurvetoQuadraticSmoothRel; end;
end;

function MSScriptHost_var: JMSScriptHost1;
begin
  asm @Result = MSScriptHost; end;
end;

function SVGClipPathElement_var: JSVGClipPathElement1;
begin
  asm @Result = SVGClipPathElement; end;
end;

function MouseEvent_var: JMouseEvent1;
begin
  asm @Result = MouseEvent; end;
end;

function RangeException_var: JRangeException1;
begin
  asm @Result = RangeException; end;
end;

function SVGTextPositioningElement_var: JSVGTextPositioningElement1;
begin
  asm @Result = SVGTextPositioningElement; end;
end;

function HTMLAppletElement_var: JHTMLAppletElement1;
begin
  asm @Result = HTMLAppletElement; end;
end;

function TextMetrics_var: JTextMetrics1;
begin
  asm @Result = TextMetrics; end;
end;

function HTMLOListElement_var: JHTMLOListElement1;
begin
  asm @Result = HTMLOListElement; end;
end;

function SVGAnimatedString_var: JSVGAnimatedString1;
begin
  asm @Result = SVGAnimatedString; end;
end;

function SVGPathSegLinetoVerticalRel_var: JSVGPathSegLinetoVerticalRel1;
begin
  asm @Result = SVGPathSegLinetoVerticalRel; end;
end;

function CDATASection_var: JCDATASection1;
begin
  asm @Result = CDATASection; end;
end;

function StyleMedia_var: JStyleMedia1;
begin
  asm @Result = StyleMedia; end;
end;

function TextRange_var: JTextRange1;
begin
  asm @Result = TextRange; end;
end;

function HTMLSelectElement_var: JHTMLSelectElement1;
begin
  asm @Result = HTMLSelectElement; end;
end;

function CSSStyleSheet_var: JCSSStyleSheet1;
begin
  asm @Result = CSSStyleSheet; end;
end;

function HTMLBlockElement_var: JHTMLBlockElement1;
begin
  asm @Result = HTMLBlockElement; end;
end;

function MSSelection_var: JMSSelection1;
begin
  asm @Result = MSSelection; end;
end;

function HTMLMetaElement_var: JHTMLMetaElement1;
begin
  asm @Result = HTMLMetaElement; end;
end;

function Selection_var: JSelection1;
begin
  asm @Result = Selection; end;
end;

function SVGAnimatedAngle_var: JSVGAnimatedAngle1;
begin
  asm @Result = SVGAnimatedAngle; end;
end;

function SVGPatternElement_var: JSVGPatternElement1;
begin
  asm @Result = SVGPatternElement; end;
end;

function SVGScriptElement_var: JSVGScriptElement1;
begin
  asm @Result = SVGScriptElement; end;
end;

function HTMLDDElement_var: JHTMLDDElement1;
begin
  asm @Result = HTMLDDElement; end;
end;

function NodeIterator_var: JNodeIterator1;
begin
  asm @Result = NodeIterator; end;
end;

function CSSStyleRule_var: JCSSStyleRule1;
begin
  asm @Result = CSSStyleRule; end;
end;

function HTMLLinkElement_var: JHTMLLinkElement1;
begin
  asm @Result = HTMLLinkElement; end;
end;

function SVGViewElement_var: JSVGViewElement1;
begin
  asm @Result = SVGViewElement; end;
end;

function HTMLFontElement_var: JHTMLFontElement1;
begin
  asm @Result = HTMLFontElement; end;
end;

function SVGTitleElement_var: JSVGTitleElement1;
begin
  asm @Result = SVGTitleElement; end;
end;

function ControlRangeCollection_var: JControlRangeCollection1;
begin
  asm @Result = ControlRangeCollection; end;
end;

function MSNamespaceInfo_var: JMSNamespaceInfo1;
begin
  asm @Result = MSNamespaceInfo; end;
end;

function SVGAnimatedTransformList_var: JSVGAnimatedTransformList1;
begin
  asm @Result = SVGAnimatedTransformList; end;
end;

function HTMLTableCaptionElement_var: JHTMLTableCaptionElement1;
begin
  asm @Result = HTMLTableCaptionElement; end;
end;

function HTMLOptionElement_var: JHTMLOptionElement1;
begin
  asm @Result = HTMLOptionElement; end;
end;

function HTMLMapElement_var: JHTMLMapElement1;
begin
  asm @Result = HTMLMapElement; end;
end;

function HTMLMenuElement_var: JHTMLMenuElement1;
begin
  asm @Result = HTMLMenuElement; end;
end;

function MouseWheelEvent_var: JMouseWheelEvent1;
begin
  asm @Result = MouseWheelEvent; end;
end;

function SVGPointList_var: JSVGPointList1;
begin
  asm @Result = SVGPointList; end;
end;

function SVGAnimatedLengthList_var: JSVGAnimatedLengthList1;
begin
  asm @Result = SVGAnimatedLengthList; end;
end;

function Window_var: JWindow1;
begin
  asm @Result = Window; end;
end;

function SVGAnimatedPreserveAspectRatio_var: JSVGAnimatedPreserveAspectRatio1;
begin
  asm @Result = SVGAnimatedPreserveAspectRatio; end;
end;

function MSSiteModeEvent_var: JMSSiteModeEvent1;
begin
  asm @Result = MSSiteModeEvent; end;
end;

function StyleSheetPageList_var: JStyleSheetPageList1;
begin
  asm @Result = StyleSheetPageList; end;
end;

function HTMLCollection_var: JHTMLCollection1;
begin
  asm @Result = HTMLCollection; end;
end;

function MSCSSProperties_var: JMSCSSProperties1;
begin
  asm @Result = MSCSSProperties; end;
end;

function HTMLImageElement_var: JHTMLImageElement1;
begin
  asm @Result = HTMLImageElement; end;
end;

function HTMLAreaElement_var: JHTMLAreaElement1;
begin
  asm @Result = HTMLAreaElement; end;
end;

function SVGAngle_var: JSVGAngle1;
begin
  asm @Result = SVGAngle; end;
end;

function HTMLButtonElement_var: JHTMLButtonElement1;
begin
  asm @Result = HTMLButtonElement; end;
end;

function HTMLSourceElement_var: JHTMLSourceElement1;
begin
  asm @Result = HTMLSourceElement; end;
end;

function CanvasGradient_var: JCanvasGradient1;
begin
  asm @Result = CanvasGradient; end;
end;

function KeyboardEvent_var: JKeyboardEvent1;
begin
  asm @Result = KeyboardEvent; end;
end;

function Document_var: JDocument1;
begin
  asm @Result = Document; end;
end;

function MessageEvent_var: JMessageEvent1;
begin
  asm @Result = MessageEvent; end;
end;

function SVGElement_var: JSVGElement1;
begin
  asm @Result = SVGElement; end;
end;

function HTMLScriptElement_var: JHTMLScriptElement1;
begin
  asm @Result = HTMLScriptElement; end;
end;

function HTMLTableRowElement_var: JHTMLTableRowElement1;
begin
  asm @Result = HTMLTableRowElement; end;
end;

function MSCSSRuleList_var: JMSCSSRuleList1;
begin
  asm @Result = MSCSSRuleList; end;
end;

function CanvasRenderingContext2D_var: JCanvasRenderingContext2D1;
begin
  asm @Result = CanvasRenderingContext2D; end;
end;

function SVGPathSegLinetoHorizontalAbs_var: JSVGPathSegLinetoHorizontalAbs1;
begin
  asm @Result = SVGPathSegLinetoHorizontalAbs; end;
end;

function SVGPathSegArcAbs_var: JSVGPathSegArcAbs1;
begin
  asm @Result = SVGPathSegArcAbs; end;
end;

function HTMLHtmlElement_var: JHTMLHtmlElement1;
begin
  asm @Result = HTMLHtmlElement; end;
end;

function SVGTransformList_var: JSVGTransformList1;
begin
  asm @Result = SVGTransformList; end;
end;

function SVGPathSegClosePath_var: JSVGPathSegClosePath1;
begin
  asm @Result = SVGPathSegClosePath; end;
end;

function HTMLFrameElement_var: JHTMLFrameElement1;
begin
  asm @Result = HTMLFrameElement; end;
end;

function SVGAnimatedLength_var: JSVGAnimatedLength1;
begin
  asm @Result = SVGAnimatedLength; end;
end;

function CSSMediaRule_var: JCSSMediaRule1;
begin
  asm @Result = CSSMediaRule; end;
end;

function HTMLQuoteElement_var: JHTMLQuoteElement1;
begin
  asm @Result = HTMLQuoteElement; end;
end;

function SVGDefsElement_var: JSVGDefsElement1;
begin
  asm @Result = SVGDefsElement; end;
end;

function XMLHttpRequest_var: JXMLHttpRequest1;
begin
  asm @Result = XMLHttpRequest; end;
end;

function HTMLTableHeaderCellElement_var: JHTMLTableHeaderCellElement1;
begin
  asm @Result = HTMLTableHeaderCellElement; end;
end;

function HTMLDListElement_var: JHTMLDListElement1;
begin
  asm @Result = HTMLDListElement; end;
end;

function SVGEllipseElement_var: JSVGEllipseElement1;
begin
  asm @Result = SVGEllipseElement; end;
end;

function SVGPathSegLinetoHorizontalRel_var: JSVGPathSegLinetoHorizontalRel1;
begin
  asm @Result = SVGPathSegLinetoHorizontalRel; end;
end;

function SVGAElement_var: JSVGAElement1;
begin
  asm @Result = SVGAElement; end;
end;

function HTMLFrameSetElement_var: JHTMLFrameSetElement1;
begin
  asm @Result = HTMLFrameSetElement; end;
end;

function Screen_var: JScreen1;
begin
  asm @Result = Screen; end;
end;

function Coordinates_var: JCoordinates1;
begin
  asm @Result = Coordinates; end;
end;

function DataTransfer_var: JDataTransfer1;
begin
  asm @Result = DataTransfer; end;
end;

function FocusEvent_var: JFocusEvent1;
begin
  asm @Result = FocusEvent; end;
end;

function Range_var: JRange1;
begin
  asm @Result = Range; end;
end;

function SVGPoint_var: JSVGPoint1;
begin
  asm @Result = SVGPoint; end;
end;

function MSPluginsCollection_var: JMSPluginsCollection1;
begin
  asm @Result = MSPluginsCollection; end;
end;

function SVGAnimatedNumberList_var: JSVGAnimatedNumberList1;
begin
  asm @Result = SVGAnimatedNumberList; end;
end;

function SVGSVGElement_var: JSVGSVGElement1;
begin
  asm @Result = SVGSVGElement; end;
end;

function HTMLLabelElement_var: JHTMLLabelElement1;
begin
  asm @Result = HTMLLabelElement; end;
end;

function HTMLLegendElement_var: JHTMLLegendElement1;
begin
  asm @Result = HTMLLegendElement; end;
end;

function HTMLDirectoryElement_var: JHTMLDirectoryElement1;
begin
  asm @Result = HTMLDirectoryElement; end;
end;

function SVGAnimatedInteger_var: JSVGAnimatedInteger1;
begin
  asm @Result = SVGAnimatedInteger; end;
end;

function SVGTextElement_var: JSVGTextElement1;
begin
  asm @Result = SVGTextElement; end;
end;

function SVGTSpanElement_var: JSVGTSpanElement1;
begin
  asm @Result = SVGTSpanElement; end;
end;

function HTMLLIElement_var: JHTMLLIElement1;
begin
  asm @Result = HTMLLIElement; end;
end;

function SVGPathSegLinetoVerticalAbs_var: JSVGPathSegLinetoVerticalAbs1;
begin
  asm @Result = SVGPathSegLinetoVerticalAbs; end;
end;

function SVGStyleElement_var: JSVGStyleElement1;
begin
  asm @Result = SVGStyleElement; end;
end;

function MSCurrentStyleCSSProperties_var: JMSCurrentStyleCSSProperties1;
begin
  asm @Result = MSCurrentStyleCSSProperties; end;
end;

function Storage_var: JStorage1;
begin
  asm @Result = Storage; end;
end;

function HTMLIFrameElement_var: JHTMLIFrameElement1;
begin
  asm @Result = HTMLIFrameElement; end;
end;

function TextRangeCollection_var: JTextRangeCollection1;
begin
  asm @Result = TextRangeCollection; end;
end;

function HTMLBodyElement_var: JHTMLBodyElement1;
begin
  asm @Result = HTMLBodyElement; end;
end;

function DocumentType_var: JDocumentType1;
begin
  asm @Result = DocumentType; end;
end;

function SVGRadialGradientElement_var: JSVGRadialGradientElement1;
begin
  asm @Result = SVGRadialGradientElement; end;
end;

function MutationEvent_var: JMutationEvent1;
begin
  asm @Result = MutationEvent; end;
end;

function DragEvent_var: JDragEvent1;
begin
  asm @Result = DragEvent; end;
end;

function HTMLTableSectionElement_var: JHTMLTableSectionElement1;
begin
  asm @Result = HTMLTableSectionElement; end;
end;

function HTMLInputElement_var: JHTMLInputElement1;
begin
  asm @Result = HTMLInputElement; end;
end;

function HTMLAnchorElement_var: JHTMLAnchorElement1;
begin
  asm @Result = HTMLAnchorElement; end;
end;

function SVGImageElement_var: JSVGImageElement1;
begin
  asm @Result = SVGImageElement; end;
end;

function HTMLParamElement_var: JHTMLParamElement1;
begin
  asm @Result = HTMLParamElement; end;
end;

function SVGAnimatedNumber_var: JSVGAnimatedNumber1;
begin
  asm @Result = SVGAnimatedNumber; end;
end;

function PerformanceTiming_var: JPerformanceTiming1;
begin
  asm @Result = PerformanceTiming; end;
end;

function HTMLPreElement_var: JHTMLPreElement1;
begin
  asm @Result = HTMLPreElement; end;
end;

function EventException_var: JEventException1;
begin
  asm @Result = EventException; end;
end;

function SVGMetadataElement_var: JSVGMetadataElement1;
begin
  asm @Result = SVGMetadataElement; end;
end;

function SVGPathSegArcRel_var: JSVGPathSegArcRel1;
begin
  asm @Result = SVGPathSegArcRel; end;
end;

function SVGPathSegMovetoAbs_var: JSVGPathSegMovetoAbs1;
begin
  asm @Result = SVGPathSegMovetoAbs; end;
end;

function SVGStringList_var: JSVGStringList1;
begin
  asm @Result = SVGStringList; end;
end;

function XDomainRequest_var: JXDomainRequest1;
begin
  asm @Result = XDomainRequest; end;
end;

function SVGLength_var: JSVGLength1;
begin
  asm @Result = SVGLength; end;
end;

function SVGPolygonElement_var: JSVGPolygonElement1;
begin
  asm @Result = SVGPolygonElement; end;
end;

function HTMLPhraseElement_var: JHTMLPhraseElement1;
begin
  asm @Result = HTMLPhraseElement; end;
end;

function SVGPathSegCurvetoCubicRel_var: JSVGPathSegCurvetoCubicRel1;
begin
  asm @Result = SVGPathSegCurvetoCubicRel; end;
end;

function MSEventObj_var: JMSEventObj1;
begin
  asm @Result = MSEventObj; end;
end;

function SVGTextContentElement_var: JSVGTextContentElement1;
begin
  asm @Result = SVGTextContentElement; end;
end;

function HTMLCanvasElement_var: JHTMLCanvasElement1;
begin
  asm @Result = HTMLCanvasElement; end;
end;

function HTMLTitleElement_var: JHTMLTitleElement1;
begin
  asm @Result = HTMLTitleElement; end;
end;

function Location_var: JLocation1;
begin
  asm @Result = Location; end;
end;

function HTMLStyleElement_var: JHTMLStyleElement1;
begin
  asm @Result = HTMLStyleElement; end;
end;

function SVGTransform_var: JSVGTransform1;
begin
  asm @Result = SVGTransform; end;
end;

function MSCSSFilter_var: JMSCSSFilter1;
begin
  asm @Result = MSCSSFilter; end;
end;

function UIEvent_var: JUIEvent1;
begin
  asm @Result = UIEvent; end;
end;

function SVGPathSeg_var: JSVGPathSeg1;
begin
  asm @Result = SVGPathSeg; end;
end;

function WheelEvent_var: JWheelEvent1;
begin
  asm @Result = WheelEvent; end;
end;

function SVGNumber_var: JSVGNumber1;
begin
  asm @Result = SVGNumber; end;
end;

function SVGPathElement_var: JSVGPathElement1;
begin
  asm @Result = SVGPathElement; end;
end;

function MSCompatibleInfo_var: JMSCompatibleInfo1;
begin
  asm @Result = MSCompatibleInfo; end;
end;

function Text_var: JText1;
begin
  asm @Result = Text; end;
end;

function SVGAnimatedRect_var: JSVGAnimatedRect1;
begin
  asm @Result = SVGAnimatedRect; end;
end;

function CSSNamespaceRule_var: JCSSNamespaceRule1;
begin
  asm @Result = CSSNamespaceRule; end;
end;

function HTMLUnknownElement_var: JHTMLUnknownElement1;
begin
  asm @Result = HTMLUnknownElement; end;
end;

function SVGPathSegList_var: JSVGPathSegList1;
begin
  asm @Result = SVGPathSegList; end;
end;

function HTMLAudioElement_var: JHTMLAudioElement1;
begin
  asm @Result = HTMLAudioElement; end;
end;

function PositionError_var: JPositionError1;
begin
  asm @Result = PositionError; end;
end;

function BrowserPublic_var: JBrowserPublic1;
begin
  asm @Result = BrowserPublic; end;
end;

function HTMLTableCellElement_var: JHTMLTableCellElement1;
begin
  asm @Result = HTMLTableCellElement; end;
end;

function MSNamespaceInfoCollection_var: JMSNamespaceInfoCollection1;
begin
  asm @Result = MSNamespaceInfoCollection; end;
end;

function SVGElementInstance_var: JSVGElementInstance1;
begin
  asm @Result = SVGElementInstance; end;
end;

function SVGCircleElement_var: JSVGCircleElement1;
begin
  asm @Result = SVGCircleElement; end;
end;

function HTMLBaseFontElement_var: JHTMLBaseFontElement1;
begin
  asm @Result = HTMLBaseFontElement; end;
end;

function CustomEvent_var: JCustomEvent1;
begin
  asm @Result = CustomEvent; end;
end;

function CSSImportRule_var: JCSSImportRule1;
begin
  asm @Result = CSSImportRule; end;
end;

function StyleSheetList_var: JStyleSheetList1;
begin
  asm @Result = StyleSheetList; end;
end;

function HTMLTextAreaElement_var: JHTMLTextAreaElement1;
begin
  asm @Result = HTMLTextAreaElement; end;
end;

function Geolocation_var: JGeolocation1;
begin
  asm @Result = Geolocation; end;
end;

function HTMLMarqueeElement_var: JHTMLMarqueeElement1;
begin
  asm @Result = HTMLMarqueeElement; end;
end;

function SVGRect_var: JSVGRect1;
begin
  asm @Result = SVGRect; end;
end;

function History_var: JHistory1;
begin
  asm @Result = History; end;
end;

function SVGPathSegCurvetoCubicAbs_var: JSVGPathSegCurvetoCubicAbs1;
begin
  asm @Result = SVGPathSegCurvetoCubicAbs; end;
end;

function TimeRanges_var: JTimeRanges1;
begin
  asm @Result = TimeRanges; end;
end;

function SVGPathSegCurvetoQuadraticAbs_var: JSVGPathSegCurvetoQuadraticAbs1;
begin
  asm @Result = SVGPathSegCurvetoQuadraticAbs; end;
end;

function CSSRule_var: JCSSRule1;
begin
  asm @Result = CSSRule; end;
end;

function SVGPathSegLinetoAbs_var: JSVGPathSegLinetoAbs1;
begin
  asm @Result = SVGPathSegLinetoAbs; end;
end;

function HTMLModElement_var: JHTMLModElement1;
begin
  asm @Result = HTMLModElement; end;
end;

function BeforeUnloadEvent_var: JBeforeUnloadEvent1;
begin
  asm @Result = BeforeUnloadEvent; end;
end;

function MSPopupWindow_var: JMSPopupWindow1;
begin
  asm @Result = MSPopupWindow; end;
end;

function SVGMatrix_var: JSVGMatrix1;
begin
  asm @Result = SVGMatrix; end;
end;

function SVGUseElement_var: JSVGUseElement1;
begin
  asm @Result = SVGUseElement; end;
end;

function Event_var: JEvent1;
begin
  asm @Result = Event; end;
end;

function ImageData_var: JImageData1;
begin
  asm @Result = ImageData; end;
end;

function HTMLTableColElement_var: JHTMLTableColElement1;
begin
  asm @Result = HTMLTableColElement; end;
end;

function SVGException_var: JSVGException1;
begin
  asm @Result = SVGException; end;
end;

function SVGAnimatedEnumeration_var: JSVGAnimatedEnumeration1;
begin
  asm @Result = SVGAnimatedEnumeration; end;
end;

function SVGLinearGradientElement_var: JSVGLinearGradientElement1;
begin
  asm @Result = SVGLinearGradientElement; end;
end;

function HTMLUListElement_var: JHTMLUListElement1;
begin
  asm @Result = HTMLUListElement; end;
end;

function SVGRectElement_var: JSVGRectElement1;
begin
  asm @Result = SVGRectElement; end;
end;

function HTMLDivElement_var: JHTMLDivElement1;
begin
  asm @Result = HTMLDivElement; end;
end;

function NamedNodeMap_var: JNamedNodeMap1;
begin
  asm @Result = NamedNodeMap; end;
end;

function MediaList_var: JMediaList1;
begin
  asm @Result = MediaList; end;
end;

function SVGPathSegCurvetoQuadraticSmoothAbs_var: JSVGPathSegCurvetoQuadraticSmoothAbs1;
begin
  asm @Result = SVGPathSegCurvetoQuadraticSmoothAbs; end;
end;

function SVGLengthList_var: JSVGLengthList1;
begin
  asm @Result = SVGLengthList; end;
end;

function SVGPathSegCurvetoCubicSmoothRel_var: JSVGPathSegCurvetoCubicSmoothRel1;
begin
  asm @Result = SVGPathSegCurvetoCubicSmoothRel; end;
end;

function ProcessingInstruction_var: JProcessingInstruction1;
begin
  asm @Result = ProcessingInstruction; end;
end;

function MSBehaviorUrnsCollection_var: JMSBehaviorUrnsCollection1;
begin
  asm @Result = MSBehaviorUrnsCollection; end;
end;

function CSSFontFaceRule_var: JCSSFontFaceRule1;
begin
  asm @Result = CSSFontFaceRule; end;
end;

function TextEvent_var: JTextEvent1;
begin
  asm @Result = TextEvent; end;
end;

function DocumentFragment_var: JDocumentFragment1;
begin
  asm @Result = DocumentFragment; end;
end;

function SVGPolylineElement_var: JSVGPolylineElement1;
begin
  asm @Result = SVGPolylineElement; end;
end;

function Position_var: JPosition1;
begin
  asm @Result = Position; end;
end;

function BookmarkCollection_var: JBookmarkCollection1;
begin
  asm @Result = BookmarkCollection; end;
end;

function CSSPageRule_var: JCSSPageRule1;
begin
  asm @Result = CSSPageRule; end;
end;

function HTMLBRElement_var: JHTMLBRElement1;
begin
  asm @Result = HTMLBRElement; end;
end;

function HTMLSpanElement_var: JHTMLSpanElement1;
begin
  asm @Result = HTMLSpanElement; end;
end;

function HTMLHeadElement_var: JHTMLHeadElement1;
begin
  asm @Result = HTMLHeadElement; end;
end;

function HTMLHeadingElement_var: JHTMLHeadingElement1;
begin
  asm @Result = HTMLHeadingElement; end;
end;

function HTMLFormElement_var: JHTMLFormElement1;
begin
  asm @Result = HTMLFormElement; end;
end;

function SVGZoomAndPan_var: JSVGZoomAndPan1;
begin
  asm @Result = SVGZoomAndPan; end;
end;

function HTMLMediaElement_var: JHTMLMediaElement1;
begin
  asm @Result = HTMLMediaElement; end;
end;

function DOMParser_var: JDOMParser1;
begin
  asm @Result = DOMParser; end;
end;

function MSMimeTypesCollection_var: JMSMimeTypesCollection1;
begin
  asm @Result = MSMimeTypesCollection; end;
end;

function StyleSheet_var: JStyleSheet1;
begin
  asm @Result = StyleSheet; end;
end;

function SVGTextPathElement_var: JSVGTextPathElement1;
begin
  asm @Result = SVGTextPathElement; end;
end;

function NodeList_var: JNodeList1;
begin
  asm @Result = NodeList; end;
end;

function HTMLDTElement_var: JHTMLDTElement1;
begin
  asm @Result = HTMLDTElement; end;
end;

function XMLSerializer_var: JXMLSerializer1;
begin
  asm @Result = XMLSerializer; end;
end;

function SVGGradientElement_var: JSVGGradientElement1;
begin
  asm @Result = SVGGradientElement; end;
end;

function NodeFilter_var: JNodeFilter1;
begin
  asm @Result = NodeFilter; end;
end;

function HTMLFieldSetElement_var: JHTMLFieldSetElement1;
begin
  asm @Result = HTMLFieldSetElement; end;
end;

function MediaError_var: JMediaError1;
begin
  asm @Result = MediaError; end;
end;

function SVGNumberList_var: JSVGNumberList1;
begin
  asm @Result = SVGNumberList; end;
end;

function HTMLBGSoundElement_var: JHTMLBGSoundElement1;
begin
  asm @Result = HTMLBGSoundElement; end;
end;

function HTMLElement_var: JHTMLElement1;
begin
  asm @Result = HTMLElement; end;
end;

function Comment_var: JComment1;
begin
  asm @Result = Comment; end;
end;

function CanvasPattern_var: JCanvasPattern1;
begin
  asm @Result = CanvasPattern; end;
end;

function HTMLHRElement_var: JHTMLHRElement1;
begin
  asm @Result = HTMLHRElement; end;
end;

function HTMLObjectElement_var: JHTMLObjectElement1;
begin
  asm @Result = HTMLObjectElement; end;
end;

function StorageEvent_var: JStorageEvent1;
begin
  asm @Result = StorageEvent; end;
end;

function HTMLEmbedElement_var: JHTMLEmbedElement1;
begin
  asm @Result = HTMLEmbedElement; end;
end;

function CharacterData_var: JCharacterData1;
begin
  asm @Result = CharacterData; end;
end;

function HTMLOptGroupElement_var: JHTMLOptGroupElement1;
begin
  asm @Result = HTMLOptGroupElement; end;
end;

function HTMLIsIndexElement_var: JHTMLIsIndexElement1;
begin
  asm @Result = HTMLIsIndexElement; end;
end;

function SVGPathSegLinetoRel_var: JSVGPathSegLinetoRel1;
begin
  asm @Result = SVGPathSegLinetoRel; end;
end;

function DOMException_var: JDOMException1;
begin
  asm @Result = DOMException; end;
end;

function MSCompatibleInfoCollection_var: JMSCompatibleInfoCollection1;
begin
  asm @Result = MSCompatibleInfoCollection; end;
end;

function SVGAnimatedBoolean_var: JSVGAnimatedBoolean1;
begin
  asm @Result = SVGAnimatedBoolean; end;
end;

function SVGSwitchElement_var: JSVGSwitchElement1;
begin
  asm @Result = SVGSwitchElement; end;
end;

function SVGPreserveAspectRatio_var: JSVGPreserveAspectRatio1;
begin
  asm @Result = SVGPreserveAspectRatio; end;
end;

function Attr_var: JAttr1;
begin
  asm @Result = Attr; end;
end;

function PerformanceNavigation_var: JPerformanceNavigation1;
begin
  asm @Result = PerformanceNavigation; end;
end;

function SVGStopElement_var: JSVGStopElement1;
begin
  asm @Result = SVGStopElement; end;
end;

function SVGSymbolElement_var: JSVGSymbolElement1;
begin
  asm @Result = SVGSymbolElement; end;
end;

function SVGElementInstanceList_var: JSVGElementInstanceList1;
begin
  asm @Result = SVGElementInstanceList; end;
end;

function CSSRuleList_var: JCSSRuleList1;
begin
  asm @Result = CSSRuleList; end;
end;

function HTMLVideoElement_var: JHTMLVideoElement1;
begin
  asm @Result = HTMLVideoElement; end;
end;

function ClientRectList_var: JClientRectList1;
begin
  asm @Result = ClientRectList; end;
end;

function SVGMaskElement_var: JSVGMaskElement1;
begin
  asm @Result = SVGMaskElement; end;
end;

function Audio_var: JAudio;
begin
  asm @Result = Audio; end;
end;

function Option_var: JOption;
begin
  asm @Result = Option; end;
end;

function Image_var: JImage;
begin
  asm @Result = Image; end;
end;

function ondragend_var: Tondragend_;
begin
  asm @Result = ondragend; end;
end;

function onkeydown_var: Tonkeydown_;
begin
  asm @Result = onkeydown; end;
end;

function ondragover_var: Tondragover_;
begin
  asm @Result = ondragover; end;
end;

function onkeyup_var: Tonkeyup_;
begin
  asm @Result = onkeyup; end;
end;

function onreset_var: Tonreset_;
begin
  asm @Result = onreset; end;
end;

function onmouseup_var: Tonmouseup_;
begin
  asm @Result = onmouseup; end;
end;

function ondragstart_var: Tondragstart_;
begin
  asm @Result = ondragstart; end;
end;

function ondrag_var: Tondrag_;
begin
  asm @Result = ondrag; end;
end;

function onmouseover_var: Tonmouseover_;
begin
  asm @Result = onmouseover; end;
end;

function ondragleave_var: Tondragleave_;
begin
  asm @Result = ondragleave; end;
end;

function history_var: JHistory;
begin
  asm @Result = history; end;
end;

function name_var: string;
begin
  asm @Result = name; end;
end;

function onafterprint_var: Tonafterprint_;
begin
  asm @Result = onafterprint; end;
end;

function onpause_var: Tonpause_;
begin
  asm @Result = onpause; end;
end;

function onbeforeprint_var: Tonbeforeprint_;
begin
  asm @Result = onbeforeprint; end;
end;

function top_var: JWindow;
begin
  asm @Result = top; end;
end;

function onmousedown_var: Tonmousedown_;
begin
  asm @Result = onmousedown; end;
end;

function onseeked_var: Tonseeked_;
begin
  asm @Result = onseeked; end;
end;

function opener_var: JWindow;
begin
  asm @Result = opener; end;
end;

function onclick_var: Tonclick_;
begin
  asm @Result = onclick; end;
end;

function onwaiting_var: Tonwaiting_;
begin
  asm @Result = onwaiting; end;
end;

function ononline_var: Tononline_;
begin
  asm @Result = ononline; end;
end;

function ondurationchange_var: Tondurationchange_;
begin
  asm @Result = ondurationchange; end;
end;

function frames_var: JWindow;
begin
  asm @Result = frames; end;
end;

function onblur_var: Tonblur_;
begin
  asm @Result = onblur; end;
end;

function onemptied_var: Tonemptied_;
begin
  asm @Result = onemptied; end;
end;

function onseeking_var: Tonseeking_;
begin
  asm @Result = onseeking; end;
end;

function oncanplay_var: Toncanplay_;
begin
  asm @Result = oncanplay; end;
end;

function onstalled_var: Tonstalled_;
begin
  asm @Result = onstalled; end;
end;

function onmousemove_var: Tonmousemove_;
begin
  asm @Result = onmousemove; end;
end;

function onoffline_var: Tonoffline_;
begin
  asm @Result = onoffline; end;
end;

function length_var: integer;
begin
  asm @Result = length; end;
end;

function onbeforeunload_var: Tonbeforeunload_;
begin
  asm @Result = onbeforeunload; end;
end;

function onratechange_var: Tonratechange_;
begin
  asm @Result = onratechange; end;
end;

function onstorage_var: Tonstorage_;
begin
  asm @Result = onstorage; end;
end;

function onloadstart_var: Tonloadstart_;
begin
  asm @Result = onloadstart; end;
end;

function ondragenter_var: Tondragenter_;
begin
  asm @Result = ondragenter; end;
end;

function onsubmit_var: Tonsubmit_;
begin
  asm @Result = onsubmit; end;
end;

function self_var: JWindow;
begin
  asm @Result = self; end;
end;

function onprogress_var: Tonprogress_;
begin
  asm @Result = onprogress; end;
end;

function ondblclick_var: Tondblclick_;
begin
  asm @Result = ondblclick; end;
end;

function oncontextmenu_var: Toncontextmenu_;
begin
  asm @Result = oncontextmenu; end;
end;

function onchange_var: Tonchange_;
begin
  asm @Result = onchange; end;
end;

function onloadedmetadata_var: Tonloadedmetadata_;
begin
  asm @Result = onloadedmetadata; end;
end;

function onplay_var: Tonplay_;
begin
  asm @Result = onplay; end;
end;

function onerror_var: JErrorFunction;
begin
  asm @Result = onerror; end;
end;

function onplaying_var: Tonplaying_;
begin
  asm @Result = onplaying; end;
end;

function parent_var: JWindow;
begin
  asm @Result = parent; end;
end;

function location_var: JLocation;
begin
  asm @Result = location; end;
end;

function oncanplaythrough_var: Toncanplaythrough_;
begin
  asm @Result = oncanplaythrough; end;
end;

function onabort_var: Tonabort_;
begin
  asm @Result = onabort; end;
end;

function onreadystatechange_var: Tonreadystatechange_;
begin
  asm @Result = onreadystatechange; end;
end;

function onkeypress_var: Tonkeypress_;
begin
  asm @Result = onkeypress; end;
end;

function frameElement_var: JElement;
begin
  asm @Result = frameElement; end;
end;

function onloadeddata_var: Tonloadeddata_;
begin
  asm @Result = onloadeddata; end;
end;

function onsuspend_var: Tonsuspend_;
begin
  asm @Result = onsuspend; end;
end;

function window_var: JWindow;
begin
  asm @Result = window; end;
end;

function onfocus_var: Tonfocus_;
begin
  asm @Result = onfocus; end;
end;

function onmessage_var: Tonmessage_;
begin
  asm @Result = onmessage; end;
end;

function ontimeupdate_var: Tontimeupdate_;
begin
  asm @Result = ontimeupdate; end;
end;

function onresize_var: Tonresize_;
begin
  asm @Result = onresize; end;
end;

function navigator_var: JNavigator;
begin
  asm @Result = navigator; end;
end;

function onselect_var: Tonselect_;
begin
  asm @Result = onselect; end;
end;

function ondrop_var: Tondrop_;
begin
  asm @Result = ondrop; end;
end;

function onmouseout_var: Tonmouseout_;
begin
  asm @Result = onmouseout; end;
end;

function onended_var: Tonended_;
begin
  asm @Result = onended; end;
end;

function onhashchange_var: Tonhashchange_;
begin
  asm @Result = onhashchange; end;
end;

function onunload_var: Tonunload_;
begin
  asm @Result = onunload; end;
end;

function onscroll_var: Tonscroll_;
begin
  asm @Result = onscroll; end;
end;

function onmousewheel_var: Tonmousewheel_;
begin
  asm @Result = onmousewheel; end;
end;

function onload_var: Tonload_;
begin
  asm @Result = onload; end;
end;

function onvolumechange_var: Tonvolumechange_;
begin
  asm @Result = onvolumechange; end;
end;

function oninput_var: Toninput_;
begin
  asm @Result = oninput; end;
end;

function status_var: string;
begin
  asm @Result = status; end;
end;

function onmouseleave_var: Tonmouseleave_;
begin
  asm @Result = onmouseleave; end;
end;

function screenLeft_var: integer;
begin
  asm @Result = screenLeft; end;
end;

function offscreenBuffering_var: variant;
begin
  asm @Result = offscreenBuffering; end;
end;

function maxConnectionsPerServer_var: integer;
begin
  asm @Result = maxConnectionsPerServer; end;
end;

function onmouseenter_var: Tonmouseenter_;
begin
  asm @Result = onmouseenter; end;
end;

function clipboardData_var: JDataTransfer;
begin
  asm @Result = clipboardData; end;
end;

function defaultStatus_var: string;
begin
  asm @Result = defaultStatus; end;
end;

function clientInformation_var: JNavigator;
begin
  asm @Result = clientInformation; end;
end;

function closed_var: boolean;
begin
  asm @Result = closed; end;
end;

function onhelp_var: Tonhelp_;
begin
  asm @Result = onhelp; end;
end;

function external_var: JBrowserPublic;
begin
  asm @Result = external; end;
end;

function event_var: JMSEventObj;
begin
  asm @Result = event; end;
end;

function onfocusout_var: Tonfocusout_;
begin
  asm @Result = onfocusout; end;
end;

function screenTop_var: integer;
begin
  asm @Result = screenTop; end;
end;

function onfocusin_var: Tonfocusin_;
begin
  asm @Result = onfocusin; end;
end;

function performance_var: variant;
begin
  asm @Result = performance; end;
end;

function outerWidth_var: integer;
begin
  asm @Result = outerWidth; end;
end;

function pageXOffset_var: integer;
begin
  asm @Result = pageXOffset; end;
end;

function innerWidth_var: integer;
begin
  asm @Result = innerWidth; end;
end;

function pageYOffset_var: integer;
begin
  asm @Result = pageYOffset; end;
end;

function screenY_var: integer;
begin
  asm @Result = screenY; end;
end;

function outerHeight_var: integer;
begin
  asm @Result = outerHeight; end;
end;

function screen_var: JScreen;
begin
  asm @Result = screen; end;
end;

function innerHeight_var: integer;
begin
  asm @Result = innerHeight; end;
end;

function screenX_var: integer;
begin
  asm @Result = screenX; end;
end;

function styleMedia_var: JStyleMedia;
begin
  asm @Result = styleMedia; end;
end;

function document_var: JDocument;
begin
  asm @Result = document; end;
end;

function localStorage_var: JStorage;
begin
  asm @Result = localStorage; end;
end;

function sessionStorage_var: JStorage;
begin
  asm @Result = sessionStorage; end;
end;

function MSGestureEvent_var: JMSGestureEvent1;
begin
  asm @Result = MSGestureEvent; end;
end;

function ErrorEvent_var: JErrorEvent1;
begin
  asm @Result = ErrorEvent; end;
end;

function SVGFilterElement_var: JSVGFilterElement1;
begin
  asm @Result = SVGFilterElement; end;
end;

function TrackEvent_var: JTrackEvent1;
begin
  asm @Result = TrackEvent; end;
end;

function SVGFEMergeNodeElement_var: JSVGFEMergeNodeElement1;
begin
  asm @Result = SVGFEMergeNodeElement; end;
end;

function SVGFEFloodElement_var: JSVGFEFloodElement1;
begin
  asm @Result = SVGFEFloodElement; end;
end;

function MSElementExtensions_var: JMSElementExtensions2;
begin
  asm @Result = MSElementExtensions; end;
end;

function MSGesture_var: JMSGesture1;
begin
  asm @Result = MSGesture; end;
end;

function TextTrackCue_var: JTextTrackCue1;
begin
  asm @Result = TextTrackCue; end;
end;

function MSHTMLDocumentViewExtensions_var: JMSHTMLDocumentViewExtensions2;
begin
  asm @Result = MSHTMLDocumentViewExtensions; end;
end;

function MSStreamReader_var: JMSStreamReader1;
begin
  asm @Result = MSStreamReader; end;
end;

function DOMTokenList_var: JDOMTokenList1;
begin
  asm @Result = DOMTokenList; end;
end;

function SVGFEFuncAElement_var: JSVGFEFuncAElement1;
begin
  asm @Result = SVGFEFuncAElement; end;
end;

function SVGFETileElement_var: JSVGFETileElement1;
begin
  asm @Result = SVGFETileElement; end;
end;

function SVGFEBlendElement_var: JSVGFEBlendElement1;
begin
  asm @Result = SVGFEBlendElement; end;
end;

function WindowTimers_var: JWindowTimers2;
begin
  asm @Result = WindowTimers; end;
end;

function MessageChannel_var: JMessageChannel1;
begin
  asm @Result = MessageChannel; end;
end;

function SVGFEMergeElement_var: JSVGFEMergeElement1;
begin
  asm @Result = SVGFEMergeElement; end;
end;

function TransitionEvent_var: JTransitionEvent1;
begin
  asm @Result = TransitionEvent; end;
end;

function MediaQueryList_var: JMediaQueryList1;
begin
  asm @Result = MediaQueryList; end;
end;

function DOMError_var: JDOMError1;
begin
  asm @Result = DOMError; end;
end;

function SVGFEPointLightElement_var: JSVGFEPointLightElement1;
begin
  asm @Result = SVGFEPointLightElement; end;
end;

function CloseEvent_var: JCloseEvent1;
begin
  asm @Result = CloseEvent; end;
end;

function WebSocket_var: JWebSocket1;
begin
  asm @Result = WebSocket; end;
end;

function ProgressEvent_var: JProgressEvent1;
begin
  asm @Result = ProgressEvent; end;
end;

function IDBObjectStore_var: JIDBObjectStore1;
begin
  asm @Result = IDBObjectStore; end;
end;

function SVGFEGaussianBlurElement_var: JSVGFEGaussianBlurElement1;
begin
  asm @Result = SVGFEGaussianBlurElement; end;
end;

function MSHTMLDocumentExtensions_var: JMSHTMLDocumentExtensions2;
begin
  asm @Result = MSHTMLDocumentExtensions; end;
end;

function IDBVersionChangeEvent_var: JIDBVersionChangeEvent1;
begin
  asm @Result = IDBVersionChangeEvent; end;
end;

function IDBIndex_var: JIDBIndex1;
begin
  asm @Result = IDBIndex; end;
end;

function FileList_var: JFileList1;
begin
  asm @Result = FileList; end;
end;

function IDBCursor_var: JIDBCursor1;
begin
  asm @Result = IDBCursor; end;
end;

function SVGFESpecularLightingElement_var: JSVGFESpecularLightingElement1;
begin
  asm @Result = SVGFESpecularLightingElement; end;
end;

function File_var: JFile1;
begin
  asm @Result = File; end;
end;

function URL_var: JURL;
begin
  asm @Result = URL; end;
end;

function IDBCursorWithValue_var: JIDBCursorWithValue1;
begin
  asm @Result = IDBCursorWithValue; end;
end;

function XMLHttpRequestEventTarget_var: JXMLHttpRequestEventTarget1;
begin
  asm @Result = XMLHttpRequestEventTarget; end;
end;

function AudioTrackList_var: JAudioTrackList1;
begin
  asm @Result = AudioTrackList; end;
end;

function MSProtocol_var: JMSProtocol1;
begin
  asm @Result = MSProtocol; end;
end;

function SVGFEMorphologyElement_var: JSVGFEMorphologyElement1;
begin
  asm @Result = SVGFEMorphologyElement; end;
end;

function SVGFEFuncRElement_var: JSVGFEFuncRElement1;
begin
  asm @Result = SVGFEFuncRElement; end;
end;

function SVGFEDisplacementMapElement_var: JSVGFEDisplacementMapElement1;
begin
  asm @Result = SVGFEDisplacementMapElement; end;
end;

function AnimationEvent_var: JAnimationEvent1;
begin
  asm @Result = AnimationEvent; end;
end;

function SVGComponentTransferFunctionElement_var: JSVGComponentTransferFunctionElement1;
begin
  asm @Result = SVGComponentTransferFunctionElement; end;
end;

function MSRangeCollection_var: JMSRangeCollection1;
begin
  asm @Result = MSRangeCollection; end;
end;

function MSHTMLElementExtensions_var: JMSHTMLElementExtensions2;
begin
  asm @Result = MSHTMLElementExtensions; end;
end;

function SVGFEDistantLightElement_var: JSVGFEDistantLightElement1;
begin
  asm @Result = SVGFEDistantLightElement; end;
end;

function SVGFEFuncBElement_var: JSVGFEFuncBElement1;
begin
  asm @Result = SVGFEFuncBElement; end;
end;

function IDBKeyRange_var: JIDBKeyRange1;
begin
  asm @Result = IDBKeyRange; end;
end;

function SVG1_1Properties_var: JSVG1_1Properties2;
begin
  asm @Result = SVG1_1Properties; end;
end;

function IDBTransaction_var: JIDBTransaction1;
begin
  asm @Result = IDBTransaction; end;
end;

function MSWindowExtensions_var: JMSWindowExtensions2;
begin
  asm @Result = MSWindowExtensions; end;
end;

function AudioTrack_var: JAudioTrack1;
begin
  asm @Result = AudioTrack; end;
end;

function SVGFEConvolveMatrixElement_var: JSVGFEConvolveMatrixElement1;
begin
  asm @Result = SVGFEConvolveMatrixElement; end;
end;

function TextTrackCueList_var: JTextTrackCueList1;
begin
  asm @Result = TextTrackCueList; end;
end;

function CSSKeyframesRule_var: JCSSKeyframesRule1;
begin
  asm @Result = CSSKeyframesRule; end;
end;

function SVGFETurbulenceElement_var: JSVGFETurbulenceElement1;
begin
  asm @Result = SVGFETurbulenceElement; end;
end;

function TextTrackList_var: JTextTrackList1;
begin
  asm @Result = TextTrackList; end;
end;

function SVGFEFuncGElement_var: JSVGFEFuncGElement1;
begin
  asm @Result = SVGFEFuncGElement; end;
end;

function SVGFEColorMatrixElement_var: JSVGFEColorMatrixElement1;
begin
  asm @Result = SVGFEColorMatrixElement; end;
end;

function Console_var: JConsole1;
begin
  asm @Result = Console; end;
end;

function SVGFESpotLightElement_var: JSVGFESpotLightElement1;
begin
  asm @Result = SVGFESpotLightElement; end;
end;

function IDBDatabase_var: JIDBDatabase1;
begin
  asm @Result = IDBDatabase; end;
end;

function MSProtocolsCollection_var: JMSProtocolsCollection1;
begin
  asm @Result = MSProtocolsCollection; end;
end;

function DOMStringList_var: JDOMStringList1;
begin
  asm @Result = DOMStringList; end;
end;

function IDBOpenDBRequest_var: JIDBOpenDBRequest1;
begin
  asm @Result = IDBOpenDBRequest; end;
end;

function HTMLProgressElement_var: JHTMLProgressElement1;
begin
  asm @Result = HTMLProgressElement; end;
end;

function SVGFEOffsetElement_var: JSVGFEOffsetElement1;
begin
  asm @Result = SVGFEOffsetElement; end;
end;

function TextTrack_var: JTextTrack1;
begin
  asm @Result = TextTrack; end;
end;

function IDBRequest_var: JIDBRequest1;
begin
  asm @Result = IDBRequest; end;
end;

function MessagePort_var: JMessagePort1;
begin
  asm @Result = MessagePort; end;
end;

function FileReader_var: JFileReader1;
begin
  asm @Result = FileReader; end;
end;

function Blob_var: JBlob1;
begin
  asm @Result = Blob; end;
end;

function ApplicationCache_var: JApplicationCache1;
begin
  asm @Result = ApplicationCache; end;
end;

function PopStateEvent_var: JPopStateEvent1;
begin
  asm @Result = PopStateEvent; end;
end;

function CSSKeyframeRule_var: JCSSKeyframeRule1;
begin
  asm @Result = CSSKeyframeRule; end;
end;

function MSStream_var: JMSStream1;
begin
  asm @Result = MSStream; end;
end;

function MSBlobBuilder_var: JMSBlobBuilder1;
begin
  asm @Result = MSBlobBuilder; end;
end;

function DOMSettableTokenList_var: JDOMSettableTokenList1;
begin
  asm @Result = DOMSettableTokenList; end;
end;

function IDBFactory_var: JIDBFactory1;
begin
  asm @Result = IDBFactory; end;
end;

function MSPointerEvent_var: JMSPointerEvent1;
begin
  asm @Result = MSPointerEvent; end;
end;

function MSManipulationEvent_var: JMSManipulationEvent1;
begin
  asm @Result = MSManipulationEvent; end;
end;

function FormData_var: JFormData1;
begin
  asm @Result = FormData; end;
end;

function MSHTMLImageElementExtensions_var: JMSHTMLImageElementExtensions2;
begin
  asm @Result = MSHTMLImageElementExtensions; end;
end;

function SVGFEImageElement_var: JSVGFEImageElement1;
begin
  asm @Result = SVGFEImageElement; end;
end;

function HTMLDataListElement_var: JHTMLDataListElement1;
begin
  asm @Result = HTMLDataListElement; end;
end;

function SVGFECompositeElement_var: JSVGFECompositeElement1;
begin
  asm @Result = SVGFECompositeElement; end;
end;

function ValidityState_var: JValidityState1;
begin
  asm @Result = ValidityState; end;
end;

function HTMLTrackElement_var: JHTMLTrackElement1;
begin
  asm @Result = HTMLTrackElement; end;
end;

function MSApp_var: JMSApp;
begin
  asm @Result = MSApp; end;
end;

function MSXMLHttpRequestExtensions_var: JMSXMLHttpRequestExtensions2;
begin
  asm @Result = MSXMLHttpRequestExtensions; end;
end;

function SVGFEDiffuseLightingElement_var: JSVGFEDiffuseLightingElement1;
begin
  asm @Result = SVGFEDiffuseLightingElement; end;
end;

function SVGFEComponentTransferElement_var: JSVGFEComponentTransferElement1;
begin
  asm @Result = SVGFEComponentTransferElement; end;
end;

function MSCSSMatrix_var: JMSCSSMatrix1;
begin
  asm @Result = MSCSSMatrix; end;
end;

function Worker_var: JWorker1;
begin
  asm @Result = Worker; end;
end;

function MSNavigatorAbilities_var: JMSNavigatorAbilities2;
begin
  asm @Result = MSNavigatorAbilities; end;
end;

function onpopstate_var: Tonpopstate_;
begin
  asm @Result = onpopstate; end;
end;

function applicationCache_var: JApplicationCache;
begin
  asm @Result = applicationCache; end;
end;

function animationStartTime_var: integer;
begin
  asm @Result = animationStartTime; end;
end;

function msAnimationStartTime_var: integer;
begin
  asm @Result = msAnimationStartTime; end;
end;

function msIndexedDB_var: JIDBFactory;
begin
  asm @Result = msIndexedDB; end;
end;

function indexedDB_var: JIDBFactory;
begin
  asm @Result = indexedDB; end;
end;

function console_var: JConsole;
begin
  asm @Result = console; end;
end;

function ActiveXObject_var: JActiveXObject;
begin
  asm @Result = ActiveXObject; end;
end;

function WScript_var: JWScript;
begin
  asm @Result = WScript; end;
end;

end.

