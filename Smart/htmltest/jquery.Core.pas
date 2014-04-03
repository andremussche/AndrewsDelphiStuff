unit jquery.Core;

interface

type
  JJQueryXHR = class;
  JJQuery = class;

  JXMLHttpRequest = class external
  end;

  JHTMLElement = class external
  end;

  JEvent = class external
  end;

  JElement = class external
  end;

  JNode = JElement;

  JPopStateEvent = class external
  end;

  JFunction = procedure;

type
  TJQueryAjaxSettings_contents_object_ = class;
  TJQueryAjaxSettings_converters_object_ = class;
  TJQueryAjaxSettings_headers_object_ = class;
  TJQueryAjaxSettings_statusCode_object_ = class;
  TJQueryAjaxSettings_xhrFields_object_ = class;
type
  TJQueryAjaxSettings_contents_object_ = class
  public
//    function  GetItems(key: string): variant; external array;
//    procedure SetItems(key: string; value: variant); external array;
//    property Items[key: string]: variant read GetItems write SetItems; external; default;
  end;

  TJQueryAjaxSettings_converters_object_ = class external
  public
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
  end;

  TJQueryAjaxSettings_headers_object_ = class external
  public
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
  end;

  TJQueryAjaxSettings_statusCode_object_ = class external
  public
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
  end;

  TJQueryAjaxSettings_xhrFields_object_ = class external
  public
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
  end;

  JJQueryAjaxSettings = class external "Object"
    property accepts: variant;
    property async: boolean;
    procedure beforeSend(jqXHR: JJQueryXHR; settings: JJQueryAjaxSettings);
    property cache: boolean;
    procedure complete(jqXHR: JJQueryXHR; textStatus: string);
    property contents: TJQueryAjaxSettings_contents_object_;
    property contentType: string;
    property context: variant;
    property converters: TJQueryAjaxSettings_converters_object_;
    property crossDomain: boolean;
    property data: variant;
    function dataFilter(data: variant; ty: variant): variant;
    property dataType: string;
    function error(jqXHR: JJQueryXHR; textStatus: string; errorThrow: string): variant;
    property global: boolean;
    property headers: TJQueryAjaxSettings_headers_object_;
    property ifModified: boolean;
    property isLocal: boolean;
    property jsonp: string;
    property jsonpCallback: variant;
    property mimeType: string;
    property password: string;
    property processData: boolean;
    property scriptCharset: string;
    property statusCode: TJQueryAjaxSettings_statusCode_object_;
    procedure success(data: variant; textStatus: string; jqXHR: JJQueryXHR);
    property timeout: integer;
    property traditional: boolean;
    property &type: string;
    property url: string;
    property username: string;
    property xhr: variant;
    property xhrFields: TJQueryAjaxSettings_xhrFields_object_;
  end;

type
  JJQueryXHR = class external "Object"(JXMLHttpRequest)//, JJQueryPromise)
    procedure overrideMimeType(mimeType: string);
    procedure abort(statusText: string = "");
  end;

type
  JJQueryCallback = class external "Object"
    function add({many?}callbacks: array of variant): variant;
    function disable(): variant;
    function empty(): variant;
    function fire({many?}arguments: array of variant): variant;
    function fired(): boolean;
    function fireWith(context: variant; {many?}args: array of variant): variant;
    function has(callback: variant): boolean;
    function lock(): variant;
    function locked(): boolean;
    function remove({many?}callbacks: array of variant): variant;
  end;

type
  TJQueryPromise_pipe_doneFilter_ = function (x: variant): variant;
  TJQueryPromise_pipe_failFilter_ = function (x: variant): variant;
  TJQueryPromise_pipe_progressFilter_ = function (x: variant): variant;
type
  JJQueryPromise = class external "Object"
    function always({many?}alwaysCallbacks: array of variant): JJQueryPromise;
    function done({many?}doneCallbacks: array of variant): JJQueryPromise;
    function fail({many?}failCallbacks: array of variant): JJQueryPromise;
    function progress({many?}progressCallbacks: array of variant): JJQueryPromise;
    function state(): string;
    function pipe(doneFilter: TJQueryPromise_pipe_doneFilter_ = nil; failFilter: TJQueryPromise_pipe_failFilter_ = nil; progressFilter: TJQueryPromise_pipe_progressFilter_ = nil): JJQueryPromise;
    function &then(doneCallbacks: variant; failCallbacks: variant = undefined; progressCallbacks: variant = undefined): JJQueryPromise;
    function promise(target: Variant): JJQueryPromise;
  end;

type
  JJQueryDeferred = class external "Object"(JJQueryPromise)
    function notify({many?}args: array of variant): JJQueryDeferred;
    function notifyWith(context: variant; {many?}args: array of variant): JJQueryDeferred;
    function reject({many?}args: array of variant): JJQueryDeferred;
    function rejectWith(context: variant; {many?}args: array of variant): JJQueryDeferred;
    function resolve({many?}args: array of variant): JJQueryDeferred;
    function resolveWith(context: variant; {many?}args: array of variant): JJQueryDeferred;
  end;

type
  JBaseJQueryEventObject = class external "Object"(JEvent)
    property data: variant;
    property delegateTarget: JElement;
    function isDefaultPrevented(): boolean;
    function isImmediatePropogationStopped(): boolean;
    function isPropagationStopped(): boolean;
    property namespace: string;
    function preventDefault(): variant;
    property relatedTarget: JElement;
    property result: variant;
    procedure stopImmediatePropagation();
    procedure stopPropagation();
    property pageX: integer;
    property pageY: integer;
    property which: integer;
    property metaKey: variant;
  end;

type
  JJQueryInputEventObject = class external "Object"(JBaseJQueryEventObject)
    property altKey: boolean;
    property ctrlKey: boolean;
    property metaKey: boolean;
    property shiftKey: boolean;
  end;

type
  JJQueryMouseEventObject = class external "Object"(JJQueryInputEventObject)
    property button: integer;
    property clientX: integer;
    property clientY: integer;
    property offsetX: integer;
    property offsetY: integer;
    property pageX: integer;
    property pageY: integer;
    property screenX: integer;
    property screenY: integer;
  end;

type
  JJQueryKeyEventObject = class external "Object"(JJQueryInputEventObject)
    property char: variant;
    property charCode: integer;
    property key: variant;
    property keyCode: integer;
  end;

type
  JJQueryPopStateEventObject = class external "Object"(JBaseJQueryEventObject)
    property originalEvent: JPopStateEvent;
  end;

type
  JJQueryEventObject = class external "Object"(JBaseJQueryEventObject)//, JJQueryInputEventObject)//, JJQueryMouseEventObject)//, JJQueryKeyEventObject)//, JJQueryPopStateEventObject)
  end;

type
  JJQuerySupport = class external "Object"
    property ajax: boolean;
    property boxModel: boolean;
    property changeBubbles: boolean;
    property checkClone: boolean;
    property checkOn: boolean;
    property cors: boolean;
    property cssFloat: boolean;
    property hrefNormalized: boolean;
    property htmlSerialize: boolean;
    property leadingWhitespace: boolean;
    property noCloneChecked: boolean;
    property noCloneEvent: boolean;
    property opacity: boolean;
    property optDisabled: boolean;
    property optSelected: boolean;
    function scriptEval(): boolean;
    property style: boolean;
    property submitBubbles: boolean;
    property tbody: boolean;
  end;

type
  JJQueryParam = class external "Object"
  end;


function JQueryParam_(obj: variant): string; overload; external;
function JQueryParam_(obj: variant; traditional: boolean): string; overload; external;

type
  TJQueryStatic_ajaxPrefilter_handler_ = function (opts: variant; originalOpts: variant; jqXHR: JJQueryXHR): variant;
  TJQueryStatic_ajaxPrefilter2_handler_ = function (opts: variant; originalOpts: variant; jqXHR: JJQueryXHR): variant;
  TJQueryStatic_JQueryStatic__object__object_ = class;
  TJQueryStatic_cssHooks_object_ = class;
  TJQueryStatic_fx_object_ = class;
  TJQueryStatic_fx_object_tick_ = procedure ();
  TJQueryStatic_fx_object_stop_ = procedure ();
  TJQueryStatic_fx_object_speeds_object_ = class;
  TJQueryStatic_proxy_fn_ = function ({many?}args: array of variant): variant;
  TJQueryStatic_Deferred_object_ = class;
  TJQueryStatic_Deferred_object_JQueryStatic_Deferred_object__fn_ = function (d: JJQueryDeferred): variant;
  TJQueryStatic_Deferred_object_new_fn_ = function (d: JJQueryDeferred): variant;
  TJQueryStatic_Event_object_ = class;
  TJQueryStatic_each_callback_ = function (indexInArray: variant; valueOfElement: variant): variant;
  TJQueryStatic_each2_callback_ = function (indexInArray: variant; valueOfElement: variant): variant;
  TJQueryStatic_each3_callback_ = function (indexInArray: integer; valueOfElement: JHTMLElement): variant;
  TJQueryStatic_each4_callback_ = function (indexInArray: integer; valueOfElement: string): variant;
  TJQueryStatic_each5_callback_ = function (indexInArray: integer; valueOfElement: integer): variant;
  TJQueryStatic_map_callback_ = function (elementOfArray: variant; indexInArray: variant): variant;
  TJQueryStatic_map2_callback_ = function (elementOfArray: variant; indexInArray: variant): variant;
type
  TJQueryStatic_JQueryStatic__object__object_ = class 
  public
  end;

  TJQueryStatic_cssHooks_object_ = class external
  public
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
  end;

  TJQueryStatic_fx_object_speeds_object_ = class 
  public
    property slow: integer;
    property fast: integer;
  end;

  TJQueryStatic_fx_object_ = class 
  public
    property tick: TJQueryStatic_fx_object_tick_;
    property interval: integer;
    property stop: TJQueryStatic_fx_object_stop_;
    property speeds: TJQueryStatic_fx_object_speeds_object_;
    property off: boolean;
    property step: variant;
  end;

  TJQueryStatic_Deferred_object_ = class external
  public
    function &new(fn: TJQueryStatic_Deferred_object_new_fn_ = nil): JJQueryDeferred;
  end;

  TJQueryStatic_Event_object_ = class external
  public
    function &new(name: string; eventProperties: variant = undefined): JJQueryEventObject;
  end;

  JJQueryStatic = class external "Object"
    function ajax(settings: JJQueryAjaxSettings): JJQueryXHR;
    function ajax(url: string; settings: JJQueryAjaxSettings = nil): JJQueryXHR;overload;
    function ajaxPrefilter(dataTypes: string; handler: TJQueryStatic_ajaxPrefilter_handler_): variant;
    function ajaxPrefilter(handler: TJQueryStatic_ajaxPrefilter2_handler_): variant;overload;
    property ajaxSettings: JJQueryAjaxSettings;
    procedure ajaxSetup();
    procedure ajaxSetup(options: JJQueryAjaxSettings);overload;
    function get(url: string; data: variant = undefined; success: variant = undefined; dataType: variant = undefined): JJQueryXHR;
    function getJSON(url: string; data: variant = undefined; success: variant = undefined): JJQueryXHR;
    function getScript(url: string; success: variant = undefined): JJQueryXHR;
    property param: JJQueryParam;
    function post(url: string; data: variant = undefined; success: variant = undefined; dataType: variant = undefined): JJQueryXHR;
    function Callbacks(flags: string = ""): JJQueryCallback;
    function holdReady(hold: boolean): variant;
    function noConflict(removeAll: boolean = false): JObject;
    function when({many?}deferreds: array of variant): JJQueryPromise;
    procedure css(e: variant; propertyName: string; value: variant = undefined);
    procedure css(e: variant; propertyName: variant; value: variant = undefined);overload;
    property cssHooks: TJQueryStatic_cssHooks_object_;
    property cssNumber: variant;
    function data(element: JElement; key: string; value: variant): variant;
    function data(element: JElement; key: string): variant;overload;
    function data(element: JElement): variant;overload;
    function dequeue(element: JElement; queueName: string = ""): variant;
    function hasData(element: JElement): boolean;
    function queue(element: JElement; queueName: string = ""): array of variant;
    function queue(element: JElement; queueName: string; newQueueOrCallback: variant): JJQuery;overload;
    function removeData(element: JElement; name: string = ""): JJQuery;
    property fx: TJQueryStatic_fx_object_;
    function proxy(fn: TJQueryStatic_proxy_fn_; context: variant; {many?}args: array of variant): variant;
    function proxy(context: variant; name: string; {many?}args: array of variant): variant;overload;
    property Deferred: TJQueryStatic_Deferred_object_;
    property Event: TJQueryStatic_Event_object_;
    procedure error(message: variant);
    property expr: variant;
    property fn: variant;
    property isReady: boolean;
    property support: JJQuerySupport;
    function contains(container: JElement; contained: JElement): boolean;
    function each(collection: variant; callback: TJQueryStatic_each_callback_): variant;
    function each(collection: array of variant; callback: TJQueryStatic_each2_callback_): variant;overload;
    function each(collection: JJQuery; callback: TJQueryStatic_each2_callback_): variant;overload;
    function each(collection: array of string; callback: TJQueryStatic_each2_callback_): variant;overload;
    function each(collection: array of integer; callback: TJQueryStatic_each2_callback_): variant;overload;
    function extend(target: variant; {many?}objs: array of variant): JObject;
    function extend(deep: boolean; target: variant; {many?}objs: array of variant): JObject;overload;
    function globalEval(code: string): variant;
    function grep(&array: array of variant; func: variant; invert: boolean = false): array of variant;
    function inArray(value: variant; &array: array of variant; fromIndex: integer = 0): integer;
    function isArray(obj: variant): boolean;
    function isEmptyObject(obj: variant): boolean;
    function isFunction(obj: variant): boolean;
    function isNumeric(value: variant): boolean;
    function isPlainObject(obj: variant): boolean;
    function isWindow(obj: variant): boolean;
    function isXMLDoc(node: JNode): boolean;
    function makeArray(obj: variant): array of variant;
    function map(&array: array of variant; callback: TJQueryStatic_map_callback_): array of variant;
    function map(&array: variant; callback: TJQueryStatic_map2_callback_): variant;overload;
    function merge(first: array of variant; second: array of variant): array of variant;
    function noop(): variant;
    function now(): integer;
    function parseJSON(json: string): variant;
    function parseXML(data: string): variant;
    function queue(element: JElement; queueName: string; newQueue: array of variant): JJQuery;overload;
    function trim(str: string): string;
    function &type(obj: variant): string;
    function unique(arr: array of variant): array of variant;
    function parseHTML(data: string; context: JHTMLElement = nil; keepScripts: boolean = false): array of variant;
  end;


function JQueryStatic_(selector: string; context: variant = undefined): JJQuery;overload; external;
function JQueryStatic_(element: JElement): JJQuery; overload; external;
function JQueryStatic_(object_: TJQueryStatic_JQueryStatic__object__object_): JJQuery;overload; external;
function JQueryStatic_(elementArray: array of JElement): JJQuery;overload; external;
function JQueryStatic_(object_: JJQuery): JJQuery;overload; external;
function JQueryStatic_(func: JFunction): JJQuery;overload; external;
function JQueryStatic_(&array: array of variant): JJQuery;overload; external;
function JQueryStatic_(): JJQuery;overload; external;
function JQueryStatic_Deferred_object_(fn: TJQueryStatic_Deferred_object_JQueryStatic_Deferred_object__fn_ = nil): JJQueryDeferred;external;
function JQueryStatic_Event_object_(name: string; eventProperties: variant = undefined): JJQueryEventObject;external;

type
  TJQuery_ajaxError_handler_ = function (event: variant; jqXHR: variant; settings: variant; exception: variant): variant;
  TJQuery_ajaxSend_handler_ = function (event: variant; jqXHR: variant; settings: variant; exception: variant): variant;
  TJQuery_ajaxStart_handler_ = function (): variant;
  TJQuery_ajaxStop_handler_ = function (): variant;
  TJQuery_ajaxSuccess_handler_ = function (event: variant; jqXHR: variant; settings: variant; exception: variant): variant;
  TJQuery_addClass2_func_ = function (index: variant; currentClass: variant): string;
  TJQuery_attr2_map_object_ = class;
  TJQuery_attr2_func_ = function (index: variant; attr: variant): variant;
  TJQuery_html2_htmlContent_ = function (index: integer; oldhtml: string): string;
  TJQuery_prop2_func_ = function (index: variant; oldPropertyValue: variant): variant;
  TJQuery_removeClass2_func_ = function (index: variant; cls: variant): variant;
  TJQuery_toggleClass2_func_ = function (index: variant; cls: variant; swtch: variant): variant;
  TJQuery_val2_func_ = function (index: variant; value: variant): variant;
  TJQuery_height2_func_ = function (index: variant; height: variant): variant;
  TJQuery_offset_result_object_ = class;
  TJQuery_offset2_func_ = function (index: variant; coords: variant): variant;
  TJQuery_position_result_object_ = class;
  TJQuery_width2_func_ = function (index: variant; height: variant): variant;
  TJQuery_data2_obj_object_ = class;
  TJQuery_animate2_options_object_ = class;
  TJQuery_bind_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_blur_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_blur2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_change_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_change2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_click_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_click2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_dblclick_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_dblclick2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_delegate_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_focus_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_focus2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_focusin_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_focusin2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_focusout_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_focusout2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_hover_handlerIn_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_hover_handlerOut_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_hover2_handlerInOut_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_keydown_handler_ = function (eventObject: JJQueryKeyEventObject): variant;
  TJQuery_keydown2_handler_ = function (eventObject: JJQueryKeyEventObject): variant;
  TJQuery_keypress_handler_ = function (eventObject: JJQueryKeyEventObject): variant;
  TJQuery_keypress2_handler_ = function (eventObject: JJQueryKeyEventObject): variant;
  TJQuery_keyup_handler_ = function (eventObject: JJQueryKeyEventObject): variant;
  TJQuery_keyup2_handler_ = function (eventObject: JJQueryKeyEventObject): variant;
  TJQuery_load2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_mousedown2_handler_ = function (eventObject: JJQueryMouseEventObject): variant;
  TJQuery_mouseevent_handler_ = function (eventObject: JJQueryMouseEventObject): variant;
  TJQuery_mouseevent2_handler_ = function (eventObject: JJQueryMouseEventObject): variant;
  TJQuery_mouseenter2_handler_ = function (eventObject: JJQueryMouseEventObject): variant;
  TJQuery_mouseleave2_handler_ = function (eventObject: JJQueryMouseEventObject): variant;
  TJQuery_mousemove2_handler_ = function (eventObject: JJQueryMouseEventObject): variant;
  TJQuery_mouseout2_handler_ = function (eventObject: JJQueryMouseEventObject): variant;
  TJQuery_mouseover2_handler_ = function (eventObject: JJQueryMouseEventObject): variant;
  TJQuery_mouseup2_handler_ = function (eventObject: JJQueryMouseEventObject): variant;
  TJQuery_off_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_off2_eventsMap_object_ = class;
  TJQuery_on_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_on2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_on2_eventsMap_object_ = class;
  TJQuery_one_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_one2_eventsMap_object_ = class;
  TJQuery_resize_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_resize2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_scroll_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_scroll2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_select_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_select2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_submit_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_submit2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_unbind_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_undelegate2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_unload_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_unload2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_error_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_error2_handler_ = function (eventObject: JJQueryEventObject): variant;
  TJQuery_after2_func_ = function (index: variant): variant;
  TJQuery_append2_func_ = function (index: variant; html: variant): variant;
  TJQuery_before2_func_ = function (index: variant): variant;
  TJQuery_prepend2_func_ = function (index: variant; html: variant): variant;
  TJQuery_text2_textString_ = function (index: integer; text: string): string;
  TJQuery_wrap2_func_ = function (index: variant): variant;
  TJQuery_wrapInner2_func_ = function (index: variant): variant;
  TJQuery_each_func_ = function (index: variant; elem: JElement): variant;
  TJQuery_filter2_func_ = function (index: variant): variant;
  TJQuery_is2_func_ = function (index: variant): variant;
  TJQuery_map_callback_ = function (index: variant; domElement: JElement): variant;
  TJQuery_not2_func_ = function (index: variant): variant;
type
  TJQuery_attr2_map_object_ = class external
  public
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
  end;

  TJQuery_offset_result_object_ = class 
  public
    property left: integer;
    property top: integer;
  end;

  TJQuery_position_result_object_ = class 
  public
    property top: integer;
    property left: integer;
  end;

  TJQuery_data2_obj_object_ = class external
  public
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
  end;

  TJQuery_animate2_options_object_ = class 
  public
    property duration: variant;
    property easing: string;
    property complete: JFunction;
    property step: JFunction;
    property queue: boolean;
    property specialEasing: variant;
  end;

  TJQuery_off2_eventsMap_object_ = class external
  public
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
  end;

  TJQuery_on2_eventsMap_object_ = class external
  public
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
  end;

  TJQuery_one2_eventsMap_object_ = class external
  public
    function  GetItems(key: string): variant; external array;
    procedure SetItems(key: string; value: variant); external array;
    property Items[key: string]: variant read GetItems write SetItems; default;
  end;

  JJQuery = class external "Object"
    function ajaxComplete(handler: variant): JJQuery;
    function ajaxError(handler: TJQuery_ajaxError_handler_): JJQuery;
    function ajaxSend(handler: TJQuery_ajaxSend_handler_): JJQuery;
    function ajaxStart(handler: TJQuery_ajaxStart_handler_): JJQuery;
    function ajaxStop(handler: TJQuery_ajaxStop_handler_): JJQuery;
    function ajaxSuccess(handler: TJQuery_ajaxSuccess_handler_): JJQuery;
    function load(url: string; data: variant = undefined; complete: variant = undefined): JJQuery;
    function serialize(): string;
    function serializeArray(): array of variant;
    function addClass(classNames: string): JJQuery;
    function addClass(func: TJQuery_addClass2_func_): JJQuery;overload;
    function addBack(selector: string = ""): JJQuery;
    function attr(attributeName: string): string;
    function attr(attributeName: string; value: variant): JJQuery;overload;
    function attr(map: TJQuery_attr2_map_object_): JJQuery;overload;
    function attr(attributeName: string; func: TJQuery_attr2_func_): JJQuery;overload;
    function hasClass(className: string): boolean;
    function html(): string;
    function html(htmlString: string): JJQuery;overload;
    function html(htmlContent: TJQuery_html2_htmlContent_): JJQuery;overload;
    function html(JQuery: Variant): JJQuery;overload;
    function prop(propertyName: string): variant;
    function prop(propertyName: string; value: variant): JJQuery;overload;
    function prop(map: variant): JJQuery;overload;
    function prop(propertyName: string; func: TJQuery_prop2_func_): JJQuery;overload;
    function removeAttr(attributeName: variant): JJQuery;
    function removeClass(className: variant = undefined): JJQuery;
    function removeClass(func: TJQuery_removeClass2_func_): JJQuery;overload;
    function removeProp(propertyName: variant): JJQuery;
    function toggleClass(className: variant; swtch: boolean = false): JJQuery;
    function toggleClass(swtch: boolean = false): JJQuery;overload;
    function toggleClass(func: TJQuery_toggleClass2_func_): JJQuery;overload;
    function val(): variant;
    function val(value: array of string): JJQuery;overload;
    function val(value: string): JJQuery;overload;
    function val(value: integer): JJQuery;overload;
    function val(func: TJQuery_val2_func_): JJQuery;overload;
    function css(propertyName: string): string;
    function css(propertyNames: array of string): string;overload;
    function css(properties: variant): JJQuery;overload;
    function css(propertyName: string; value: variant): JJQuery;overload;
    function css(propertyName: variant; value: variant): JJQuery;overload;
    function height(): integer;
    function height(value: integer): JJQuery;overload;
    function height(value: string): JJQuery;overload;
    function height(func: TJQuery_height2_func_): JJQuery;overload;
    function innerHeight(): integer;
    function innerWidth(): integer;
    function offset(): TJQuery_offset_result_object_;
    function offset(coordinates: variant): JJQuery;overload;
    function offset(func: TJQuery_offset2_func_): JJQuery;overload;
    function outerHeight(includeMargin: boolean = false): integer;
    function outerWidth(includeMargin: boolean = false): integer;
    function position(): TJQuery_position_result_object_;
    function scrollLeft(): integer;
    function scrollLeft(value: integer): JJQuery;overload;
    function scrollTop(): integer;
    function scrollTop(value: integer): JJQuery;overload;
    function width(): integer;
    function width(value: integer): JJQuery;overload;
    function width(value: string): JJQuery;overload;
    function width(func: TJQuery_width2_func_): JJQuery;overload;
    function clearQueue(queueName: string = ""): JJQuery;
    function data(key: string; value: variant): JJQuery;
    function data(obj: TJQuery_data2_obj_object_): JJQuery;overload;
    function data(key: string = ""): variant;overload;
    function dequeue(queueName: string = ""): JJQuery;
    function removeData(nameOrList: variant = undefined): JJQuery;
    function promise(&type: variant = undefined; target: variant = undefined): JJQueryPromise;
    function animate(properties: variant; duration: variant = undefined; complete: JFunction = nil): JJQuery;
    function animate(properties: variant; duration: variant; easing: string; complete: JFunction = nil): JJQuery;overload;
    procedure animate(properties: variant; options: TJQuery_animate2_options_object_);overload;
    function delay(duration: integer; queueName: string = ""): JJQuery;
    function fadeIn(duration: variant = undefined; callback: variant = undefined): JJQuery;
    function fadeIn(duration: variant; easing: string; callback: variant = undefined): JJQuery;overload;
    function fadeOut(duration: variant = undefined; callback: variant = undefined): JJQuery;
    function fadeOut(duration: variant; easing: string; callback: variant = undefined): JJQuery;overload;
    function fadeTo(duration: variant; opacity: integer; callback: variant = undefined): JJQuery;
//    function fadeTo(duration: variant; opacity: integer; easing: string = ""; callback: variant = undefined): JJQuery;overload;
    function fadeToggle(duration: variant = undefined; callback: variant = undefined): JJQuery;
//    function fadeToggle(duration: variant = undefined; easing: string = ""; callback: variant = undefined): JJQuery;overload;
    function finish(): JJQuery;
    function hide(duration: variant = undefined; callback: variant = undefined): JJQuery;
//    function hide(duration: variant = undefined; easing: string = ""; callback: variant = undefined): JJQuery;overload;
    function show(duration: variant = undefined; callback: variant = undefined): JJQuery;
//    function show(duration: variant = undefined; easing: string = ""; callback: variant = undefined): JJQuery;overload;
    function slideDown(duration: variant = undefined; callback: variant = undefined): JJQuery;
//    function slideDown(duration: variant = undefined; easing: string = ""; callback: variant = undefined): JJQuery;overload;
    function slideToggle(duration: variant = undefined; callback: variant = undefined): JJQuery;
//    function slideToggle(duration: variant = undefined; easing: string = ""; callback: variant = undefined): JJQuery;overload;
    function slideUp(duration: variant = undefined; callback: variant = undefined): JJQuery;
//    function slideUp(duration: variant = undefined; easing: string = ""; callback: variant = undefined): JJQuery;overload;
    function stop(clearQueue: boolean = false; jumpToEnd: boolean = false): JJQuery;
//    function stop(queue: variant = undefined; clearQueue: boolean = false; jumpToEnd: boolean = false): JJQuery;overload;
    function toggle(duration: variant = undefined; callback: variant = undefined): JJQuery;
//    function toggle(duration: variant = undefined; easing: string = ""; callback: variant = undefined): JJQuery;overload;
    function toggle(showOrHide: boolean): JJQuery;overload;
    function bind(eventType: string; eventData: variant = undefined; handler: TJQuery_bind_handler_ = nil): JJQuery;
    function bind(eventType: string; eventData: variant; preventBubble: boolean): JJQuery;overload;
    function bind(eventType: string; preventBubble: boolean): JJQuery;overload;
    procedure bind({many?}events: array of variant);overload;
    function blur(eventData: variant = undefined; handler: TJQuery_blur_handler_ = nil): JJQuery;
    function blur(handler: TJQuery_blur2_handler_): JJQuery;overload;
    function change(eventData: variant = undefined; handler: TJQuery_change_handler_ = nil): JJQuery;
    function change(handler: TJQuery_change2_handler_): JJQuery;overload;
    //function click(eventData: variant; handler: TJQuery_click_handler_): JJQuery;
    function click(handler: TJQuery_click2_handler_): JJQuery;overload;
    function dblclick(eventData: variant = undefined; handler: TJQuery_dblclick_handler_ = nil): JJQuery;
    function dblclick(handler: TJQuery_dblclick2_handler_): JJQuery;overload;
    function delegate(selector: variant; eventType: string; handler: TJQuery_delegate_handler_): JJQuery;
    function focus(eventData: variant = undefined; handler: TJQuery_focus_handler_ = nil): JJQuery;
    function focus(handler: TJQuery_focus2_handler_): JJQuery;overload;
    function focusin(eventData: variant; handler: TJQuery_focusin_handler_): JJQuery;
    function focusin(handler: TJQuery_focusin2_handler_): JJQuery;overload;
    function focusout(eventData: variant; handler: TJQuery_focusout_handler_): JJQuery;
    function focusout(handler: TJQuery_focusout2_handler_): JJQuery;overload;
    function hover(handlerIn: TJQuery_hover_handlerIn_; handlerOut: TJQuery_hover_handlerOut_): JJQuery;
    function hover(handlerInOut: TJQuery_hover2_handlerInOut_): JJQuery;overload;
    function keydown(eventData: variant = undefined; handler: TJQuery_keydown_handler_ = nil): JJQuery;
    function keydown(handler: TJQuery_keydown2_handler_): JJQuery;overload;
    function keypress(eventData: variant = undefined; handler: TJQuery_keypress_handler_ = nil): JJQuery;
    function keypress(handler: TJQuery_keypress2_handler_): JJQuery;overload;
    function keyup(eventData: variant = undefined; handler: TJQuery_keyup_handler_ = nil): JJQuery;
    function keyup(handler: TJQuery_keyup2_handler_): JJQuery;overload;
    function load(eventData: variant = undefined; handler: TJQuery_load2_handler_ = nil): JJQuery;overload;
    function load(handler: TJQuery_load2_handler_): JJQuery;overload;
    function mousedown(): JJQuery;
    function mousedown(eventData: variant; handler: TJQuery_mousedown2_handler_): JJQuery;overload;
    function mousedown(handler: TJQuery_mousedown2_handler_): JJQuery;overload;
    function mouseevent(eventData: variant; handler: TJQuery_mouseevent_handler_): JJQuery;
    function mouseevent(handler: TJQuery_mouseevent2_handler_): JJQuery;overload;
    function mouseenter(): JJQuery;
    function mouseenter(eventData: variant; handler: TJQuery_mouseenter2_handler_): JJQuery;overload;
    function mouseenter(handler: TJQuery_mouseenter2_handler_): JJQuery;overload;
    function mouseleave(): JJQuery;
    function mouseleave(eventData: variant; handler: TJQuery_mouseleave2_handler_): JJQuery;overload;
    function mouseleave(handler: TJQuery_mouseleave2_handler_): JJQuery;overload;
    function mousemove(): JJQuery;
    function mousemove(eventData: variant; handler: TJQuery_mousemove2_handler_): JJQuery;overload;
    function mousemove(handler: TJQuery_mousemove2_handler_): JJQuery;overload;
    function mouseout(): JJQuery;
    function mouseout(eventData: variant; handler: TJQuery_mouseout2_handler_): JJQuery;overload;
    function mouseout(handler: TJQuery_mouseout2_handler_): JJQuery;overload;
    function mouseover(): JJQuery;
    function mouseover(eventData: variant; handler: TJQuery_mouseover2_handler_): JJQuery;overload;
    function mouseover(handler: TJQuery_mouseover2_handler_): JJQuery;overload;
    function mouseup(): JJQuery;
    function mouseup(eventData: variant; handler: TJQuery_mouseup2_handler_): JJQuery;overload;
    function mouseup(handler: TJQuery_mouseup2_handler_): JJQuery;overload;
    function off(events: string = ""; selector: variant = undefined; handler: TJQuery_off_handler_ = nil): JJQuery;
    function off(eventsMap: TJQuery_off2_eventsMap_object_; selector: variant = undefined): JJQuery;overload;
    function on(events: string; selector: variant = undefined; data: variant = undefined; handler: TJQuery_on_handler_ = nil): JJQuery;
//    function on(events: string; selector: variant = undefined; handler: TJQuery_on2_handler_ = nil): JJQuery;overload;
//    function on(eventsMap: TJQuery_on2_eventsMap_object_; selector: variant = undefined; data: variant = undefined): JJQuery;overload;
    function one(events: string; selector: variant = undefined; data: variant = undefined; handler: TJQuery_one_handler_ = nil): JJQuery;
    function one(eventsMap: TJQuery_one2_eventsMap_object_; selector: variant = undefined; data: variant = undefined): JJQuery;overload;
    function ready(handler: variant): JJQuery;
    function resize(eventData: variant = undefined; handler: TJQuery_resize_handler_ = nil): JJQuery;
    function resize(handler: TJQuery_resize2_handler_): JJQuery;overload;
    function scroll(eventData: variant = undefined; handler: TJQuery_scroll_handler_ = nil): JJQuery;
    function scroll(handler: TJQuery_scroll2_handler_): JJQuery;overload;
    function select(eventData: variant = undefined; handler: TJQuery_select_handler_ = nil): JJQuery;
    function select(handler: TJQuery_select2_handler_): JJQuery;overload;
    function submit(eventData: variant = undefined; handler: TJQuery_submit_handler_ = nil): JJQuery;
    function submit(handler: TJQuery_submit2_handler_): JJQuery;overload;
    function trigger(eventType: string; {many?}extraParameters: array of variant): JJQuery;
    function trigger(event: JJQueryEventObject): JJQuery;overload;
    function triggerHandler(eventType: string; {many?}extraParameters: array of variant): JObject;
    function unbind(eventType: string = ""; handler: TJQuery_unbind_handler_ = nil): JJQuery;
    function unbind(eventType: string; fls: boolean): JJQuery;overload;
    function unbind(evt: variant): JJQuery;overload;
    function undelegate(): JJQuery;
    function undelegate(selector: variant; eventType: string; handler: TJQuery_undelegate2_handler_ = nil): JJQuery;overload;
    function undelegate(selector: variant; events: variant): JJQuery;overload;
    function undelegate(namespace: string): JJQuery;overload;
    function unload(eventData: variant = undefined; handler: TJQuery_unload_handler_ = nil): JJQuery;
    function unload(handler: TJQuery_unload2_handler_): JJQuery;overload;
    property context: JElement;
    property jquery: string;
    function error(handler: TJQuery_error_handler_): JJQuery;
    function error(eventData: variant; handler: TJQuery_error2_handler_): JJQuery;overload;
    function pushStack(elements: array of variant): JJQuery;
    function pushStack(elements: array of variant; name: variant; arguments: variant): JJQuery;overload;
    function after({many?}content: array of variant): JJQuery;
    procedure after(func: TJQuery_after2_func_);overload;
    function append({many?}content: array of variant): JJQuery;
    procedure append(func: TJQuery_append2_func_);overload;
    function appendTo(target: variant): JJQuery;
    function before({many?}content: array of variant): JJQuery;
    procedure before(func: TJQuery_before2_func_);overload;
    function clone(withDataAndEvents: boolean = false; deepWithDataAndEvents: boolean = false): JJQuery;
    function detach(selector: variant = undefined): JJQuery;
    function empty(): JJQuery;
    function insertAfter(target: variant): JJQuery;
    function insertBefore(target: variant): JJQuery;
    function prepend({many?}content: array of variant): JJQuery;
    function prepend(func: TJQuery_prepend2_func_): JJQuery;overload;
    function prependTo(target: variant): JJQuery;
    function remove(selector: variant = undefined): JJQuery;
    function replaceAll(target: variant): JJQuery;
    function replaceWith(func: variant): JJQuery;
    function text(): string;
    function text(textString: variant): JJQuery;overload;
    function text(textString: TJQuery_text2_textString_): JJQuery;overload;
    function toArray(): array of variant;
    function unwrap(): JJQuery;
    function wrap(wrappingElement: variant): JJQuery;
    function wrap(func: TJQuery_wrap2_func_): JJQuery;overload;
    function wrapAll(wrappingElement: variant): JJQuery;
    function wrapInner(wrappingElement: variant): JJQuery;
    function wrapInner(func: TJQuery_wrapInner2_func_): JJQuery;overload;
    function each(func: TJQuery_each_func_): JJQuery;
    function get(index: integer = 0): variant;
    function index(): integer;
    function index(selector: string): integer;overload;
    function index(element: variant): integer;overload;
    property length: integer;
    property selector: string;
    function  GetItems(x: string): variant; overload; external array;
    procedure SetItems(x: string; value: variant); overload; external array;
    property Items[x: string]: variant read GetItems write SetItems; default;
    function  GetItems(x: integer): JHTMLElement; overload; external array;
    procedure SetItems(x: integer; value: JHTMLElement); overload; external array;
//    property Items[x: integer]: JHTMLElement read GetItems write SetItems; default;
    function add(selector: string; context: variant = undefined): JJQuery;
    function add({many?}elements: array of variant): JJQuery;overload;
//    function add(html: string): JJQuery;overload;
    function add(obj: JJQuery): JJQuery;overload;
    function children(selector: variant = undefined): JJQuery;
    function closest(selector: string): JJQuery;
//    function closest(selector: string; context: JElement = nil): JJQuery;overload;
    function closest(obj: JJQuery): JJQuery;overload;
    function closest(element: variant): JJQuery;overload;
//    function closest(selectors: variant; context: JElement = nil): array of variant;overload;
    function contents(): JJQuery;
    function &end(): JJQuery;
    function eq(index: integer): JJQuery;
    function filter(selector: string): JJQuery;
    function filter(func: TJQuery_filter2_func_): JJQuery;overload;
    function filter(element: variant): JJQuery;overload;
    function filter(obj: JJQuery): JJQuery;overload;
    function find(selector: string): JJQuery;
    function find(element: variant): JJQuery;overload;
    function find(obj: JJQuery): JJQuery;overload;
    function first(): JJQuery;
    function has(selector: string): JJQuery;
    function has(contained: JElement): JJQuery;overload;
    function &is(selector: string): boolean;
    function &is(func: TJQuery_is2_func_): boolean;overload;
    function &is(element: variant): boolean;overload;
    function &is(obj: JJQuery): boolean;overload;
    function last(): JJQuery;
    function map(callback: TJQuery_map_callback_): JJQuery;
    function next(selector: string = ""): JJQuery;
    function nextAll(selector: string = ""): JJQuery;
    function nextUntil(selector: string = ""; filter: string = ""): JJQuery;
//    function nextUntil(element: JElement = nil; filter: string = ""): JJQuery;overload;
    function &not(selector: string): JJQuery;
    function &not(func: TJQuery_not2_func_): JJQuery;overload;
    function &not(element: variant): JJQuery;overload;
    function &not(obj: JJQuery): JJQuery;overload;
    function offsetParent(): JJQuery;
    function parent(selector: string = ""): JJQuery;
    function parents(selector: string = ""): JJQuery;
    function parentsUntil(selector: string = ""; filter: string = ""): JJQuery;
//    function parentsUntil(element: JElement = nil; filter: string = ""): JJQuery;overload;
    function prev(selector: string = ""): JJQuery;
    function prevAll(selector: string = ""): JJQuery;
    function prevUntil(selector: string = ""; filter: string = ""): JJQuery;
//    function prevUntil(element: JElement = nil; filter: string = ""): JJQuery;overload;
    function siblings(selector: string = ""): JJQuery;
    function slice(start: integer; &end: integer = 0): JJQuery;
    function queue(queueName: string = ""): array of variant;
    function queue(queueName: string; newQueueOrCallback: variant): JJQuery;overload;
    function queue(newQueueOrCallback: variant): JJQuery;overload;
  end;

type
  JEventTarget = class external "Object"
  end;


function jQuery: JJQueryStatic; overload;
function JQuery(selector: string; context: variant = undefined): JJQuery;overload;
//function $_var: JJQueryStatic;


implementation

function jQuery: JJQueryStatic;
begin
  asm @Result = jQuery; end;
end;

function JQuery(selector: string; context: variant = undefined): JJQuery;overload;
begin
  asm @Result = jQuery(@selector, @context); end;
end;


//function $_var: JJQueryStatic;
//begin
//  asm @Result = $; end;
//end;

end.

