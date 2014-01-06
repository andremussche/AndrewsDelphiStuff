
var TObject={
	$ClassName: "TObject",
	$Parent: null,
	ClassName: function (s) { return s.$ClassName },
	ClassType: function (s) { return s },
	ClassParent: function (s) { return s.$Parent },
	$Init: function () {},
	Create: function (s) { return s },
	Destroy: function (s) { for (var prop in s) if (s.hasOwnProperty(prop)) delete s.prop },
	Destroy$: function(s) { return s.ClassType.Destroy(s) },
	Free: function (s) { if (s!==null) s.ClassType.Destroy(s) }
};
var Exception={
	$ClassName: "Exception",
	$Parent: TObject,
	$Init: function () { FMessage="" },
	Create: function (s,Msg) { s.FMessage=Msg; return s }
};
function $W(e) { return e.ClassType?e:Exception.Create($New(Exception),e.constructor.name+", "+e.message) };
function $New(c) { var i={ClassType:c}; c.$Init(i); return i };
function $Is(o,c) {
	if (o===null) return false;
	return $Inh(o.ClassType,c);
}
;
function $Inh(s,c) {
	if (s===null) return false;
	while ((s)&&(s!==c)) s=s.$Parent;
	return (s)?true:false;
}
;
function $CheckFunc(i,z) { if (i) return i; throw Exception.Create($New(Exception),"Function pointer is nil"+z); };
function $Check(i,z) { if (i) return i; throw Exception.Create($New(Exception),"Object not instantiated"+z); };
/// TServer = class (TObject)
///  [line: 6, column: 3, file: SmartServerMain]
var TServer = {
   $ClassName:"TServer",
   $Parent:TObject
   ,$Init:function ($) {
      TObject.$Init($);
   }
   /// procedure TServer.Run()
   ///  [line: 20, column: 19, file: SmartServerMain]
   ,Run:function(Self) {
      var fileserver = null;
      var server = null;
      var port$2 = 0;
      var value = 0;
      var io = null;
      fileserver = TNodeStaticServer.Create$3(TNodeStaticServer,"./public",null);
      server = http().createServer(function (request$1, response) {
         Console().log("http request: "+$Check(request$1," in  [line: 28, column: 46, file: SmartServerMain]").url);
         if ($Check(request$1," in  [line: 29, column: 18, file: SmartServerMain]").url=="/") {
            response.end("Hello World!");
         } else {
            fileserver.serve(request$1,response);
         }
      });
      port$2 = 80;
      if ($Check(Process()," in TServer.Run [line: 36, column: 14, file: SmartServerMain]").env.PORT>0) {
         port$2 = parseInt($Check(Process()," in TServer.Run [line: 37, column: 21, file: SmartServerMain]").env.PORT,10);
      }
      server.listen(port$2,"");
      Console().log("Server running at http://127.0.0.1:"+port$2.toString());
      value = 0;
      io = socketio().listen(server);
      $Check(io," in TServer.Run [line: 43, column: 13, file: SmartServerMain]").sockets.on("connection",function (socket$1) {
         socket$1.emit("dataPushed",["test"].slice());
         socket$1.on("requestFromClient",function (data, callback) {
            Console().log("Received from client: "+data);
            value+=parseInt(data,10);
            if (value>100) {
               value = 0;
            }
            $CheckFunc(callback," in  [line: 53, column: 19, file: SmartServerMain]")(value);
            $Check(io," in  [line: 55, column: 21, file: SmartServerMain]").sockets.emit("dataFromServer",[value].slice());
         });
      });
   }
   ,Destroy:TObject.Destroy
};
/// TNodeProcess_memoryUsage_result_object = class (TObject)
///  [line: 238, column: 3, file: NodeJS.Core]
var TNodeProcess_memoryUsage_result_object = {
   $ClassName:"TNodeProcess_memoryUsage_result_object",
   $Parent:TObject
   ,$Init:function ($) {
      TObject.$Init($);
   }
   ,Destroy:TObject.Destroy
};
/// TEventEmitter_listeners_result_object = class (TObject)
///  [line: 127, column: 3, file: NodeJS.Core]
var TEventEmitter_listeners_result_object = {
   $ClassName:"TEventEmitter_listeners_result_object",
   $Parent:TObject
   ,$Init:function ($) {
      TObject.$Init($);
   }
   ,Destroy:TObject.Destroy
};
function Process() {
   var Result = null;
    Result = process; return Result
};
function Console() {
   var Result = null;
    Result = console; return Result
};
function http() {
   return require("http");
};
/// TNodeEventEmitter_listeners_result_object = class (TObject)
///  [line: 25, column: 3, file: NodeJS.events]
var TNodeEventEmitter_listeners_result_object = {
   $ClassName:"TNodeEventEmitter_listeners_result_object",
   $Parent:TObject
   ,$Init:function ($) {
      TObject.$Init($);
   }
   ,Destroy:TObject.Destroy
};
/// TNodeSocket_address_result_object = class (TObject)
///  [line: 43, column: 3, file: NodeJS.net]
var TNodeSocket_address_result_object = {
   $ClassName:"TNodeSocket_address_result_object",
   $Parent:TObject
   ,$Init:function ($) {
      TObject.$Init($);
   }
   ,Destroy:TObject.Destroy
};
/// TReadableStream_pipe_options_object_stream = class (TObject)
///  [line: 35, column: 3, file: NodeJS.stream]
var TReadableStream_pipe_options_object_stream = {
   $ClassName:"TReadableStream_pipe_options_object_stream",
   $Parent:TObject
   ,$Init:function ($) {
      TObject.$Init($);
   }
   ,Destroy:TObject.Destroy
};
/// TNodeStaticServer = class (TObject)
///  [line: 15, column: 3, file: Node_Static]
var TNodeStaticServer = {
   $ClassName:"TNodeStaticServer",
   $Parent:TObject
   ,$Init:function ($) {
      TObject.$Init($);
   }
   /// function TNodeStaticServer.Create(aPath: String; aOptions: TNodeStaticOptions = nil) : JNodeStaticServer
   ///  [line: 31, column: 34, file: Node_Static]
   ,Create$3:function(Self, aPath, aOptions) {
      var Result = null;
      var nodestatic = undefined;
      var server$1 = undefined;
      nodestatic = require("node-static");
      server$1 = null;
      
    server$1 = new ((nodestatic).Server)(aPath, aOptions);
  Result = server$1;
      return Result
   }
   ,Destroy:TObject.Destroy
};
/// TNodeStaticOptions = class (TObject)
///  [line: 20, column: 3, file: Node_Static]
var TNodeStaticOptions = {
   $ClassName:"TNodeStaticOptions",
   $Parent:TObject
   ,$Init:function ($) {
      TObject.$Init($);
      $.cache$1 = 0;
   }
   ,Destroy:TObject.Destroy
};
function socketio() {
   return require("socket.io");
};
var Server = null;
var $Application = function() {
   try {
      Server = TObject.Create($New(TServer));
      TServer.Run(Server);
   } catch ($e) {
      var e = $W($e);
       console.error(e)    }
}
$Application();

