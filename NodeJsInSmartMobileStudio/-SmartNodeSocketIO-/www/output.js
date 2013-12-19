
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
   ///  [line: 19, column: 19, file: SmartServerMain]
   ,Run:function(Self) {
      var server = null;
      var value = 0;
      var io = null;
      server = http().createServer(function (request$1, response) {
         response.end("Hello World!");
      });
      server.listen(80,"");
      Console().log("Server running at http://127.0.0.1:80/");
      value = 0;
      io = socketio().listen(server);
      $Check(io," in TServer.Run [line: 32, column: 13, file: SmartServerMain]").sockets.on("connection",function (socket$1) {
         socket$1.emit("dataPushed",["test"].slice());
         socket$1.on("requestFromClient",function (data, callback) {
            Console().log("Received from client: "+data);
            value+=parseInt(data,10);
            if (value>100) {
               value = 0;
            }
            $CheckFunc(callback," in  [line: 42, column: 19, file: SmartServerMain]")(value);
            $Check(io," in  [line: 44, column: 21, file: SmartServerMain]").sockets.emit("dataFromServer",[value].slice());
         });
      });
   }
   ,Destroy:TObject.Destroy
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

