/** this code is copied (and modified) from:
* http://martinsikora.com/nodejs-and-websocket-simple-chat-tutorial
* Modified by: André Mussche, andre.mussche@gmail.com
* It is a PoC to see if RemObjects SDK for Javascript can be used on the server
* side (in Node.js) too.
* Note: The RemObjectsSDK.js file is appended at the end of this file
*/

// http://ejohn.org/blog/ecmascript-5-strict-mode-json-and-more/
//"use strict";  RemObjectsSDK.js is not "strict" save yet

// Optional. You will see this name in eg. 'ps' or 'top' command
process.title = 'node-remobjects';

// Port where we'll run the websocket server
var webSocketsServerPort = 7000;

// websocket and http servers
var webSocketServer = require('websocket').server;
var http = require('http');

/** HTTP server */
var server = http.createServer(function(request, response) {
    // Not important for us. We're writing WebSocket server, not HTTP server
});
server.listen(webSocketsServerPort, function() {
    console.log((new Date()) + " Server is listening on port " + webSocketsServerPort);
});

/** WebSocket server */
var wsServer = new webSocketServer({
    // WebSocket server is tied to a HTTP server. To be honest I don't understand why.
    httpServer: server
});

// This callback function is called every time someone
// tries to connect to the WebSocket server
wsServer.on('request', function(request) {
    console.log((new Date()) + ' Connection from origin ' + request.origin + '.');

    // accept connection - you should check 'request.origin' to make sure that
    // client is connecting from your website
    // (http://en.wikipedia.org/wiki/Same_origin_policy)
    var connection = request.accept(null, request.origin); 
    
    console.log((new Date()) + ' Connection accepted from ' + connection.remoteAddress);

    // user sent some message
    connection.on('message', function(msg) {
        if (msg.type === 'utf8') { // accept only text

          console.log((new Date()) + ' Received Message: ' + msg.utf8Data);
          try {
            
            var message = new RemObjects.SDK.JSONMessage();
            //load data
            message.setRequestStream(msg.utf8Data);
            
            //todo: an invoke JS file should be generated for the server side
            //so it automatically calls the right function, read params, etc
            //now it's done by hand (but can be easily generated)
            if (message.fRequestObject.method == "NewService.Sum") 
            {                        
                console.log((new Date()) + ' RO: ' + message.fRequestObject.method);

                var A = message.read("A", "Integer");
                var B = message.read("B", "Integer");                      
                //do the sum :)    
                var result = parseInt(A) + parseInt(B);
                
                //clear request
                message.fRequestObject.params = new Object();  
                //write result (note: in fRequestObject)
                message.write("Result", "Integer", result);
                
                //copy data to response
                message.fResponseObject.result = message.fRequestObject.params; 
                console.log((new Date()) + ' RO result: ' + JSON.stringify(message.fResponseObject) );
                //send data to client back
                connection.sendUTF( JSON.stringify(message.fResponseObject) );
            }
                                
          } catch (e) {
              throw new Error("Message processing error: " + e.message);
          };
        }
    });

    // user disconnected
    connection.on('close', function(connection) {
            console.log((new Date()) + " Peer "
                + connection.remoteAddress + " disconnected.");
        }
    );

});




/** RemObjectsSDK.js */ 
/** --------------------------------------------- */

//RemObjects SDK classes
//interface

var RemObjects = {};



RemObjects.SDK = {
    RTTI : {
    },

    Enum : {},

    ROComplexType : function ROComplexType() {
    },

    ROEnumType : function ROEnumType() {
    },

    ROStructType : function ROStructType() {
    },

    ROArrayType : function ROArrayType() {
        this.elementType = "";
        this.items = [];
    },

    ROException : function ROException(e) {
        if (e) {
            this.name = e.name;
            this.message = e.message;
        };
        this.fields = new RemObjects.SDK.ROStructType();
    },

    ClientChannel : function ClientChannel(aUrl) {
        this.url = aUrl;
        //post
    },

    HTTPClientChannel : function HTTPClientChannel(aUrl) {
        RemObjects.SDK.ClientChannel.call(this, aUrl);
    },

    WebSocketClientChannel : function WebSocketClientChannel(aUrl) {
        RemObjects.SDK.ClientChannel.call(this, aUrl);
    },

    Message : function Message() {
        this.fClientID = RemObjects.UTIL.NewGuid();
        this.fRequestObject = {};
        this.fResponseObject = {};
        //clone
    },

    JSONMessage : function JSONMessage() {
        RemObjects.SDK.Message.call(this);
        //initialize
        //finalize
        //write
        //read
        //requestStream
        //setResponseStream

    },

    BinMessage : function BinMessage() {
        RemObjects.SDK.Message.call(this);
        //initialize
        //finalize
        //write
        //read
        //requestStream
        //setResponseStream
    },

    BinHeader : function BinHeader() {


        this.fHeader = [0x52, 0x4f, 0x31, 0x30, 0x37];  //should contain 0x1c bytes
        for (var i = 5; i<0x1c; this.fHeader[i++] = 0);
        //readFrom
        //asStream
        //isValidHeader
        //getCompressed
        //setCompressed
        //getMessageType
        //setMessageType
        //setClientID

    // Header BINARY LAYOUT: 0x1C bytes
    //
    // Keep in sync with
    //  - Delphi - uROBINMessage.pas
    //  - C#     - BinMessage.cs
    //
    // 52 4f 31 30  = "RO10" basic RO signature for RO 1.0
    // XX YY ZZ --  = XX: subversion (currenly "7")
    //		 YY: option flags: 01 = compressed
    //		 ZZ: message type as defined in uROClientIntf
    //     --: reserved for future use
    // -- -- UU UU  = UU: user data (word)
    // CC CC CC CC    0x10 bytes ClientID (guid)
    // CC CC CC CC
    // CC CC CC CC
    // CC CC CC CC


    },


    RemoteService : function RemoteService(aChannel, aMessage, aServiceName) {
        this.fChannel = aChannel;
        this.fMessage = aMessage;
        this.fServiceName = aServiceName;
    },


    ROService : function ROService(aChannel, aMessage, aServiceName) {
        if (aChannel && !aMessage && !aServiceName) {
            if (aChannel instanceof RemObjects.SDK.RemoteService) { //first parameter actually contains RemoteService
                this.fChannel = aChannel.fChannel;
                this.fMessage = aChannel.fMessage;
                this.fServiceName = aChannel.fServiceName;
            }
            else {
                throw new Error("ROService constructor: When using single argument version of the constructor, type of the argument should be RemoteService");
            };
        }
        else {
            this.fChannel = aChannel;
            this.fMessage = aMessage;
            this.fServiceName = aServiceName;
        };
    }

};

RemObjects.UTIL = {

    testBrowser : function testBrowser() {
        var result = "";
        if (typeof(JSON) == 'undefined')
            result += "Browser doesn't support JSON\n";

        var AJAX;
        if (typeof(XMLHttpRequest) == 'undefined') {
            try {
                AJAX = new XMLHttpRequest();
            } catch (e) {
                try {
                    AJAX = new ActiveXObject("Msxml2.XMLHTTP");
                } catch (e) {
                    try {
                        AJAX = new ActiveXObject("Microsoft.XMLHTTP");
                    } catch (e) {
                        result += "Browser doesn't support XMLHttpRequest object\n";
                    };
                };
            };
        };

        result += RemObjects.UTIL.testBrowserBinary();
        return result;
    },

    testBrowserBinary : function testBrowserBinary() {
        var result = "";
        if (!(((typeof(XMLHttpRequest) != 'undefined') && (typeof(XMLHttpRequest.prototype.sendAsBinary) != 'undefined')) || (typeof(WebKitBlobBuilder) != 'undefined') || (typeof(BlobBuilder) != 'undefined') || (typeof(MozBlobBuilder) != 'undefined')))
            result += "Browser doesn't support sending binary data\n";
        return result;
    },

    showError : function showError(msg, e) {
        var result = "";
        if (e) {
            result += e.name + ": " + e.message;
        } else {
            result += msg.getErrorMessage() + "\n";
        };
        result += "\nCall stack:\n";
        var fn = showError;
        if (!(fn.caller)) //possibly IE
            fn = arguments.callee;
        while((fn = fn.caller) !== null) {
            var fnName = fn.toString().match(/^function\s*(\w+)\(/);
            fnName = (fnName) ? fnName[1] : 'anonymous function';
            result += fnName;
            result += "\n";
        }
        alert(result);
    },

    toJSON : function toJSON(aValue) {
        if(typeof(JSON) != 'undefined') {
            return JSON.stringify(aValue);
        } else {
            throw new Error("Your browser doesn't support JSON.stringify");
        };
    },

    parseJSON : function parseJSON(aValue) {
        if(typeof(JSON) != 'undefined') {
            return JSON.parse(aValue);
        } else {
            throw new Error("Your browser doesn't support JSON.parse");
        };
    },

    NewGuid : function NewGuid() {
        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g,
        function(c) {
        var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
        return v.toString(16); })
    },

    GuidToArray : function GuidToArray(aGuid) {
        var result = [];
        aGuid = aGuid.replace(/-/g, "");
        for (var i = 0; i < 16; result.push(parseInt(aGuid.substr(i++ * 2, 2), 16)));
        return result;
    },

    guidToByteArray : function guidToByteArray(aGuid) {
        function readPart(str, start, end) {
            var result = "";
            for (var i = start; i <= end; result += String.fromCharCode(parseInt(aGuid.substr(i++ * 2 + 1, 2), 16)));
            return result;
        };

        function readPartReversed(str, start, end) {
            var result = "";
            for (var i = end; i >= start; result += String.fromCharCode(parseInt(aGuid.substr(i-- * 2 + 1, 2), 16)));
            return result;
        };
        aGuid = aGuid.replace(/-/g, "");
        return readPartReversed(aGuid, 0, 3) + readPartReversed(aGuid, 4, 5)
               + readPartReversed(aGuid, 6, 7) + readPart(aGuid, 8, 9) + readPart(aGuid, 10, 15);
    },

    byteArrayToGuid : function byteArrayToGuid(byteArray) {
        function zeroPad(num,count) {
            var numZeropad = num + '';
            while (numZeropad.length < count) {
                numZeropad = "0" + numZeropad;
            }
            return numZeropad;
        };
        function readPartReversed(str, start, end) {
            var result = "";
            for (var i = end; i >= start; i--) {
                result += zeroPad((str.charCodeAt(i) & 0xFF).toString(16).toUpperCase(), 2);
            };
            return result;
        };
        function readPart(str, start, end) {
            var result = "";
            for (var i = start; i <= end; i++) {
                result += zeroPad((str.charCodeAt(i) & 0xFF).toString(16).toUpperCase(), 2);
            };
            return result;
        };
        return "{" + readPartReversed(byteArray, 0, 3) + "-"
                   + readPartReversed(byteArray, 4, 5) + "-"
                   + readPartReversed(byteArray, 6, 7) + "-"
                   + readPart(byteArray, 8, 9) + "-"
                   + readPart(byteArray, 10, 15)
                + "}";
    },


    strToByteArray : function strToByteArray(str) {
        var byteArray = [];
        for (var i = 0; i < str.length; i++)
            if (str.charCodeAt(i) <= 0x7F)
                byteArray.push(str.substr(i, 1));
            else {
                var h = encodeURIComponent(str.charAt(i)).substr(1).split('%');
                for (var j = 0; j < h.length; j++)
                    byteArray.push(String.fromCharCode(parseInt(h[j], 16)));
            };
        return byteArray.join("");
    },



    byteArrayToStr : function byteArrayToStr(byteArray) {
        var str = '';
        for (var i = 0; i < byteArray.length; i++)
            str += byteArray.charCodeAt(i) <= 0x7F ?
                    byteArray.charCodeAt(i) === 0x25 ? "%25" : // %
                            byteArray.substr(i, 1) :
                    "%" + (byteArray.charCodeAt(i) & 0xFF).toString(16).toUpperCase();
        return decodeURIComponent(str);
    },

    byteArrayToUtf16 : function byteArrayToUtf16(byteArray) {
        var str = '';
        for (var i = 0; i < byteArray.length / 2; i++) 
            str += String.fromCharCode((byteArray.charCodeAt(i * 2) & 0xFF) + ((byteArray.charCodeAt(i * 2 + 1) & 0xFF) << 8));
        return str;
    },

    utf16ToByteArray : function utf16ToByteArray(str) {
        var byteArray = "";
        for (var i = 0; i < str.length; i++) {
            byteArray += String.fromCharCode(str.charCodeAt(i) & 0xFF);
            byteArray += String.fromCharCode((str.charCodeAt(i) & 0xFF00) >> 8);
        };
        return byteArray;
    },

    ISO8601toDateTime : function ISO8601toDateTime(str) {
        var regexp = "([0-9]{4})(-([0-9]{2})(-([0-9]{2})" +
            "(T([0-9]{2}):([0-9]{2})(:([0-9]{2})(\.([0-9]+))?)?" +
            "(Z|(([-+])([0-9]{2}):([0-9]{2})))?)?)?)?";
        var d = str.match(new RegExp(regexp));

        var offset = 0;
        var date = new Date(d[1], 0, 1);

        if (d[3]) { date.setMonth(d[3] - 1); }
        if (d[5]) { date.setDate(d[5]); }
        if (d[7]) { date.setHours(d[7]); }
        if (d[8]) { date.setMinutes(d[8]); }
        if (d[10]) { date.setSeconds(d[10]); }
        if (d[12]) { date.setMilliseconds(Number("0." + d[12]) * 1000); }
        if (d[14]) {
            offset = (Number(d[16]) * 60) + Number(d[17]);
            offset *= ((d[15] == '-') ? 1 : -1);
            offset -= date.getTimezoneOffset();
            var time = (Number(date) + (offset * 60 * 1000));
            date.setTime(Number(time));
        }

        return date;
    },


    decimalToString : function decimalToString(aDecimal) { //aDecimal - array [0..6]
        var sign = (aDecimal[6] & 0x80000000) != 0;
        var scale = (aDecimal[6] & 0xFF0000) >> 16;
        var pos = 31;
        var aDec = aDecimal.slice();
        var aResult = [];
        var modres;
        var d;
        while ((aDec[0] != 0) || (aDec[1] != 0) || (aDec[2] != 0)
                || (aDec[3] != 0) || (aDec[4] != 0) || (aDec[5] != 0)
                || ((31 - pos) < scale)) {
          modres = 0;
          for (var i = 5; i >= 0; i--) {
              d = (modres << 16) | aDec[i];
              modres = d % 10;
              aDec[i] = Math.floor(d / 10);
          };
          aResult[pos] = modres.toString(10);
          pos--;
          if ((31 - pos) == scale) {
            aResult[pos] = ".";
            pos--;
            if ((aDec[0] == 0) && (aDec[1] == 0) && (aDec[2] == 0)) {
              aResult[pos] = '0';
              pos--;
            };
          };
        };
        if (pos == 31)
            return "0";
        if (sign) {
          aResult[pos] = '-';
          pos--;
        };
        return aResult.join("");
    },

    stringToDecimal : function stringToDecimal(aString) {
        var mulres;
        var d;
        var aRes = [0, 0, 0, 0, 0, 0, 0];
        var pos = 0;
        var scalepos = -1;
        var c;
        var n;
        for (var i = 1; i <= aString.length; i++) {
            mulres = 0;
            c = aString.substr(i - 1, 1);
            if (n = parseFloat(c)) {
                mulres = n
            } else if (c == "-") {
                aRes[6] = 0x80000000;
                continue;
            } else if (c == ".") {
                if (scalepos == -1)
                    scalepos = pos;
                continue;
            } else
                continue;


            for (var j = 0; j < 6; j++) {
                d = aRes[j] * 10 + mulres;
                mulres = d >> 16;
                aRes[j] = d & 0xffff;
            };
            pos++;
        };
        if (scalepos != -1) {
            pos = pos - scalepos;
            aRes[6] = aRes[6] | (pos << 16);
        };
        return aRes;
    }



};


RemObjects.SDK.Enum.BinMessage = {
    mtMessage      : 0,
    mtException    : 1,
    mtEvent        : 2,
    mtPoll         : 0x80,
    mtPollResponse : 0x81
};



//RO.SDK implementation



RemObjects.SDK.ROException.prototype = new Error();

RemObjects.SDK.ROEnumType.prototype = new RemObjects.SDK.ROComplexType();

RemObjects.SDK.ROEnumType.prototype.writeTo = function writeTo(aMessage) {
    aMessage.write("", "Integer", this.enumValues.indexOf(this.value));
};

RemObjects.SDK.ROEnumType.prototype.readFrom = function readFrom(aMessage) {
    this.value = this.enumValues[aMessage.read("", "Integer")];
};

RemObjects.SDK.ROEnumType.prototype.toObject = function toObject() {
    return this.value;
};

RemObjects.SDK.ROEnumType.prototype.fromObject = function fromObject(aValue) {
    this.value = aValue; //todo: add check
};


RemObjects.SDK.ROStructType.prototype = new RemObjects.SDK.ROComplexType();

RemObjects.SDK.ROStructType.prototype.writeTo = function writeTo(aMessage) {
    for (var prop in this) {
        if ((typeof this[prop]) != "function") {
            aMessage.write(prop, this[prop].dataType, this[prop].value);
        };
    };
};

RemObjects.SDK.ROStructType.prototype.readFrom = function readFrom(aMessage) {
    for (var prop in this) {
        if ((typeof this[prop]) != "function") {
            this[prop].value = aMessage.read(prop, this[prop].dataType);
        };
    };
};


RemObjects.SDK.ROStructType.prototype.toObject = function toObject() {
    var result = {};
    for (var prop in this) {
        if ((typeof this[prop]) != "function") {
            if (this[prop].value instanceof RemObjects.SDK.ROComplexType) {
                result[prop] = this[prop].value.toObject();
            } else
                result[prop] = this[prop].value;
        };
    };
    return result;
};

RemObjects.SDK.ROStructType.prototype.fromObject = function fromObject(aValue) {
    for (var prop in this) {
        if ((typeof this[prop]) != "function") { //!!!
            if (RemObjects.SDK.RTTI[this[prop].dataType] && RemObjects.SDK.RTTI[this[prop].dataType].prototype instanceof RemObjects.SDK.ROComplexType) {
                this[prop].value = new RemObjects.SDK.RTTI[this[prop].dataType]();
                this[prop].value.fromObject(aValue[prop]);
            } else {
                if (this[prop].dataType == "DateTime") {
                    this[prop].value = RemObjects.UTIL.ISO8601toDateTime(aValue[prop]);
                } else {
                    this[prop].value = aValue[prop];
                };
            };

        };
    };
    return this;
};


RemObjects.SDK.ROArrayType.prototype = new RemObjects.SDK.ROComplexType();
RemObjects.SDK.ROArrayType.prototype.constructor = RemObjects.SDK.ROArrayType;


RemObjects.SDK.ROArrayType.prototype.writeTo = function writeTo(aMessage) {
    for (var i=0; i<this.items.length; i++ ) {
            aMessage.write("", this.elementType, this.items[i]);
    };
};

RemObjects.SDK.ROArrayType.prototype.readFrom = function readFrom(aMessage) {
    for (var i=0; i<this.items.length; i++ ) {
            this.items[i] = aMessage.read("", this.elementType);
    };
};


RemObjects.SDK.ROArrayType.prototype.toObject = function toObject() {
    var result = [];
    for (var i = 0; i < this.items.length; i++)
        if (this.items[i] instanceof RemObjects.SDK.ROComplexType) {
            result.push(this.items[i].toObject());
        } else
            result.push(this.items[i]);
    return result;
};

RemObjects.SDK.ROArrayType.prototype.fromObject = function fromObject(aValue) {
    var itemType = RemObjects.SDK.RTTI[this.elementType];
    if(itemType) {
        for (var i = 0; i < aValue.length; i++) {
            var item = new itemType();
            item.fromObject(aValue[i]);
            this.items.push(item);
        };
    } else {
        if (this.elementType == "DateTime") {
            for (var i = 0; i < aValue.length; i++) {
                this.items.push(RemObjects.UTIL.ISO8601toDateTime(aValue[i]));
            };
        } else {
            this.items = aValue;
        };
    };
    return this;
};

RemObjects.SDK.ClientChannel.prototype.dispatch = function dispatch(aMessage, onSuccessFunction, onErrorFunction) {
    var that = this;
    this.post(aMessage.requestStream(), aMessage instanceof RemObjects.SDK.BinMessage, function ajax_post_success(__response) {
        try {
            aMessage.setResponseStream(__response);
            if (onSuccessFunction)
                onSuccessFunction(aMessage);
        } catch (e) {
            if (onErrorFunction)
                onErrorFunction(aMessage, e);
        };
    }, function ajax_post_error(__response, __status) {
            aMessage.setErrorResponse("AJAX status: " + __status + "\nResponse: " +__response);
            try {
                if (__response)
                    aMessage.setResponseStream(__response);
                if (onErrorFunction)
                    onErrorFunction(aMessage);
            } catch (e) {
                if ((e.name == "EROSessionNotFound") && !(that.retrying)) {
                    if (that.onLoginNeeded) {
                        that.onLoginNeeded(function() {
                            that.retrying = true;
                            that.dispatch(aMessage, function(__msg) {
                                that.retrying = false;
                                onSuccessFunction(__msg)
                            },
                                    function(__msg, __e) {
                                        that.retrying = false;
                                        onErrorFunction(__msg, __e);
                                    });
                        });
                    };
                }
                else {
//                    if (window[e.name] && window[e.name].prototype instanceof RemObjects.SDK.ROException) {
                    if (RemObjects.SDK.RTTI[e.name] && RemObjects.SDK.RTTI[e.name].prototype instanceof RemObjects.SDK.ROException) {
                        e = new RemObjects.SDK.RTTI[e.name](e);
                        e.fields.readFrom(aMessage);
                    };
                    if (onErrorFunction)
                        onErrorFunction(aMessage, e);
                };
            };
    });
};

RemObjects.SDK.ClientChannel.prototype.onLoginNeeded = function onLoginNeeded(aCallback) {
    alert("Default onLoginNeeded handler: override channel.onLoginNeeded and call aCallback there after successful login");
    aCallback();
};

RemObjects.SDK.HTTPClientChannel.prototype = new RemObjects.SDK.ClientChannel("");
RemObjects.SDK.HTTPClientChannel.prototype.constructor = RemObjects.SDK.HTTPClientChannel;
RemObjects.SDK.HTTPClientChannel.prototype.post = function post(aMessage, isBinary, onSuccess, onError) {
  var ajaxObject;
  if ((typeof(Ti) != 'undefined') && (typeof(Ti.Network) != 'undefined')) {
      ajaxObject = new TitaniumAjaxWrapper(this.url);
  } else {
      ajaxObject = new AjaxWrapper(this.url);
  };
  ajaxObject.post(aMessage, isBinary, onSuccess, onError);
//    $.post(this.url, aMessage, aCallback, "json");
};

RemObjects.SDK.Message.prototype.clone = function clone() {
    var cloned = new this.constructor();
    cloned.fClientID = this.fClientID;
    return cloned;
};

RemObjects.SDK.Message.prototype.setErrorResponse = function setErrorResponse(aResponse) {
    this.fResponseObject.error = {message: aResponse};
};

RemObjects.SDK.Message.prototype.getErrorMessage = function getErrorMessage() {
    if (this.fResponseObject.error)
        return this.fResponseObject.error.message;
    else
        return "";
};


RemObjects.SDK.BinHeader.prototype.asStream = function asStream() {
    var result = "";
    var parser = new BinaryParser();
    for (var i = 0; i < 0x1c; i++)
        result += parser.encodeInt(this.fHeader[i], 8, false);
    return result;
};

RemObjects.SDK.BinHeader.prototype.readFrom = function readFrom(aStream) {
    var parser = new BinaryParser();
    for (var i = 0; i < 0x1c; i++)
        this.fHeader[i] = parser.decodeInt(aStream.substr(i, 1), 8, false);
};

RemObjects.SDK.BinHeader.prototype.isValidHeader = function isValidHeader() {
    var tmp = "";
    for (var i = 0; i < 5; tmp+=String.fromCharCode(this.fHeader[i++]));
    return (tmp == "RO107");
};

RemObjects.SDK.BinHeader.prototype.getCompressed = function getCompressed() {
    return this.fHeader[5];
};

RemObjects.SDK.BinHeader.prototype.setCompressed = function setCompressed(aValue) {
    this.fHeader[5] = aValue ? 1 : 0;
};

RemObjects.SDK.BinHeader.prototype.getMessageType = function getMessageType() {
    return this.fHeader[6];
};

RemObjects.SDK.BinHeader.prototype.setMessageType = function setMessageType(aValue) {
    this.fHeader[6] = aValue;
};

RemObjects.SDK.BinHeader.prototype.setClientID = function setClientID(aValue) {
    var guid = RemObjects.UTIL.GuidToArray(aValue);
    this.fHeader.length -= 16;
    this.fHeader = this.fHeader.concat(guid);
};


RemObjects.SDK.BinMessage.prototype = new RemObjects.SDK.Message();
RemObjects.SDK.BinMessage.prototype.constructor = RemObjects.SDK.BinMessage;


RemObjects.SDK.BinMessage.prototype.initialize = function initialize(aServiceName, aMethodName) {
    var header = new RemObjects.SDK.BinHeader();
    header.setCompressed(false);
    header.setMessageType(RemObjects.SDK.Enum.BinMessage.mtMessage);
    header.setClientID(this.fClientID);
    this.fRequestObject = header.asStream();
    this.parser = new BinaryParser();
    this.fRequestObject += this.parser.encodeInt(aServiceName.length, 32, false) + aServiceName;
    this.fRequestObject += this.parser.encodeInt(aMethodName.length, 32, false) + aMethodName;
};

RemObjects.SDK.BinMessage.prototype.finalize = function finalize() {

};

RemObjects.SDK.BinMessage.prototype.write = function write(aName, aType, aValue) {
    if (aValue instanceof RemObjects.SDK.ROComplexType) {
        if (!(aValue instanceof RemObjects.SDK.ROEnumType)) {
            this.fRequestObject += this.parser.encodeInt(1, 8, false);
            if (aValue instanceof RemObjects.SDK.ROStructType)
                this.writeStrWithLength(aType);
            if (aValue instanceof RemObjects.SDK.ROArrayType)
                this.fRequestObject += this.parser.encodeInt(aValue.items.length, 32, false);
        };
        aValue.writeTo(this);
    }
    else
    switch (aType) {

        case "Decimal":
            var decimal = RemObjects.UTIL.stringToDecimal(aValue.toString());
            for (var i = 0; i < 6; i++)
                this.fRequestObject += this.parser.encodeInt(decimal[i], 16, false);
            this.fRequestObject += this.parser.encodeInt(decimal[6], 32, false);
            break;

        case "Double":
            this.fRequestObject += this.parser.encodeFloat(aValue, 52, 11);
            break;

        case "Boolean":
            this.fRequestObject += this.parser.encodeInt((aValue ? 1 : 0), 32, false);
            break;

        case "Binary":
            this.fRequestObject += this.parser.encodeInt(1, 8, false);
            this.writeStrWithLength(aValue);
            break;
        
        case "Integer":
            this.fRequestObject += this.parser.encodeInt(aValue, 32, true);
            break;

        case "Currency":
            var cur = this.parser.encodeInt(aValue * 10000, 48, true);
            this.fRequestObject += cur;
            if ((cur.charCodeAt(cur.length - 1) == 0) || (cur.charCodeAt(cur.length - 1) == 0xFF)) {
                this.fRequestObject += cur.substr(cur.length - 1, 1) + cur.substr(cur.length - 1, 1);
            };
            break;

        case "Guid":
            this.fRequestObject += RemObjects.UTIL.guidToByteArray(aValue);
            break;

        case "DateTime":
            this.fRequestObject += this.parser.encodeFloat(aValue / 86400000 + 25569.0, 52, 11);
            break;

        case "WideString":
            this.fRequestObject += this.parser.encodeInt(aValue.length, 32, true);
            this.fRequestObject += RemObjects.UTIL.utf16ToByteArray(aValue);
            break;
        case "Utf8String":
            aValue = RemObjects.UTIL.strToByteArray(aValue);
        case "AnsiString":
            this.writeStrWithLength(aValue);
            break;
        case "Variant":
            this.writeVariant(aValue);
            break;

        default:
        throw new Error("BinMessage.write: Unknown type of " + aName + " - " + aType);
    };

};

RemObjects.SDK.BinMessage.prototype.writeVariant = function writeVariant(aValue) {
    var tmpValue;
    var tmpInt;
    var tmpFloat;
    if ((aValue == undefined) || (aValue == null)) {
        this.writeInteger(0x0001); //varNull
    } else if(!isNaN(aValue)) {
        if((tmpInt = parseInt(aValue)) == (tmpFloat = parseFloat(aValue))) {
            this.writeInteger(0x0003); //varInt32
            this.write("", "Integer", tmpInt);
        } else {
            this.writeInteger(0x0005); //varDouble
            this.write("", "Double", tmpFloat);
        };
    } else if (typeof(aValue) == "string") {
        this.writeInteger(0x0008); //varString
        this.write("", "Utf8String", aValue);
    } else if (aValue.length) {
        this.writeInteger(0x200C); //varVariant
        this.writeInteger(0); //lowBound
        this.writeInteger(aValue.length - 1); //highBound
        for (var i = 0; i < aValue.length; i++)
            this.writeVariant(aValue[i]);
    } else throw new Error("writeVariant: unknown type")

};

RemObjects.SDK.BinMessage.prototype.writeInteger = function writeVariant(aValue) {
    this.fRequestObject += this.parser.encodeInt(aValue, 32, true);
};

RemObjects.SDK.BinMessage.prototype.writeStrWithLength = function writeStrWithLength(aValue) {
    this.fRequestObject += this.parser.encodeInt(aValue.length, 32, false) + aValue;
};



RemObjects.SDK.BinMessage.prototype.read = function read(aName, aType) {
    var value;

    if (RemObjects.SDK.RTTI[aType] && RemObjects.SDK.RTTI[aType].prototype instanceof RemObjects.SDK.ROComplexType) {
        value = new RemObjects.SDK.RTTI[aType]();
        if (!(value instanceof RemObjects.SDK.ROEnumType)) {
            if (this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos++, 1), 8, false)){ //assigned
                //this.fStreamPos +=1;
                if (value instanceof RemObjects.SDK.ROStructType)
                    this.fStreamPos += (4 + this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false)); //skip name
                if (value instanceof RemObjects.SDK.ROArrayType) {
                    value.items.length = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
                    this.fStreamPos += 4;
                };
                value.readFrom(this);
            };

        } else {
            value.readFrom(this);
        };
    }
    else
    switch (aType) {

        case "Decimal":
            var decimal = [];
            for (var i = 0; i < 6; i++) {
                decimal[i] = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 2), 16, false);
                this.fStreamPos += 2;
            };
            decimal[6] = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = parseFloat(RemObjects.UTIL.decimalToString(decimal));
            break;
        case "Double":
            value = this.parser.decodeFloat(this.fResponseString.substr(this.fStreamPos, 8), 52, 11);
            this.fStreamPos += 8;
            this.fResponseObject[aName] = value;
            break;
        
        case "DateTime":
            value = this.parser.decodeFloat(this.fResponseString.substr(this.fStreamPos, 8), 52, 11);
            this.fStreamPos += 8;
            this.fResponseObject[aName] = value;
            value = new Date(Math.round((value - 25569.0) * 86400000));
            break;

        case "Boolean":
            value = !(this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false) == 0);
            this.fStreamPos += 4;
            this.fResponseObject[aName] = value;
            break;

        case "Integer":
            value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, true);
            this.fStreamPos += 4;
            this.fResponseObject[aName] = value;
            break;

        case "Currency":
            value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 6), 48, true) / 10000;
            this.fStreamPos += 8;
            this.fResponseObject[aName] = value;
            break;

        case "Utf8String":
            var len = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = RemObjects.UTIL.byteArrayToStr(this.fResponseString.substr(this.fStreamPos, len));
            this.fStreamPos += len;
            break;
        case "WideString":
            var len = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = RemObjects.UTIL.byteArrayToUtf16(this.fResponseString.substr(this.fStreamPos, len * 2));
            this.fStreamPos += len * 2;
            break;

        case "Binary":
            var isAssigned = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 1), 8, false);
            this.fStreamPos += 1;
            if (isAssigned == 0) break;
        case "AnsiString":
            var len = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            value = this.fResponseString.substr(this.fStreamPos, len);
            this.fStreamPos += len;
            break;

        case "Guid":
            value = RemObjects.UTIL.byteArrayToGuid(this.fResponseString.substr(this.fStreamPos, 16));
            this.fStreamPos += 16;
            break;

        case "Variant":
            value = this.readVariant();
            break;

        default:
            if (RemObjects.SDK.RTTI[aType] && (typeof(RemObjects.SDK.RTTI[aType]) == "function") && (RemObjects.SDK.RTTI[aType].prototype instanceof RemObjects.SDK.ROComplexType)) {
                value = new RemObjects.SDK.RTTI[aType]();
                value.readFrom(this);
            } else {
                this.fResponseObject.error = {message : "BinMessage.read: Unknown type of " + aName + " - " + aType};
                throw new Error(this.fResponseObject.error.message);
            };
    };
    return value;
};

RemObjects.SDK.BinMessage.prototype.readVariant = function readVariant() {
    var code = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
    this.fStreamPos += 4;
    var result;

    if ((code & 0x2000) == 0x2000) {
        if (code == 0x2011) { //varBinary
            var binLength = this.read("", "Integer");
            result = this.fResponseString.substr(this.fStreamPos, binLength);
            this.fStreamPos += binLength;
        } else {//varArray
            //var itemCode = code & 0xFFF;
            result = [];
            var lowBound = this.read("", "Integer");
            var highBound = this.read("", "Integer");
            for (var i = lowBound; i <= highBound; i++)
                result[i] = this.readVariant();
        };
        return result;
    };

    switch(code) {
        case 0x000A: //varError
        case 0x0000: return undefined; //varEmpty
        case 0x0001: return null; //varNull
        case 0x0002: //varInt16
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 2), 16, true);
            this.fStreamPos += 2;
            return result;
        case 0x0003: //varInt32
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, true);
            this.fStreamPos += 4;
            return result;
        case 0x0004: //varSingle
            result = this.parser.decodeFloat(this.fResponseString.substr(this.fStreamPos, 4), 23, 8);
            this.fStreamPos += 4;
            return result;
        case 0x0005: return this.read("", "Double");//varDouble
        case 0x0006: return this.read("", "Currency");//varCurrency
        case 0x0007: return this.read("", "DateTime");//varDateTime
//varDispatch = 0x0009
        case 0x000B: return this.read("", "Boolean");//varBoolean
//varVariant = 0x000C
//varUnknown = 0x000D
        case 0x000E: return this.read("", "Decimal");//varDecimal
        case 0x0010: //varInt8
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 1), 8, true);
            this.fStreamPos += 1;
            return result;
        case 0x0011: //varByte
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 1), 8, false);
            this.fStreamPos += 1;
            return result;
        case 0x0012: //varWord
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 2), 16, false);
            this.fStreamPos += 2;
            return result;
        case 0x0013: //varLongWord
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4;
            return result;
        case 0x0014: //varInt64
            result = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 8), 48, true);
            this.fStreamPos += 8;
            return result;
        case 0x0072: return this.read("", "Guid");//varGuid
        case 0x0100: return this.read("", "AnsiString");//varDelphiString
        case 0x0008: //varOleStr
        case 0x0102: return this.read("", "Utf8String");//varDelphiUtfString
        default : throw new Error("readVariant: unknown varCode 0x" + code.toString(16));
    };
};


RemObjects.SDK.BinMessage.prototype.requestStream = function requestStream() {
    return this.fRequestObject;
};

RemObjects.SDK.BinMessage.prototype.setResponseStream = function setResponseStream(aResponse) {
    this.fResponseString = aResponse;
    var header = new RemObjects.SDK.BinHeader();
    header.readFrom(this.fResponseString.substr(0, 28));
    this.fStreamPos = 28; //skip header

    if (!header.isValidHeader()) {
        this.fResponseObject.error = {message : "Invalid response: unsupported binary message signature"};
        throw new Error(this.fResponseObject.error.message);
    };
    if (header.getCompressed()) {
        this.fResponseObject.error = {message : "Invalid response: compression is not supported for binary message"};
        throw new Error(this.fResponseObject.error.message);
    };

    switch (header.getMessageType()) {

        case RemObjects.SDK.Enum.BinMessage.mtMessage :
            var value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4 + value; //skip service name
            value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            this.fStreamPos += 4 + value; //skip method name
            break;

        case RemObjects.SDK.Enum.BinMessage.mtException :
            var value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            var exceptionClass = this.fResponseString.substr(this.fStreamPos + 4, value);
            this.fStreamPos +=  4 + value;
            value = this.parser.decodeInt(this.fResponseString.substr(this.fStreamPos, 4), 32, false);
            var exceptionMessage = this.fResponseString.substr(this.fStreamPos + 4, value);
            this.fStreamPos += 4 + value; //skip method name
            this.fResponseObject.error = {message : exceptionClass + ":\n" + exceptionMessage};
            var __e = new Error(exceptionMessage);
            __e.name = exceptionClass;
            throw __e;
            break;

        default:
            throw new Error("Unsupported binary message type - 0x" + header.getMessageType().toString(16));
    };


};


RemObjects.SDK.JSONMessage.prototype = new RemObjects.SDK.Message();
RemObjects.SDK.JSONMessage.prototype.constructor = RemObjects.SDK.JSONMessage;

RemObjects.SDK.JSONMessage.prototype.initialize = function initialize(aServiceName, aMethodName) {
    this.fRequestObject.id = "{" + this.fClientID + "}";
    this.fRequestObject.method = aServiceName + "." + aMethodName;
    this.fRequestObject.params = {};
};

RemObjects.SDK.JSONMessage.prototype.finalize = function finalize() {

};

RemObjects.SDK.JSONMessage.prototype.write = function write(aName, aType, aValue) {
    if (aValue instanceof RemObjects.SDK.ROComplexType)
        this.fRequestObject.params[aName] = aValue.toObject();
    else
        this.fRequestObject.params[aName] = aValue;
};


RemObjects.SDK.JSONMessage.prototype.read = function read(aName, aType) {
    if (this.fResponseObject.result == undefined) {
        throw new Error("JSONMessage.read: result property is missing:\n" + RemObjects.UTIL.toJSON(this.fResponseObject));
    };
    if (this.fResponseObject.result[aName] == undefined) {
        throw new Error("JSONMessage.read error:\n" + aName + ":" + aType + " is missing.");
    };

    var result;
    if (RemObjects.SDK.RTTI[aType] && RemObjects.SDK.RTTI[aType].prototype instanceof RemObjects.SDK.ROComplexType) {
        result = new RemObjects.SDK.RTTI[aType]();
        if (RemObjects.SDK.RTTI[aType].prototype instanceof RemObjects.SDK.ROEnumType) {
            result.value = this.fResponseObject.result[aName];
        } else {
            result.fromObject(this.fResponseObject.result[aName]);
        };
    } else {
        if (aType == "DateTime") {
            result = RemObjects.UTIL.ISO8601toDateTime(this.fResponseObject.result[aName]);
        } else {
            result = this.fResponseObject.result[aName];
        };
    };
    return result;
};


RemObjects.SDK.JSONMessage.prototype.requestStream = function requestStream() {
    return RemObjects.UTIL.toJSON(this.fRequestObject);
};

RemObjects.SDK.JSONMessage.prototype.setResponseStream = function setResponseStream(aResponse) {
    try {
        this.fResponseObject = RemObjects.UTIL.parseJSON(aResponse);
    } catch (e) {
        throw new Error("JSONMessage.setResponseStream:\n JSON parsing error: " + e.message + "\nServer response:\n" + aResponse);
    };
    if (this.fResponseObject.error  && (this.fResponseObject.error.name == "ROJSONException")) {
        var __e;
        __e = new Error(this.fResponseObject.error.message);
        __e.name = this.fResponseObject.error.roexception.type;
        throw __e;
    };
};


function TitaniumAjaxWrapper(url) {
            this.updating = false;
            this.urlCall = url;
};


TitaniumAjaxWrapper.prototype.post = function post(passData, isBinary, onSuccessFunction, onErrorFunction) {

    if (this.updating) {
        return false;
    };
    this.AJAX = null;

    this.AJAX = Ti.Network.createHTTPClient({
        onload: function(e) {
            Ti.API.info("onload");
            if (isBinary) {
                onSuccessFunction(this.responseData, 200);
            } else {
                onSuccessFunction(this.responseText, 200);
            };
        },
        onerror: function(e) {
            Ti.API.info('XHR Error ' + e.error);
        },
        timeout:5000
    });
    this.updating = new Date();
    var uri = this.urlCall + '?' + this.updating.getTime();

    Ti.API.info("TitaniumAjaxWrapper " + uri);
    this.AJAX.open("POST", uri);
    // if (isBinary) {
    //     this.AJAX.setRequestHeader('Content-Type','multipart/form-data');
    // }
    this.AJAX.send(passData);
};

function AjaxWrapper(url) {
            this.updating = false;
            this.urlCall = url;
};


AjaxWrapper.prototype.abort = function abort() {
    if (this.updating) {
        this.updating = false;
        this.AJAX.abort();
        this.AJAX = null;
    };
};

AjaxWrapper.prototype.post = function post(passData, isBinary, onSuccessFunction, onErrorFunction) {

    if (this.updating) {
        return false;
    };
    this.AJAX = null;


    try {
        this.AJAX = new XMLHttpRequest();
    } catch (e) {
        // Internet Explorer Browsers
        try {
            this.AJAX = new ActiveXObject("Msxml2.XMLHTTP");
        } catch (e) {
            try {
                this.AJAX = new ActiveXObject("Microsoft.XMLHTTP");
            } catch (e) {
                throw new Error("Your browser doesn't support XMLHttpRequest object");
            };
        };
    };


         if (this.AJAX == null) {
             return false;
         } else {
             this.onSuccess = onSuccessFunction || function () {
             };
             this.onError = onErrorFunction || function () {
             };
             var that = this;

             this.AJAX.onreadystatechange = function onreadystatechange() {
                 if (that.AJAX.readyState == 4) {
                     that.updating = false;
                     if (that.AJAX.status == 200) {
                         that.onSuccess(that.AJAX.responseText, that.AJAX.status, that.AJAX.responseXML);
                     } else {
                        that.onError(that.AJAX.responseText, that.AJAX.status, that.AJAX.responseXML);
                     };
                     that.AJAX = null;
                 };
             };
             this.updating = new Date();
                 var uri = this.urlCall + '?' + this.updating.getTime();
                 this.AJAX.open("POST", uri, true);
                 //this.AJAX.setRequestHeader("Content-Length", passData.length);
                 if (isBinary == true) {
                    if (this.AJAX.overrideMimeType)
                        this.AJAX.overrideMimeType('text/plain; charset=x-user-defined');
                    this.AJAX.setRequestHeader("Content-type", "application/octet-stream");
                    if (this.AJAX.sendAsBinary) {
                        this.AJAX.sendAsBinary(passData);
                    } else {
                            if (bbClass = window.WebKitBlobBuilder || window.BlobBuilder || window.MozBlobBuilder) {
                                var bb = new bbClass();
                                var len = passData.length;
                                var data = new Uint8Array(len);
                                for (var i=0; i<len; i++) {
                                    data[i] = passData.charCodeAt(i);
                                };
                                bb.append(data.buffer);
                                this.AJAX.send(bb.getBlob("application/octet-stream"));
                            }
                            else
                                throw new Error("AjaxWrapper.post: sendAsBinary is not defined, no data sent");
                    };
                 } else {
                     this.AJAX.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
                     this.AJAX.send(passData);
                 };
            return true;
         };
};

// binary parser by Jonas Raoni Soares Silva
function BinaryParser() {
    this.BigEndian = true;
};

BinaryParser.prototype.warn = function warn(msg) {throw new Error(msg)};

BinaryParser.prototype.decodeFloat = function decodeFloat( data, precisionBits, exponentBits ){
    var b = new this.Buffer( this.bigEndian, data );
    b.checkBuffer( precisionBits + exponentBits + 1 );
    var bias = Math.pow( 2, exponentBits - 1 ) - 1, signal = b.readBits( precisionBits + exponentBits, 1 ), exponent = b.readBits( precisionBits, exponentBits ), significand = 0,
    divisor = 2, curByte = b.buffer.length + ( -precisionBits >> 3 ) - 1;
    do
        for( var byteValue = b.buffer[ ++curByte ], startBit = precisionBits % 8 || 8, mask = 1 << startBit; mask >>= 1; ( byteValue & mask ) && ( significand += 1 / divisor ), divisor *= 2 );
    while( precisionBits -= startBit );
    return exponent == ( bias << 1 ) + 1 ? significand ? NaN : signal ? -Infinity : +Infinity : ( 1 + signal * -2 ) * ( exponent || significand ? !exponent ? Math.pow( 2, -bias + 1 ) * significand : Math.pow( 2, exponent - bias ) * ( 1 + significand ) : 0 );
};
BinaryParser.prototype.encodeFloat = function encodeFloat( data, precisionBits, exponentBits ){
    var bias = Math.pow( 2, exponentBits - 1 ) - 1, minExp = -bias + 1, maxExp = bias, minUnnormExp = minExp - precisionBits,
    status = isNaN( n = parseFloat( data ) ) || n == -Infinity || n == +Infinity ? n : 0,
    exp = 0, len = 2 * bias + 1 + precisionBits + 3, bin = new Array( len ),
    signal = ( n = status !== 0 ? 0 : n ) < 0, n = Math.abs( n ), intPart = Math.floor( n ), floatPart = n - intPart,
    i, lastBit, rounded, j, result;
    for( i = len; i; bin[--i] = 0 );
    for( i = bias + 2; intPart && i; bin[--i] = intPart % 2, intPart = Math.floor( intPart / 2 ) );
    for( i = bias + 1; floatPart > 0 && i; ( bin[++i] = ( ( floatPart *= 2 ) >= 1 ) - 0 ) && --floatPart );
    for( i = -1; ++i < len && !bin[i]; );
    if( bin[( lastBit = precisionBits - 1 + ( i = ( exp = bias + 1 - i ) >= minExp && exp <= maxExp ? i + 1 : bias + 1 - ( exp = minExp - 1 ) ) ) + 1] ){
        if( !( rounded = bin[lastBit] ) ) {
            for( j = lastBit + 2; !rounded && j < len; rounded = bin[j++] ) {};
        };
        for( j = lastBit + 1; rounded && --j >= 0; ( bin[j] = !bin[j] - 0 ) && ( rounded = 0 ) ){};
    };
    for( i = i - 2 < 0 ? -1 : i - 3; ++i < len && !bin[i]; ){};
    if( ( exp = bias + 1 - i ) >= minExp && exp <= maxExp ) {
        ++i;
    } else if( exp < minExp ){
        exp != bias + 1 - len && exp < minUnnormExp && this.warn( "encodeFloat::float underflow" );
        i = bias + 1 - ( exp = minExp - 1 );
    };
    if( intPart || status !== 0 ){
        this.warn( intPart ? "encodeFloat::float overflow" : "encodeFloat::" + status );
        exp = maxExp + 1;
        i = bias + 2;
        if( status == -Infinity ) {
            signal = 1;
        } else if( isNaN( status ) )
            bin[i] = 1;
    };
    for( n = Math.abs( exp + bias ), j = exponentBits + 1, result = ""; --j; result = ( n % 2 ) + result, n = n >>= 1 );
    for( n = 0, j = 0, i = ( result = ( signal ? "1" : "0" ) + result + bin.slice( i, i + precisionBits ).join( "" ) ).length, r = []; i; j = ( j + 1 ) % 8 ){
        n += ( 1 << j ) * result.charAt( --i );
        if( j == 7 ){
            r[r.length] = String.fromCharCode( n );
            n = 0;
        };
    };
    r[r.length] = n ? String.fromCharCode( n ) : "";
    return ( this.bigEndian ? r.reverse() : r ).join( "" );
};

BinaryParser.prototype.encodeInt = function encodeInt(number, bits, signed){
           var max = Math.pow(2, bits), r = [];
           (number >= max || number < -Math.pow(2, bits-1)) && this.warn("encodeInt::overflow") && (number = 0);
           number < 0 && (number += max);
           for(; number; r[r.length] = String.fromCharCode(number % 256), number = Math.floor(number / 256));
           for(bits = -(-bits >> 3) - r.length; bits--; r[r.length] = "\0");
           return (this.bigEndian ? r.reverse() : r).join("");
       };

BinaryParser.prototype.decodeInt = function decodeInt(data, bits, signed){
           var b = new this.Buffer(this.bigEndian, data), x = b.readBits(0, bits), max = Math.pow(2, bits);
           return signed && x >= max / 2 ? x - max : x;
       };



   with({p: (BinaryParser.prototype.Buffer = function Buffer(bigEndian, buffer){
   this.bigEndian = bigEndian || 0, this.buffer = [], this.setBuffer(buffer);
   }).prototype}){
   p.readBits = function(start, length){
       //shl fix: Henri Torgemane ~1996 (compressed by Jonas Raoni)
       function shl(a, b){
//           for(++b; --b; a = ((a %= 0x7fffffff + 1) & 0x40000000) == 0x40000000 ? a * 2 : (a - 0x40000000) * 2 + 0x7fffffff + 1);
           for(++b; --b; a = ((a %= 0x7fffffffffff + 1) & 0x400000000000) == 0x400000000000 ? a * 2 : (a - 0x400000000000) * 2 + 0x7fffffffffff + 1);
           return a;
       };
       if(start < 0 || length <= 0)
           return 0;
       this.checkBuffer(start + length);
       for(var offsetLeft, offsetRight = start % 8, curByte = this.buffer.length - (start >> 3) - 1,
           lastByte = this.buffer.length + (-(start + length) >> 3), diff = curByte - lastByte,
           sum = ((this.buffer[ curByte ] >> offsetRight) & ((1 << (diff ? 8 - offsetRight : length)) - 1))
           + (diff && (offsetLeft = (start + length) % 8) ? (this.buffer[ lastByte++ ] & ((1 << offsetLeft) - 1))
           << (diff-- << 3) - offsetRight : 0); diff; sum += shl(this.buffer[ lastByte++ ], (diff-- << 3) - offsetRight)
       );
       return sum;
   };
   p.setBuffer = function setBuffer(data){
       if(data){
           for(var l, i = l = data.length, b = this.buffer = new Array(l); i; b[l - i] = data.charCodeAt(--i) & 0xFF);
//           for(var l, i = l = data.length, b = this.buffer = new Array(l); i; b[l - i] = data.charCodeAt(--i));
           this.bigEndian && b.reverse();
       };
   };
   p.hasNeededBits = function hasNeededBits(neededBits){
       return this.buffer.length >= -(-neededBits >> 3);
   };
   p.checkBuffer = function checkBuffer(neededBits){
       if(!this.hasNeededBits(neededBits)) {
           throw new Error("checkBuffer::missing bytes");};
   };
   };
   
   
//*************************************************************//   
//some extra stuff for Websockets PoC
//by: André Mussche, andre.mussche@gmail.com
   
function WebSocketsWrapper(url) {
    this.updating = false;
    this.urlCall = url;
};

WebSocketsWrapper.prototype.post = function post(passData, isBinary, onSuccessFunction, onErrorFunction) 
{
    if (this.updating) {
        return false;
    };
    this.WS = null;

    if ("WebSocket" in window) 
    {
        // Create new websocket connection
        this.WS = new WebSocket(this.urlCall);   //e.g. "ws://localhost:7000/"

        // Called after connection is established
        this.WS.onopen = function() {
            this.send(passData);
        };

        // Called when a new message is received
        this.WS.onmessage = function(msg) { 
            this.updating = false;
            if (msg.data) 
            {
              if (typeof msg.data === "string") {
                onSuccessFunction(msg.data);
              }
              else {  //Blob
                var reader = new FileReader();
                var d = "";
                reader.onloadend = function() {
                    //console.log(reader.result);
                    onSuccessFunction(reader.result);
                };
                reader.readAsText(msg.data);  
              }  
            }
        };

        this.WS.onerror = onErrorFunction;
        
        // Called when connection is closed
        this.WS.onclose = function() { };
    } else {
        alert('Browser doesn\'t support websockets!');
    }
        
    this.updating = new Date();
};   
   
RemObjects.SDK.WebSocketClientChannel.prototype = new RemObjects.SDK.ClientChannel("");
RemObjects.SDK.WebSocketClientChannel.prototype.constructor = RemObjects.SDK.WebSocketClientChannel;
RemObjects.SDK.WebSocketClientChannel.prototype.post = function post(aMessage, isBinary, onSuccess, onError) 
{
    ajaxObject = new WebSocketsWrapper(this.url);
    ajaxObject.post(aMessage, isBinary, onSuccess, onError);
};

//load data on server side (Node.js)
RemObjects.SDK.JSONMessage.prototype.setRequestStream = function setRequestStream(aRequest) {
    try {
        this.fRequestObject = RemObjects.UTIL.parseJSON(aRequest);
        
        //RO for JS is only meant for the client side (yet :)), so we must copy the request
        //into response, so the .read can read it from there
        this.fResponseObject.result = this.fRequestObject.params; 
    } catch (e) {
        throw new Error("JSONMessage.setRequestStream:\n JSON parsing error: " + e.message + "\nServer response:\n" + aResponse);
    };
};
   