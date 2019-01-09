"use strict";

var platformSpecificWS = typeof module !== "undefined" && module.require
      ? module.require('ws')
      : WebSocket;


exports.newWebSocketImpl = function runNewWebSocketImpl (params) {
    var socket = new platformSpecificWS(params.url, params.protocols);
    socket.binaryType = params.binaryType;
    var cont = params.continue({ url : socket.url, protocol : socket.protocol });
    var contBinary = params.continueBinary({ url : socket.url, protocol : socket.protocol });
    var capabilities = {
        "send"              : function(m) {socket.send(m);},
        "close"             : function() {socket.close();},
        "close\'"           : function(xs) {socket.close(xs.code,xs.reason);},
        "getBufferedAmount" : function() {return socket.bufferedAmount;}
    };


    socket.addEventListener("close", function(e) {
        cont.onclose({
            code: e.code,
            reason: e.reason,
            wasClean: e.wasClean
        });
        contBinary.onclose({
            code: e.code,
            reason: e.reason,
            wasClean: e.wasClean
        });
    });

    socket.addEventListener("error", function(e) {
        cont.onerror(e);
        contBinary.onerror(e);
    });

    socket.addEventListener("message", function(e) {
        if (params.isBinary(e.data)) {
            contBinary.onmessage(capabilities, e.data);
        } else {
            cont.onmessage(capabilities, e.data);
        }
    });

    socket.addEventListener("open", function() {
        cont.onopen(capabilities);
        contBinary.onopen(capabilities);
    });
};


function isBinaryImpl (f) {
    return function isBinaryImpl_ (d) {
        return d instanceof f;
    };
};


exports.isBinaryArrayBufferImpl = isBinaryImpl(ArrayBuffer);
exports.isBinaryBlobImpl = isBinaryImpl(Blob);
