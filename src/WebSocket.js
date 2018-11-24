"use strict";

var platformSpecificWS = typeof module !== "undefined" && module.require
      ? module.require('ws')
      : WebSocket;


exports.newWebSocketImpl = function runNewWebSocketImpl (params) {
  var socket = new platformSpecificWS(params.url, params.protocols);
  var cont = params.continue({ url : socket.url, protocol : socket.protocol });
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
  });

  socket.addEventListener("error", function(e) {
    cont.onerror(e);
  });

  socket.addEventListener("message", function(e) {
    cont.onmessage(capabilities, e.data);
  });

  socket.addEventListener("open", function() {
    cont.onopen(capabilities);
  });
};
