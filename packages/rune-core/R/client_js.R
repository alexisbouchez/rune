# =============================================================================
# Client JS loader
# =============================================================================
# Returns the client-side JS as a string. The JS file lives in inst/js/client.js
# and is read at server startup.
# =============================================================================

#' Get the client JS runtime as a string.
#' Checks RUNE_CLIENT_JS env var first (set by rune_load), then falls back
#' to an inline minified version.
get_client_js <- function() {
  # RUNE_CLIENT_JS is set by rune_load() to the absolute path
  js_path <- Sys.getenv("RUNE_CLIENT_JS", "")
  if (nzchar(js_path) && file.exists(js_path)) {
    return(paste(readLines(js_path, warn = FALSE), collapse = "\n"))
  }

  # Try system.file for installed package use
  pkg_path <- system.file("js", "client.js", package = "rune")
  if (nzchar(pkg_path) && file.exists(pkg_path)) {
    return(paste(readLines(pkg_path, warn = FALSE), collapse = "\n"))
  }

  # Inline fallback (minimal version)
  get_client_js_inline()
}

#' Inline fallback client JS (used if the file can't be found).
get_client_js_inline <- function() {
  '(function(){
  "use strict";
  var ws=null;
  function connect(){
    var protocol=location.protocol==="https:"?"wss:":"ws:";
    ws=new WebSocket(protocol+"//"+location.host+"/");
    ws.onopen=function(){
      ws.send(JSON.stringify({type:"init",path:location.pathname}));
    };
    ws.onmessage=function(e){
      try{
        var msg=JSON.parse(e.data);
        if(msg.type==="patch"){
          var el=document.getElementById("rune-island-"+msg.island);
          if(el){
            var t=document.createElement("div");t.innerHTML=msg.html;
            var n=t.firstElementChild;if(n){el.replaceWith(n);bindEvents(n);}
          }
        }else if(msg.type==="reload"){location.reload();}
      }catch(err){console.error(err);}
    };
    ws.onclose=function(){setTimeout(connect,1000);};
  }
  function send(id,type,payload){
    if(ws&&ws.readyState===1){
      var m={type:"event",id:id};m.type=type;
      if(payload)for(var k in payload)m[k]=payload[k];
      ws.send(JSON.stringify(m));
    }
  }
  function bindEvents(el){
    el.querySelectorAll("button[id],[data-rune-click]").forEach(function(b){
      b.addEventListener("click",function(){send(b.id||b.getAttribute("data-rune-click"),"click",{});});
    });
    el.querySelectorAll("input[id],textarea[id],select[id]").forEach(function(i){
      i.addEventListener("input",function(){send(i.id,"input",{value:i.value});});
    });
    el.querySelectorAll("form[id]").forEach(function(f){
      f.addEventListener("submit",function(e){
        e.preventDefault();var d={};for(var j=0;j<f.elements.length;j++){
          var fi=f.elements[j];if(fi.name)d[fi.name]=fi.value;}
        send(f.id,"submit",{data:d});
      });
    });
  }
  document.addEventListener("DOMContentLoaded",function(){
    document.querySelectorAll("[data-rune-island]").forEach(bindEvents);
    document.addEventListener("click",function(e){
      var t=e.target.closest("button[id],[data-rune-click]");
      if(t)send(t.id||t.getAttribute("data-rune-click"),"click",{});
    });
    document.addEventListener("input",function(e){
      if(e.target.id)send(e.target.id,"input",{value:e.target.value});
    });
    connect();
  });
  })();'
}
