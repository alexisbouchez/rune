// =============================================================================
// Rune Client Runtime
// =============================================================================
// Tiny vanilla JS runtime that:
//   1. Opens a WebSocket to the Rune server
//   2. Sends an "init" message with the current path
//   3. Listens for DOM patch messages and applies them (replace island HTML)
//   4. Captures UI events (click, input, submit) on interactive elements
//      and sends them to the server
// =============================================================================

(function() {
  "use strict";

  // --- WebSocket connection ---
  var protocol = location.protocol === "https:" ? "wss:" : "ws:";
  var wsUrl = protocol + "//" + location.host + "/";
  var ws = null;
  var reconnectDelay = 1000;
  var maxReconnectDelay = 10000;

  function connect() {
    ws = new WebSocket(wsUrl);

    ws.onopen = function() {
      console.log("[Rune] Connected");
      reconnectDelay = 1000;
      // Send init message with current path
      ws.send(JSON.stringify({
        type: "init",
        path: location.pathname
      }));
    };

    ws.onmessage = function(event) {
      try {
        var msg = JSON.parse(event.data);
        if (msg.type === "patch") {
          applyPatch(msg.island, msg.html);
        } else if (msg.type === "chart") {
          renderChart(msg.id, msg.config);
        } else if (msg.type === "body_class") {
          if (msg.action === "add") document.body.classList.add(msg.className);
          else if (msg.action === "remove") document.body.classList.remove(msg.className);
        } else if (msg.type === "reload") {
          location.reload();
        }
      } catch (e) {
        console.error("[Rune] Message parse error:", e);
      }
    };

    ws.onclose = function() {
      console.log("[Rune] Disconnected. Reconnecting in", reconnectDelay, "ms...");
      setTimeout(function() {
        reconnectDelay = Math.min(reconnectDelay * 2, maxReconnectDelay);
        connect();
      }, reconnectDelay);
    };

    ws.onerror = function(err) {
      console.error("[Rune] WebSocket error:", err);
      ws.close();
    };
  }

  // --- Chart.js support ---
  var chartInstances = {};

  function renderChart(id, config) {
    var canvas = document.getElementById(id);
    if (!canvas) {
      console.warn("[Rune] Canvas not found for chart:", id);
      return;
    }
    if (chartInstances[id]) {
      chartInstances[id].destroy();
    }
    var ctx = canvas.getContext("2d");
    chartInstances[id] = new Chart(ctx, config);
  }

  // --- DOM Patching ---
  // Simple strategy: replace the innerHTML of the island container.
  // Uses morphdom-like approach but much simpler: just swap outer HTML.
  function applyPatch(islandId, html) {
    var el = document.getElementById("rune-island-" + islandId);
    if (!el) {
      console.warn("[Rune] Island not found:", islandId);
      return;
    }

    // Create a temporary container, parse the new HTML, and replace
    var temp = document.createElement("div");
    temp.innerHTML = html;
    var newEl = temp.firstElementChild;

    if (newEl) {
      // Preserve focus state
      var focused = document.activeElement;
      var focusedId = focused ? focused.id : null;
      var selStart = null, selEnd = null;
      if (focused && (focused.tagName === "INPUT" || focused.tagName === "TEXTAREA")) {
        selStart = focused.selectionStart;
        selEnd = focused.selectionEnd;
      }

      el.replaceWith(newEl);

      // Restore focus
      if (focusedId) {
        var refocus = document.getElementById(focusedId);
        if (refocus) {
          refocus.focus();
          if (selStart !== null && (refocus.tagName === "INPUT" || refocus.tagName === "TEXTAREA")) {
            refocus.selectionStart = selStart;
            refocus.selectionEnd = selEnd;
          }
        }
      }

      // Re-bind events on the new island
      bindIslandEvents(newEl);
    }
  }

  // --- Event binding ---
  // We use event delegation on the document for simplicity,
  // but also bind directly on island elements after patching.

  function sendEvent(id, type, payload) {
    if (ws && ws.readyState === WebSocket.OPEN) {
      var msg = { type: "event", id: id, event: type };
      // Merge payload
      if (payload) {
        for (var key in payload) {
          msg[key] = payload[key];
        }
      }
      ws.send(JSON.stringify(msg));
    }
  }

  // Bind events within an island element
  function bindIslandEvents(container) {
    // Click events on buttons and elements with data-rune-click
    var clickables = container.querySelectorAll("button[id], [data-rune-click]");
    clickables.forEach(function(el) {
      el.addEventListener("click", function(e) {
        var targetId = el.id || el.getAttribute("data-rune-click");
        if (targetId) {
          sendEvent(targetId, "click", {});
        }
      });
    });

    // Input events
    var inputs = container.querySelectorAll("input[id], textarea[id], select[id]");
    inputs.forEach(function(el) {
      var evtName = (el.type === "checkbox" || el.type === "radio") ? "change" : "input";
      el.addEventListener(evtName, function(e) {
        var val = (el.type === "checkbox") ? String(el.checked) : el.value;
        sendEvent(el.id, "input", { value: val });
      });
    });

    // Form submit events
    var forms = container.querySelectorAll("form[id]");
    forms.forEach(function(el) {
      el.addEventListener("submit", function(e) {
        e.preventDefault();
        var formData = {};
        var elements = el.elements;
        for (var i = 0; i < elements.length; i++) {
          var field = elements[i];
          if (field.name) {
            formData[field.name] = field.value;
          }
        }
        sendEvent(el.id, "submit", { data: formData });
      });
    });
  }

  // --- Initial setup ---
  // Use event delegation for the initial page load
  document.addEventListener("DOMContentLoaded", function() {
    // Bind events on all islands
    var islands = document.querySelectorAll("[data-rune-island]");
    islands.forEach(function(island) {
      bindIslandEvents(island);
    });

    // Also set up delegation for elements outside islands
    document.addEventListener("click", function(e) {
      var target = e.target.closest("button[id], [data-rune-click]");
      if (target) {
        var id = target.id || target.getAttribute("data-rune-click");
        if (id) {
          sendEvent(id, "click", {});
        }
      }
    });

    document.addEventListener("input", function(e) {
      if (e.target.id && (e.target.tagName === "INPUT" || e.target.tagName === "TEXTAREA" || e.target.tagName === "SELECT")) {
        var val = (e.target.type === "checkbox") ? String(e.target.checked) : e.target.value;
        sendEvent(e.target.id, "input", { value: val });
      }
    });
    document.addEventListener("change", function(e) {
      if (e.target.id && e.target.tagName === "INPUT" && (e.target.type === "checkbox" || e.target.type === "radio")) {
        sendEvent(e.target.id, "input", { value: String(e.target.checked) });
      }
    });

    document.addEventListener("submit", function(e) {
      var form = e.target;
      if (form.id && form.tagName === "FORM") {
        e.preventDefault();
        var formData = {};
        var elements = form.elements;
        for (var i = 0; i < elements.length; i++) {
          var field = elements[i];
          if (field.name) {
            formData[field.name] = field.value;
          }
        }
        sendEvent(form.id, "submit", { data: formData });
      }
    });

    // Connect WebSocket
    connect();
  });

})();
