const toggle_reset_icon = (v = false, id) => {
  const icon = document.getElementById(id).getElementsByTagName("i")[0];
  if (v) {
    icon.className = "fa fa-backspace";
  } else {
    icon.className = "fa fa-plus";
  }
};

const set_input_focus = (id) => {
  const el = document.getElementById(id);
  if (el) {
    el.focus();
  }
};

const append_edge_selector_hint = () => {
  var helper_from = document.createElement("span");
  helper_from.innerHTML = "&#x25fc;";
  helper_from.className = "edge-selector-hint-from";
  
  var helper_to = document.createElement("span");
  helper_to.innerHTML = "&#x2b24;";
  helper_to.className = "edge-selector-hint-to";
  
  $("label[for='from_edge-selectized']").after(helper_from);
  $("label[for='to_edge-selectized']").after(helper_to);
};

const wrap_btn_text_in_span = (id, text) => {
  var $el = $("#" + id);
  $el.html([$el.children()[0], "<span class='btn-text'>" + text + "</span>"]);
};

$( document ).ready(function() {
  setTimeout(append_edge_selector_hint, 1000);
  setTimeout(function() {wrap_btn_text_in_span("downloadButton", "Download")}, 1000);
  setTimeout(function() {wrap_btn_text_in_span("\\._bookmark_", "Bookmark")}, 1000);
});

// Block name change updates while Shiny is re-rendering to avoid wonkiness
var text_input_timeout;
$(document).on("shiny:busy", (e) => { 
  text_input_timeout = setTimeout(() => {
    $("#node_list_node_name").prop("disabled", true);
  }, 500);
});
$(document).on("shiny:idle", (e) => { 
  clearTimeout(text_input_timeout);
  $("#node_list_node_name").prop("disabled", false);
});

// Block node change buttons when updating names to avoid infinite looping wonkiness
// disables buttons when user starts typing in text box
$("#node_list_node_name").keydown(() => { 
  $("#nodeListButtons button").prop("disabled", true);
});

// re-enable buttons when Shiny updates or the text bar loses focus (in case no change)
$(document).on("shiny:value", () => { 
  $("#nodeListButtons button").prop("disabled", false);
});
$("#node_list_node_name").blur(() => { 
  $("#nodeListButtons button").prop("disabled", false);
});
