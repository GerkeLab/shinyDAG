const toggle_reset_icon = (v = false, id) => {
  const icon = document.getElementById(id).getElementsByTagName("i")[0]
  if (v) {
    icon.className = "fa fa-backspace"
  } else {
    icon.className = "fa fa-plus"
  }
};

const set_input_focus = (id) => {
  const el = document.getElementById(id)
  if (el) {
    el.focus()
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