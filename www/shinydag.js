const set_input_focus = (id) => {
  const el = document.getElementById(id);
  if (el) {
    el.focus();
  }
};

const wrap_btn_text_in_span = (id, text) => {
  var $el = $("#" + id);
  $el.html([$el.children()[0], "<span class='btn-text'>" + text + "</span>"]);
};

$( document ).ready(function() {
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
  $("#shinydag-toolbar-node-list-action button").prop("disabled", true);
});

// re-enable buttons when Shiny updates or the text bar loses focus (in case no change)
$(document).on("shiny:value", () => { 
  $("#shinydag-toolbar-node-list-action button").prop("disabled", false);
});
$("#node_list_node_name").blur(() => { 
  $("#shinydag-toolbar-node-list-action button").prop("disabled", false);
});

// Block undo/redo buttons during Shiny updates as well
var undo_disable_timeout = null;
$("#undo_rv-history_back, #undo_rv-history_forward").on("click", () => {
  undo_disable_timeout = setTimeout(() => {
    $("#undo_rv-history_back").parent().addClass("disable-buttons");
  }, 10);
})

// Bock undo/redo with a delay for general Shiny updates
$(document).on("shiny:busy", () => {
  if (!undo_disable_timeout) {
    undo_disable_timeout = setTimeout(() => {
      $("#undo_rv-history_back").parent().addClass("disable-buttons");
    }, 250);
  }
});

// re-enable undo/redo buttons when Shiny is idle
$(document).on("shiny:idle", () => {
  clearTimeout(undo_disable_timeout);
  undo_disable_timeout = null;
  $("#undo_rv-history_back").parent().removeClass("disable-buttons");
});

// Animate logo when app is busy
var app_busy_timeout;
$(document).on("shiny:busy", e => { 
  app_busy_timeout = setTimeout(() => {
    $(".gerkelab-logo").addClass("gerkelab-spinner"); 
  }, 500);
});
$(document).on("shiny:idle", e => { 
  clearTimeout(app_busy_timeout);
  $(".gerkelab-logo").removeClass("gerkelab-spinner"); 
});
