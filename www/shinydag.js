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

// Helper function to create SVG elements
// ref: https://stackoverflow.com/a/22555293
var svgNS = "http://www.w3.org/2000/svg";
const new_svg = width => {
  var svg_el = document.createElementNS(svgNS, "svg");
  svg_el.setAttributeNS(null, "viewBox", "0 0 100 100");
  svg_el.setAttributeNS(null, "width", width);
  return svg_el;
};

const circle_svg = () => {
  // create a simple SVG circle shape
  var svg = new_svg("15px");
  var circle = document.createElementNS(svgNS, "circle");
  circle.setAttributeNS(null, "id", "edge-selector-hint-circle");
  circle.setAttributeNS(null, "cx", "50%");
  circle.setAttributeNS(null, "cy", "50%");
  circle.setAttributeNS(null, "r", "45%");
  circle.setAttributeNS(null, "fill", "#ddd");
  circle.setAttributeNS(null, "stroke", "#999");
  svg.appendChild(circle);
  return svg;
};

const triangle_svg = () => {
  // create a simple SVG triangle shape
  var svg = new_svg("15px");
  var triangle = document.createElementNS(svgNS, "polygon");
  triangle.setAttributeNS(null, "id", "edge-selector-hint-triangle");
  triangle.setAttributeNS(null, "points", "0,100 50,10 100,100");
  triangle.setAttributeNS(null, "fill", "#ddd");
  triangle.setAttributeNS(null, "stroke", "#999");
  svg.appendChild(triangle);
  return svg;
};

const append_edge_selector_hint = () => {
  // Add shape hints to edge selection labels because can't from inside Shiny
  var helper_from = document.createElement("span");
  helper_from.appendChild(triangle_svg());
  helper_from.className = "edge-selector-hint";
  
  var helper_to = document.createElement("span");
  helper_to.appendChild(circle_svg());
  helper_to.className = "edge-selector-hint";
  
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
