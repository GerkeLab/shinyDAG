const toggle_reset_icon = (v = false, id) => {
  const icon = document.getElementById(id).getElementsByTagName("i")[0]
  if (v) {
    icon.className = "fa fa-backspace"
  } else {
    icon.className = "fa fa-plus"
  }
}

const set_input_focus = (id) => {
  const el = document.getElementById(id)
  if (el) {
    el.focus()
  }
}

// Watch #nodeLabel input and make reset icon "backspace" if text is input
const nodelabel = document.querySelector("#nodeLabel_text")
nodelabel.addEventListener("input", e => {
  toggle_reset_icon(e.srcElement.value, "nodeLabel_reset")
})
