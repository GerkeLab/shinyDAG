const nodelabel = document.querySelector("#nodeLabel_text")
const nodelabel_reset = document.querySelector("#nodeLabel_reset")

const toggle_reset_icon = (v = null, el) => {
  icon = el.getElementsByTagName("i")[0]
  if (v) {
    icon.className = "fa fa-backspace"
  } else {
    icon.className = "fa fa-plus"
  }
}

nodelabel.addEventListener("input", e => {
  toggle_reset_icon(e.srcElement.value, nodelabel_reset)
})
