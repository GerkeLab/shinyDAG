library(shiny)
library(shinydashboard)
library(DiagrammeR)
library(dagitty)
library(igraph)
library(texPreview)
library(shinyAce)
library(shinyBS)
library(dplyr)
library(ggdag)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)
library(shinyThings)
source("R/node.R")
source("R/edge.R")
source("R/columns.R")
source("R/module/clickpad.R")
source("R/module/dagPreview.R")
source("R/module/examples.R")
source("R/xcolorPicker.R")
source("R/aes_ui.R")
# Additional libraries: tidyr, digest, rlang

enableBookmarking(store = "server")

tex_opts$set(list(
  density = 1200,
  margin = list(left = 0, top = 0, right = 0, bottom = 0),
  cleanup = c("aux", "log")
))



# Functions ---------------------------------------------------------------

DEBUG <- getOption("shinydag.debug", FALSE)
debug_input <- function(x, x_name = NULL) {
  if (!isTRUE(DEBUG)) return()

  if (is.null(x)) {
    cat(if (!is.null(x_name)) paste0(x_name, ":"), "NULL", "\n")
  } else if (inherits(x, "igraph")) {
    cat(capture.output(print(x)), "", sep = "\n")
  } else if (length(x) == 1 && !is.list(x)) {
    cat(if (!is.null(x_name)) paste0(x_name, ":"), if (length(names(x))) names(x), "-", x, "\n")
  } else if (is.list(x) && length(x) == 0) {
    cat(if (!is.null(x_name)) paste0(x_name, ":"), "list()", "\n")
  } else {
    if (!inherits(x, "data.frame")) x <- tibble::enframe(x)
    cat(if (!is.null(x_name)) paste0(x_name, ":"), knitr::kable(x), "", sep = "\n")
  }
}
debug_line <- function(...) {
  if (!isTRUE(DEBUG)) return()
  cli::cat_line(...)
}


buildUsepackage <- if (length(find("build_usepackage"))) texPreview::build_usepackage else texPreview::buildUsepackage

# use y if x is.null
`%||%` <- function(x, y) if (is.null(x)) y else x
# use y if x is not null(ish) (otherwise NULL)
`%??%` <- function(x, y) if (!is.null(x) && x != "") y

warnNotification <- function(...) showNotification(
  paste0(...), duration = 5, closeButton = TRUE, type = "warning"
)

invertNames <- function(x) setNames(names(x), unname(x))

# String utilities ----

str_and <- function(...) {
  x <- c(...)
  last <- if (length(x) > 2) ", and " else " and "
  glue::glue_collapse(x, sep = ", ", last = last)
}

str_plural <- function(x, word, plural = paste0(word, "s")) {
  if (length(x) > 1) plural else word
}
