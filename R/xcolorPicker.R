# xcolors list ----

if (!file.exists(here::here("data", "xcolors.cvs"))) {
  message("Getting xcolors color list")
  read_gz <- function(x) readLines(gzcon(url(x)))
  
  xcolors <- 
    list(
      # x11 = "http://www.ukern.de/tex/xcolor/tex/x11nam.def.gz",
      svg = "http://www.ukern.de/tex/xcolor/tex/svgnam.def.gz"
    ) %>% 
    purrr::map(read_gz) %>% 
    purrr::flatten_chr() %>% 
    stringr::str_subset("^(%%|\\\\| )", negate = TRUE) %>% 
    stringr::str_remove("(;%|\\})$") %>% 
    readr::read_csv(col_names = c("color", "r", "g", "b")) %>% 
    arrange(color) %>% 
    readr::write_csv(here::here("data", "xcolors.csv"))
} else {
  xcolors <- 
    here::here("data/xcolors.csv") %>% 
    read.csv(stringsAsFactors = FALSE)
}

# Color Functions ----

choose_dark_or_light <- function(x, black = "#000000", white = "#FFFFFF") {
  # x = color_hex
  color_rgb <- col2rgb(x)[, 1]
  # from https://stackoverflow.com/a/3943023/2022615
  color_rgb <- color_rgb / 255
  color_rgb[color_rgb <= 0.03928] <- color_rgb[color_rgb <= 0.03928]/12.92
  color_rgb[color_rgb > 0.03928] <- ((color_rgb[color_rgb > 0.03928] + 0.055)/1.055)^2.4
  lum <- t(c(0.2126, 0.7152, 0.0722)) %*% color_rgb
  if (lum[1, 1] > 0.179) eval(black) else eval(white)
}

xcolor_style <- function(hex, text, ...) {
  glue::glue('background-color:{hex};color:{text}')
}

# Prep Color List ----

xcolors <- 
  xcolors %>% 
  mutate(
    hex = rgb(r, g, b, maxColorValue = 1),
    text = purrr::map_chr(hex, choose_dark_or_light)
  ) %>% 
  select(color, hex, text)


xcolors_list <- xcolors$color
names(xcolors_list) <- purrr::pmap_chr(xcolors, xcolor_style)

xcolor_label <- function(value) {
  xcolors %>% filter(color == value) %>% purrr::pmap_chr(xcolor_style)
}

# xcolorPicker() ----

xcolorPicker <- function(inputId, label = NULL, selected = NULL, ...) {
  selectizeInput(
    inputId,
    label = label,
    choices = c("", xcolors_list),
    multiple = FALSE,
    selected = selected,
    options = list(
      searchField = "value",
      render = I(
        '{
          item: (item, escape) => `<div>${escape(item.value)}</div>`,
          option: (item, escape) => `<div style="${item.label};line-height: 2.5em">${escape(item.value)}</div>`
        }'
      ))
  )
}