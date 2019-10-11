# ---- Node Helper Functions ----
node_new <- function(nodes, hash, name, gap_y = 0.75, min_y = 1) {
  # new nodes are added into the clickpad area but with x = -0.75
  # need to check if there are other nodes in the holding space and adjust y
  taken_y <- nodes %>% purrr::keep(~ !is.na(.$y)) %>% purrr::map_dbl(`[[`, "y")
  new_y <- find_new_y(taken_y, gap_y, min_y)
  nodes[[hash]] <- list(name = name, x = -0.75, y = new_y)
  nodes
}

find_new_y <- function(y, gap_y = 0.75, min_y = 0.5) {
  if (!length(y)) return(min_y)
  if (min(y) >= (min_y + gap_y)) return(min_y)
  if (length(y) == 1) return(y + gap_y)
  
  y <- sort(y)
  
  gap_size <- c(lead(y) - y)[-length(y)]
  if (any(gap_size >= 2 * gap_y)) {
    first_gap <- which(gap_size > 2 * gap_y)[1]
    return(y[first_gap] + gap_y)
  }
  
  max(y) + gap_y
}

node_name_valid <- function(nodes, name, warn = FALSE) {
  if (!nzchar(name)) {
    warnNotification("Please specify a name for the node")
    return(FALSE)
  }
  name_in_nodes <- vapply(nodes, function(n) name == n$name, FALSE)
  if (any(name_in_nodes)) {
    if (warn) warnNotification('"', name, '" is already the name of a node')
    FALSE
  } else {
    TRUE
  }
}

node_names <- function(nodes, all = FALSE) {
  if (!length(nodes)) {
    return(character())
  }
  x <- invertNames(sapply(nodes, function(x) x$name))
  if (all) {
    return(x)
  }
  in_dag <- sapply(nodes, function(n) n$x >= 0)
  x[in_dag]
}

node_name_from_hash <- function(nodes, hash) {
  invertNames(node_names(nodes))[hash]
}

node_update <- function(nodes, hash, name = NULL, x = NULL, y = NULL, name_latex = NULL) {
  # in general update a property if arg is not null, default to current value
  nodes[[hash]]$name <- name %||% nodes[[hash]]$name
  nodes[[hash]]$x <- x %||% nodes[[hash]]$x
  nodes[[hash]]$y <- y %||% nodes[[hash]]$y
  # for name_latex precedence is arg > name (arg) > existing > name (existing)
  nodes[[hash]]$name_latex <- name_latex %||% (
    (name %??% name) %>% escape_quotes() %>% escape_latex()
  ) %||% nodes[[hash]]$name_latex %||% (
    (nodes[[hash]]$name %??% nodes[[hash]]$name) %>% escape_quotes() %>% escape_latex()
  )
  nodes
}

node_set_attribute <- function(nodes, hash, attribs) {
  for (node in names(nodes)) {
    for (attrib in attribs) {
      nodes[[node]][[attrib]] <- node %in% hash
    }
  }
  nodes
}

node_unset_attribute <- function(nodes, hashes, attribs) {
  for (hash in hashes) {
    for (attrib in attribs) {
      nodes[[hash]][[attrib]] <- FALSE
    }
  }
  nodes
}

node_with_attribute <- function(nodes, attrib) {
  if (length(nodes) == 0) return(NULL)
  n <- nodes %>% 
    purrr::map(attrib) %>% 
    purrr::keep(isTRUE)
  if (length(n)) n
}

node_parent <- function(nodes) {
  names(node_with_attribute(nodes, "parent"))
}

node_child <- function(nodes) {
  names(node_with_attribute(nodes, "child"))
}

node_adjusted <- function(nodes) {
  names(node_with_attribute(nodes, "adjusted"))
}

node_delete <- function(nodes, hash) {
  .nodes <- nodes[setdiff(names(nodes), hash)]
  if (length(.nodes)) .nodes else list()
}

node_frame <- function(nodes, full = FALSE) {
  if (!length(nodes)) {
    return(tibble())
  }
  x <- bind_rows(nodes) %>%
    mutate(hash = names(nodes)) %>%
    select(hash, everything()) %>% 
    mutate(visible = !is.na(x), in_dag = x > 0) %>% 
    node_frame_complete()
  if (full) {
    return(x)
  }
  filter(x, in_dag)
}

node_frame_complete <- function(nodes) {
  nodes$adjusted   <- nodes[["adjusted"]]   %||% FALSE
  nodes$color_draw <- nodes[["color_draw"]] %||% "Black"
  nodes$color_fill <- nodes[["color_fill"]] %||% "White"
  nodes$color_text <- nodes[["color_text"]] %||% "Black"
  nodes
}

node_vertices <- function(nodes) {
  v_df <- node_frame(nodes)
  vertices(
    name = v_df$name,
    x = v_df$x,
    y = v_df$y,
    hash = v_df$hash
  )
}

node_nearest <- function(nodes, coordinfo, threshold = 0.5) {
  nodes %>%
    node_frame() %>%
    mutate(dist = (x - coordinfo$x)^2 + (y - coordinfo$y)^2) %>%
    arrange(dist) %>%
    filter(dist <= threshold) %>%
    slice(1) %>%
    select(-dist)
}

nodes_in_dag <- function(nodes, include_staged = FALSE) {
  n <- nodes %>%
    purrr::keep(~ !is.na(.$x))
  
  if (!include_staged) {
    n <- purrr::keep(n, ~ .$x > 0)
  }
  names(n)
}

node_btn_id <- function(node_hash) paste0("node_toggle_", node_hash)
node_btn_get_hash <- function(node_btn_id) sub("node_toggle_", "", node_btn_id, fixed = TRUE)

node_tikz_style <- function(hash, adjusted, color_draw, color_fill, color_text, ...) {
  # B/.style={fill=DarkRed, text=White}
  if (!adjusted && color_fill == "White" && color_text == "Black") {
    return(NA_character_)
  }
  style <- 
    list(
      draw = if (adjusted) color_draw, 
      fill = color_fill, 
      text = color_text
    ) %>% 
    purrr::compact() %>% 
    purrr::imap_chr(~ glue::glue("{.y}={.x}")) %>% 
    paste(collapse = ", ")
  
  glue::glue("{hash}/.style={{{style}}}")
}

node_frame_add_style <- function(nodes) {
  if (!"name_latex" %in% names(nodes)) nodes$name_latex <- ""
  nodes <- node_frame_replace_default(nodes)
  nodes %>% 
    mutate(
      tikz_style = purrr::pmap_chr(nodes, node_tikz_style),
      name_latex = case_when(
        is.na(name_latex) | name_latex == "" ~ escape_latex(name),
        TRUE ~ name_latex
      ),
      tikz_node = case_when(
        !is.na(tikz_style) ~ paste(glue::glue("|[{hash}]| {name_latex}")),
        TRUE ~ name_latex
      )
    )
}

node_frame_replace_default <- function(nodes) {
  tidyr::replace_na(nodes, list(
    child = FALSE,
    exposure = FALSE,
    outcome = FALSE,
    adjusted = FALSE,
    name_latex = "",
    visible = FALSE,
    in_dag = FALSE,
    color_draw = "Black",
    color_fill = "White",
    color_text = "Black"
  ))
}

escape_quotes <- function(x) {
  x %??% gsub("(['\"])", "\\\\\\1", x)
}

escape_latex <- function(x, force = FALSE) {
  if (is.null(x)) return(NULL)
  if (!force && grepl("$", x, fixed = TRUE)) {
    # has at least one dollar sign so we'll try to parse out the math
    x_math <- chunk_math(x)
    is_math <- attr(x_math, "is_math")
    if (is.null(is_math) || !any(is_math)) {
      # no math, just escape the original string
      return(escape_latex(x, force = TRUE))
    }
    x_math[!is_math] <- x_math[!is_math] %>% 
      purrr::map_chr(escape_latex, force = TRUE)
    
    return(paste0(x_math, collapse = ""))
  }
  
  ## escape: # $ % ^ & _ { } 
  ## replace: ~ -> \~{}
  ## replace: \ -> \textbackslash
  ## replace: < > -> \textless \textgreater
  x <- gsub("\\", "\\textbackslash ", x, fixed = TRUE)
  x <- gsub("<", "\\textless ", x, fixed = TRUE)
  x <- gsub(">", "\\textgreater ", x, fixed = TRUE)
  x <- gsub("([#$%^&_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\~{}", x, fixed = TRUE)
  x
}

chunk_math <- function(x) {
  x_s <- strsplit(x, character())[[1]]
  
  idx <- which(grepl("$", x_s, fixed = TRUE))
  if (!length(idx)) {
    return(x)
  }
  # remove \\$ pairs from indexes
  idx_has_escape <- which(grepl("\\", x_s[idx[idx > 1L] - 1L], fixed = TRUE))
  if (length(idx_has_escape)) {
    idx <- idx[-(idx_has_escape + as.integer(any(idx == 1)))]
  }
  
  # only include $ that touch at least one alphanum character
  x_around_dollar <- purrr::map_chr(idx, ~ {
    substr(x, max(0, .x - 1, na.rm = TRUE), min(nchar(x), .x + 1))
  })
  idx_no_adjacent_alpha <- which(!grepl("[[:alnum:]+=*{}.-]", x_around_dollar))
  if (length(idx_no_adjacent_alpha)) {
    idx <- idx[-idx_no_adjacent_alpha]
  }
  if (!length(idx)) {
    return(x)
  }
  
  # finally, find the math chunks
  chunks <- c()
  is_math <- c()
  i <- 1L
  while (i < length(idx)) {
    # idx[i - 1] ... idx[i]-1 -> not math
    # idx[i]...idx[i+1] -> math
    # skip ahead to idx[i + 2]
    idx_not_math <- max(idx[i-1], 0, na.rm = TRUE)
    chunks <- c(
      chunks,
      if (idx_not_math != idx[i] - 1L) substr(x, idx_not_math, idx[i] - 1L),
      substr(x, idx[i], idx[i + 1]),
      if (is.na(idx[i + 2]) & !idx[i + 1] == nchar(x)) {
        substr(x, idx[i + 1] + 1, nchar(x))
      }
    )
    is_math <- c(
      is_math,
      if (idx_not_math != idx[i] - 1L) FALSE,
      TRUE,
      if (is.na(idx[i + 2]) & !idx[i + 1] == nchar(x)) FALSE
    )
    i <- i + 2L
  }
  
  attributes(chunks)$is_math <- is_math
  chunks
}
