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
  has_position <- sapply(nodes, function(x) !is.na(x$x))
  x[has_position]
}

node_name_from_hash <- function(nodes, hash) {
  invertNames(node_names(nodes))[hash]
}

node_update <- function(nodes, hash, name = NULL, x = NULL, y = NULL) {
  nodes[[hash]]$name <- name %||% nodes[[hash]]$name
  nodes[[hash]]$x <- x %||% nodes[[hash]]$x
  nodes[[hash]]$y <- y %||% nodes[[hash]]$y
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
    mutate(visible = !is.na(x), in_dag = x > 0)
  if (full) {
    return(x)
  }
  filter(x, in_dag)
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