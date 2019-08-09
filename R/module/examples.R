add_slug <- function(ex) {
  ex %>% 
    purrr::map(~ {
      .x$slug <- gsub("[.]rds$", "", .x$file, ignore.case = TRUE)
      .x
    })
}

keep_ex_with_file <- function(ex) {
  ex %>% 
    purrr::keep(~ file.exists(.$file))
}

nullify_missing <- function(ex, field = "image") {
  ex %>% 
    purrr::modify_depth(
      .depth = 1,
      ~ purrr::modify_at(., field, ~ {
        if (!file.exists(.x)) list(NULL) else .x
      })
    )
}

full_path <- function(ex, field, path = file.path("www", "examples")) {
  ex %>% 
    purrr::modify_depth(
      .depth = 1,
      ~ purrr::modify_at(., field, ~ file.path(path, .x))
    )
}

rel_path <- function(ex, field, path = file.path("www/")) {
  ex %>% 
    purrr::modify_depth(
      .depth = 1,
      ~ purrr::modify_at(., field, ~ if (!is.null(.x)) sub(path, "", .x, fixed = TRUE) else list(NULL))
    )
}

load_example_values <- function(ex) {
  purrr::map(ex, ~ {
    values <- readRDS(.x$file)
    .x$values <- list()
    .x$values$nodes <- values$rvn$nodes
    .x$values$edges <- values$rve$edges
    .x
  })
}

load_examples <- function(path = file.path("www", "examples")) {
  ex_yaml <- file.path(path, "examples.yml")
  if (!file.exists(ex_yaml)) {
    stop("Unable to locate ", ex_yaml)
  }
  
  ex <- yaml::read_yaml(ex_yaml)
  
  ex %>% 
    add_slug() %>% 
    full_path("image", path) %>% 
    full_path("file", path) %>% 
    keep_ex_with_file() %>% 
    nullify_missing("image") %>% 
    rel_path("image") %>% 
    load_example_values()
}


EXAMPLES <- load_examples()

examples_UI <- function(id) {
  ns <- NS(id)
  
  make_examples_ui <- function(name, description, slug, image = NULL, ...) {
    tagList(
      tags$h3(name),
      if (!is.null(image)) tags$div(
        class = "example-image", 
        tags$img(src = image)
      ),
      tags$p(
        HTML(description)
      ),
      actionButton(ns(slug), "Load Example")
    )
  }
  
  tagList(
    EXAMPLES %>% 
      purrr::map(`[`, c("name", "description", "slug", "image")) %>% 
      purrr::map(~ purrr::pmap(.x, make_examples_ui))
  )
}

examples <- function(input, output, session) {
  input_ids <- EXAMPLES %>% purrr::map_chr("slug")
  values <- EXAMPLES %>% purrr::map("values")
  names(values) <- input_ids
  
  lagged_value <- setNames(rep(0L, length(input_ids)), input_ids)
  
  example_value <- reactiveVal(NULL)

  observe({
    current_btn_vals <- purrr::map_int(input_ids, ~ input[[.x]])
    req(any(current_btn_vals > 0L))
    # cli::cat_line("lagged: ", lagged_value)
    # cli::cat_line("current: ", current_btn_vals)
    
    idx <- which(current_btn_vals != lagged_value)
    lagged_value <<- current_btn_vals
    
    if (!length(idx)) {
      example_value(NULL)
      return(NULL)
    }
    
    changed_input <- input_ids[idx]
    
    example_value(values[[changed_input]])
  })
  
  return(reactive(example_value()))
}


# ui <- fluidPage(
#   examples_UI("example")
# )
# server <- function(input, output, session){
#   callModule(examples, 'example')
# }
# shinyApp(ui, server)