class_3_col <- "col-md-4 col-md-offset-0 col-sm-8 col-sm-offset-2 col-xs-12"


# Component Builders ------------------------------------------------------

two_column_flips_on_mobile <- function(left, right, override_width_classes = TRUE) {
  
  left_col_class <- "col-sm-12 col-md-pull-6 col-md-6 col-lg-5 col-lg-pull-7"
  right_col_class <- "col-sm-12 col-md-push-6 col-md-6 col-lg-7 col-lg-push-5"
  
  if (!override_width_classes) {
    right <- tags$div(class = right_col_class, right)
    left  <- tags$div(class = left_col_class,  left)
  } else {
    strip_col_class <- function(x) gsub("col-(xs|sm|md|lg)-\\d{1,2}\\s*", "", x)
    left$attrib$class  <- strip_col_class(left$attrib$class)
    right$attrib$class <- strip_col_class(right$attrib$class)
    
    left$attrib$class  <- paste(left$attrib$class,  left_col_class)
    right$attrib$class <- paste(right$attrib$class, right_col_class)
  }
  
  fluidRow(right, left)
}

col_4 <- function(x) {
  tags$div(class = "col-sm-6 col-md-3", style = "min-height: 80px", x)
}