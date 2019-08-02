source("../node.R")

latex_text <- list(
  list(t = "$m^2$",           e = "$m^2$"),
  list(t = "a $m^2$",         e = "a $m^2$"),
  list(t = "$m^2$ b",         e = "$m^2$ b"),
  list(t = "a $m^2$ b",       e = "a $m^2$ b"),
  list(t = "a $e=$$m^2$ b",   e = "a $e=$$m^2$ b"),
  list(t = "a $$ math",       e = "a \\$\\$ math"),
  list(t = "\\textbackslash", e = "\\textbackslash textbackslash"),
  list(t = "# of",            e = "\\# of"),
  list(t = "$ amount",        e = "\\$ amount"),
  list(t = "my $$ is",        e = "my \\$\\$ is"),
  list(t = "$m $$ m$",        e = "$m $$ m$"),
  list(t = "$$ is $mc^2$",    e = "\\$\\$ is $mc^2$"),
  list(t = "a > b",           e = "a \\textgreater  b"), #<< extra space before b
  list(t = "a < b",           e = "a \\textless  b"), #<< same
  list(t = "a % b",           e = "a \\% b"),
  list(t = "a_b",             e = "a\\_b"),
  list(t = "a & b",           e = "a \\& b"),
  list(t = "a & b \\ c",      e = "a \\& b \\textbackslash  c"), #<< + space
  list(t = "{a}",             e = "\\{a\\}"),
  list(t = "a ~ b",           e = "a \\~{} b")
)

passed_test <- purrr::map_lgl(latex_text, function(x) {
  identical(escape_latex(x$t), x$e)
})

if (all(passed_test)) {
  cat('\nAll (', sum(passed_test), ') tests passed!', sep = '')
} else {
  cat('\nThere were', sum(!passed_test), "failures...")
  purrr::walk(latex_text[!passed_test], function(x) {
    cat("\n'", x$t, "' returned '", escape_latex(x$t), "' not '", x$e, "'", sep = "")
  })
}
