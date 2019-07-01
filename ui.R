class_3_col <- "col-md-4 col-md-offset-0 col-sm-8 col-sm-offset-2 col-xs-12"

# UI ----------------------------------------------------------------------

function(request) {
  dashboardPage(
    title = "shinyDAG",
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      shinyjs::useShinyjs(),
      tags$script(src = "shinydag.js", async = TRUE),
      includeCSS("www/shinydag.css"),
      fluidRow(
        
        tabBox(# ---- Box: Controls ----
          title = div(img(src = "GerkeLab.png", width = 40, height = 40)),
          width = "12 col-md-push-6 col-md-6 col-lg-7 col-lg-push-5",
          id = "tab_control",
          # ---- Tab: Build ----
          tabPanel(
            "Build",
            value = "build",
            uiOutput("nodeListButtonsLabel"),
            tags$div(
              class = "btn-toolbar",
              role = "toolbar",
              uiOutput("nodeListButtons"),
              # uiOutput("nodeListActions")
              tags$div(
                class = "btn-group",
                role = "group",
                tags$div(
                  id = "node_list_actions_deselected",
                  actionButton("node_list_node_add", "", icon("plus")),
                  shinyjs::hidden(
                    actionButton(
                      "node_list_node_remove", "", icon("eraser"),
                      alt = "Remove from DAG",
                      `data-toggle` = "tooltip",
                      `data-placement` = "bottom",
                      title = "Remove from DAG")
                  ),
                  shinyjs::hidden(
                    actionButton(
                      "node_list_node_delete", "", icon("trash"),
                      alt = "Delete Node",
                      `data-toggle` = "tooltip",
                      `data-placement` = "bottom",
                      title = "Delete Node")
                  )
                )
              )
            ),
            shinyjs::hidden(
              fluidRow(
                id = "node_list_node_name_container",
                column(
                  12,
                  textInput("node_list_node_name", "", width = "100%")
                )
              )
            ),
            plotOutput("clickPad", click = "pad_click", dblclick = "pad_dblclick", height = "600px", width = "100%"),
            fluidRow(
              tags$div(
                class = "col-xs-9 col-sm-6 col-sm-offset-2 col-md-4 col-md-offset-0",
                selectizeInput(
                  "from_edge",
                  "Parent Node",
                  width = "100%",
                  choices = c("Add a node to the plot area" = "")
                )
              ),
              tags$div(
                class = "col-xs-3 col-md-2",
                actionButton("ui_edge_swap_btn", "", icon("exchange-alt"))
              ),
              tags$div(
                class = "col-xs-9 col-sm-6 col-sm-offset-2 col-md-4 col-md-offset-0",
                selectizeInput(
                  "to_edge",
                  "Child Node",
                  width = "100%",
                  choices = c("Add a node to the plot area" = "")
                )
              ),
              tags$div(
                class = "col-xs-3 col-md-2",
                uiOutput("ui_edge_btn")
              )
            ),
            fluidRow(
              tags$div(
                class = class_3_col,
                shinyjs::disabled(
                  selectInput("exposureNode", "Exposure", choices = c("None" = ""), width = "100%")
                )
              ),
              tags$div(
                class = class_3_col,
                shinyjs::disabled(
                  selectInput("outcomeNode", "Outcome", choices = c("None" = ""), width = "100%")
                )
              ),
              tags$div(
                class = class_3_col,
                shinyjs::disabled(
                  selectizeInput("adjustNode", "Adjust for...", choices = c("None" = ""), width = "100%", multiple = TRUE)
                )
              )
            ),
            fluidRow(
              tags$div(
                class = "col-sm-12",
                uiOutput("openExpOutcomePaths")
              )
            )
          ),
          # ---- Tab: Edit Aesthetics ----
          tabPanel(
            "Edit aesthetics",
            value = "edit_edge_aesthetics",
            selectInput(
              "arrowShape",
              "Select arrow head",
              choices = c(
                "stealth",
                "stealth'",
                "diamond",
                "triangle 90",
                "hooks",
                "triangle 45",
                "triangle 60",
                "hooks reversed",
                "*"
              ),
              selected = "stealth"
            ),
            uiOutput("edge_aes_ui")
          ),
          # ---- Tab: Edit LaTeX ----
          tabPanel(
            "Edit LaTeX",
            value = "edit_latex",
            tags$p(
              "Use this tab to manually edit the TikZ generated by shinyDAG."
            ),
            helpText(
              "Note that changes made to the TikZ code below will not affect",
              "the DAG settings in the app. Changes made to the DAG elsewhere",
              "in shinyDAG will overwrite any changes made to the manually",
              "edited TikZ code below."
            ),
            uiOutput("texEdit")
          ),
          # ---- Tab: About ----
          tabPanel(
            "About shinyDAG",
            value = "about",
            h6("Development Team: Jordan Creed, Travis Gerke, and Garrick Aden-Buie"),
            h6(
              "For more information on our lab and other projects please check",
              "out our website at",
              tags$a(href = "http://gerkelab.com", "gerkelab.com")
            ),
            h6(
              "All code is available on GitHub at",
              tags$a(
                href = "https://github.com/GerkeLab/ShinyDAG",
                "GerkeLab/ShinyDag"
              )
            ),
            h6(
              "Any errors or comments can be directed to",
              tags$a(href = "mailto:travis.gerke@moffitt.org", "travis.gerke@moffitt.org"),
              "or",
              tags$a(href = "mailto:jordan.h.creed@moffitt.org", "jordan.h.creed@moffitt.org")
            )
          )
        ), # Controls Box Ends ----
        
        box( # ---- Box: DAG ----
          title = "shinyDAG",
          width = "12 col-md-pull-6 col-md-6 col-lg-5 col-lg-pull-7",
          column(
            width = 12, 
            align = "center", 
            shinyjs::hidden(tags$div(
              id = "tikzOut-help",
              class="alert alert-danger",
              role="alert",
              HTML(
                "<p>An error occurred while compiling the preview.",
                "Are there syntax errors in your labels?",
                "Single <code>$</code> need to be escaped: <code>\\$</code>.</p>"
              )
            )),
            uiOutput("tikzOut")
          ),
          fluidRow(
            tags$div(
              class = class_3_col,
              tags$div(
                id = "showPreviewContainer",
                prettySwitch("showPreview", "Preview DAG", status = "primary", fill = TRUE),
                uiOutput("showPreview_helptext")
              )
            ),
            tags$div(
              class = class_3_col,
              selectInput(
                "downloadType",
                "Type of download",
                choices = list(
                  "PDF" = "pdf",
                  "PNG" = "png",
                  "LaTeX TikZ" = "tikz",
                  "dagitty (R: RDS)" = "dagitty",
                  "ggdag (R: RDS)" = "ggdag"
                )
              ),
              uiOutput("downloadType_helptext")
            ),
            tags$div(
              class = class_3_col,
              div(
                class = "btn-group",
                role = "group",
                id = "download-buttons",
                downloadButton("downloadButton"),
                bookmarkButton(label = "Bookmark")
              )
            )
          )
        ) # DAG box ends ----
      )
    )
  )
}
