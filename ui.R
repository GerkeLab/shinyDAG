
# UI ----------------------------------------------------------------------

function(request) {
  dashboardPage(
    title = "shinyDAG",
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      shinyjs::useShinyjs(),
      fluidRow(
        # ---- Box: DAG ----
        box(
          title = "shinyDAG",
          column(12, align = "center", uiOutput("tikzOut")),
          # column(12, align = "center", imageOutput("tikzOut")),
          tags$style(
            type = "text/css",
            "#showPreviewContainer { padding-top: 32px; }",
            "#download-buttons { padding-top: 25px; }"
          ),
          fluidRow(
            column(4, tags$div(id = "showPreviewContainer", 
                               prettySwitch("showPreview", "Preview DAG", status = "primary", fill = TRUE))),
            column(4, 
                   selectInput("downloadType", "Type of download",
                               choices = list("PDF" = 4, "PNG" = 3, "LaTeX TikZ" = 2, "dagitty R object" = 1, "ggdag R object" = 5)
                   )
            ),
            column(
              4, 
              div(
                class = "btn-group",
                role = "group",
                id = "download-buttons",
                downloadButton("downloadButton"),
                bookmarkButton()
              )
            )
          ),
          # br(), br(),
          prettySwitch(
            inputId = "showFlow",
            label = "Examine DAG elements",
            status = "primary",
            fill = TRUE
          ),
          conditionalPanel(
            condition = "input.showFlow == 1",
            fluidRow(
              column(6, "Open paths", uiOutput("openPaths")),
              column(6, "Closed paths", uiOutput("closedPaths"))
            )
          )
        ),
        # ---- Box: Controls ----
        tabBox(
          title = div(img(src = "GerkeLab.png", width = 40, height = 40)),
          id = "tab_control",
          # ---- Tab: Build ----
          tabPanel(
            "Build",
            value = "build",
            tags$style(
              type = "text/css",
              "#node_delete { margin-top: 20px; color: #FFF }",
              "#edge_btn { margin-top: 25px; color: #FFF }",
              "#ui_edge_swap_btn { margin-top: 25px; }",
              "@media (min-width: 768px) { #node_delete { margin-left: -25px; } }"
            ),
            uiOutput("nodeListButtonsLabel"),
            uiOutput("nodeListButtons"),
            fluidRow(
              column(10, 
                     searchInput("nodeLabel", label = "", value = "", placeholder = NULL,
                                 btnSearch = icon("check"), btnReset = icon("backspace"), width = "100%")
              ),
              column(2, uiOutput("node_ui_remove"))
            ),
            plotOutput("clickPad", click = "pad_click", dblclick = "pad_dblclick"),
            fluidRow(
              column(
                width = 5, 
                selectizeInput(
                  "from_edge", "Parent Node", 
                  width = "100%",
                  choices = c("Add a node to the plot area" = "")
                )
              ),
              column(
                width = 1, 
                actionButton("ui_edge_swap_btn", "", icon("exchange-alt"))
              ),
              column(
                width = 5, 
                selectizeInput(
                  "to_edge", "Child Node", 
                  width = "100%",
                  choices = c("Add a node to the plot area" = "")
                )
              ),
              column(
                width = 1, 
                uiOutput("ui_edge_btn")
              )
            ),
            checkboxGroupInput("adjustNode", "Select nodes to adjust", inline = TRUE),
            radioButtons("exposureNode", "Exposure", choices = c("None" = ""), inline = TRUE),
            radioButtons("outcomeNode", "Outcome", choices = c("None" = ""), inline = TRUE)
          ),
          # ---- Tab: Edit Aesthetics ----
          tabPanel(
            "Edit aesthetics",
            value = "edit_edge_aesthetics",
            selectInput("arrowShape", "Select arrow head", choices = c(
              "stealth", "stealth'", "diamond",
              "triangle 90", "hooks", "triangle 45",
              "triangle 60", "hooks reversed", "*"
            ), selected = "stealth"),
            uiOutput("curveAngle"),
            helpText("A negative degree will change the orientation of the curve."),
            fluidRow(
              column(4, uiOutput("curveColor")),
              column(4, uiOutput("curveLty")),
              column(4, uiOutput("curveThick"))
            )
          ),
          # ---- Tab: Edit LaTeX ----
          tabPanel(
            "Edit LaTeX",
            value = "edit_latex",
            helpText("WARNING: Editing code here will only change the appearance of the DAG and not the information on paths provided."),
            uiOutput("texEdit"),
            actionButton("redoTex", "Initiate Editing!"),
            conditionalPanel(
              condition = "input.redoTex == 1",
              uiOutput("tikzOutNew"),
              selectInput("downloadType2", "Type of download",
                          choices = list("PDF" = 3, "PNG" = 2, "LaTeX TikZ" = 1)
              ),
              downloadButton("downloadButton2")
            )
          ),
          # ---- Tab: About ----
          tabPanel(
            "About shinyDAG",
            value = "about",
            h6("Development Team: Jordan Creed, Travis Gerke, and Garrick Aden-Buie"),
            h6("For more information on our lab and other projects please check out our website at", tags$a(href = "http://gerkelab.com", "gerkelab.com")),
            h6("All code is available on GitHub at", tags$a(href = "https://github.com/GerkeLab/ShinyDAG", "GerkeLab/ShinyDag")),
            h6("Any errors or comments can be directed to", 
               tags$a(href = "mailto:travis.gerke@moffitt.org", "travis.gerke@moffitt.org"), 
               "or", tags$a(href = "mailto:jordan.h.creed@moffitt.org", "jordan.h.creed@moffitt.org")
            )
          )
        )
      )
    )
  )
}