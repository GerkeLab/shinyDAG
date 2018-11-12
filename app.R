library(shiny)
library(shinydashboard)
library(DiagrammeR)
library(dagitty)
library(stringr)
library(igraph)
library(texPreview)
library(shinyAce)
library(dplyr)
library(ggdag)
library(shinyWidgets)

tex_opts$set(list(
  density = 1200,
  margin = list(left = 0, top = 0, right = 0, bottom = 0),
  cleanup = c("aux", "log")
))

buildUsepackage <- if (length(find("build_usepackage"))) texPreview::build_usepackage else texPreview::buildUsepackage

# UI ----------------------------------------------------------------------

ui <- dashboardPage(
  title = "shinyDAG",
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(
        title = "shinyDAG",
        column(12, align = "center", uiOutput("tikzOut")),
        selectInput("downloadType", "Type of download",
          choices = list("PDF" = 4, "PNG" = 3, "Latex Tikz" = 2, "dagitty R object" = 1, "ggdag R object" = 5)
        ),
        downloadButton("downloadButton"),
        br(), br(),
        prettySwitch(
          inputId = "showFlow",
          label = "Examine DAG elements",
          status = "primary",
          fill = TRUE
        ),
        conditionalPanel(
          condition = "input.showFlow == 1",
          # textOutput("adjustText"),
          # verbatimTextOutput("adjustSets"),
          fluidRow(
            column(6, "Open paths", verbatimTextOutput("openPaths")),
            column(6, "Closed paths", verbatimTextOutput("closedPaths"))
          )
        )
      ),
      tabBox(
        title = div(img(src = "GerkeLab.png", width = 40, height = 40)),
        tabPanel(
          "Build",
          tags$style(
            type = "text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
          ),
          textInput("nodeLabel", "To add a node: type a label and click the grid"),
          checkboxInput("clickType", "Click to remove a node", value = FALSE),
          plotOutput("clickPad",
            click = "click1"
          ),
          fluidRow(
            column(6, uiOutput("fromEdge")),
            column(6, uiOutput("toEdge"))
          ),
          actionButton("edgeButton1", "Add edge!"),
          actionButton("edgeButton2", "Remove edge!"),
          uiOutput("adjustNodeCreate"),
          uiOutput("exposureNodeCreate"),
          uiOutput("outcomeNodeCreate")
        ),
        tabPanel(
          "Edit aesthetics",
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
        tabPanel(
          "Edit LaTex",
          helpText("WARNING: Editing code here will only change the appearance of the DAG and not the information on paths provided."),
          uiOutput("texEdit"),
          actionButton("redoTex", "Initiate Editing!"),
          conditionalPanel(
            condition = "input.redoTex == 1",
            uiOutput("tikzOutNew"),
            selectInput("downloadType2", "Type of download",
              choices = list("PDF" = 3, "PNG" = 2, "Latex Tikz" = 1)
            ),
            downloadButton("downloadButton2")
          )
        ),
        tabPanel(
          "About shinyDAG",
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


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # ---- Session Temp Directory ----
  SESSION_TEMPDIR <- file.path("www", session$token)
  dir.create(SESSION_TEMPDIR)
  onSessionEnded(function() {
    message("Removing session tempdir: ", SESSION_TEMPDIR)
    unlink(SESSION_TEMPDIR, recursive = TRUE)
  })
  message("Using session tempdir: ", SESSION_TEMPDIR)


  
  # ---- Reactive Values ----
  rv <- reactiveValues(
    g    = make_empty_graph(),
    pts  = list(x = vector("numeric", 0), y = vector("numeric", 0), name = vector("character", 0)),
    pts2 = as.data.frame(cbind(x = rep(1:7, each = 7), y = rep(1:7, 7), name = rep(NA, 49)))
  )

  # ---- Click Pad ----
  # adding/removing points/nodes on clickPad
  observeEvent(input$click1, {
    if (input$nodeLabel %in% rv$pts$name) {
      showNotification(
        "Unpredictable Behavior: duplicate names",
        duration = 5,
        closeButton = TRUE, type = "warning"
      )
    }

    if (input$clickType == FALSE & input$nodeLabel != "") {
      # Add points
      rv$pts$x <- c(rv$pts$x, round(input$click1$x))
      rv$pts$y <- c(rv$pts$y, round(input$click1$y))
      rv$pts$name <- c(rv$pts$name, input$nodeLabel)
      rv$pts2$name <- ifelse(
        round(input$click1$x) == rv$pts2$x & round(input$click1$y) == rv$pts2$y,
        input$nodeLabel, 
        rv$pts2$name
      )
      
      # Add Nodes on DAG
      rv$g <- rv$g %>% 
        add_vertices(
          1,
          name = input$nodeLabel,
          x = round(input$click1$x),
          y = round(input$click1$y),
          color = "white",
          shape = "none"
        )
    } else if (input$clickType == TRUE) {
      # Remove Point
      rmPoint <- intersect(grep(round(input$click1$x), rv$pts$x), grep(round(input$click1$y), rv$pts$y))
      if (length(rmPoint) > 0) {
        rv$pts$x[[rmPoint]] <- NA
        rv$pts$y[[rmPoint]] <- NA
        rv$pts$name[[rmPoint]] <- NA
        rv$pts2$name <- ifelse(
          round(input$click1$x) == rv$pts2$x & round(input$click1$y) == rv$pts2$y,
          NA, 
          rv$pts2$name
        )
      }
      
      # Remove Node
      rmNode <- intersect(grep(round(input$click1$x), V(rv$g)$x), grep(round(input$click1$y), V(rv$g)$y))
      if (length(rmNode) > 0) {
        rmNode <- V(rv$g)$name[[rmNode]]
        rv$g <- rv$g %>% delete_vertices(rmNode)
      }
    }
    updateTextInput(session, "nodeLabel", value = "")
  })

  # clickPad display
  output$clickPad <- renderPlot({
    if (length(rv$pts$x >= 1)) {
      plot(rv$pts$x, rv$pts$y, xlim = c(1, 7), ylim = c(1, 7), bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i", col = "white")
      text(rv$pts$x, rv$pts$y, labels = rv$pts$name, cex = 2)
      grid()
    } else {
      plot(rv$pts$x, rv$pts$y, xlim = c(1, 7), ylim = c(1, 7), bty = "n", xaxt = "n", yaxt = "n", ylab = "", xlab = "", xaxs = "i")
      grid()
    }
  })

  # ---- Node - Options ----
  output$adjustNodeCreate <- renderUI({
    checkboxGroupInput("adjustNode", "Select nodes to adjust",
      choices = rv$pts$name[!is.na(rv$pts$name)],
      inline = TRUE
    )
  })

  output$exposureNodeCreate <- renderUI({
    checkboxGroupInput("exposureNode", "Exposure",
      choices = rv$pts$name[!is.na(rv$pts$name)],
      inline = TRUE
    )
  })

  output$outcomeNodeCreate <- renderUI({
    checkboxGroupInput("outcomeNode", "Outcome",
      choices = rv$pts$name[!is.na(rv$pts$name)],
      inline = TRUE
    )
  })

  output$adjustText <- renderText({
    if (is.null(input$exposureNode) & is.null(input$outcomeNode)) {
      paste0("Minimal sufficient adjustment sets")
    } else {
      paste0(
        "Minimal sufficient adjustment set(s) to estimate the effect of ",
        input$exposureNode, " on ", input$outcomeNode
      )
    }
  })
  
  # ---- Edges - Add/Remove ----
  output$fromEdge <- renderUI({
    selectInput("fromEdge2", "Parent node", choices = c("---", rv$pts$name[!is.na(rv$pts$name)]))
  })

  output$toEdge <- renderUI({
    selectInput("toEdge2", "Child node", choices = c("---", rv$pts$name[!is.na(rv$pts$name)]))
  })

  # add/remove edges to DAG
  observeEvent(input$edgeButton1, {
    if (input$fromEdge2 %in% V(rv$g)$name & input$toEdge2 %in% V(rv$g)$name) {
      rv$g <- rv$g %>%
        add_edges(c(input$fromEdge2, input$toEdge2)) %>%
        set_edge_attr("color", value = "black")
    }
  })

  observeEvent(input$edgeButton2, {
    if (input$fromEdge2 %in% V(rv$g)$name & input$toEdge2 %in% V(rv$g)$name) {
      rv$g <- rv$g %>%
        delete_edges(paste0(input$fromEdge2, "|", input$toEdge2))
    }
  })

  # ---- DAG Diagnostics ----
  output$adjustSets <- renderPrint({
    if (!is.null(input$exposureNode) & !is.null(input$outcomeNode)) {
      daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
      daggityCode1 <- paste(daggityCode1, collapse = ";")
      daggityCode2 <- paste0("dag { ", daggityCode1, " }")

      g2 <- dagitty(daggityCode2)

      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode

      adjustResults <- adjustmentSets(g2)
      return(adjustResults)
    } else {
      return(print("Please indicate exposure and outcome"))
    }
  })

  output$condInd <- renderPrint({
    daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
    daggityCode1 <- paste(daggityCode1, collapse = ";")
    daggityCode2 <- paste0("dag { ", daggityCode1, " }")

    g2 <- dagitty(daggityCode2)

    exposures(g2) <- input$exposureNode
    outcomes(g2) <- input$outcomeNode
    adjustedNodes(g2) <- input$adjustNode

    test <- impliedConditionalIndependencies(g2)

    return_list <- vector("character", 0)
    for (i in 1:length(test)) {
      return_list <- c(return_list, paste0(test[[i]]$X, " is independent of ", test[[i]]$Y, " given: ", paste0(test[[i]]$Z, collapse = " and ")))
    }
    return(cat(return_list, sep = "\n")) # } else{return(print(""))}
  })

  output$openPaths <- renderPrint({
    if (!is.null(input$exposureNode) & !is.null(input$outcomeNode)) {
      daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
      daggityCode1 <- paste(daggityCode1, collapse = ";")
      daggityCode2 <- paste0("dag { ", daggityCode1, " }")

      g2 <- dagitty(daggityCode2)

      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode
      adjustedNodes(g2) <- input$adjustNode

      allComb <- as.data.frame(combn(names(g2), 2))

      pathData <- list(path = vector("character", 0), open = vector("character", 0))
      for (i in 1:ncol(allComb)) {
        pathResults <- paths(g2, from = allComb[1, i], to = allComb[2, i], Z = input$adjustNode)
        pathData$path <- c(pathData$path, pathResults$paths)
        pathData$open <- c(pathData$open, pathResults$open)
      }

      openPaths <- grep("TRUE", pathData$open)

      return(cat(pathData$path[openPaths][str_count(pathData$path[openPaths], "-") >= 1], sep = "\n"))
    } else {
      return(print(""))
    }
  })

  output$closedPaths <- renderPrint({
    if (!is.null(input$exposureNode) & !is.null(input$outcomeNode)) {
      daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
      daggityCode1 <- paste(daggityCode1, collapse = ";")
      daggityCode2 <- paste0("dag { ", daggityCode1, " }")

      g2 <- dagitty(daggityCode2)

      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode
      adjustedNodes(g2) <- input$adjustNode

      allComb <- as.data.frame(combn(names(g2), 2))

      pathData <- list(path = vector("character", 0), open = vector("character", 0))
      for (i in 1:ncol(allComb)) {
        pathResults <- paths(g2, from = allComb[1, i], to = allComb[2, i], Z = input$adjustNode)
        pathData$path <- c(pathData$path, pathResults$paths)
        pathData$open <- c(pathData$open, pathResults$open)
      }

      closedPaths <- grep("FALSE", pathData$open)

      return(cat(pathData$path[closedPaths][str_count(pathData$path[closedPaths], "-") >= 1], sep = "\n"))
    } else {
      return(print(""))
    }
  })
  
  # ---- Appearance Interface ----
  output$curveAngle <- renderUI({
    g <- rv$g
    req(ends(g, E(g)))
    if (length(ends(g, E(g))[, 1]) >= 1) {
      lapply(1:length(ends(g, E(g))[, 1]), function(i) {
        sliderInput(paste0("angle", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2]), paste0("Angle for ", paste0(ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2])),
          min = -180, max = 180, value = ifelse(is.null(input[[paste0("angle", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2])]]), 0, input[[paste0("angle", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2])]])
        )
      })
    }
  })

  output$curveColor <- renderUI({
    g <- rv$g
    req(ends(g, E(g)))
    lapply(1:length(ends(g, E(g))[, 1]), function(i) {
      textInput(paste0("color", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2]),
        paste0("Edge for ", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2]),
        value = ifelse(is.null(input[[paste0("color", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2])]]), "black", input[[paste0("color", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2])]])
      )
    })
  })

  output$curveLty <- renderUI({
    g <- rv$g
    req(ends(g, E(g)))
    lapply(1:length(ends(g, E(g))[, 1]), function(i) {
      selectInput(paste0("lty", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2]),
        paste0("Line type for ", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2]),
        choices = c("solid", "dashed"),
        selected = ifelse(is.null(input[[paste0("lty", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2])]]), "solid", input[[paste0("lty", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2])]])
      )
    })
  })

  output$curveThick <- renderUI({
    g <- rv$g
    req(ends(g, E(g)))
    lapply(1:length(ends(g, E(g))[, 1]), function(i) {
      selectInput(paste0("lineT", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2]),
        paste0("Line thickness for ", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2]),
        choices = c("ultra thin", "very thin", "thin", "semithick", "thick", "very thick", "ultra thick"),
        selected = ifelse(is.null(input[[paste0("lineT", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2])]]), "thin", input[[paste0("lineT", ends(g, E(g))[i, 1], "->", ends(g, E(g))[i, 2])]])
      )
    })
  })
  
  # ---- Render DAG ----
  output$tikzOut <- renderUI({
    tikzUpdateOutput()
    if (!file.exists(file.path(SESSION_TEMPDIR, "DAGimageDoc.pdf"))) return(NULL)
    
    serve_file_path <- file.path(sub("www/", "", SESSION_TEMPDIR, fixed = TRUE), "DAGimageDoc.pdf")
    
    tags$iframe(
      style = "height:600px; width:100%", 
      src = serve_file_path,
      scrolling = "auto",
      zoom = if (length(V(rv$g)$name) < 1) 300,
      seamless = "seamless"
    )
  })
  
  tikzUpdateOutput <- reactiveVal(TRUE) # Triggers PDF update when value changes

  # Re-render TeX preview
  observe({
    if (length(V(rv$g)$name) >= 1) {
      styleZ <- "\\tikzset{ module/.style={draw, rectangle},
      label/.style={ } }"
      startZ <- "\\begin{tikzpicture}[>=latex]"
      endZ <- "\\end{tikzpicture}"
      pathZ <- "\\path[->,font=\\scriptsize,>=angle 90]"

      nodeFrame <- rv$pts2
      nodeFrame <- nodeFrame[nodeFrame$x >= min(nodeFrame[!is.na(nodeFrame$name), ]$x) &
        nodeFrame$x <= max(nodeFrame[!is.na(nodeFrame$name), ]$x) &
        nodeFrame$y >= min(nodeFrame[!is.na(nodeFrame$name), ]$y) &
        nodeFrame$y <= max(nodeFrame[!is.na(nodeFrame$name), ]$y), ]
      nodeFrame$name <- ifelse(is.na(nodeFrame$name), "~", nodeFrame$name)
      nodeFrame$nameA <- ifelse(nodeFrame$name %in% input$adjustNode, paste0(" |[module]| ", nodeFrame$name), nodeFrame$name)
      nodeLines <- vector("character", 0)
      for (i in unique(nodeFrame$y)) {
        createLines <- paste0(paste(nodeFrame[nodeFrame$y == i, ]$nameA, collapse = "&"), "\\\\")
        nodeLines <- c(nodeLines, createLines)
      }
      nodeLines <- rev(nodeLines)
      nodeLines2 <- nodeLines

      nodeLines <- paste0("\\matrix(m)[matrix of nodes, row sep=2.6em, column sep=2.8em,text height=1.5ex, text depth=0.25ex, nodes={label}] {", paste(nodeLines, collapse = ""), "};")

      edgeLines <- vector("character", 0)

      if (length(E(rv$g)) >= 1) {
        edgeFrame <- as.data.frame(ends(rv$g, E(rv$g)))
        edgeFrame$name <- paste0(edgeFrame$V1, "->", edgeFrame$V2)
        edgeFrame$angle <- edgeFrame$color <- edgeFrame$thick <- edgeFrame$type <- edgeFrame$loose <- NA
        edgeFrame$parent <- edgeFrame$child <- NA

        nodeFrame$revY <- rev(nodeFrame$y)

        for (i in 1:length(edgeFrame$name)) {
          edgeFrame$angle[i] <- ifelse(!is.null(input[[paste0("angle", edgeFrame$name[i])]]), as.numeric(input[[paste0("angle", edgeFrame$name[i])]]), 0)
          edgeFrame$color[i] <- ifelse(is.null(input[[paste0("color", edgeFrame$name[i])]]), "black", input[[paste0("color", edgeFrame$name[i])]])
          edgeFrame$thick[i] <- ifelse(is.null(input[[paste0("lineT", edgeFrame$name[i])]]), "thin", input[[paste0("lineT", edgeFrame$name[i])]])
          edgeFrame$type[i] <- ifelse(is.null(input[[paste0("lty", edgeFrame$name[i])]]), "solid", input[[paste0("lty", edgeFrame$name[i])]])
          edgeFrame$parent[i] <- paste0(
            "(m-", (nodeFrame[nodeFrame$name == edgeFrame$V1[i], ]$revY - min(nodeFrame$revY) + 1), "-",
            (nodeFrame[nodeFrame$name == edgeFrame$V1[i], ]$x - min(nodeFrame$x) + 1), ")"
          )
          edgeFrame$child[i] <- paste0(
            "(m-", (nodeFrame[nodeFrame$name == edgeFrame$V2[i], ]$revY - min(nodeFrame$revY) + 1), "-",
            (nodeFrame[nodeFrame$name == edgeFrame$V2[i], ]$x - min(nodeFrame$x) + 1), ")"
          )
          createEdge <- paste0(
            edgeFrame$parent[i], " edge [>=", input$arrowShape, ", bend left = ", edgeFrame$angle[i],
            ", color = ", edgeFrame$color[i], ",", edgeFrame$type[i], ",", edgeFrame$thick[i],
            "] node[auto] {$~$} ", edgeFrame$child[i], " "
          )
          edgeLines <- c(edgeLines, createEdge)
        }
      }

      edgeLines <- paste0(pathZ, paste(edgeLines, collapse = ""), ";")

      allLines <- c(styleZ, startZ, nodeLines, edgeLines, endZ)

      tikzTemp <- paste(allLines, collapse = "")

      useLib <- "\\usetikzlibrary{matrix,arrows,decorations.pathmorphing}"

      pkgs <- paste(buildUsepackage(pkg = list("tikz"), uselibrary = useLib), collapse = "\n")

      texPreview(
        obj = tikzTemp,
        stem = "DAGimage",
        fileDir = SESSION_TEMPDIR,
        imgFormat = "png",
        returnType = "shiny",
        density = tex_opts$get("density"),
        keep_pdf = TRUE,
        usrPackages = pkgs,
        margin = tex_opts$get("margin"),
        cleanup = tex_opts$get("cleanup")
      )
    }
    tikzUpdateOutput(!isolate(tikzUpdateOutput()))
  })
  
  # ---- Download Files ----
  # Merge tikz TeX source into main TeX file
  merge_tex_files <- function(main_file, input_file, out_file) {
    x <- readLines(main_file)
    y <- readLines(input_file)
    which_line <- grep("input{", x, fixed = TRUE)
    which_line <- intersect(which_line, grep(basename(input_file), x))
    x[which_line] <- paste(y, collapse = "\n")
    writeLines(x, out_file)
  }

  output$downloadButton <- downloadHandler(
    filename = function() {
      paste0("DAG", Sys.Date(), ifelse(input$downloadType == 1, ".RData",
        ifelse(input$downloadType == 2, ".tex",
          ifelse(input$downloadType == 3, ".png",
            ifelse(input$downloadType == 5, ".RData", ".pdf")
          )
        )
      ))
    },
    content = function(file) {
      if (input$downloadType == 1) {
        daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
        daggityCode1 <- paste(daggityCode1, collapse = ";")
        daggityCode2 <- paste0("dag { ", daggityCode1, " }")

        g2 <- dagitty(daggityCode2)

        exposures(g2) <- input$exposureNode
        outcomes(g2) <- input$outcomeNode
        adjustedNodes(g2) <- input$adjustNode

        dagitty_code <- g2
        save(dagitty_code, file = file)
      } else if (input$downloadType == 2) {
        merge_tex_files(
          file.path(SESSION_TEMPDIR, "DAGimageDoc.tex"),
          file.path(SESSION_TEMPDIR, "DAGimage.tex"),
          file
        )
      } else if (input$downloadType == 3) {
        myfile <- file.path(SESSION_TEMPDIR, "DAGimage.png")
        file.copy(myfile, file)
      } else if (input$downloadType == 5) {
        daggityCode1 <- paste0(ends(rv$g, E(rv$g))[, 1], "->", ends(rv$g, E(rv$g))[, 2])
        daggityCode1 <- paste(daggityCode1, collapse = ";")
        daggityCode2 <- paste0("dag { ", daggityCode1, " }")

        g2 <- dagitty(daggityCode2)

        exposures(g2) <- input$exposureNode
        outcomes(g2) <- input$outcomeNode
        adjustedNodes(g2) <- input$adjustNode

        tidy_dag <- tidy_dagitty(g2)
        save(tidy_dag, file = file)
      } else {
        myfile <- file.path(SESSION_TEMPDIR, "DAGimageDoc.pdf")
        file.copy(myfile, file)
      }
    }, contentType = NA
  )

  # ---- TeX Editor ----
  output$texEdit <- renderUI({
    if (length(V(rv$g)$name) >= 1) {
      styleZ <- "\\\\tikzset{ module/.style={draw, rectangle},
      label/.style={ } }"
      startZ <- "\\\\begin{tikzpicture}[>=latex]"
      endZ <- "\\\\end{tikzpicture}"
      pathZ <- "\\\\path[->,font=\\\\scriptsize,>=angle 90]"

      nodeFrame <- points2
      nodeFrame <- nodeFrame[nodeFrame$x >= min(nodeFrame[!is.na(nodeFrame$name), ]$x) &
        nodeFrame$x <= max(nodeFrame[!is.na(nodeFrame$name), ]$x) &
        nodeFrame$y >= min(nodeFrame[!is.na(nodeFrame$name), ]$y) &
        nodeFrame$y <= max(nodeFrame[!is.na(nodeFrame$name), ]$y), ]
      nodeFrame$name <- ifelse(is.na(nodeFrame$name), "~", nodeFrame$name)
      nodeFrame$nameA <- ifelse(nodeFrame$name %in% input$adjustNode, paste0(" |[module]| ", nodeFrame$name), nodeFrame$name)
      nodeLines <- vector("character", 0)
      for (i in unique(nodeFrame$y)) {
        createLines <- paste0(paste(nodeFrame[nodeFrame$y == i, ]$nameA, collapse = "&"), "\\\\\\\\")
        nodeLines <- c(nodeLines, createLines)
      }
      nodeLines <- rev(nodeLines)
      nodeLines2 <- nodeLines

      nodeLines <- paste0("\\\\matrix(m)[matrix of nodes, row sep=2.6em, column sep=2.8em,text height=1.5ex, text depth=0.25ex, nodes={label}] {", paste(nodeLines, collapse = ""), "};")

      edgeLines <- vector("character", 0)

      if (length(E(rv$g)) >= 1) {
        edgeFrame <- as.data.frame(ends(rv$g, E(rv$g)))
        edgeFrame$name <- paste0(edgeFrame$V1, "->", edgeFrame$V2)
        edgeFrame$angle <- edgeFrame$color <- edgeFrame$thick <- edgeFrame$type <- edgeFrame$loose <- NA
        edgeFrame$parent <- edgeFrame$child <- NA

        nodeFrame$revY <- rev(nodeFrame$y)

        for (i in 1:length(edgeFrame$name)) {
          edgeFrame$angle[i] <- ifelse(!is.null(input[[paste0("angle", edgeFrame$name[i])]]), as.numeric(input[[paste0("angle", edgeFrame$name[i])]]), 0)
          edgeFrame$color[i] <- ifelse(is.null(input[[paste0("color", edgeFrame$name[i])]]), "black", input[[paste0("color", edgeFrame$name[i])]])
          edgeFrame$thick[i] <- ifelse(is.null(input[[paste0("lineT", edgeFrame$name[i])]]), "thin", input[[paste0("lineT", edgeFrame$name[i])]])
          edgeFrame$type[i] <- ifelse(is.null(input[[paste0("lty", edgeFrame$name[i])]]), "solid", input[[paste0("lty", edgeFrame$name[i])]])
          edgeFrame$parent[i] <- paste0(
            "(m-", (nodeFrame[nodeFrame$name == edgeFrame$V1[i], ]$revY - min(nodeFrame$revY) + 1), "-",
            (nodeFrame[nodeFrame$name == edgeFrame$V1[i], ]$x - min(nodeFrame$x) + 1), ")"
          )
          edgeFrame$child[i] <- paste0(
            "(m-", (nodeFrame[nodeFrame$name == edgeFrame$V2[i], ]$revY - min(nodeFrame$revY) + 1), "-",
            (nodeFrame[nodeFrame$name == edgeFrame$V2[i], ]$x - min(nodeFrame$x) + 1), ")"
          )
          createEdge <- paste0(
            edgeFrame$parent[i], " edge [>=", input$arrowShape, ", bend left = ", edgeFrame$angle[i],
            ", color = ", edgeFrame$color[i], ",", edgeFrame$type[i], ",", edgeFrame$thick[i],
            "] node[auto] {$~$} ", edgeFrame$child[i], " "
          )
          edgeLines <- c(edgeLines, createEdge)
        }
      }

      edgeLines <- paste0(pathZ, paste(edgeLines, collapse = ""), ";")

      allLines <- c(styleZ, startZ, nodeLines, edgeLines, endZ)

      tikzTemp <- paste(allLines, collapse = "")
    } else {
      startZ <- "\\\\begin{tikzpicture}[>=latex]"
      endZ <- "\\\\end{tikzpicture}"

      allLines <- c(startZ, endZ)

      tikzTemp <- paste(allLines, collapse = "")
    }
    aceEditor("texChange", mode = "latex", value = paste(allLines, collapse = "\n"), theme = "cobalt")
  })

  output$tikzOutNew <- renderUI({
    tikzTemp <- input$texChange

    useLib <- "\\usetikzlibrary{matrix,arrows,decorations.pathmorphing}"

    pkgs <- paste(buildUsepackage(pkg = list("tikz"), uselibrary = useLib), collapse = "\n")

    texPreview(
      obj = tikzTemp,
      stem = "DAGimageEdit",
      fileDir = SESSION_TEMPDIR,
      imgFormat = "png",
      returnType = "shiny",
      density = tex_opts$get("density"),
      keep_pdf = TRUE,
      usrPackages = pkgs,
      margin = tex_opts$get("margin"),
      cleanup = tex_opts$get("cleanup")
    )
    
    session_id <- sub("www/", "", SESSION_TEMPDIR, fixed = TRUE)

    return(tags$iframe(
      style = "height:560px; width:100%", src = file.path(session_id, "DAGimageEditDoc.pdf"),
      scrolling = "no", seamless = "seamless"
    ))
  })

  output$downloadButton2 <- downloadHandler(
    filename = function() {
      paste0("DAG", Sys.Date(), ifelse(input$downloadType2 == 1, ".tex",
        ifelse(input$downloadType2 == 2, ".png", ".pdf")
      ))
    },
    content = function(file) {
      if (input$downloadType2 == 1) {
        merge_tex_files(
          file.path(SESSION_TEMPDIR, "DAGimageEditDoc.tex"),
          file.path(SESSION_TEMPDIR, "DAGimageEdit.tex"),
          file
        )
      } else if (input$downloadType2 == 2) {
        myfile <- file.path(SESSION_TEMPDIR, "DAGimageEdit.png")
        file.copy(myfile, file)
      } else {
        myfile <- file.path(SESSION_TEMPDIR, "DAGimageEditDoc.pdf")
        file.copy(myfile, file)
      }
    }, contentType = NA
  )
}

# Run the application
shinyApp(ui = ui, server = server)
