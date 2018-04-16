list.of.packages <- c("shiny","shinydashboard","DiagrammeR","dagitty","stringr","igraph","xtable",
"texPreview","magick","pdftools","png")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)!=0) install.packages(new.packages)

library(shiny)
library(shinydashboard)
library(DiagrammeR)
#library(magrittr)
library(dagitty)
library(stringr)
library(igraph)
library(xtable)
library(texPreview)
library(magick)
library(pdftools)
library(png)


ui <- dashboardPage(dashboardHeader(title = "ShinyDAG"),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      fluidRow(
                        box(title="DAG",
                            column(12, align="center",imageOutput("tikzOut")),
                            textOutput("adjustText"),
                            verbatimTextOutput("adjustSets"),
                            fluidRow(
                              column(6,"Open paths",verbatimTextOutput("openPaths")),
                              column(6,"Closed paths",verbatimTextOutput("closedPaths"))
                            ),
                            selectInput("downloadType","Type of download",
                                        choices=list("igraph R object" = 1,"Latex Tikz" = 2, "PNG" = 3,"PDF" = 4)),
                            downloadButton("downloadButton")
                        ),
                        tabBox(title="Options",
                               tabPanel("Import",
                                        fileInput("file1","Choose a file",
                                                  accept=c('text/csv', 
                                                           'text/comma-separated-values,text/plain', 
                                                           '.csv')),
                                        tags$hr(),
                                        h5(helpText("Select file parameters:")),
                                        checkboxInput(inputId = 'header1', label= 'Header', value= TRUE),
                                        br(),
                                        radioButtons(inputId = 'sep1', label = 'Seperator', 
                                                     choices = c(Comma=',', Semicolon=';', Tab='\t', Space= ' '), 
                                                     selected= ','),
                                        actionButton("upload1", "Upload File"),
                                        tags$hr(),
                                        h5(helpText("Please restrict new names to a single word")),
                                        fluidRow(column(6,uiOutput("oldName")),column(6,textInput("newName","New name"))),
                                        actionButton("renameButton","Rename!"),
                                        tags$hr(),
                                        h5(helpText("To add unmeasured variables:")),
                                        textInput("newVar","Unmeasured variable name"),
                                        actionButton("addVar","Add!")
                                        ),
                               tabPanel("Build",
                                        # textInput("nodeLabel","To add a node: type a label and click the grid"),
                                        uiOutput("nodeNames"),
                                        checkboxInput("clickType","Click to remove a node",value=FALSE),
                                        plotOutput("clickPad",
                                                   click = "click1"),
                                        fluidRow(
                                          column(6,uiOutput("fromEdge")),
                                          column(6,uiOutput("toEdge"))
                                        ),
                                        actionButton("edgeButton1","Add edge!"),
                                        actionButton("edgeButton2","Remove edge!"),
                                        uiOutput("arcList"),
                                        uiOutput("adjustNodeCreate"),
                                        uiOutput("exposureNodeCreate"),
                                        uiOutput("outcomeNodeCreate")),
                               tabPanel("Edit",
                                        uiOutput("curveAngle"),
                                        helpText("An edge must be curved in first tab in order to control the degree of the curve."),
                                        helpText("A negative degreee will change the orientation of the curve."),
                                        fluidRow(
                                          column(4,uiOutput("curveColor")),
                                          column(4,uiOutput("curveLty")),
                                          column(4,uiOutput("curveThick"))
                                        )
                               ),
                               tabPanel("Model",
                                        selectInput("modelType","Distribution",
                                                    choices = c("binomial","gaussian","Gamma",
                                                                "inverse.gaussian","poisson",
                                                                "quasi","quasibinomial","quasipoisson")),
                                        uiOutput("chooseModel"),
                                        verbatimTextOutput("showModel"),
                                        tableOutput("modelResults")),
                               tabPanel("About ShinyDAG",
                                        h6("Development Team: Jordan Creed and Travis Gerke"),
                                        h6("For more information on our lab and other projects please check out our website at http://travisgerke.com"),
                                        h6("All code is available from https://github.com/tgerke/shinyDAG"),
                                        h6("Any errors or comments can be directed to travis.gerke@moffitt.org or jordan.h.creed@moffitt.org"))
                        )
                      )
                    )
)

###################################################################################################
server <- function(input, output,session) {
  values <- reactiveValues(usr_dat = NULL)
  
  # importing data 
  observeEvent(input$upload1,{
    samplefile<-input$file1
    if(is.null(samplefile)){return()}
    values$usr_dat<-read.table(file=samplefile$datapath, sep= input$sep1, header= input$header1, stringsAsFactors= FALSE)
  })
  
  output$oldName <- renderUI({selectInput("oldName1","Select variable",choices=colnames(values$usr_dat))})
  
  observeEvent(input$renameButton,{
    names(values$usr_dat) <<- ifelse(colnames(values$usr_dat)==input$oldName1,input$newName,colnames(values$usr_dat))
  })
  
  observeEvent(input$addVar,{
    values$usr_dat$DAGNEWVAR <<- NA
    names(values$usr_dat) <<- ifelse(colnames(values$usr_dat)=="DAGNEWVAR",input$newVar,colnames(values$usr_dat))
  })
  
  output$nodeNames <- renderUI({selectInput("nodeLabel","Select variable",choices=colnames(values$usr_dat))})
  
  # initalize empty DAG drawing
  finalDag <- create_graph() 
  g <- make_empty_graph()
  
  # make reactive 
  makeReactiveBinding('finalDag')
  makeReactiveBinding('g')
  
  # click Pad points
  points <- list(x=vector("numeric", 0), y=vector("numeric", 0), name=vector("character",0)) 
  makeReactiveBinding('points')
  
  # edge data
  edges <- list(from=vector("character", 0), to=vector("character", 0)) 
  makeReactiveBinding('edges')
  
  # adding/removing points on clickPad
  observeEvent(input$click1,{
    if(input$clickType==FALSE & input$nodeLabel!=""){
      points$x <<- c(points$x,round(input$click1$x))
      points$y <<- c(points$y,round(input$click1$y))
      points$name <<- c(points$name,input$nodeLabel)
    } else if(input$clickType==TRUE){
      rmNode <- intersect(grep(round(input$click1$x),points$x),grep(round(input$click1$y),points$y))
      points$x[[rmNode]] <<- NA
      points$y[[rmNode]] <<- NA
      points$name[[rmNode]] <<- NA
    } else{
      points$x <<- points$x
      points$y <<- points$y
      points$name <<- points$name
    }
    updateTextInput(session, "nodeLabel", value="")
  })
  
  
  
  # clickPad display
  output$clickPad <- renderPlot({
    if(length(points$x>=1)){
      plot(points$x,points$y, xlim=c(1, 7), ylim=c(1, 7),bty='n',xaxt='n',yaxt='n',ylab="",xlab="",xaxs="i",col="white")
      text(points$x,points$y, labels=points$name, cex= 2)
      grid()
    } else{
      plot(points$x,points$y, xlim=c(1, 7), ylim=c(1, 7),bty='n',xaxt='n',yaxt='n',ylab="",xlab="",xaxs="i")
      grid()
    }
  })
  
  output$adjustNodeCreate <- renderUI({
    checkboxGroupInput("adjustNode","Select nodes to adjust",choices = points$name[!is.na(points$name)],
                       inline=TRUE)
  })
  
  output$exposureNodeCreate <- renderUI({
    checkboxGroupInput("exposureNode","Exposure",choices = points$name[!is.na(points$name)],
                       inline=TRUE)
  })
  
  output$outcomeNodeCreate <- renderUI({
    checkboxGroupInput("outcomeNode","Outcome",choices = points$name[!is.na(points$name)],
                       inline=TRUE)
  })
  
  output$adjustText <- renderText({
    if(is.null(input$exposureNode) & is.null(input$outcomeNode)){
      paste0("")
    } else{paste0("Minimal sufficient adjustment sets to estimate the effect of ",
                  input$exposureNode," on ",input$outcomeNode)}
  })
  
  # add/remove nodes on DAG
  observeEvent(input$click1,{
    if(input$clickType==FALSE & input$nodeLabel!=""){
      g <<- g %>% add_vertices(1,
                               name= input$nodeLabel,
                               x = round(input$click1$x), 
                               y = round(input$click1$y),
                               color = "white",
                               shape = "none") # shape = "rectangle" if adjusting 
    } else if(input$clickType==TRUE){
      rmNode <- intersect(grep(round(input$click1$x),V(g)$x),grep(round(input$click1$y),V(g)$y))
      rmNode <- V(g)$name[[rmNode]]
      g <<- g %>% delete_vertices(rmNode)
    } else {
      g <<- g
    }
  })
  
  output$fromEdge <- renderUI({
    selectInput("fromEdge2", "Parent node",choices = c("---",points$name[!is.na(points$name)]))
    
  })
  
  output$toEdge <- renderUI({
    selectInput("toEdge2", "Child node",choices = c("---",points$name[!is.na(points$name)]))
    
  })
  
  # add/remove edges to DAG
  observeEvent(input$edgeButton1,{
    if(input$fromEdge2 %in% V(g)$name & input$toEdge2 %in% V(g)$name){
      g <<- g %>%
        add_edges(c(input$fromEdge2,input$toEdge2))  %>%
        set_edge_attr("color", value = "black")
    } else {
      g <<- g 
    }
    
  })
  
  observeEvent(input$edgeButton2,{
    if(input$fromEdge2 %in% V(g)$name & input$toEdge2 %in% V(g)$name){
      g <<- g %>%
        delete_edges(paste0(input$fromEdge2,"|",input$toEdge2))
    } else {
      g <<- g 
    }
    
  })
  
  output$curvedEdges <- renderUI({
    checkboxGroupInput("curvEdges","Curve these edges",choices = paste0(ends(g,E(g))[,1],",",ends(g,E(g))[,2]),
                       inline=TRUE)
  })
  
  
  output$arcList <- renderUI({
    checkboxGroupInput("arcListC","Select edges to curve",choices = paste0(ends(g,E(g))[,1],"->",ends(g,E(g))[,2]),
                       inline=TRUE)
  })
  
  # needed for modeling 
  output$adjustSets <- renderPrint({
    if(!is.null(input$exposureNode) & !is.null(input$outcomeNode)){
      daggityCode1 <- paste0(ends(g,E(g))[,1],"->",ends(g,E(g))[,2])
      daggityCode1 <- paste(daggityCode1,collapse=";")
      daggityCode2 <- paste0("dag { ",daggityCode1, " }") 
      
      g2 <- dagitty(daggityCode2)
      
      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode
      adjustedNodes(g2) <- input$adjustNode
      
      adjustResults <- adjustmentSets(g2)
      return(adjustResults)} else{return(print("Please indicate exposure and outcome"))}
  })
  
  output$condInd <- renderPrint({
    if(!is.null(input$exposureNode) & !is.null(input$outcomeNode)){
      daggityCode1 <- paste0(ends(g,E(g))[,1],"->",ends(g,E(g))[,2])
      daggityCode1 <- paste(daggityCode1,collapse=";")
      daggityCode2 <- paste0("dag { ",daggityCode1, " }") 
      
      g2 <- dagitty(daggityCode2)
      
      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode
      adjustedNodes(g2) <- input$adjustNode
      
      condResults <- impliedConditionalIndependencies(g2)
      return(condResults)} else{return(print(""))}
  })
  
  output$openPaths <- renderPrint({
    if(!is.null(input$exposureNode) & !is.null(input$outcomeNode)){
      daggityCode1 <- paste0(ends(g,E(g))[,1],"->",ends(g,E(g))[,2])
      daggityCode1 <- paste(daggityCode1,collapse=";")
      daggityCode2 <- paste0("dag { ",daggityCode1, " }") 
      
      g2 <- dagitty(daggityCode2)
      
      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode
      adjustedNodes(g2) <- input$adjustNode
      
      allComb <- as.data.frame(combn(names(g2), 2))
      
      pathData <- list(path=vector("character",0),open=vector("character",0))
      for(i in 1:ncol(allComb)){
        pathResults <- paths(g2,from=allComb[1,i],to=allComb[2,i],Z=input$adjustNode)
        pathData$path <- c(pathData$path, pathResults$paths)
        pathData$open <- c(pathData$open, pathResults$open)
      }
      
      openPaths <- grep("TRUE",pathData$open)
      
      return(cat(pathData$path[openPaths][str_count(pathData$path[openPaths], "-") > 1],sep="\n"))} else{return(print(""))}
  })
  
  output$closedPaths <- renderPrint({
    if(!is.null(input$exposureNode) & !is.null(input$outcomeNode)){
      daggityCode1 <- paste0(ends(g,E(g))[,1],"->",ends(g,E(g))[,2])
      daggityCode1 <- paste(daggityCode1,collapse=";")
      daggityCode2 <- paste0("dag { ",daggityCode1, " }") 
      
      g2 <- dagitty(daggityCode2)
      
      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode
      adjustedNodes(g2) <- input$adjustNode
      
      allComb <- as.data.frame(combn(names(g2), 2))
      
      pathData <- list(path=vector("character",0),open=vector("character",0))
      for(i in 1:ncol(allComb)){
        pathResults <- paths(g2,from=allComb[1,i],to=allComb[2,i],Z=input$adjustNode)
        pathData$path <- c(pathData$path, pathResults$paths)
        pathData$open <- c(pathData$open, pathResults$open)
      }
      
      closedPaths <- grep("FALSE",pathData$open)
      
      return(cat(pathData$path[closedPaths][str_count(pathData$path[closedPaths], "-") > 1],sep="\n"))} else{return(print(""))}
  })
  
  output$curveAngle<-renderUI({
    lapply(1:length(input$arcListC),function(i){
      sliderInput(input$arcListC[[i]],paste0("Angle for ",input$arcListC[[i]]),
                  min=-180,max=180,value=45)
    })
  })
  
  output$curveColor<-renderUI({
    lapply(1:length(ends(g,E(g))[,1]),function(i){
      textInput(paste0("color",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                paste0("Edge for ",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                value="black")
    })
  })
  
  output$curveLty<-renderUI({
    lapply(1:length(ends(g,E(g))[,1]),function(i){
      selectInput(paste0("lty",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                  paste0("Line type for ",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                  choices=c("solid","dashed"),selected = "solid")
    })
  })
  
  output$curveThick<-renderUI({
    lapply(1:length(ends(g,E(g))[,1]),function(i){
      selectInput(paste0("lineT",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                  paste0("Line thickness for ",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                  choices=c("ultra thin","very thin","thin","semithick","thick","very thick","ultra thick"),
                  selected = "thin")
    })
  })
  
  
  output$tikzOut<-renderImage({
    imgWidth  <- session$clientData$output_tikzOut_width
    imgHeight <- session$clientData$output_tikzOut_height
    if(length(V(g)$name)>=1){
      startZ <- "\\begin{tikzpicture}[>=latex]"
      endZ <- "\\end{tikzpicture}"
      
      nodeFrame <- as.data.frame(cbind(name=V(g)$name,x=V(g)$x, y=V(g)$y, adjust=V(g)$shape))
      
      nodeLines <- vector("character",0)
      for(i in 1:length(nodeFrame$name)){
        createNode <- paste0("\\node (",nodeFrame$name[i],") at (",nodeFrame$x[i],",",
                             nodeFrame$y[i],")",ifelse(nodeFrame$name[i] %in% input$adjustNode," [draw, rectangle] "," "),
                             "{",nodeFrame$name[i],"};")
        nodeLines <- c(nodeLines,createNode)
      }
      
      fromNode <- trimws(gsub(",.*","",input$curvEdges))
      toNode <- trimws(gsub(".*,","",input$curvEdges))
      
      edgeLines <- vector("character",0)
      
      if(length(E(g))>=1){
        for(i in 1:length(ends(g,E(g))[,1])){
          if(paste0(ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]) %in% input$arcListC){
            edgeAngle <- ifelse(is.null(input[[paste0(ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),45,as.numeric(input[[paste0(ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]))
            edgeColor <- ifelse(is.null(input[[paste0("color",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),"black",input[[paste0("color",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]])
            edgeThickness <- ifelse(is.null(input[[paste0("lineT",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),"thin",input[[paste0("lineT",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]])
            edgeLineType <- ifelse(is.null(input[[paste0("lty",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),"solid",input[[paste0("lty",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]])
            createEdge <- paste0("\\path[->,color=",edgeColor,", ",edgeThickness,", ",edgeLineType,"]"," [auto] ","(",ends(g,E(g))[i,1],") edge",
                                 " [bend left= ",edgeAngle,"] ",
                                 "(",ends(g,E(g))[i,2],");")
            edgeLines <- c(edgeLines,createEdge)
          } else {
            edgeColor <- ifelse(is.null(input[[paste0("color",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),"black",input[[paste0("color",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]])
            edgeThickness <- ifelse(is.null(input[[paste0("lineT",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),"thin",input[[paste0("lineT",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]])
            edgeLineType <- ifelse(is.null(input[[paste0("lty",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),"solid",input[[paste0("lty",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]])
            createEdge <- paste0("\\path[->,color=",edgeColor,", ",edgeThickness,", ",edgeLineType,"]","(",ends(g,E(g))[i,1],") edge",
                                 "(",ends(g,E(g))[i,2],");")
            edgeLines <- c(edgeLines,createEdge)
          }
        }
      }
      
      allLines <- c(startZ,nodeLines,edgeLines,endZ)
      cat(allLines,sep="\n")
      
      tikzTemp <- paste(allLines,collapse="")
      
      useLib="\\usetikzlibrary{positioning,shapes.geometric}"
      
      pkgs=paste(buildUsepackage(pkg = list('tikz'),uselibrary = useLib),collapse='\n')
      
      texPreview(obj = tikzTemp,
                 stem = 'DAGimage',
                 fileDir = getwd(),
                 imgFormat = 'png',
                 returnType = 'shiny',
                 density=tex_opts$get("density"),
                 keep_pdf = TRUE,
                 usrPackages = pkgs,
                 margin = tex_opts$get("margin"),
                 cleanup = tex_opts$get("cleanup")
      )

      filename <- normalizePath(file.path(paste0(getwd(),'/DAGimage.png')))
      img <- readPNG(paste0(getwd(),"/DAGimage.png"))
      widthRatio <- imgWidth/dim(img)[2]
      heightRatio <- imgHeight/dim(img)[1]
      widthValue <- imgWidth
      heightValue <- imgHeight
      if(dim(img)[2]>=dim(img)[1]){
        widthValue <- imgWidth
        heightValue <- (dim(img)[1]/dim(img)[2])*imgWidth
        heightValue <- min(widthRatio * dim(img)[1],imgHeight)
      }
      if(dim(img)[1]>=dim(img)[2]){
        heightValue <- imgHeight
        widthValue <- (dim(img)[2]/dim(img)[1])*imgHeight
        widthValue <- min(heightRatio * dim(img)[2],imgWidth)
      }

      list(src = filename,width=widthValue,height=heightValue)
      # list(src = filename,width=imgWidth,height=imgHeight)
      
    } else{
      startZ <- "\\begin{tikzpicture}[>=latex]"
      endZ <- "\\end{tikzpicture}"
      
      allLines <- c(startZ,endZ)
      cat(allLines,sep="\n")
      
      tikzTemp <- paste(allLines,collapse="")
      
      useLib="\\usetikzlibrary{positioning,shapes.geometric}"
      
      pkgs=paste(buildUsepackage(pkg = list('tikz'),uselibrary = useLib),collapse='\n')
      
      texPreview(obj = tikzTemp,
                 stem = 'DAGimage',
                 fileDir = getwd(),
                 imgFormat = 'png',
                 returnType = 'shiny',
                 density=1200,
                 usrPackages = pkgs)
      filename <- normalizePath(file.path(paste0(getwd(),'/DAGimage.png')))
      
      list(src = filename)
    }
    
  },deleteFile = FALSE)
  
  output$downloadButton <- downloadHandler(
    filename = function() {
      paste0("DAG",Sys.Date(),ifelse(input$downloadType==1,".RData",
                                     ifelse(input$downloadType==2,".tex",
                                            ifelse(input$downloadType==3,".png",".pdf"))))
    },
    content = function(file) {
      if(input$downloadType==1){
        save(g,file=file)
      } else if (input$downloadType==2){
        myfile <- paste0(getwd(),"/DAGimageDoc.tex")
        file.copy(myfile, file)
      } else if (input$downloadType==3){
        myfile <- paste0(getwd(),"/DAGimageDoc.png")
        file.copy(myfile, file)
      } else {
        myfile <- paste0(getwd(),"/DAGimageDoc.pdf")
        file.copy(myfile, file)
      }
    }, contentType = NA
  )
  
  output$chooseModel <- renderUI({
    daggityCode1 <- paste0(ends(g,E(g))[,1],"->",ends(g,E(g))[,2])
    daggityCode1 <- paste(daggityCode1,collapse=";")
    daggityCode2 <- paste0("dag { ",daggityCode1, " }")
    
    g2 <- dagitty(daggityCode2)
    
    exposures(g2) <- input$exposureNode
    outcomes(g2) <- input$outcomeNode
    adjustedNodes(g2) <- input$adjustNode
    
    adjustResults <- as.character(adjustmentSets(g2))
    adjustC <- list()
    if(adjustResults!="list()"){
      for(i in 1:length(adjustResults)){
        temp <- gsub('\\\"',"",adjustResults[[i]])
        temp <- gsub("c\\(","",temp)
        temp <- gsub("\\)","",temp)
        temp <- strsplit(temp, ",", fixed=TRUE)[[1]]
        adjustC[[i]] <- temp
      }
    } 
    modelsX <- c(paste0(input$outcomeNode,"~",input$exposureNode),
                paste0(input$outcomeNode,"~",input$exposureNode,"+",paste(input$adjustNode,collapse = "+")))
    for(i in 1:length(adjustC)){
      modelsX <- c(modelsX,paste0(input$outcomeNode,"~",input$exposureNode,"+",paste(adjustC[[i]],collapse = "+")))
    }
    selectInput("runModel","Select model",
                choices=modelsX)
    })
  
  output$modelResults <- renderTable({
      modelX <- glm(as.formula(input$runModel),
                    data=values$usr_dat, family=input$modelType)
      modelX.x <- summary(modelX)
      modelX.x <- modelX.x$coefficients
      modelX.x <- as.data.frame(cbind(Coefficients=row.names(modelX.x),modelX.x))
      modelX.x[,2]<-round(as.numeric(as.character(modelX.x[,2])),2)
      modelX.x[,3]<-round(as.numeric(as.character(modelX.x[,3])),2)
      modelX.x[,4]<-round(as.numeric(as.character(modelX.x[,4])),2)
      modelX.x[,5]<-round(as.numeric(as.character(modelX.x[,5])),2)
      return(xtable(modelX.x, include.rownames=TRUE))

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

