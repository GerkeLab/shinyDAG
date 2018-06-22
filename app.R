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

tex_opts$set(list(density=1200,
                  margin = list(left = 0, top = 0, right = 0, bottom = 0),
                  cleanup = c("aux","log")))

dir.create(file.path(getwd(), "www"))

download.file(url="https://www.dropbox.com/s/ndmblxnkwvfwpot/GerkeLab-1200dpi-square.png?dl=1",
              destfile = file.path(paste0(getwd(),'/www/GerkeLab.png')) )

ui <- dashboardPage(title = "shinyDAG",
                    dashboardHeader(disable=TRUE),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      fluidRow(
                        box(title="shinyDAG",
                            column(12, align="center",uiOutput("tikzOut")),
                            selectInput("downloadType","Type of download",
                                        choices=list("PDF" = 4, "PNG" = 3,"Latex Tikz" = 2, "dagitty R object" = 1,"ggdag R object" = 5)),
                            downloadButton("downloadButton"),
                            br(),br(),
                            prettySwitch(
                              inputId = "showFlow",
                              label = "Examine DAG elements", 
                              status = "primary",
                              fill = TRUE
                            ),
                            conditionalPanel(condition = "input.showFlow == 1",
                                             # textOutput("adjustText"),
                                             # verbatimTextOutput("adjustSets"),
                                             fluidRow(
                                               column(6,"Open paths",verbatimTextOutput("openPaths")),
                                               column(6,"Closed paths",verbatimTextOutput("closedPaths"))
                                             ))
                            ),
                        tabBox(title=div(img(src="GerkeLab.png",width=40,height=40)),
                               tabPanel("Build",
                                        tags$style(type="text/css",
                                                   ".shiny-output-error { visibility: hidden; }",
                                                   ".shiny-output-error:before { visibility: hidden; }"
                                        ),
                                        textInput("nodeLabel","To add a node: type a label and click the grid"),
                                        checkboxInput("clickType","Click to remove a node",value=FALSE),
                                        plotOutput("clickPad",
                                                   click = "click1"),
                                        fluidRow(
                                          column(6,uiOutput("fromEdge")),
                                          column(6,uiOutput("toEdge"))
                                        ),
                                        actionButton("edgeButton1","Add edge!"),
                                        actionButton("edgeButton2","Remove edge!"),
                                        uiOutput("adjustNodeCreate"),
                                        uiOutput("exposureNodeCreate"),
                                        uiOutput("outcomeNodeCreate")),
                               tabPanel("Edit aesthetics",
                                        selectInput("arrowShape","Select arrow head", choices = c("stealth","stealth'","diamond",
                                                                                                  "triangle 90","hooks","triangle 45",
                                                                                                  "triangle 60","hooks reversed","*"), selected = "stealth"),
                                        uiOutput("curveAngle"),
                                        helpText("A negative degree will change the orientation of the curve."),
                                        fluidRow(
                                          column(4,uiOutput("curveColor")),
                                          column(4,uiOutput("curveLty")),
                                          column(4,uiOutput("curveThick"))
                                        )
                                        ),
                               tabPanel("Edit LaTex",
                                        helpText("WARNING: Editing code here will only change the appearance of the DAG and not the information on paths provided."),
                                        uiOutput("texEdit"),
                                        actionButton("redoTex","Initiate Editing!"),
                                        conditionalPanel(
                                          condition = "input.redoTex == 1",
                                          uiOutput("tikzOutNew"),
                                          selectInput("downloadType2","Type of download",
                                                      choices=list("PDF" = 3,"PNG" = 2,"Latex Tikz" = 1)),
                                          downloadButton("downloadButton2")
                                        )),
                               tabPanel("About shinyDAG",
                                        h6("Development Team: Jordan Creed and Travis Gerke"),
                                        h6("For more information on our lab and other projects please check out our website at http://travisgerke.com"),
                                        h6("All code is available from https://github.com/GerkeLab/ShinyDAG"),
                                        h6("Any errors or comments can be directed to travis.gerke@moffitt.org or jordan.h.creed@moffitt.org"))
                            )
                      )
                    )
                    )

###################################################################################################
server <- function(input, output,session) {

  g <- make_empty_graph()
  
  makeReactiveBinding('g')
  
  # click Pad points
  points <- list(x=vector("numeric", 0), y=vector("numeric", 0), name=vector("character",0)) 
  makeReactiveBinding('points')
  
  points2 <- as.data.frame(cbind(x=rep(1:7,each=7), y=rep(1:7,7), name=rep(NA,49))) 
  makeReactiveBinding('points2')
  
  # edge data
  edges <- list(from=vector("character", 0), to=vector("character", 0)) 
  makeReactiveBinding('edges')
  
  errorMessage1 <- NULL
    
  # adding/removing points on clickPad
  observeEvent(input$click1,{
    if(input$nodeLabel %in% points$name){
      errorMessage1<<- showNotification("Unpredictable Behavior: duplicate names",
      duration = 5, 
      closeButton = TRUE, type="warning"
    )}
    
    if(input$clickType==FALSE & input$nodeLabel!=""){
    points$x <<- c(points$x,round(input$click1$x))
    points$y <<- c(points$y,round(input$click1$y))
    points$name <<- c(points$name,input$nodeLabel)
    points2$name <<- ifelse(round(input$click1$x)==points2$x & round(input$click1$y)==points2$y,
                            input$nodeLabel,points2$name)
    } else if(input$clickType==TRUE){
      rmNode <- intersect(grep(round(input$click1$x),points$x),grep(round(input$click1$y),points$y))
      if(length(rmNode)>0){
        points$x[[rmNode]] <<- NA
        points$y[[rmNode]] <<- NA
        points$name[[rmNode]] <<- NA
        points2$name <<- ifelse(round(input$click1$x)==points2$x & round(input$click1$y)==points2$y,
                                NA,points2$name)
      }
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
      paste0("Minimal sufficient adjustment sets")
    } else{paste0("Minimal sufficient adjustment set(s) to estimate the effect of ",
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
                            shape = "none")
    } else if(input$clickType==TRUE){
      rmNode <- intersect(grep(round(input$click1$x),V(g)$x),grep(round(input$click1$y),V(g)$y))
      if(length(rmNode)>0){
        rmNode <- V(g)$name[[rmNode]]
        g <<- g %>% delete_vertices(rmNode)
        } else {g <<- g}
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

  output$adjustSets <- renderPrint({
    if(!is.null(input$exposureNode) & !is.null(input$outcomeNode)){
    daggityCode1 <- paste0(ends(g,E(g))[,1],"->",ends(g,E(g))[,2])
    daggityCode1 <- paste(daggityCode1,collapse=";")
    daggityCode2 <- paste0("dag { ",daggityCode1, " }") 
    
    g2 <- dagitty(daggityCode2)
    
    exposures(g2) <- input$exposureNode
    outcomes(g2) <- input$outcomeNode
    
    adjustResults <- adjustmentSets(g2)
    return(adjustResults)} else{return(print("Please indicate exposure and outcome"))}
  })
  
  output$condInd <- renderPrint({
      daggityCode1 <- paste0(ends(g,E(g))[,1],"->",ends(g,E(g))[,2])
      daggityCode1 <- paste(daggityCode1,collapse=";")
      daggityCode2 <- paste0("dag { ",daggityCode1, " }") 
      
      g2 <- dagitty(daggityCode2)
      
      exposures(g2) <- input$exposureNode
      outcomes(g2) <- input$outcomeNode
      adjustedNodes(g2) <- input$adjustNode

      test <- impliedConditionalIndependencies(g2)
      
      return_list <- vector("character",0)
      for(i in 1:length(test)){
        return_list <- c(return_list,paste0(test[[i]]$X," is independent of ",test[[i]]$Y," given: ",paste0(test[[i]]$Z,collapse = " and ")))
      }
      return(cat(return_list, sep="\n"))#} else{return(print(""))}
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
      
      return(cat(pathData$path[openPaths][str_count(pathData$path[openPaths], "-") >= 1],sep="\n"))} else{return(print(""))}
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
      
      return(cat(pathData$path[closedPaths][str_count(pathData$path[closedPaths], "-") >= 1],sep="\n"))} else{return(print(""))}
  })

  output$curveAngle<-renderUI({
    if(length(ends(g,E(g))[,1])>=1){
    lapply(1:length(ends(g,E(g))[,1]),function(i){
      sliderInput(paste0("angle",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),paste0("Angle for ",paste0(ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])),
                  min=-180,max=180,value=ifelse(is.null(input[[paste0("angle",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),0,input[[paste0("angle",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]))
    })
    }
  })

  output$curveColor<-renderUI({
    lapply(1:length(ends(g,E(g))[,1]),function(i){
      textInput(paste0("color",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                paste0("Edge for ",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                value=ifelse(is.null(input[[paste0("color",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),"black",input[[paste0("color",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]))
    })
  })
  
  output$curveLty<-renderUI({
    lapply(1:length(ends(g,E(g))[,1]),function(i){
      selectInput(paste0("lty",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                  paste0("Line type for ",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                  choices=c("solid","dashed"),
                  selected = ifelse(is.null(input[[paste0("lty",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),"solid",input[[paste0("lty",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]))
    })
  })
  
  output$curveThick<-renderUI({
    lapply(1:length(ends(g,E(g))[,1]),function(i){
      selectInput(paste0("lineT",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                  paste0("Line thickness for ",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2]),
                  choices=c("ultra thin","very thin","thin","semithick","thick","very thick","ultra thick"),
                  selected = ifelse(is.null(input[[paste0("lineT",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]),"thin",input[[paste0("lineT",ends(g,E(g))[i,1],"->",ends(g,E(g))[i,2])]]))
    })
  })

    output$tikzOut<-renderUI({
    if(length(V(g)$name)>=1){
      styleZ <- "\\tikzset{ module/.style={draw, rectangle},
      label/.style={ } }"
      startZ <- "\\begin{tikzpicture}[>=latex]"
      endZ <- "\\end{tikzpicture}"
      pathZ <- "\\path[->,font=\\scriptsize,>=angle 90]"
      
      nodeFrame <- points2
      nodeFrame <- nodeFrame[nodeFrame$x>=min(nodeFrame[!is.na(nodeFrame$name),]$x) &
                             nodeFrame$x<=max(nodeFrame[!is.na(nodeFrame$name),]$x) & 
                             nodeFrame$y>=min(nodeFrame[!is.na(nodeFrame$name),]$y) & 
                             nodeFrame$y<=max(nodeFrame[!is.na(nodeFrame$name),]$y),]
      nodeFrame$name <- ifelse(is.na(nodeFrame$name),"~",nodeFrame$name)
      nodeFrame$nameA <- ifelse(nodeFrame$name %in% input$adjustNode, paste0(" |[module]| ",nodeFrame$name), nodeFrame$name)
      nodeLines <- vector("character",0)
      for (i in unique(nodeFrame$y)){
        createLines <- paste0(paste(nodeFrame[nodeFrame$y==i,]$nameA,collapse="&"),"\\\\")
        nodeLines <- c(nodeLines,createLines)
      }
      nodeLines <- rev(nodeLines)
      nodeLines2 <- nodeLines
    
      nodeLines <- paste0("\\matrix(m)[matrix of nodes, row sep=2.6em, column sep=2.8em,text height=1.5ex, text depth=0.25ex, nodes={label}] {",paste(nodeLines,collapse=""),"};")
      
      edgeLines <- vector("character",0)
      
      if(length(E(g))>=1){
      edgeFrame <- as.data.frame(ends(g,E(g)))
      edgeFrame$name <- paste0(edgeFrame$V1,"->",edgeFrame$V2)
      edgeFrame$angle <- edgeFrame$color <- edgeFrame$thick <- edgeFrame$type <- edgeFrame$loose <- NA
      edgeFrame$parent <- edgeFrame$child <- NA
      
      nodeFrame$revY <- rev(nodeFrame$y)
    
      for(i in 1:length(edgeFrame$name)){
          edgeFrame$angle[i] <- ifelse(!is.null(input[[paste0("angle",edgeFrame$name[i])]]),as.numeric(input[[paste0("angle",edgeFrame$name[i])]]),0)
          edgeFrame$color[i] <- ifelse(is.null(input[[paste0("color",edgeFrame$name[i])]]),"black",input[[paste0("color",edgeFrame$name[i])]])
          edgeFrame$thick[i] <- ifelse(is.null(input[[paste0("lineT",edgeFrame$name[i])]]),"thin",input[[paste0("lineT",edgeFrame$name[i])]])
          edgeFrame$type[i] <- ifelse(is.null(input[[paste0("lty",edgeFrame$name[i])]]),"solid",input[[paste0("lty",edgeFrame$name[i])]])
          edgeFrame$parent[i] <- paste0("(m-",(nodeFrame[nodeFrame$name==edgeFrame$V1[i],]$revY-min(nodeFrame$revY)+1),"-",
                                        (nodeFrame[nodeFrame$name==edgeFrame$V1[i],]$x-min(nodeFrame$x)+1),")")
          edgeFrame$child[i] <- paste0("(m-",(nodeFrame[nodeFrame$name==edgeFrame$V2[i],]$revY-min(nodeFrame$revY)+1),"-",
                                       (nodeFrame[nodeFrame$name==edgeFrame$V2[i],]$x-min(nodeFrame$x)+1),")")
          createEdge <- paste0(edgeFrame$parent[i]," edge [>=",input$arrowShape,", bend left = ",edgeFrame$angle[i],
                               ", color = ",edgeFrame$color[i],",",edgeFrame$type[i],",", edgeFrame$thick[i],
                               "] node[auto] {$~$} ",edgeFrame$child[i]," ")
          edgeLines <- c(edgeLines,createEdge)
      }
      }

      edgeLines <- paste0(pathZ,paste(edgeLines,collapse=""),";")
      
      allLines <- c(styleZ,startZ,nodeLines,edgeLines,endZ)
      
      tikzTemp <- paste(allLines,collapse="")
      
      useLib="\\usetikzlibrary{matrix,arrows,decorations.pathmorphing}"
      
      pkgs=paste(buildUsepackage(pkg = list('tikz'),uselibrary = useLib),collapse='\n')
      
      texPreview(obj = tikzTemp,
                 stem = 'DAGimage',
                 fileDir = paste0(getwd(),"/www"),
                 imgFormat = 'png',
                 returnType = 'shiny',
                 density=tex_opts$get("density"),
                 keep_pdf = TRUE,
                 usrPackages = pkgs,
                 margin = tex_opts$get("margin"),
                 cleanup = tex_opts$get("cleanup")
      )
      
      filename <- normalizePath(file.path(paste0(getwd(),'/www/DAGimageDoc.pdf')))
      
      return(tags$iframe(style="height:600px; width:100%",src = "DAGimageDoc.pdf", 
                         scrolling="auto",seamless="seamless"))
      
    } else{
      startZ <- "\\begin{tikzpicture}[>=latex]"
      endZ <- "\\end{tikzpicture}"
      
      allLines <- c(startZ,endZ)
      
      tikzTemp <- paste(allLines,collapse="")
      
      useLib="\\usetikzlibrary{matrix,arrows,decorations.pathmorphing}"
      
      pkgs=paste(buildUsepackage(pkg = list('tikz'),uselibrary = useLib),collapse='\n')
      
      texPreview(obj = tikzTemp,
                 stem = 'DAGimage',
                 fileDir = paste0(getwd(),"/www"),
                 imgFormat = 'png',
                 returnType = 'shiny',
                 density=300,
                 usrPackages = pkgs)
      
      filename <- normalizePath(file.path(paste0(getwd(),'/www/DAGimageDoc.pdf')))
      
      return(tags$iframe(style="height:600px; width:100%",src = "DAGimageDoc.pdf", zoom=300,
                         scrolling="auto",seamless="seamless"))
    }
    
  })
  
  output$downloadButton <- downloadHandler(
    filename = function() {
      paste0("DAG",Sys.Date(),ifelse(input$downloadType==1,".RData",
                                     ifelse(input$downloadType==2,".tex",
                                            ifelse(input$downloadType==3,".png",
                                                   ifelse(input$downloadType==5,".RData",".pdf")))))
    },
    content = function(file) {
      if(input$downloadType==1){
        daggityCode1 <- paste0(ends(g,E(g))[,1],"->",ends(g,E(g))[,2])
        daggityCode1 <- paste(daggityCode1,collapse=";")
        daggityCode2 <- paste0("dag { ",daggityCode1, " }") 
        
        g2 <- dagitty(daggityCode2)
        
        exposures(g2) <- input$exposureNode
        outcomes(g2) <- input$outcomeNode
        adjustedNodes(g2) <- input$adjustNode
        
        dagitty_code <- g2
        save(dagitty_code,file=file)
      } else if (input$downloadType==2){
        myfile <- paste0(getwd(),"/www/DAGimageDoc.tex")
        file.copy(myfile, file)
      } else if (input$downloadType==3){
        myfile <- paste0(getwd(),"/www/DAGimage.png")
        file.copy(myfile, file)
      } else if (input$downloadType==5) {
        daggityCode1 <- paste0(ends(g,E(g))[,1],"->",ends(g,E(g))[,2])
        daggityCode1 <- paste(daggityCode1,collapse=";")
        daggityCode2 <- paste0("dag { ",daggityCode1, " }") 
        
        g2 <- dagitty(daggityCode2)
        
        exposures(g2) <- input$exposureNode
        outcomes(g2) <- input$outcomeNode
        adjustedNodes(g2) <- input$adjustNode
        
        tidy_dag <- tidy_dagitty(g2)
        save(tidy_dag,file=file)
      }else {
        myfile <- paste0(getwd(),"/www/DAGimageDoc.pdf")
        file.copy(myfile, file)
    }
}, contentType = NA
  )
  
  output$texEdit <- renderUI({
    if(length(V(g)$name)>=1){
      styleZ <- "\\\\tikzset{ module/.style={draw, rectangle},
      label/.style={ } }"
      startZ <- "\\\\begin{tikzpicture}[>=latex]"
      endZ <- "\\\\end{tikzpicture}"
      pathZ <- "\\\\path[->,font=\\\\scriptsize,>=angle 90]"
      
      nodeFrame <- points2
      nodeFrame <- nodeFrame[nodeFrame$x>=min(nodeFrame[!is.na(nodeFrame$name),]$x) &
                               nodeFrame$x<=max(nodeFrame[!is.na(nodeFrame$name),]$x) & 
                               nodeFrame$y>=min(nodeFrame[!is.na(nodeFrame$name),]$y) & 
                               nodeFrame$y<=max(nodeFrame[!is.na(nodeFrame$name),]$y),]
      nodeFrame$name <- ifelse(is.na(nodeFrame$name),"~",nodeFrame$name)
      nodeFrame$nameA <- ifelse(nodeFrame$name %in% input$adjustNode, paste0(" |[module]| ",nodeFrame$name), nodeFrame$name)
      nodeLines <- vector("character",0)
      for (i in unique(nodeFrame$y)){
        createLines <- paste0(paste(nodeFrame[nodeFrame$y==i,]$nameA,collapse="&"),"\\\\\\\\")
        nodeLines <- c(nodeLines,createLines)
      }
      nodeLines <- rev(nodeLines)
      nodeLines2 <- nodeLines
      
      nodeLines <- paste0("\\\\matrix(m)[matrix of nodes, row sep=2.6em, column sep=2.8em,text height=1.5ex, text depth=0.25ex, nodes={label}] {",paste(nodeLines,collapse=""),"};")
      
      edgeLines <- vector("character",0)
      
      if(length(E(g))>=1){
        edgeFrame <- as.data.frame(ends(g,E(g)))
        edgeFrame$name <- paste0(edgeFrame$V1,"->",edgeFrame$V2)
        edgeFrame$angle <- edgeFrame$color <- edgeFrame$thick <- edgeFrame$type <- edgeFrame$loose <- NA
        edgeFrame$parent <- edgeFrame$child <- NA
        
        nodeFrame$revY <- rev(nodeFrame$y)
        
        for(i in 1:length(edgeFrame$name)){
          edgeFrame$angle[i] <- ifelse(!is.null(input[[paste0("angle",edgeFrame$name[i])]]),as.numeric(input[[paste0("angle",edgeFrame$name[i])]]),0)
          edgeFrame$color[i] <- ifelse(is.null(input[[paste0("color",edgeFrame$name[i])]]),"black",input[[paste0("color",edgeFrame$name[i])]])
          edgeFrame$thick[i] <- ifelse(is.null(input[[paste0("lineT",edgeFrame$name[i])]]),"thin",input[[paste0("lineT",edgeFrame$name[i])]])
          edgeFrame$type[i] <- ifelse(is.null(input[[paste0("lty",edgeFrame$name[i])]]),"solid",input[[paste0("lty",edgeFrame$name[i])]])
          edgeFrame$parent[i] <- paste0("(m-",(nodeFrame[nodeFrame$name==edgeFrame$V1[i],]$revY-min(nodeFrame$revY)+1),"-",
                                        (nodeFrame[nodeFrame$name==edgeFrame$V1[i],]$x-min(nodeFrame$x)+1),")")
          edgeFrame$child[i] <- paste0("(m-",(nodeFrame[nodeFrame$name==edgeFrame$V2[i],]$revY-min(nodeFrame$revY)+1),"-",
                                       (nodeFrame[nodeFrame$name==edgeFrame$V2[i],]$x-min(nodeFrame$x)+1),")")
          createEdge <- paste0(edgeFrame$parent[i]," edge [>=",input$arrowShape,", bend left = ",edgeFrame$angle[i],
                               ", color = ",edgeFrame$color[i],",",edgeFrame$type[i],",", edgeFrame$thick[i],
                               "] node[auto] {$~$} ",edgeFrame$child[i]," ")
          edgeLines <- c(edgeLines,createEdge)
        }
      }
      
      edgeLines <- paste0(pathZ,paste(edgeLines,collapse=""),";")
      
      allLines <- c(styleZ,startZ,nodeLines,edgeLines,endZ)

      tikzTemp <- paste(allLines,collapse="")


    } else{
      startZ <- "\\\\begin{tikzpicture}[>=latex]"
      endZ <- "\\\\end{tikzpicture}"

      allLines <- c(startZ,endZ)

      tikzTemp <- paste(allLines,collapse="")

    }
    aceEditor("texChange",mode="latex",value=paste(allLines,collapse="\n"), theme="cobalt")
  })
  
  output$tikzOutNew<-renderUI({

      tikzTemp <- input$texChange
      
      useLib="\\usetikzlibrary{matrix,arrows,decorations.pathmorphing}"
      
      pkgs=paste(buildUsepackage(pkg = list('tikz'),uselibrary = useLib),collapse='\n')
      
      texPreview(obj = tikzTemp,
                 stem = 'DAGimageEdit',
                 fileDir = paste0(getwd(),"/www"),
                 imgFormat = 'png',
                 returnType = 'shiny',
                 density=tex_opts$get("density"),
                 keep_pdf = TRUE,
                 usrPackages = pkgs,
                 margin = tex_opts$get("margin"),
                 cleanup = tex_opts$get("cleanup")
      )
      
      # filename <- normalizePath(file.path(paste0(getwd(),'/DAGimageEdit.png')))

      return(tags$iframe(style="height:560px; width:100%",src = "DAGimageEditDoc.pdf", 
                         scrolling="no",seamless="seamless"))
    
  })
  
  output$downloadButton2 <- downloadHandler(
    filename = function() {
      paste0("DAG",Sys.Date(),ifelse(input$downloadType2==1,".tex",
                                            ifelse(input$downloadType2==2,".png",".pdf")))
    },
    content = function(file) {
      if(input$downloadType2==1){
        myfile <- paste0(getwd(),"/www/DAGimageEditDoc.tex")
        file.copy(myfile, file)
      } else if (input$downloadType2==2){
        myfile <- paste0(getwd(),"/www/DAGimageEdit.png")
        file.copy(myfile, file)
      } else {
        myfile <- paste0(getwd(),"/www/DAGimageEditDoc.pdf")
        file.copy(myfile, file)
      } 
    }, contentType = NA
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

