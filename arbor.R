library(shiny)
library(treeio)
library(ggtree)
library(ggplot2)
library(ggimage)
library(aplot)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(purrr)
library(BiocManager)

if (FALSE) {
  # install dev version of ggtree if needed
  options(repos = BiocManager::repositories())
  devtools::install_github("YuLab-SMU/ggtree")
}

source('draw-functions.R') 
source('nwkFile-module.R')
source('csvFile-module.R')

debug <- TRUE
debug <- FALSE
if (debug) {
    library(reactlog)
    reactlog_enable()
    # once app has closed, display reactlog from shiny
    shiny::reactlogShow()
} 
if (FALSE) {
    library(reactlog)
    reactlog_disable()
}

ui <- dashboardPage(
    dashboardHeader(title = "Tree Data Visualization"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem('Introduction', tabName = 'introduction', icon = icon('book-open')),
            menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
            menuItem("Plots", tabName = "plot", icon = icon("images")),
            menuItem('Contact', tabName = 'contact', icon = icon('envelope'))
        ),
        collapsed = FALSE
    ),
    
    dashboardBody(
        useShinyjs(),
        tags$head(
            tags$style(HTML("
                      .shiny-html-output { overflow-x: scroll; }
                      " )
            )
        ),
        tabItems(
            tabItem(tabName = "introduction",
                    p('Abor is an R shiny app for 
                    viewing tree data and their associated data properties. Users can simply upload 
                    tree data and csv columns in Data Upload, and then 
                    the plots would be shown in Plots. Only .nwk is accepted 
                    for tree data upload. Only csv is accepted for both 
                    heat map and bar plot data upload. Users can download the plots in multiple file formats.')
            ),
            
            tabItem(tabName = "upload",
                    fluidRow(
                        column(width = 4,
                               box(title = 'Step 1: Tree', width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   nwkFileUI("treefile"),
                                   hr(),
                                   verbatimTextOutput("info.nwk")
                               )),
                        
                        column(width = 4,
                               box(title = 'Step 2: Heatmap', width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   csvFileUI("hmfile"),
                                   hr(),
                                   tableOutput("table.hm")
                               )),
                               
                        column(width = 4,
                               box(title = 'Step 2: Bar Plot', width = NULL,
                                   status = "primary", solidHeader = TRUE,
                                   collapsible = TRUE,
                                   csvFileUI("barfile"),
                                   hr(),
                                   tableOutput("table.bar")
                               )
                        )),
                   
                        box(title = "Upload Status", width = NULL, 
                            status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            verbatimTextOutput("uploadStatusText")
                        )
                    
            ),
            
            
            tabItem(tabName = "plot",
                    fluidRow(column(12, uiOutput("figure"))),
                    hr(),
                    fluidRow(
                        column(12,
                               box(id = "download.buttons",
                                   title = "Download",
                                   status = "primary", 
                                   solidHeader = TRUE,
                                   downloadButton("downloadpng", 'Download-png'),
                                   br(),
                                   downloadButton("downloadjpg", 'Download-jpg'),
                                   br(),
                                   downloadButton("downloadpdf", 'Download-pdf')))),
                    
                    fluidRow(column(12,
                                    box(id = "plot.advanced.option",
                                        title = "Advanced Options",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        collapsible = TRUE,
                                        collapsed = TRUE,
                                        numericInput("plot.width", "Plot width on screen", 700, min = 500)))),

            ),
            
            tabItem(tabName = 'contact',
                    "Authors: Xiaowei Zhan, Jennifer Liu.\nGithub: https://github.com/jliuob/Arbor"
            )
        )
    ))

server <- function(input, output, session) {
    ## this stores all data
    v <- reactiveValues(l = list())
    
    # ## UI customization
    #   shinyjs::disable('hmfile')
    #   shinyjs::disable('barfile')
    #   observeEvent(input$treefile, {
    #       enable('hmfile')
    #       enable('barfile')
    #   })
    
    # Data upload panel --------------------------------------------------------
    output$uploadStatusText <- renderText({
        l <- v$l
        nTree <- length(Filter(function(x){x$type == "tree"}, l))
        nHeatmap <- length(Filter(function(x){x$type == "heatmap"}, l))
        nBar <- length(Filter(function(x){x$type == "barplot"}, l))
        
        sprintf("Uploaded %d tree plot(s), %d heatmap(s), and %d bar plot(s)",
                nTree, nHeatmap, nBar)
    })
    
    # Rearrangement ------------------------------------------------------------
    upload.button.nwk <- reactiveValues(n=NULL)
    upload.button.hm <- reactiveValues(n=NULL)
    upload.button.bar <- reactiveValues(n=NULL)

    # observeEvent(input$treefile, {
    #     print("obs event: tree")
    #     if (!is.null(input$treefile)) {
    #         v$l[[length(v$l) + 1]] <- list(type = 'tree',
    #                                        data = read.tree(input$treefile$datapath))
    #         reset("treefile")
    #         cat("input updated by adding a tree", length(v$l), "done\n")
    #     }
    # })
    ret.nwk <- nwkFileServer(
      "nwkfile",
      session,
      example.file.name = "tree.nwk",
      upload.button = upload.button.nwk
    )
    # output$table.hm <- renderTable({
    #   head(ret.hm$data())
    # })
    output$info.nwk <- renderText({
      paste("Uploaded tree has length: ", length(ret.nwk$data()))
    })
    observeEvent(upload.button.nwk$n, {
      message("Upload button pressed!")
      print("obs event: nwk")
      message(is.null(ret.nwk$data()))
      if (!is.null(ret.nwk$data())) {
        v$l[[length(v$l) + 1]] <- list(type = 'tree',
                                       data = ret.nwk$data())
      }
    })
    
    ret.hm <- csvFileServer(
        "hmfile",
        session,
        select.header.labels = c("X-axis", "Y-axis"),
        select.header.values = c("x", "y"),
        example.file.name = "heatmap.csv",
        upload.button = upload.button.hm
    )
    output$table.hm <- renderTable({
        head(ret.hm$data())
    })
    output$choices <- renderText({
        ret.hm$choices
    })
    output$values <- renderText({
        ret.hm$values()
    })
    observeEvent(upload.button.hm$n, {
        message("Upload button pressed!")
        print("obs event: heatmap.plot")
        message(is.null(ret.hm$data()))
        if (!is.null(ret.hm$data())) {
            # print("ret.hm", ret.hm)
            v$l[[length(v$l) + 1]] <- list(type = 'heatmap',
                                           data = ret.hm$data(),
                                           x = ret.hm$values()[1],
                                           y = ret.hm$values()[2])
          # reset("heatmap"): reset only works for traditional Shiny input widget
        }
    })
    
    ret.bar <- csvFileServer(
        "barfile",
        session,
        select.header.labels = c("X-axis", "Y-axis (Tree Nodes)"),
        select.header.values = c("Group", "Label"),
        example.file.name = "bar.csv",
        upload.button = upload.button.bar
    )
    output$table.bar <- renderTable({
        head(ret.bar$data())
    })
    output$choices <- renderText({
        ret.bar$choices
    })
    output$values <- renderText({
        ret.bar$values()
    })
    observeEvent(upload.button.bar$n, {
        message("Upload button pressed!")
        print("obs event: barplot")
        message("plotWidth = ", plotWidth())
        if (!is.null(ret.bar$data())) {
            v$l[[length(v$l) + 1]] <- list(type = 'barplot',
                                           data = ret.bar$data(),
                                           x = ret.bar$values()[1],
                                           y = ret.bar$values()[2])
            # reset("barplot")
        }
    })
    
    # Show figure -------------------------------------------------------------
    plotWidth <- reactive({
        if (is.null(input$plot.width)) {
            ret = 800 * max(1, length(v$l))
        } else {
            ret = input$plot.width
        }
      message("plotWidth = ", ret)
    })
    plotInput <- reactive({
        draw(v$l)
    })  
    output$figure <- renderUI({
        plotOutput("ggplot", width = plotWidth())
    })
    
    output$ggplot <- renderPlot({
        draw(v$l)
    }, res = 96)
    
    get.raster.size <- reactive({
        size = 20
        w.h.ratio = max(1, length(v$l))
        list(height = size / w.h.ratio, width = size)
    })
    output$downloadpng <- downloadHandler(
        filename = function() {
            paste0(input$treefile, ".png")
        },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "png",
                   width = get.raster.size()$width, 
                   height = get.raster.size()$height)
        }
    )
    
    output$downloadjpg <- downloadHandler(
        filename = function() {
            paste0(input$treefile, ".jpg")
        },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "jpg",
                   width = get.raster.size()$width, 
                   height = get.raster.size()$height)
        }
    )
    get.pdf.size <- reactive({
        size = 20
        w.h.ratio = max(1, length(v$l))
        list(height = size / w.h.ratio, width = size)
    })
    output$downloadpdf <- downloadHandler(
        filename = function() {
            paste0(input$treefile, ".pdf")
        },
        content = function(file) {
            ggsave(file, plot = plotInput(), device = "pdf", 
                   width = get.pdf.size()$width, 
                   height = get.pdf.size()$height)
        }
    )
}

shinyApp(ui, server)
