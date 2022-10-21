
if (FALSE) {
  select.header.labels = c("X-axis", "Y-axis")
  select.header.values = c("x", "y")
  example.file.name = "example.csv"
}

# Module UI function
csvFileUI <- function(id,
                      label = "Upload a CSV file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  tagList(tabsetPanel(
    id = ns("csv.upload.panel"),
    type = "hidden",
    
    tabPanel(
      ns("upload.panel"),
      fileInput(ns("file"), label),
      actionLink(ns("load.example"), "Use example"),
      br(),
      downloadLink(ns("download.example"), "Download example"),
      #hr(),
      #actionButton(ns("next.button"), "Next")
    ),
    tabPanel(
      ns("option.panel"),
      h3("Graph options"),
      uiOutput(ns("list.for.select.header")),
      br(),
      h3("CSV file options"),
      checkboxInput(ns("heading"), "Has heading", value = TRUE),
      hr(),
      actionButton(ns("back.button"), "Back"),
      actionButton(ns("upload.button"), "Upload")
    )
  ))
}

# Server ------------------------------------------------------------------
# the reactive flow
# upload.file, csv read options -> d -> selectInput components -> v
#
# Module server function
# id: input id
# parent: a session object and it is usually from `server <- function(input, output, session)`
# select.header.labels: text labels visible to the users
# select.header.values: values associated with each text label
# example.file.name: file name for the example file
# upload.button: a reactive value
csvFileServer <- function(id,
                          parent,
                          select.header.labels = c("X-axis", "Y-axis"),
                          select.header.values = c("x", "y"),
                          example.file.name = "example.csv",
                          upload.button = NULL) {
  moduleServer(id,
               ## Below is the module function
               function(input, output, session) {
                 ns <- session$ns
                 useExample <- reactiveVal(FALSE)
                 observeEvent(input$load.example, {
                   cat("[csvFile-module] useExample set to TRUE\n")
                   useExample(TRUE)
                   updateTabsetPanel(
                     session = parent,
                     inputId = ns("csv.upload.panel"),
                     selected = ns("option.panel")
                   )
                 })
                 n.upload.button.pressed <- reactiveVal(0)
                 ## d stores file contents
                 d <- reactive({
                   cat("[csvFile-module] d start\n")
                   if (useExample()) {
                     read.csv(example.file.name, header = input$heading)
                   } else {
                     req(input$file)
                     print(input$file)
                     read.csv(input$file$datapath, header = input$heading)
                   }
                 })
                 v <- reactive({
                   message("v <- reactive()\n")
                   if (useExample()) {
                     message("useExample = TRUE")
                     ret <- select.header.values
                   } else {
                     message("useExample = FALSE")
                     ret <-
                       map_chr(select.header.labels, ~ input[[(.x)]] %||% "")
                   }
                   cat("v = ")
                   print(ret)
                   cat("\n")
                   ret
                 })
                 observeEvent(input$file, {
                   ns <- session$ns
                   cat("useExample set to FALSE\n")
                   useExample(FALSE)
                   
                   updateTabsetPanel(
                     session = parent,
                     inputId = ns("csv.upload.panel"),
                     selected = ns("option.panel")
                   )
                   n = colnames(d())
                   map(
                     select.header.labels,
                     ~ updateSelectInput(
                       session = parent,
                       inputId = ns(.x),
                       choices = n
                     )
                   )
                 })
                 observeEvent(input$back.button, {
                   # back button clicked
                   updateTabsetPanel(
                     session = parent,
                     inputId = ns("csv.upload.panel"),
                     selected = ns("upload.panel")
                   )
                 })
                 observeEvent(input$upload.button, {
                   n.upload.button.pressed(n.upload.button.pressed() + 1)
                   if (!is.null(upload.button)) {
                     upload.button$n <- input$upload.button
                   }
                   message("rv upload.button pressed", upload.button$n)
                   message("button inside module pressed")
                 })
                 output$list.for.select.header = renderUI({
                   # create selectInput components
                   cat('generate selectInput components\n')
                   header = colnames(d())
                   if (useExample()) {
                     cat("use example\n")
                     map2(
                       select.header.labels,
                       select.header.values,
                       ~ selectInput(ns(.x), .x,
                                     choices = header,
                                     selected = .y)
                     )
                   } else {
                     cat("not use example\n")
                     map(select.header.labels,
                         ~ selectInput(ns(.x), .x,
                                       choices = header))
                   }
                 })
                 
                 output$download.example <- downloadHandler(
                   filename = function() {
                     return(example.file.name)
                   },
                   content = function(file) {
                     file.copy(example.file.name, file)
                   }
                 )
                 
                 # Return the reactive that yields the data frame
                 ret = list(
                   data = d,
                   choices = select.header.labels,
                   values = v
                 )
                # print(ret)
                 return(ret)
               })
}

if (FALSE) {
  ui <- fluidPage(
    csvFileUI("csvFile")
  )
  
  server <- function(input, output, session) {
    upload.button.hm <- reactiveValues(n=NULL)
    
    d <- csvFileServer("csvFile", session,
                       select.header.labels = c("X-axis", "Y-axis"),
                       select.header.values = c("x", "y"),
                       example.file.name = "heatmap.csv",
                       upload.button = upload.button.hm)
    observeEvent(upload.button.hm$n, {
      message("Upload button pressed!")
      print("obs event: heatmap.plot")
      message(is.null(ret.hm$data()))
      if (!is.null(ret.hm$data())) {
        print("ret.hm = ")
        
        print( list(type = 'heatmap',
                                       data = ret.hm$data(),
                                       x = ret.hm$values()[1],
                                       y = ret.hm$values()[2]))
        }
    })  
  }
  
  shinyApp(ui, server)
  
}
