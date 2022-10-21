
if (FALSE) {
  select.header.labels = c("X-axis", "Y-axis")
  select.header.values = c("x", "y")
  example.file.name = "tree.nwk"
}

# Module UI function
nwkFileUI <- function(id,
                      label = "Upload a Newick file") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  tagList(tabsetPanel(
    id = ns("nwk.upload.panel"),
    type = "hidden",
    
    tabPanel(
      ns("upload.panel"),
      fileInput(ns("file"), label, buttonLabel = "Upload",
                accept = ".nwk",
                multiple = FALSE),
      actionLink(ns("load.example"), "Use example"),
      br(),
      downloadLink(ns("download.example"), "Download example"),
      #hr(),
      #actionButton(ns("next.button"), "Next")
    ),
    tabPanel(
      ns("option.panel"),
      h3("File selected"),
      #uiOutput(ns("list.for.select.header")),
      #br(),
      #h3("Newick file options"),
      # checkboxInput(ns("heading"), "Has heading", value = TRUE),
      # TODO: show # of tree nodes, depths
      # hr(),
      actionButton(ns("back.button"), "Back"),
      actionButton(ns("upload.button"), "Upload")
    )
  ))
}

# Server ------------------------------------------------------------------
# the reactive flow
# upload.file, nwk read options -> d -> selectInput components -> v
#
# Module server function
# id: input id
# parent: a session object and it is usually from `server <- function(input, output, session)`
# select.header.labels: text labels visible to the users
# select.header.values: values associated with each text label
# example.file.name: file name for the example file
# upload.button: a reactive value
nwkFileServer <- function(id,
                          parent,
                          # select.header.labels = c("X-axis", "Y-axis"),
                          # select.header.values = c("x", "y"),
                          example.file.name = "tree.nwk",
                          upload.button = NULL) {
  moduleServer(id,
               ## Below is the module function
               function(input, output, session) {
                 ns <- session$ns
                 useExample <- reactiveVal(FALSE)
                 observeEvent(input$load.example, {
                   cat("[nwkFile-module] useExample set to TRUE\n")
                   useExample(TRUE)
                   updateTabsetPanel(
                     session = parent,
                     inputId = ns("nwk.upload.panel"),
                     selected = ns("option.panel")
                   )
                 })
                 n.upload.button.pressed <- reactiveVal(0)
                 ## d stores file contents
                 d <- reactive({
                   cat("[nwkFile] d start\n")
                   if (useExample()) {
                     read.tree(example.file.name)
                   } else {
                     req(input$file)
                     print(input$file)
                     read.tree(input$file$datapath)
                   }
                 })
                 # v <- reactive({
                 #   message("v <- reactive()\n")
                 #   if (useExample()) {
                 #     message("useExample = TRUE")
                 #     ret <- select.header.values
                 #   } else {
                 #     message("useExample = FALSE")
                 #     ret <-
                 #       map_chr(select.header.labels, ~ input[[(.x)]] %||% "")
                 #   }
                 #   cat("v = ")
                 #   print(ret)
                 #   cat("\n")
                 #   ret
                 # })
                 observeEvent(input$file, {
                   ns <- session$ns
                   cat("useExample set to FALSE\n")
                   useExample(FALSE)
                   
                   updateTabsetPanel(
                     session = parent,
                     inputId = ns("nwk.upload.panel"),
                     selected = ns("option.panel")
                   )
                   # n = colnames(d())
                   # map(
                   #   select.header.labels,
                   #   ~ updateSelectInput(
                   #     session = parent,
                   #     inputId = ns(.x),
                   #     choices = n
                   #   )
                   # )
                 })
                 observeEvent(input$back.button, {
                   # back button clicked
                   updateTabsetPanel(
                     session = parent,
                     inputId = ns("nwk.upload.panel"),
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
                 # output$list.for.select.header = renderUI({
                 #   # create selectInput components
                 #   cat('generate selectInput components\n')
                 #   header = colnames(d())
                 #   if (useExample()) {
                 #     cat("use example\n")
                 #     map2(
                 #       select.header.labels,
                 #       select.header.values,
                 #       ~ selectInput(ns(.x), .x,
                 #                     choices = header,
                 #                     selected = .y)
                 #     )
                 #   } else {
                 #     cat("not use example\n")
                 #     map(select.header.labels,
                 #         ~ selectInput(ns(.x), .x,
                 #                       choices = header))
                 #   }
                 # })
                 
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
                   data = d # ,
                   # choices = select.header.labels,
                   # values = v
                 )
                 print(ret)
                 return(ret)
               })
}


if (FALSE) {
  ui <- fluidPage(
    nwkFileUI("nwkFile")
  )
  
  server <- function(input, output, session) {
    upload.button.nwk <- reactiveValues(n=NULL)
    
    ret.nwk <- nwkFileServer("nwkFile", session,
                             example.file.name = "tree.nwk",
                             upload.button = upload.button.nwk)
    observeEvent(upload.button.nwk$n, {
      message("Upload button pressed!")
      print("obs event: heatmap.plot")
      message(is.null(ret.nwk$data()))
      if (!is.null(ret.nwk$data())) {
        print("ret.nwk = ")
        
        print( list(type = 'tree',
                    data = ret.nwk$data()))
      }
    })  
  }
  
  shinyApp(ui, server)
}

## adapter from: summary.phylo()
to.summary.text <- function(object, line.break = "\n") {
  textCon = textConnection("ret", open = "w")
  
  nb.tip <- length(object$tip.label)
  nb.node <- object$Nnode
  cat(file = textCon, "  Number of tips:", nb.tip, line.break)
  cat(file = textCon, "  Number of nodes:", nb.node, line.break)
  if (is.null(object$edge.length)) 
    cat(file = textCon, "  No branch lengths.", line.break)
  else {
    cat(file = textCon, "  Branch lengths:", line.break)
    cat(file = textCon, "    mean:", mean(object$edge.length), line.break)
    cat(file = textCon, "    variance:", var(object$edge.length), line.break)
    cat(file = textCon, "    min:", min(object$edge.length), line.break)
    cat(file = textCon, "    median:", median(object$edge.length), line.break)
    cat(file = textCon, "    max:", max(object$edge.length), line.break)
    # cat(file = textCon, "    distribution summary:\n")
    # print(summary(object$edge.length)[-4])
  }
  if (is.null(object$root.edge)) 
    cat(file = textCon, "  No root edge.", line.break)
  else cat(file = textCon, "  Root edge:", object$root.edge, line.break)
  if (nb.tip <= 10) {
    cat(file = textCon, "  Tip labels: [", object$tip.label[1])
    cat(file = textCon, paste(", ", object$tip.label[-1]), "]")
    cat(file = textCon, sep = line.break)
  }
  else {
    cat(file = textCon, "  First ten tip labels: [", object$tip.label[1])
    cat(file = textCon, paste(", ", object$tip.label[2:10], "]")) 
    cat(file = textCon, sep = line.break)
  }
  cat(file = textCon, line.break)
  
  if (is.null(object$node.label)) 
    cat(file = textCon, "  No node labels.", line.break)
  else {
    if (nb.node <= 10) {
      cat(file = textCon, "  Node labels: [")
      cat(file = textCon, paste(", ", object$node.label, "]"))
      cat(file = textCon, line.break)
    }
    else {
      cat(file = textCon, "  First ten node labels: [")
      cat(file = textCon, paste(", ", object$node.label[1:10], "]"), 
          sep = line.break)
    }
  }  
  
  close(textCon)
  ret
}
