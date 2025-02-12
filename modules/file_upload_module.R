
# File upload UI module

fileUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload CSV File", accept = c(".csv")),
    radioButtons(ns("sep"), "Separator:", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
    actionButton(ns("load"), "Load Data")
  )
}

# File upload server module

fileUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactiveVal(NULL)  # Store uploaded data
    
    observeEvent(input$load, {
      req(input$file)
      data(read_data(input$file$datapath, sep = input$sep))
    })
    
    return(data)
  })
}
