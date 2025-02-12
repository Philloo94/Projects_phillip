
# Data display module

# Data display ui module
dataDisplayUI <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns("table"))
  )
}

# Data display server module
dataDisplayServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderTable({
      req(data())
      data()
    })
  })
}
