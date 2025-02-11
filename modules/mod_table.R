
# modules/mod_table.R

mod_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "Data Table", width = 6, DTOutput(ns("table")))
  )
}

mod_table_server <- function(input, output, session) {
  output$table <- renderDT({
    datatable(iris)
  })
}
