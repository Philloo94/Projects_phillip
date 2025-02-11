
# modules/mod_chart.R

mod_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "Histogram", width = 6, plotOutput(ns("plot")))
  )
}

mod_chart_server <- function(input, output, session) {
  output$plot <- renderPlot({
    hist(rnorm(100), col = "skyblue", main = "Random Histogram")
  })
}
