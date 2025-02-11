
source("functions/data_utils.R")

server <- function(input, output, session) {
  callModule(mod_chart_server, "chart1")
  callModule(mod_table_server, "table1")
}
