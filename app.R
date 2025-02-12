#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)

source("global.R")
source("ui.R")
source("server.R")
source("modules/mod_chart.R")
source("modules/mod_table.R")
source("functions/read_data.R")
source("functions/read_data.R") 
source("modules/file_upload_module.R")
source("modules/data_display_module.R")

shinyApp(ui = ui, server = server)