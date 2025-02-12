
# Load modules
source("modules/file_upload_module.R")
source("modules/data_display_module.R")
source("functions/read_data.R") 

ui <- fluidPage(
  
  # Title of the application
  titlePanel("Carbon Majors Emissions Data"),
  
  # File upload module UI
  fileUploadUI("file1"),
  
  # Display module UI
  dataDisplayUI("display1")
)

