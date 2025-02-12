# server.ui file

#Load modules
source("modules/file_upload_module.R")
source("modules/data_display_module.R")
source("functions/read_data.R") 

server <- function(input, output, session) {
  
# Call the file upload module, with id "file1"
uploaded_data <- fileUploadServer("file1")
  
# Call the data display module, with id "display1"
dataDisplayServer("display1", uploaded_data)

}
