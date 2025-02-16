# server.ui file

#Load modules
source("modules/file_upload_module.R")
source("modules/data_display_module.R")
source("functions/read_data.R")
source("modules/dashboard_module.R")
source("modules/gemini_module.R")

server <- function(input, output, session) {
  
  # 📂 Call the file upload module (returns reactive dataset)
  uploaded_data <- fileUploadServer("file1")
  
  # 🔍 Call the data display module (to show uploaded data in a table)
  dataDisplayServer("display1", uploaded_data)
  
  # 📊 Call the dashboard module, passing the uploaded data to it
  dashboardServer("dashboard", uploaded_data)
  
  # Pass 'filtered_data()' to the geminiServer
#  callModule(geminiServer, "gemini_module", filtered_data = filtered_data)  
  geminiServer("gemini_module", filtered_data = filtered_data)
  
  
}

