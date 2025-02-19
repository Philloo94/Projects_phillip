
# Load modules
source("modules/file_upload_module.R")
source("modules/data_display_module.R")
source("functions/read_data.R")
source("modules/dashboard_module.R")
source("modules/gemini_module.R")

# library(shiny)
# library(bs4Dash)


ui <- bs4DashPage(
  
#  🔹 HEADER (Title Bar)
  # header = bs4DashNavbar(
  #   title = "Dashboard"
  # ),
  
#  🔹 HEADER (Title Bar)
  header = bs4DashNavbar(
    # Add the CSS class to the title
    tags$div(
      class = "navbar-title",  # This will apply the styles from style.css
      "Carbon Emissions in Energy Sector"
    )
  ),
  
  # 🔹 SIDEBAR (Navigation)
  sidebar = bs4DashSidebar(
    skin = "light",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      bs4SidebarMenuItem("Data Table", tabName = "data_table", icon = icon("table")),
      bs4SidebarMenuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line"))
    )
  ),
  
  # 🔹 BODY (Main Content)
  body = bs4DashBody(
    
    
    # Line to load CSS file
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    bs4TabItems(
      
      # 📂 Upload Data Tab
      bs4TabItem(tabName = "upload",
                 fluidRow(
                   bs4Card(
                     title = "Upload CSV File",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = 12,
                     fileUploadUI("file1")
                   )
                 )
      ),
      
      # 📊 Data Table Tab
      bs4TabItem(tabName = "data_table",
                 fluidRow(
                   bs4Card(
                     title = "Emissions Data Table",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     width = 12,
                     dataDisplayUI("display1")
                   )
                 )
      ),

      #######
      # ✅ Dashboard Tab (Corrected)
      bs4TabItem(tabName = "dashboard",  # Add tabName
                 dashboardUI("dashboard"),  # Place dashboardUI inside bs4TabItem
                 geminiUI("gemini_module")
      )
      

    ),
    
#   geminiUI("gemini_module")
  )
)


