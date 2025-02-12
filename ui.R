
ui <- dashboardPage(
  dashboardHeader(title = "Production-Ready Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Charts", tabName = "charts", icon = icon("chart-line")),
      menuItem("Tables", tabName = "tables", icon = icon("table"))
    )
  ),
  dashboardBody(
    mod_chart_ui("chart1"),
    mod_table_ui("table1")
  )
)


