
## Dashboard UI Function
dashboardUI <- function(id) {
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "dashboard",
    
    # Control Bar: Year Filter
    bs4DashControlbar(
      skin = "light",
      title = "Filters",
      sliderInput(ns("year_filter"), 
                  "Select Year Range:", 
                  min = 2000, max = 2023, 
                  value = c(2010, 2023), 
                  step = 1),
    
    # Parent Entity Filter
      selectInput(ns("parent_entity_filter"), 
                   "Select Parent Entity:", 
                   choices = NULL, multiple = TRUE),
    
    # Parent Type Filter
      selectInput(ns("parent_type_filter"), 
                  "Select Parent Type:", 
                  choices = NULL, 
#                  selected = "All",
                  multiple = TRUE), 
    
    # Commodity Filter
      selectInput(ns("commodity_filter"), 
                  "Select Commodity:", 
                  choices = NULL, 
#                  selected = "All",
                  multiple = TRUE), 
    
    # Product unit Filter
      selectInput(ns("production_unit_filter"), 
                  "Select Production Unit:", 
                  choices = NULL, 
  #                selected = "All",
                  multiple = TRUE)
    
    ),
    
    # First row: Summary Value Boxes
    fluidRow(
      bs4ValueBoxOutput(ns("total_companies"), width = 4),
      bs4ValueBoxOutput(ns("total_emissions"), width = 4),
      bs4ValueBoxOutput(ns("top_emitter"), width = 4)
    ),
    
    # Second row: Emissions Trend Plot
    fluidRow(
      bs4Card(
        title = "Emissions Over Time",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        plotOutput(ns("emissions_trend"))
      )
    ),
    
    # Third row: Emissions Data Table
    fluidRow(
      bs4Card(
        title = "Emissions Data Table",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        dataTableOutput(ns("emissions_table"))
      )
    )
  )
}

## Dashboard Server Function
dashboardServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Debugging: Check if data is available
    observe({
      if (is.null(data())) {
        print("No data available!")  # If data is NULL, print a warning
      } else {
        print("Data found, preview:")
        print(head(data()))
      }
    })
    
    # Debugging: Print when function starts
    print("dashboardServer function is running...")
    
    # Populate Filter Dropdowns Based on Available Data
    observe({
      req(data())  # Ensure data is loaded before updating UI
      
      # updateSelectizeInput(session, "company_filter", choices = unique(data()$parent_entity), server = TRUE)
      # updateSelectInput(session, "industry_filter", choices = unique(data()$industry))
      # updateSelectInput(session, "region_filter", choices = unique(data()$region))
      # Dynamically update Parent Entity filter choices
    
      updateSelectInput(session, "parent_entity_filter", 
                        choices = c("All", sort(unique(data()$parent_entity))), 
                        selected = "All")
      
      updateSelectInput(session, "parent_type_filter", 
                        choices = c("All", sort(unique(data()$parent_type))), 
                        selected = "All")
      
      updateSelectInput(session, "commodity_filter", 
                        choices = c("All", sort(unique(data()$commodity))), 
                        selected = "All")
      
      updateSelectInput(session, "production_unit_filter", 
                        choices = c("All", sort(unique(data()$production_unit))), 
                        selected = "All")
    })
    
    # Reactive function to filter data based on the selected year range
    filtered_data <- reactive({
      req(data())  # Ensure data is available
      
      # Debugging: Print input values
      print(paste("Year filter values:", input$year_filter))
      
      # Ensure input$year_filter exists and is valid
      if (length(input$year_filter) < 2) {
        print("Error: year_filter has invalid length")
        return(NULL)  # Return NULL to prevent further errors
      }
      
      # Ensure `year` column exists in dataset
      if (!"year" %in% names(data())) {
        stop("Error: Column 'year' not found in dataset")
      } 
      
      # Filter data based on selected year range
#      data() %>%
      filtered <- data() %>%      
        filter(year >= input$year_filter[1] & year <= input$year_filter[2])
      
      # Apply parent entity filter
      if ("All" %in% input$parent_entity_filter) {
        # If "All" is selected, do not filter by parent entity (include all parent entities)
        # filtered <- filtered
      } else {
        # If specific parent entities are selected, filter the data for the selected parent entities
        filtered <- filtered %>%
          filter(parent_entity %in% input$parent_entity_filter)
      }
      
      # Apply parent type filter
      if ("All" %in% input$parent_type_filter) {
        # If "All" is selected, do not filter by parent type (include all parent types)
         filtered <- filtered
      } else {
        # If specific parent types are selected, filter the data for the selected parent types
        filtered <- filtered %>%
          filter(parent_type %in% input$parent_type_filter)
      }
      
      # Apply commodity filter
      if ("All" %in% input$commodity_filter) {
        # If "All" is selected, do not filter by commodity (include all commodities)
         filtered <- filtered
      } else {
        # If specific commodities are selected, filter the data for the selected commodities
        filtered <- filtered %>%
          filter(commodity %in% input$commodity_filter)
      }
      
      # Apply production unit filter
      if ("All" %in% input$production_unit_filter) {
        # If "All" is selected, do not filter by production unit (include all production units)
         filtered <- filtered
      } else {
        # If specific production units are selected, filter the data for the selected product units
        filtered <- filtered %>%
          filter(production_unit %in% input$production_unit_filter)
      }
      
      return(filtered)  # ✅ Ensure the filtered dataset is returned      
      
    })
######### latest code added    
    # Return filtered_data to make it accessible outside
    return(filtered_data)   
    
################    
    
    # Total Companies Value Box
    output$total_companies <- renderbs4ValueBox({
#### latest code added
   #   req(filtered_data())  # Ensure that filtered_data() exists      
########      
      bs4ValueBox(
#       value = length(unique(filtered_data()$parent_entity)),  # Count unique companies
        value = tags$span(length(unique(filtered_data()$parent_entity))),  # Wrap the value in a shiny tag (span)        
        subtitle = "Total Companies",
        icon = icon("industry"),
        color = "primary"
      )
    })
    
    # Total Emissions Value Box
    output$total_emissions <- renderbs4ValueBox({
#### latest code added
   #   req(filtered_data())  # Ensure that filtered_data() exists      
########           
      bs4ValueBox(
#       value = round(sum(filtered_data()$total_emissions_MtCO2e, na.rm = TRUE), 2),  # Sum of total emissions
        value = tags$span(round(sum(filtered_data()$total_emissions_MtCO2e, na.rm = TRUE), 2)),  # Wrap the value in a shiny tag        
        subtitle = "Total Emissions",
        icon = icon("cloud"),
        color = "danger"
      )
    })
    
    # Top Emitter Value Box
    # output$top_emitter <- renderbs4ValueBox({
    #   top_company <- filtered_data() %>%
    #     group_by(parent_entity) %>%
    #     summarise(TotalEmissions = sum(total_emissions_MtCO2e, na.rm = TRUE)) %>%
    #     arrange(desc(TotalEmissions)) %>%
    #     slice(1) %>%
    #     pull(parent_entity)  # Get the top emitter
    #   
    #   bs4ValueBox(
    #     value = tags$span(top_company),  # Wrap the value in a span tag
    #     subtitle = "Top Emitter",
    #     icon = icon("fire"),
    #     color = "warning"
    #   )
    # })
    
    # Top Emitter Value Box
    output$top_emitter <- renderbs4ValueBox({
      #### latest code added
      #req(filtered_data())  # Ensure that filtered_data() exists      
      ########           
      # Ensure filtered data is valid
      data_to_check <- filtered_data()
      
      # Debugging: Print data preview to check if filtered_data() is returning correct data
      print(head(data_to_check))
      
      # Ensure there is data to process
      if (nrow(data_to_check) == 0) {
        return(bs4ValueBox(
          value = "No Data",
          subtitle = "Top Emitter",
          icon = icon("fire"),
          color = "warning"
        ))
      }
      
      top_company <- data_to_check %>%
        group_by(parent_entity) %>%
        summarise(TotalEmissions = sum(total_emissions_MtCO2e, na.rm = TRUE)) %>%
        arrange(desc(TotalEmissions)) %>%
        slice(1) %>%
        pull(parent_entity)  # Get the top emitter
      
      # Wrap the value in a shiny tag (e.g., tags$span)
      bs4ValueBox(
        value = tags$span(top_company),  # Wrap the value in a span tag
        subtitle = "Top Emitter",
        icon = icon("fire"),  # Use the icon() function to wrap the icon name
        color = "warning"
      )
    })
    
    # Emissions Trend Plot
    output$emissions_trend <- renderPlot({
    #   ggplot(filtered_data(), aes(x = year, y = total_emissions_MtCO2e, color = parent_entity)) +
    #     geom_line() + theme_minimal() +
    #     labs(title = "Emissions Over Time", x = "Year", y = "Total Emissions")
    # })

        
    df <- filtered_data()  # Access filtered data reactively

    df$year <- as.numeric(df$year)  # Ensure year is numeric if necessary
    
#######      

    # ggplot(df, aes(x = year, y = total_emissions_MtCO2e, color = parent_entity)) +
    #   geom_line() +
    #   theme_minimal() +
    #   labs(
    #     title = "Emissions Over Time",
    #     x = "Year",
    #     y = "Total Emissions"
    #   ) +
    #   scale_x_continuous(
    #     breaks = seq(min(df$year), max(df$year), by = 1)  # Ensure gridlines for each year
    #   ) +
    #   theme(
    #     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
    #     panel.grid.major = element_line(color = "gray80")    # Optional: add light gridlines for better readability
    #   )
##########
    
    ggplot(df, aes(x = year, y = total_emissions_MtCO2e, color = parent_entity)) +
      geom_line() + 
      theme_minimal() +
      labs(
        title = "Emissions Over Time",
        x = "Year", 
        y = "Total Emissions"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        panel.grid.major = element_line(color = "gray80")    # Optional: add light gridlines for better readability
      )    
######      
      
  })
    
    # Emissions Data Table
    output$emissions_table <- renderDataTable({
      datatable(filtered_data(), options = list(pageLength = 10))
    })
 })
}
