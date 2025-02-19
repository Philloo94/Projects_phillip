library(bs4Dash)
library(shiny)
library(httr)
library(jsonlite)



geminiUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    # ✅ Enable JavaScript support
    
    # AI Icon & Analysis Selection (Only on Dashboard)
    conditionalPanel(
      condition = "input.sidebarItemExpanded == 'dashboard'",
      tags$div(
        class = "ai-icon-container",
        
        # AI Robot Icon (Visual Only)
        tags$i(class = "fas fa-robot ai-icon", title = "AI Insights"),
        
        # Dropdown for selecting analysis type (Triggers AI Request)
        selectInput(
          ns("summary_type"),
          "Choose Analysis Type",
          choices = c("Trend Summary", "Comparison Summary", "Industry Impact")
        )
      )
    )
    
  )
}



geminiServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    # Debugging statement to check if module runs
    
### lastest code added
    # Debugging: Check if filtered_data is accessible
    observe({
      print("Gemini Module: Checking filtered data...")
      print(head(filtered_data()))
    }) 
    
    # Example: Use filtered_data inside gemini_module
    output$summary_table <- renderTable({
      req(filtered_data())  # Ensure data is available before rendering
      filtered_data()  # Return the filtered dataset
    })
    
#########
    
    print("geminiServer module is running...")
    
    api_key <- Sys.getenv("GEMINI_API_KEY")
    
    #    print(api_key)
    print(paste0("Using API Key: ****", substr(
      api_key, nchar(api_key) - 4, nchar(api_key)
    )))
    
    
    if (api_key == "") {
      stop("❌ GEMINI_API_KEY is missing! Set it in your .Renviron file.")
    }
    
    
    generate_prompt <- function(filtered_data, prompt_type) {
      data <- filtered_data()  # ✅ Extract data from the reactive expression
      
      # Ensure data is not empty
      if (is.null(data) || nrow(data) == 0) {
        return("⚠️ No data available for analysis.")
      }
      
      min_year <- min(data$year, na.rm = TRUE)
      max_year <- max(data$year, na.rm = TRUE)
      unique_companies <- unique(data$parent_entity)
      num_companies <- length(unique_companies)
      
      num_to_display <- min(5, num_companies)  # Display up to 5 companies
      top_companies <- paste(head(unique_companies, num_to_display), collapse = ", ")
      
      summary_stats <- paste0(
        "The dataset spans from ",
        min_year,
        " to ",
        max_year,
        " and includes ",
        num_companies,
        " companies such as ",
        top_companies,
        "."
      )
      
      prompt_text <- ""
      
      if (prompt_type == "Trend Summary") {
        prompt_text <- paste0(
          "In not more than 300 words, please analyze the trend of total emissions from ",
          min_year,
          " to ",
          max_year,
          ". ",
          "Identify significant increases, decreases, or patterns in emissions. ",
          "The key companies in this dataset are: ",
          top_companies,
          ". ",
          summary_stats
        )
      } else if (prompt_type == "Comparison Summary") {
        first_year <- min(data$year, na.rm = TRUE)
        last_year <- max(data$year, na.rm = TRUE)
        first_total <- sum(data$total_emissions_MtCO2e[data$year == first_year], na.rm = TRUE)
        last_total <- sum(data$total_emissions_MtCO2e[data$year == last_year], na.rm = TRUE)
        percent_change <- ifelse(first_total == 0, 0, round(((last_total - first_total) / first_total
        ) * 100, 2))
        
        prompt_text <- paste0(
          "In not more than 300 words, please compare the total emissions in ",
          first_year,
          " and ",
          last_year,
          ". ",
          "The total emissions changed by ",
          percent_change,
          "%. ",
          "Make the analysis based on the following contributing companies: ",
          top_companies,
          ". ",
          summary_stats
        )
      } else if (prompt_type == "Industry Impact") {
        prompt_text <- paste0(
          "In not more than 300 words, please evaluate the oil and gas industry’s environmental impact over time. ",
          "Assess whether emission reduction efforts have been effective. ",
          "The dataset contains emissions data from companies such as ",
          top_companies,
          ". ",
          summary_stats
        )
      }
      
      return(prompt_text)
    }
    
    
    response_text <- reactiveVal("")
    loading <- reactiveVal(FALSE)
    
    observeEvent(input$summary_type, {
      req(input$summary_type, filtered_data())  # Ensure both summary type and filtered data are available
      
      response_text("")  # Clear any previous analysis
      loading(TRUE)  # Show the loading spinner
      
      # Generate the prompt text based on selected summary type and filtered data
      prompt_text <- generate_prompt(filtered_data(), input$summary_type)
      
      # Define the API URL for the Gemini service
      url <- paste0(
        "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateText?key=",
        api_key
      )
      
      # Send the request to the API
      response <- tryCatch({
        httr::POST(
          url,
          body = toJSON(list(contents = list(
            list(parts = list(list(text = prompt_text)))
          )), auto_unbox = TRUE),
          encode = "json",
          content_type_json()
        )
      }, error = function(e) {
        return(NULL)  # Return NULL if there's an error
      })
      
      if (is.null(response)) {
        response_text("⚠️ API request failed. Please check your internet connection or API key.")
        loading(FALSE)  # Hide loading spinner
        return()
      }
      
      if (response$status_code != 200) {
        response_text(paste0(
          "⚠️ API Error: ",
          response$status_code,
          " - ",
          content(response, as = "text")
        ))
        loading(FALSE)  # Hide loading spinner
        return()
      }
      
      response_content <- content(response, as = "text", encoding = "UTF-8")
      response_json <- fromJSON(response_content, flatten = TRUE)
      
      if (is.null(response_json$candidates) ||
          length(response_json$candidates) == 0) {
        response_text("⚠️ No valid response received from Gemini API.")
        loading(FALSE)  # Hide loading spinner
        return()
      }
      
      ai_output <- response_json$candidates[[1]]$output
      formatted_response <- gsub("\n", "\n\n", ai_output)  # Format for readability
      formatted_response <- paste0("📊 **AI Analysis:**\n\n", formatted_response)
      
      response_text(formatted_response)  # Store the AI-generated response
      loading(FALSE)  # Hide the loading spinner
      
      # Show the modal with the analysis result once it's ready
      showModal(
        bs4Modal(
          id = ns("ai_modal"),
          title = "AI Analysis",
          status = "primary",
          solidHeader = TRUE,
          closable = TRUE,
          width = "medium",
          uiOutput(ns("loading_ui")),
          # Loading spinner will be shown here
          verbatimTextOutput(ns("analysis")),
          downloadButton(ns("download_analysis"), "Download Analysis")
        )
      )
    })
    
    output$loading_ui <- renderUI({
      if (loading()) {
        # ✅ Show only when `loading()` is TRUE
        tags$div(
          class = "loading-container",
          tags$img(src = "www/loading.gif", class = "loading-spinner"),
          tags$p("Generating analysis... Please wait.", class = "loading-text")
        )
      } else {
        NULL  # ✅ Hide when `loading()` is FALSE
      }
    })
    
    output$analysis <- renderText({
      req(response_text())
      response_text()
    })
    
    output$download_analysis <- downloadHandler(
      filename = function() {
        paste0("AI_Analysis_", Sys.Date(), ".txt")
      },
      content = function(file) {
        writeLines(response_text(), file)
      }
    )
  })
}

