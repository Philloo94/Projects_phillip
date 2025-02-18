library(bs4Dash)
library(shiny)
library(httr)
library(jsonlite)


## ui for the gemini module

geminiUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # AI Icon in Navbar (Only visible on Dashboard)
    conditionalPanel(
#      condition = "input.sidebar == 'dashboard'",
       condition = "input.sidebarItemExpanded == 'dashboard'",
      tags$div(
        id = ns("ai_icon"),
        class = "ai-icon-container",
        tags$i(class = "fas fa-robot ai-icon", title = "AI Insights"),
        tags$div(
          class = "ai-dropdown",
          selectInput(ns("summary_type"), "Choose Analysis Type", 
                      choices = c("Trend Summary", "Comparison Summary", "Industry Impact")),
          actionButton(ns("analyze"), "Get AI Analysis")
        )
      )
    ),
    
 # Hidden Modal Popup for AI Analysis
    bs4Modal(
      id = ns("ai_modal"),
      title = "AI Analysis",
      status = "primary",
      solidHeader = TRUE,
      closable = TRUE,
      width = "medium",
      uiOutput(ns("loading_ui")),  # Loading Spinner
      verbatimTextOutput(ns("analysis")),
      downloadButton(ns("download_analysis"), "Download Analysis")  # Export Button
    )
  )
}


geminiServer <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
# Debugging statement to check if module runs
    print("geminiServer module is running...")
    
    api_key <- Sys.getenv("GEMINI_API_KEY")
    
#    print(api_key)
    print(paste0("Using API Key: ****", substr(api_key, nchar(api_key)-4, nchar(api_key))))
    
    
    if (api_key == "") {
      stop("❌ GEMINI_API_KEY is missing! Set it in your .Renviron file.")
    }
    
    generate_prompt <- function(filtered_data, prompt_type) {
      min_year <- min(filtered_data$year, na.rm = TRUE)
      max_year <- max(filtered_data$year, na.rm = TRUE)
      unique_companies <- unique(filtered_data$parent_entity)
      num_companies <- length(unique_companies)
 #    top_companies <- paste(head(unique_companies, 5), collapse = ", ")  # Show first 5 companies
      num_to_display <- min(5, num_companies)
      top_companies <- paste(head(unique_companies, num_to_display), collapse = ", ")
      
      
      summary_stats <- paste0(
        "The dataset spans from ", min_year, " to ", max_year,
        " and includes ", num_companies, " companies such as ", top_companies, "."
      )
      
      prompt_text <- ""
      
      if (prompt_type == "Trend Summary") {
        prompt_text <- paste0(
          "In not more than 300 words, please analyze the trend of total emissions from ", min_year, " to ", max_year, ". ",
          "Identify significant increases, decreases, or patterns in emissions. ", 
          "The key companies in this dataset are: ", top_companies, ". ", 
          summary_stats
        )
      } else if (prompt_type == "Comparison Summary") {
        first_year <- min(filtered_data$year, na.rm = TRUE)
        last_year <- max(filtered_data$year, na.rm = TRUE)
        first_total <- sum(filtered_data$total_emissions_MtCO2e[filtered_data$year == first_year], na.rm = TRUE)
        last_total <- sum(filtered_data$total_emissions_MtCO2e[filtered_data$year == last_year], na.rm = TRUE)
        percent_change <- round(((last_total - first_total) / first_total) * 100, 2)
        
        prompt_text <- paste0(
          "In not more than 300 words, please compare the total emissions in ", first_year, " and ", last_year, ". ",
          "The total emissions changed by ", percent_change, "%. ", 
          "Make the analysis based on the following contributing companies: ", top_companies, ". ",
          summary_stats
        )
      } else if (prompt_type == "Industry Impact") {
        prompt_text <- paste0(
          "In not more than 300 words, please evaluate the oil and gas industry’s environmental impact over time. ",
          "Assess whether emission reduction efforts have been effective. ",
          "The dataset contains emissions data from companies such as ", top_companies, ". ", 
          summary_stats
        )
      }
      
      return(prompt_text)
    }
    
    response_text <- reactiveVal("")
    loading <- reactiveVal(FALSE)
    
    # observeEvent(input$analyze, {
    #   req(input$summary_type, filtered_data())
    
    # React to the AI icon being clicked
    observeEvent(input$ai_icon_clicked, {
      req(input$summary_type, filtered_data())  # Ensure that both the summary type and filtered data are available
      
      response_text("")  # Clear previous text
      loading(TRUE)  # Show loading spinner
      
      prompt_text <- generate_prompt(filtered_data(), input$summary_type)
      
      url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateText?key=", api_key)
      
      response <- tryCatch({
        httr::POST(
          url,
          # body = toJSON(list(
          #   prompt = prompt_text
          # ), auto_unbox = TRUE),
          body = toJSON(list(
            contents = list(
              list(
                parts = list(
                  list(text = prompt_text)
                )
              )
            )
          ), auto_unbox = TRUE),
          encode = "json",
          content_type_json()
        )
      }, error = function(e) {
        return(NULL)
      })
      
      loading(FALSE)  # Hide loading spinner
      
      if (is.null(response)) {
        response_text("⚠️ API request failed. Please check your internet connection or API key.")
        return()
      }
      
      if (response$status_code != 200) {
        response_text(paste0("⚠️ API Error: ", response$status_code, " - ", content(response, as = "text")))
        return()
      }
      
      response_content <- content(response, as = "text", encoding = "UTF-8")
      response_json <- fromJSON(response_content, flatten = TRUE)
      
      if (is.null(response_json$candidates) || length(response_json$candidates) == 0) {
        response_text("⚠️ No valid response received from Gemini API.")
        return()
      }
      
#      ai_output <- response_json$candidates[[1]]$output
      if (length(response_json$candidates) == 0 || is.null(response_json$candidates[[1]]$output)) {
        response_text("⚠️ No valid response received from Gemini API.")
        return()
      }
      
      
      ai_output <- response_json$candidates[[1]]$output
      
      
      formatted_response <- gsub("\n", "\n\n", ai_output)  # Add paragraph spacing
      formatted_response <- paste0("📊 **AI Analysis:**\n\n", formatted_response)
      
      response_text(formatted_response)
    })
    
    # output$loading_ui <- renderUI({
    #   if (loading()) {
    #     tags$div(style = "text-align:center; font-size:18px;",
    #              tags$img(src = "loading.gif", height = "50px"),
    #              tags$p("Generating analysis... Please wait."))
    #   } else {
    #     NULL
    #   }
    # })
    
    # output$loading_ui <- renderUI({
    #   conditionalPanel(
    #     condition = "input.analyze > 0",  # Show only after the "Get AI Analysis" button is clicked
    #     tags$div(
    #       class = "loading-container",
    #       tags$img(src = "www/loading.gif", class = "loading-spinner"),
    #       tags$p("Generating analysis... Please wait.", class = "loading-text")
    #     )
    #   )
    # })
    
    
    output$loading_ui <- renderUI({
      if (loading()) {  # ✅ Show only when `loading()` is TRUE
        tags$div(
          class = "loading-container",
          tags$img(src = "www/loading.gif", class = "loading-spinner"),
          tags$p("Generating analysis... Please wait.", class = "loading-text")
        )
      } else {
        NULL  # ✅ Hide when `loading()` is FALSE
      }
    })
    
    
    # output$loading_ui <- renderUI({
    #   if (loading()) {
    #     tags$div(class = "loading-container",
    #              tags$img(src = "loading.gif", class = "loading-spinner"),
    #              tags$p("Generating analysis... Please wait.", class = "loading-text"))
    #   } else {
    #     NULL
    #   }
    # })
    
    
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

