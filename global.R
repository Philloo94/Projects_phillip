
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

# Load data
data <- iris


# Define a helper function
calculate_summary <- function(df){
  summary(df)
}