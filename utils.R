library(tidyverse)
library(shiny)
library(shinyWidgets)

read_uploaded_data <- function(file) {
  if (is.null(file)) return(NULL)
  df <- read.csv(file$datapath, header = TRUE, stringsAsFactors = FALSE)
  return(df)
}

# Function to remove missing values
remove_missing_values <- function(df) {
  df <- na.omit(df)  # Removes rows with any missing values
  return(df)
}

# Function to show basic summary
get_data_summary <- function(df) {
  if (is.null(df)) return("No data available.")
  summary(df)
}