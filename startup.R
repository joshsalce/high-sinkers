library(devtools)
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(reshape2)
library(class)

# Split the data into training and testing sets
set.seed(909)

# devtools::install_github(repo = "BillPetti/baseballr")
library(baseballr)


# Imports ==============================================================================================================

#' Purpose: Retrieves Statcast data for a given start and end date, querying data in chunks of 4 days.
#'
#' @param start_date_str The start date of the season (format: yyyy-mm-dd).
#' @param end_date_str The end date of the season (format: yyyy-mm-dd).
#' 
#' @return A data frame containing the retrieved Statcast data.
GetSeasonStatcastData <- function(start_date_str, end_date_str) {
  
  # Initialize an empty data frame to store the results
  data_df = data.frame()
  
  # Generate a sequence of dates by 4 days and ensure the end date is included
  weekly_dates <- unique(
    c(seq(from = as.Date(start_date_str), to = as.Date(end_date_str), by = "4 days"), as.Date(end_date_str))
  )
  
  # Loop through all elements except the last one
  date_indices <- 1:(length(weekly_dates) - 1)
  for (i in date_indices) {
    week_start_date <- weekly_dates[i]
    
    # Determine the end date for the current query
    week_end_date <- format(as.Date(
      ifelse(i == max(date_indices), weekly_dates[i + 1], weekly_dates[i + 1] - 1)
    ), "%Y-%m-%d")
    
    # Attempt to query Statcast data
    tryCatch({
      # Print start and end date for queries
      print(paste("Query Start Date:", week_start_date, "Query End Date:", week_end_date))
      
      # Query Statcast data
      week_data <- statcast_search(
        start_date = week_start_date,
        end_date = week_end_date
      )
      
      # Append the retrieved data to the main data frame
      data_df <- bind_rows(data_df, week_data)
    }, error = function(e) {
      data_df <- GetSeasonStatcastData(start_date_str, end_date_str)
    })
  }
  
  # Return the complete data frame
  return(data_df)
}