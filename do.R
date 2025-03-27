#' Add Event Identification Columns
#'
#' Purpose: Adds swing/no-swing, contact/swinging-strike, and foul/fair event columns for dataframe  
#' based on description values in pitch data.
#'
#' @param dataframe A dataframe (list or vector format) from pitch data.
#' @return A modified dataframe with added Swing_Event, Contact_Event, and Foul_Event columns.
add_id_cols <- function(dataframe) {
  
  # Define non-swing, contact, swinging strike, and foul events
  non_swings <- c("ball", "called_strike", "hit_by_pitch", "blocked_ball")
  contact_events <- c("foul", "hit_into_play", "foul_bunt")
  sw_strikes <- c("swinging_strike", "foul_tip", "bunt_foul_tip", "missed_bunt", "swinging_strike_blocked")
  foul_events <- c("foul", "foul_bunt")
  
  #  Add columns for swing, contact, foul/fair events
  dataframe <- dataframe %>%
    mutate(
      swing_value = ifelse(description %in% non_swings, 0, 1),
      contact_value = ifelse(description %in% contact_events, 1, 
                             ifelse(description %in% sw_strikes, 0, NA)),
      foul_value = ifelse(type == "X", 0, 
                          ifelse(description %in% foul_events, 1, NA)),
    )
  
  return(dataframe)
}