apply_datetime_shift <- function(data, brush, shift_value, shift_unit) {
  # Calculate the shift in seconds based on unit
  shift_secs <- switch(shift_unit,
                       "mins" = shift_value * 60,
                       "hours" = shift_value * 3600,
                       "days" = shift_value * 86400,
                       0)
  
  data %>%
    mutate(
      is_selected = between(as.numeric(DT_round), brush$xmin, brush$xmax) & 
                    between(mean, brush$ymin, brush$ymax),
      
      DT_round = if_else(is_selected, DT_round + seconds(shift_secs), DT_round),
      
      # Update derived date columns
      year = year(DT_round),
      week = week(DT_round),
      weekday = lubridate::wday(DT_round, week_start = 7),
      day = yday(DT_round),
      season = case_when(
        month(DT_round) %in% c(12, 1, 2, 3, 4) ~ "winter_baseflow",
        month(DT_round) %in% c(5, 6, 7) ~ "snowmelt",
        month(DT_round) %in% c(8, 9, 10, 11) ~ "monsoon",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-is_selected)
}
