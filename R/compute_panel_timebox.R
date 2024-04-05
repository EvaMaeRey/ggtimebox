compute_panel_timebox <- function(data, scales, break_time = 3, start_hour = 9){
  
  start_time_scalar <- start_hour * 60
  
  data |> 
    dplyr::mutate(full_time = .data$minutes + break_time) |>
    dplyr::mutate(end_time_minutes = cumsum(.data$full_time)) |>
    dplyr::mutate(start_time_minutes = lag(.data$end_time_minutes) |>
                    tidyr::replace_na(0)) |>
    dplyr::mutate(clock_start = Sys.Date() + 
             minutes(start_time_minutes) + hours(start_hour)) |>
    dplyr::mutate(clock_end = Sys.Date() + 
             minutes(end_time_minutes) + hours(start_hour)) |>
    dplyr::mutate(y = start_time_minutes) |>
    dplyr::mutate(x = 0) |>
    dplyr::mutate(ymin = start_time_minutes,
           ymax = end_time_minutes) |>
    dplyr::mutate(xmin = 0,
           xmax = 1) |>
    dplyr::mutate(task_and_minutes = paste(task, minutes)) |>
    dplyr::mutate(clock_hour_minute = 
                    hms::as_hms(clock_start) %>% stringr::str_remove("...$"))
  
}

StatTimebox <- ggplot2::ggproto(`_class` = "StatTimebox",
                     `_inherit` = ggplot2::Stat,
                     compute_panel = compute_panel_timebox,
                     required_aes = c("task", "minutes"),
                     default_aes = ggplot2::aes(label =
                                          ggplot2::after_stat(task_and_minutes)))
