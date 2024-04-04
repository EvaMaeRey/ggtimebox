stamp_currenttime <- function(..., color = "magenta", alpha = .5, start_hour = 9){
  
  current_time <- (Sys.time() |> hms::as_hms() |> as.numeric()) / 60 - start_hour*60
  
  ggplot2::geom_hline(yintercept = current_time, color = color, alpha = alpha, ...)
  
  }
