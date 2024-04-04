stat_timebox <- function(mapping = NULL, 
                         data = NULL,
                         geom = GeomRect,
                         position = "identity", 
                         na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTimebox,        # proto object from step 2
    geom = geom,   # inherit other behavior
    data = data, 
    mapping = mapping,
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
