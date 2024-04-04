stamp_workday <- function(...){
  annotate(geom = "rect", 
           ymin = 0*60, ymax = -8*60, 
           xmin = -.45, xmax = .45, ... )
}
