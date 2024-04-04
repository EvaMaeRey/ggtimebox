CoordTransfixed <- ggplot2::ggproto(`_class` = "CoordTransfixed", 
                       `_inherit` = ggplot2::CoordTrans)

CoordTransfixed$aspect <- function (self, ranges){  # inner function of CoordFixed$aspect
    diff(ranges$y.range)/diff(ranges$x.range) * self$ratio
}

# borrowing from coord_trans set up and adding ratio argument
coord_transfixed <- function (x = "identity", y = "identity", xlim = NULL, ylim = NULL, 
    limx = lifecycle::deprecated(), limy = lifecycle::deprecated(), clip = "on", expand = TRUE, ratio = 1) 
{
    if (lifecycle::is_present(limx)) {
        deprecate_warn0("3.3.0", "coord_trans(limx)", "coord_trans(xlim)")
        xlim <- limx
    }
    if (lifecycle::is_present(limy)) {
        deprecate_warn0("3.3.0", "coord_trans(limy)", "coord_trans(ylim)")
        ylim <- limy
    }
    ggplot2:::check_coord_limits(xlim)
    ggplot2:::check_coord_limits(ylim)
    if (is.character(x)) 
        x <- scales::as.transform(x)
    if (is.character(y)) 
        y <- scales::as.transform(y)
    ggplot2::ggproto(NULL, CoordTransfixed, trans = list(x = x, y = y), 
                     limits = list(x = xlim, y = ylim), expand = expand, 
                     clip = clip, ratio = ratio)
}

coord_canvas <- function(ratio = 1, ...){
  
  coord_transfixed(y = "reverse", ratio = ratio, ...)

}

coord_timebox <- function(ratio = 1/60,  ...){
  
  coord_canvas(ratio = ratio, ...)
  
}
