grid.share.legend <- function(...,
                              ncol = length(list(...)),
                              nrow = 1,
                              position = c("bottom", "right")) {
  # See: https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  require(grid)
  require(gridExtra)
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
    "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
      legend, ncol = 1, heights = unit.c(unit(1, "npc") - lheight, lheight)),
    "right" = arrangeGrob(do.call(arrangeGrob, gl),
      legend, ncol = 2, widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  # grid.newpage()
  grid.draw(combined)
}
