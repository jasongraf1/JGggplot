ggViolin.plot <- function(data, x, y, facet = NULL, fill.col = NULL,
                          facet.cols = NULL, scales = "fixed", CI = .5){
  # create barplot of proportions with counts superimposed over plots
  # allows for faceting by 1 or 2 groups entered as character vector
  require(ggplot2)
  require(plyr)
  data <- as.data.frame(data)
  #e.g. ggViolin.plot(chickwts, feed, weight)

  if (class(x) == 'character') {
    xvar <- x
  } else {
    xvar <- deparse(substitute(x))
  }
  if (class(y) == 'character'){
    yvar <- y
  } else {
    yvar <- deparse(substitute(y))
  }
  if (!is.numeric(data[, yvar])){
    stop("y variable must be numeric")
  }
  if(is.null(fill.col)) {
    fill.col <- "gray"
  } else fill.col <- fill.col # for allowing aes colors (tbc...)

  if (!is.null(facet)){
    if (length(facet) > 1){
      fvar1 <- facet[1]
      fvar2 <- facet[2]
      p <- ggplot(data, aes_string(xvar, yvar)) +
        geom_violin(fill = fill.col) +
        stat_summary(fun.data = median_hilow,
                     geom = "pointrange",
                     fun.args = list(conf.int = CI)) +
        facet_grid(as.formula(paste(fvar1, "~", fvar2)),
                   scales = scales)
    } else {
      #fvar <- deparse(substitute(facet))
      fvar <- facet
      p <- ggplot(data, aes_string(xvar, yvar)) +
        geom_violin(fill = fill.col) +
        stat_summary(fun.data = median_hilow,
                     geom = "pointrange",
                     fun.args = list(conf.int = CI)) +
        facet_wrap(as.formula(paste("~", fvar)),
                   ncol = facet.cols, scales = scales)
    }} else {
    p <- ggplot(data, aes_string(xvar, yvar)) +
      geom_violin(fill = fill.col) +
      stat_summary(fun.data = median_hilow,
                   geom = "pointrange",
                   fun.args = list(conf.int = CI))
  }
  return(p)
}
