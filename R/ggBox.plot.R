
ggBox.plot <- function(data, x, y, facet = NULL, fill.col = NULL,
                       facet.cols = NULL, notch = TRUE, scales = "fixed",
                       width = 0.7){
  # create barplot of proportions with counts superimposed over plots
  # allows for faceting by 1 or 2 groups entered as character vector

  # e.g. ggBox.plot(chickwts, feed, weight, notch = F)
  require(ggplot2)
  data <- as.data.frame(data)
  if (is.character(x)) {
    xvar <- x
  } else {
    xvar <- deparse(substitute(x))
  }

  if (is.character(y)) {
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
        geom_boxplot(fill = fill.col, notch = notch,
                     width = width) +
        facet_grid(as.formula(paste(fvar1, "~", fvar2)),
                   scales = scales)
    } else {
      #fvar <- deparse(substitute(facet))
      fvar <- facet
      p <- ggplot(data, aes_string(xvar, yvar)) +
        geom_boxplot(fill = fill.col, notch = notch,
                     width = width) +
        facet_wrap(as.formula(paste("~", fvar)),
                   ncol = facet.cols, scales = scales)
    }} else {
    p <- ggplot(data, aes_string(xvar, yvar)) +
      geom_boxplot(fill = fill.col, notch = notch,
                   width = width)
  }
  return(p)
}
