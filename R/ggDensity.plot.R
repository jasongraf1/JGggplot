
ggDensity.plot <- function(data, xvar, facet = NULL, as.hist = FALSE,
                           fill.col = NULL, line.col = "black",
                           scales = "fixed",
                           bin.width = NULL, add.line = T,
                           alpha.h = 1, alpha.d = .2,
                           fill.d = "blue"){
  # create plot representing density of numeric vector
  # shows a histogram with a density curve overlayed

  # e.g.: ggDensity.plot(data.frame(x = rnorm(1000)), x)

  require(ggplot2)
  data <- as.data.frame(data)
  #xvar <- deparse(substitute(x))
  if (is.character(xvar)) {
    xvar <- xvar
  } else {
    xvar <- deparse(substitute(xvar))
  }
  if (!is.numeric(data[, xvar])){
    stop("x variable must be numeric")
  }
  if (is.null(bin.width)){
    bin.width <- (max(data[, xvar]) - min(data[, xvar]))/40
  }
  if (is.null(fill.col)) {
    fill.col <- "lightgray" # set default fill color
  }
  if (line.col == F) {
    line.col <- fill.col # set default line color to match fill color
  }
  if (as.hist){
    p <- ggplot(data, aes_string(xvar)) +
      geom_histogram(binwidth = bin.width,
                     fill = fill.col, color = line.col,
                     alpha = alpha.h)
  } else {
    p <- ggplot(data, aes_string(xvar)) +
      geom_histogram(aes(y = ..density..), binwidth = bin.width,
                     fill = fill.col, color = line.col,
                     alpha = alpha.h)
    if (add.line & fill.d == F){
      p <- p + geom_density()
    } else if (add.line) {
      p <- p + geom_density(alpha = alpha.d, fill = fill.d)
    }}
  if (!is.null(facet)){
    if (length(facet) > 1){
      fvar1 <- facet[1]
      fvar2 <- facet[2]
      p <- p + facet_grid(as.formula(paste(fvar1, "~", fvar2)),
                          scales = scales)
    } else {
      fvar1 <- facet
      p <- p + facet_wrap(as.formula(paste("~", fvar1)),
                          scales = scales)
    }}
  return (p)
}
