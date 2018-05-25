ggQQ.plot <- function (data, var, add.labels = F) {
  # function for qqnorm plot in ggplot
  # var is a numeric vector
  # following four lines from base R's qqline()
  require(ggplot2)
  data <- as.data.frame(data)
  #e.g. ggQQ.plot(mtcars, wt)
  if (is.character(var)){
    v <- var
  } else {
    v <- deparse(substitute(var))
  }
  y <- quantile(data[, v], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(data, aes_string(sample = v)) + stat_qq() +
    geom_abline(slope = slope, intercept = int)
  if (add.labels){
    df_new <- ggplot_build(p)$data[[1]]
    df_new$level <- data$level[order(data[, v])]
    p2 <- ggplot(df_new, aes(theoretical, sample, label = level)) +
      geom_abline(slope = slope, intercept = int, linetype = 2) +
      geom_text(size = 3.5, fontface = "italic")
    return (p2)
  } else return(p)
}
