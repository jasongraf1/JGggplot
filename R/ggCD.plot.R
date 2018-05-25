ggCD.plot <- function(fmla, data, add.label = TRUE,
                      ylevs = 2:1, text.col = "black"){
  require(ggplot2)
  if (!plyr::is.formula(fmla)){
    stop("First argument must be a formula...")
  }
  data <- as.data.frame(data)
  cdens <- cdplot(fmla, data = data, ylevel = ylevs, plot = F)
  vars <- all.vars(fmla)
  x <- data[, vars[2]]
  levs <- levels(data[, vars[1]])[ylevs] # order the levels correctly
  d <- data.frame(x = rep(x, 2),
                  y = c(cdens[[1]](x), rep(1, length(x))),
                  type = rep(levs, each = nrow(data))
  )
  p <- ggplot(d, aes(x, y)) +
    geom_area(aes(fill = type), position = "identity") +
    labs(y = "Proportion of response", x = vars[2]) +
    guides(fill = guide_legend(title = vars[1]))
  if (add.label){
    p <- p + annotate("text", label = levs,
                      x = rep(min(x), 2) - (max(x) - min(x))/50,
                      y = c(0, 1),
                      angle = 90, hjust = c(-.1, 1.1),
                      col = text.col)
  }
  return(p)
}
