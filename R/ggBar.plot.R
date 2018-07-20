ggBar.plot <- function(data, x, y, facet = NULL, percent = T,
                       facet.cols = NULL, scales = "fixed",
                       width = 0.7, size = 5, opp.cols = FALSE,
                       horiz = F){
  require(ggplot2)
  require(plyr)
  data <- as.data.frame(data)
  xvar <- deparse(substitute(x))
  yvar <- deparse(substitute(y))
  xlevs <- length(levels(data[, xvar]))
  ylevs <- length(levels(data[, yvar]))
  if(opp.cols) {
    ycols <- c("black", rep("white", ylevs - 1))
  } else ycols <- rep("black", ylevs)

  if (!is.null(facet)){
    if (length(facet) > 1){
      fvar1 <- facet[1]
      fvar2 <- facet[2]
      f1levs <- length(levels(data[, fvar1]))
      f2levs <- length(levels(data[, fvar2]))
      levs <- f1levs * f2levs * xlevs
      mydata <- data[,c(xvar, fvar1, fvar2, yvar)]
      mytable <- ftable(mydata)
      d <- as.data.frame(mytable)
      d$Prop <- as.data.frame(prop.table(mytable, 1))[, ncol(d)]
      d <- ddply(d, c(xvar, fvar1, fvar2), transform,
                 pos = 1 - cumsum(Prop) + (0.5 * Prop))
      # make plot
      p <- ggplot(d, aes_(substitute(x), ~Prop)) +
        geom_bar(aes_(fill = substitute(y)),
                 stat = "identity", width = .7, color = "black") +
        geom_text(aes(label = Freq, y = pos),
                  color = rep(ycols, xlevs), size = size) +
        facet_grid(as.formula(paste(fvar1, "~", fvar2)),
                   scales = scales)
    } else {
      #fvar <- deparse(substitute(facet))
      fvar <- facet
      flevs <- length(levels(data[, fvar]))
      mydata <- data[,c(xvar, fvar, yvar)]
      mytable <- ftable(mydata)
      d <- as.data.frame(mytable)
      d$Prop <- as.data.frame(prop.table(mytable, 1))[, ncol(d)]
      d <- ddply(d, c(xvar, fvar),
                 transform, pos = 1 - cumsum(Prop) + (0.5 * Prop))
      p <- ggplot(d, aes_(substitute(x), ~Prop)) +
        geom_bar(aes_(fill = substitute(y)),
                 stat = "identity", width = width, col = "black") +
        geom_text(aes(label = Freq, y = pos),
                  col = rep(ycols, xlevs*flevs), size = size) +
        facet_wrap(as.formula(paste("~", fvar)),
                   ncol = facet.cols, scales = scales)}
  } else {
    mydata <- data[, c(xvar, yvar)]
    mytable <- table(mydata)
    d <- as.data.frame(mytable)
    d$Prop <- as.data.frame(prop.table(mytable, 1))[, 3]
    d <- ddply(d, xvar, transform, pos = 1 - cumsum(Prop) + (0.5 * Prop))
    p <- ggplot(d, aes_(substitute(x), ~Prop)) +
      geom_bar(aes_(fill = substitute(y)),
               stat = "identity", width = .7, color = "black") +
      geom_text(aes(label = Freq, y = pos),
                color = rep(ycols, xlevs), size = size)}

  if (percent){
    p <- p + scale_y_continuous(breaks = seq(0, 1, .25),
                                labels = paste("%", seq(0,100,25), sep = "")) +
      labs(x = "", y = "percentage of tokens")
  } else p <- p + labs(x = "", y = "proportion of tokens")

  if (horiz) p <- p + coord_flip()

  return(p)
}
