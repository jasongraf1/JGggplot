
ggMosaic.plot <- function(data, x, y, myscale = 1, ...){
  # 2 dimensional mosaic plot for ggplot2
  # still needs some work....
  data <- as.data.frame(data)

  xvar <- deparse(substitute(x))
  yvar <- deparse(substitute(y))
  xlevs <- length(levels(data[, xvar]))
  ylevs <- length(levels(data[, yvar]))
  mydata <- data[c(xvar, yvar)];
  mytable <- table(mydata);
  widths <- c(0, cumsum(apply(mytable, 1, sum)));
  heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});

  alldata <- data.frame();
  allnames <- data.frame();
  for(i in 1:nrow(mytable)){
    for(j in 1:ncol(mytable)){
      alldata <- rbind(alldata, c(widths[i], widths[i+1],
                                  heights[j, i], heights[j+1, i]));
    }}
  colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")
  # add space between rectangles
  # on horizontal axis, add space scaled to the size of the dataset
  # (if N < 50, set to default 1)
  if(nrow(data) > 50){ myscale <- round(nrow(data)/50, 0) }
  else {myscale <- myscale}

  alldata$xmin <- alldata$xmin +
    rep(seq(0, (nrow(mytable) - 1)* myscale,
            by = myscale), each = ncol(mytable))

  alldata$xmax <- alldata$xmax +
    rep(seq(0, (nrow(mytable) - 1)*myscale,
            by = myscale), each = ncol(mytable))

  # on vertical axis
  yadj <- rep(0.03*0:(ncol(mytable) - 1), nrow(mytable))
  for(i in 1:nrow(alldata)){
    alldata[i, c("ymin", "ymax")] <- alldata[i, c("ymin", "ymax")] + yadj[i]
  }
  # add variable names
  alldata[[xvar]] <- factor(rep(dimnames(mytable)[[1]],
                                rep(ncol(mytable), nrow(mytable))))

  alldata[[yvar]] <- factor(rep(dimnames(mytable)[[2]], nrow(mytable)))

  # add chi-squared residuals
  alldata$residuals <- as.data.frame(chisq.test(t(mytable))$residuals)[, "Freq"]

  # add columns for plotting the variable labels
  alldata$xvarCenter <- alldata$xmin + (alldata$xmax-alldata$xmin)/2
  ymargins <- prop.table(table(data[, yvar]))
  #alldata$yvarCenter <- c(0, cumsum(ymargins)[1:length(levels(data[, yvar])) - 1]) + ymargins/2
  alldata$yvarCenter <- alldata[1:ylevs, "ymin"] + (alldata[1:ylevs, "ymax"] - alldata[1:ylevs, "ymin"])/2

  # for labels
  xlabs <- seq(0, nrow(alldata) - 1, by = nrow(alldata)/xlevs) + 1
  ylabs <- seq(0, nrow(alldata) - 1, by = nrow(alldata)/ylevs) + 1

  # plot
  p <- ggplot(alldata, aes(xmin = xmin, xmax = xmax,
                           ymin = ymin, ymax = ymax)) +
    ylim(0, 0.1 + max(alldata$ymax)) +
    geom_rect(color = "black", aes(fill = residuals)) +
    geom_text(data = alldata[xlabs, ],
              aes_string(label = xvar,
                         x = "xvarCenter"), y = .05 + max(alldata$ymax)) +
    geom_text(data = alldata[ylabs, ],
              aes_string(label = as.character(yvar),
                         y = "yvarCenter"), x = - 0, hjust = 1.5) +
    labs(x = "", y = "") + # no axis labels
    scale_fill_gradient2(name = "Standardized\nresiduals",
                         limits = range(alldata$residuals)) +
    #xlim(- max(alldata$xmax)/5, max(alldata$xmax)) +
    theme(panel.grid = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank())

  return(p)
}
