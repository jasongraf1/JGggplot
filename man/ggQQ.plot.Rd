\name{ggQQ.plot}
\alias{ggQQ.plot}
\title{Quantile-Quantile Plot with ggplot2}
\usage{
ggQQ.plot(data, x)
}

\description{
Produces a normal QQ plot of the values in \code{x} using \code{ggplot2}. Adds a line to a "theoretical", by default normal, quantile-quantile plot which passes through the probs quantiles, by default the first and third quartiles.
}
\arguments{
\item{data}{ a dataframe }
\item{x}{ a character vector containing the name of the column in \code{data} containing numeric vector whose values will be plotted. }
}
\value{
A \code{\link{ggplot2}} object to which other layers can be added.
}
\seealso{
\code{\link{qqnorm}}, \code{\link{qqline}}, \code{\link{geom_point}}, \code{\link{geom_abline}}, \code{\link{stat_qq}}.
}

\author{Jason Grafmiller}
\examples{
d <- data.frame(
  x1 = rnorm(1000, 2),
  x2 = rchisq(1000, 2))

ggQQ.plot(d, "x1")

ggQQ.plot(d, "x2")

}