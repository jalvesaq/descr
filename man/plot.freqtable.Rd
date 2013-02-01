\name{plot.freqtable}
\alias{plot.freqtable}
\title{Bar plot from object of class freqtable}
\description{
  This function receives a \code{freqtable} object as its main
  argument and produces a barplot.
}
\usage{
\method{plot}{freqtable}(x, y.axis = "count", \dots)
}

\arguments{
  \item{x}{A object of class \code{freqtable}.}
  \item{y.axis}{Character string, indicating what variable to use in the y
    axis, "count" or "percent", when plotting the frequency table.}
  \item{\dots}{Further arguments to be passed to \code{\link[graphics]{barplot}}.}
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

