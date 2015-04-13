\name{plot.CrossTable}
\alias{plot.CrossTable}
\title{Mosaic plot from object of class CrossTable}
\description{
  This function receives a \code{\link{CrossTable}} object as its main
  argument and produces a mosaicplot.
}
\usage{
\method{plot}{CrossTable}(x, xlab, ylab, main = "", col,
           inv.x = FALSE, inv.y = FALSE, \dots)
}

\arguments{
  \item{x}{A object of class CrossTable.}
  \item{xlab}{See \code{\link[graphics]{plot.default}}.}
  \item{ylab}{See \code{\link[graphics]{plot.default}}.}
  \item{main}{See \code{\link[graphics]{plot.default}} and \code{\link[graphics]{title}}.}
  \item{col}{A specification for the default plotting color. (See section
    \sQuote{Color Specification} of \code{\link[graphics]{par}}). If the
    argument is missing, a gray scale is used to make the plot easier to
    interpret.}
  \item{inv.x}{A logical value indicating whether the order of the levels of
    the \code{x} variable should be inverted.}
  \item{inv.y}{A logical value indicating whether the order of the levels of
    the \code{y} variable should be inverted.}
  \item{\dots}{Further arguments to be passed to
    \code{\link[graphics]{mosaicplot}}.}
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

\seealso{\code{\link{CrossTable}}, \code{\link{crosstab}}.}

