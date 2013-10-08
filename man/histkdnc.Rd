\name{histkdnc}
\alias{histkdnc}
\title{Histogram with kernel density and normal curve}
\description{
  Plots a histogram with kernel density and normal curve.
}
\usage{
histkdnc(v, breaks = 0, include.lowest = TRUE, right = TRUE,
         main = "Histogram with kernel density and normal curve",
         xlab = deparse(substitute(v)), col = grey(0.90),
         col.cur = c("red", "blue"), lty.cur = c(1, 1),
         xlim = NULL, ylim = NULL, ...) 
}
\arguments{
  \item{v}{The object from which the histogram is desired.}
  \item{breaks}{See \link[graphics]{hist}.}
  \item{include.lowest}{See \link[graphics]{hist}.}
  \item{right}{See \link[graphics]{hist}.}
  \item{main}{See \link[graphics]{hist}.}
  \item{xlab}{See \link[graphics]{hist}.}
  \item{col}{See \link[graphics]{hist}.}
  \item{col.cur}{Vector of size two with the colors of, respectively, kernel
    density and normal curve.}
  \item{lty.cur}{Vector of size two with line type of, respectively, kernel
    density and normal curve.}
  \item{xlim}{See \link[graphics]{plot.default} and \link[graphics]{hist}.}
  \item{ylim}{See \link[graphics]{plot.default} and \link[graphics]{hist}.}
  \item{\dots}{Further arguments to be passed to \link[graphics]{hist}.}
}


\details{
  The function plots a histogram of the object \code{x} with its kernel density
  and a normal curve with the same mean and standard deviation of \code{x}.
}
\value{
  NULL.
}
\author{Dirk Enzmann (modified by Jakson Aquino\email{jalvesaq@gmail.com}).}

