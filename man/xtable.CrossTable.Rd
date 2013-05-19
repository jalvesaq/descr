\name{xtable.CrossTable}
\alias{xtable.CrossTable}
\title{CrossTable method for xtable}
\description{
  The method creates an object of class xtable.
}
\usage{
\method{xtable}{CrossTable}(x, caption = NULL, label = NULL, align = NULL,
       digits = 1, display = NULL, multirow = FALSE, hline = FALSE, \dots)
}

\arguments{
  \item{x}{A object of class CrossTable.}
  \item{caption}{See \code{\link[xtable]{xtable}}.}
  \item{label}{See \code{\link[xtable]{xtable}}.}
  \item{align}{See \code{\link[xtable]{xtable}}.}
  \item{display}{See \code{\link[xtable]{xtable}}.}
  \item{digits}{See \code{\link[base]{round}}.}
  \item{multirow}{A logical value indicating whether the command
    \verb{\multirow} should be added to the table. See the Details section
    below.}
  \item{hline}{A logical value indicating whether the command
    \verb{\hline} should be added to the table. See the Details section
    below.}
  \item{\dots}{Further arguments to be passed to \code{\link[base]{format}}.}
}

\details{
  If either \code{multirow} or \code{hline} is \code{TRUE}, the
  \code{sanitize.text.function} argument of \code{\link[xtable]{print.xtable}}
  must be defined. You will also have to add \verb{\usepackage\{multirow\}} to
  your Rnoweb document.  See the \code{Example} section of
  \code{\link{crosstab}}.
}
\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

\seealso{\code{\link{CrossTable}}, \code{\link{crosstab}},
  \code{\link[xtable]{print.xtable}}.}

