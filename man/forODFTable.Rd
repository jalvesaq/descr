\name{forODFTable}
\alias{forODFTable}

\title{Convert an object of class CrossTable into a matrix for odfTable}
\description{The function converts an object of class CrossTable into a matrix
  to be printed by \samp{odfTable()} of \pkg{odfWeave} package.}
\usage{
forODFTable(x, digits = 1, \dots) 
}
\arguments{
  \item{x}{A object of class \samp{CrossTable}.}
  \item{digits}{See \link[base]{round}.}
  \item{\dots}{Optional arguments passed to \link[base]{format}.}
}
\value{
  A matrix.
}
\author{Jakson A. Aquino \email{jalvesaq@gmail.com}.}

\seealso{\link{CrossTable}}

\examples{
\dontrun{
library(odfWeave)
data(infert, package = "datasets")
x <- crosstab(infert$education, infert$induced, expected = TRUE)

# Use the function directly:
odfTable(forODFTable(x))

# Create a method for odfTable:
odfTable.CrossTable <- function(x) odfTable(forODFTable(x))
odfTable(x)
methods(odfTable)
}
}
