\name{freq}
\alias{freq}
\title{Frequency table}
\description{
  Prints a frequency table of the selected object. Optionally, the frequency
  might be weighted.
}
\usage{
freq(x, w, user.missing, plot = getOption("descr.plot"), \dots)
}
\arguments{
  \item{x}{The factor from which the frequency of values is desired.}
  \item{w}{An optional vector for a weighted frequency table.}
  \item{user.missing}{Character vector, indicating what levels must be
    treated as missing values while calculating valid percents. Levels
    representing user missing values are not shown in the
    \code{\link[graphics]{barplot}}.}
  \item{plot}{Logical: if \code{TRUE} (default), a barplot is produced.
    You may put
   
    \code{options(descr.plot = FALSE)}
   
    in your \file{.Rprofile} to change the default function behavior.}
  \item{\dots}{Further arguments to be passed to \code{\link{plot.freqtable}}
    if \code{plot = TRUE}.}
}
\details{
  A column with cumulative percents are added to the frequency table if \code{x}
  is an \code{ordered factor}.
}
\value{
  A matrix with class \code{c("matrix", "freqtable")} with the attribute
  \code{"xlab"} which is a character string corresponding to either the
  attribute "label" of \code{x} or, if \code{x} does not have this attribute,
  the name of \code{x}. The returned object can be plotted, generating a
  \code{\link[graphics]{barplot}}.
}
\author{Jakson A. Aquino \email{jalvesaq@gmail.com}, based on function written by Dirk Enzmann}

\examples{
x <- c(rep(1, 100), rep(2, 120), rep(3, 10), rep(NA, 12))
w <- c(rep(1.1, 122), rep(0.9, 120))
x <- factor(x, levels = c(1, 2, 3),
            labels = c("No", "Yes", "No answer"))
attr(x, "label") <- "Do you agree?"

freq(x, y.axis = "percent")
f <- freq(x, w, user.missing = "No answer", plot = FALSE)
f
plot(f)

# If the decimal separator in your country is a comma:
# options(OutDec = ",")
library(xtable)
print(xtable(f))
}
