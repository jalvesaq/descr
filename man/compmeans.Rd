\name{compmeans}
\alias{compmeans}
\title{Means of a numerical vector according to a factor}
\description{Calculates the means of a numerical vector according to a factor.}
\usage{
compmeans(x, f, w, sort = FALSE, maxlevels = 60,
          user.missing, plot = getOption("descr.plot"),
          relative.widths = TRUE, col = "lightgray",
          warn = getOption("descr.warn"), \dots)
}
\arguments{
  \item{x}{A numeric vector.}
  \item{f}{A factor.}
  \item{w}{Optional vector with weights.}
  \item{sort}{If \code{TRUE}, sorts the lines by the means values.}
  \item{maxlevels}{Maximum number of levels that \code{x} converted into factor
    should have.}
  \item{user.missing}{Character vector, indicating what levels of \code{f}
    must be treated as missing values.}
  \item{plot}{Logical: if \code{TRUE} (default), a boxplot is produced.
    You may put

    \code{options(descr.plot = FALSE)}

    in your \file{.Rprofile} to change the default function behavior.}
  \item{relative.widths}{If \code{TRUE}, the boxes widths will be proportional
    to the number of elements in each level of \code{f}.}
  \item{col}{Vector with the boxes colors.}
  \item{warn}{Warn if conversion from factor into numeric or from numeric into
    factor was performed and if missing values were dropped (default: \code{TRUE}).}
  \item{\dots}{Further arguments to be passed to either
    \code{\link[graphics]{boxplot}} (if \code{w} is missing) or
    \code{\link[graphics]{bxp}} (for \code{w} weighted boxplot).}
}
\value{
  A matrix with class \code{c("matrix", "meanscomp")} with labels attributes
  for \code{x} and \code{f}. The returned object can be plotted, generating
  a \code{\link[graphics]{boxplot}} of \code{x} grouped by \code{f}.
}
\author{Jakson A. Aquino \email{jalvesaq@gmail.com}, with code for weighted
  boxplots written by Stefan Kraft for simPopulation package.}

\seealso{\code{\link[graphics]{boxplot}}.}

\examples{
sex <- factor(c(rep("F", 900), rep("M", 900)))
income <- 100 * (rnorm(1800) + 5)
weight <- rep(1, 1800)
weight[sex == "F" & income > 500] <- 3
attr(income, "label") <- "Income"
attr(sex, "label") <- "Sex"
compmeans(income, sex, col = "lightgray", ylab = "income", xlab = "sex")
comp <- compmeans(income, sex, weight, plot = FALSE)
plot(comp, col = c("pink", "lightblue"), ylab = "income", xlab = "sex")

\dontrun{
library(xtable)
# If the decimal separator in your country is a comma:
# options(OutDec = ",")
print(xtable(comp, caption = "Income according to sex", label = "tab:incsx"))
}
}
