\name{crosstab}
\alias{crosstab}
\title{Cross tabulation with mosaic plot}
\description{
  This function is a wrapper for \code{\link{CrossTable}}, adding a
  mosaic plot and making it easier to do a weighted cross-tabulation.
}
\usage{
crosstab(x, y, weight = NULL, digits = 3, max.width = NA,
         expected = FALSE, prop.r = FALSE, prop.c = FALSE,
         prop.t = FALSE, prop.chisq = FALSE, chisq = FALSE,
         fisher = FALSE, mcnemar = FALSE, resid = FALSE,
         sresid = FALSE, asresid = FALSE,
         missing.include = FALSE, format = "SPSS",
         dnn = NULL, user.missing.x, user.missing.y,
         cell.layout = TRUE, plot = getOption("descr.plot"),
         xlab = deparse(substitute(x)),
         ylab = deparse(substitute(y)), main = "", \dots) 
}

\arguments{
  \item{x, y}{Vectors in a matrix or a dataframe.}
  \item{weight}{An optional vector for a weighted cross tabulation.}
  \item{digits}{See \code{\link{CrossTable}}.}
  \item{max.width}{See \code{\link{CrossTable}}.}
  \item{expected}{See \code{\link{CrossTable}}.}
  \item{prop.r}{See \code{\link{CrossTable}}.}
  \item{prop.c}{See \code{\link{CrossTable}}.}
  \item{prop.t}{See \code{\link{CrossTable}}.}
  \item{prop.chisq}{See \code{\link{CrossTable}}.}
  \item{chisq}{See \code{\link{CrossTable}}.}
  \item{fisher}{See \code{\link{CrossTable}}.}
  \item{mcnemar}{See \code{\link{CrossTable}}.}
  \item{resid}{See \code{\link{CrossTable}}.}
  \item{sresid}{See \code{\link{CrossTable}}.}
  \item{asresid}{See \code{\link{CrossTable}}.}
  \item{missing.include}{See \code{\link{CrossTable}}.}
  \item{format}{See \code{\link{CrossTable}}.}
  \item{dnn}{See \code{\link{CrossTable}}.}
  \item{cell.layout}{See \code{\link{CrossTable}}.}
  \item{user.missing.x}{An optional character vector with the levels of
    \code{x} that should be treated as missing values.}
  \item{user.missing.y}{An optional character vector with the levels of
    \code{y} that should be treated as missing values.}
  \item{plot}{Logical: if \code{TRUE} (default), a mosaic plot is produced.
    You may put
    
    \code{options(descr.plot = FALSE)}
    
    in your \file{.Rprofile} to change the default function behavior.}
  \item{main}{An overall title for the plot (see
    \code{\link[graphics]{title}}).}
  \item{xlab}{A title for the x axis (see \code{\link[graphics]{title}}).}
  \item{ylab}{A title for the y axis (see \code{\link[graphics]{title}}).}
  \item{\dots}{Further arguments to be passed to
    \code{\link[graphics]{mosaicplot}}.}
}

\details{
  \code{crosstab} invokes the \code{\link{CrossTable}} with all boolean
  options set to \code{FALSE} and \code{"SPSS"} as the default \code{format}
  option. The returned CrossTable object can be plotted as a
  \code{\link[graphics]{mosaicplot}}.
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

\seealso{\code{\link{CrossTable}}, \code{\link{plot.CrossTable}},
  \code{\link{xtable.CrossTable}}.}

\examples{
educ <- sample(c(1, 2), 200, replace = TRUE, prob = c(0.3, 0.7))
educ <- factor(educ, levels = c(1, 2), labels = c("Low", "High"))
income <- sample(c(1, 2, 3), 200, replace = TRUE,
                 prob = c(0.3, 0.4, 0.3))
income <- factor(income, levels = c(1, 2, 3),
                 labels = c("Low", "Middle", "High"))
attr(educ, "label") <- "Education level"
attr(income, "label") <- "Income level"
w <- sample(c(10, 15, 19), 200, replace = TRUE)

crosstab(income, educ, ylab = "Education", xlab = "Income")
ct <- crosstab(income, educ, w, expected = TRUE, plot = FALSE)
ct
plot(ct, inv.y = TRUE)
\dontrun{
library(xtable)
print(xtable(ct))
print(xtable(ct, decimal.mark = ",", multirow = TRUE, hline = TRUE),
      sanitize.text.function = function(x) x, include.rownames = FALSE)
}
}
