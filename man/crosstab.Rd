\name{crosstab}
\alias{crosstab}
\title{Cross tabulation with mosaic plot}
\description{
  This function is a wrapper for \code{\link{CrossTable}}, adding a
  mosaic plot and making it easier to do a weighted cross-tabulation.
}
\usage{
crosstab(dep, indep, weight = NULL,
         digits = list(expected = 1, prop = 3, percent = 1, others = 3),
         max.width = NA,
         expected = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE,
         prop.chisq = FALSE, chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
         resid = FALSE, sresid = FALSE, asresid = FALSE,
         missing.include = FALSE, drop.levels = TRUE, format = "SPSS",
         cell.layout = TRUE, row.labels = !cell.layout,
         percent = (format == "SPSS" && !row.labels),
         total.r, total.c, dnn = NULL, xlab = NULL,
         ylab = NULL, main = "", user.missing.dep, user.missing.indep,
         plot = getOption("descr.plot"), \dots)
}

\arguments{
  \item{dep, indep}{Vectors in a matrix or a dataframe. \code{dep} should be
    the dependent variable, and \code{indep} should be the independent one.}
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
  \item{drop.levels}{See \code{\link{CrossTable}}.}
  \item{format}{See \code{\link{CrossTable}}.}
  \item{cell.layout}{See \code{\link{CrossTable}}.}
  \item{row.labels}{See \code{\link{CrossTable}}.}
  \item{percent}{See \code{\link{CrossTable}}.}
  \item{total.r}{See \code{\link{CrossTable}}.}
  \item{total.c}{See \code{\link{CrossTable}}.}
  \item{dnn}{See \code{\link{CrossTable}}.}
  \item{xlab}{See \code{\link[graphics]{plot.default}}.}
  \item{ylab}{See \code{\link[graphics]{plot.default}}.}
  \item{main}{An overall title for the plot (see
    \code{\link[graphics]{plot.default}} and \code{\link[graphics]{title}}).}
  \item{user.missing.dep}{An optional character vector with the levels of
    \code{dep} that should be treated as missing values.}
  \item{user.missing.indep}{An optional character vector with the levels of
    \code{indep} that should be treated as missing values.}
  \item{plot}{Logical: if \code{TRUE} (default), a mosaic plot is produced.
    You may put

    \code{options(descr.plot = FALSE)}

    in your \file{.Rprofile} to change the default function behavior.}
  \item{\dots}{Further arguments to be passed to
    \code{\link[graphics]{mosaicplot}}.}
}

\details{
  \code{crosstab} invokes the \code{\link{CrossTable}} with all boolean
  options set to \code{FALSE} and \code{"SPSS"} as the default \code{format}
  option. The returned CrossTable object can be plotted as a
  \code{\link[graphics]{mosaicplot}}. Note that the gray scale colors used by
  default in the mosaic plot do not have any statistical meaning. The colors
  are used only to ease the plot interpretation.

  Differently from \code{\link{CrossTable}}, this function requires both
  \code{dep} and \code{indep} arguments. If you want an univariate tabulation,
  you should try either \code{\link{CrossTable}} or \code{\link{freq}}.
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

\seealso{\code{\link{CrossTable}}, \code{\link{plot.CrossTable}},
  \code{\link{xtable.CrossTable}}.}

\examples{
educ <- sample(c(1, 2), 200, replace = TRUE, prob = c(0.3, 0.7))
educ <- factor(educ, levels = c(1, 2), labels = c("Low", "High"))
opinion <- sample(c(1, 2, 9), 200, replace = TRUE,
                 prob = c(0.4, 0.55, 0.05))
opinion <- factor(opinion, levels = c(1, 2, 9),
                 labels = c("Disagree", "Agree", "Don't know"))
attr(educ, "label") <- "Education level"
attr(opinion, "label") <- "Opinion"
weight <- sample(c(10, 15, 19), 200, replace = TRUE)

crosstab(opinion, educ, xlab = "Education", ylab = "Opinion")
ct <- crosstab(opinion, educ, weight,
               dnn = c("Opinion", "Education"),
               user.missing.dep = "Don't know",
               expected = TRUE, prop.c = TRUE, prop.r = TRUE,
               plot = FALSE)
ct
plot(ct, inv.y = TRUE)

# Get the table of observed values as an object of class "table"
tab <- ct$tab
class(tab)
tab

# Get the complete cross table as "matrix"
complete.tab <- descr:::CreateNewTab(ct)
class(complete.tab)
complete.tab

## xtable support
library(xtable)

# Print ugly table
print(xtable(ct))

# Print pretty table
# Add to the preamble of your Rnoweb document:
# \usepackage{booktabs}
# \usepackage{multirow}
# \usepackage{dcolumn}
# \newcolumntype{d}{D{.}{.}{-1}}
print(xtable(ct, align = "llddd", multirow = TRUE, hline = TRUE,
             row.labels = TRUE, percent = FALSE,
             caption = "Opinion according to level of education"),
      booktabs = TRUE, include.rownames = FALSE,
      sanitize.text.function = function(x) x)
}
