\name{CrossTable}
\alias{CrossTable}
\title{Cross tabulation with tests for factor independence}
\description{
  An implementation of a cross-tabulation function with output
  similar to S-Plus crosstabs() and SAS Proc Freq (or SPSS format)
  with Chi-square, Fisher and McNemar tests of the independence
  of all table factors.
}
\usage{
CrossTable(x, y,
           digits = list(expected = 1, prop = 3, percent = 1, others = 3),
           max.width = NA, expected = FALSE,
           prop.r = TRUE, prop.c = TRUE, prop.t = TRUE,
           prop.chisq = TRUE, chisq = FALSE, fisher = FALSE,
           mcnemar = FALSE, resid = FALSE, sresid = FALSE,
           asresid = FALSE, missing.include = FALSE,
           drop.levels = TRUE, format = c("SAS","SPSS"),
           dnn = NULL, cell.layout = TRUE,
           row.labels = !cell.layout,
           percent = (format == "SPSS" && !row.labels),
           total.r, total.c, xlab = NULL, ylab = NULL, \dots)
}
\arguments{
  \item{x}{A vector or a matrix. If y is specified, x must be a vector.}
  \item{y}{A vector in a matrix or a dataframe.}
  \item{digits}{Named list with number of digits after the decimal point for
    four categories of statistics: expected values, cell proportions,
    percentage and others statistics. It can also be a numeric vector with a
    single number if you want the same number of digits in all statistics.}
  \item{max.width}{In the case of a 1 x n table, the default will be
    to print the output horizontally. If the number of columns exceeds
    max.width, the table will be wrapped for each successive increment
    of max.width columns. If you want a single column vertical table,
    set max.width to 1.}
  \item{prop.r}{If \code{TRUE}, row proportions will be included.}
  \item{prop.c}{If \code{TRUE}, column proportions will be included.}
  \item{prop.t}{If \code{TRUE}, table proportions will be included.}
  \item{expected}{If \code{TRUE}, expected cell counts from the
    \eqn{\chi^2}{Chi-Square} will be included.}
  \item{prop.chisq}{If \code{TRUE}, chi-square contribution of each cell will be
    included.}
  \item{chisq}{If \code{TRUE}, the results of a chi-square test will be
    printed after the table.}
  \item{fisher}{If \code{TRUE}, the results of a Fisher Exact test will
    be printed after the table}
  \item{mcnemar}{If \code{TRUE}, the results of a McNemar test will be printed
  after the table.}
  \item{resid}{If \code{TRUE}, residual (Pearson) will be included.}
  \item{sresid}{If \code{TRUE},  standardized residual will be included.}
  \item{asresid}{If \code{TRUE}, adjusted standardized residual will be
    included.}
  \item{missing.include}{If \code{TRUE}, then NA values, if present, are
    included as level \code{"NA"} of both x and y. You can change the new
    level label by setting the value of descr.na.replacement option. Example:
    \code{options(descr.na.replacement = "Missing")}.}
  \item{drop.levels}{If \code{TRUE}, then remove any unused factor levels.}
  \item{format}{Either SAS (default) or SPSS, depending on the type of
    output desired.}
  \item{dnn}{The names to be given to the dimensions in the result (the dimnames
    names).}
  \item{cell.layout}{If \code{TRUE}, print the cell layout.}
  \item{row.labels}{If \code{TRUE}, add labels to rows of calculated
    statistics.}
  \item{percent}{A logical value indicating whether to add the percentage
    symbol \samp{prop.r}, \samp{prop.c} and \samp{prop.t} if \samp{format} is
    \samp{"SPSS"}.}.
  \item{total.r}{If \code{TRUE}, print row totals.}
  \item{total.c}{If \code{TRUE}, print column totals.}
  \item{xlab}{A title for the x axis when plotting the CrossTable object (see
    \code{\link[graphics]{title}}). If missing, \code{dnn[1]} is used if not
    \code{NULL}.}
  \item{ylab}{A title for the y axis when plotting the CrossTable object (see
    \code{\link[graphics]{title}}). If missing, \code{dnn[2]} is used if not
    \code{NULL}.}
  \item{\dots}{Optional arguments passed to \code{\link[stats]{chisq.test}}.}
}
\details{
  A summary table will be generated with cell row, column and table
  proportions and marginal totals and proportions. Expected cell counts can be
  printed if desired. In the case of a 2 x 2 table, both corrected and
  uncorrected values will be included for appropriate tests. In the case of
  tabulating a single vector, cell counts and table proportions will be
  printed.

  Note 1: If 'x' is a vector and 'y' is not specified, no statistical tests
  will be performed, even if any are set to \code{TRUE}.

  Note 2: 'x' and 'y' labels will be truncated if the table is not going to
  fit to the screen, according to the value of \code{getOption("width")}.


  If both arguments \samp{total.c} and \samp{total.r} are missing, both will
  be \code{TRUE}. If only one of them is missing, the other will have the same
  value of the not missing one.
}
\value{
  A list of class \code{CrossTable} containing parameters used by the
  \code{print.CrossTable} method and the following components:

  tab:          An n by m matrix containing table cell counts.

  prop.row:     An n by m matrix containing cell row proportions.

  prop.col:     An n by m matrix containing cell column proportions.

  prop.tbl:     An n by m matrix containing cell table proportions.

  chisq:        Results from the Chi-Square test. A list with class 'htest'.
  See \code{\link[stats]{chisq.test}} for details.

  chisq.corr:   Results from the corrected Chi-Square test. A list with class
  'htest'. See \code{\link[stats]{chisq.test}} for details. ONLY included in
  the case of a 2 x 2 table.

  fisher.ts:    Results from the two-sided Fisher Exact test. A list with
  class 'htest'. See \code{\link[stats]{fisher.test}} for details. ONLY
  included if 'fisher' = TRUE.

  fisher.lt:    Results from the Fisher Exact test with HA = "less". A list
  with class 'htest'. See \code{\link[stats]{fisher.test}} for details. ONLY
  included if 'fisher' = TRUE and in the case of a 2 x 2 table.

  fisher.gt:    Results from the Fisher Exact test with HA = "greater". A list
  with class 'htest'. See \code{\link[stats]{fisher.test}} for details. ONLY
  included if 'fisher' = TRUE and in the case of a 2 x 2 table.

  mcnemar:      Results from the McNemar test. A list with class 'htest'. See
  \code{\link[stats]{mcnemar.test}} for details. ONLY included if 'mcnemar' =
  TRUE.

  mcnemar.corr: Results from the corrected McNemar test. A list with class
  'htest'. See \code{\link[stats]{mcnemar.test}} for details. ONLY included if
  'mcnemar' = TRUE and in the case of a 2 x 2 table.

  resid/sresid/asresid: Pearson Residuals (from chi-square tests).

}
\author{Jakson Aquino \email{jalvesaq@gmail.com} has splited the function
  \code{CrossTable} (from the package \code{gmodels}) in two:
  \code{CrossTable} and \code{print.CrossTable}. The \code{gmodels}'s function
  was developed by Marc Schwartz (original version posted to r-devel on Jul
  27, 2002. SPSS format modifications added by Nitin Jain based upon code
  provided by Dirk Enzmann).}

\seealso{\code{\link{crosstab}} (a wrapper to \samp{CrossTable} that makes it
  easier to do a weighted contingency table), \code{\link{plot.CrossTable}},
  \code{\link{forODFTable}}, \code{\link[base]{table}},
  \code{\link[base]{prop.table}}, \code{\link[stats]{xtabs}}.}

\examples{
# Simple cross tabulation of education versus prior induced
# abortions using infertility data
data(warpbreaks, package = "datasets")
ct <- CrossTable(warpbreaks$wool, warpbreaks$tension,
                 dnn = c("Wool", "Tension"))
data(esoph, package = "datasets")
ct <- CrossTable(esoph$alcgp, esoph$agegp, expected = TRUE,
                 chisq = FALSE, prop.chisq = FALSE,
                 dnn = c("Alcohol consumption", "Tobacco consumption"))
plot(ct, inv.y = TRUE)
print(ct)

# While printing the object, you can replace some (but not all)
# arguments previously passed to CrossTable
print(ct, format = "SPSS", cell.layout = FALSE, row.labels = TRUE)

# For better examples, including the use of xtable,
# see the documentation of crosstab().
}
