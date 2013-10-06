\name{data.frame2txt}
\alias{data.frame2txt}
\title{Export a data.frame and create scripts to input the data again.}
\description{
  Export a data.frame to a tab delimited text and create R and SPSS/PSPP
  scripts to input the data again.
}
\usage{
data.frame2txt(x, datafile = "x.txt", r.codefile = "x.R",
               sps.codefile = "x.sps", df.name = deparse(substitute(x)),
               user.missing)
}
\arguments{
  \item{x}{The data.frame to be exported.}
  \item{datafile}{The name of the tab delimited file to be created.}
  \item{r.codefile}{The name of the R script to read the data file.}
  \item{sps.codefile}{The name of the SPSS/PSPP script to read the data file.}
  \item{df.name}{The name of the data.frame object to be created by the R
    script.}
  \item{user.missing}{Labels of levels that must be coded as user missing
    in the sps script.}
}

\value{
  The return value of \code{\link[utils]{write.table}}.
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

\examples{
data(CO2)
data.frame2txt(CO2)
}

