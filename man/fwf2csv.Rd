\name{fwf2csv}
\alias{fwf2csv}
\title{Fast conversion of a fwf file into a csv one}
\description{
  Convert fixed width formated file into a tab separated one.
}
\usage{
fwf2csv(fwffile, csvfile, names, begin, end,
        verbose = getOption("verbose"))
}
\arguments{
  \item{fwffile}{The fixed width format file.}
  \item{csvfile}{The csv file to be created. The fields will be separated by tab
    characters and there will be no quotes around strings.}
  \item{names}{A character vector with column names.}
  \item{begin}{A numeric vector with the begin offset of values in the fixed
    width format file.}
  \item{end}{A numeric vector with the end offset of values in the fixed width
    format file.}
  \item{verbose}{Logical: if \code{TRUE} a message about the number of saved
    lines is printed.}
}

\value{
  NULL.
}

\details{
  The return value is NULL, but \code{cvsfile} is created if the function is
  successful. The file is a text table with fields separated by tabular
  characters without quotes around the strings. This function is useful if you
  have a very big fixed width formated file to read and \link[utils]{read.fwf}
  would be too slow.
}

\examples{
\dontrun{
tab <- rbind(c("state",   1,  2),
             c("municp",  3,  5),
             c("house",   6,  8),
             c("cond",    9,  9),
             c("sex",    10, 10),
             c("age",    11, 12),
             c("income", 13, 16))

fwf2csv("example.txt", "example.csv",
        names = tab[, 1],
        begin = as.numeric(tab[, 2]),
        end = as.numeric(tab[, 3]))
df <- read.table("example.csv", header = TRUE,
                 sep = "\t", quote = "")
}
}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

