\name{labels2R}
\alias{labels2R}
\title{Conversion of specially written text file into R code}
\description{
  Convert a specially written text file with information on variable labels
  and value labels into R code that converts integer vectors into factor
  variables.
}
\usage{
labels2R(lfile, rfile, dfname = "b")
}
\arguments{
  \item{lfile}{The path to the text file to be converted.}
  \item{rfile}{The path to the file to be created.}
  \item{dfname}{Name of data.frame where the variables are.}
}

\value{
  NULL.
}

\details{
  The return value is NULL, but \code{rfile} is created if the function is
  successful. The file is an R code that converts numeric vectors into
  factors. The text file must have a format as in the example below:

  \preformatted{
  v1 Sex
  1 Female
  2 Male

  v2 Household income

  v3 Taking all things together, would you say you are...
  1 Very happy
  2 Rather happy
  3 Not very happy
  4 Not at all happy
  }

  The above code would be converted into:

  \preformatted{
  b$v1 <- factor(b$v1, levels=c(1, 2), labels=c("Female", "Male"))
  attr(b$v1, "label") <- "Sex"
  attr(b$v2, "label") <- "Household income"
  b$v3 <- factor(b$v3, levels=c(1, 2, 3, 4),
                 labels=c("Very happy", "Rather happy",
                          "Not very happy", "Not at all happy"))
  attr(b$v3, "label") <- "Taking all things together, would you say you are..."
  }

}

\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

