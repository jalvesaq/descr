\name{descr}
\alias{descr}
\title{Summary of an object}
\description{
  Wrapper for the function \code{summary} of \pkg{base} package, including
  information about variable label.  The function prints the \code{label}
  attribute of the object and, then, invokes \code{summary(object)}. If the object
  is a data frame, the function prints the \code{label} and invokes
  \code{summary} for each variable in the data frame.
}
\usage{
descr(x)
}
\arguments{
  \item{x}{The object to be described.}
}

\value{
  Null.
}

\author{Jakson Aquino \email{jalvesaq@gmail.com}}

\seealso{\code{\link[base]{summary}}}

