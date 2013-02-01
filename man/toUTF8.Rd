\name{toUTF8}
\alias{toUTF8}

\title{Conversion to UTF-8 encoding}
\description{Converts the encoding of some attributes of an object to UTF-8}
\usage{
toUTF8(x, from = "WINDOWS-1252") 
}
\arguments{
  \item{x}{A R object, usually a variable of a data frame or a data frame.}
  \item{from}{A string indicating the original encoding. Common values are
    \code{"LATIN1"} and \code{"WINDOWS-1252"}. Type \code{iconvlist()} for the
    complete list of available encodings.}
}
\details{
  The function converts the attribute \code{label} of \code{x} from the
  specified encoding into UTF-8. If \code{x} is a factor, the levels are
  converted as well. If \code{x} is a data.frame, the function makes the
  conversions in all of its variables.
}
\value{
  The object with its label and levels converted.
}
\author{Jakson A. Aquino \email{jalvesaq@gmail.com}.}

\seealso{\link[base]{iconv}}

