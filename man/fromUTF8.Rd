\name{fromUTF8}
\alias{fromUTF8}

\title{Conversion from UTF-8 encoding}
\description{Converts the encoding of some attributes of an object from UTF-8 into other
  encoding.}
\usage{
fromUTF8(x, to = "WINDOWS-1252") 
}
\arguments{
  \item{x}{A R object, usually a variable of a data frame or a data frame.}
  \item{to}{A string indicating the desired encoding. Common values are
    \code{"LATIN1"} and \code{"WINDOWS-1252"}. Type \code{iconvlist()} for the
    complete list of available encodings.}
}
\details{
  The function converts the attribute \code{label} of \code{x} from UTF-8 into the
  specified encoding. If \code{x} is a factor, the levels are converted as well.
  If \code{x} is a data.frame, the function makes the conversions in all of its
  variables.
}
\value{
  The object with its label and levels converted.
}
\author{Jakson A. Aquino \email{jalvesaq@gmail.com}.}

\seealso{\link[base]{iconv}}

