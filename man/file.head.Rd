\name{file.head}
\alias{file.head}
\title{Prints first lines of a file.}
\description{
  The function prints the first lines of a file, optionally truncating the lines
  according to the screen width. The lines are truncated at
  \code{getOption("width") - 2}.
}
\usage{
file.head(file, n, truncate.cols = TRUE)
}
\arguments{
  \item{file}{Character: The name of the file whose first lines should be
    printed.}
  \item{n}{The number of lines to show.}
  \item{truncate.cols}{Logical: if \code{TRUE} truncate the lines.}
}
\value{
  \code{NULL}.
}
\author{Jakson A. Aquino \email{jalvesaq@gmail.com}}

