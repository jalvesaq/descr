\encoding{UTF-8}
\name{LogRegR2}
\alias{LogRegR2}
\title{Pseudo R² of logistic regression}
\description{
  The function calculates multiple R² analogues (pseudo R²) of logistic
  regression.
}
\usage{
LogRegR2(model)
}
\arguments{
  \item{model}{A logistic regression model.}
}

\details{The function calculates McFaddens R², Cox & Snell Index, and
  Nagelkerke Index of a logistic regression model.
}

\value{
  A object of class \code{list} with the calculated indexes.
}

\author{Dirk Enzmann}

