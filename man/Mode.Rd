\name{Mode}
\alias{Mode}
\title{computes the mode of an array}
\description{returns the mode of a vector}
\usage{Mode(x)}
\arguments{
  \item{x}{vector to find mode of}
}



\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{

Mode(rep(letters[1:3],1:3))
Mode(c(TRUE,TRUE,FALSE))
Mode(c(TRUE,TRUE,FALSE,FALSE))
}
