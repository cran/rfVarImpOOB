\name{Accuracy}
\alias{Accuracy}
\title{computes accuracy of a vector }
\description{Accuracy is defined as the proportion of correct labels}
\usage{Accuracy(y, yHat, dig = 8)}
\arguments{
  \item{y}{vector of categorical/nominal values}
  \item{yHat}{prediction/estimate}
  \item{dig}{number of digits}
}

\value{Accuracy defined as proportion of values equal to majority  }

\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{


Accuracy(c(rep(0,9),1), 1)
Accuracy(c(rep(0,9),1), 0)
}
