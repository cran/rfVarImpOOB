\name{gini_index}
\alias{gini_index}
\title{compute Gini impurity for binary values only}
\description{simple function to compute simple or penalized Gini impurity
The "penalty" compares the class probabilities \code{pHat} with a reference estimate \code{pEst} 
which would typically serve as a prediction (e.g. in a tree node).}
\usage{gini_index(pHat, pEst = NULL, k = 2, kind = 1, w = 2)}
\arguments{
  \item{pHat}{probabilities from the current data,}
  \item{pEst}{estimated class probabilities (typically from an earlier inbag estimation). Only pass if you intend to compute the "validation-penalized Gini"}
  \item{k}{exponent of penalty term: abs(pHat-pEst)^k}
  \item{kind}{kind of penalty}
  \item{w}{weights, default is 2 if you pass just a single probability instead of the vector (p,1-p)}
}

\value{simple or penalized Gini impurity}

\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{
#Test binary case:

gini_index(0.5,0.5,kind=1)
gini_index(0.9,0.1,kind=1)
gini_index(0.1,0.9,kind=1)

gini_index(0.5,0.5,kind=2)
gini_index(0.9,0.1,kind=2)
gini_index(0.1,0.9,kind=2)


gini_index(0.5,0.5,kind=3)
gini_index(0.9,0.1,kind=3)
gini_index(0.1,0.9,kind=3)

}
