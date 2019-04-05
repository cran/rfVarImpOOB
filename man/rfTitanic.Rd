\name{rfTitanic}
\alias{rfTitanic}
\title{fit a random forest model on the titanic data}
\description{convenience function to reduce overhead of repeatedly fitting RF to titanic data}
\usage{rfTitanic(formel = Survived ~ Sex + Pclass + PassengerId, nRows = 500, 
    ntree = 10, mtry = 3, nodesize = 1)}
\arguments{
  \item{formel}{formula}
  \item{nRows}{subsample size}
  \item{ntree}{number of trees}
  \item{mtry}{mtry}
  \item{nodesize}{nodesize}
}



\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{
rfTit = rfTitanic(nRows = 500,nodesize=10)
}
