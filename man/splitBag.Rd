\name{splitBag}
\alias{splitBag}
\title{splits the data from parent node into left and right children}
\description{The function properly splits on factor levels}
\usage{splitBag(treeRow, bag, tree)}
\arguments{
  \item{treeRow}{current row of tree dataframe to be}
  \item{bag}{The data for the current row}
  \item{tree}{tree (from randomForest::getTree)}
}

\value{list with elements left_daughter, right_daughter}

\author{Markus Loecher <Markus.Loecher@gmail.com>}





