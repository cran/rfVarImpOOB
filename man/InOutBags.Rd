\name{InOutBags}
\alias{InOutBags}
\title{separates data into inbag and outbag}
\description{convenience function to mitigate risk of improperly disentangling train/test
NOTE: the original row names (too dangerous for repeated rows) are not kept but instead recorded in a separate column}
\usage{InOutBags(RF, data, k, inclRowNames = TRUE, NullRowNames = TRUE, 
    verbose = 0)}
\arguments{
  \item{RF}{object returned by call to randomForest()}
  \item{data}{data which was used to train the RF. NOTE: assumes setting of inbag=TRUE while training}
  \item{k}{tree number}
  \item{inclRowNames}{create extra column of original row names}
  \item{NullRowNames}{if TRUE set row names to NULL}
  \item{verbose}{level of verbosity}
}

\value{inbag and outbag subsets of the original data}

\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{
rfTit = rfTitanic(nRows = 200,nodesize=10, ntree = 5)

k=1
tmp <- InOutBags(rfTit$RF, rfTit$data, k)

}
