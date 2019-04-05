\name{GiniImportanceForest}
\alias{GiniImportanceForest}
\title{computes inbag and OOB Gini importance averaged over all trees in a forest}
\description{workhorse function of this package}
\usage{GiniImportanceForest(RF, data, ylabel = "Survived", zeroLeaf = TRUE, 
    agg = c("mean", "median", "none")[1], score = c("PMDI21", 
        "MDI", "MDA", "MIA")[1], Predictor = Mode, verbose = 0)}
\arguments{
  \item{RF}{object returned by call to randomForest()}
  \item{data}{data which was used to train the RF. NOTE: assumes setting of inbag=TRUE while training}
  \item{ylabel}{name of dependent variable}
  \item{zeroLeaf}{if TRUE discard the information gain due to splits resulting in n=1}
  \item{agg}{method of aggregating importance scores across trees. If "none" return the raw arrays (for debugging)}
  \item{score}{scoring method:MDI=mean decrease impurity (Gini),MDA=mean decrease accuracy (permutation),MIA=mean increase accuracy}
  \item{Predictor}{function to estimate node prediction, such as Mode or mean or median. Alternatively, pass an array of numbers as replacement for the yHat column of tree}
  \item{verbose}{level of verbosity}
}

\value{matrix with variable importance scores and their stdevs}

\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{

data("titanic_train", package = "rfVarImpOOB",  envir = environment())
set.seed(123)
ranRows=sample(nrow(titanic_train), 300)
data=titanic_train[ranRows,]

RF = randomForest::randomForest(formula = Survived ~ Sex + Pclass + PassengerId,
                                data=data,
                                ntree=5,importance=TRUE,
                                mtry=3,keep.inbag=TRUE, 
                                nodesize = 20)
data$Survived = as.numeric(data$Survived)-1
VI_Titanic = GiniImportanceForest(RF, data,ylab="Survived")
}
