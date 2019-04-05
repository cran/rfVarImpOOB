\name{plotVI2}
\alias{plotVI2}
\title{creates barplots for variable importances}
\description{creates barplots for variable importances including permutation scores}
\usage{plotVI2(VIbench, decreasing = TRUE, with_MDA = TRUE, ordered_by = "inbag", 
    score = "Gini Importance", horizontal = TRUE, fill = "order", 
    labelSize = 10, nrow = 3)}
\arguments{
  \item{VIbench}{matrix with importance scores as returned by GiniImportanceForest}
  \item{decreasing}{which direction to sort}
  \item{with_MDA}{also visualize mean decrease in accuracy (permutation importance)}
  \item{ordered_by}{how to order}
  \item{score}{type of importance score: Gini, MIA,..}
  \item{horizontal}{horizontal barplot instead of vertical ?}
  \item{fill}{fill style for barplots; use e.g. shQuote("blue") to pass color strings}
  \item{labelSize}{size of axis labels}
  \item{nrow}{number of rows of ploztz arrangement}
}



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
plotVI2(VI_Titanic,decreasing = TRUE)

}
