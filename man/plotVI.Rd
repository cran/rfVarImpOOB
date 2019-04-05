\name{plotVI}
\alias{plotVI}
\title{creates barplots for variable importances}
\description{creates barplots for variable importances}
\usage{plotVI(VIbench, order_by = "Gini_OOB", decreasing = TRUE)}
\arguments{
  \item{VIbench}{matrix with importance scores as returned by GiniImportanceForest}
  \item{order_by}{how to order}
  \item{decreasing}{which direction to sort}
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
plotVI(VI_Titanic,decreasing = TRUE)

}
