\name{mlogloss}
\alias{mlogloss}
\title{computes log loss for multiclass problem}
\description{computes log loss for multiclass problem}
\usage{mlogloss(actual, pred_m, eps = 0.001)}
\arguments{
  \item{actual}{integer vector with truth labels, values range from 0 to n - 1 classes}
  \item{pred_m}{predicted probs: column 1 => label 0, column 2 => label 1 and so on}
  \item{eps}{numerical cutoff taken very high}
}



\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{

# require(nnet)
# set.seed(1)
# actual = as.integer(iris$Species) - 1
# fit = nnet(Species ~ ., data = iris, size = 2)
# pred = predict(fit, iris)#note this is a 3-column prediction matrix!
# 
# mlogloss(actual, pred) # 0.03967

#library(titanic)
#baseline prediction
#data(titanic_train, package="titanic")
yHat = mean(titanic_train$Survived)#0.383838
mlogloss(titanic_train$Survived,yHat)
#try factors
titanic_train$Survived = as.factor(titanic_train$Survived)
mlogloss(titanic_train$Survived,yHat)
}
