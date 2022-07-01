## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
rerun = TRUE
runArabidopsis=FALSE
library(rfVarImpOOB)
data("titanic_train", package = "rfVarImpOOB",  envir = environment())

## ----eval=rerun---------------------------------------------------------------
naRows = is.na(titanic_train$Age)
data2=titanic_train[!naRows,]
RF =randomForest(Survived ~ Age + Sex + Pclass + PassengerId, data=data2, ntree=50,importance=TRUE,mtry=2, keep.inbag=TRUE)

## ----PMDI2_RF,eval=rerun------------------------------------------------------
if (is.factor(data2$Survived)) data2$Survived = as.numeric(data2$Survived)-1
VI_PMDI3 = GiniImportanceForest(RF, data2,score="PMDI22",Predictor=mean)
plotVI2(VI_PMDI3, score="PMDI22", decreasing = TRUE)

