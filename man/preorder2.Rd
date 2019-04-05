\name{preorder2}
\alias{preorder2}
\title{recursive traversal of tree assigning row numbers of data for each node and leaf}
\description{Recursive calling stops at leaf after which the function propagates back up the tree}
\usage{preorder2(treeRow, bag, tree, verbose = 0)}
\arguments{
  \item{treeRow}{current row of tree dataframe to be}
  \item{bag}{The data for the current row}
  \item{tree}{tree (from randomForest::getTree to be traversed}
  \item{verbose}{level of verbosity}
}

\value{tree with rownames in column node}

\author{Markus Loecher <Markus.Loecher@gmail.com>}




\examples{
data("titanic_train", package = "rfVarImpOOB",  envir = environment())

set.seed(123)
ranRows=sample(nrow(titanic_train), 300)

RF = randomForest::randomForest(formula = Survived ~ Sex + Pclass + PassengerId,
                    data=titanic_train[ranRows,],
                    ntree=5,importance=TRUE,
                    mtry=3,keep.inbag=TRUE, 
                    nodesize = 1)
k=2
tree = randomForest::getTree(RF, k, labelVar = TRUE) 
tree$node=NA
inbag = rep(rownames(RF$inbag),time=RF$inbag[,k])
#trainBag=titanic_train[inbag,]
trainBag=titanic_train[ranRows,][inbag,]
tree=preorder2(1,trainBag,tree)
}
