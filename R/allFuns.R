

preorder2 <- structure(function# recursive traversal of tree assigning row numbers of data for each node and leaf
### Recursive calling stops at leaf after which the function propagates back up the tree
(
  treeRow, ##<< current row of tree dataframe to be. Default should be the root node, which is coded as 1 
  bag,  ##<< The data for the current row
  tree,  ##<< tree (from randomForest::getTree to be traversed
 # ASSIGN=TRUE, ##<<  unused!! no more global assignments
  verbose=0  ##<< level of verbosity
){
  #print(tree[treeRow, ])
  #if (length(tree[treeRow, "status"])==0) browser()
  if (tree[treeRow, "status"] ==1){ #no leaf!
    left_daughter = tree[treeRow, 'left daughter']
    right_daughter = tree[treeRow, 'right daughter']
    
    daughters = splitBag(treeRow,bag,tree)# returns row names !!
    
    tree=preorder2(left_daughter,bag[daughters$left_daughter,],tree, verbose=verbose)
    tree=preorder2(right_daughter,bag[daughters$right_daughter,],tree,verbose=verbose)
  }
  #rowNamesOrig = as.character(floor(as.numeric(rownames(bag))))
  rowNamesOrig = rownames(bag)
 
  if (nrow(bag)>0) {
    tree$node[treeRow] <- list(rowNamesOrig)
  }
  if (verbose) cat(treeRow,":", rowNamesOrig,"\n")
  return(tree)
  ### tree with rownames in column node
}, ex = function(){
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
  attr(tree, "rflib") = "randomForest"
  inbag = rep(rownames(RF$inbag),time=RF$inbag[,k])
  #trainBag=titanic_train[inbag,]
  trainBag=titanic_train[ranRows,][inbag,]
  tree=preorder2(1,trainBag,tree)
})

intToBin = function# translate decimal to binary format
### The function translate decimal to binary format (https://stackoverflow.com/questions/6614283/converting-decimal-to-binary-in-r)
(x,##<< decimal number to be converted
 n=NULL, ##<< number of bits
 asChar = FALSE ##<< return as character?
) 
{
  y <- as.integer(x)
  class(y) <- "binmode"
  y <- as.character(y)
  dim(y) <- dim(x)
  if (asChar) return(y)
  
  res = as.integer(unlist(strsplit(y, "")))
  if (!is.null(n)){
    K = length(res)
    if (K < n) res = c(rep(0,n-K),res)
  }
  return(res)
}

dectobin <- function# translate decimal to binary format
### The function translate decimal to binary format
(y,##<< decimal number to be converted
 n=NULL ##<< number of bits
) {
  # find the binary sequence corresponding to the decimal number 'y'
  stopifnot(length(y) == 1, mode(y) == 'numeric')
  q1 <- (y / 2) %/% 1
  r <- y - q1 * 2
  res = c(r)
  while (q1 >= 1) {
    q2 <- (q1 / 2) %/% 1
    r <- q1 - q2 * 2
    q1 <- q2
    res = c(r, res)
  }
  
  if (!is.null(n)){
    K = length(res)
    if (K < n) res = c(rep(0,n-K),res)
  }
  
  return(res)
}

# intToBin <- function# translate decimal to binary format
# ### The function translate decimal to binary format
# (
#   x, ##<< decimal number to be converted 
#   verbose = FALSE   ##<< 
# ){
#   K = ceiling(log2(x))
#   xBinary = rep(0, K)
#   xBinary[1] = 1
#   x = x-2^(K-1)
#   #y = x
#   if (K>1){
#     for (i in 2:K){
#       K = ceiling(log2(x))
#     }
#   }
#   
# }

splitBag <- function# splits the data from parent node into left and right children
### The function properly splits on factor levels
(
  treeRow, ##<< current row of tree dataframe to be 
  bag,  ##<< The data for the current row
  tree   ##<< tree (from randomForest::getTree)
){
  
  split_var = as.character(tree[treeRow,'split var'])
  if (is.factor(bag[,split_var])){
    #levels = unique(bag[,split_var])
    #levels = sort(levels, decreasing = TRUE)
    levels = levels(bag[,split_var])
    n = length(levels)#} else{
    if (n<2) n=0;
    #print(treeRow)
    treeLib = attr(tree, "rflib")
    if ("randomForest" %in% treeLib){
      binCode =rev(intToBin(tree[treeRow,'split point'],n=n))
      split_point = try(as.character(levels[binCode])) #rF encodes male as 10, female as 01
      if (inherits(split_point, "try-error")) browser()
      mask =  bag[,split_var] %in% split_point
    } else if ("ranger" %in% treeLib) {
      #print("ranger")
      ### ranger help for treeInfo:
      #In the "partition" mode, the splitval values for 
      #unordered factor are comma separated lists of values, 
      #representing the factor levels (in the original order) 
      #going to the right.
      goingRight = as.numeric(strsplit(tree[treeRow,"splitval"],",")[[1]])
      mask =  !(bag[,split_var] %in% levels[goingRight])
    } else {
      cat("tree attribute ", treeLib, "not found\n")
    }
    left_daughter = rownames(bag)[mask]
    right_daughter = rownames(bag)[!mask]
    #browser()
    
  } else{
    split_point = tree[treeRow,'split point']
    mask = bag[,split_var] <= split_point
    #ranger: values smaller or equal go to the left, 
    #larger values to the right.
    left_daughter = rownames(bag)[mask]
    right_daughter = rownames(bag)[!mask]
    #if (verbose >0) {diagnostics(in_node_df, left_daughter, right_daughter, split_var)} 
  }
  return(list(left_daughter = (left_daughter), right_daughter = (right_daughter)))
  ### list with elements left_daughter, right_daughter
} 

InfGain <- function#computes information gain for each parent node in a tree
### information gain for each parent node in a tree
(
  tree, ##<< tree (from randomForest::getTree)
  total=TRUE,  ##<< if TRUE compute the sum instead of the mean
  zeroLeaf = TRUE, ##<< if TRUE discard the information gain due to splits resulting in n=1
  score=c("PMDI21","MDI","MDA","MIA")[1], ##<< scoring method:MDI=mean decrease impurity (Gini),MDA=mean decrease accuracy (permutation),MIA=mean increase accuracy
  verbose=0 ##<< level of verbosity
){
  
  IG_result = MIA_result = rep(NA, nrow(tree))
  tree$n_node = 0
  
  for (i in 1:nrow(tree)){
    en_node = tree[i, 'gini_index'] # get the entropy of that row
    n_node = tree[i, 'node'] %>% unlist %>%  na.omit %>%length # get the number of elements of the bag
    tree[i, 'n_node'] = n_node
    
    ld = tree[i, 'left daughter'] # row number of the left daughter
    rd = tree[i, 'right daughter'] # right daughter
    #browser()
    
    if (ld==0|rd==0){ # information gain for terminal node = NA
      IG=NA
      IG_result[i] = IG
      next
    }
    
    en_ld = tree[ld, 'gini_index'] # gini index of the left daughter
    en_rd = tree[rd, 'gini_index'] # right daughter
    
    nld = tree[ld, 'node']%>% unlist %>% na.omit %>% length  # get the number of elements of left daughter
    nrd = tree[rd, 'node']%>% unlist %>% na.omit %>% length # right
    dfCorrection=0#idea for later
    IG = en_node - sum(en_ld*(nld-dfCorrection), en_rd*(nrd-dfCorrection))/(n_node-2*dfCorrection) # formula of information gain
    if (total) IG = IG*(n_node-2*dfCorrection)
    #new idea: discount inf gain entirely if one of the children only has one element.
    if (zeroLeaf) IG = IG*ifelse(pmin(nld,nrd)<2,0,1)
    #if (length(IG)!=1) browser()
    IG_result[i] = IG
    
    if (score =="MIA"){
      MIA = sum(tree$Accuracy[ld]*nld, tree$Accuracy[rd]*nrd)/(n_node) -tree$Accuracy[i]
      MIA=MIA*n_node
      MIA_result[i] = MIA
      #if (is.na(MIA) | MIA == 0) browser()
    }
  }
  #browser()
  tree$`IG_gini`=round(IG_result,3)
  if (score =="MIA") {
    tree$IG_acc = round(MIA_result,3)
    tree[is.na(tree[,"IG_acc"]),"IG_acc"] = 0 
  }
  
  tree[is.na(tree[,'IG_gini']),'IG_gini'] = 0 
  
  
  return(tree)
  ### tree object augmented with information gain at each node
} 
 

GiniImportanceForest <- structure(function#computes inbag and OOB Gini importance averaged over all trees in a forest
### workhorse function of this package
(
  RF, ##<< object returned by call to randomForest() 
  data, ##<< data which was used to train the RF. NOTE: assumes setting of inbag=TRUE while training 
  ylabel = "Survived", ##<< name of dependent variable
  zeroLeaf = TRUE, ##<< if TRUE discard the information gain due to splits resulting in n=1
  agg = c("mean","median","none")[1], ##<< method of aggregating importance scores across trees. If "none" return the raw arrays (for debugging)
  score=c("PMDI21", "MDI","MDA","MIA")[1], ##<< scoring method:MDI=mean decrease impurity (Gini),MDA=mean decrease accuracy (permutation),MIA=mean increase accuracy
  Predictor=Mode, ##<< function to estimate node prediction, such as Mode or mean or median. Alternatively, pass an array of numbers as replacement for the yHat column of tree
  verbose=0 ##<< level of verbosity
){
  stopifnot(!is.null(RF$inbag))
  vars = dimnames(attr(RF$terms, "factors"))
  stopifnot(ylabel == setdiff(vars[[1]], vars[[2]]))
  
  
  nTrees = ncol(RF$inbag)
  
  VIbench = as.data.frame(randomForest::importance(RF))
  vars = rownames(VIbench)
  Gini_inbag=Gini_OOB = matrix(0,nrow=length(vars),ncol=nTrees,dimnames=list(vars,1:nTrees))
  
  #VItrain = c(PassengerId=0, Sex=0, Pclass=0)
  inTree=outTree=list()
  
  for (k in 1:nTrees){
    trackingInfo=c(k=k,bag="in");#print(trackingInfo)
    #inbag = as.numeric(names((RF$inbag[RF$inbag[,k]>0,k])))
    inbag = rep(rownames(RF$inbag),time=RF$inbag[,k])
    #trainBag=titanic_train[inbag,]
    trainBag=data[inbag,]
    rownames(trainBag)  = 1:nrow(trainBag)
    #cat("k=",k,"\n")
    inImp =GiniImportanceTree(trainBag,RF,k,ylabel=ylabel, zeroLeaf=zeroLeaf,score=score,Predictor=Predictor)
    yScore=colnames(inImp)[2]
    Gini_inbag[,k] =  inImp[vars,yScore]
    inTree[[k]] =GiniImportanceTree(trainBag,RF,k,returnTree=TRUE,ylabel=ylabel, zeroLeaf=zeroLeaf,score=score,Predictor=Predictor)
    
    trackingInfo=c(k=k,bag="out");#print(trackingInfo)
    outbag = names((RF$inbag[RF$inbag[,k]==0,k]))
    OOB = data[outbag,]
    rownames(OOB)  = 1:nrow(OOB)
    if (verbose<0) outTree[[k]] =GiniImportanceTree(OOB, RF, k,returnTree=TRUE,ylabel=ylabel, Predictor=inTree[[k]]$yHat, zeroLeaf=zeroLeaf,score=score)
    outImp =GiniImportanceTree(OOB, RF, k,ylabel=ylabel, Predictor=inTree[[k]]$yHat, zeroLeaf=zeroLeaf,score=score)
    #browser()
    yScore=colnames(outImp)[2]
    Gini_OOB[,k] = outImp[vars,yScore]
    
    #VItrain["PassengerId"] = VItrain["PassengerId"] +tmp["PassengerId","sum_IG_gini"]
    #VItrain["Sex"] = VItrain["Sex"] + tmp["Sex","sum_IG_gini"]
    #VItrain["Pclass"] = VItrain["Pclass"] + tmp["Pclass","sum_IG_gini"]
  }
  if (verbose<0)  return(list(inTree=inTree, outTree=outTree))
  #browser()
  if (agg == "none"){
    return(list(Gini_OOB=Gini_OOB,Gini_inbag=Gini_inbag))
  } else if (agg == "mean"){
    VIbench$Gini_OOB = rowMeans(Gini_OOB, na.rm = TRUE)
    VIbench$Gini_inbag = rowMeans(Gini_inbag, na.rm = TRUE)
  } else if (agg == "median"){
    VIbench$Gini_OOB = apply(Gini_OOB,1,median, na.rm = TRUE)
    VIbench$Gini_inbag = apply(Gini_inbag,1,median, na.rm = TRUE)
  }
  
  VIbench$Gini_OOB_sd = apply(Gini_OOB,1,sd, na.rm = TRUE)/sqrt(ncol(Gini_OOB))
  VIbench$Gini_inbag_sd = apply(Gini_inbag, 1,sd, na.rm = TRUE)/sqrt(ncol(Gini_inbag))
  
  
  return(VIbench)
  ### matrix with variable importance scores and their stdevs
}  , ex = function(){
  
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
})




GiniImportanceTree <- structure(function# computes Gini information gain for one tree from randomForest
### computes importance scores for an individual tree. 
### These can be based on Gini impurity or Accuracy or logloss 
(
  bag, ##<< data to compute the Gini gain for
  RF, ##<< object returned by call to randomForest() 
  k, ##<< which tree
  ylabel = "Survived", ##<< name of dependent variable
  returnTree = FALSE, ##<< if TRUE returns the tree data frame otherwise the aggregated Gini importance grouped by split variables
  zeroLeaf = TRUE, ##<< if TRUE discard the information gain due to splits resulting in n=1
  score=c("PMDI21", "MDI","MDA","MIA")[1], 
  ### scoring method:PMDI=mean decrease penalized Gini impurity (note:the last digit is the exponent of the penalty!), 
  ### MDI=mean decrease impurity (Gini), MDA=mean decrease accuracy (permutation),
  ### MIA=mean increase accuracy
  Predictor=Mode, ##<< function to estimate node prediction, such as Mode or mean or median. Alternatively, pass an array of numbers as replacement for the yHat column of tree
  verbose=0 ##<< level of verbosity
){
  #brain dead "solution"  to the CRAN note issues,
  #see https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
  IG_acc=IG_gini=`split var`=NULL
  
  yUniq = sort(base::unique(bag[,ylabel])) 
  if (!all(yUniq %in% c(0,1) )){
    print(yUniq)
  }
  stopifnot(all(yUniq %in% c(0,1))) 
  
  #preorder(1,OOB)
  #proper inbag keeping duplicates:
  #inbag = as.numeric(names((rf3a$inbag[rf3a$inbag[,k]>0,k])))
  if ("randomForest" %in% class(RF)){
    tree = as.data.frame(randomForest::getTree(RF, k, labelVar = TRUE))
    attr(tree, "rflib") = "randomForest"
  } else if ("ranger" %in% class(RF)) {
    tree = as.data.frame(getTree_ranger(RF, k))
    attr(tree, "rflib") = "ranger"
  }
  tree$gini_index = NA
  tree$IG_gini= NA
  if (score =="MIA"){
    tree$Accuracy = NA
    tree$IG_acc = NA
  }
  if (inherits(Predictor, "function")) {
    tree$yHat=sample(bag[,ylabel],nrow(tree))
  } else {
    tree$yHat= Predictor
  }
  tree$node=NA
  #browser()
  tree=preorder2(1,bag,tree)#uses unmodified row names now !! (e.g. "6.1)
  
  for (j in 1:nrow(tree)){
    y=bag[unlist(tree[j,"node"]),ylabel]#NEEDS to be (0,1) !!!!
    if (inherits(Predictor, "function")) {
      tree$yHat[j] = Predictor(na.omit(y))
      #print(table(y))
      tree$gini_index[j] = round(gini_index(mean(y,na.rm=TRUE)),9)
    } else if (grepl("PMDI", score) ) {#penalized Gini for validation data only:
      p=as.numeric(substr(score,5,5))#exponent of the penalty
      kind=as.numeric(substr(score,6,6))#kind of penalty
      tree$gini_index[j] = round(gini_index(mean(y,na.rm=TRUE),Predictor[j],p,kind),9)
    } else {#this should be for validation data only:
      tree$gini_index[j] = round(gini_index(mean(y,na.rm=TRUE),Predictor[j],kind=kind),9)
    }
  }
  #tree$gini_index[j] = round(gini_index(titanic_train[unlist(tree[j,"node"]),"Survived"]),4)
  if (score =="MIA"){
    for (j in 1:nrow(tree)){
      y=bag[unlist(tree[j,"node"]),ylabel]
      #yMajority = Mode(y)
      #tree$Accuracy[j] = round(mean(y==yMajority),9)
      tree$Accuracy[j] = Accuracy(y, yHat = tree$yHat[j])
      #if (abs(tree$Accuracy[j])>1) browser()
    }
      
  }
  
  tree=InfGain(tree, zeroLeaf=zeroLeaf,score=score)
  
  if (returnTree) return(tree)
  
  if (score =="MIA") {
    df = dplyr::group_by(tree[,c('split var', 'IG_acc')], `split var`)
    pivot = dplyr::summarise(df,`sum_IG_acc`=sum(`IG_acc`))
    
  } else{
    df = dplyr::group_by(tree[,c('split var', 'IG_gini')], `split var`)
    pivot = dplyr::summarise(df,`sum_IG_gini`=sum(`IG_gini`))
  }
    
    count = dplyr::count(df)
  pivot$`count` = count$n
  pivot = as.data.frame(pivot)
  
  pivot = pivot[!is.na(pivot$`split var`),]
  rownames(pivot) = pivot[,1]
  #if (pivot["PassengerId","sum_IG_gini"] < 0) browser()
  if (any(is.na(pivot))) browser()
  
  return(pivot)
  ### if returnTree==TRUE returns the tree data frame otherwise the aggregated Gini importance grouped by split variables
} , ex = function(){
  
  rfTit = rfTitanic(nRows = 500,nodesize=10)
  rfTit$data$Survived = as.numeric(rfTit$data$Survived)-1
  k=1
  tmp <- InOutBags(rfTit$RF, rfTit$data, k)
  IndivTree =getTree(rfTit$RF,k)
  #plot(as.party(tmp))#does not work
  InTree = GiniImportanceTree(tmp$inbag,rfTit$RF,k,returnTree=TRUE)
  OutTree = GiniImportanceTree(tmp$outbag,rfTit$RF,k,returnTree=TRUE)
  
})


rfTitanic <- structure(function#fit a random forest model on the titanic data
### convenience function to reduce overhead of repeatedly fitting RF to titanic data
(
  formel = Survived ~ Sex + Pclass + PassengerId, ##<< formula
  nRows=500, ##<< subsample size
  ntree=10, ##<< number of trees
  mtry=3, ##<< mtry
  nodesize = 1 ##<< nodesize
){
  #library(titanic)
  titanic_train=NULL
  data("titanic_train", package = "rfVarImpOOB",  envir = environment())
  
  set.seed(123)
  ranRows=sample(nrow(titanic_train), nRows)
  train=titanic_train[ranRows,]
  
  rf = randomForest::randomForest(formula = formel,
                      data=train,
                      ntree=ntree,importance=TRUE,
                      mtry=mtry,keep.inbag=TRUE, 
                      nodesize = nodesize)
  
  return(list(RF=rf,data=train))
  
} , ex = function(){
  rfTit = rfTitanic(nRows = 500,nodesize=10)
})



Mode <- structure(function#computes the mode of an array
### returns the mode of a vector
(
  x ##<< vector to find mode of
) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}, ex = function(){
  
  Mode(rep(letters[1:3],1:3))
  Mode(c(TRUE,TRUE,FALSE))
  Mode(c(TRUE,TRUE,FALSE,FALSE))
})

if (0){
  library(inlinedocs)
  setwd("C:/Users/loecherm/Dropbox/Markus/Research/codeandstats/randomforest_investigation/")
  package.skeleton.dx("rfVarImpOOB");system("rm rfVarImpOOB/man/rfVarImpOOB-package.Rd");
  system("rm rfVarImpOOB/man/InfGain.Rd");#system("rm rfVarImpOOB/man/InOutBags.Rd")
}

