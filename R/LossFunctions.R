#Gini index 
gini_process <- structure(function#computes Gini index 
###computes Gini index 
(
  classes, ##<< vector of factors/categorical vars
  splitvar = NULL ##<< split variable
){
  #Assumes Splitvar is a logical vector
  if (is.null(splitvar)){
    base_prob <-table(classes)/length(classes)
    return(1-sum(base_prob**2))
  }
  base_prob <-table(splitvar)/length(splitvar)
  crosstab <- table(classes,splitvar)
  crossprob <- prop.table(crosstab,2)
  No_Node_Gini <- 1-sum(crossprob[,1]**2)
  Yes_Node_Gini <- 1-sum(crossprob[,2]**2)
  return(sum(base_prob * c(No_Node_Gini,Yes_Node_Gini)))
  ### Gini index
}, ex = function(){
  #Test binary case:
  
  #50/50split
  gini_process(c(rep(0,10),rep(1,10)))#0.5 CORRECT !
  #10/90split
  gini_process(c(rep(0,1),rep(1,9)))#0.18= CORRECT ! 
  #0/100split
  gini_process(factor(c(rep(0,0),rep(1,10)), levels=c(0,1)))#0
  
  
  #Test binary case:
  
  #25/25/25/25 split
  gini_process(factor(c(rep(0,5),rep(1,5),rep(2,5),
                        rep(3,5)), levels=c(0:3)))#0.75 = 4*0.25*0.75 CORRECT !
  #10/10/10/70 split
  gini_process(factor(c(rep(0,1),rep(1,1),rep(2,1),
                        rep(3,7)), levels=c(0:3)))#0.48 = 3*0.1*0.9+0.7*0.3  CORRECT !
  #0/0/0/100 split
  gini_process(factor(c(rep(0,0),rep(1,0),rep(2,0),
                        rep(3,20)), levels=c(0:3)))#0. CORRECT !
  
})


gini_index <- structure(function# compute Gini impurity for binary values only
### simple function to compute simple or penalized Gini impurity
### The "penalty" compares the class probabilities \code{pHat} with a reference estimate \code{pEst} 
### which would typically serve as a prediction (e.g. in a tree node).
(
  pHat, ##<< probabilities from the current data,
  pEst=NULL, ##<< estimated class probabilities (typically from an earlier inbag estimation). Only pass if you intend to compute the "validation-penalized Gini"
  k = 2, ##<< exponent of penalty term: abs(pHat-pEst)^k
  kind = 1, ##<< kind of penalty
  w=2 ##<< weights, default is 2 if you pass just a single probability instead of the vector (p,1-p)
) {
  
  gHat =pHat*(1.0 -pHat)
  gEst = pEst*(1.0 -pEst)
  if (!is.null(pEst)){
    if (kind==1){
      g = 2*gHat + abs(pHat-pEst)^k
      #g = 1- sum(pHat*pHat + (1.0 -pHat)*(1.0 -pHat))
      #g = 1- sum(pEst*pEst + (1.0 -pEst)*(1.0 -pEst))
      #g = g + abs(pHat-pEst)^k   
    } else if (kind==2) {#
      #g = 1- sum(pHat*pHat + (1.0 -pHat)*(1.0 -pHat))
      #g = pmin(0.5, g + abs(pHat-pEst)^k) #truncate at maximum uncertainty
      #g = g + 3*abs(pHat-0.5)*abs(pHat-pEst)^k 
      g = gHat + gEst + abs(pHat-pEst)^k
    } else if (kind==3) {#symmetric in p1 and p2
      g = gHat + gEst + 0.5*abs(pHat-pEst)^k
    }
  } else {
    g = sum(pHat * (1.0 -pHat)) * w
  }  
  return(g)
### simple or penalized Gini impurity
}, ex = function(){
  #Test binary case:
  
  gini_index(0.5,0.5,kind=1)
  gini_index(0.9,0.1,kind=1)
  gini_index(0.1,0.9,kind=1)
  
  gini_index(0.5,0.5,kind=2)
  gini_index(0.9,0.1,kind=2)
  gini_index(0.1,0.9,kind=2)
  
  
  gini_index(0.5,0.5,kind=3)
  gini_index(0.9,0.1,kind=3)
  gini_index(0.1,0.9,kind=3)
  
})



Accuracy <- structure(function# computes accuracy of a vector 
### Accuracy is defined as the proportion of correct labels
(
  y, ##<< vector of categorical/nominal values
  yHat, ##<< prediction/estimate
  dig = 8 ##<< number of digits
)
{
  if (length(y) ==0 | all(is.na(y))) return(0)
  #hack introduced on March 21:
  if (is.factor(yHat)) {yHat=as.numeric(yHat);yHat=yHat-min(yHat)}
  #return(round(1-mlogloss(y,yHat),dig) )
  
  if (missing(yHat)) yHat = Mode(y)
  acc=mean(y==yHat,na.rm=TRUE)
  return(round(acc,dig))
### Accuracy defined as proportion of values equal to majority  
}, ex = function(){
  
  
  Accuracy(c(rep(0,9),1), 1)
  Accuracy(c(rep(0,9),1), 0)
})



mlogloss <- structure(function#computes log loss for multiclass problem
### computes log loss for multiclass problem
(
  actual, ##<< integer vector with truth labels, values range from 0 to n - 1 classes
  pred_m, ##<< predicted probs: column 1 => label 0, column 2 => label 1 and so on
  eps = 1e-3 ##<< numerical cutoff taken very high
){
  n=length(actual)
  pred_m_org=pred_m
  if (n==0 | any(is.na(actual)) | any(is.na(pred_m)) ) return(1) #browser()
  if (length(pred_m)==1){#scalar prediction for binary case, label 1
    pred_m=rep(pred_m,n)
    pred_m = cbind("0"=1-pred_m,"1"=pred_m)
  }
  if (is.factor(actual)){
    actual=as.numeric(actual)
    actual=actual-min(actual)
  }
  viol =max(actual) >= ncol(pred_m) || min(actual) < 0
  #print(viol)
  if (!is.logical(viol)) browser()
  if(viol){
    stop(cat('True labels should range from 0 to', ncol(pred_m) - 1, '\n'))
  }
  
  pred_m[pred_m > 1 - eps] = 1 - eps
  pred_m[pred_m < eps] = eps
  pred_m = t(apply(pred_m, 1, function(r)r/sum(r)))
  actual_m = matrix(0, nrow = nrow(pred_m), ncol = ncol(pred_m))
  actual_m[matrix(c(1:nrow(pred_m), actual + 1), ncol = 2)] = 1
  LL=-sum(actual_m * log(pred_m))/nrow(pred_m)
  if (is.na(LL)) browser()
  return(LL)
}, ex = function(){
  
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
})

lpnorm <- structure(function#Compute the Lp norm of a vector.
### Compute the Lp norm of a vector.
(
  x, ##<< vector to compute the Lp norm of
  p = 2 ##<< parameter of p norm
){
  if(is.vector(x) && !is.list(x)){
    if(p == Inf) return(max(abs(x)))
    if(p >= 1) return( sum(abs(x)^p)^(1/p) )
    if(0 <= p && p < 1) return( sum(abs(x)^p) )
  }
  if(is.matrix(x)) return(apply(x, 1, lpnorm, p))
  if(is.list(x)) return(sapply(x, lpnorm, p))
  NA 
### Lp norm of a vector or NA
}, ex = function(){
  lpnorm(1:10)
  lpnorm(matrix(1:25, 5, 5))
  lpnorm(split(1:25, rep(1:5, each = 5)))
  
  lpnorm(1:10, 1)
  lpnorm(matrix(1:25, 5, 5), 1)
  lpnorm(split(1:25, rep(1:5, each = 5)), 1)
  
  lpnorm(rnorm(10), 0)
  lpnorm(matrix(rnorm(25), 5, 5), 0)
  lpnorm(split(rnorm(25), rep(1:5, each = 5)), 0)
  
  lpnorm(-5:5, Inf)
  lpnorm(matrix(-25:-1, 5, 5), Inf)
  lpnorm(split(-25:-1, rep(1:5, each = 5)), Inf)
})
