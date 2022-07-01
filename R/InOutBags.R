InOutBags <-  structure(function#separates data into inbag and outbag
### convenience function to mitigate risk of improperly disentangling train/test
### NOTE: the original row names (too dangerous for repeated rows) are not kept but instead recorded in a separate column
(
 RF, ##<< object returned by call to randomForest() or ranger()
 data, ##<< data which was used to train the RF. NOTE: assumes setting of inbag=TRUE while training
 k, ##<< tree number
 inclRowNames = TRUE, ##<< create extra column of original row names
 NullRowNames=TRUE, ##<< if TRUE set row names to NULL
 verbose = 0 ##<< level of verbosity
){
 n=nrow(data)
 
 if ("randomForest" %in% class(RF)){
   inRows = rep(rownames(RF$inbag),time=RF$inbag[,k])
   outRows = names((RF$inbag[RF$inbag[,k]==0,k]))
 } else if ("ranger" %in% class(RF)) {
   inRows = rep(rownames(data),time=RF$inbag.counts[[k]])
   outRows = rownames(data)[RF$inbag.counts[[k]]==0] 
 }
 
 inbag = data[inRows,]
 inbag$origRows=inRows
 
 outbag = data[outRows,] 
 outbag$origRows=outRows
 
 if (NullRowNames) {
   rownames(inbag) = rownames(outbag) = NULL
 } else {
   rownames(inbag)  = 1:nrow(inbag)
   rownames(outbag) = 1:nrow(outbag) 
 }
 
 return(list(inbag=inbag,outbag=outbag))
 ### inbag and outbag subsets of the original data
}, ex = function(){
  rfTit = rfTitanic(nRows = 200,nodesize=10, ntree = 5)
  
  k=1
  tmp <- InOutBags(rfTit$RF, rfTit$data, k)
  
})