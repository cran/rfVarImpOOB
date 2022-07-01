getTree_ranger = function(
  RF, ##<< object returned by call to ranger() 
  k ##<< which tree
){
  single_Tree2 = ranger::treeInfo(RF, k) 
  #translate structures:
  single_Tree2$status=1
  single_Tree2$status[single_Tree2$terminal] = -1
  single_Tree2$'left daughter' = single_Tree2$leftChild + 1
  single_Tree2$'right daughter' = single_Tree2$rightChild + 1
  
  single_Tree2$'left daughter'[is.na(single_Tree2$'left daughter')] = 0
  single_Tree2$'right daughter'[is.na(single_Tree2$'right daughter')] = 0
    
  single_Tree2$'split var' = single_Tree2$splitvarName
  single_Tree2$'split point' = single_Tree2$splitval
  single_Tree2$node=NA

  #single_Tree2 = as.data.frame(getTree_ranger(RF, k))
  attr(single_Tree2, "rflib") = "ranger"
  
  return(single_Tree2)  
}


decision_path_SingleTree = function(
  RF, ##<< object returned by call to randomForest() 
  train_data, ##<< data used to fit the random forest
  k, ##<< which tree
  ylabel = "Survived",
  returnNodeInfo = FALSE ##<< return node info as well ?
){
  ### Important: we assign the CFC duue to a split to the child instead of the parent !!
  if (!is.numeric(train_data[,ylabel])) {
    train_data[,ylabel] = as.numeric(train_data[,ylabel])
    train_data[,ylabel] = train_data[,ylabel]-min(train_data[,ylabel])
    #print(unique(train_data[,ylabel]))
  }
  bags <- InOutBags(RF, train_data, k)
  InTree = GiniImportanceTree(bags$inbag,RF,k,Predictor=mean,ylabel=ylabel,returnTree=TRUE) 
  n_nodes = nrow(InTree)
  
  #NodeInfo =data.frame(nodeID = 1:n_nodes, parentID = NA)
  NodeInfo = InTree[,c("left daughter", "right daughter", "split var", "split point", "status", "yHat", "gini_index",  "IG_gini")]
  colnames(NodeInfo)[1:6] = c("leftChild", "rightChild","splitvarName", "splitval","terminal",     "prediction")
  NodeInfo$splitvarName = as.character(NodeInfo$splitvarName)
  NodeInfo$terminal = ifelse(NodeInfo$terminal == -1, TRUE, FALSE)
  NodeInfo$parentID = NodeInfo$CFC = NodeInfo$deltaGini =NA
  NodeInfo$parent_splitvarName =""
  
  for (i_node in 1:n_nodes){#assign parent ID:
    if (!NodeInfo$terminal[i_node]) {
      NodeInfo$parentID[NodeInfo$leftChild[i_node]] = i_node
      NodeInfo$parentID[NodeInfo$rightChild[i_node]] = i_node
      NodeInfo$parent_splitvarName[NodeInfo$leftChild[i_node]]=NodeInfo$splitvarName[i_node]
      NodeInfo$parent_splitvarName[NodeInfo$rightChild[i_node]]=NodeInfo$splitvarName[i_node]
    }
  }
  for (i_node in 2:n_nodes){
    parentNode=NodeInfo$parentID[i_node]
    NodeInfo$CFC[i_node] = NodeInfo$prediction[i_node]-NodeInfo$prediction[parentNode]
    NodeInfo$deltaGini[i_node] = NodeInfo$gini_index[parentNode]-NodeInfo$gini_index[i_node]
  }
  
  NodeInfo = NodeInfo[,c("leftChild", "rightChild","splitvarName","parentID","parent_splitvarName", "splitval","terminal",     "prediction", "CFC", "gini_index", "deltaGini", "IG_gini")]
  
  TerminalNodes=which(NodeInfo$terminal)
  decision_paths = as.list(TerminalNodes)
  names(decision_paths) = as.character(TerminalNodes)
  for (i_node in TerminalNodes){
    parentNode=NodeInfo$parentID[i_node]
    pNodes = as.numeric(decision_paths[as.character(i_node)])
    while(!is.na(parentNode)){
      pNodes=c(pNodes,parentNode)
      parentNode=NodeInfo$parentID[parentNode]
    }
    #print(pNodes)
    #browser()
    decision_paths[[as.character(i_node)]]=pNodes
  }
  #subset(NodeInfo, terminal)
  #TerminalNodeInfo
  if (returnNodeInfo)
    return(list(NodeInfo=NodeInfo, decision_paths=decision_paths))
  
  return(decision_paths)
}

# ComputeCFC_Forest = function(#also computes local impurity defined in 
#   TerminalNodes, ##<< terminal nodes from data traversing this single tree; typically an inbag from the random forest
#   NodeInfo, ##<< object returned by call to GetNodeInfo_SingleTree() 
#   decision_paths, ##<< object returned by call to GetNodeInfo_SingleTree()
#   uniqNodes = FALSE ##<< return only the lookup table for the unique terminal nodes
# ){
#   
#   if ("randomForest" %in% class(RF)){
#     tree = as.data.frame(randomForest::getTree(RF, k, labelVar = TRUE))
#     attr(tree, "rflib") = "randomForest"
#   } else if ("ranger" %in% class(RF)) {
#     tree = as.data.frame(getTree_ranger(RF, k))
#     attr(tree, "rflib") = "ranger"
#   }
# }

ComputeCFC_SingleTree = function(#also computes local impurity defined in 
  TerminalNodes, ##<< terminal nodes from data traversing this single tree; typically an inbag from the random forest
  NodeInfo, ##<< object returned by call to GetNodeInfo_SingleTree() 
  decision_paths, ##<< object returned by call to GetNodeInfo_SingleTree()
  uniqNodes = FALSE ##<< return only the lookup table for the unique terminal nodes
){
  TerminalNodes_full = TerminalNodes#we will later return the full table
  TerminalNodes = unique(TerminalNodes)#more efficient !
  N = length(TerminalNodes)
  
  features = as.character(na.omit(unique(NodeInfo$splitvarName)))
  p=length(features)
  CFC = localGini = matrix(0, ncol=p,nrow=N, dimnames=list(TerminalNodes,features))
  
  for (i_node in as.character(TerminalNodes)){
    pNodes = decision_paths[[i_node]]
    pNodes = pNodes[-length(pNodes)]#the last node is always the root node
    for (j_node in pNodes){
      ftr = NodeInfo[j_node, "parent_splitvarName"]
      if (ftr %in% features) {
        CFC[i_node, ftr] = CFC[i_node, ftr] + NodeInfo[j_node, "CFC"]
        localGini[i_node, ftr] = localGini[i_node, ftr] + NodeInfo[j_node, "deltaGini"]
      } else print(ftr)
      
      #we assigned the CFC due to a split to the child instead of the parent !!
    }
  }
  
  if (uniqNodes)
    return(list(CFC=CFC,localGini=localGini))
  
  CFC_full = CFC[as.character(TerminalNodes_full),]
  localGini_full = localGini[as.character(TerminalNodes_full),]
  
  return(list(CFC=CFC_full,localGini=localGini_full))
}

guidedSample = function(data2sample, template,n=1,indicesOnly=FALSE){
  ftrs =colnames(template)
  data2sample$rowNum = 1:nrow(data2sample)
  data2sample$key = apply(data2sample[,ftrs], 1, paste, collapse="-")
  y=list()
  
  for (i in 1:nrow(template)){
    x =subset(data2sample, data2sample$key == paste(template[i,], collapse="-"))
    
    y[[i]] =x[sample(nrow(x),n),,drop=FALSE]
  }
  
  y = do.call("rbind.data.frame",y)
  if (indicesOnly)
    return(y$rowNum)
  
  return(y)
}

if(0){
  TreeNodes = decision_path_SingleTree(RF,train_data,k=1,returnNodeInfo=TRUE)
  NodeInfo=TreeNodes$NodeInfo
  decision_paths=TreeNodes$decision_paths
  
  RF_preds = predict(RF, titanic_train[1:10,], predict.all = TRUE, nodes = TRUE)
  TerminalNodes_Forest = attr(RF_preds, "nodes")
  
  CFC_localGini = ComputeCFC_SingleTree(TerminalNodes_Forest[,1],NodeInfo,decision_paths)
  View(CFC_localGini$CFC)
}
