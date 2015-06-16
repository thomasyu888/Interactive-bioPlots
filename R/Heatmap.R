
HCtoJSON<-function(hc){

  labels<-hc$labels
  merge<-data.frame(hc$merge)

  for (i in (1:nrow(merge))) {

    if (merge[i,1]<0 & merge[i,2]<0) {
      eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]),list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]>0 & merge[i,2]<0) {
      eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node", merge[i,1], ", list(name=labels[-merge[i,2]])))")))}
    else if (merge[i,1]<0 & merge[i,2]>0) {
      eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]), node", merge[i,2],"))")))}
    else if (merge[i,1]>0 & merge[i,2]>0) {
      eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node",merge[i,1] , ", node" , merge[i,2]," ))")))}
  }

  eval(parse(text=paste0("JSON<-(node",nrow(merge), ")")))

  return(JSON)
}


cluster_mat = function(mat, distance, method, cor_method){
  #if(!(method %in% c("ward.D","ward.D2","single", "complete", "average", "mcquitty", "median", "centroid"))){
  #  stop("clustering method has to one form the list: 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid'.")
  #}
  if(!(method %in% c("ward","single", "complete", "average", "mcquitty", "median", "centroid"))){
    stop("clustering method has to one form the list: 'ward', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid'.")
  }
  if(!(distance %in% c("correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) & class(distance) != "dist"){
    print(!(distance %in% c("correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) | class(distance) != "dist")
    stop("distance has to be a dissimilarity structure as produced by dist or one measure  form the list: 'correlation', 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski'")
  }
  if (distance == "correlation"){
    d = as.dist(1 - cor(t(mat),method=cor_method))
  }
  else {
    if(class(distance) == "dist") {
      d = distance
    }
    else{
      d = dist(mat, method = distance)
    }
  }
  #ward.D2 doesn't work in flashClust
  return(flashClust(d, method = method)) #hclust replaced by flashClust from WCGNA (much faster than hclust)
  #return (hclust(d,method=method))
}
