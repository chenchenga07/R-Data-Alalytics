library(flexclust)

city = c("NYC","NYC","Los Angeles","Jersey City")
income = c(20,16,10,4)
rating = c(6,2,3,2)
test = cbind(city,income,rating)
str(test)


distance = function(x,y) {
  if (x[1] == y[1])
    d1 = 0
  else
    d1 = 1
  
  d2 = abs(as.numeric(x[2])-as.numeric(y[2]))
  d3 = abs(as.numeric(x[3])-as.numeric(y[3]))
    
  dis = d1 + d2 + d3
  
  return(dis)
}

distance(test[1,], test[2,])
distance(test[1,], test[3,])


distanceMatrix = function(df1,df2) {
  m = matrix(rep(NA,nrow(df1)*nrow(df2)), nrow=nrow(df1), ncol=nrow(df2))
  for (i in 1:nrow(df1)){
    for (j in 1:nrow(df2)){
      m[i,j] = distance(df1[i,],df2[j,])
    }
  }
  return(m)
}
  
distanceMatrix(test, test)


centroid = function(df) {
  city = tail(names(sort(table(df[,1]))), 1)
  income = mean(as.numeric(df[,2]))
  rating = mean(as.numeric(df[,3]))
  p = cbind(city,income,rating)
  return(p)
}

centroid(test)



cheng = kccaFamily(which=NULL, dist=distanceMatrix, cent=centroid, name="cheng",
           preproc = NULL, trim=0, groupFun = "minSumClusters")
k = kcca(test, 2, family=cheng, weights=NULL, group=NULL,
     control=NULL, simple=FALSE, save.data=FALSE)
summary(k)
clusters(k)
