library(flexclust)

# create sample data as matrix
# cause one of the columns are string, all numbers are stored as strings
# the result is a string matrix
city = c("NYC","NYC","Los Angeles","Jersey City")
income = c(20,16,10,4)
rating = c(6,2,3,2)
test = cbind(city,income,rating)
str(test)

# define distance function which calculate distance between 2 observations

# if "city"(column 1) is different, return 1, else return 0
# other columns' distance is calculated by absloute difference of 2 numbers
# the total distance is sum of all 3 columns' distances

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


# create pair-wise distance matrix
distanceMatrix = function(m1,m2) {
  m = matrix(
    rep(NA,nrow(m1)*nrow(m2)), 
    nrow=nrow(m1), 
    ncol=nrow(m2)
  )
  
  for (i in 1:nrow(m1)){
    for (j in 1:nrow(m2)){
      m[i,j] = distance(m1[i,],m2[j,])
    }
  }
  
  return(m)
}
  
distanceMatrix(test, test)


# define cluster center
# pick the most frequently showed city as center
# calculate mean of income and rating as center
centroid = function(m) {
  city = tail(names(sort(table(m[,1]))), 1)
  income = mean(as.numeric(m[,2]))
  rating = mean(as.numeric(m[,3]))
  p = cbind(city,income,rating)
  return(p)
}

centroid(test)


# define kccaFamily with previous self-built functions
cheng = kccaFamily(which=NULL, dist=distanceMatrix, cent=centroid, name="cheng",
           preproc = NULL, trim=0, groupFun = "minSumClusters")

# run k-means clustering using the customized cheng function
k = kcca(test, 2, family=cheng, weights=NULL, group=NULL,
     control=NULL, simple=FALSE, save.data=FALSE)

# check how many clusters
summary(k)

# check cluster assignment of each observations
clusters(k)
