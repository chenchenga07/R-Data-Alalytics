setwd("/Users/chencheng/Desktop/R Review/Github/R")

### load and prepare the data
customerData=read.csv("./Bank/Customer Database.csv", stringsAsFactors=T)
str(customerData)

# change variable zip_code, county_code and occupation from integer to factor
customerData[,1] = as.factor(customerData[,1]) # zip_code
customerData[,2] = as.factor(customerData[,2]) # country_code
customerData[,12] = as.factor(customerData[,12]) # occupation
summary(customerData)
save.image(file = "Bank.RData")

customerDataPrep = customerData

# drop variables not used in the following segmentation modeling
drop = c("zip_code","county_code","county_name","income","occupation",
         "child0_3","child4_6","child7_9","child10_11","child13_18","home_owner","dwelling_type",
         "great_outdoors","sporting_life","health_fitness","luxury_life","doit_yourselfer","truck_owner","motor_cycle","sports","entertainment_enth","hobbyists",
         "avid_readers","collectors","travel","pets","music","toys","art_craft","gardening","family","food","cars","casino_gambling","contributors",
         "travel_business","travel_personal","travel_vacation","donor_arts_or_cultural",
         "AAPPAREL","ACASHCONT","AEDUCATN","AENTRTAIN","AFOODTOTL","AGTOTALE","AHEALTH","AHOUSING","AINSPENSN","AREADING","ATRANS")

customerDataPrep = customerDataPrep[,!(names(customerDataPrep) %in% drop)]

# replace year_structure NA with median 
customerDataPrep$year_structure = 
  ifelse(is.na(customerDataPrep$year_structure), median(customerDataPrep$year_structure,na.rm = T), 
         customerDataPrep$year_structure)

# replace NA with 0 in some columns
c = c("education_code","children","contributor_index")
customerDataPrep[c][is.na(customerDataPrep[c])] = 0
summary(customerDataPrep)

# drop NA records
customerDataPrep = customerDataPrep[complete.cases(customerDataPrep),]


# rescale all numeric variables
rescale = function(r) {
  if (sd(r) != 0) 
    res = (r - mean(r))/sd(r) else res = 0 * r
    return(res)
}

numericVariablesScaled = apply(customerDataPrep[,5:15],2,rescale)
customerDataPrep = cbind(customerDataPrep[,1:4],numericVariablesScaled)

save.image(file="Bank.Rdata")


### convert multiple numeric variables into a smaller group of components/factors for the following modeling

# approach 1: principal component analysis (PCA)
pca.out = prcomp(numericVariablesScaled,scale=T)
print(pca.out)
summary(pca.out) 
# check cucumlative proportion
# first 5 components explain around 70% of variances
# PC1 highly related with num_in_hhld(-), num_of_children(-)
# PC2 highly related with age, len_of_residence
# PC3 highly related with contributor_index(-), education_code(-)
# PC4 highly related with year_structure, median_home_value(-)
# PC5 highly related with median_income(-)

plot(pca.out) 
# check how many components' variances are above 1 / has big drop

pca.var = pca.out$sdev^2 # create variances from sd
pca.pve = pca.var/sum(pca.var)
plot(pca.pve)
plot(cumsum(pca.pve)) # visualize

# approach 2: factor analysis
library(GPArotation)
library(psych)
fa.parallel(numericVariablesScaled,fa="fa",n.iter=30,main="Scree plots with parallel analysis")
# sharp breaks in the plot suggest the appropriate number of factors to extract
# parallel analysis suggests that the number of factors =  5 

fa1 = factanal(numericVariablesScaled,factors = 5,rotation = "varimax", scores = "regression")
fa1
head(fa1$scores)
# factor1 highly related with num_of_children, children, num_in_hhld
# factor2 highly related with num_of_adults, num_in_hhld
# factor3 highly related with age, education_code
# factor4 highly related with contributor_index
# factor5 highly related with median_home_value


# combine string variables with first 5 components together for kcca clustering
customerDataPrep = as.matrix(cbind(customerDataPrep[,1:4],pca.out$x[,1:5]))

# define distance function which calculate distance between 2 observations

# if "city"(column 1) is different, return 1, else return 0
# if "state"(column 2) is different, return 1, else return 0
# if "gender"(column 3) is different, return 1, else return 0, 
#   for any comparations with NA records, return 0.5
# if "marital_status"(column 4) is different, return 1, else return 0
#   for any comparations with NA records, return 0.5

# other columns' distance is calculated by absloute difference of 2 numbers
# the total distance is sum of all columns' distances

distance = function(x,y) {
  if (x[1] == y[1] & x[2] == y[2])
    d1 = 0
  else if (x[1] != y[1] & x[2] == y[2])
    d1 = 0.5
  else
    d1 = 1
  
  if (x[3] == '') 
    d3 = 0.5
  else if (y[3] == '')
    d3 = 0.5
  else if (x[3] == y[3])
    d3 = 0
  else
    d3 = 1
  
  if (x[4] == '') 
    d4 = 0.5
  else if (y[4] == '')
    d4 = 0.5
  else if (x[4] == y[4])
    d4 = 0
  else
    d4 = 1
  
  d5 = abs(as.numeric(x[5])-as.numeric(y[5]))
  d6 = abs(as.numeric(x[6])-as.numeric(y[6]))
  d7 = abs(as.numeric(x[7])-as.numeric(y[7]))
  d8 = abs(as.numeric(x[8])-as.numeric(y[8]))
  d9 = abs(as.numeric(x[9])-as.numeric(y[9]))
  
  dis = d1 + d3 + d4 + d5 + d6 + d7 + d8 + d9
  return(dis)
}

# test
distance(customerDataPrep[1,], customerDataPrep[2,])
distance(customerDataPrep[34,], customerDataPrep[34,])
distance(customerDataPrep[43,], customerDataPrep[43,])


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

# test
distanceMatrix(customerDataPrep[1:2,], customerDataPrep[1:2,])


# define cluster center
# pick the most frequently showed city/state/gender/marital_status as center
# calculate mean of other numeric variables as center
centroid = function(m) {
  city = tail(names(sort(table(m[,1]))), 1)
  state = tail(names(sort(table(m[,2]))), 1)
  gender = tail(names(sort(table(m[,3]))), 1)
  marital_status = tail(names(sort(table(m[,4]))), 1)
  PC1 = mean(as.numeric(m[,5]))
  PC2 = mean(as.numeric(m[,6]))
  PC3 = mean(as.numeric(m[,7]))
  PC4 = mean(as.numeric(m[,8]))
  PC5 = mean(as.numeric(m[,9]))
  
  p = cbind(city,state,gender,marital_status,PC1,PC2,PC3,PC4,PC5)
  return(p)
}

#test
centroid(customerDataPrep[1:2,])


# define kccaFamily with previous self-built functions
cheng = kccaFamily(which=NULL, dist=distanceMatrix, cent=centroid, name="cheng",
                   preproc = NULL, trim=0, groupFun = "minSumClusters")

# run k-means clustering using the customized cheng function
k = kcca(customerDataPrep, 3, family=cheng, weights=NULL, group=NULL,
         control=NULL, simple=FALSE, save.data=FALSE)
k
k@centers
summary(k)



# bring cluster NO. back to the complete data table to check each segment's traits and preferences
customerData$clusterNo = clusters(k)
summary(customerData)
write.csv(x=customerData,file = "./Bank/customerDataWithCluster.csv",row.names = TRUE)

segmentation1 = subset(customerData,customerData$clusterNo == 1)
segmentation2 = subset(customerData,customerData$clusterNo == 2)
segmentation3 = subset(customerData,customerData$clusterNo == 3)
summary(segmentation1)
summary(segmentation2)
summary(segmentation3)

# check customers' interest (percentage)
a = c("great_outdoors","sporting_life","health_fitness","luxury_life","doit_yourselfer","truck_owner","motor_cycle","sports","entertainment_enth","hobbyists",
      "avid_readers","collectors","travel","pets","music","toys","art_craft","gardening","family","food","cars","casino_gambling","contributors",
      "travel_business","travel_personal","travel_vacation","donor_arts_or_cultural")
segment1 = rep(NA,27)
segment2 = rep(NA,27)
segment3 = rep(NA,27)
preference = data.frame(row.names=a,segment1,segment2,segment3)

#segment1
for (i in 27:53) {
  preference[i-26,1]=nrow(subset(segmentation1,segmentation1[,i] == 1))/nrow(segmentation1)}
#segment2
for (i in 27:53) {
  preference[i-26,2]=nrow(subset(segmentation2,segmentation2[,i] == 1))/nrow(segmentation2)}
#segment3
for (i in 27:53) {
  preference[i-26,3]=nrow(subset(segmentation3,segmentation3[,i] == 1))/nrow(segmentation3)}

View(preference)


# check income allocation (average)
b = c("AAPPAREL","ACASHCONT","AEDUCATN","AENTRTAIN","AFOODTOTL","AGTOTALE","AHEALTH","AHOUSING","AINSPENSN","AREADING","ATRANS")
segment_1 = rep(NA,11)
segment_2 = rep(NA,11)
segment_3 = rep(NA,11)

incomeAllocation = data.frame(row.names=b,segment_1,segment_2,segment_3)

#segment1
for (i in 56:66) {
  incomeAllocation[i-55,1]=sum(segmentation1[i],na.rm=T)/nrow(segmentation1)}
#segment2
for (i in 56:66) {
  incomeAllocation[i-55,2]=sum(segmentation2[i],na.rm=T)/nrow(segmentation2)}
#segment3
for (i in 56:66) {
  incomeAllocation[i-55,3]=sum(segmentation3[i],na.rm=T)/nrow(segmentation3)}

View(incomeAllocation)


sum(incomeAllocation$segment_1) # 67830.1
sum(incomeAllocation$segment_2) # 66028.58
sum(incomeAllocation$segment_3) # 67268.09





# ????????????????????????????
# estimate market size
randomData=read.csv("./Bank/Random Database.csv", stringsAsFactors=T)
randomData$clusterNo = km2$cluster
segmentation1MktShare = nrow(subset(randomPopulationData,randomPopulationData$clusterNo == 1))/nrow(randomPopulationData)
# 0.419
segmentation2MktShare = nrow(subset(randomPopulationData,randomPopulationData$clusterNo == 2))/nrow(randomPopulationData)
# 0.43065
segmentation3MktShare = nrow(subset(randomPopulationData,randomPopulationData$clusterNo == 3))/nrow(randomPopulationData)
# 0.15035
