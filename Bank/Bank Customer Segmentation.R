setwd("/Users/chencheng/Desktop/R Review/Github/R/Bank")

### load and prepare the data
customerData=read.csv("./Customer Database.csv", stringsAsFactors=T)
str(customerData)

# change variable zip_code, county_code and occupation from integer to factor
customerDataPrep = customerData
customerDataPrep[,1] = as.factor(customerDataPrep[,1]) # zip_code
customerDataPrep[,2] = as.factor(customerDataPrep[,2]) # country_code
customerDataPrep[,12] = as.factor(customerDataPrep[,12]) # occupation

# change categorical variables into numeric
library(dummies)
customerDataPrep = dummy.data.frame(customerDataPrep, names = c("gender","marital_status"))

# drop variables not used in the following segmentation modeling
drop = c("zip_code","county_code","city","state","county_name","gender","marital_status","income","occupation",
         "child0_3","child4_6","child7_9","child10_12","child13_18","home_owner","dwelling_type",
         "great_outdoors","sporting_life","health_fitness","luxury_life","doit_yourselfer","truck_owner","motor_cycle","sports","entertainment_enth","hobbyists",
         "avid_readers","collectors","travel","pets","music","toys","art_craft","gardening","family","food","cars","casino_gambling","contributors",
         "travel_business","travel_personal","travel_vacation","donor_arts_or_cultural",
         "AAPPAREL","ACASHCONT","AEDUCATN","AENTRTAIN","AFOODTOTL","AGTOTALE","AHEALTH","AHOUSING","AINSPENSN","AREADING","ATRANS")

customerDataPrep = customerDataPrep[,!(names(customerDataPrep) %in% drop)]

# replace year_structure NA with median 
customerDataPrep$year_structure = 
  ifelse(is.na(customerDataPrep$year_structure), median(customerDataPrep$year_structure,na.rm = T), 
         customerDataPrep$year_structure)

summary(customerDataPrep)

# replace NA with 0 in some columns
c = c("education_code","children","contributor_index")
customerDataPrep[c][is.na(customerDataPrep[c])] = 0
summary(customerDataPrep)

# drop NA records
customerDataPrep = customerDataPrep[complete.cases(customerDataPrep),]


# rescale data
rescale = function(r) {
  if (sd(r) != 0) 
    res = (r - mean(r))/sd(r) else res = 0 * r
    return(res)
}

segmentDataScaled = apply(customerDataPrep,2,rescale)
ncol(segmentDataScaled)
# 15 variables
save.image(file="Bank.Rdata")


### convert multiple variables into a smaller group of components/factors for the following modeling

# approach 1: principal component analysis (PCA)
pca.out = prcomp(segmentDataScaled,scale=T)
print(pca.out)
summary(pca.out) 
# check cucumlative proportion
# first 6 components explain around 70% of variances
# PC1 highly related with num_in_hhld, marital_statusM
# PC2 highly related with genderM
# PC3 highly related with num_of_children(-), children(-)
# PC4 highly related with education_code, age
# PC5 highly related with year_structure(-), median_home_value
# PC6 highly related with median_income, age


plot(pca.out) 
# check how many components' variances are above 1 / has big drop

pca.var = pca.out$sdev^2 # create variances from sd
pca.pve = pca.var/sum(pca.var)
plot(pca.pve)
plot(cumsum(pca.pve)) # visualize

# approach 2: factor analysis
library(GPArotation)
library(psych)
fa.parallel(segmentDataScaled,fa="fa",n.iter=30,main="Scree plots with parallel analysis")
# sharp breaks in the plot suggest the appropriate number of factors to extract
f1 = fa(segmentDataScaled,rotate="Varimax",nfactors=2)
plot(f1)

fa1 = factanal(segmentDataScaled,factors = 2,rotation = "varimax", scores = "regression")
fa1
head(fa1$scores)
# factor1 highly related with num_in_hhld, num_of_adults, num_of_children
# factor2 highly related with genderM, genderF(-)



# k-means segmentation 

# define how many clusters should be used

# approach 1: pamk()
pm = pamk(segmentDataScaled,scaling=T, usepam = F, criterion="multiasw")
pm$nc
# 2

# approach 2: wss plot 
library(cluster) 
library(fpc)
set.seed(123)

wss = (nrow(segmentDataScaled)-1)*sum(apply(segmentDataScaled,2,var))
for (i in 1:ncol(segmentDataScaled)) wss[i] = sum(kmeans(segmentDataScaled,centers=i)$withinss)
plot(1:ncol(segmentDataScaled), wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#found 2,3,4 clusters maybe suitable in segmentation


# use first 6 principal components to do the clustering

#plot
km1 = kmeans(pca.out$x[,1:6],2,iter.max = 20, nstart=2)
# first 6 components, 2 clusters
km2 = kmeans(pca.out$x[,1:6],3,iter.max = 20, nstart=2)
km3 = kmeans(pca.out$x[,1:6],4,iter.max = 20, nstart=2)

clusplot(pca.out$x[,1:6], km1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(pca.out$x[,1:6], km1$cluster)
clusplot(pca.out$x[,1:6], km2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(pca.out$x[,1:6], km2$cluster)# the clearest one
clusplot(pca.out$x[,1:6], km3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(pca.out$x[,1:6], km3$cluster)

percsize = paste(1:3," = ",format(km2$size/sum(km2$size)*100,digits=2),"%",sep="")
pie(km2$size,labels=percsize)
# segment 1 consists 38% of total records
# segment 2 consists 24% of total records
# segment 3 consists 38% of total records

km2$centers
scaledCenters = km2$centers

# scale back cluster centers
options(scipen = 999)
for (i in 1:ncol(scaledCenters)){
  Centers[,i] = sd(customerDataPrep[,i])*scaledCenters[,i] + mean(customerDataPrep[,i])
}
View(Centers)



# go back to the complete data table to check each segment's traits and preferences
customerData$clusterNo = km2$cluster

write.csv(x=customerData,file = "./customerDataWithCluster.csv",row.names = TRUE)


segmentation1 = subset(customerData,customerData$clusterNo == 1)
segmentation2 = subset(customerData,customerData$clusterNo == 2)
segmentation3 = subset(customerData,customerData$clusterNo == 3)

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


sum(incomeAllocation$segment_1) # 67517.75
sum(incomeAllocation$segment_2) # 66179.98
sum(incomeAllocation$segment_3) # 67439.29





# prediction

randomData=read.csv("./Bank/Random Database.csv", stringsAsFactors=T)
# should prepare data set into the same format with pca.out first

randomData$pred_cluster = predict(km2, newdata=randomData)

# estimate market size
segmentation1MktShare = nrow(subset(randomData,randomData$clusterNo == 1))/nrow(randomData)
# 0.419
segmentation2MktShare = nrow(subset(randomData,randomData$clusterNo == 2))/nrow(randomData)
# 0.43065
segmentation3MktShare = nrow(subset(randomData,randomData$clusterNo == 3))/nrow(randomData)
# 0.15035
