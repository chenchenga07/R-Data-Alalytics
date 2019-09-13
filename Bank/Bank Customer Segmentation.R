setwd("/Users/chencheng/Desktop/R Review/Github/R")

# load the data
customerData=read.csv("./Bank/Customer Database.csv", stringsAsFactors=T)
str(customerData)

# change variable zip_code and county_code from integer to factor
customerDataPrep = customerData
customerDataPrep[,1] = as.factor(customerDataPrep[,1])
customerDataPrep[,2] = as.factor(customerDataPrep[,2])

# change categorical variables into numeric
library(dummies)
customerDataPrep = dummy.data.frame(customerDataPrep, 
                                 names = c("gender","marital_status",
                                          "income","occupation","home_owner",
                                          "dwelling_type"))

# drop variables not used in the following segmentation modeling
drop = c("zip_code","county_code","city","state","county_name")
customerDataPrep = customerDataPrep[,!(names(customerDataPrep) %in% drop)]
str(customerDataPrep)

# replace year_structure NA with median 
customerDataPrep$year_structure = 
  ifelse(is.na(customerDataPrep$year_structure), median(customerDataPrep$year_structure,na.rm = T), 
         customerDataPrep$year_structure)


# replace NA with 0 in some columns
c = c("child0_3","child4_6","child7_9","child10_11","child13_18","children",
      "great_outdoors","sporting_life","health_fitness","luxury_life","doit_yourselfer","truck_owner",
      "motor_cycle","sports","entertainment_enth","hobbyists","avid_readers","collectors","travel","pets","music",
      "toys","art_craft","gardening","family","food","cars","casino_gambling","contributors","travel_business",
      "travel_personal","travel_vacation","donor_arts_or_cultural")

customerDataPrep[c][is.na(customerDataPrep[c])] = 0

# or
# for (i in c){
#    customerDataPrep[i][is.na(customerDataPrep[i])] = 0
# }


# drop NA records
customerDataPrep = customerDataPrep[complete.cases(customerDataPrep),]


# rescale data
rescale = function(r) {
  if (sd(r) != 0) 
    res = (r - mean(r))/sd(r) else res = 0 * r
  return(res)
}

segmentDataScaled = apply(customerDataPrep,2,rescale)


# converting multiple variables into a smaller group of components/factors for the following modeling

# approach 1: principal component analysis (PCA)
pca.out = prcomp(segmentDataScaled,scale=T)
print(pca.out)
summary(pca.out) # check cucumlative proportion 
plot(pca.out) 
# check how many components' variances are above 1 / has big drop

pca.var = pca.out$sdev^2 # create variances from sd
pca.pve = pca.var/sum(pca.var)
plot(pca.pve)
plot(cumsum(pca.pve)) # visualize

# approach 2: factor analysis
library(GPArotation)
library(psych)
fa.parallel(segmentDataScaled,fa="fa",n.iter=100,main="Scree plots with parallel analysis")
f1 = fa(segmentDataScaled,rotate="Varimax",nfactors=15)
f1


# k-means segmentation 

# define how many clusters should be used
# pamk() or wss approach
library(cluster) 
library(fpc)
set.seed(123)

wss <- (nrow(segmentDataScaled)-1)*sum(apply(segmentDataScaled,2,var))
for (i in 1:127) wss[i] <- sum(kmeans(segmentDataScaled,centers=i)$withinss)
plot(1:127, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
#found 2,3,4 of clusters maybe used in segmentation


# use first X principal components to do the clustering


#plot
km1 = kmeans(segmentDataScaled,2,iter.max = 20, nstart=2)
km2 = kmeans(segmentDataScaled,3,iter.max = 20, nstart=2)
km3 = kmeans(segmentDataScaled,4,iter.max = 20, nstart=2)

clusplot(segmentDataScaled, km1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(segmentDataScaled, km1$cluster)
clusplot(segmentDataScaled, km2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(segmentDataScaled, km2$cluster)# the clearest one
clusplot(segmentDataScaled, km3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(segmentDataScaled, km3$cluster)

percsize = paste(1:3," = ",format(km2$size/sum(km2$size)*100,digits=2),"%",sep="")
pie(km2$size,labels=percsize)

km2$centers

age1 = 0.3161951*((max(segmentData$age)-min(segmentData$age))) + min(segmentData$age)
age1#48.06166
age2 = 0.3173620*((max(segmentData$age)-min(segmentData$age))) + min(segmentData$age)
age2#48.23902
age3 = 0.2879400*((max(segmentData$age)-min(segmentData$age))) + min(segmentData$age)
age3#43.76688
Sage = c(age1,age2,age3)

median_income1 = 0.3815172*((max(segmentData$median_income)-min(segmentData$median_income))) + min(segmentData$median_income)
median_income1#97.28689
median_income2 = 0.6873246*((max(segmentData$median_income)-min(segmentData$median_income))) + min(segmentData$median_income)
median_income2#175.2678
median_income3 = 0.9355699*((max(segmentData$median_income)-min(segmentData$median_income))) + min(segmentData$median_income)
median_income3#238.5703
Smedian_income = c(median_income1,median_income2,median_income3)

num_in_hhld1 = 0.4622212*((max(segmentData$num_in_hhld)-min(segmentData$num_in_hhld))) + min(segmentData$num_in_hhld)
num_in_hhld1#4.159991
num_in_hhld2 = 0.4197030*((max(segmentData$num_in_hhld)-min(segmentData$num_in_hhld))) + min(segmentData$num_in_hhld)
num_in_hhld2#3.777327
num_in_hhld3 = 0.3192551*((max(segmentData$num_in_hhld)-min(segmentData$num_in_hhld))) + min(segmentData$num_in_hhld)
num_in_hhld3#2.873296
Snum_in_hhld = c(num_in_hhld1,num_in_hhld2,num_in_hhld3)

num_of_adults1 = 0.4230316*((max(segmentData$num_of_adults)-min(segmentData$num_of_adults))) + min(segmentData$num_of_adults)
num_of_adults1#2.961221
num_of_adults2 = 0.4043300*((max(segmentData$num_of_adults)-min(segmentData$num_of_adults))) + min(segmentData$num_of_adults)
num_of_adults2#2.83031
num_of_adults3 = 0.3588769*((max(segmentData$num_of_adults)-min(segmentData$num_of_adults))) + min(segmentData$num_of_adults)
num_of_adults3#2.512138
Snum_of_adults = c(num_of_adults1,num_of_adults2,num_of_adults3)

num_of_children1 = 0.14077557*((max(segmentData$num_of_children)-min(segmentData$num_of_children))) + min(segmentData$num_of_children)
num_of_children1#1.126205
num_of_children2 = 0.10851730*((max(segmentData$num_of_children)-min(segmentData$num_of_children))) + min(segmentData$num_of_children)
num_of_children2#0.8681384
num_of_children3 = 0.08426172*((max(segmentData$num_of_children)-min(segmentData$num_of_children))) + min(segmentData$num_of_children)
num_of_children3#0.6740938
Snum_of_children = c(num_of_children1,num_of_children2,num_of_children3)

median_home_value1 = 0.48373080*((max(segmentData$median_home_value)-min(segmentData$median_home_value))) + min(segmentData$median_home_value)
median_home_value1#483.2471
median_home_value2 = 0.87317341*((max(segmentData$median_home_value)-min(segmentData$median_home_value))) + min(segmentData$median_home_value)
median_home_value2#872.3002
median_home_value3 = 0.06725415*((max(segmentData$median_home_value)-min(segmentData$median_home_value))) + min(segmentData$median_home_value)
median_home_value3#67.1869
Smedian_home_value = c(median_home_value1,median_home_value2,median_home_value3)

len_of_residence1 = 0.1918611*((max(segmentData$len_of_residence)-min(segmentData$len_of_residence))) + min(segmentData$len_of_residence)
len_of_residence1#9.209333
len_of_residence2 = 0.1970838*((max(segmentData$len_of_residence)-min(segmentData$len_of_residence))) + min(segmentData$len_of_residence)
len_of_residence2#9.460022
len_of_residence3 = 0.1578608*((max(segmentData$len_of_residence)-min(segmentData$len_of_residence))) + min(segmentData$len_of_residence)
len_of_residence3#7.577318
Slen_of_residence = c(len_of_residence1,len_of_residence2,len_of_residence3)

symmarySegmentData = data.frame(Sage,Smedian_income,Snum_in_hhld,Snum_of_adults,Snum_of_children,Smedian_home_value,Slen_of_residence)

# more segmentation insights
# preferences
existingCustomerData$clusterNo = km2$cluster
write.csv(x=existingCustomerData,file = "/Users/chencheng/Documents/Rochester/Spring/MKT Projects/Data/existingSegments.csv",row.names = TRUE)

segmentation1 = subset(existingCustomerData,existingCustomerData$clusterNo == 1)
segmentation2 = subset(existingCustomerData,existingCustomerData$clusterNo == 2)
segmentation3 = subset(existingCustomerData,existingCustomerData$clusterNo == 3)

a=c("great_outdoors","sporting_life","health_fitness","luxury_life","doit_yourselfer","truck_owner","motor_cycle","sports","entertainment_enth","hobbyists",
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

# income allocation
b=c("AAPPAREL","ACASHCONT","AEDUCATN","AENTRTAIN","AFOODTOTL","AGTOTALE","AHEALTH","AHOUSING","AINSPENSN","AREADING","ATRANS")
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

write.csv(x=incomeAllocation,file = "/Users/chencheng/Documents/Rochester/Spring/MKT Projects/Data/incomeAllocation.csv",row.names = TRUE)

sum(incomeAllocation$segment_1)#67065.17
sum(incomeAllocation$segment_2)#72005
sum(incomeAllocation$segment_3)#68015.33

save.image(file="Redo Cheng Chen.RData")

# estimate market size
randomPopulationData$clusterNo = km2$cluster
segmentation1MktShare = nrow(subset(randomPopulationData,randomPopulationData$clusterNo == 1))/nrow(randomPopulationData)
# 0.419
segmentation2MktShare = nrow(subset(randomPopulationData,randomPopulationData$clusterNo == 2))/nrow(randomPopulationData)
# 0.43065
segmentation3MktShare = nrow(subset(randomPopulationData,randomPopulationData$clusterNo == 3))/nrow(randomPopulationData)
# 0.15035