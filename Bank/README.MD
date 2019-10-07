# Bank Existing Customer Segmentation Analysis

Use a Bank's 20,000 exisiting customers' demographic and interest data to do a segmentation analysis. Providing insights to the bank's new trust product design.

## Part 1: Prepare the Data

### load data source
```
setwd("/Users/chencheng/Desktop/R Review/Github/R/Bank")
customerData=read.csv("./Customer Database.csv", stringsAsFactors=T)
str(customerData)
```

### change variable zip_code, county_code and occupation from integer to factor
```
customerData[,1] = as.factor(customerData[,1]) # zip_code
customerData[,2] = as.factor(customerData[,2]) # country_code
customerData[,12] = as.factor(customerData[,12]) # occupation
summary(customerData)

customerDataPrep = customerData
```

### drop variables not used in the following segmentation modeling
```
drop = c("zip_code","county_code","county_name","income","num_of_adults","children",
         "child0_3","child4_6","child7_9","child10_12","child13_18",
       "great_outdoors","sporting_life","health_fitness","luxury_life","doit_yourselfer",
       "truck_owner","motor_cycle","sports","entertainment_enth","hobbyists","avid_readers",
       "collectors","travel","pets","music","toys","art_craft","gardening","family","food","cars",
       "casino_gambling","contributors","travel_business","travel_personal","travel_vacation","donor_arts_or_cultural",
       "AAPPAREL","ACASHCONT","AEDUCATN","AENTRTAIN","AFOODTOTL","AGTOTALE",
       "AHEALTH","AHOUSING","AINSPENSN","AREADING","ATRANS")

customerDataPrep = customerDataPrep[,!(names(customerDataPrep) %in% drop)]
summary(customerDataPrep)
```

### replace NA in education_code, year_structure with median
```
customerDataPrep$education_code = 
  ifelse(is.na(customerDataPrep$education_code), median(customerDataPrep$education_code,na.rm = T), 
         customerDataPrep$education_code)

customerDataPrep$year_structure = 
  ifelse(is.na(customerDataPrep$year_structure), median(customerDataPrep$year_structure,na.rm = T), 
         customerDataPrep$year_structure)

customerDataPrep$contributor_index = 
  ifelse(is.na(customerDataPrep$contributor_index), median(customerDataPrep$contributor_index,na.rm = T), 
         customerDataPrep$contributor_index)

summary(customerDataPrep)
```

### reorder columns - categorical variables first, numerical variables last
```
customerDataPrep = customerDataPrep[,c(1,2,3,4,8,11,12,5,6,7,9,10,13,14,15,16)]
customerDataPrepCopy = customerDataPrep
```

### disable scientific notation
```
options(scipen = 999)
```

### rescale all numeric variables
```
rescale = function(r) {
  if (sd(r) != 0) 
    res = (r - mean(r))/sd(r) else res = 0 * r
    return(res)
}

numericVariablesScaled = apply(customerDataPrep[,8:16],2,rescale)
customerDataPrep = cbind(customerDataPrep[,1:7],numericVariablesScaled)

save.image(file="Bank.Rdata")
```


## Part 2: Convert Multiple Numeric Variables into a Smaller Group of Components/Factors for the Following Modeling

### approach 1: principal component analysis (PCA)
```
pca.out = prcomp(numericVariablesScaled,scale=T)
print(pca.out)
summary(pca.out) 
```

check cucumlative proportion

first 5 components explain around 70% of variances

PC1 highly related with num_in_hhld(-), num_of_children(-).......

check how many components' variances are above 1 / has big drop
```
plot(pca.out) 
pca.var = pca.out$sdev^2 # create variances from sd
pca.pve = pca.var/sum(pca.var)
plot(pca.pve)
plot(cumsum(pca.pve)) # visualize
```

### approach 2: factor analysis
```
library(GPArotation)
library(psych)
fa.parallel(numericVariablesScaled,fa="fa",n.iter=30,main="Scree plots with parallel analysis")
```

sharp breaks in the plot suggest the appropriate number of factors to extract
parallel analysis suggests that the number of factors =  5 

```
fa1 = factanal(numericVariablesScaled,factors = 5,rotation = "varimax", scores = "regression")
fa1
head(fa1$scores)
```

factor1 highly related with num_of_children, children, num_in_hhld.....

### use principal component analysis (PCA)'s result in this case
combine string variables with first 5 components together for kcca clustering
```
customerDataPrep = as.matrix(cbind(customerDataPrep[,1:7],pca.out$x[,1:5]))
```


## Part 3: KCCA Segmentation Analysis
### define distance function which calculate distance between 2 observations

For any comparations with NA records, return 0.5. Other columns' distance is calculated by absloute difference of 2 numbers. The total distance is the sum of all columns' distance.

```
distance = function(x,y) {
  if (x[1] == y[1] & x[2] == y[2])
    d1 = 0
  else if (x[1] != y[1] & x[2] == y[2])
    d1 = 0.5
  else
    d1 = 1
  
  if (x[3] == '' | y[3] == '') 
    d3 = 0.5
  else if (x[3] == y[3])
    d3 = 0
  else
    d3 = 1
  
  if (x[4] == ''| y[4] == '') 
    d4 = 0.5
  else if (x[4] == y[4])
    d4 = 0
  else
    d4 = 1
  
  if (x[5] == 0 | y[5] == 0) 
    d5 = 0.5
  else if (x[5] == y[5])
    d5 = 0
  else
    d5 = 1
  
  if (x[6] == ''| y[6] == '') 
    d6 = 0.5
  else if (x[6] == y[6])
    d6 = 0
  else
    d6 = 1
  
  if (x[7] == y[7]) 
    d7 = 0
  else
    d7 = 1
  
  d8 = abs(as.numeric(x[8])-as.numeric(y[8]))
  d9 = abs(as.numeric(x[9])-as.numeric(y[9]))
  d10 = abs(as.numeric(x[10])-as.numeric(y[10]))
  d11 = abs(as.numeric(x[11])-as.numeric(y[11]))
  d12 = abs(as.numeric(x[12])-as.numeric(y[12]))
  
  dis = d1+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12
  return(dis)
}
```

test
```
distance(customerDataPrep[1,], customerDataPrep[2,])
distance(customerDataPrep[34,], customerDataPrep[34,])
```

### create pair-wise distance matrix
```
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
```

test
```
distanceMatrix(customerDataPrep[1:2,], customerDataPrep[1:2,])
```

### define cluster center
pick the most frequently showed city/state/gender/marital_status as center
calculate mean of other numeric variables as center
```
centroid = function(m) {
  city = tail(names(sort(table(m[,1]))), 1)
  state = tail(names(sort(table(m[,2]))), 1)
  gender = tail(names(sort(table(m[,3]))), 1)
  marital_status = tail(names(sort(table(m[,4]))), 1)
  occupation = tail(names(sort(table(m[,5]))), 1)
  home_owner = tail(names(sort(table(m[,6]))), 1)
  dwelling_type = tail(names(sort(table(m[,7]))), 1)
  PC1 = mean(as.numeric(m[,8]))
  PC2 = mean(as.numeric(m[,9]))
  PC3 = mean(as.numeric(m[,10]))
  PC4 = mean(as.numeric(m[,11]))
  PC5 = mean(as.numeric(m[,12]))
  
  p = cbind(city,state,gender,marital_status,occupation,home_owner,dwelling_type,PC1,PC2,PC3,PC4,PC5)
  return(p)
}
```

test
```
centroid(customerDataPrep[1:2,])
```

### define kccaFamily with previous self-built functions
```
install.packages("flexclust")
library(flexclust)

cheng = kccaFamily(which=NULL, dist=distanceMatrix, cent=centroid, name="cheng",
                   preproc = NULL, trim=0, groupFun = "minSumClusters")
```

### run k-means clustering using the customized cheng function
```
set.seed(23)
k = kcca(customerDataPrep, 3, family=cheng, weights=NULL, group=NULL,
         control=NULL, simple=FALSE, save.data=FALSE)
k
summary(k)
save.image(file="Bank_Kcca.Rdata")
```

|cluster|  1 |   2|   3|
|-------|----|----|----|
|   size|6804|4309|8887| 

k@centers
|city            |state|gender|marital_status|occupation|home_owner|dwelling_type|
|----------------|-----|------|--------------|----------|----------|-------------|
|San Francisco   |   CA|  M   |             M|         0|         Y|            S|          
|San Jose        |   CA|  M   |             M|         0|         Y|            S|          
|Los Angeles     |   CA|  F   |             M|         0|         Y|            S|        


## Part 4: Bring cluster NO. Back to the Complete Data Table to Check Each Segment's Traits and Preferences
```
customerData$clusterNo = clusters(k)
summary(customerData)
write.csv(x=customerData,file = "./customerDataWithCluster.csv",row.names = TRUE)

segmentation1 = subset(customerData,customerData$clusterNo == 1)
segmentation2 = subset(customerData,customerData$clusterNo == 2)
segmentation3 = subset(customerData,customerData$clusterNo == 3)

### check some numerica variables' segment centers
```
m = c("age","median_income","education_code","num_in_hhld","num_of_children", 
      "child0_3","child4_6","child7_9","child10_12","child13_18",
      "year_structure","median_home_value","len_of_residence","contributor_index")
m1 = c("age","median_income","education_code","num_in_hhld","num_of_children",
       "year_structure","median_home_value","len_of_residence","contributor_index")
m2 = c("child0_3","child4_6","child7_9","child10_12","child13_18")

segment1 = rep(NA,14)
segment2 = rep(NA,14)
segment3 = rep(NA,14)
numVarCenter = data.frame(row.names = m,segment1,segment2,segment3)

for (i in m1){
  numVarCenter[i,1] = mean(segmentation1[[i]],na.rm = T)
  numVarCenter[i,2] = mean(segmentation2[[i]],na.rm = T)
  numVarCenter[i,3] = mean(segmentation3[[i]],na.rm = T)
}

for (i in m2){
  numVarCenter[i,1] = (nrow(subset(segmentation1,segmentation1[,i] == 1))/nrow(segmentation1))*100
  numVarCenter[i,2] = (nrow(subset(segmentation2,segmentation2[,i] == 1))/nrow(segmentation2))*100
  numVarCenter[i,3] = (nrow(subset(segmentation3,segmentation3[,i] == 1))/nrow(segmentation3))*100
}

View(numVarCenter)
```

  
### check customers' interest (percentage)
```
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
```

### check income allocation (average)
b = c("AAPPAREL","ACASHCONT","AEDUCATN","AENTRTAIN","AFOODTOTL","AGTOTALE","AHEALTH","AHOUSING","AINSPENSN","AREADING","ATRANS")
segment_1 = rep(NA,11)
segment_2 = rep(NA,11)
segment_3 = rep(NA,11)

incomeAllocation = data.frame(row.names=b,segment_1,segment_2,segment_3)

#segment1
for (i in 55:65) {
  incomeAllocation[i-54,1] = mean(segmentation1[[i]],na.rm = T)}
#segment2
for (i in 55:65) {
  incomeAllocation[i-54,2] = mean(segmentation2[[i]],na.rm = T)}
#segment3
for (i in 55:65) {
  incomeAllocation[i-54,3] = mean(segmentation3[[i]],na.rm = T)}

View(incomeAllocation)

sum(incomeAllocation$segment_1) # 68151.75
sum(incomeAllocation$segment_2) # 69816.83
sum(incomeAllocation$segment_3) # 69970.82
```
