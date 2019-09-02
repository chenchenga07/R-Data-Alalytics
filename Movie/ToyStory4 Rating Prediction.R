# Toy Story 4 rating predcition

# using around 80,000 existing users' ratings towards 6 Toy Story and Cars movies
# predict individual user's ratings for cars3 given their ratings of previous 5 movies

## load data source
getwd()
ratings <- read.csv("./Movie/ratings.csv", stringsAsFactors=FALSE)

allSix <- subset(ratings, ratings$movieId %in% c(1,3114,45517,78499,87876,170957))
str(allSix) # 123225 obs.

## data re-organize
uniqueUser <- length(unique(allSix$userId))
# 80734 unique userID

toyStory1 <- rep(NA,uniqueUser)
toyStory2 <- rep(NA,uniqueUser)
cars1 <- rep(NA,uniqueUser)
toyStory3 <- rep(NA,uniqueUser)
cars2 <- rep(NA,uniqueUser)
cars3 <- rep(NA,uniqueUser)
userId <- unique(allSix$userId)
finalDB <- data.frame(userId,toyStory1,toyStory2,cars1,toyStory3,cars2,cars3)

toyStory1Subset = subset(allSix,allSix$movieId==1)
for (i in unique(toyStory1Subset$userId)){
  finalDB[finalDB$userId==i,2]=toyStory1Subset[toyStory1Subset$userId==i,3]
}

toyStory2Subset = subset(allSix,allSix$movieId==3114)
for (i in unique(toyStory2Subset$userId)){
  finalDB[finalDB$userId==i,3]=toyStory2Subset[toyStory2Subset$userId==i,3]
}

cars1Subset = subset(allSix,allSix$movieId==45517)
for (i in unique(cars1Subset$userId)){
  finalDB[finalDB$userId==i,4]=cars1Subset[cars1Subset$userId==i,3]
}

toyStory3Subset = subset(allSix,allSix$movieId==78499)
for (i in unique(toyStory3Subset$userId)){
  finalDB[finalDB$userId==i,5]=toyStory3Subset[toyStory3Subset$userId==i,3]
}

cars2Subset = subset(allSix,allSix$movieId==87876)
for (i in unique(cars2Subset$userId)){
  finalDB[finalDB$userId==i,6]=cars2Subset[cars2Subset$userId==i,3]
}

cars3Subset = subset(allSix,allSix$movieId==170957)
for (i in unique(cars3Subset$userId)){
  finalDB[finalDB$userId==i,7]=cars3Subset[cars3Subset$userId==i,3]
}


View(finalDB)
save.image(file="animatedMovie.Rdata")



## exploratory analysis

finalDBMeans <- colMeans(finalDB[,2:7],na.rm=TRUE)
finalDBMeans
# mean ratings of each movie
# toyStory1 toyStory2     cars1 toyStory3     cars2     cars3 
# 3.886649  3.809977  3.347277  3.870090  2.805595  3.128425 

x=finalDB[,2:7]
y=finalDB[,2:7]
cor(x,y,use="pairwise.complete.obs")
# correlations between ratings of each movie
#            toyStory1 toyStory2     cars1 toyStory3     cars2     cars3
# toyStory1 1.0000000 0.7497058 0.4315111 0.6912760 0.2813384 0.4119281
# toyStory2 0.7497058 1.0000000 0.4475096 0.6845280 0.3636317 0.3800859
# cars1     0.4315111 0.4475096 1.0000000 0.4472142 0.7187948 0.7421214
# toyStory3 0.6912760 0.6845280 0.4472142 1.0000000 0.3499898 0.3699212
# cars2     0.2813384 0.3636317 0.7187948 0.3499898 1.0000000 0.7065726
# cars3     0.4119281 0.3800859 0.7421214 0.3699212 0.7065726 1.0000000

# movies within the same collection have a higher correlation
# toyStory1 and toyStory2 are most similar, correlation around 0.75
# toyStory1 and cars2 are most different, correlation around 0.28



## predictive modeling
TS1Vec = c('toyStory1','I(toyStory1^2)') 
TS2Vec = c('toyStory2','I(toyStory2^2)') 
c1Vec = c('cars1','I(cars1^2)') 
TS3Vec = c('toyStory3','I(toyStory3^2)') 
c2Vec = c('cars2','I(cars2^2)') 
c3Vec = c('cars3','I(cars3^2)') 

fullSet = expand.grid(TS1Vec,TS2Vec,c1Vec,TS3Vec,c2Vec,c3Vec)
# create a data frame "fullSet" with all combinations of the supplied factors

formulaSet = paste("cars3 ~",apply(fullSet,1,paste,collapse='+')) 

for(i in 1:length(formulaSet))
  {print(lm(as.formula(formulaSet[i]),data=finalDB,na.action=na.exclude))}
# print out all the formula in formulaSet 


# compare models
AIC <- rep(NA,length(formulaSet))
BIC <- rep(NA,length(formulaSet))
MSE <- rep(NA,length(formulaSet))
result <- data.frame(formulaSet,AIC,BIC,MSE)

for(i in unique(formulaSet)){ 
  result[result$formulaSet==i,2] = AIC(lm(as.formula(i),data=finalDB))
  result[result$formulaSet==i,3] = BIC(lm(as.formula(i),data=finalDB))
}

set.seed(123)
randOrder=order(runif(nrow(finalDB)))
trainingData=subset(finalDB,randOrder < 0.9*nrow(finalDB))
testData=subset(finalDB,randOrder >= 0.9*nrow(finalDB))

# ignore NA records
for(i in unique(formulaSet)){ 
  linearLM = lm(as.formula(i),data=trainingData)
  result[result$formulaSet==i,4] = mean((testData$cars3 - predict(linearLM,testData))^2,na.rm = T)
}

View(result)

result[which.min(result$AIC),]
#    formulaSet       
# 52 cars3 ~ I(toyStory1^2)+I(toyStory2^2)+cars1+toyStory3+I(cars2^2)+I(cars3^2)
# AICmin = -46.36302

result[which.min(result$BIC),]
#    formulaSet       
# 52 cars3 ~ I(toyStory1^2)+I(toyStory2^2)+cars1+toyStory3+I(cars2^2)+I(cars3^2)
# BICmin = -24.13003

result[which.min(result$MSE),]
#    formulaSet       
# 52 cars3 ~ I(toyStory1^2)+I(toyStory2^2)+cars1+toyStory3+I(cars2^2)+I(cars3^2)
# MSEmin = 0.01315301

modelSelected = lm(cars3 ~ I(toyStory1^2)+I(toyStory2^2)+cars1+toyStory3+I(cars2^2)+I(cars3^2),data=finalDB)
mean(predict(modelSelected,data=finalDB))
# predicted cars ratings' mean: 3.168067

mean(finalDB[,7],na.rm = T)
# actual cars ratings' mean: 3.128425

t.test(predict(modelSelected,data=finalDB),finalDB[,7])
# p-value = 0.6944  predicted and actual ratings don't have significant difference

# does the higher predicted rating caused by 
# we only use users who have viewed all 5 previous movies as data source to traing the model?
# those people are into this movie genre so that they tent to rate higher





# people who don't rate all 6 movies
notRatingAll6 <- 
  subset(finalDB,
         is.na(finalDB[,2])|is.na(finalDB[,3])|is.na(finalDB[,4])|
           is.na(finalDB[,5])|is.na(finalDB[,6])|is.na(finalDB[,7]))

notRatingAll6Means <- colMeans(notRatingAll6[,2:7],na.rm=TRUE)
notRatingAll6Means
# toyStory1 toyStory2     cars1 toyStory3     cars2     cars3 
# 3.886196  3.809973  3.343629  3.869753  2.801237  3.101156


RatingAll6 <- 
  subset(finalDB,
         !is.na(finalDB[,2])&!is.na(finalDB[,3])&!is.na(finalDB[,4])&
           !is.na(finalDB[,5])&!is.na(finalDB[,6])&!is.na(finalDB[,7]))

RatingAll6Means <- colMeans(RatingAll6[,2:7])
RatingAll6Means
# toyStory1 toyStory2     cars1 toyStory3     cars2     cars3 
# 4.147059  3.810924  3.600840  3.911765  2.852941  3.168067 

t.test(RatingAll6[,2],notRatingAll6[,2]) #p-value = 0.0001124 *
t.test(RatingAll6[,3],notRatingAll6[,3]) #p-value = 0.9897
t.test(RatingAll6[,4],notRatingAll6[,4]) #p-value = 0.006824 *
t.test(RatingAll6[,5],notRatingAll6[,5]) #p-value = 0.5373
t.test(RatingAll6[,6],notRatingAll6[,6]) #p-value = 0.5949
t.test(RatingAll6[,7],notRatingAll6[,7]) #p-value = 0.583

# the rating difference among those 2 user group for ToyStory1 and Cars1 are statistics significant

# plot out to have an overview
group <- c("toyStory1","toyStory2","cars1","toyStory3","cars2","cars3")
Means <- data.frame(RatingAll6Means,notRatingAll6Means,group)
library(tidyr)
MeansLF <- gather(Means, table, mean, RatingAll6Means:notRatingAll6Means) 
#Create long format
View(MeansLF)

p <- ggplot(MeansLF, aes(y = mean, x = group,fill = table)) +
  geom_bar(position = "dodge",stat = "identity")+
  labs(x="Movie",y="Average Ratings")+
  theme(legend.title=element_blank())+
  ylim(0,5)
p





# fill NA with mean ratings from notRatingAll6
# train the prediction model again and check the result

finalDBFillNA <- finalDB

finalDBFillNA[which(is.na(finalDBFillNA$toyStory1)==TRUE),2] = mean(notRatingAll6$toyStory1,na.rm = T)
finalDBFillNA[which(is.na(finalDBFillNA$toyStory2)==TRUE),3] = mean(notRatingAll6$toyStory2,na.rm = T)
finalDBFillNA[which(is.na(finalDBFillNA$cars1)==TRUE),4] = mean(notRatingAll6$cars1,na.rm = T)
finalDBFillNA[which(is.na(finalDBFillNA$toyStory3)==TRUE),5] = mean(notRatingAll6$toyStory3,na.rm = T)
finalDBFillNA[which(is.na(finalDBFillNA$cars2)==TRUE),6] = mean(notRatingAll6$cars2,na.rm = T)

# train models
result2 <- data.frame(formulaSet,AIC,BIC,MSE)

for(i in unique(formulaSet)){ 
  result2[result2$formulaSet==i,2] = AIC(lm(as.formula(i),data=finalDBFillNA))
  result2[result2$formulaSet==i,3] = BIC(lm(as.formula(i),data=finalDBFillNA))
}

set.seed(123)
randOrder2=order(runif(nrow(finalDBFillNA)))
trainingData2=subset(finalDBFillNA,randOrder2 < 0.9*nrow(finalDBFillNA))
testData2=subset(finalDBFillNA,randOrder2 >= 0.9*nrow(finalDBFillNA))

for(i in unique(formulaSet)){ 
  linearLM2 = lm(as.formula(i),data=trainingData2)
  result2[result2$formulaSet==i,4] = mean((testData2$cars3 - predict(linearLM2,testData2))^2,na.rm = T)
}

View(result2)

result2[which.min(result2$AIC),]
# 59 cars3 ~ toyStory1+I(toyStory2^2)+cars1+I(toyStory3^2)+I(cars2^2)+I(cars3^2)
# AICmin = 58.75145

result2[which.min(result2$BIC),]
# 59 cars3 ~ toyStory1+I(toyStory2^2)+cars1+I(toyStory3^2)+I(cars2^2)+I(cars3^2) 
# BICmin = 88.16548

result2[which.min(result2$MSE),]
# 43 cars3 ~ toyStory1+I(toyStory2^2)+cars1+I(toyStory3^2)+cars2+I(cars3^2) 
# MSEmin = 0.03329003

# filling NA with mean will lead to larger variance compared with dropping NA records
