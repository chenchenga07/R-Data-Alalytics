# Toy Story 4 rating predcition

# using around 80,000 existing users' ratings towards 6 Toy Story and Cars movies
# can predict individual user's ratings for cars3 given their ratings of previous 5 movies

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

colMeans(finalDB[,2:7],na.rm=TRUE)
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

randOrder=order(runif(nrow(finalDB)))
trainingData=subset(finalDB,randOrder < 0.9*nrow(finalDB))
testData=subset(finalDB,randOrder >= 0.9*nrow(finalDB))

for(i in unique(formulaSet)){ 
  linearLM = lm(as.formula(i),data=trainingData)
  result[result$formulaSet==i,4] = mean((testData$cars3 - predict(linearLM,testData))^2,na.rm = T)
}

View(result)

result[which.min(result$AIC),]
#    formulaSet       
# 52 cars3 ~ I(toyStory1^2)+I(toyStory2^2)+cars1+toyStory3+I(cars2^2)+I(cars3^2)

result[which.min(result$BIC),]
#    formulaSet       
# 52 cars3 ~ I(toyStory1^2)+I(toyStory2^2)+cars1+toyStory3+I(cars2^2)+I(cars3^2)

result[which.min(result$MSE),]
#    formulaSet       
# 52 cars3 ~ I(toyStory1^2)+I(toyStory2^2)+cars1+toyStory3+I(cars2^2)+I(cars3^2)


modelSelected = lm(cars3 ~ I(toyStory1^2)+I(toyStory2^2)+cars1+toyStory3+I(cars2^2)+I(cars3^2),data=finalDB)
mean(predict(modelSelected,data=finalDB))
# predicted cars ratings' mean: 3.168067

mean(finalDB[,7],na.rm = T)
# actual cars ratings' mean: 3.128425

t.test(predict(modelSelected,data=finalDB),finalDB[,7])
# p-value = 0.6944  predicted and actual ratings don't have significant difference




### find those only have cars3 ratings
### fill out previous 5 movie ratings with mean()
### compare results
