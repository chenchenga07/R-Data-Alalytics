install.packages('gclus')
#install gclus package
library('gclus')
# load gclus package
detach('package:gclus')

getwd()
#"/Users/chencheng/Desktop/R review/1. R Bootcamp"
setwd("/Users/chencheng/Desktop/R review/1. R Bootcamp")

mode('apple')
#"character"
today <- as.Date("2019-08-10")
mode(today)
#"numeric"
class(today)
#"Date"

save.image('myfile.Rdata')
# saves the workspace to myfile.Rdata

firstName <- 'Cheng'; lastName <- 'Chen'
fullName <- paste(firstName,'',lastName)
fullName
#"Cheng  Chen"

tolower(fullName)
#"cheng  chen"
toupper(fullName)
"CHENG  CHEN"
is.character(fullName)
#TRUE

ls()
# Lists objects in the current workspace
rm(list=ls())
# Clears the workspace

paste('Today is',date(),'!')
#"Today is Sat Aug 10 20:11:44 2019 !"



#######  vectors: 1 dimensional array of elements with same data type   #####
#form the vector
numericVector <- c(1,9,4,-2,82.56)
characterVector <- c('mountains','waterfalls','flowers')
logicalVector <- c(TRUE, FALSE, TRUE, TRUE, FALSE)

numericVector*2
# 2.00  18.00   8.00  -4.00 165.12
vector2 <- c(1,1,2,3,4)
numericVector+vector2
# 2.00 10.00  6.00  1.00 86.56

vector2[2:4]
# 1 2 3
vector2[c(1,2,5)]
# 1 1 4
vector2[vector2 <=2 | vector2 != 4]
# 1 1 2 3
#get the elements that are less than 2 or not equal to 4
vector2[-1]
# 1 2 3 4
#get all elements but first
vector2[-c(1,5)]
# 1 2 3
#get all elements excluding the ones at the 1st and 5th place

increasingSeq <- 1:10
increasingSeq
# 1  2  3  4  5  6  7  8  9 10

seq(from = -1, to = 2,by = 0.5)
# -1.0 -0.5  0.0  0.5  1.0  1.5  2.0
seq(from = 10,length=3,by=10)
#10 20 30

rep(8,5)
# 8 8 8 8 8
rep(1:3,2)
# 1 2 3 1 2 3
rep(1:2,each=2)
# 1 1 2 2





######  matrices: a 2 dimensional array of elements in same mode  #########
m <- matrix (1:20, nrow=5, ncol=4)
m
# [,1] [,2] [,3] [,4]
# [1,]    1    6   11   16
# [2,]    2    7   12   17
# [3,]    3    8   13   18
# [4,]    4    9   14   19
# [5,]    5   10   15   20

m <- matrix(c(-3,5,8,0,2,11),2,3,byrow=T)
m
#       [,1] [,2] [,3]
# [1,]   -3    5    8
# [2,]    0    2   11

m[1,2]
# 5
m[,2]
# 5 2
m[1,]
# -3  5  8

#join 2 or more vectors/matrics by column/rows
cbind(c(1,2),m[,2])
#       [,1] [,2]
# [1,]    1    5
# [2,]    2    2

rbind(c(1,2),m[,2])
#        [,1] [,2]
# [1,]    1    2
# [2,]    5    2


######### Data Frames: columns can contain different modes of data #######

patientID <- c(1,2,3,4)
age <- c(25,34,28,52)
diabetesType <- c("Type1","Type2","Type3","Type4")
status <- c("Poor","Improved","Excellent","Poor")

patientData <- data.frame(patientID,age,diabetesType,status)
patientData
rownames(patientData) <- c("Alan","Jenny","Bill","George")

patientData
patientData[2,4]
patientData["Jenny","status"]

#all values in column 2 --- a list
patientData[,2]
patientData[[2]]
patientData[['age']]
patientData$age

mode(patientData[['age']])
class(patientData[['age']])
#"numeric"

#Vertical Slice  ----  a data frame
patientData[2]
patientData['age']
patientData[c('diabetesType','status')]

mode(patientData['age'])
#"list"
class(patientData['age'])
#"data.frame"

#Horizontal Slice  ----  a data frame
patientData[2,]
patientData["Alan",]
patientData[c('Alan','Jenny'),]
patientData[age>50,]





###############   factor    ############
status <- c('Poor','Improved','Excellent')
as.factor(status)
status <- factor(status,ordered = T,levels = c('Improved','Poor','Excellent'))
status


###############   read csv file  ########
iris <- read.csv('IrisDataset.csv',header=T)
str(iris)
iris <- read.csv('IrisDataset.csv',stringsAsFactors=FALSE)
str(iris)




########   Conditional Execution   #####
grade <- 86

ifelse(grade>90,'A','A-')

########   For loop   #####
for(i in 1:3) print(paste('Square Root of',i,'is',sqrt(i)))
# [1] "Square Root of 1 is 1"
# [1] "Square Root of 2 is 1.4142135623731"
# [1] "Square Root of 3 is 1.73205080756888"

for(i in 1:3) {
  print(paste('Square Root of',i,'is',sqrt(i)))
  }
# [1] "Square Root of 1 is 1"
# [1] "Square Root of 2 is 1.4142135623731"
# [1] "Square Root of 3 is 1.73205080756888"

j <- 0
for(i in 1:10){
  if(i<5) j=j+1
  else j=j-1
  print(i)
  print(j)
  }

########   While loop   #####
# Executes a statement until the given condition is no longer true
x <- 0
while(x < 10){
  x <- x+2
  print(x)
}


########   User Written Functions   #####
CalculateSum <- function(x=5,y=2){
  return(x+y)
}
CalculateSum()
# 7

CalculateSum1 <- function(x,y){
  return(x+y)
}
CalculateSum(10,5)
# 15



###############   Apply    ##########
myMatrix <- matrix(1:10,nrow=2)
myMatrix <- matrix(1:10,nrow=2,byrow = T)
myMatrix
apply(myMatrix,1,sum)#row
apply(myMatrix,2,sum)#column]


threeTimes <- function(x){
  x <- x*3
  return(x)
}
sapply(1:7,threeTimes)
lapply(1:3,threeTimes)

# String Concatenation
name <- 'Bob'
cat("Hi", name, "\b.\n", "Isn\'t R", "\t", "GREAT?\n")

# \ --- before a ' sign
# \b ---- remove space 
# \n ---- start a new line
# \t ---- tab



#################   Descriptive Statistics   #############3
mtcar <- read.csv('mtcars.csv', header = T)
mtcar

mtcar[1:10,]
myVars <- c('mpg','hp','wt')
myVars
mtcars[1:10,myVars]

mean(mtcar$mpg)
median(mtcar$mpg)
sd((mtcar$mpg))#标准差
var((mtcar$mpg))#方差

#把数据从小到大排列，看看排在x%位置的数是什么（不一定存在在list中）
#50% quantile = median
quantile(mtcar$mpg)
quantile(mtcar$mpg,probs = c(0.05,0.95))

new <- c(1,2,3,4,5,6,7,8,9,10)
new <- c(10,2,3,4,5,6,7,8,9,1)
quantile(new)

new <- c(1,2,3,10,11,12,13,20,21,22)
quantile(new)
plot(new)
range(new)

summary(mtcar)
myVars
aggregate(mtcars[myVars],by = list(mtcars$am),mean)
aggregate(mtcars[myVars],by = list(am = mtcars$am),mean)#change column name into "am"



########   Tables   ##############
#table, with, xtabs, prop.table

diamonds <- read.csv('diamonds.csv')
str(diamonds)
tableDiamondCut <- with(diamonds, table(cut))
# same with: tableDiamondCut <- table(diamonds$cut,diamonds$color)
tableDiamondCut
# cut
# Fair      Good     Ideal   Premium Very Good 
# 1610      4906     21551     13791     12082 

prop.table(tableDiamondCut)
# cut
# Fair       Good      Ideal    Premium  Very Good 
# 0.02984798 0.09095291 0.39953652 0.25567297 0.22398962 
prop.table(tableDiamondCut)*100

tableCutNColor <- with(diamonds, table(cut,color))
tableCutNColor
#              color
# cut            D    E    F    G    H    I    J
# Fair       163  224  312  314  303  175  119
# Good       662  933  909  871  702  522  307
# Ideal     2834 3903 3826 4884 3115 2093  896
# Premium   1603 2337 2331 2924 2360 1428  808
# Very Good 1513 2400 2164 2299 1824 1204  678

tableCutNColor <- xtabs(~color+cut,data=diamonds)#count
tableCutNColor
tableCutNColor2 <- xtabs(price~color+cut,data=diamonds)#sum(price)
tableCutNColor2/tableCutNColor#avg(price)

diamonds$binnedPrice <- cut(diamonds$price,
                            breaks = c(0,5000,10000,15000,Inf),
                            labels = c("<=5000","5000-10000","10001-15000",">15000"))
tableBinnedPrice <- table(diamonds$cut, diamonds$binnedPrice)
tableBinnedPrice

margin.table(tableCutNColor,margin = 1)#row sums
margin.table(tableCutNColor,margin = 2)#column sums

prop.table(tableCutNColor,margin = 1)#row proportions
prop.table(tableCutNColor,margin = 2)#column proportions
prop.table(tableCutNColor,margin = 2)*100#column percentages





########## regressions  ##########
women <-read.csv('women.csv')
regWomen <- lm(weight~height, data = women)
summary(regWomen)

########## plot  ##########
Arthritis <-read.csv('Arthritis.csv')
library(vcd)

treatCount <- table(Arthritis$Treatment)
barplot(treatCount,
        main='Simple Bar Plot',
        xlab='Treatment',
        ylab='Count',
        col='light blue')

plot(Arthritis$Treatment,
        main='Simple Bar Plot',
        xlab='Treatment',
        ylab='Count',
        col='light blue')

counts <- table(Arthritis$Treatment,Arthritis$Improved)
counts
#stacked bar plot
barplot(counts,
        main='Stacked Bar Plot',
        xlab='Improved Status',
        ylab='Count',
        col=c('light blue','pink'),
        legend=rownames(counts))

#grouped bar plot
barplot(counts,
        main='Grouped Bar Plot',
        xlab='Improved Status',
        ylab='Count',
        col=c('light blue','pink'),
        legend=rownames(counts),
        beside = T)
        
#ggplot2
library(ggplot2)
ggplot(data=mtcars,aes(x=wt,y=mpg))+
  geom_point(color='red')+   #geom--specify the chart type
  labs(title='Mileage by Automobile Weight',  #labs--add annotations
       x='Weight',
       y='Miles Per Gallon')


abalone <- read.csv("Abalones.csv", header = TRUE)
ggplot(abalone,aes(x=Rings))+
  geom_histogram(color='brown',fill='hot pink',binwidth=2)+
  labs(title='Abalones',x='Rings')

ggplot(abalone,aes(x=Sex,y=Rings))+
  geom_boxplot(color='blue',notch=T,fill='sky blue')+
  labs(title='Abalones',x="Sex")

#stacked bar chart
Salaries <- read.csv('Salaries.csv')
ggplot(Salaries,aes(x=rank,fill=sex))+
  geom_bar(position='stack')+
  labs(title='Stacked Bar Chart',x='Academic Rank',y='Count')
#position='dodge' is same with 'beside' style in plot()

#can also achieved by plot(), but need to trim a sub data table first
a <- table(Salaries$sex,Salaries$rank) #fill with count
barplot(a,
        main='Stacked Bar Chart',
        xlab='Academic Rank',
        ylab='Count',
        col=c('light blue','pink'),
        legend=rownames(a),
        beside = F)

df  <- data.frame(a = 1, B = 2, f = 4) #to data frame
df
df  <- cbind(a = 1, B = 2, f = 4)#to matrix(can only contain numbers)
df
a

#plots superimposed
ggplot(Salaries,aes(x=rank,y=salary))+
  geom_boxplot(fill='sky blue',color='black',notch=T)+
  geom_point(position='jitter',color='sea green',alpha=0.5)+
  geom_rug(color='hot pink')+
  labs(title='faculty Salaries by Rank',x='Academic Rank',y='Salary')
  
  
#faceting -- displaying graphs side-by-side
ggplot(Salaries,aes(x=yrs.since.phd,y=salary,color=rank,shape=rank))+
  geom_point()+
  labs(title='Faculty Salaries',x='Years since PhD',y='Salary')+
  facet_grid(.~sex)
  
ggplot(Salaries,aes(x=rank,y=salary)) +
  geom_point()+
  facet_grid((.~sex))

ggplot(Salaries,aes(x=rank,y=salary,fill=sex))+
  geom_boxplot()+
  scale_x_discrete(breaks=c('AsstProf','AssocProf','Prof'),
                   labels=c('Assistant\nProfessor','Associate\nProfessor','Professor'))+
  scale_y_continuous(breaks=c(50000,100000,150000,200000),
                     labels=c('$50k','$100k','$150k','$200k'))+
  labs(title='Faculty Salary by Rank and Gender',x='Rank',y='Salary',fill='Gender')+
  theme(legend.position = 'bottom')

#fill='Gender' is the label for the index box



