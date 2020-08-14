#Data Analytics Final Project#
#Course:BU.510.650.84.SP19 Data Analytics#

##First part-Data exploration##
############
###########
#########
#######
#####
###
##
#
#clear the global environment
remove(list = ls())
heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
heart.data<-na.omit(heart.data)
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
View(heart.data)
head(heart.data,3) 
dim(heart.data)

heart.data$num[heart.data$num > 0] <- 1
View(heart.data)
barplot(table(heart.data$num),
        main="Fate", col="black")
library(ISLR)
library(glmnet)

#add labels only for plot
mosaicplot(heart.data$sex ~ heart.data$num,
           main="Fate by Gender", shade=FALSE,color=TRUE,
           xlab="Gender", ylab="Heart disease") 

boxplot(heart.data$age ~ heart.data$num,
        main="Fate by Age",
        ylab="Age",xlab="Heart disease")

#
boxplot(heart.data$trestbps ~ heart.data$num,
        main="Fate by restbps",
        ylab="trestbps",xlab="Heart disease")

boxplot(heart.data$chol~ heart.data$num,
        main="Fate by chol",
        ylab="chol",xlab="Heart disease")

#
boxplot(heart.data$fbs~ heart.data$num,
        main="Fate by fbs",
        ylab="fbs",xlab="Heart disease") 

#
levels(heart.data$num) = c("No disease","Disease")
levels(heart.data$cp) = c("1","2","3","4")
mosaicplot(heart.data$cp ~ heart.data$num,
           main="Fate by cp", shade=FALSE,color=TRUE,
           xlab="cp", ylab="Heart disease") 

#Remove NA
s = sum(is.na(heart.data))
heart.data <- na.omit(heart.data)

#Unsupervised learning-clustering
library("cluster")

#age terstbps
set.seed(1)
km.out=kmeans(heart.data[,c(1,4)],6,nstart=20)
km.out
plot(heart.data$age,heart.data$trestbps,col=km.out$cluster,pch=km.out$cluster,xlab="age", ylab="trestbps")

#age chol
set.seed(1)
km.out=kmeans(heart.data[,c(1,5)],5,nstart=20)
km.out
plot(heart.data$age,heart.data$chol,col=km.out$cluster,pch=km.out$cluster,xlab="age", ylab="thalach")

#age&fbs
set.seed(1)
km.out=kmeans(heart.data[,c(1,6)],3,nstart=20)
km.out
plot(heart.data$age,heart.data$fbs,col=km.out$cluster,pch=km.out$cluster,xlab="age", ylab="fbs")

#restecg thal
set.seed(1)
km.out4=kmeans(heart.data[,c(7,13)],2,nstart=20)
km.out4
plot(heart.data$restecg,heart.data$thal,col=km.out4$cluster,pch=km.out4$cluster,xlab="restecg", ylab="thal")

#restecg oldpeak 
set.seed(1)
km.out5=kmeans(heart.data[,c(7,10)],4,nstart=20)
km.out5
plot(heart.data$restecg,heart.data$oldpeak,col=km.out5$cluster,pch=km.out5$cluster,xlab="restecg", ylab="oldpeak")

#cp assumption
set.seed(1)
km.out5=kmeans(heart.data[,c(3,14)],3,nstart=20)
km.out5
plot(heart.data$cp,heart.data$num,col=km.out5$cluster,pch=km.out5$cluster,xlab="cp", ylab="num")

#oldpeak
set.seed(1)
km.out6=kmeans(heart.data[,c(9,10)],2,nstart=20)
km.out6
plot(heart.data$oldpeak,heart.data$exang,col=km.out6$cluster,pch=km.out6$cluster,xlab="exang", ylab="oldpeak")

#fit2
set.seed(1)
fit2=kmeans(heart.data,2,nstart=30)
clusplot(heart.data,fit2$cluster,labels=0.1,lines=0,main="K-means clustering")
aggregate(heart.data,by=list(fit2$cluster),labels=0.1,lines=0,FUN=mean)


##Second part-Decision tree##
############
###########
#########
#######
#####
###
##
#
#clear the global environment
remove(list = ls())
heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
heart.data<-na.omit(heart.data)
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

# Decision Tree
library(ISLR)
View(heart.data)
library(tree)
High = ifelse(heart.data$num > 0, "Yes", "No")
heart.data = data.frame(heart.data,High)
heart.data$cp<-as.factor(heart.data$cp)
heart.data$sex<-as.factor(heart.data$sex)
heart.data$fbs<-as.factor(heart.data$fbs)
heart.data$restecg<-as.factor(heart.data$restecg)
heart.data$exang<-as.factor(heart.data$exang)
heart.data$slope<-as.factor(heart.data$slope)
heart.data$ca<-as.factor(heart.data$ca)
heart.data$thal<-as.factor(heart.data$thal)
heart.data$num<-as.factor(heart.data$num)
set.seed(1)
tree.heart.data = tree(High~.-num, heart.data)
summary(tree.heart.data)
plot(tree.heart.data)
text(tree.heart.data,pretty=0)
set.seed(1)
tree1.heart.data = tree(cp~.-(High+num), heart.data)
summary(tree1.heart.data)
plot(tree1.heart.data)
text(tree1.heart.data,pretty=0)

set.seed(1)

train = sample(1:nrow(heart.data), nrow(heart.data)/2)
heart.data.test = heart.data[-train,]
High.test = High[-train]
tree.heart.data = tree(High ~ . -num, heart.data, subset=train)
tree.pred = predict(tree.heart.data,heart.data.test,type="class")
table(tree.pred, High.test)
mean(tree.pred==High.test)

set.seed(1)
cv.heart.data = cv.tree(tree.heart.data,FUN=prune.misclass)
names(cv.heart.data)
cv.heart.data
prune.heart.data = prune.misclass(tree.heart.data,best=5)
plot(prune.heart.data)
text(prune.heart.data, pretty = 0)
tree.pred = predict(prune.heart.data,heart.data.test,type="class")
table(tree.pred, High.test)
mean(tree.pred==High.test)

set.seed(1)
train = sample(1:nrow(heart.data), nrow(heart.data)/2)
heart.data.test = heart.data[-train,]
cp=heart.data$cp
cp.test = cp[-train]
tree1.heart.data = tree(cp~.-(High+num), heart.data, subset=train)
tree1.pred = predict(tree1.heart.data,heart.data.test,type="class")
table(tree1.pred, cp.test)
mean(tree1.pred==cp.test)

set.seed(1)
cv.heart.data1 = cv.tree(tree1.heart.data,FUN=prune.misclass)
names(cv.heart.data1)
cv.heart.data1
prune.heart.data1 = prune.misclass(tree1.heart.data,best=7)
plot(prune.heart.data1)
text(prune.heart.data1, pretty = 0)
tree1.pred = predict(prune.heart.data1,heart.data.test,type="class")
table(tree1.pred, cp.test)
mean(tree1.pred==cp.test)


##Third part-Logit Regression##
############
###########
#########
#######
#####
###
##
#
#clear the global environment
remove(list = ls())
heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
heart.data<-na.omit(heart.data)
names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
View(heart.data)
head(heart.data,3) 
dim(heart.data)


library(ISLR)
library(glmnet)


heart.data$num_new                      <-"0"
heart.data$num_new[heart.data$num>0]    <-"1"
heart.data$sex                          <-as.factor(heart.data$sex)
heart.data$cp                           <-as.factor(heart.data$cp)
heart.data$fbs                          <-as.factor(heart.data$fbs)
heart.data$restecg                      <-as.factor(heart.data$restecg)
heart.data$exang                        <-as.factor(heart.data$exang)
heart.data$slope                        <-as.factor(heart.data$slope)
heart.data$ca                           <-as.factor(heart.data$ca)
heart.data$thal                         <-as.factor(heart.data$thal)
heart.data$num_new                      <-as.factor(heart.data$num_new)

# randomly split data into two subsets
set.seed(1)
train=sample(1:nrow(heart.data), nrow(heart.data)/2) 

# the variable test will store the numbers of remaining rows, which will be our test data
test=(-train)

# we create the part of Default.v3 that will be our training data
heart.data.train=heart.data[train,] 

# we create the part of Default.v3 that will be our testing data
heart.data.test=heart.data[test,] 

# store the output of the testing set in a different data frame
heart.data.test.output=heart.data$num_new[test] 

logreg.fit <- glm(num_new~.-num,data=heart.data.train, family = binomial)
summary(logreg.fit)
logreg.fit.prob=predict(logreg.fit,heart.data.test,type="response")
summary(logreg.fit.prob)
logreg.fit.pred=rep("0",149)
logreg.fit.pred[logreg.fit.prob>0.5]="1"
logreg.fit.pred
mean(logreg.fit.pred==heart.data.test.output)

#do a new test with significant predictors:sex, cp, trestbps, thalach, ca, thal

logreg.fit1 <- glm(num_new~sex+cp+trestbps+thalach+ca+thal, data=heart.data.train,family = binomial)
summary(logreg.fit1)
logreg.fit1.prob=predict(logreg.fit1,heart.data.test,type="response")
summary(logreg.fit1.prob)
logreg.fit1.pred=rep("0",149)
logreg.fit1.pred[logreg.fit1.prob>0.5]="1"
logreg.fit1.pred
mean(logreg.fit1.pred==heart.data.test.output)

#The end#
