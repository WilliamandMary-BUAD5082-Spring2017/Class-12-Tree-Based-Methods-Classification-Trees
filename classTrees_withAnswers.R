# Lab from the book using the tree package
# Fitting Classification Trees

#Some packages you might need to install
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("tree")
#install.packages("rattle")
#install.packages("RColorBrewer")

library(tree)
library(ISLR)
library(RColorBrewer)
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200

# Example using rpart a superior package for decision trees
# Make sure your workind directory is set correctly in order 
# to read the csv file

rm(list=ls())
require(rpart)
weather<-read.table("myweatherdata.csv",sep=",",header=T)
###Discard unwanted columns
names(weather)
discardcols <- names(weather) %in% c("Date", "Location","RISK_MM")
weather<- weather[!discardcols]
names(weather)
###Build 90% training vector of indices and 10% test data frame named weather.test
set.seed(527)
train<-sample(1:nrow(weather), .9*nrow(weather))
weather.test<-weather[-train,]
###build rpart decision tree
mymodel <- rpart(RainTomorrow ~ .,data=weather[train,],method="class",
                 parms=list(split="information"),control=rpart.control(usesurrogate=0,maxsurrogate=0,minsplit=53))
###various info and graphic displays
print(mymodel)
printcp(mymodel)
plot(mymodel)
text(mymodel,pretty=0)
require(rattle)
fancyRpartPlot(mymodel, main="Decision Tree")
asRules(mymodel)
summary(mymodel)
###Tuning Parameters
anothermodel<-rpart(RainTomorrow ~ .,data=weather[train,], method="class", 
                    parms=list(split="information"), control=rpart.control(minsplit=10, 
                                                                           minbucket=5,maxdepth=20,usesurrogate=0,maxsurrogate=0))
###Building a maximal model
mymodel<-rpart(RainTomorrow~.,data=weather[train,],method="class",
               parms=list(split="information"),control=rpart.control(usesurrogate=0,
                                                                     maxsurrogate=0,cp=0,minbucket=1,minsplit=2))
fancyRpartPlot(mymodel, main="Maximal Decision Tree")
print(mymodel$cptable)
plotcp(mymodel)
grid()
###Pruning the maximal tree to minimize xerror
mymodel$cptable
xerr<-mymodel$cptable[,"xerror"]
minxerr<-which.min(xerr)
mincp<-mymodel$cptable[minxerr,"CP"]
mymodel.prune<-prune(mymodel,cp=mincp)
mymodel.prune$cptable
fancyRpartPlot(mymodel.prune, main="Decision Tree With Minimum C.V. Error")
asRules(mymodel.prune)
###Specifying a Loss Matrix
mymodel.prune<-rpart(RainTomorrow~.,data=weather[train,],method="class",
                     parms=list(split="information"),control=rpart.control(usesurrogate=0,
                                                                           maxsurrogate=0,cp=0,minbucket=1,minsplit=2,loss=matrix(c(0,1,10,0), byrow=TRUE, nrow=2)))
###Still want to prune even when using Loss Matrix to build maximal tree
mymodel.prune$cptable
xerr<-mymodel.prune$cptable[,"xerror"]
minxerr<-which.min(xerr)
mincp<-mymodel.prune$cptable[minxerr,"CP"]
mymodel.prune<-prune(mymodel.prune,cp=mincp)
###Predict on the test set
mymodel.prune.predict <- predict(mymodel.prune, newdata=weather.test, type="class")
table(weather.test$RainTomorrow, mymodel.prune.predict,dnn=c("Actual", "Predicted"))
round(100*table(weather.test$RainTomorrow, mymodel.prune.predict,dnn=c("% Actual", "% Predicted"))/length(mymodel.prune.predict))


# Exercise

churndata<-read.table("churndata.csv",header=T,sep=",")
###transform variables and deal with missing values###
churndata$area<-factor(churndata$area)
churndata<-na.omit(churndata)

# Create training and test sets with an 80/20 split
train<-sample(1:nrow(churndata), .8*nrow(churndata))
churndata.test<-churndata[-train,]

# Create and examine classification model with cp=0, minsplit=2,minbucket=1 (we'll prune the tree later)
rpart<-rpart(churn ~ .,data=churndata[train,], method="class",parms=list(split="information"),control=rpart.control(usesurrogate=0, maxsurrogate=0,cp=0, minsplit=2,minbucket=1))

#Evaluate the models predictive power (make predictions on test set and print a confusion matrix)
predict <- predict(rpart, newdata=churndata.test, type="class")
table(churndata.test$churn, predict,dnn=c("Actual", "Predicted"))

# Determine the best complexity parameter, prune the tree and evaluate the predictive power of the new tree
rpart$cptable
xerr<-rpart$cptable[,"xerror"]
minxerr<-which(xerr==min(xerr))
minxerr
mincp<-rpart$cptable[minxerr,"CP"]
mincp
rpart.prune<-prune(rpart,cp=mincp)
predict <- predict(rpart.prune, newdata=churndata.test, type="class")
table(churndata.test$churn, predict,dnn=c("Actual", "Predicted"))

# Plot the models next to each other and observe the differences (use fancyRpartPlot)
par(mfrow=c(1,2))
fancyRpartPlot(rpart, main="Decision Tree With No Pruning")
fancyRpartPlot(rpart.prune, main="Decision Tree With Minimum C.V. Error")

# Play around with some of the settings in the creation of the tree and see
# what kind of changes are made to the end model