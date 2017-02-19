###Set up###
rm(list=ls())
require(rpart)     #this is the recursive partitioning package
require(rattle)    # the fancyRpartPlot and asRules functions at the end of this script are in the rattle package
###get data and explore###                    
churndata<-read.table("churndata.csv",header=T,sep=",")
names(churndata)
str(churndata) 
summary(churndata)
par(mfrow=c(4, 4))                 #tell R we want a 4x4 grid of plots on the same screen
for(i in c(2,4:17)) {hist(churndata[,i], xlab=names(churndata)[i],main=names(churndata)[i])}
###transform variables and deal with missing values###
churndata$area<-factor(churndata$area)
churndata<-na.omit(churndata)
###partition data into training, validate and test subsets (60/20/20)###
set.seed(527)
nobs <- nrow(churndata) 
trainrows <- sample(nobs, 0.6* nobs) 
validaterows <- sample(setdiff(seq_len(nobs), trainrows), 0.2* nobs) 
testrows <- setdiff(setdiff(seq_len(nobs), trainrows), validaterows)
length(union(testrows,union(validaterows,trainrows)))
length(intersect(trainrows,intersect(validaterows,testrows)))
train<-churndata[trainrows,]
validate<-churndata[validaterows,]
test<-churndata[testrows,]

###create and examine classification model with cp=0, minsplit=2,minbucket=1 (we'll prune the tree later) ###
rpart<-rpart(churn ~ .,data=train, method="class",parms=list(split="information"),control=rpart.control(usesurrogate=0, maxsurrogate=0,cp=0, minsplit=2,minbucket=1))
print(rpart)
printcp(rpart)
windows()                          #open new graphics window
fancyRpartPlot(rpart, main="Customer Churn Prediction Model")
predict <- predict(rpart, newdata=train, type="class")
table(train$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(train$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))
###evaluate predictive power using validate dataset###
predict <- predict(rpart, newdata=validate, type="class")
table(validate$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(validate$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))
###prune classification tree###
rpart$cptable
xerr<-rpart$cptable[,"xerror"]
minxerr<-which(xerr==min(xerr))
minxerr
mincp<-rpart$cptable[minxerr,"CP"]
mincp
rpart.prune<-prune(rpart,cp=mincp)
###compare pruned/original models on validate dataset###
rpart.prune$cptable
windows()                          #open new graphics window
fancyRpartPlot(rpart.prune, main="Pruned Customer Churn Prediction Model")
predict <- predict(rpart.prune, newdata=validate, type="class")
table(validate$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(validate$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))
###evaluate predictive power using test dataset###
asRules(rpart.prune)
predict <- predict(rpart.prune, newdata=test, type="class")
table(test$churn, predict,dnn=c("Actual", "Predicted"))
round(100*table(test$churn, predict,dnn=c("% Actual", "% Predicted"))/length(predict))
###score new dataset and create file###
newdata<-read.table("newcustomerdata.csv",header=T,sep=",")
names(newdata)
str(newdata) 
newdata$area<-factor(newdata$area)
summary(newdata)
par(mfrow=c(4, 4))                 #tell R we want a 4x4 grid of plots on the same screen
for(i in c(3:16)) {hist(newdata[,i], xlab=names(newdata)[i],main=names(newdata)[i])}
newdata<-na.omit(newdata)
predict <- predict(rpart.prune, newdata=newdata, type="class")
newdata$churn<-predict   #add to newdata frame
write.table(newdata,file="prediction.csv",sep=",")
