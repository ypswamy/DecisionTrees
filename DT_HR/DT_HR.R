library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)

hr<- read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/Kaggle/DT_HR/HR_comma_sep.csv")
hr
dim(hr)
nrow(hr)
names(hr)
summary(hr)
str(hr)
pairs(hr)

names(hr)
hr <- rename(hr,Department=sales)
names(hr)

str(hr$left)
hr$left <- as.factor(hr$left)
str(hr$left)

colSums(is.na(hr))

#Splitting data to test and train
set.seed(123)
index<-sample(nrow(hr),0.7*nrow(hr))
train<-hr[index,]
test<- hr[-index,]

dt <- rpart(left~.,data=train,method = "class", control = rpart.control(cp=0.02,maxdepth = 5),parms = list(split="gini"))

dt
plot(dt)
text(dt)

rpart.plot(dt,cex=0.6)
printcp(dt)
plotcp(dt,minline = T)

# Pruning the tree
dt1<-prune(dt,cp=0.023)
dt1
plot(dt1)
text(dt1)

rpart.plot(dt1,main = "Classification Tree for Predicting Employee attrition", cex=0.7)

pred<- predict(dt1,newdata = test,type="class")
confusionMatrix(pred,test$left)


#Plotting RoC Curve
library(ROCR)
pred1 <- prediction(as.numeric(pred),as.numeric(test$left))
perf<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="blue")
abline(0,1, lty = 8, col = "grey")		

auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values"))
auc
