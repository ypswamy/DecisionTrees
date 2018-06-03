#Lets use the ‘census income‘ dataset and apply various decision tree methods to predict #whether a person’s income will exceed $50K/yr. 

library(tree)
library(rpart)
library(party)

fullData <- read.csv("C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/DT_Adult/adult.data", header=F) 

summary(fullData)
head(fullData)
str(fullData)
dim(fullData)

names(fullData) <- c("age", "workclass", "fnlwgt", "education", "educationnum", "maritalstatus", "occupation", "relationship", "race", "sex", "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "response")

fullData <- fullData[, c(15, 1:13)] # remove a factor with more than 31 levels.
set.seed(100)

train <- sample (1:nrow(fullData), .8*nrow(fullData)) # training row indices
inputData <- fullData[train, ] # training data
testData <- fullData[-train, ] # test data

library(tree)
# model the tree, including all the variables
treeMod <- tree(response ~ ., data = inputData)  
# Plot the tree model
plot(treeMod)  
# Add text to the plot
text(treeMod, pretty=0)  
# Predict the training data
out <- predict(treeMod) 
# actuals
input.response <- as.character(inputData$response) 
# predicted
pred.response <- colnames(out)[max.col(out, ties.method = c("first"))] 
# misclassification %
mean (input.response != pred.response) 

#Step 2: Prune the tree

# run the cross validation
cvTree <- cv.tree(treeMod, FUN = prune.misclass)  
# plot the CV
plot(cvTree)  
# set size corresponding to lowest value in below plot. try 4 or 16.
treePrunedMod <- prune.misclass(treeMod, best = 9) 
plot(treePrunedMod)
text(treePrunedMod, pretty = 0)

#Step 3: Re-calculate the mis-classification error with pruned tree
# fit the pruned tree
out <- predict(treePrunedMod) 
# predicted
pred.response <- colnames(out)[max.col(out, ties.method = c("random"))] 
# Calculate Mis-classification error.
mean(inputData$response != pred.response) 

#Step 4: Predict
# Predict testData with Pruned tree
out <- predict(treePrunedMod, testData)  

