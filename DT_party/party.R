/********************************/
/*            CASE 1            */    
/********************************/

library (party)

#Step 1: Build the model tree

# build the tree model
fit <- ctree (response ~ pred1 + pred2 + pred3, data = inputData)  

# the ctree
plot (fit, main="Conditional Inference Tree")  

#Step 2: Predict On New or Test Data

# predict on test data
pred.response <- as.character (predict(fit), testData) 

# actuals
input.response <- as.character (testData$response) 

# misclassification %
mean (input.response != pred.response) 


/********************************/
/*            CASE 2            */    
/********************************/

#Step 1: Build the tree

library (rpart)

# build the model
rpartMod <- rpart(response ~ ., data = inputData, method = "class")  

# print the cptable
printcp(rpartMod)  

# predict probabilities
out <- predict(rpartMod) 

# predict response
pred.response <- colnames(out)[max.col(out, ties.method = c("random"))] 

# % misclassification error 
mean(inputData$response != pred.response) 

#Step 2: Predict the Test Data
out <- predict(rpartMod, testData)
