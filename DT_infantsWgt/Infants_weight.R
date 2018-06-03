https://www.r-bloggers.com/using-decision-trees-to-predict-infant-birth-weights/
#Birth weights of infants
library(MASS)
library(rpart)
head(birthwt)

#distribution of infant weights:

hist(birthwt$bwt)

#number of infants born with low weight.

table(birthwt$low)

#In the dataset, all the variables are stored as numeric. Before we build our model, we need to #convert the categorical variables to factor.

cols <- c('low', 'race', 'smoke', 'ht', 'ui')
birthwt[cols] <- lapply(birthwt[cols], as.factor)
head(birthwt)

#let us split our dataset so that we have a training set and a testing set.

set.seed(1)
train <- sample(1:nrow(birthwt), 0.75 * nrow(birthwt))

#let us build the model. We will use the rpart function for this.
#Since low = bwt <= 2.5, we exclude bwt from the model, and since it is a classification #task, we specify method = 'class'
birthwtTree <- rpart(low ~ . - bwt, data = birthwt[train, ], method = 'class')
summary(birthwtTree)

plot(birthwtTree)
text(birthwtTree, pretty = 0)

#Let us see how the model performs on the test set.

birthwtPred <- predict(birthwtTree, birthwt[-train, ], type = 'class')
table(birthwtPred, birthwt[-train, ]$low)

#the accuracy is (31 + 5) / (31 + 5 + 2 + 10) = 75%



