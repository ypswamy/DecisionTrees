set.seed(678)
path <- 'https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/titanic_csv.csv'
titanic <-read.csv(path)
head(titanic)


#From the head and tail output, you can notice the data is not shuffled.
#To overcome this issue, you can use the function sample().

shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)

#use this index to shuffle the titanic dataset.

titanic <- titanic[shuffle_index, ]
head(titanic)
install.packages("dplyr")

library(dplyr)
# Drop variables
clean_titanic <- titanic % > %

#Drop unnecessary variables
select(-c(home.dest, cabin, name, X, ticket)) % > % 

#Convert to factor level
#Add label to the variable pclass. 1 becomes Upper, 2 becomes MIddle and 3 becomes lower
mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),

#Add label to the variable survived. 1 Becomes No and 2 becomes Yes
survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) % > %

#Remove the NA observations
na.omit()
glimpse(clean_titanic)




train <- sample (1:nrow(titanic), 0.8*nrow(titanic)) 

# training data
data_train <- titanic[train, ] 

# test data
data_test <- titanic[-train, ] 
dim(data_train)
dim(data_test)

#Use the function prop.table() combined with table() to verify if the randomization process is correct.
prop.table(table(data_train$survived))
prop.table(table(data_test$survived))

library(rpart)
library(rpart.plot)

fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)


#You want to predict which passengers are more likely to survive after the collision from the test set. 
#It means, you will know among those 209 passengers, which one will survive or not.

predict_unseen <-predict(fit, data_test, type = 'class')

#Testing the passenger who didn't make it and those who did.

table_mat <- table(data_test$survived, predict_unseen)
table_mat


accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

#You have a score of 78 percent for the test set. You can replicate the same exercise with the training dataset.



accuracy_tune <- function(fit) {
    predict_unseen <- predict(fit, data_test, type = 'class')
    table_mat <- table(data_test$survived, predict_unseen)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_Test
}


#You can try to tune the parameters and see if you can improve the model over the default value. As a reminder, you need to get an accuracy higher than 0.78

control <- rpart.control(minsplit = 4,
    minbucket = round(5 / 3),
    maxdepth = 3,
    cp = 0)
tune_fit <- rpart(survived~., data = data_train, method = 'class', control = control)
accuracy_tune(tune_fit)

