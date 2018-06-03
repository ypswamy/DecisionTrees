install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
data("iris")
summary(iris)
structure(iris)
str(iris)
names(iris)

#split out entire dataset into two parts - the training set and the testing set. 

indexes = sample(150, 110)
iris_train = iris[indexes,]
iris_test = iris[-indexes,]

anyNA(wine_df)
training[["Species"]] = factor(training[["Species"]])
testing[["Species"]] = factor(testing[["Species"]])

target = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

#Build and plot model

tree = rpart(target, data = iris_train, method = "class")
rpart.plot(tree)

#Now that our model is built, we need to cross-check its validity by pitching it against #our test data. So we use the predict function to predict the classes of the test data. #And then create a matrix showing the comparison between the prediction result and the #actual category.

predictions = predict(tree, iris_test)
table(predictions, iris$Species)

na_count <-sapply(iris, function(y) sum(length(which(is.na(y)))))
na_count
na_count <- data.frame(na_count)
