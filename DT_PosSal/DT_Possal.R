# Decision Tree Regression

# Importing the dataset
dataset = read.csv('C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/DT_PosSal/Position_Salaries.csv')
dataset
dim(dataset)
colnames(dataset)
nrow(dataset)
head(dataset)
summary(dataset)
str(dataset)

pairs(dataset)

library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

dim(training_set)
dim(test_set)

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))
				  
plot(regressor)
text(regressor)

printcp(regressor) # display the results 
plotcp(regressor) # visualize cross-validation results 
summary(regressor) # detailed summary of splits

# Predicting a new result with Decision Tree Regression
y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the Decision Tree Regression results (higher resolution)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')

# Plotting the tree
plot(regressor)
text(regressor)



