https://daviddalpiaz.github.io/r4sl/trees.html

library(ISLR)
data(Carseats)
str(Carseats)

Carseats$Sales = as.factor(ifelse(Carseats$Sales <= 8, "Low", "High"))
summary(Carseats$Sales)
str(Carseats)

seat_tree = rpart(Sales ~ ., data = Carseats)
summary(seat_tree)

plot(seat_tree)
text(seat_tree, pretty = 0)
title(main = "Unpruned Classification Tree")

dim(Carseats)

set.seed(2)
seat_idx = sample(1:nrow(Carseats), 200)
seat_trn = Carseats[seat_idx,]
seat_tst = Carseats[-seat_idx,]

seat_tree = rpart(Sales ~ ., data = seat_trn)
summary(seat_tree)

#Note that, the tree is not using all of the available variables.
summary(seat_tree)$used

names(Carseats)[which(!(names(Carseats) %in% summary(seat_tree)$used))]

plot(seat_tree)
text(seat_tree, pretty = 0)
title(main = "Unpruned Classification Tree")

seat_trn_pred = predict(seat_tree, seat_trn, type = "class")
seat_tst_pred = predict(seat_tree, seat_tst, type = "class")

# train confusion
table(predicted = seat_trn_pred, actual = seat_trn$Sales)

# test confusion
table(predicted = seat_tst_pred, actual = seat_tst$Sales)

accuracy = function(actual, predicted) {
  mean(actual == predicted)
}

# train acc
accuracy(predicted = seat_trn_pred, actual = seat_trn$Sales)
# test acc
accuracy(predicted = seat_tst_pred, actual = seat_tst$Sales)

#Here it is easy to see that the tree has been over-fit. The train set performs much #better than the test set.

#We will now use cross-validation to find a tree by considering trees of different sizes #which have been pruned from our original tree.

set.seed(3)
seat_tree_cv = rpart(seat_tree, FUN = prune.misclass)

# index of tree with minimum error
min_idx = which.min(seat_tree_cv$dev)
min_idx

# number of terminal nodes in that tree
seat_tree_cv$size[min_idx]

# misclassification rate of each tree
seat_tree_cv$dev / length(seat_idx)

par(mfrow = c(1, 2))
# default plot
plot(seat_tree_cv)
# better plot
plot(seat_tree_cv$size, seat_tree_cv$dev / nrow(seat_trn), type = "b",xlab = "Tree Size", ylab = "CV Misclassification Rate")

seat_tree_prune = prune.misclass(seat_tree, best = 9)
summary(seat_tree_prune)

plot(seat_tree_prune)
text(seat_tree_prune, pretty = 0)
title(main = "Pruned Classification Tree")

# train
seat_prune_trn_pred = predict(seat_tree_prune, seat_trn, type = "class")
table(predicted = seat_prune_trn_pred, actual = seat_trn$Sales)

accuracy(predicted = seat_prune_trn_pred, actual = seat_trn$Sales

# test
seat_prune_tst_pred = predict(seat_tree_prune, seat_tst, type = "class")
table(predicted = seat_prune_tst_pred, actual = seat_tst$Sales)

accuracy(predicted = seat_prune_tst_pred, actual = seat_tst$Sales)