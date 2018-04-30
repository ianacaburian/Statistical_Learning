library(randomForest)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
#______________________________________________________________________________
 
set.seed(1)
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, importance = TRUE)
#   recall that bagging is a special case of random forest with m = p.
#   Therefore, the randomForest() can also be used for bagging.
bag.boston
#   mtry = 13 indicates taht all 13 predictors should be considered
#   for each split of the tree - in other words, that bagging should be done.

# How well does this bagged model perform on the test set?
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0, 1)
mean((yhat.bag - boston.test) ^ 2)
#> 13.50808
#   The test set MSE associated with the bagged regression tree is 13.16,
#   almost half that obtained using an optimally-pruned single tree.

# Change the number of tree grown by randomForest() using ntree
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test) ^ 2)
#> 13.94835

# Grow a random forest by using smaller value for mtry
#   By default, randomForest() uses mtry = p/3 for regression trees, and
#   m = sqrt(p) for classification trees.
set.seed(1)
rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test) ^ 2)
#> 11.66454
#   This test MSE indicates taht random forests yielded an improvement over
#   bagging in this case.

# View the importance of each variable
importance(rf.boston)
#   Two measures of variable importance are reported. 
#   The first is based upon the mean decrease of accuracy in predictions on
#   the out of bag samples when a given variable is excluded from the model.
#   The latter is a measure of the total decrease in node impurity that
#   results from splits over that, variable averaged over all trees
#   (plotted in Figure 8.9). In regression trees, node impurity is measured
#   by the training RSS, and for classification trees by the deviance.

# Plots of importance measures
varImpPlot(rf.boston)
#   The results indicate that across all of the trees considered in the 
#   random forest, the wealth level of the community (lstat) and the house 
#   size (rm) are by far the two most important variables.