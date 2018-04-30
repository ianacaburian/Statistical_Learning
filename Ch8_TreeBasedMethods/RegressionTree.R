library(tree)
library(MASS)
#______________________________________________________________________________

# Fit regression tree
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)
#   Notice that only three of the variables have been used in constructing
#   the tree, the deviance is simply the sum of squared errors for the tree.

plot(tree.boston)
text(tree.boston, pretty = 0)
#   The tree indicates that lower values of lstat (perc of people with lower
#   socioeconomic status) corresponds to more expensive houses.
#   The tree predicts a median house price of $46,400 for larger homes in
#   in suburbs in which residents have high socioeconomic status
#   rm >= 7.437 and lstat < 9.715.

# Test if tree pruning improves performance
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')
#   Cv has selected the most complex tree (8), i.e. unpruned.

# Prune the tree anyways
prune.boston <- prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

# Use the cv selected tree (unpruned) to predict on test data
yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0, 1)
mean((yhat - boston.test) ^ 2)
#> 25.05
#   This is the test set MSE associated with the regression tree.
#   Therefore, the root MSE is 5.005, indicating that this model leads to
#   test predictions that are within around $5005 of the true median home
#   value for the suburb.