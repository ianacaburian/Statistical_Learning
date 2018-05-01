library(tree)
library(randomForest)
library(ISLR)
attach(Carseats)
y <- which(names(Carseats) == "Sales")
p <- ncol(Carseats) - 1

set.seed(1)
train <- sample(1:nrow(Carseats), nrow(Carseats) / 2)
Carseats.train <- Carseats[train, - y]
Carseats.test <- Carseats[-train, - y]
Y.train <- Carseats[train, y]
Y.test <- Carseats[-train, y]
#______________________________________________________________________________

# Fit regression tree
tree.carseats <- tree(Sales ~ ., Carseats, subset = train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)

# Test MSE
yhat <- predict(tree.carseats, newdata = Carseats.test)
mean((yhat - Y.test)^2)
#> 4.148897

# Using CV to determine the optimal level of tree complexity
cv.carseats <- cv.tree(tree.carseats)
#   FUN = prune.misclass is omitted as tree is not a classification tree.
cv.carseats
#   8 and 9 both have deviance 1039.212
bestNodeNum <- which.min(cv.carseats$dev)
#> 8

# Plot error rate as a function of both size and k (or alpha).
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# Prune the tree to obtain the best (8-node) tree.
prune.carseats <- prune.tree(tree.carseats, best = bestNodeNum)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# Evaluate the performance of the pruned tree on the test data.
yhat <- predict(prune.carseats, newdata = Carseats.test)
mean((yhat - Y.test) ^ 2)
#> 5.09085
#   Pruning to the CV-selected 8-node tree increases the test MSE from
#   4.148897 to 5.09085. Pruning the tree does not improve the test MSE.

# Use bagging to analyze the data
bag.carseats <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = p, importance = T)
yhat.bag <- predict(bag.carseats, newdata = Carseats.test)
mean((yhat.bag - Y.test) ^ 2)
#> 2.616106

# View the importance of each variable
importance(bag.carseats)
#   %IncMSE: mean decrease of accuracy in predictions on the out of bag 
#   samples when a given variable is excluded from the model.
#   IncNodePurity: total decrease in node impurity that results from splits 
#   over that, variable averaged over all trees (plotted in Figure 8.9). 
#   In regression trees, node impurity is measured by the training RSS.
varImpPlot(bag.carseats)
#   Price and ShelveLoc appear by far to be the most important according
#   to both increase in MSE and node purity.

# Use random forests to analyze the data
# Using m = p/2
rf.carseatsdiv2 <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = p / 2, ntree = 500)
yhat.rfdiv2 <- predict(rf.carseatsdiv2, newdata = Carseats.test)
mean((yhat.rfdiv2 - Y.test) ^ 2)
#> 2.806046

# Using m = sqrt(p)
rf.carseatsrootp <- randomForest(Sales ~ ., data = Carseats, subset = train, mtry = sqrt(p), ntree = 500)
yhat.rfrootp <- predict(rf.carseatsrootp, newdata = Carseats.test)
mean((yhat.rfrootp - Y.test) ^ 2)
#> 3.260077

importance(rf.carseatsdiv2)