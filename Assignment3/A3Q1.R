library(gbm)
library(randomForest)
library(tree)


# Fix and prepare dataset
Wine <- read.csv("winequality-red.csv", sep = ";")
attach(Wine)
dim(Wine) #> 1599 12
sum(is.na(Wine)) #> 0
# fix(Wine)
ycol <- which(names(Wine) == "quality")
p <- ncol(Wine) - 1

# Split the data into training and test sets
set.seed(330)
train <- sample(1:nrow(Wine), 1199)
Wine.train <- Wine[train, - ycol]
Wine.test <- Wine[-train, - ycol]
Y.train <- Wine[train, ycol]
Y.test <- Wine[-train, ycol]
testMSE = function(yhat)
    mean((yhat - Y.test) ^ 2)

# Fit initial regression tree
tree.init <- tree(quality ~ ., Wine, subset = train)
plot(tree.init) # Figure 1
text(tree.init, pretty = 0)

# Test initial tree
yhat.init <- predict(tree.init, newdata = Wine.test)
mse.init <- testMSE(yhat.init) #> 0.5137307

# Cross validate to determine optimal model complexity
cv.Wine <- cv.tree(tree.init)
cv.Wine # Table 2
par(mfrow = c(1, 2)) # Figure 2
plot(cv.Wine$size, cv.Wine$dev, type = "b",
     ylab = "Error Rate", xlab = "Tree Sizes")
plot(cv.Wine$k, cv.Wine$dev, type = "b",
     ylab = "Error Rate", xlab = "Cost Complexity (k)")

# Tree pruning
minDev <- min(cv.Wine$dev) #> 536.4785
bestTreeSizes <- cv.Wine$size[which(cv.Wine$dev == minDev)] #> 11 10 8
tree.pruned <- prune.tree(tree.init, best = min(bestTreeSizes))
par(mfrow = c(1,1))
plot(tree.pruned) # Figure 3
text(tree.pruned, pretty = 0)

# Evaluate the performance of the pruned tree on the test data.
yhat.pruned <- predict(tree.pruned, newdata = Wine.test)
mse.pruned <- testMSE(yhat.pruned) #> 0.5230838

# Fitting and testing boosting tree models
boostTrees = function(ntrees, lambda) {
    tree.boost <- gbm(quality ~ ., data = Wine[train,], distribution = "gaussian",
                      n.trees = ntrees, shrinkage = lambda, interaction.depth = 1,
                      verbose = F)
    yhat.boost <- predict(tree.boost, newdata = Wine.test, n.trees = ntrees)
    return (testMSE(yhat.boost))
}

# Testing performance for different tuning parameter settings.
mse.boost <- rep(0, 6)
mse.boost[1] <- boostTrees(ntrees = 500, lambda = 0.1)        #> 0.4399855
mse.boost[2] <- boostTrees(ntrees = 1000, lambda = 0.1)       #> 0.4473003
mse.boost[3] <- boostTrees(ntrees = 1000, lambda = 0.01)      #> 0.4398412
mse.boost[4] <- boostTrees(ntrees = 10000, lambda = 0.01)     #> 0.4442466
mse.boost[5] <- boostTrees(ntrees = 10000, lambda = 0.001)    #> 0.4404776
mse.boost[6] <- boostTrees(ntrees = 100000, lambda = 0.001)   #> 0.4468656
plot(mse.boost, type = 'l',
     ylab = "Test MSE", xlab = "Tree Index") # Figure 4
which.min(mse.boost) #> 3
tree.boost <- gbm(quality ~ ., data = Wine[train,], distribution = "gaussian",
                  n.trees = 1000, shrinkage = 0.01, interaction.depth = 1)

# Fitting and testing forests
randForest = function(ntrees, m) {
    forest <- randomForest(quality ~ ., data = Wine, subset = train, importance = T,
                           mtry = m, ntree = ntrees)
    yhat.forest <- predict(forest, newdata = Wine.test)
    return (testMSE(yhat.forest))
}

# Bagging
mse.bag <- rep(0, 3)
mse.bag[1] <- randForest(500, p)  #> 0.3813888
mse.bag[2] <- randForest(750, p)  #> 0.3798959
mse.bag[3] <- randForest(1000, p) #> 0.3811326
which.min(mse.bag) #> 2
tree.bag <- randomForest(quality ~ ., data = Wine, subset = train, importance = T,
                         mtry = p, ntree = 750)

# Random Forests
mse.forest <- rep(0, 4)
mse.forest[1] <- randForest(500, p / 2)     #> 0.3763889
mse.forest[2] <- randForest(750, p / 2)     #> 0.3768942
mse.forest[3] <- randForest(500, p / 3)     #> 0.3766073
mse.forest[4] <- randForest(750, p / 3)     #> 0.3736360
mse.forest[5] <- randForest(1000, p / 3)    #> 0.3748047
which.min(mse.forest) #> 4
tree.forest <- randomForest(quality ~ ., data = Wine, subset = train, importance = T,
                            mtry = p / 3, ntree = 750)

# Compare each model's test MSE.
mse.compare <- rep(0, 3)
mse.compare[1] <- mse.boost[which.min(mse.boost)]
mse.compare[2] <- mse.bag[which.min(mse.bag)]
mse.compare[3] <- mse.forest[which.min(mse.forest)]
barplot(mse.compare, ylab = "Test MSE", xlab = "Model") # Figure 5

# Variable analyses of the ensemble models.
summary(tree.boost) # Table 5 and Figure 6
varImpPlot(tree.bag) # Figure 7
varImpPlot(tree.forest) # Figure 8