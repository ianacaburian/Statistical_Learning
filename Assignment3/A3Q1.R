library(tree)
library(randomForest)

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
summary(tree.init) # Table 1
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
plot(tree.pruned) # Figure 3
text(tree.pruned, pretty = 0)

# Evaluate the performance of the pruned tree on the test data.
yhat.pruned <- predict(tree.pruned, newdata = Wine.test)
mse.pruned <- testMSE(yhat.pruned) #> 0.5230838