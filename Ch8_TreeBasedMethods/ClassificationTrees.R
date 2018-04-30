library(tree)
library(ISLR)
attach(Carseats)
High <- ifelse(Sales <= 8, "No", "Yes") 
#   Convert "Sales" from continuous to discrete variable
Carseats <- data.frame(Carseats, High)
#   Merge High with the rest of Carseats
#______________________________________________________________________________

# Fit classification tree that predicts High
tree.carseats <- tree(High ~ . - Sales, Carseats) # Sales excluded for High

summary(tree.carseats)
#> Residual mean deviance:0.4575 = 170.7 / 373
#   This is the deviance 170.7 (see equation in text book) divided by
#   n - |T0| = 400 - 27 = 373
#> Misclassification error rate:0.09 = 36 / 400
#   This is the training error rate.


# Plot the tree structure
plot(tree.carseats)
text(tree.carseats, pretty = 0)
#   The most important indicator of Sales appears to be shelving location,
#   since it is the first branch.

# Print output corresponding to each branch of the tree
tree.carseats
#> node), split, n, deviance, yval, (yprob)
#>      * denotes terminal node
#   This is the format. split = criterion for splitting the node. 
#   n = num obs in the branch. yval = overall prediction for the branch.
#   yprob = the fraction of observations in that branch that take on (Yes No)
#   Terminal nodes are indicated using asterisks.

#> 1) root 400 541.500 No(0.59000 0.41000)
#> 2) ShelveLoc:Bad, Medium 315 390.600 No(0.68889 0.31111)

# Evaluate performance on test data.
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
#   Data is now split into training and test
tree.carseats = tree(High ~ . - Sales, Carseats, subset = train)
#   Fit tree using the training data
tree.pred <- predict(tree.carseats, Carseats.test, type = "class")
#   "class" instructs R to return the actual class prediction
table(tree.pred, High.test)
#>          High.test
#> tree.pred No Yes
#>       No  86  27
#>       Yes 30  57
(86 + 57) / 200
#> 0.715
#   This approach has lead to correct predictions for around 71.5% of the 
#   locations in the test data set.

# Tree pruning
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
#   Performs cv in order to determine the optimal level of tree complexity;
#   cost complexity pruning is used in order to select a sequence of trees for
#   consideration. FUN = prune.misclass in order to indicate that we want the
#   classification error rate to guide the cv and pruning process, rather than
#   the default for the cv.tree() function, which is deviance.
names(cv.carseats)
#   "dev" corresponds to the cv error rate.
cv.carseats
#   Reports the number of terminal nodes (size) of each tree considered as well
#   as the corresponding error rate and the value of the cost-complexity
#   parameter used (k, which corresponds to alpha in equation 8.4).
#> $`size`
#> [1] 19 17 14 13 9 7 3 2 1
#> $dev
#> [1] 55 55 53 52 50 56 69 65 80
#   The tree with 9 terminal nodes (5th tree) corresponds with the lowest
#   cv error rate, with 50 cv errors.

# Plot error rate as a function of both size and k.
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# Prune the tree to obtain the nine-node tree.
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# Evaluate the performance of the pruned tree on the test data.
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
#>          High.test
#> tree.pred No Yes
#>       No  94  24
#>       Yes 22  60
(94 + 60) / 200
#> 0.77
#   77% of the test observations are correctly classified, so not only has
#   the pruning process produced a more interpretable tree, but it has also
#   improved the classification accuracy.

# Check another tree's accuracy (try the 15 node tree)
prune.carseats <- prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
#>          High.test
#> tree.pred No Yes
#>       No  86  22
#>       Yes 30  62
(86 + 62) / 200
#> 0.74