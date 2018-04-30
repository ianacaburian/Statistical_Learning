library(gbm)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
#______________________________________________________________________________

# Fit boosted regression trees
set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
#   If it were a binary classification problem, we would use 
#   distribution = "bernoulli". n.trees = 5000 indicates that we want
#   5000 trees, and the option interaction.depth = 4 limits the depth
#   of each tree.

summary(boost.boston)
#   We see that lstat and rm are by far the most important variables.

# Partial dependence plots for lstat and rm.
par(mfrow = c(1, 2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")
#   These plots illustrate the marginal effect of the selected variables on
#   the response after integrating out the other variables.
#   In this case, as we might expect, median house prices are increasing
#   with rm and decreasing with lstat.

# Predict medv on the test set
yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test) ^ 2)
#> 11.8
#   This test MSE is similar to the test MSE for random forests and
#   superior to that for bagging.

# Perform boosting with a different shrinkage parameter lambda (eq 8.10).
boost.boston <- gbm(medv ~ ., data = Boston[train,], distribution = "gaussian",
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.2,
                    verbose = F)
#   The shrinkage parameter lambda = 0.2 (default is 0.001).

yhat.boost <- predict(boost.boston, newdata = Boston[-train,], n.trees = 5000)
mean((yhat.boost - boston.test) ^ 2)
#> 11.84434
#   Lambda = 0.2 leads to a slightly lower test MSE than lambda = 0.001
