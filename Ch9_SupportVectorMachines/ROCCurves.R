library(e1071)
library(ROCR)
#______________________________________________________________________________

set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
train <- sample(200, 100)

# Plot an ROC curve given a vector containing a numerical score for each
# observation, pred, and a vector containing the class label for each 
# observation, truth.
rocplot <- function(pred, truth, ...) {
    predob <- prediction(pred, truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
}

# Obtain fitted values for each observation, i.e. the numerical scores used
# to obtain the class labels.
svmfit.opt <- svm(y ~ ., data = dat[train,], kernel = "radial",
                         gamma = 2, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.opt, dat[train,], decision.values = T))$decision.values

# ROC plot.
par(mfrow = c(1, 2))
rocplot(fitted, dat[train, "y"], main = "Training Data")
#   SVM appears to be producing accurate predictions.

# Increase gamma to produce a more flexible fit for accuracy improvements.
svmfit.flex <- svm(y ~ ., data = dat[train,], kernel = "radial",
                   gamma = 50, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.flex, dat[train,], decision.values = T))$decision.values
rocplot(fitted, dat[train, "y"], add = T, col = "red")
#   Note that these curves are fitted on the training data.

# Compute ROC curves on the test data.
fitted <- attributes(predict(svmfit.opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], main = "Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train, "y"], add = T, col = "red")