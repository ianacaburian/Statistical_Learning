library(e1071)
#______________________________________________________________________________

# Generate observations of two classes.
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1,] <- x[y == 1,] + 1
plot(x, col = (3 - y))
#   The classes appear to not be linearly separable.

# Code the response as a factor for svm() to perform classification, not regression
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = F)
#   scale = F tells svm() not to scale each feature to have mean zero or sd = 1.
#   Depending on the application, scale = T might be preferred.

plot(svmfit, dat)
#   The two features correspond to the two axes.
#   The feature space is linearly divided into two regions of assigned classes.
#   The crosses represent the support vectors. 
#   There appears to be one observation misclassified near the x-axis, 
#   indicated in red among black observations.

# Identify the support vectors.
svmfit$index

summary(svmfit)
#   Reports number of support vectors and how many in each class ( 4 3 ).

# Try a smaller cost value.
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = F)
plot(svmfit, dat)
svmfit$index
#   Smaller cost value -> wider margin -> more support vectors.

# Perform cross validation.
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
#   Default is k = 10-fold CV.

summary(tune.out)
#> best performance: 0.1
#   Setting cost = 0.1 results in the lowest CV error rate.

# Access the best model obtained via tune().
bestmod <- tune.out$best.model
summary(bestmod)
#   16 support vectors, 8 in each class.

# Predict the class label on a set of test observations.
xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = T)
xtest[ytest == 1,] <- xtest[ytest == 1,] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)
#>        truth
#> predict -1  1
#>      -1 11  1
#>      1   0  8

#   19 test observations are correctly classified.

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = .01, scale = F)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)
#>        truth
#> predict -1  1
#>      -1 11  2
#>      1   0  7

#   18 correct classifications result from a smaller cost value.

# Consider a situation in which the two classes are linearly separable.
x[y == 1,] <- x[y == 1,] + 0.5
plot(x, col = (y + 5) / 2, pch = 19)

# Fit the support vector classifier.
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1e5)
#   This time, use a large cost value so that no observations are misclassified.
summary(svmfit)
plot(svmfit, dat)
#   No training errors were made and only 3 support vectors were used.
#   However, we can see that the margin is very narrow (because the observations
#   that are not support vectors are very close to the boundary).
#   It seems likely that this model will perform poorly on test data.

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)
#   With cost = 1 comes a misclassified observation, but we also obtain a much
#   wider margin and make use of seven support vectors.
#   This way, it is more likely that this model will perform better on test data.

