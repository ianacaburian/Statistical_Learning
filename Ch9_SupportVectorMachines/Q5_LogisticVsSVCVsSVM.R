library(e1071)

# A non-linear decision boundary can also be obtained by performing logistic
# regression using non-linear transformation of the features.
set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1 ^ 2 - x2 ^ 2 > 0)

plot(x1, x2, xlab = "X1", ylab = "X2", col = (3 - y), pch = (3 - y))

# Fit logistic regresion to the data 
logit.fit <- glm(y ~ x1 + x2, family = "binomial")
summary(logit.fit)

# Apply this model to the training data in order to obtain a predicted class
# label for each training observation.
data <- data.frame(x1 = x1, x2 = x2, y = y)
probs <- predict(logit.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1,]$x1, data[preds == 1,]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0,]$x1, data[preds == 0,]$x2, col = (4 - 0), pch = (3 - 0))
#   The decision boundary appears to be linear.

# Fit a logistic regression model to the data using non-linear functions of
# X1 and X2 as predictors (e.g. X1^2, X1:X2, log(X2), etc.)
logitnl.fit <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), family = "binomial")
summary(logitnl.fit)

# Apply this new model to the training data in order to obtain a predicted class
# label for each training observation.
data <- data.frame(x1 = x1, x2 = x2, y = y)
probs <- predict(logitnl.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1,]$x1, data[preds == 1,]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0,]$x1, data[preds == 0,]$x2, col = (4 - 0), pch = (3 - 0))
#   The decision boundary appears to be non-linear and very close to the true
#   boundary.

# Fit a SVC (linear) to the data and plot.
data$y <- as.factor(data$y)
svm.fit <- svm(y ~ x1 + x2, data, kernel = "linear", cost = 0.01)
preds <- predict(svm.fit, data)
plot(data[preds == 0,]$x1, data[preds == 0,]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1,]$x1, data[preds == 1,]$x2, col = (4 - 1), pch = (3 - 1))
#   All points classified to a single class.

# Fit a SVM to the data and plot.
data$y <- as.factor(data$y)
svmnl.fit <- svm(y ~ x1 + x2, data, kernel = "radial", gamma = 1)
preds <- predict(svmnl.fit, data)
plot(data[preds == 0,]$x1, data[preds == 0,]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1,]$x1, data[preds == 1,]$x2, col = (4 - 1), pch = (3 - 1))
#   The result is very close to the non-linear logistic regression (and thus the 
#   true boundary).

# Comments.
#   As expected, the linear-based models performed very poorly because the true
#   boundary was non-linear.
#   Also demonstrated was the ease in which SVM allows to be tuned between
#   linearity and non-linearity. 
#   With logistic regression, the model itself could not be tuned, requiring
#   the data to be transformed into producing a non-linear model.
#   With SVM, the model allows for tuning via the kernel parameter for easier
#   fitting to either linear or non-linear modelling.