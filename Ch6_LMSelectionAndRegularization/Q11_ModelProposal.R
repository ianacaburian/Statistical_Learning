library(MASS)
library(leaps)
library(glmnet)
library(ISLR)
library(pls)
attach(Boston)
sum(is.na(Boston)) #> 0

x.all <- model.matrix(crim ~ ., data = Boston)
y.all <- Boston$crim 
testMSEs <- rep(0, 5)

# a)
#_______________________________________________________________________________


# Best Subset Selection

predict.regsubsets <- function(object, newdata, id, ...)
{
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars]%*%coefi
}

k <- 10
p <- ncol(Boston) - 1
set.seed(1)
folds = sample(rep(1:k, length = nrow(Boston)))                      
    # Text book uses replace = TRUE here, however it also stresses that folds
    # must be the same size and non-overlapping. Therefore, the default 
    # replace = FALSE is used.

cv.errors <- matrix(NA, k, p, dimnames = list(NULL, paste(1:p)))

for (j in 1:k)
{
    best.fit <- regsubsets(crim ~ ., data = Boston[folds != j,], nvmax = p)
    for (i in 1:p)
    {
        pred <- predict(best.fit, Boston[folds == j,], id = i)
        cv.errors[j, i] <- mean((y.all[folds == j] - pred)^2)
    }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
best.model <- which.min(mean.cv.errors) #> 9
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = 'b', xlab = "Number of Variables")
    # We see that CV selects a 9-var model.
coef(best.fit, id = 9)
    # Variables: zn, indus, nox, dis, rad, ptratio, black, lstat, medv.

testMSEs[1] <- mean.cv.errors[best.model] 
testMSEs[1] #> 43.47287
#_______________________________________________________________________________


# Ridge Regression with CV selected tuning parameter.

x <- model.matrix(crim ~ ., Boston)[, -1]
y <- Boston$crim
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

# Select tuning parameter via CV
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestRidgeLambda <- cv.out$lambda.min
bestRidgeLambda #> 0.7908625

# Test the selected model and report its test error.
grid = 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = bestRidgeLambda, newx = x[test, ])
testMSEs[2] <- mean((ridge.pred - y.test)^2)
testMSEs[2] #> 38.36587
#_______________________________________________________________________________


# Lasso with CV selected tuning parameter.

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)

bestLassoLambda <- cv.out$lambda.min
bestLassoLambda #> 0.09979553
lasso.pred <- predict(lasso.mod, s = bestLassoLambda, newx = x[test,])
testMSEs[3] <- mean((lasso.pred - y.test)^2) 
testMSEs[3] #> 38.3096

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestLassoLambda)
lasso.coef #> 12 non-zero coefficients.
#_______________________________________________________________________________


# PCR

set.seed(1)
pcr.fit <- pcr(crim ~ ., data = Boston, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
    # 13 components appear to have the lowest MSEP.
summary(pcr.fit)
    # The 13 component model has CV RMSEP of 7.025 - the lowest.

M <- 13
pcr.pred <- predict(pcr.fit, x[test,], ncomp = M)
testMSEs[4] <- mean((pcr.pred - y.test) ^2)
testMSEs[4] #> 39.27592
#_______________________________________________________________________________


# PLS

set.seed(1)
pls.fit <- plsr(crim ~ ., data = Boston, subset = train, scale = TRUE, validation = "CV")
validationplot(pls.fit, val.type = "MSEP")
    # From 9 components onwards, RMSEP (at its lowest) appears to be 
    # indistinguishable.
summary(pls.fit)
    # The lowest adjCV RMSEP of 6.99 comes from the 10 component model.
    # However, like the plot, the CV is reported to be almost equal from
    # 9 to 13 components. 

M <- 10
pls.pred <- predict(pls.fit, x[test,], ncomp = M)
testMSEs[5] <- mean((pls.pred - y.test)^2) 
testMSEs[5] #> 39.26028
#_______________________________________________________________________________


# b)

testMSEs #> 43.47287 38.36587 38.30960 39.27592 39.26028
#_______________________________________________________________________________
