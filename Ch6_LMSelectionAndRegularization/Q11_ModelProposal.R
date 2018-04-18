library(MASS)
library(leaps)
library(glmnet)
library(ISLR)
library(pls)
attach(Boston)
sum(is.na(Boston)) #> 0

x.all <- model.matrix(crim ~ ., data = Boston)
y.all <- Boston$crim 
testMSEs <- rep(0, 7)

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
    # Text book uses replace = TRUE here, which was found to be incorrect as 
    # it results in unequal fold sizes.
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
plot(mean.cv.errors, type = 'b')
    # We see that CV selects a 9-var model.

testMSEs[1] <- mean.cv.errors[best.model] 
testMSEs[1] #> 43.47287
#_______________________________________________________________________________


# Forward Stepwise Selection.

cv.errors <- matrix(NA, k, p, dimnames = list(NULL, paste(1:p)))

for (j in 1:k)
{
    fwd.fit <- regsubsets(crim ~ ., data = Boston[folds != j,], nvmax = p,
                           method = "forward")
    for (i in 1:p)
    {
        pred <- predict(fwd.fit, Boston[folds == j,], id = i)
        cv.errors[j, i] <- mean((y.all[folds == j] - pred)^2)
    }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
best.model <- which.min(mean.cv.errors) #> 13
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = 'b')
    # We see that CV selects a 13-var model.

testMSEs[2] <- mean.cv.errors[best.model] 
testMSEs[2] #> 43.52076
#_______________________________________________________________________________


# Backward Stepwise Selection.

cv.errors <- matrix(NA, k, p, dimnames = list(NULL, paste(1:p)))

for (j in 1:k)
{
    bwd.fit <- regsubsets(crim ~ ., data = Boston[folds != j,], nvmax = p,
                           method = "backward")
    for (i in 1:p)
    {
        pred <- predict(bwd.fit, Boston[folds == j,], id = i)
        cv.errors[j, i] <- mean((y.all[folds == j] - pred)^2)
    }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
best.model <- which.min(mean.cv.errors) #> 13
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = 'b')
    # We see that CV selects a 13-var model.

testMSEs[3] <- mean.cv.errors[best.model] 
testMSEs[3] #> 43.52076
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
testMSEs[4] <- mean((ridge.pred - y.test)^2)
testMSEs[4] #> 38.36587
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
testMSEs[5] <- mean((lasso.pred - y.test)^2) 
testMSEs[5] #> 38.3096

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
testMSEs[6] <- mean((pcr.pred - y.test) ^2)
testMSEs[6] #> 39.27592
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
testMSEs[7] <- mean((pls.pred - y.test)^2) 
testMSEs[7] #> 39.26028
#_______________________________________________________________________________


# b)

testMSEs #> 43.47287 43.52076 43.52076 38.36587 38.30960 39.27592 39.26028
    # According to the corresponding test MSE, the best model appears to be
    # obtained by the lasso.
    # The second lowest test MSE comes from the model obtained by ridge
    # regression which consists of all variables.
#_______________________________________________________________________________


# c)
    # The methods that yield sparse models eliminated up to 6 variables in the
    # best subset selection method and 2 variables in the lasso method.
    # The model obtained by the lasso model is therefore chosen as it not only
    # performs the best in terms of test MSE, but is also more interpretable
    # than most of the other models due to its reduced number of variables.

    # The reason that the lasso method has performed variable selection is 
    # because there exists a lambda where a region of constant RSS intersects 
    # with the polytope shaped constraint region where the coefficient for tax 
    # and age equal 0.




