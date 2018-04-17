library(glmnet)
librar(ISLR)
library(pls)
attach(College)
#_______________________________________________________________________________


# a)

set.seed(11)
train <- sample(nrow(College), nrow(College)/2)
test <- (-train)

College.train <- College[train, ]
College.test <- College[test, ]

testErrors <- rep(0, 5)
#_______________________________________________________________________________


# b)

names(College) # "Apps" = Number of Applications received

lm.fit <- lm(Apps ~., data = College.train)
lm.pred <- predict(lm.fit, newdata = College.test)

OLStestMSE <- mean((College.test$Apps - lm.pred)^2)
OLStestMSE #> 1538442

testErrors[1] <- OLStestMSE
#_______________________________________________________________________________


# c)

x.train <- model.matrix(Apps ~ ., data = College.train)
x.test <- model.matrix(Apps ~., data = College.test)
y.train <- College.train$Apps
y.test <- College.test$Apps

lambdas <- 10^seq(4, -2, length = 100)

ridgeCV.model <- cv.glmnet(x.train, y.train, alpha = 0, lambda = lambdas, thresh = 1e-12)
bestlambda <- ridgeCV.model$lambda.min
bestlambda #> 18.73817

# Report test error
ridge.pred <- predict(ridgeCV.model, s = bestlambda, newx = x.test)
testErrors[2] <- mean((ridge.pred - y.test)^2) #> 1608859
    # The testMSE for ridge regression is higher than the OLS testMSE
#_______________________________________________________________________________


# d)

lassoCV.model <- cv.glmnet(x.train, y.train, alpha = 1, lambda = lambdas, thresh = 1e-12)
bestlambda <- lassoCV.model$lambda.min
bestlambda #> 21.54435

# Report test error
lasso.pred <- predict(lassoCV.model, s = bestlambda, newx = x.test)
testErrors[3] <- mean((lasso.pred - y.test)^2) #> 1635280

# Number of non-zero coefficient estimates
x.all <- model.matrix(Apps ~ ., data = College)
y.all <- College$Apps
lasso.model <- glmnet(x.all, y.all, alpha = 1)
lasso.coef <- predict(lasso.model, s = bestlambda, type = "coefficients")
    # 16 non-zero coefficients
#_______________________________________________________________________________


# e)

pcr.fit <- pcr(Apps ~ ., data = College, subset = train, scale = T, validation = "CV", nvmax = 25)
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP") 
    # lowest appears to be M = 10.
M <- 10 

# Report test error
pcr.pred <- predict(pcr.fit, College.test, ncomp = M)
testErrors[4] <- mean((y.test - pcr.pred)^2) #> 3014496
#_______________________________________________________________________________


# f)

pls.fit <- plsr(Apps ~ ., data = College, subset = train, scale = T, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
    # lowest appears to be M = 12 with adjCV 1095

# Report test error 
pls.pred <- predict(pls.fit, College.test, ncomp = M)
testErrors[5] <- mean((pls.pred - y.test)^2) #> 1508987
#_______________________________________________________________________________


# g)

testErrors #> 1538442 1608859 1635280 3014496 1508987
which.min(testErrors) #> 5
    # PLS gives the lowest test MSE and therefore appears to be the best at
    # predicting the number of applications received given all the predictors.