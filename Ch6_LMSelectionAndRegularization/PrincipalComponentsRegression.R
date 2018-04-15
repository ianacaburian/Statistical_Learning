library(ISLR)
# fix(Hitters)
names(Hitters)
dim(Hitters) #=> 322 20
sum(is.na(Hitters$Salary)) #=> 59

Hitters <- na.omit(Hitters) # remove all of the rows that have missing values
dim(Hitters) #=> 263 20
sum(is.na(Hitters)) #=> 0

x <- model.matrix(Salary ~ ., Hitters)[,-1]
y <- Hitters$Salary
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
#_______________________________________________________________________________


# Principal Components Regression

library(pls)
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")
    # scale = TRUE standardizes each predictor, using 6.6, prior to generating
    # the principal components.
summary(pcr.fit)
    # CV score is provided for each possible number of components, ranging from
    # M = 0 onwards.
    # pcr() reports the root mean squared error, so in order to obtain the 
    # usual MSE, this quantity must be squared.
    # Percentage of variance explained in the predictors and in the response
    # using different numbers of components is also provided (discussed in Ch10)

validationplot(pcr.fit, val.type = "MSEP")
    # CV scores (MSE) plot shows that the smallest CV error occurs when M = 16
    # components are used. Being barely fewer than M = 19 amounts to simply
    # performing OLS, because when all of the components are used in PCR, no
    # dimension reduction occurs.
    # However, this plot also shows that the CV error is roughly the same when
    # only one component is included in the model. This suggests that a model
    # that uses just a small number of components might suffice.


# Perform PCR on the training data and evaluate its test set performance.

set.seed(1)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
    # Now we find that the lowest CV error occurs when M = 7 components are
    # used.

pcr.pred <- predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test) ^2) #=> 96556
    # This test MSE is competitive with the results obtained using ridge
    # regression and the lasso.
    # However, the final model is less interpretable as it does not perform
    # variable selection or even directly produce coefficient estimates.

pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)
    # Finally, PCR is fit on the full data set, using M = 7, the number of 
    # components identified by CV.
#_______________________________________________________________________________

# Partial Least Squares

set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
    # The lowest CV occurs when only M = 2 partial least squares directions 
    # are used.
validationplot(pls.fit, val.type = "MSEP")

pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2) #=> 101417
    # The test set MSE is comparable to, but slightly higher than, the test MSE
    # obtained using ridge regression, the lasso, and PCR.

pls.fit <- plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)
    # PLS is performed using the full data set, using M = 2, the number of 
    # components identified by cross-validation.
    # Notice that the percentage of variance in Salary that the two-component
    # PLS fit explains, 46.40% is almost as much as taht explained using the 
    # final seven-component model PCR fit, 46.69%. This is because PCR only
    # attempts to maximize the amount of variance explained in the predictors,
    # while PLS searches for directions that explain variance in both the 
    # predictors and the response.