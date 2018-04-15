library(ISLR)
# fix(Hitters)
names(Hitters)
dim(Hitters) #=> 322 20
sum(is.na(Hitters$Salary)) #=> 59

Hitters <- na.omit(Hitters) # remove all of the rows that have missing values
dim(Hitters) #=> 263 20
sum(is.na(Hitters)) #=> 0
#_______________________________________________________________________________

# Validation Set Approach

library(leaps)
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test <- (!train)
regfit.best <- regsubsets(Salary ~ ., data = Hitters[train,], nvmax = 19)
    # access the training data only then apply regsubsets() to perform best 
    # subset selection.

test.mat = model.matrix(Salary ~ ., data = Hitters[test,])
    # Make a model matrix for the test data

val.errors = rep(NA, 19)
for(i in 1:19)
{
    coefi <- coef(regfit.best, id = i)
    pred <- test.mat[, names(coefi)]%*%coefi
    val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}
    # for each size i, extract the coefficients from regfit.best for the best
    # model of that size, multiply them into the appropriate columns of the 
    # test model matrix to form the predictions, and compute the test MSE.
    # This is tedious because there is no predict() method for regsubsets().

val.errors
which.min(val.errors) #=> 10
    # We find that the best model is the one that contains 10 variables.
coef(regfit.best, 10)

predict.regsubsets <- function(object, newdata, id, ...)
{
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars]%*%coefi
}
    # Function written to perform the above steps later on.

regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 10)
    # Finally, best subset selection is performed on the full data set and
    # the best 10 var model is selected.
    # Notice that the best 10 var model on the full data set has a different
    # set of vars than the best 10 var model on the training set.


# Choose among the models of different sizes using CV.
# That is, perform best subset selection within each of the k training sets.

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
    # Create a vector that allocates each observation to one of k = 10 folds,
    # and then create a matrix in which we will store the results.

for (j in 1:k)
{
    best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j,], nvmax = 19)
    for (i in 1:19)
    {
        pred <- predict(best.fit, Hitters[folds == j,], id = i)
        cv.errors[j, i] <- mean((Hitters$Salary[folds == j] - pred)^2)
    }
}
    # In the jth fold, the elements of folds that equal j are in the test set,
    # and the remainder are in the training set. We make our predictions for
    # each model size (using our new predict() method), compute the test errors
    # on the appropriate subset, and store them in the appropriate slot in the
    # matrix cv.errors.
    # This gives a 10 x 19 matrix, of which the (i, j)th element corresponds
    # to the test MSE for the ith CV fold for the best j-var model.

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors 
    # apply() will average over the columns of this matrix in order to obtain
    # a vector for which the jth element is the cross-validation error for
    # the j-var model

par(mfrow = c(1, 1))
plot(mean.cv.errors, type = 'b')
    # We see that CV selects an 11-var model.

reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 11)
    # Best subset selection is now performed on the full data set in order
    # to obtain the 11-var model.
