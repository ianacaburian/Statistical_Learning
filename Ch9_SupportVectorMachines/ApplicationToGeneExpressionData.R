library(e1071)
library(ISLR)
#______________________________________________________________________________

# Khan data set.
# Consists of a number of tissue samples corresponding to four distinct types
# of small round blue cell tumors. For each tissue sample, expression
# measurements for 2308 genes are available. 
names(Khan)
#> "xtrain" "xtest"  "ytrain" "ytest"
#   The data set is already split into training and test sets.
dim(Khan$xtrain)
#> 63 2308
dim(Khan$xtest)
#> 20 2308
length(Khan$ytrain)
#> 63
length(Khan$ytest)
#> 20

table(Khan$ytrain)
#>  1  2  3  4 
#>  8 23 12 20 

table(Khan$ytest)
#> 1 2 3 4 
#> 3 6 6 5 

# Predict cancer subtype using gene expression measurements through a 
# support vector approach. In this data set, there are a very large number of
# features relative to the number of observations. This suggests that we should
# use a linear kernel, because the additional flexibility that will result from
# using a polynomial or radial kernel is unnecessary.
dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out <- svm(y ~ ., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y)
#>      1  2  3  4
#>   1  8  0  0  0
#>   2  0 23  0  0
#>   3  0  0 12  0
#>   4  0  0  0 20

#   There are no training errors.
#   This should not be surprising because the large number of variables
#   relative to the number of observations implies that it is easy to find the
#   hyperplanes that fully separate the classes. 

# Evaluate performance on test set.
dat.te <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te = predict(out, newdata = dat.te)
table(pred.te, dat.te$y)
#> pred.te 1 2 3 4
#>       1 3 0 0 0
#>       2 0 6 2 0
#>       3 0 0 4 0
#>       4 0 0 0 5

# Using cost = 10 yields two test set errors on this data.
