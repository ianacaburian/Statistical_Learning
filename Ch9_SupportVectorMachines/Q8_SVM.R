library(e1071)
library(ISLR)
set.seed(1)
train <- sample(nrow(OJ), 800)
OJ.train <- OJ[train,]
OJ.test <- OJ[-train,]
ter <- rep(0, 3)
#______________________________________________________________________________

# Fit linear SVM with cost = 0.01.
svm.linear <- svm(Purchase ~ ., data = OJ.train, kernel = "linear", cost = 0.01)
summary(svm.linear)
#> 432 support vectors.
#> ( 215 217 )
#> Levels: CH MM

# Training error rate for cost = 0.01.
train.pred <- predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)
#>     train.pred
#>       CH  MM
#>   CH 439  55
#>   MM  78 228
(78 + 55) / nrow(OJ.train)
#> 16.625% training error rate.

# Test error rate for cost = 0.01.
test.pred <- predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)
#>     test.pred
#>       CH  MM
#>   CH 141  18
#>   MM  31  80
(31 + 18) / nrow(OJ.test)
#> 18.14815% test error rate.

# CV-select optimal cost value.
set.seed(2)
tune.out <- tune(svm, Purchase ~ ., data = OJ.train, kernel = "linear",
                 ranges = list(cost = 10 ^ seq(-2, 1, by = 0.25)))
summary(tune.out)
#> best cost = 0.1

# Fit linear SVM with CV-selected optimal cost.
svm.linear2 <- svm(Purchase ~ ., data = OJ.train, kernel = "linear", cost = 0.1)
summary(svm.linear2)
#> 343 Support vectors.
#> ( 171 172 )
#> Levels: CH MM

# Training error rate for cost = 0.1.
train.pred2 <- predict(svm.linear2, OJ.train)
table(OJ.train$Purchase, train.pred2)
#>     train.pred2
#>       CH  MM
#>   CH 438  56
#>   MM  71 235
(71 + 56) / nrow(OJ.train)
#> 15.875% training error rate.

# Test error rate for cost = 0.1.
test.pred2 <- predict(svm.linear2, OJ.test)
table(OJ.test$Purchase, test.pred2)
#>     train.pred2
#>       CH  MM
#>   CH 140  19
#>   MM  32  79
ter[1] <- (32 + 19) / nrow(OJ.test)
#> 18.88889% test error rate.
#______________________________________________________________________________

# Fit radial SVM with default gamma and cost.
svm.radial <- svm(Purchase ~ ., data = OJ.train, kernel = "radial")
summary(svm.radial)
#> 379 support vectors.
#> ( 188 191 )
#> Levels: CH MM

# Training error rate for default cost.
train.pred <- predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)
#>     train.pred
#>       CH  MM
#>   CH 455  39
#>   MM  77 229
(77 + 39) / nrow(OJ.train)
#> 14.5% training error rate.

# Test error rate for cost = 0.01.
test.pred <- predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)
#>     test.pred
#>       CH  MM
#>   CH 141  18
#>   MM  28  83
ter[2] <- (28 + 18) / nrow(OJ.test)
#> 17.03704% test error rate.

# CV-select optimal cost value.
set.seed(2)
tune.out <- tune(svm, Purchase ~ ., data = OJ.train, kernel = "radial",
                 ranges = list(cost = 10 ^ seq(-2, 1, by = 0.25)))
summary(tune.out)
#> best cost = 1
#______________________________________________________________________________

# Fit polynomial SVM of degree 2.
svm.poly <- svm(Purchase ~ ., data = OJ.train, kernel = "polynomial", degree = 2)
summary(svm.poly)
#> 454 support vectors.
#> ( 224 230 )
#> Levels: CH MM

# Training error rate for default cost = 1.
train.pred <- predict(svm.poly, OJ.train)
table(OJ.train$Purchase, train.pred)
#>     train.pred
#>       CH  MM
#>   CH 461  33
#>   MM 105 201
(105 + 33) / nrow(OJ.train)
#> 17.25% training error rate.

# Test error rate for cost = 0.01.
test.pred <- predict(svm.poly, OJ.test)
table(OJ.test$Purchase, test.pred)
#>     test.pred
#>       CH  MM
#>   CH 149  10
#>   MM  41  70
(41 + 10) / nrow(OJ.test)
#> 18.88889% test error rate.

# CV-select optimal cost value.
set.seed(2)
tune.out <- tune(svm, Purchase ~ ., data = OJ.train, kernel = "polynomial", degree = 2,
                 ranges = list(cost = 10 ^ seq(-2, 1, by = 0.25)))
summary(tune.out)
#> best cost = 10

# Fit linear SVM with CV-selected optimal cost.
svm.poly2 <- svm(Purchase ~ ., data = OJ.train, kernel = "polynomial", degree = 2, cost = 10)
summary(svm.poly2)
#> 342 Support vectors.
#> ( 170 172 )
#> Levels: CH MM

# Training error rate for cost = 10.
train.pred2 <- predict(svm.poly2, OJ.train)
table(OJ.train$Purchase, train.pred2)
#>     train.pred2
#>       CH  MM
#>   CH 450  44
#>   MM  72 234
(72 + 44) / nrow(OJ.train)
#> 14.5% training error rate.

# Test error rate for cost = 10.
test.pred2 <- predict(svm.poly2, OJ.test)
table(OJ.test$Purchase, test.pred2)
#>     train.pred2
#>       CH  MM
#>   CH 140  19
#>   MM  31  80
ter[3] <- (31 + 19) / nrow(OJ.test)
#> 18.51852% test error rate.

which.min(ter)
#> 2
#   Radial SVM appears to be the best as it produces the lowest test error rate.