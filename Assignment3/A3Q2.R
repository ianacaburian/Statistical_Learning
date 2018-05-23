library(e1071)
load("datacens.Rdata")

# Inspect the data.
str(datacens$train) # Table 7
sd(datacens$train$age) # Table 8
sd(datacens$train$hr_per_week) # Table 8
mean(datacens$train$age) # Table 8
mean(datacens$train$hr_per_week) # Table 8

# Partition the data to avoid excessive completion time.
set.seed(330)
train <- sample(nrow(datacens$train), 500)

# Fit an SVM with unscaled data.
svmfit <- svm(income ~ ., data = datacens$train[train,], kernel = "radial",
              scale = F)
ypred <- predict(svmfit, datacens$val)
table(predict = ypred, truth = datacens$val$income) # Table 9
(263 + 1860) / nrow(datacens$val) # 23.73%

# Fit an SVM with scaled data.
svmfitsc <- svm(income ~ ., data = datacens$train[train,], kernel = "radial",
                scale = T)
ypredsc <- predict(svmfitsc, datacens$val)
table(predict = ypredsc, truth = datacens$val$income) # Table 9
(311 + 1216) / nrow(datacens$val) # 17.07%

# Determine the best SVM parameters
tune.out <- tune(svm, income ~ ., data = datacens$train[train,], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out) # Table 11: best cost = 100, gamma = 0.5

# Fit an SVM to the training set using the best tuning parameters.
svmbestfit <- svm(income ~ ., data = datacens$train, kernel = "radial",
                  scale = T, cost = 100, gamma = 0.5)
ypredtrain <- predict(svmbestfit, datacens$train)
table(predict = ypredtrain, truth = datacens$train$income) # Table 13
#>        truth
#> predict     0     1
#>       0 15449   966
#>       1   538  4261
(538 + 966) / nrow(datacens$train)
# 7.09%

ypredtest <- predict(svmbestfit, datacens$val)
table(predict = ypredtest, truth = datacens$val$income) # Table 15
#>        truth
#> predict    0    1
#>       0 5967  954
#>       1  700 1327
(700 + 954) / nrow(datacens$val)
# 18.48%

# Produce a plot illustrating the hyperplane and the 
# testing dataset on two dimensions.
train2d <- datacens$train[c("age", "hr_per_week", "income")]
svm2d <- svm(income ~ ., data = train2d, kernel = "radial",
             scale = T, cost = 100, gamma = 0.5)
test2d <- datacens$val[c("age", "hr_per_week", "income")]
plot(svm2d, test2d) # Figure 9