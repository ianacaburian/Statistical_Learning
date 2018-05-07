library(e1071)

str(datacens$train) # Table 3
sd(datacens$train$age) # Table 4
sd(datacens$train$hr_per_week) # Table 4
mean(datacens$train$age) # Table 4
mean(datacens$train$hr_per_week) # Table 4

svmfit <- svm(income ~ ., data = datacens$train[1:10000,], kernel = "radial",
              scale = F)
ypred <- predict(svmfit, datacens$val)
table(predict = ypred, truth = datacens$val$income) # Table 5
(308 + 1263) / nrow(datacens$val)
# 17.557%

svmfitsc <- svm(income ~ ., data = datacens$train[1:10000,], kernel = "radial",
                scale = T)
ypredsc <- predict(svmfitsc, datacens$val)
table(predict = ypredsc, truth = datacens$val$income) # Table 5
(351 + 1054) / nrow(datacens$val)
# 15.70%

