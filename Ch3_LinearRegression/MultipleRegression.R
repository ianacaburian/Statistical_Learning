library(MASS) # needed for Boston data
attach(Boston)
# lm.fit = lm(medv ~ lstat + age, data = Boston)  # use lstat + age
lm.fit = lm(medv ~ ., data = Boston)              # use all predictors
lm.fit1 = lm(medv ~ . -age, data = Boston)        # use all except age
lm.fit2 = update(lm.fit, ~ . -age)                # same as lm.fit1
summary(lm.fit)

# Individual Components
# ?summary.lm # see what individual components are accessible.
summary(lm.fit)$r.sq   # R2
summary(lm.fit)$sigma  # RSE

# VIF
library(car)
vif(lm.fit)

# Interaction term x1:x2
summary(lm(medv ~ lstat * age, data = Boston))  # lstat*age == lstat + age + lstat:age