library(ISLR)
set.seed(1)
train <- sample(392, 196)
train
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
#_______________________________________________________________________________

attach(Auto)
estimatedResponse = predict(lm.fit, Auto)
errors = (mpg - estimatedResponse)[-train]
    # selected only the errors not in the training set
validationSetMSE = mean(errors^2)
validationSetMSE # => 26.14
#_______________________________________________________________________________
 
# Now the test error for the polynomial and cubic regressions is estimated.

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
    # => 19.82259

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
    # => 19.78252
#_______________________________________________________________________________

# Choose a different training set.

set.seed(2)
train = sample(392, 196)
lm.fit = lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
    # => 23.30

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
    # => 18.90

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)
    # => 19.26

# The results between the different training sets are consistent:
# The model that predicts mpg using a quadratic function of horsepower performs
# better than a model that involves only a linear function of horsepower,
# and there is little evidence in faavor of a model that uses a cubic function
# of horsepower.