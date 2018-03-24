library(ISLR)
dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822 # = 0.0598

# Standardizing the data using scale()
standardized.X = scale(Caravan[,-86])
    # col 86 is excluded as Purchase is qualitative.
var(Caravan[,1])
var(standardized.X[,1])
var(standardized.X[,2])
    # Now, every column of standardized.X has a 
    # sd of 1, and a mean of 0

# Split observations into a test and a training set.
test = 1:1000 
    # vector of indices used to split standardized.X
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]


# KNN Predictions
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred) # = 0.118
    # On the surface, 12% may be considered a good KNN error rate.
mean(test.Y != "No")     # = 0.59
    # Considering that only 5.9% of customers purchased insurance,
    # the error rate can be reduced to 5.9% by just always predicing 'No',
    # ==> in this context, 12% is unacceptable.

table(knn.pred, test.Y)
9 / (68 + 9) # = 0.117
    # The KNN prediction 11.7% does twice as good as random guessing.

knn.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5 / (21 + 5) # = 0.192
    # Increasing K from 1 to 3 improves our prediction from 11.7% to 19.2%

knn.pred = knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4 / (11 + 4) # = 0.267
    # Increasing K from 3 to 5 improves our prediction from 19.2% to 26.7%
    # Four times better than random guessing.


# Comparison of KNN and Logistic
glm.fit = glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)
glm.probs = predict(glm.fit, Caravan[test,], type = "response")
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.5] = "Yes"
table(glm.pred, test.Y)
    # If we use 0.5 as the predicted probability cut-off for the classifier, 
    # ==> then only 7 of the test obs are predicted to purchase, and
    # ==> all of these predictions are incorrect.

# Changing the predicted probability cut-off.
glm.pred = rep("No", 1000)
glm.pred[glm.probs > 0.25] = "Yes"
table(glm.pred, test.Y)
11 / (22 + 11) # = 0.333
    # If we instead predict a purchase any time the predicted 
    # probability of purchase exceeds 0.25, we get much better results.
    # We predict that 33 people will purchase insurance, and 
    # we are correct for about 33% of these,
    # ==> over 5 times better than random guess.

 
