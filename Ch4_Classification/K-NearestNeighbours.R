library(ISLR)
library(MASS)
library(class)

attach(Smarket)
train = (Year < 2005)
     # Vector of all observations occuring before 2005
Smarket.2005 = Smarket[!train,]
     # Submatrix of observations in 2005.
Direction.2005 = Direction[!train]

# knn()
# Forms predictions using a single command, rather than a two-step approach.
# Requires 4 inputs:
    # 1. A matrix containing the predictors associated with the training data.
    # 2. A matrix containing the predictors associated with the data for which we wish to make predictions.
    # 3. A vector containing the class labels for the training observations.
    # 4. A value for K, the number of nearest neighbours to be used by the classifier.

train.X = cbind(Lag1, Lag2)[train,] # Input 1
test.X = cbind(Lag1, Lag2)[!train,] # Input 2
train.Direction = Direction[train]  # Input 3
    # cbind(), "column bind" used to bind the Lag1 and
    # Lag2 vars together into two matrices, one for the training
    # set and the other for the test set.

set.seed(1)
    # Ensures reproducibility of results because if several
    # observations are tied as nearest neighbours, then R will
    # randomly break the time.


# KNN Predictions
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
(83 + 43) 252 # = 0.5
    # Only 50% observations are correctly predicted,
    # ==> K = 1 is not very good.

knn.pred = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) # = 0.54
    # Increasing k from 1 to 3 shows some improvement.
    # Textbook says further increases do nothing...

knn.pred = knn(train.X, test.X, train.Direction, k = 4)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) # = 0.55

knn.pred = knn(train.X, test.X, train.Direction, k = 5)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) # = 0.48

knn.pred = knn(train.X, test.X, train.Direction, k = 6)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005) # = 0.48

# The highest being 0.55 tells us that QDA provides the best
# results of the methods examined so far.

