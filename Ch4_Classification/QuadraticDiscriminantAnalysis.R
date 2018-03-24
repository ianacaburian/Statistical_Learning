library(ISLR)
library(MASS)
attach(Smarket)
train = (Year < 2005)
     # Vector of all observations occuring before 2005
Smarket.2005 = Smarket[!train,]
     # Submatrix of observations in 2005.
Direction.2005 = Direction[!train]

qda.fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
    # Output contains the group means but not the coefficients because
    # the classifier involves a quadratic rather than a linear function.

# QDA predictions
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005) # = 60%
    # This improvement suggests that the quadratic form assumed
    # by QDA may capture the true relationship more accurately than
    # the linear forms assumed by LDA and logistic regression.