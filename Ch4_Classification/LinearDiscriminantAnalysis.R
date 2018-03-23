library(ISLR)
library(MASS)
attach(Smarket)
train = (Year < 2005)
     # Vector of all observations occuring before 2005
Smarket.2005 = Smarket[!train,]
     # Submatrix of observations in 2005.
Direction.2005 = Direction[!train]

# Fit the LDA
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
    # Prior probabilities (pi^k) indicates 49.2% of the training
    # observations correspond to days during which the market went down.
    # It also provides the group means;
    # these are the avg of each predictor within each class,
    # and are used by LDA as estimates of mu k.
    # These suggest that there is a tendency for the previous 2 days'
    # returns to be negative on days when the market increase, 
    # and a tendency for the previous days' returns to be
    # positive on days when the market declines.

# The Coefficients of Linear Discriminants provides the linear combination
# of the predictors that are used to form the LDA decision rule.
# These are the multipliers of the elements of X = x in 4.19.
coef(lda.fit)
    # If -0.642*Lag1 - 0.514*Lag2 is large,
    # ==> the LDA classifier will predict a market increase, and
    # if it is small,
    # ==> the LDA classifier will predict a market decline.

# Plots of the Linear Discriminants
plot(lda.fit)
    # The discriminants are obtained by computing
    # -0.642*Lag1 -0.514*Lag2 for each of the training observations.


# LDA predictions
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
    # The predict function returns a list with three elements.
    # 1. 'class', contains LDA's predictions about the movement of the market.
    # 2. 'posterior', is a matrix whose kth column contains the posterior probability
                    # that the corresponding observation belongs to the kth class, computed from 4.10.
    # 3. x contains the linear discriminants.
 
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005) 
    # 56% ==> the LDA and logistic regression predictions are almost identical

# Applying a 50% threshold to the posterior probabilities allows
# us to recreate the predictions contained in lda.pred$class
sum(lda.pred$posterior[,1] >= 0.5) # = 70.
sum(lda.pred$posterior[,1] < 0.5)  # = 182.

lda.pred$posterior[1:20,1]
lda.class[1:20]
    # Notice that the posterior probability output by the model
    # corresponds to the probability that the market will decrease.

# Using a posterior probability threshold other than 50%
sum(lda.pred$posterior[,1] > 0.9)
    # Predicts a market decrease when we are very certain (90%)
    # that the market will indeed decrease on that day.
    # A result of 0 means that there were no days in 2005 that
    # meet this threshold of 90%.

# The greatest posterior probability of decrease in all of 2005 was 52.02%

