library(ISLR)
names(Smarket)
    # Lag1 - 5: percentage returns for 5 previous trading days
    # Volume: num (in billions) of shares traded on the previous day
    # Today: percentage return on the date in questions
    # Direction: whether market was up or down on this date

summary(Smarket)
pairs(Smarket)


# Correlation matrix
cor(Smarket[,-9]) # excludes qualitative var 'Direction'
    # Only notable correlation is between 'Year' and 'Volume'

attach(Smarket)
plot(Volume)
    # Some confirmation that 'Volume' is increasing over time.


# Fit the logistic regression model
glm.fit = glm( Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial)
summary(glm.fit)
    # Negative Lag1 coefficient suggests if the market had a positive return yesterday, 
    # ==> it is less likely to go up today.
    # Lag1 has smallest p-value, although still large,
    # ==> no clear evidence of association with 'Direction'.
coef(glm.fit)
summary(glm.fit)$coef
    # access the coefficients

# Predict probabilities that the market will go up.
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:10]
contrasts(Direction)
    # Tells us that these values correspond to to the 
    # probability of the market going up (rather than down).

glm.pred = rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up"
    # To make a prediction for a particular day,
    # these predicted probabilities must be converted
    # into class labels, Up or Down.
    # The first command creates a vector of 1250 Down elements.
    # The second line transforms to 'Up' all of the elements for 
    # which the predicted probability of a market increase
    # exceeds 0.5.

table(glm.pred, Direction)

mean(glm.pred == Direction)
    # Fraction of days for which the prediction was correct
    # Same as (507 + 145) / 1250
    # > 0.5216 means the regression predicted correctly 52.2% of the time.
    # 1 - 52.2 = 47.8% is the training error rate, a little better than guessing.


# Fit the model using part of the data,
# then examine how well it predicts the remaining data.
# This will give a more realistic error rate.

train = (Year < 2005)
     # Vector of all observations occuring before 2005
Smarket.2005 = Smarket[!train,]
     # Submatrix of observations in 2005.
dim(Smarket.2005)
     # Confirms thata there are 252 observations in 2005.
Direction.2005 = Direction[!train]

# Fit the model using data not in 2005.
glm.fit = glm( Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial, subset = train)

# Obtain predicted probabilities from new test data using our fitted model.
glm.probs = predict(glm.fit, Smarket.2005, type = "response")

# Finally, compute predictions for 2005 and compare to actual
# movements of the market over the same time.
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005) # Training error rate = 48%
mean(glm.pred != Direction.2005) # Test error rate = 52%
    # Confirms how training error rate can be overly optimistic


# Attempt improvements by retaining only the predictors with
# the best p-values: Lag1 and Lag2.
glm.fit = glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
    # 56% - an improvement
106 / (106 + 76)
    # 58% suggests that on days when the model predicts an increase in the market,
    # a possible trading strategy of buying on these days may be more successful.


# Predict returns associated with particular values.
predict(glm.fit, newdata = data.frame( Lag1 = c(1.2, 1.5),
                                       Lag2 = c(1.1, -0.8)), type = "response")
    # Predicts 'Direction' on a day when Lag1 and Lag2 equal 1.2 and 1.1 respectively,
    # and on a day when they equal 1.5 and -0.8 respectively.