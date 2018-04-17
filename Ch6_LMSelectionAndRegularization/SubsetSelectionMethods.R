library(ISLR)
# fix(Hitters)
names(Hitters)
dim(Hitters) #=> 322 20
sum(is.na(Hitters$Salary)) #=> 59

Hitters <- na.omit(Hitters) # remove all of the rows that have missing values
dim(Hitters) #=> 263 20
sum(is.na(Hitters)) #=> 0
#_______________________________________________________________________________

# Best Subset Selection

library(leaps) # to use regsubsets()
regfit.full <- regsubsets(Salary ~ ., Hitters)
    # identifies the model with the best RSS.
summary(regfit.full)
    # summary() outputs the best set of variables for each model size.
    # An asterisk indicates that a given variable is included in the
    # corresponding model.

# Fit up to a 19-variable model using nvmax = 19
regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg.summary = summary(regfit.full)
names(reg.summary)
    # returns RSq, RSS, adjusted RSq, Cp, and BIC which can be examined to try
    # to select the best overall model.

reg.summary$rsq
    # RSq increases from 32% in the one var model to 55% when all vars included.
    # As expected, RSq increases monotonically as more vars are included.

# Plot RSS and Adjusted RSq for comparison
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables", 
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", 
     ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2) #=> 11
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

# Plot the Cp and BIC with minimum point indication.
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp) #=> 10
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)
which.min(reg.summary$bic) #=> 6
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

# Display the selected variables for the best model with a given number of
# predictors, ranked according to the BIC, Cp, adjusted RSq, or AIC,
# via regsubsets() built-in plot() command.
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
    # The top row in each plot corresponds to the optimal model.
    # Black squares indicate which variables are selected in the model.
    # There are several models with similar, optimal BIC value of -150.
    # However, the best model at the top consists of 6 vars.

coef(regfit.full, 6)
    # Coef estimates associated with the best model according to the lowest BIC.
#_______________________________________________________________________________

# Forward and Backward Stepwise Selection

regfit.fwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, 
                        method = "forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, 
                        method = "backward")
summary(regfit.bwd)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)
    # Comparing methods reveals different results.
    # The best 7 var models are different for each method, i.e. each model
    # has selected different variables, with different coefficient estimates.



