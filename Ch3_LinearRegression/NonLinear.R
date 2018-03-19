library(MASS) # needed for Boston data
attach(Boston)

# x^2 in R must be wrapped using I(), ==> I(X^2)
lm.fit1 = lm(medv ~ lstat + I(lstat^2))


# Investigate the quadratic term

summary(lm.fit1)
    # low p-value suggests quadratic term improves model

lm.fit = lm(medv ~ lstat)
anova(lm.fit, lm.fit1)
    # F-stat = 135 and low p-value suggests quadratic model is far superior

par(mfrow = c(2, 2))
plot(lm.fit1)
    # Residuals display no discernible pattern


# Polynomials using poly()
lm.fit5 = lm(medv ~ poly(lstat, 5))
    # Shorter than including each order using I()

summary(lm.fit5)
    # Including each term up to the fifth order improves the model


# Log transform
summary(lm(medv ~ log(rm), data = Boston))
