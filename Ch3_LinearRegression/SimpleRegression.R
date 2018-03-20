names(Boston)
# ?Boston 

# lm() to fit a simple linear regression model
attach(Boston)
lm.fit = lm(medv~lstat)
summary(lm.fit)

# Coefficients
coef(lm.fit) # safer than "lm.fit$coefficients"


# Confidence intervals
confint(lm.fit)

# Conf and pred intervals using predict()
# The 95% conf int associated with a lstat val of 10
# is (24.47, 25.63), 
# and the 95% pred int is (12.828, 37.28).

predict( lm.fit, 
         data.frame(lstat = c(5, 10, 15)),
         interval = "confidence")
predict( lm.fit,
         data.frame(lstat = c(5, 10, 15)),
         interval = "prediction")


# Plot with the least squares regression line.
plot(lstat, medv)

# abline(a, b): a = intercept, b = slope
abline(lm.fit)
abline(lm.fit, lwd = 3)               # line width
abline(lm.fit, lwd = 3, col = "red")  # line colour

plot(lstat, medv, col = "red")        # plot char colour
plot(lstat, medv, pch = 20)           # plot char selection
plot(lstat, medv, pch = "+")           
plot(1:20, 1:20, pch = 1:20)          # plot chars 1:20 plotted


# Diagnostic plots
par(mfrow = c(2, 2))
plot(lm.fit)          

# Further residuals plots
plot(predict(lm.fit), residuals(lm.fit))  # residuals from a lin fit
plot(predict(lm.fit), rstudent(lm.fit))   # studentized residuals

plot(hatvalues(lm.fit))                   # leverage stats
which.max(hatvalues(lm.fit))              # largest leverage stat