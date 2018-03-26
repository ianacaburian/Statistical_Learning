library(ISLR)

# a)
set.seed(1) # for consistent results
x = rnorm(100)

# b)
eps = rnorm(100, 0, sqrt(0.25))  # arg3 is sd where var is 0.25

# c)
y = -1 + 0.5 * x + eps
length(y) # = 100
B0 = -1
B1 = 0.5

# d)
plot(x, y)
    # appears to follow a linear relationship

# e)
lm.fit = lm(y ~ x)
summary(lm.fit)
> B^0 = -1.03, B^1 = 0.52
   # Close to true values

# f)
abline(lm.fit, col = 2)
abline(B0, B1, col = 3) 
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

# g)
lm.fit2 = lm(y ~ x + I(x^2))
summary(lm.fit2)
    # R2 has improved from 0.44 to 0.45,
    # ==> fitting the training data has improved.

    # High p-value for the quadratic term,
    # ==> suggests there is no relationship between y and x^2
    # ==> fitting a quadratic term may not improve the model for test data