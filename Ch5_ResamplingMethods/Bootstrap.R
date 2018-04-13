library(ISLR)
# Estimating the Accuracy of a Statistic of Interest

# alpha.fn() takes as input the (X, Y) data as well as a vector indicating which
# observations should be used to estimate alpha. 
# The function then outputs the estimate for alpha based on the selected
# observations (based on applying 5.7 to the observations indexed by 'index'.

alpha.fn = function(data, index)
{
    X = data$X[index]
    Y = data$Y[index]
    return ((var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2*cov(X, Y)))
}

alpha.fn(Portfolio, 1:100) # => 0.576

s <- sample(100, 100, replace = T)
    # Randomly select 100 observations from the range 1 to 100 with replacement.
    # This is equivalent to constructing a new bootstrap data set and 
    # recomputing alpha-hat based on the new data set.

set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T)) # => 0.596
    # This command can be performed many times to implement a bootstrap
    # analysis, recording all of the corresponding estimates for alpha, and 
    # computing the resulting standard deviation.
    # The following boot() function automates this.

boot(Portfolio, alpha.fn, R = 1000)
    # Bootstrap Statistics :
    #      original        bias    std. error
    # t1* 0.5758321 -7.315422e-05  0.08861826

    # This output shows that using the original data, alpha-hat = 0.5758,
    # and that the bootstrap estimate for SE(alpha-hat) is 0.0886.
#_______________________________________________________________________________

# Estimating the Accuracy of a Linear Regression Model

# Bootstrap can be used to assess the variability of the coef estimates and
# predictions from a statistical learning method. Here, we bootstrap to 
# assess the variablity of the estimates B0 and B1 for the linear model
# mpg ~ horsepower.

boot.fn = function(data, index)
    return(coef(lm(mpg 	~ horsepower, data = data, subset = index)))

boot.fn(Auto, 1:392)
    # (Intercept)  horsepower 
    #  39.9358610  -0.1578447 

# Create bootstrap estimates for the intercept and slope terms by randomly 
# sampling from among the observations with replacement.
# Example 1:
set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
    # (Intercept)  horsepower 
    #  38.7387134  -0.1481952 

# Example 2:
boot.fn(Auto, sample(392, 392, replace = T))
    # (Intercept)  horsepower 
    #  40.0383086  -0.1596104 

# Compute the standard errors of 1000 bootstrap estimates for the intercept and
# slope terms.
boot(Auto, boot.fn, 1000)
    # Bootstrap Statistics :
    #       original      bias    std. error
    # t1* 39.9358610  0.02972191 0.860007896
    # t2* -0.1578447 -0.00030823 0.007404467

    # Bootstrap estimate for SE(B-hat0) is 0.86, and for SE(B-hat1) is 0.0074.

summary(lm(mpg ~ horsepower, data = Auto))$coef
    # Standard formulas can be used to compute the standard errors for the 
    # regression coefficients in a linear model.
    # => B-hat0 = 0.717, B-hat1 = 0.0064.

    # Interestingly, these are somewhat different from the estimates obtained
    # using the bootstrap. Does this indicate a problem with the bootstrap?
    # In fact, it suggests the opposite. 
    # Recall that the standard formulas (eq 3.8) rely on certain assumptions. 
    # E.g. they depend on the unknown param sig^2, the noise variance. 
    # The estimate for the noise variance depends on the linear model being 
    # correct (which in this case, it is not; see figure 3.8). 
    # Further, the standard formulas assume that the xi are fixed, and all 
    # the variability comes from the variation in the errors ei.
    # The bootstrap approach does not rely on any of these assumptions, and
    # so it is likely giving a more accurate estimate of the standard errors
    # of B-hat0 and B-hat1 than is the summary() function.

# Compute the previous estimates using a quadratic model.
boot.fn = function(data, index)
    coefficients(lm(mpg ~ horsepower + I(horsepower^2), 
                    data = data, subset = index))
set.seed(1)
boot(Auto, boot.fn, 1000)
    # Bootstrap Statistics :
    #         original        bias     std. error
    # t1* 56.900099702  6.098115e-03 2.0944855842
    # t2* -0.466189630 -1.777108e-04 0.0334123802
    # t3*  0.001230536  1.324315e-06 0.0001208339

    # There is now a better correspondence between the bootstrap estimates 
    # and the standard estimates of SE(B-hat0), SE(B-hat1) and SE(B-hat2), 
    # since the quadratic model provides a good fit to the data.
