library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto) 
    # same as lm() when family arg omitted. 
    # glm is preferred in order to be used with cv.glm() below.

cv.err = cv.glm(Auto, glm.fit)
    # cv. glm() produces a list with several components.

cv.err$delta # => 24.23 24.23
    # the delta vector contains the cv results.
    # In this case, the numbers are identical, corresponding to the LOOCV
    # statistic given in Ch5.1
    # The cv estimate for the test error is approx. 24.23

#_______________________________________________________________________________

# Repeat this procedure for increasingly complex polynomial fits.

cv.error = rep(0, 5) # init vector

for (i in 1:5)
{
    glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
    cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}

cv.error # => 24.23151 19.24821 19.33498 19.42443 19.03321
    # As in figure 5.4, a sharp drop in estimated test MSE occurs between the
    # linear and quadratic fits, but then no clear improvement from using 
    # higher-order polynomials.
