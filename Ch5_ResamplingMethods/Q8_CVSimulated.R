library(boot)
#_______________________________________________________________________________


# a)
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)

    # n = 100, p = 2
    # Y = X - 2X^2 + Epsilon

#_______________________________________________________________________________


# b)
plot(x, y)
    # Data is scattered in a quadratic shape.
    # The range of X is from -2 to 2, that of Y is from -8 to 2
#_______________________________________________________________________________


# c)
set.seed(1)
Data <- data.frame(x, y)

#   i.
glm.fit <- glm(y ~ x, data = Data)
cv.err <- cv.glm(Data, glm.fit)
cv.err$delta #> 5.890979 5.888812

# Process the polynomial models using a for loop.
npoly <- 4
cv.error <- rep(0, npoly)

for (i in 1:npoly)
{
    glm.fit <- glm(y ~ poly(x, i), data = Data)
    cv.error[i] <- cv.glm(Data, glm.fit)$delta[1]
}
cv.error #> 5.890979 1.086596 1.102585 1.114772
#_______________________________________________________________________________


# d)
set.seed(2)
npoly <- 4
cv.error2 <- rep(0, npoly)

for (i in 1:npoly)
{
    glm.fit <- glm(y ~ poly(x, i), data = Data)
    cv.error2[i] <- cv.glm(Data, glm.fit)$delta[1]
}
cv.error2 #> 5.890979 1.086596 1.102585 1.114772
    # Same as cv.error as LOOCV will validate the same folds every time, i.e. it
    # evaluates n folds of a single observation.
#_______________________________________________________________________________


# e) 
    # The model using the first and second order polynomials had the lowest 
    # error, which was to be expected because the true form of Y was quadratic.
#_______________________________________________________________________________


# f)
npoly <- 4
olsErrors <- rep(0, npoly)

for (i in 1:npoly)
{
    glm.fit <- glm(y ~ poly(x, i), data = Data)
    cv.error2[i] <- cv.glm(Data, glm.fit)$delta[1]
}
cv.error2 #> 5.890979 1.086596 1.102585 1.114772
#_______________________________________________________________________________


# g)
summary(glm.fit)
alpha <- rep(0.05, npoly)
summary(glm.fit)$coefficients[-1, 4] < alpha
    # poly(x, i)1 poly(x, i)2 poly(x, i)3 poly(x, i)4 
    #        TRUE        TRUE       FALSE       FALSE 
    # P-values show statistical significance of linear and quadratic terms, 
    # which agrees with the CV results.
