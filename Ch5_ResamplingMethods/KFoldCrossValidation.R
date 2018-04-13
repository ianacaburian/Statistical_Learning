set.seed(17)
cv.error.10 = rep(0, 10)
for (i in 1:10)
{
    glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
    cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.error.10 # => 9.18924 19.30662 19.33799 18.87911 19.02103 18.89609
            #    19.71201 18.95140 19.50196

    # Still shows little evidence that using a cubic or higher-order polynomial
    # terms leads to lower test error than simply using a quadratic fit.

    # Previously in LOOCV, the two numbers in $delta were essentially the same.
    # Here, they are slightly different as the first number is the standard
    # k-fold CV estimate, as in Ch5.3.
    # The second is a bias-corrected version.

    # Note also that the computation time is much shorter than LOOCV. 
    # In the linear case (this case), LOOCV should be faster but the cv library
    # doesn't make use of the mathematical shortcut in the formula.
#_______________________________________________________________________________

