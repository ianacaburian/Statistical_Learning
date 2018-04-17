library(leaps)
#_______________________________________________________________________________


# a)

set.seed(1)
X <- rnorm(100)
eps <- rnorm(100)
#_______________________________________________________________________________


# b)

beta0 = 3
beta1 = 2
beta2 = -3
beta3 = 0.3
Y = beta0 + beta1 * X + beta2 * X^2 + beta3 * X^3 + eps
#_______________________________________________________________________________


# c)

data.full = data.frame(y = Y, x = X)
regfit.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
reg.summary = summary(regfit.full)
names(reg.summary)

# Find the model size for best cp, BIC and adjr2
minCp <- which.min(reg.summary$cp) #> 3
minBIC <- which.min(reg.summary$bic) #> 3
maxAdjR2 <- which.max(reg.summary$adjr2) #> 3
    # According to the Cp, BIC, and adjusted RSq, the 3rd model is the best.

# Plots to provide evidence
par(mfrow = c(2, 2))
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l") # type l means line chart
points(minCp, reg.summary$cp[minCp], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(minBIC, reg.summary$bic[minBIC], col = "red", cex = 2, pch = 20)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "adjr2", type = "l")
points(maxAdjR2, reg.summary$adjr2[maxAdjR2], col = "red", cex = 2, pch = 20)

# Report the coefficients of the best model obtained.
coefficients(regfit.full, id = 3)
    # (Intercept) poly(x, 10, raw = T)1 poly(x, 10, raw = T)2 poly(x, 10, raw = T)7 
    #  3.07627412            2.35623596           -3.16514887            0.01046843 
#_______________________________________________________________________________


# d)

regfit.fwd <- regsubsets(y ~ poly(x, 10, raw = T), 
                         data = data.full, nvmax = 10, method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(y ~ poly(x, 10, raw = T), 
                         data = data.full, nvmax = 10, method = "backward")
summary(regfit.bwd)

fwd.summary <- summary(regfit.fwd)
bwd.summary <- summary(regfit.bwd)

which.min(fwd.summary$cp) #> 3
which.min(bwd.summary$cp) #> 3
which.min(fwd.summary$bic) #> 3
which.min(bwd.summary$bic) #> 3
which.max(fwd.summary$adjr2) #> 3
which.max(bwd.summary$adjr2) #> 3

par(mfrow = c(3, 2))
plot(fwd.summary$cp, xlab = "Subset Size", ylab = "Forward Cp", pch = 20, type = "l")
points(3, fwd.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$cp, xlab = "Subset Size", ylab = "Backward Cp", pch = 20, type = "l")
points(3, bwd.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(fwd.summary$bic, xlab = "Subset Size", ylab = "Forward BIC", pch = 20, 
    type = "l")
points(3, fwd.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$bic, xlab = "Subset Size", ylab = "Backward BIC", pch = 20, 
    type = "l")
points(3, bwd.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(fwd.summary$adjr2, xlab = "Subset Size", ylab = "Forward Adjusted R2", 
    pch = 20, type = "l")
points(3, fwd.summary$adjr2[3], pch = 4, col = "red", lwd = 7)
plot(bwd.summary$adjr2, xlab = "Subset Size", ylab = "Backward Adjusted R2", 
    pch = 20, type = "l")
points(3, bwd.summary$adjr2[3], pch = 4, col = "red", lwd = 7)

coefficients(mod.fwd, id = 3)
coefficients(mod.bwd, id = 3)
coefficients(mod.fwd, id = 3)
#_______________________________________________________________________________


# e)

library(glmnet)

# Use CV to select the optimal lambda.
xmat = model.matrix(Y ~ poly(X, 10, raw = T), data = data.full)[, -1]
lasso.mod = cv.glmnet(xmat, Y, alpha = 1)
best.lambda = lasso.mod$lambda.min
best.lambda #> 0.03991

# Create plots of the CV error as a function of lambda
plot(lasso.mod)

# Report the resulting coefficient estimates, and discuss the results obtained
lasso.coef <- predict(lasso.mod, type = "coefficients", s = best.lambda)
lasso.coef
    # Lasso picks the first, second, fifth, and seventh order polynomial.
    # Compared to previous subset selection methods, it has discarded the 
    # third order polynomial in favour of the fifth, and negligibly the seventh.
#_______________________________________________________________________________


# f)

beta7 <- 7
Y <- beta0 + beta7*X^7 + eps


# Best subset selection

data.full = data.frame(y = Y, x = X)
regfit.full = regsubsets(y ~ poly(x, 10, raw = T), data = data.full, nvmax = 10)
reg.summary = summary(regfit.full)
names(reg.summary)

# Find the model size for best cp, BIC and adjr2
minCp <- which.min(reg.summary$cp) #> 2
minBIC <- which.min(reg.summary$bic) #> 1
maxAdjR2 <- which.max(reg.summary$adjr2) #> 4

# Plots to provide evidence
par(mfrow = c(2, 2))
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(minCp, reg.summary$cp[minCp], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(minBIC, reg.summary$bic[minBIC], col = "red", cex = 2, pch = 20)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "adjr2", type = "l")
points(maxAdjR2, reg.summary$adjr2[maxAdjR2], col = "red", cex = 2, pch = 20)

# Report the coefficients of the best model obtained.
coefficients(regfit.full, id = minCp)
    # (Intercept) poly(x, 10, raw = T)2 poly(x, 10, raw = T)7 
    #   3.0704904            -0.1417084             7.0015552 
coefficients(regfit.full, id = minBIC)
    # (Intercept) poly(x, 10, raw = T)7 
    #     2.95894               7.00077 
coefficients(regfit.full, id = maxAdjR2)
    # (Intercept) poly(x, 10, raw = T)1 poly(x, 10, raw = T)2 poly(x, 10, raw = T)3 poly(x, 10, raw = T)7 
    #   3.0762524             0.2914016            -0.1617671            -0.2526527             7.0091338 
    # The BIC's model of 1 var has very closely chosen the true coefficients.


# Lasso model 

xmat = model.matrix(Y ~ poly(X, 10, raw = T), data = data.full)[, -1]
lasso.mod = cv.glmnet(xmat, Y, alpha = 1)
best.lambda = lasso.mod$lambda.min
best.lambda #> 13.57478

lasso.coef <- predict(lasso.mod, type = "coefficients", s = best.lambda)
lasso.coef
    # Like the BIC, lasso has chosen the 1 var model but the coefficients are
    # significantly less precise to the true coefficients
