library(ISLR)
# fix(Hitters)
names(Hitters)
dim(Hitters) #=> 322 20
sum(is.na(Hitters$Salary)) #=> 59

Hitters <- na.omit(Hitters) # remove all of the rows that have missing values
dim(Hitters) #=> 263 20
sum(is.na(Hitters)) #=> 0
#_______________________________________________________________________________

# Ridge Regression

x <- model.matrix(Salary ~ ., Hitters)[,-1]
    # model.matrix() produces a matrix corresponding to the 19 predictors as
    # well as automatrically transforming any qualitative variables into dummy
    # variables. This is needed for the glmnet() method.
y <- Hitters$Salary

library(glmnet)
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
    # glmnet() is implemented over a grid of values ranging from lambda = 10^10
    # to lambda = 10^-2, essentially covering the full range of scenarios from 
    # the null model containing only the intercept (i.e. lambda approaching 
    # infinity), to the least squares fit (i.e. lambda approaching 0).
    # Note that by default, glmnet() standardizes the vars to the same scale.

crm <- coef(ridge.mod)
    # Matrix of vectors of ridge regression coefficients with their associated
    # lambda values.
    # Predictors and the intercept are in rows and lambda values are in columns.
dim(crm) #=> 20 100

ridge.mod$lambda[50] #=> 11498
coef(ridge.mod)[,50]
    # With a large lambda, the coefficients are very small.
sqrt(sum(coef(ridge.mod)[-1, 50]^2)) #=> 6.36

ridge.mod$lambda[60] #=> 705
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2)) #=> 57.1
    # Smaller lambda of 705 results in a much large l2norm of 57.1

predict(ridge.mod, s = 50, type = "coefficients")[1:20,]
    # Ridge regression coefficients for a new lambda value of 50.


# Split the samples into a training and test set to estimate the test error
# of ridge regression and the lasso.
    # Rather than producing a random vector of TRUE, FALSE elements with 
    # observations selected corresponding to TRUE for the training data (as
    # previously done), another approach is adopted.
    # Here, we will randomly choose a subset of numbers between 1 and n;
    # these will then be used as the indices for the training observations.

set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thres = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test)^2) #=> 101037
    # Ridge regression model is fitted on the training set, then MSE is 
    # evaluated on the test set, using lambda = 4. 
    # The predict() method is used again with the newx arg replacing 
    # "type = "coefficients"" as used previously.

    # Note that if we had instead simply fit a model with just an intercept
    # we would have predicted each test observation using the mean of the
    # training observations. In that case, we could compute the test set MSE
    # like this:
mean((mean(y[train]) - y.test)^2) #=> 193253
    # We could also get the same result by fitting a ridge regression model
    # with a very large value of lambda:
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2) #=> 193253
    # It appears that fitting a ridge regression model with lambda = 4 leads
    # to a much lower test MSE than fitting a model with just an intercept.

ridge.pred <- predict(ridge.mod, s = 0, newx = [test,], exact = T)
mean((ridge.pred - y.test)^2) #=> 114783
lm(y ~ x, subset = train) # OLS
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:20,]
    # We now check whether there is any benefit to performing ridge regression
    # with lambda = 4 instead of just performing OLS, i.e. ridge regression
    # with lambda = 0. In order for glmnet() to yield the exact OLS coefficients
    # when lambda = 0, we use the arg exact = T when calling predict().
    # Otherwise, predict() will interpolate over the grid of lambda values used
    # in fitting the glmnet() model, yielding approx results.
    # However, there still remains a slight discrepancy when using exact = T,
    # therefore, if an OLS model is desired, just use lm() (instead of glmnet()
    # with lambda = 0).

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
    # Instead of arbitrarily choosing lambda = 4, we use CV to choose the 
    # tuning parameter via the built-in CV function cv.glmnet().
    # By default, the function performs ten-fold CV.

plot(cv.out)
bestlambda <- cv.out$lambda.min
bestlambda #=> 212
    # We see that the value of lambda that results in the smallest CV error is
    # 212.

ridge.pred <- predict(ridge.mod, s = bestlambda, newx = x[test,])
mean((ridge.pred - y.test)^2) #=> 96016
    # Test MSE associated with the best lambda = 212.
    # Compared to the result with lambda = 4 (i.e. 101037), this new value of
    # 96016 reveals a significant improvement when 212 is chosen as a tuning
    # parameter.

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlambda)[1:20,]
    # Note that there are no coefficients = 0, as ridge regression does not 
    # perform variable selection.
#_______________________________________________________________________________

# The Lasso





