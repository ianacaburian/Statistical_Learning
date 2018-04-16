library(ISLR)
summary(Default)
attach(Default)
#_______________________________________________________________________________


# a)
set.seed(1)
glm.fit <- glm(default ~ income + balance, data = Default, family = binomial)
#_______________________________________________________________________________


# b)
computeValSetError <- function()
{
#   i.
n <- dim(Default)[1] #=> 10000
nTrain <- dim(Default)[1] / 2 #=> 5000
train <- sample(n, nTrain)

#   ii.
glm.fitTrain <- glm(default ~ income + balance, data = Default, subset = train, family = binomial)

#   iii.
testData <- Default[-train, ]
glm.probs <- predict(glm.fitTrain, newdata = testData, type = "response")
    # newdata: An optional data frame in which to look for variables with which
    #          to predict. If omitted, the fitted values are used.

glm.pred <- rep("No", nTrain)
glm.pred[glm.probs > 0.5] <- "Yes"

#   iv.
testPreds <- testData$default
errors <- glm.pred != testPreds
valSetError <- mean(errors)

return(valSetError)
}
computeValSetError() #=> 0.0286
#_______________________________________________________________________________


# c)
computeValSetError() #=> 0.0236
computeValSetError() #=> 0.028
computeValSetError() #=> 0.0268
# Comments:
(0.0286 + 0.0236 + 0.028 + 0.0268) / 4 #=> 2.675 error rate is the average