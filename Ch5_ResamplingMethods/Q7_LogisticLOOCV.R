library(ISLR)
library(boot)
summary(Weekly)
set.seed(1)
attach(Weekly)
#_______________________________________________________________________________


# a)
glm.fit1 <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(glm.fit1)

# b)
nMinusn1 <- Weekly[-1, ]
glm.fit2 <- glm(Direction ~ Lag1 + Lag2, data = nMinusn1, family = binomial)
summary(glm.fit2)

# c)
glm.probs <- predict(glm.fit2, newdata = Weekly[1, ], type = "response")
glm.probs > 0.5 #> TRUE (Up)
Weekly[1,]$Direction #> "Down"
    # Predicted incorrectly

# d)
n <- dim(Weekly)[1]
errors <- rep(0, n)
for(i in 1:n)
{
    glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = binomial)
    glm.probs <- predict(glm.fit, newdata = Weekly[i, ], type = "response")
    errors[i] <- as.integer((glm.probs > 0.5) != (Weekly[i, ]$Direction == "Up"))
}

# e)
numErrors <- sum(errors)
numErrors #> 490
LOOCVTestErrorEstimate <- mean(errors)
LOOCVTestErrorEstimate #> 44.99541% error rate
