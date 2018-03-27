library(ISLR)
attach(Weekly)
 

# a) ***************************************************************************

summary(Weekly)
pairs(Weekly)
    # The plot with the pattern that stands out is that between Year and Volume.

cor(Weekly[-9])
    # The visual observation is supported by the correlation between Year and
    # Volume as being the only notable value in the correlation matrix: 0.84.

plot(Volume)
    # Further visual confirmation that Volume is increasing over time.


# d) ***************************************************************************

train <- (Year <= 2008)               # Segregate the training data
Weekly.09_10 <- Weekly[!train,]       # Submatrix of observations in 2009-2010
numTestObs <- dim(Weekly.09_10)[1]    # Number of observations in 2009-2010
Direction.09_10 = Direction[!train]   # Boolean vector to reference the test data

glm.lr = glm(Direction ~ Lag2, 
                  data = Weekly, family = binomial, subset = train)
glm.probs = predict(glm.lr, Weekly.09_10, type = "response")
glm.pred = rep("Down", numTestObs)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.09_10)
mean(glm.pred == Direction.09_10) # ==> Training Error Rate = 0.625


# e) ***************************************************************************

