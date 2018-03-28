library(ISLR)
library(MASS)
library(class)
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

formula <- as.formula(Direction ~ Lag2)
train <- (Year <= 2008)               # Segregate the training data
Weekly.09_10 <- Weekly[!train,]       # Submatrix of observations in 2009-2010
numTestObs <- dim(Weekly.09_10)[1]    # Number of observations in 2009-2010
Direction.09_10 <- Direction[!train]  # Boolean vector to reference the test data

glm.fit <- glm(formula, data = Weekly, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Weekly.09_10, type = "response")
glm.pred <- rep("Down", numTestObs)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.09_10)
mean(glm.pred == Direction.09_10) # ==> Test Error Rate = 0.625


# e) ***************************************************************************

lda.fit <- lda(formula, data = Weekly, subset = train)
lda.class <- predict(lda.fit, Weekly.09_10)$class
table(lda.class, Direction.09_10)
mean(lda.class == Direction.09_10) # ==> Test Error Rate = 0.625


# f) ***************************************************************************

qda.fit <- qda(formula, data = Weekly, subset = train)
qda.class <- predict(qda.fit, Weekly.09_10)$class
table(qda.class, Direction.09_10)
mean(qda.class == Direction.09_10) # ==> Test Error Rate = 0.587


# g) ***************************************************************************

train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.09_10)
mean(knn.pred == Direction.09_10) # ==> Test Error Rate = 0.5


# h) ***************************************************************************

    # The best results are produced by the logistic regression and LDA methods.
    # Both resulted in test error rates of 62.5%, a better rate than that of 
    # QDA (58.7%) and KNN (50%).