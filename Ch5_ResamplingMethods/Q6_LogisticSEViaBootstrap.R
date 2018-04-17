library(ISLR)
library(boot)
summary(Default)
attach(Default)
#_______________________________________________________________________________


# a)
set.seed(1)
glm.fit <- glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)
glm.coefs <- coef(glm.fit)[-1]
glm.stderrs <- summary(glm.fit)$coefficients[-1,2]

#_______________________________________________________________________________


# b)
boot.fn <- function(data, subset)
{
    glm.fit <- glm(default ~ income + balance, 
                   data = data, subset = subset, family = binomial)
    coefs <- coef(glm.fit)[-1]
    return(coefs)
}

#_______________________________________________________________________________


# c)
boot(Default, boot.fn, R = 1000)
    # Bootstrap Statistics :
    #         original       bias     std. error
    # t1* 2.080898e-05 5.870933e-08 4.582525e-06
    # t2* 5.647103e-03 2.299970e-06 2.267955e-04
#_______________________________________________________________________________


# d)
glm.coefs #=> 2.080898e-05 5.647103e-03
glm.stderrs #=> 4.985167e-06 2.273731e-04
    # Bootstrap has done a great job in producing coefficient estimates compared
    # to the traditional way. For the above precision setting, both results 
    # are equal.