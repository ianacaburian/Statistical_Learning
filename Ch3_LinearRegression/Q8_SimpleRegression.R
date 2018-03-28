auto <- read.table("Auto.data", header = T, na.strings = "?")


# a) ***************************************************************************

attach(auto)
lm.fit <- lm(mpg ~ horsepower)
summary(lm.fit)

# i.
# The large F-stat and its low p-value,
# ==> reject H0, conclude signif relationship with at least 1 predictor

# ii.
# The R2 = 0.6 ==> medium strength

# iii.
# B1 = -0.15 ==> negative

# iv.
predict(lm.fit, data.frame(horsepower = 98), interval = "confidence")
predict(lm.fit, data.frame(horsepower = 98), interval = "prediction")


# b) ***************************************************************************

plot(horsepower, mpg)
abline(lm.fit)


# c) ***************************************************************************

par(mfrow = c(2, 2))
plot(lm.fit) 
    # Residuals display discernible U-pattern ==> linear model is a poor fit