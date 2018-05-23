library(neuralnet)
fleeceweight <- read.csv("FleeceWeight.csv")
str(fleeceweight)

# Standardize features.
normalize <- function(x) {
    if (is.factor(x)) {
        return(x)
    }
    else {
        return((x - min(x)) / (max(x) - min(x)))
    }    
}
fleeceweight_norm <- as.data.frame(lapply(fleeceweight, normalize))
str(fleeceweight_norm) # Confirm standardization.

# Partition the data into 75% training, 25% testing sets.
fleeceweight_train <- fleeceweight_norm[1:3000,]
fleeceweight_test <- fleeceweight_norm[3001:4000,]

# Construct formula.
vars <- names(fleeceweight)
y <- vars[16]
f <- as.formula(paste(y, " ~ ", paste(vars[!vars %in% y], collapse = " + ")))

# Train a neural net with 1 to 8 hidden units to predict adult wool production.
set.seed(330)
models <- list()
model_cors <- rep(0, 8)
for (i in 1:8) {
    models[[i]] <- neuralnet(f, data = fleeceweight_train, hidden = i, stepmax = 1e6)
    mr <- compute(models[[i]], fleeceweight_test[1:15])
    ps <- mr$net.result
    model_cors[i] <- cor(ps, fleeceweight_test[16])    
}
model_cors
#> [1] 0.8691455268 0.8975236002 0.9078753080 0.9093134044 0.9188200338
#> [6] 0.9202633249 0.9230829551 0.9211179594

# Plot of the single hidden unit neural net.
plot(models[[1]]) # Figure 10

# Plot of the correlation coefficient obtained by each neural net.
plot(model_cors, type = "l",
     ylab = "Correlation Coefficient", xlab = "Hidden Units") #> Figure 11
