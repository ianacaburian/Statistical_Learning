library(neuralnet)
set.seed(123456)
concrete <- read.csv("concrete.csv")
str(concrete)
#   Values range from 0 to over 1000, => standardize.
#   If the data follows a:
#   * normal distribution, => use scale().
#   * uniform distribution or are severely nonnormal, => normalize.

# Standardize the features.
normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete, normalize))
#   Any transformation applied to the data prior to training the model will
#   have to be applied in reverse later on, in order to convert back to the
#   original units of measurement.
v <- names(concrete)
#   Strength is the 9th name.
summary(concrete_norm[v[9]])
#   Normalization is confirmed as Min = 0, Max = 1.

# Partition the data into 75% training, 25% testing sets.
concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]

# Train a simple multilayer feedforward network (single hidden node)
f <- as.formula(paste(v[9], " ~ ", v[1], " + ", v[2], " + ", v[3], " + ", v[4], " + ",
                                   v[5], " + ", v[6], " + ", v[7], " + ", v[8]))
concrete_model <- neuralnet(f, data = concrete_train)
#   In this simple model, there is one input node for each of the 8 features,
#   followed by a single hidden node and a single output node that predicts
#   the concrete strength.
plot(concrete_model)
#> Error: 5.670 Steps: 1059
#   The weights are labelled on arrows from each feature.
#   Bias terms are indicated by the nodes labelled with the number 1. 
#   These bias terms are numeric constants that allow the value at the 
#   indicated nodes to be shifted upward or downward, much like the 
#   intercept in a linear equation.
#   Error = Sum of Squared Errors (SSE) on the training data.
#   Steps = number of training steps.

# Generate predictions on the test set.
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
#   compute() returns a list with two components:
#   $neurons stores the neurons for each layer in the network,
#   $net.result stores the predicted values.

# Obtain correlation between two numeric vectors (evaluate test performance).
cor(predicted_strength, concrete_test[9])
#> 0.716
#   Being a numerical prediction problem and not classification, a confusion
#   matrix cannot be used. Instead, we must measure the correlation between
#   our predicted concrete strength and the true value. This provides insight
#   into the strength of the linear association between the two variables.
#   Correlations close to 1 indicate strong linear relationships between
#   two vars and thus good model performance.

# Improving model performance by increasing the number of hidden nodes.
concrete_model2 <- neuralnet(f, data = concrete_train, hidden = 5)
plot(concrete_model2)
#> Error: 1.653 Steps: 5322
#   Notice that reduced error required more steps than the simple model.

model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test[9])
#> 0.841

















