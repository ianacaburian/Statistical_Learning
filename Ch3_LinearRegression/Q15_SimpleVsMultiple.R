library(MASS)
attach(Boston)

n <- names(Boston) 
predictors <- n[n != "crim"]
numP <- length(predictors)

simples <- vector(mode = "list", length = numP)
alpha = 0.05
pvals <- c()
signifpreds <- c()
for (i in 1:numP)
{
	formula <- as.formula(paste("crim ~", predictors[i]))
	simples[[i]] <- lm(formula)
	f <- summary(simples[[i]])$fstatistic
	p <- pf(f[1], f[2], f[3], lower.tail = F)
	pvals[i] <- p
	if (p < alpha) signifpreds <- c(signifpreds, predictors[i])
}

nonsignifpreds = predictors[!(predictors %in% signifpreds)]
    # All models apart from lm(crim ~ chas) produce P-values < 0.05,
    # suggesting that there is a statistically significant association
    # between predictor and the response in those models.


    


