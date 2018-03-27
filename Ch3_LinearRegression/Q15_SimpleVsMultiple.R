library(MASS)
attach(Boston)

n <- names(Boston) 
allPreds <- n[n != "crim"]
numPreds <- length(allPreds)
simples <- vector(mode = "list", length = numPreds)
pVals <- c()
alpha = 0.05
signifPreds <- c()

for (i in 1:numPreds)
{
	formula <- as.formula(paste("crim ~", allPreds[i]))
	simples[[i]] <- lm(formula)
	f <- summary(simples[[i]])$fstatistic
	p <- pf(f[1], f[2], f[3], lower.tail = F)
	pVals[i] <- p
	if (p < alpha) signifPreds <- c(signifPreds, allPreds[i])
}

signifPreds 
    # The simple linear regression models, that are fitted using each of 
    # these predictors, produce P-values < 0.05.
    # This suggests that there is a significant association between each
    # of these predictors and the response, per-capita crime rate.

nonSignifPreds <- allPreds[!allPreds %in% signifPreds]
nonSignifPreds # ==> "chas"
    # "chas", the predictor that indicates if the house is situated on the
    # banks of the Charles River, produces a model that cannot reject the null 
    # hypothesis: that there is no significant association with crime.

chasIndex = which(allPreds %in% "chas")
pVals[chasIndex] # ==> 0.209
    # As the P-value for "chas" falls well outside of our chosen alpha of 0.05,
    # our linear model cannot conclude that Charles River bank areas have an 
    # association with crime rate per-capita.


# Proportion of non-retail business acres per town.
indusIndex = which(allPreds %in% "indus")
coef(simples[[indusIndex]]) # ==> B1 = 0.5097763
pVals[indusIndex] # ==> 1.45e-21
    # There appears to be a positive association with the proportion of 
    # non-retail business acres per town and per-capita crime rate.
    # That is, industrial areas may be less safer to live in.

par(mfrow = c(2, 2))
plot(simples[[indusIndex]])
    # The nature of the relationship may not be simply linear.
    # The residuals vs fitted plot displays a non-random pattern around 0 and 
    # the Q-Q plot shows signs or nonlinearity.


# Average number of rooms per dwelling.
rmIndex = which(allPreds %in% "rm")
coef(simples[[rmIndex]]) # ==> B1 = -2.684051
pVals[rmIndex] # ==> 6.35e-07
    # There appears to be a steeper negative association with the average
    # number of rooms in a house and per-capita crime rate.
    # That is, housing that implies higher income may affect crime rate.

par(mfrow = c(2, 2))
plot(simples[[rmIndex]])
    # The residuals vs fitted plot has a slightly more random pattern than that
    # of indus. The Q-Q plot however shows similar nonlinearity.


# Weighted mean of distances to five Boston employment centres.
disIndex = which(allPreds %in% "dis")
coef(simples[[disIndex]]) # ==> B1 = -1.550902
pVals[disIndex] # ==> 8.52e-19
    # There appears to be a negative association with the weighted mean
    # distances to five Boston employment centres and per-capita crime rate.
    # That is, distributing the location of employment resources may affect
    # an area's crime rate.

par(mfrow = c(2, 2))
plot(simples[[disIndex]])
    # Similar to indus, the nature of the relationship suggests an improvement
    # may be possible with a more flexible model.