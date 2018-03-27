library(MASS)
attach(Boston)
n <- names(Boston) 
allPreds <- n[n != "crim"]
numPreds <- length(allPreds)


# a) ***************************************************************************

simples <- vector(mode = "list", length = numPreds)
pVals <- c()
alpha = 0.05
signifPreds <- c()

for (i in 1:numPreds)
{
	formula <- as.formula(paste("crim ~", allPreds[i]))
	simples[[i]] <- lm(formula)
	p <- coef(summary(simples[[1]]))[2,4]
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
coef(simples[[indusIndex]])[2] # ==> B1 = 0.5097763
pVals[indusIndex] # ==> 1.45e-21
    # There appears to be a positive association with the proportion of 
    # non-retail business acres per town and per-capita crime rate.
    # That is, industrial areas may be less safer to live in.

par(mfrow = c(2, 2))
plot(simples[[indusIndex]])
    # The nature of the relationship may not be simply linear.
    # The residuals vs fitted plot displays a non-random pattern around 0 and 
    # the Q-Q plot shows signs or nonlinearity.
    # The general appearance of these plots suggests a better model is needed.


# Average number of rooms per dwelling.
rmIndex = which(allPreds %in% "rm")
coef(simples[[rmIndex]])[2] # ==> B1 = -2.684051
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
coef(simples[[disIndex]])[2] # ==> B1 = -1.550902
pVals[disIndex] # ==> 8.52e-19
    # There appears to be a negative association with the weighted mean
    # distances to five Boston employment centres and per-capita crime rate.
    # That is, distributing the location of employment resources may affect
    # an area's crime rate.

par(mfrow = c(2, 2))
plot(simples[[disIndex]])
    # Similar to indus, the residuals suggests an improvement may be possible 
    # with a more flexible model, or one with multiple predictors.


# b) ***************************************************************************

lm.multi = lm(crim ~ ., data = Boston)
pValsMulti <- coef(summary(lm.multi))[-1,4]
signifPredsMulti <- pValsMulti[pValsMulti < alpha]
signifPredsMulti
    # The multiple regression model reveals that previous assumptions do not 
    # hold up. Predictors "rm" and "indus" are missing from our list of 
    # significant predictors. This suggests that if we account for more measured
    # information, they end up not contributing significantly in explaining
    # crime rate per-capita.
    # At the same time, these predictors, of which previously investigated "dis"
    # is included, produce P-values that allow a rejection of the corresponding
    # null hypotheses, H0: Bj = 0.


# c) ***************************************************************************

uniVCoefs <- c()
for (i in 1:numPreds)
{
	uniVCoefs[i] <- coef(simples[[i]])[2]
}
multiVCoefs <- coef(lm.multi)[-1]
plot(uniVCoefs, multiVCoefs)

    # The plot makes it plain to see the drastic change in results from a) to b).
    # Most apparent is the change in signs of the coefficients for around half 
    # of the predictors. For "nox", this appears extreme with a value of 30 in
    # its simple linear model and -10 in the multiple regression model.


# d) ***************************************************************************

allQuantPreds = allPreds[allPreds != "chas"]
numQuantPreds = length(allQuantPreds)
polys <- vector(mode = "list", length = numQuantPreds)
signifPredsPoly <- vector(mode = "list", length = numQuantPreds)

for (i in 1:numQuantPreds)
{
	formula <- as.formula(paste("crim ~", "poly(", allQuantPreds[i], ", 3)"))
	polys[[i]] <- lm(formula)
	polyPs <- coef(summary(polys[[i]]))[-1,4]
	signifPolyPs <- c()
	for (j in 1:length(polyPs))
	{
		if (polyPs[j] < alpha) signifPolyPs <- c(signifPolyPs, j)
	}
	signifPredsPoly[[i]] <- signifPolyPs
}

signifPredsPoly # ==> list 10 is the only list that contains only a 1.
allQuantPreds[10] # ==> "black"
    # The polynomial terms that produced significant P-values are listed for
    # each predictor. As suspected, there exist predictors for which the 
    # polynomial model is considered an improvement. In fact, only the 
    # polynomial model "crim" ~ "black" produces only its first polynomial term 
    # as significant. Whereas half of the predictors produce models where all 
    # three polynomial terms exhibit significance.