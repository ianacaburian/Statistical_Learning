states = row.names(USArrests)
states

names(USArrests)

apply(USArrests, 2, mean)
#   The variables have vastly different means
apply(USArrests, 2, var)
#   The variances are also vastly different, 
#   which is a sign that they need to be scaled to 
#   avoid Assault from dominating the PCA.

# Perform PCA
pr.out = prcomp(USArrests, scale = TRUE)
#   By default this function centers the variables to have mean zero.
#   scale = T standardizes vars to have sd = 1.

names(pr.out)
pr.out$center
pr.out$scale
#   These corresponds to the means and sd used for scaling prior to PCA.

# Principal component loading vector
pr.out$rotation
#   There show four distinct principal components.
#   There is expected to generally be min(n-1, p) informative pc's.

# Principal component score vector matrix.
dim(pr.out$x)
#   There is no need to manually multiply the data by the loading vectors 
#   to obtain the pc score vectors. 
#   pr.out$x is a matrix that has as its colums the pc score vectors.

biplot(pr.out, scale = 0)
#   scale = 0 ensures that the arrows are scaled to represent the loadings;
#   other values for scale give slightly different biplots with different interpretations.

pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)
#   Transformed the previous biplot to imitate Figure 10.1

# SD of each principal component.
pr.out$sdev

# The variance explained by each principal component.
pr.var = pr.out$sdev^2
pr.var

# The proportion of variance explained by each principal component.
pve = pr.var / sum(pr.var)
pve
#   0.6201 => 62.01% of the variance is explained by the first pc.

# Plot the PVE (proportion of variance explained).
plot(pve, xlab = " Principal Component ",
     ylab = " Proportion of Variance Explained ",
     ylim = c(0, 1), type = 'b')
plot(cumsum(pve), xlab = " Principal Component ",
     ylab = "Cumulative Proportion of Variance Explained ",
     ylim = c(0, 1), type = 'b')
#   cumsum() computes the cumulative sum of the elements of a numerical vector.

