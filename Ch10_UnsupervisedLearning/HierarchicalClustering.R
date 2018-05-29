set.seed(2)
x = matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4

# Comparing clustering using different linkage methods.
# dist() is used to compute the 50x50 inter-observation Euclidean distance matrix.
hc.complete = hclust(dist(x), method = 'complete')
hc.average = hclust(dist(x), method = 'average')
hc.single = hclust(dist(x), method = 'single')

# Plot the dendrograms
par(mfrow = c(1, 3))
plot(hc.complete, main = " Complete Linkage ", xlab = "", sub = "", cex = .9)
plot(hc.average, main = " Average Linkage ", xlab = "", sub = "", cex = .9)
plot(hc.single, main = " Single Linkage ", xlab = "", sub = "", cex = .9)

# Determine the cluster labels for each obs associated with a 
# given cut of the dendrogram.
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
#   Single linkage assigns a cluster to a lone observation.

cutree(hc.single, 4)
#   A more sensible answer is obtained with four clusters although there are still two singletons.

# Scale the variables before performing hierarchical clustering of the obs.
xsc = scale(x)
plot(hclust(dist(xsc), method = 'complete'),
     main = " Hierarchical Clustering with Scaled Features ")

# Correlation-based distance using the as.dist() function converts an arbitrary
# square symmetric matrix into a form that the hclust() function recognizes as
# a distance matrix. This only makes sense for data with at least three features
# since the absolute correlation between any two observations with measurements
# on two features is always 1. Hence, we will cluster a three-dimensional data set.
x = matrix(rnorm(30 * 3), ncol = 3)
dd = as.dist(1 - cor(t(x)))
plot(hclust(dd, method = 'complete'),
     main = " Complete Linkage with Correlation - Based Distance ",
     xlab = "", sub = "")

