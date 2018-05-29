library(ISLR)

# The NCI60 cancer cell line microarray data, which consists of 
# 6830 gene expression measurements on 64 cancer cell lines.
# Each cell line is labeled with a cancer type.
# We do not make use of the cancer types in performing PCA and
# clustering, we will check to see the extent to which these 
# cancer types agree with the results of these unsupervised techniques.

# Clustering the cell lines with the goal of finding out whether
# or not the obs cluster into distinct type of cancer.

# Begin with standardizing the vars.
sd.data = scale(nci.data)
#   Recall that this is optional and should be performed only if we want
#   each gene to be on the same scale.

# Perform hierarchical clustering using euclidean distance as the
# dissimilarity measure.
par(mfrow = c(1, 3))
data.dist = dist(sd.data)
plot(hclust(data.dist), labels = nci.labs,
     main = " Complete Linkage ", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = 'average'), labels = nci.labs,
     main = " Average Linkage ", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = 'single'), labels = nci.labs,
     main = " Single Linkage ", xlab = "", sub = "", ylab = "")
#   We see that the choice of linkage certainly affects the results.
#   Typically, single linkage tends to yield "trailing" clusters:
#   very large clusters onto which individual observations attach one-by-one.
#   On the other hand, complete and average linkage tend to yield more balanced,
#   attractive clusters.
#   For this reason, complete and average linkage are generally preferred to single linkage.

#   Clearly cell lines within a single cancer type do tend to cluster together,
#   although the clustering is not perfect. 

# Cut the dendrograms at the height that will yield a particular number of clusters, say four:
hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out, 4)
table(hc.clusters, nci.labs)
#   There are some clear patterns. All the leukemia cell lines fall in cluser 3,
#   while the breast cancer cell lines are spread out over three different clusters.

# Plot the cut on the dendrogram.
par(mfrow = c(1, 1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = " red ")
#   h = 139 plots a horizontal line at height 139 on the dendrogram.
#   It is easy to verify that the resulting clusters are the same as
#   the ones we obtained using cutree(hc.out, 4).

# Summary of hclust
hc.out

# K-means and hierarchical with a dendrogram cut obtaining the same number
# of clusters can yield very different results.

# How do these hierarchical clustering results compare to what we get
# if we perform K-means clustering with k = 4?
set.seed(2)
km.out = kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)
#   We see that the four clusters obtained using hierarchical clustering
#   and K-means clustering are somewhat different.
#   Cluster 2 in K-means clustering is identical to cluster 3 in hierarchical.
#   However, the other clusters differ:
#   for instance, cluser 4 in K-means contains a portion of the obs
#   assigned to cluser 2 by hierarchical.

# Rather than performing hierarchical on the entire data matrix,
# we can simply perform hierarchical on the first few pc score vectors:
hc.out = hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs,
     main = " Hier. Clust . on First Five Score Vectors ")
table(cutree(hc.out, 4), nci.labs)
#   Not surprisingly, these results are different from the ones that we 
#   obtained when we performed hierarchical clustering on the full data set.

#   Sometimes performing clustering on the first few pc score vectors
#   can give better results than performing clustering on the full data.
#   In this situation, we might view the principal component step as one 
#   of denoising the data.
#   We could also perform K - means clustering on the first few
#   principal component score vectors rather than the full data set.