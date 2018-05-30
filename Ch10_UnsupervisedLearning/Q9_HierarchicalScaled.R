library(ISLR)
set.seed(1)
dsc <- scale(USArrests)
d1 <- dist(dsc) ^ 2
d2 <- as.dist(1 - cor(t(dsc)))
summary(d2 / d1)

# Using hierarchical clustering with complete linkage and Euclidean distance, 
# cluster the states.
set.seed(2)
hc.complete <- hclust(dist(USArrests), method = "complete")
plot(hc.complete)

# Cut the dendrogram at a height that results in three distinct clusters. 
# Which states belong to which clusters ?
cutree(hc.complete, 3)

# Hierachically cluster the states using complete linkage and Euclidean distance, 
# after scaling the variables to have standard deviation one.
sd.data <- scale(USArrests)
hc.complete.sd <- hclust(dist(sd.data), method = "complete")
plot(hc.complete.sd)

# What effect does scaling the variables have on the hierarchical clustering obtained? 
# In your opinion, should the variables be scaled before the inter-observation 
# dissimilarities are computed ? Provide a justification for your answer.
cutree(hc.complete.sd, 3)
table(cutree(hc.complete, 3), cutree(hc.complete.sd, 3))
#   Scaling the variables affect the clusters obtained although the trees are 
#   somewhat similar. The variables should be scaled beforehand because the 
#   data measures have different units.