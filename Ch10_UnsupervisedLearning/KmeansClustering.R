
# Simulate an example where there a two true clusters.
set.seed(2)
x = matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4

# Perform K-means clustering with K = 2.
km.out = kmeans(x, 2, nstart = 20)

# Cluster assignments
km.out$cluster

# Plot observations coloured according to assignment.
plot(x, col = (km.out$cluster + 1),
    main = "K- Means Clustering Results with K=2",
    xlab = "", ylab = "", pch = 20, cex = 2)

# Try K = 3
set.seed(4)
km.out = kmeans(x, 3, nstart = 20)
km.out
plot(x, col = (km.out$cluster + 1),
     main = "K- Means Clustering Results with K=3",
     xlab = "", ylab = "", pch = 20, cex = 2)

# Run kmeans() with multiple initial cluster assignments with nstart().
set.seed(3)
km.out = kmeans(x, 3, nstart = 1)
km.out$tot.withinss
km.out = kmeans(x, 3, nstart = 20)
km.out$tot.withinss
#   Tot.withinss is the total within-cluster sum of squares,
#   which we seek to minimize by performing K-means clustering.
#   The individual within-cluster sum-of-squares are contained in the 
#   vector km.out$wihinss.
#   Compare the difference with different values for nstart().

#   It is strongly recommended to run K-means clustering with a large
#   value of nstart, such as 20 or 50, since otherwise an undesirable
#   local optimum may be obtained.