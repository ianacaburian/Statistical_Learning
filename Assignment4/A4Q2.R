load("asx99.Rdata")
dates <- row.names(asx99)
names(asx99)
dim(asx99)
n <- dim(asx99)[1]

# Question 2.1
qbe <- asx99[, "QBE"]
Y <- qbe[2:n]

# Question 2.2
X <- asx99[n-1,]

# Question 2.3, 2.4 and 2.5
# PCA to produce component score vectors for k-means clustering.
pr.out = prcomp(t(asx99), scale = T)

# Scree plot (each pc PVE) and cumulative PVE.
pve = 100 * pr.out$sdev ^ 2 / sum(pr.out$sdev ^ 2)
par(mfrow = c(1, 2)) # Figure 5
plot(pve, type = "o", ylab = " PVE ", xlab = " Principal Component ",
     col = " blue ")
plot(cumsum(pve), type = "o", ylab = " Cumulative PVE ",
     xlab = "Principal Component ", col = " brown3 ")

# K-means clustering on principle score vectors.
set.seed(1)
k = 16
km.out = kmeans(pr.out$x[, 1:3], k, nstart = 20)
sort(km.out$cluster) # Table 7