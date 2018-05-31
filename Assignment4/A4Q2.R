
load("asx99.Rdata")
dates <- row.names(asx99)
names(asx99)
dim(asx99)
n <- dim(asx99)[1]
qbe <- asx99[, "QBE"]
Y <- qbe[2:n]
X <- asx99[n-1,]

means <- apply(asx99, 2, mean)
sd <- apply(asx99, 2, sd)
qbesd <- sd["QBE"]
qbesd
qbemean <- means["QBE"]
qbemean

pr.out = prcomp(t(asx99), scale = T)
# Scree plot (each pc PVE) and cumulative PVE.
pve = 100 * pr.out$sdev ^ 2 / sum(pr.out$sdev ^ 2)
par(mfrow = c(1, 2))
plot(pve, type = "o", ylab = " PVE ", xlab = " Principal Component ",
     col = " blue ")
plot(cumsum(pve), type = "o", ylab = " Cumulative PVE ",
     xlab = "Principal Component ", col = " brown3 ")

set.seed(1)
km.out = kmeans(pr.out$x[, 1:3], 16, nstart = 20)
km.out$cluster

hc.out = hclust(dist(pr.out$x[, 1]), method = 'average')
hc.clusters = cutree(hc.out, 40)
table(km.out$cluster, hc.clusters)