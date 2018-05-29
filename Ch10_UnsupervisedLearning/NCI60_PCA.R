library(ISLR)
nci.labs = NCI60$labs
nci.data = NCI60$data

# The NCI60 cancer cell line microarray data, which consists of 
# 6830 gene expression measurements on 64 cancer cell lines.
# Each cell line is labeled with a cancer type.
# We do not make use of the cancer types in performing PCA and
# clustering, we will check to see the extent to which these 
# cancer types agree with the results of these unsupervised techniques.

dim(nci.data)
#> 64 6830

# Begin by examining the cancer types for the cell lines.
nci.labs[1:4]
table(nci.labs)

# PCA
pr.out = prcomp(nci.data, scale = TRUE)
#   One could reasonably argue that it is better not to scale the degrees.

# Assign a colour to each of the 64 cell lines based on the cancer type 
# it corresponds to.
Cols = function(vec) {
    cols = rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}

# Plot the PC score vectors.
par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = " Z2")
plot(pr.out$x[, c(1, 3)], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = " Z3")
#   Cell lines corresponding to a single cancer type do tend to have
#   Similar values on the first few principal component score vectors.
#   This indicates that cell lines from the same cancer type tend
#   to have pretty similar gene expression levels.

# Summary of the PVE of the pc's
summary(pr.out)

# Plot the variance explained.
plot(pr.out)
#   Note that the height of each bar is given by squaring the corresponding
#   element of pr.out$sdev.

# Scree plot (each pc PVE) and cumulative PVE.
pve = 100 * pr.out$sdev^2 / sum(pr.out$sdev^2)
par(mfrow = c(1, 2))
plot(pve, type = "o", ylab = " PVE ", xlab = " Principal Component ",
     col = " blue ")
plot(cumsum(pve), type = "o", ylab = " Cumulative PVE ",
     xlab = "Principal Component ", col = " brown3 ")
#   Together, it is sen that the first 7 pc's explain around 40% of the variance.
#   This is not a huge amount. However, looking at the scree plot we see that
#   while each of the first 7 pc's explain a substantial amount of variance,
#   there is a marked decrease in the variance explained by further pc's.
#   That is, there is an "elbow" in the plot after around the 7th pc.
#   This suggests that there may be little benefit to examining more than
#   7 or so pc's.
