load("sn.Rdata")
p <- dim(sn)[2]
words <- sn[, 4:p]

# Question 1.1.1
apply(words, 2, mean) # Table 1
apply(words, 2, sd) # Table 2

sc <- scale(words)

# Question 1.1.2
par(mfrow = c(2, 1)) # Figure 1
hc.average = hclust(dist(t(sc)), method = 'average')
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = .9)

hc.complete = hclust(dist(t(sc)), method = 'complete')
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .9)

# Question 1.1.3
sort(cutree(hc.complete, 5))
sort(cutree(hc.complete, 6))
sort(cutree(hc.complete, 7)) 
sort(cutree(hc.complete, 8)) # Table 3
sort(cutree(hc.complete, 9))
sort(cutree(hc.complete, 10))

# Question 1.2
m <- sn[with(sn, gender == "M"),]
f <- sn[with(sn, gender == "F"),]
nrow(m) / nrow(f) #> 4.1 ratio female to male

femagewords <- f[, 3:p]
maleagewords <- m[, 3:p]

# Traditional approach
apply(femagewords, 2, mean) / apply(maleagewords, 2, mean) # Table 4

# PCA
prfem.out = prcomp(femagewords, scale = TRUE)
prmale.out = prcomp(maleagewords, scale = TRUE)
par(mfrow = c(1, 1))
biplot(prfem.out, scale = 0) # Figure 2
biplot(prmale.out, scale = 0) # Figure 3

# Hierarchical clustering applied separately to the genders.
fmwscaled <- scale(f[, 3:p])
mawscaled <- scale(m[, 3:p])
par(mfrow = c(2, 1)) # Figure 4
hc.complete = hclust(dist(t(fmwscaled)), method = 'complete')
plot(hc.complete, main = "Female", xlab = "", sub = "", cex = .9) 
hc.complete = hclust(dist(t(mawscaled)), method = 'complete')
plot(hc.complete, main = "Male", xlab = "", sub = "", cex = .9) 



