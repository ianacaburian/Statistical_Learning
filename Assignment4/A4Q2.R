load("asx99.Rdata")
names(asx99)
dim(asx99)
n <- dim(asx99)[1]
qbe <- asx99[, "QBE"]
Y <- qbe[2:n]
X <- asx99[n,]

means <- apply(asx99, 2, mean)
sd <- apply(asx99, 2, sd)
sd

pr.out$rotation