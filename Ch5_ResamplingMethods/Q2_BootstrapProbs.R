# g) 
# Figure 1
n <- 1:100000
plot(n, 1 - (1 - 1/n)^n)

# h)
store <- rep(NA, 10000)
set.seed(1)
for (i in 1:10000)
{
    store[i] <- sum(sample(1:100, rep = TRUE) == 4) > 0
}
mean(store) #> 0.6408

