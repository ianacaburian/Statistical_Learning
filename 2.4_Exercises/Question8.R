setwd("C:/GitRepos/Statistical_Learning/2.4_Exercises")
getwd()

# a)
college <- read.csv("College.csv")

# b)
fix(college)
rownames(college)=college[,1]
fix(college)
college=college[,-1]
fix(college)

# c)
summary(college)
pairs(college)

# Reference the first ten cols of college
ref10 <- college[,1:10]
pairs(ref10)

attach(college) # Alternatively, access vars via college$Private
plot(Private, Outstate)
# NB: Categorical var is passed as x arg for boxplot.



