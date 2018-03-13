rm(list=ls(all=TRUE))
setwd("C:/GitRepos/Statistical_Learning/2.4_Exercises")

# a)
college = read.csv("College.csv")

# b)
# Adjusting data to recognize row names.
fix(college)
rownames(college) = college[,1]
fix(college)
college = college[,-1]
fix(college)

# c)
# Summary and pairs plot.
summary(college)
pairs(college)

# Reference the first ten cols of college
ref10 = college[,1:10]
pairs(ref10)

# Boxplot
attach(college) 							# saves accessing vars via college$Private
plot(Private, Outstate)						# plot() will boxplot if its x arg is qualitative

# "Top10perc" is binned to create "Elite" 
# as the group of unis where more than 
# 50% of students were from their 
# high school's top 10%.
Elite = rep("No", nrow(college))			# replicates values
Elite[college$Top10perc > 50] = "Yes"

Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
plot(Elite, Outstate)

# Histograms
par(mfrow=c(2,2))
# Percent of faculty with Ph.D.'s
hist(PhD)
hist(Grad.Rate)
hist(Top10perc)
hist(Top25perc)
