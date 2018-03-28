# a) ***************************************************************************

# setwd()
college <- read.csv("College.csv")


# b) ***************************************************************************

# Adjusting data to recognize row names.
fix(college)
rownames(college) <- college[,1]
fix(college)
college <- college[,-1]
fix(college)


# c) ***************************************************************************

# Summary and pairs plot.
# i.
summary(college)

# ii.
pairs(college)
ref10 <- college[,1:10] # Reference the first ten cols of college
pairs(ref10)

# Boxplot
# iii.
attach(college)
plot(Private, Outstate)

# Binning
# iv.
Elite <- rep("No", nrow(college))	
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
    # "Top10perc" is binned, creating a group of unis where more than 50% of 
    # students were from their high school's top 10%.

college <- data.frame(college, Elite)
summary(college)
plot(Elite, Outstate)

# Histograms
# v.
par(mfrow = c(2, 4))
hist(Accept)
hist(PhD)
hist(Top10perc)
hist(Room.Board)

hist(Enroll)
hist(Terminal)
hist(Top25perc)
hist(Personal)
