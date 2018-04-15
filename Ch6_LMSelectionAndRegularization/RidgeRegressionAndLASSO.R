library(ISLR)
# fix(Hitters)
names(Hitters)
dim(Hitters) #=> 322 20
sum(is.na(Hitters$Salary)) #=> 59

Hitters <- na.omit(Hitters) # remove all of the rows that have missing values
dim(Hitters) #=> 263 20
sum(is.na(Hitters)) #=> 0
#_______________________________________________________________________________

