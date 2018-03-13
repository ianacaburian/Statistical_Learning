
auto = read.table("Auto.data")    # contains errors to be fixed
fix(auto)

auto = read.table
(	
"Auto.data", 
header = T,           # sets first line as variable names to fix missing value errors.
na.strings = "?"      # sets values marked in the data with a "?" char as NA values.
)
fix(auto)

# Since only five rows contain missing observations,
# na.omit() can be used to simply remove them.
dim(auto)             # [1] 397	9
auto = na.omit(auto)
dim(auto)             # [1] 392	9

# Once corrected, use names() to check var names
names(auto)

# b)
# Range of quantitative predictors
attach(auto)
range(mpg)
range(displacement)
range(horsepower)
range(weight)
range(acceleration)

# c)
# Mean and sd
mean(mpg)          # also found in summary()
sd(mpg)
sd(displacement)
sd(horsepower)
sd(weight)
sd(acceleration)

# d)
# Remove obs 10 - 85
auto2 = auto[-(10:85),]
summary(auto)
summary(auto2)    # means changed

# e)
# Plots
cylinders = as.factor(cylinders)    # qualitative vars with numerical values require they be explicitly set as such
plot(cylinders, mpg)
plot(cylinders , mpg , col =" red ")
plot(cylinders , mpg , col =" red ", varwidth =T)
plot(cylinders , mpg , col =" red ", varwidth =T, horizontal =T)
plot(cylinders , mpg , col =" red ", varwidth =T, xlab =" cylinders ", ylab =" MPG ")

# Histograms
hist(mpg)
hist(mpg, col = 2)
hist(mpg, col = 2, breaks = 15)

# Pairs plots
pairs(auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, auto)

# Identify a vars val by left clicking on a point, right click to exit
plot(horsepower, mpg)
identify(horsepower, mpg, name)

# f)
plot(cylinders, mpg)    # mpg for 8 cyl is signif lower than the rest
plot(horsepower, mpg)   # as horsepower increases, mpg decreases
plot(weight, mpg)       # as weight increases, mpg decreases
plot(name, mpg)         # no apparent relationship between name and mpg