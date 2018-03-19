library(ISLR) # needed for Carseats data

lm.fit = lm(Sales ~. + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# constrasts() returns the coding R uses for the dummy vars
attach(Carseats)
contrasts(ShelveLoc) # Quality of the Shelve Location of the carseats

summary(lm.fit)
    # The coefficient for ShelveLocGood is positive, indicating it is associated with high sales
    # ShelveLocMedium is less but still positive, indicating it is still preferred over "Bad"