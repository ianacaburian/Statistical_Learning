library(ISLR)
names(Smarket)
# Lag1 - 5: percentage returns for 5 previous trading days
# Volume: num (in billions) of shares traded on the previous day
# Today: percentage return on the date in questions
# Direction: whether market was up or down on this date

summary(Smarket)
pairs(Smarket)


# Correlation matrix
cor(Smarket[,-9]) # excludes qualitative var 'Direction'
    # Only notable correlation is between 'Year' and 'Volume'

attach(Smarket)
plot(Volume)
    # Some confirmation that 'Volume' is increasing over time.

