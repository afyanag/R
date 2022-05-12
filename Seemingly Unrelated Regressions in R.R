# Seemingly Unrelated Regressions in R
# Copyright 2013 by Ani Katchova

# install.packages("systemfit")
library(systemfit)

mydata <- read.csv("C:/Econometrics/Data/sur_scores.csv")
attach(mydata)

# Defining variables
Y1 <- math
Y2 <- read
X1 <- cbind(female, prog, science) 
X2 <- cbind(female, socst)
eq1 <- Y1 ~ X1
eq2 <- Y2 ~ X2
system <- list (eq1 = eq1, eq2 = eq2)

# Descriptive statistics
summary(Y1)
summary(Y2)
summary(X1)
summary(X2)

# OLS regression
olsreg1 <- lm(Y1 ~ X1)
summary(olsreg1)

olsreg2 <- lm(Y2 ~ X2)
summary(olsreg2)

# SUR
sur <- systemfit(system, method = "SUR", data = mydata)
summary(sur)
