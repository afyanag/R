# Multinomial Probit and Logit Models in R
# Copyright 2013 by Ani Katchova

# install.packages("mlogit")
library(mlogit)

mydata<- read.csv("C:/Econometrics/Data/multinomial_fishing1.csv")
attach(mydata)

# Descriptive statistics
table(mode)

# Reshaping the data from wide to long format
#mydata$mode<-as.factor(mydata$mode)
mldata<-mlogit.data(mydata, varying=4:15, choice="mode", shape="wide")
mldata[1:20,]

# Multinomial logit model coefficients 
mlogit.model1 <- mlogit(mode ~ 1 | income, data=mldata, reflevel="charter")
summary(mlogit.model1)

# Multinomial logit model coefficients (with different base outcome)
mlogit.model2 <- mlogit(mode ~ 1 | income, data = mldata, reflevel="pier")
summary(mlogit.model2)

# Multinomial logit model odds ratios 
exp(coef(mlogit.model1))


# Conditional logit model
clogit.model1 <- mlogit(mode ~ price+catch |income, data = mldata, reflevel="charter")
summary(clogit.model1)

clogit.model2 <- mlogit(mode ~ price+catch | income, data = mldata, reflevel="pier")
summary(clogit.model2)


# Setting mean values for variables to use for marginal effects 
m <- mlogit(mode ~ price+catch |income, data = mldata, reflevel="charter")
z <- with(mldata, data.frame(price = tapply(price, index(m)$alt, mean), 
                             catch = tapply(catch, index(m)$alt, mean), income = mean(income)))

# Multinomial logit model marginal effects
effects(mlogit.model1, covariate = "income", data = z)

# Conditional logit model marginal effects
effects(clogit.model1, covariate = "income", data = z)
effects(clogit.model1, covariate = "price", data = z)
effects(clogit.model1, covariate = "catch", data = z)

# Multinomial probit model coefficients 
#mprobit.model1 <- mlogit(mode ~ 1 | income, data = mldata, reflevel="charter", probit=TRUE)
#summary(mprobit.model1)


# Hauseman-McFadden test of independence of irrelevant alternatives
m1<- mlogit(mode ~ 1 | income, data = mldata, reflevel="beach")
m2<- mlogit(mode ~ 1 | income, data = mldata, reflevel="beach", alt.subset=c("beach", "pier", "private"))
hmftest(m1, m2)

