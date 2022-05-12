# Survival Analysis in R


# install.packages("survival")
library(survival)

mydata<- read.csv("C:/Econometrics/Data/survival_unemployment.csv")
attach(mydata)

# Define variables 
time <- spell
event <- event
X <- cbind(logwage, ui, age)
group <- ui

# Descriptive statistics
summary(time)
summary(event)
summary(X)
summary(group)

# Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(time,event) ~ 1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="Survival Probability")

# Kaplan-Meier non-parametric analysis by group
kmsurvival1 <- survfit(Surv(time, event) ~ group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")

# Nelson-Aalen non-parametric analysis
nasurvival <- survfit(coxph(Surv(time,event)~1), type="aalen")
summary(nasurvival)
plot(nasurvival, xlab="Time", ylab="Survival Probability")


# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time,event) ~ X, method="breslow")
summary(coxph)


# Exponential, Weibull, and log-logistic parametric model coefficients
# Opposite signs from Stata results, Weibull results differ; same as SAS
exponential <- survreg(Surv(time,event) ~ X, dist="exponential")
summary(exponential)

weibull <- survreg(Surv(time,event) ~ X, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(time,event) ~ X, dist="loglogistic")
summary(loglogistic)

# ADDITIONAL CODE

# Kaplan-Meier plots with legend
plot(kmsurvival1, xlab="Time", ylab="Survival Probability", col=c("red", "black"))
legend("bottomleft", c("not insured", "insured"), bty="n", col=c("red", "black"))

# Rescaling the Weibull coefficients using survreg() gives the same results as Stata
weibull$coefficients/weibull$scale

library(SurvRegCensCov) 

# Converting the coefficients of the Weibull model using survreg() gives the same results as Stata
ConvertWeibull(weibull) 

# Weibull model estimated using SurRegCensCov package
WeibullReg(Surv(time,event) ~ X) 

