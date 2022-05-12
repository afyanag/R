# Regression Specification

# Outline:
#   RESET
#   Proxy variables
#   Measurement error in the dependent and independent variables

# Data files: 
#   wage1.csv
#   wage2.csv

# setup
rm(list = ls()) 
directory <- "C:/Econometrics/DataR/"

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


# RESET -------------------------------------------------------------------
# RESET includes squares and cubes of the fitted values in the
# regression model and tests for joint coefficient significance.

# Wage example
wage1 <- read.csv(paste0(directory, "wage1.csv"))

# Regression model of wage
model1 <- lm(wage ~ educ + exper + tenure, wage1)
summary(model1)
wage1 %<>% mutate(yhat = fitted(model1),
                  yhatsq = yhat^2,
                  yhatcube = yhat^3)

# RESET, testing for joint significance of coefficients on yhatsq and yhatcube
model1_RESET <- update(model1, ~ . + yhatsq + yhatcube)
summary(model1_RESET)
# The null hypothesis is the model is well-specified
linearHypothesis(model1_RESET, c("yhatsq = 0", "yhatcube = 0"))
# p-value<0.05 so the model is misspecified.

# Regression model of log wage
model2 <- lm(lwage ~ educ + exper + tenure, wage1)
summary(model2)
wage1 %<>% mutate(lyhat = fitted(model2),
                  lyhatsq = lyhat^2,
                  lyhatcube = lyhat^3)
model2_RESET <- update(model2, ~ . + lyhatsq + lyhatcube, wage1)
summary(model2_RESET)
linearHypothesis(model2_RESET, c("lyhatsq = 0", "lyhatcube = 0"))

# Generating squares of variables
wage1 %<>% mutate(educsq = educ^2,
                  tenuresq = tenure^2)

# Regression model of wage including square terms
model3 <- update(model1, ~ . + educsq + expersq + tenuresq)
summary(model3)
wage1 %<>% mutate(yhat1 = fitted(model3),
                  yhat1sq = yhat1^2,
                  yhat1cube = yhat1^3)
model3_RESET <- update(model3, ~ . + yhat1sq + yhat1cube)
summary(model3_RESET)
linearHypothesis(model3_RESET, c("yhat1sq = 0", "yhat1cube = 0"))

# Regression model of lwage including square terms
model4 <- update(model2, ~ . + educsq + expersq + tenuresq)
summary(model4)
wage1 %<>% mutate(lyhat1 = fitted(model4),
                  lyhat1sq = lyhat1^2,
                  lyhat1cube = lyhat1^3)
model4_RESET <- update(model4, ~ . + lyhat1sq + lyhat1cube, wage1)
summary(model4_RESET)
linearHypothesis(model4_RESET, c("lyhat1sq = 0", "lyhat1cube = 0"))
# This is a correctly specified model.


# Proxy variable ----------------------------------------------------------
  
# Wage example with IQ
wage2 <- read.csv(paste0(directory, "wage2.csv"))

# Generate a standard normal variable
set.seed(0)
# set.seed (0) sets the same seed number for replication purposes
# random numbers may not be replicated across software because of different algorithms
wage2 %<>% mutate(r = rnorm(n = nrow(wage2)))
head(wage2$r, 10)

# Generate a fake "abil" variable
wage2 %<>% mutate(abil1 = 5 + 10*IQ + r,
                  abil = round(abil1, 0))

# New dataset
wage2 %>% select(wage, educ, abil, IQ) %>%
  stargazer(type = "text", digits = 1)

wage2 %>% select(wage, educ, abil, IQ) %>%
  head(5)

# True model with educ and ability 
# wage = beta0 + beta1*educ + beta2*abil + u
model5 <- lm(wage ~ educ + abil, wage2)
summary(model5)
(beta2 <- coef(model5)["abil"])

# Model with omitted variable abil, coefficient on educ is biased
model6 <- update(model5, ~ . - abil)
summary(model6)

# IQ is proxy for omitted variable abil
# Model for abil on IQ
# abil = delta0 + delta2*IQ + v
model6 <- lm(abil ~ IQ, wage2)
summary(model6)
(delta2 <- coef(model6)["IQ"])

# Model with IQ as proxy for abil
model7 <- lm(wage ~ educ + IQ, wage2)
summary(model7)
coef(model7)["IQ"]
beta2*delta2
# The coefficient on educ is not biased in a model with proxy.
# The coefficient on the proxy variable is a multiple of two coefficients
# beta2 (coeff on ability in wage eq) and delta2 (coeff on IQ in abil eq).


# Measurement error -------------------------------------------------------

# Generate mismeasured variable wage_m, rounded up to next 5 dollars
wage1 %<>% mutate(wage_m = case_when(
  wage > 20 ~ 25,
  wage > 15 ~ 20,
  wage > 10 ~ 15,
  wage > 5 ~ 10,
  wage > 0 ~ 5
))

# Generate mismeasured variable exper_m, rounded up to next 10 years
wage1 %<>% mutate(exper_m = case_when(
  exper > 40 ~ 50,
  exper > 30 ~ 40,
  exper > 20 ~ 30,
  exper > 10 ~ 20,
  exper > 0 ~ 10
))

select(wage1, wage, wage_m, exper, exper_m) %>% head(10)
select(wage1, wage, wage_m, exper, exper_m) %>% stargazer(type = "text")

# Model with no measurement error
# model1 <- lm(wage ~ educ + exper + tenure, wage1)
summary(model1)

# Model with mismeasured dependent variable wage_m
model8 <- update(model1, wage_m ~ .)
summary(model8)
# Coefficients are not biased, but the coefficient on exper became insignificant.

# Model with mismeasured independent variable exper_m
model9 <- update(model1, ~ . - exper + exper_m)
summary(model9)
# Attenuation bias - coefficient on exper is biased toward zero and became insignificant.
