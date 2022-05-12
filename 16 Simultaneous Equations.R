# Simultaneous Equations
  
# Outline:
#   Simultaneous equations
#   Testing for rank condition

# Data files: 
#   MROZ.csv

# setup
rm(list = ls()) 
directory <- "C:/Econometrics/DataR/"

# Install packages
PackageNames <- c("tidyverse", "stargazer", "magrittr", "AER", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}   


# Simultaneous equations --------------------------------------------------

# Labor supply and demand data for working women
MROZ <- read.csv(paste0(directory, "MROZ.csv"))

# keep only working women
MROZ %<>% filter(inlf == 1)

MROZ %>% 
  select(hours, lwage, educ, exper, expersq, age, kidslt6, nwifeinc) %>% 
  stargazer(type = "text")

MROZ %>% 
  select(hours, lwage, educ, exper, expersq, age, kidslt6, nwifeinc) %>% 
  head(10)

# Regression for hours using OLS estimation
model1 <- lm(hours ~ lwage + educ + age + kidslt6 + nwifeinc, MROZ)
summary(model1)

# Regression for hours using 2SLS estimation
# lwage is instrumented by variables from the other equation
model2 <- ivreg(hours ~ lwage + educ + age + kidslt6 + nwifeinc | 
                  . - lwage + exper + expersq, data = MROZ)
summary(model2, diagnostics = TRUE)

# Regression for lwage using OLS estimation
model3 <- lm(lwage ~ hours + educ + exper + expersq, MROZ)
summary(model3)

# Regression for lwage using 2SLS estimation
# hours is instrumented by variables from the other equation
model4 <- ivreg(lwage ~ hours + educ + exper + expersq | 
                  ~ . - hours + age + kidslt6 + nwifeinc, 
                data = MROZ)
summary(model4, diagnostics = TRUE)


# Testing for rank condition ----------------------------------------------

# Testing for rank condition involves estimating the reduced form equation 
# and testing for significance of the instrument variables.

# Reduced form equation for lwage, identifying equation for hours
model5 <- lm(lwage ~ educ + age + kidslt6 + nwifeinc + exper + expersq, MROZ)
summary(model5)
linearHypothesis(model5, c("exper = 0", "expersq = 0"))

# Reduced form equation for hours, identifying equation for lwage
model6 <- lm(hours ~ educ + age + kidslt6 + nwifeinc + exper + expersq, MROZ)
summary(model6)
linearHypothesis(model6, c("age = 0", "kidslt6 = 0", "nwifeinc = 0"))
