# Regression with Indicator Variables

# Outline:
#   Single indicator variable
#   Interaction terms with another indicator variable
#   Several related indicator variables
#   Interaction terms with a non-indicator variable
#   F-test for differences across groups
#   Chow test for differences across groups
# 
# Data files:
#   wage1.csv

# setup
rm(list = ls()) 
directory <- "C:/Econometrics/DataR/"

# Install packages
PackageNames <- c("tidyverse", "stargazer", "broom", "magrittr", "car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

# Single indicator variable --------------------------------------------
# Wage example
wage1 <- read.csv(paste0(directory, "wage1.csv"))
wage1 %>% 
  select(wage, educ, exper, tenure, female, married) %>% 
  head(10)
wage1 %>% 
  select(wage, educ, exper, tenure, female, married) %>%
  str
wage1 %>% 
  select(wage, educ, exper, tenure, female, married) %>%
  stargazer(type = "text")

# Average wage
lm(wage ~ 1, wage1) %>% summary
wage1 %>% select(wage) %>% stargazer(type = "text")

# Regression of wage on female
lm(wage ~ female, wage1) %>% summary

# Graph of wage on female
ggplot(wage1, aes(x = female, y = wage)) +
  theme_bw() +
  geom_point() +
  geom_smooth(aes(col = 'fitted line'), method = "lm", se = F)

# Summary statistics for groups of females and males
wage1 %>% 
  select(wage) %>% # keep 'wage'
  split(wage1$female) %>%  # split the data.frame based on 'female'
  walk(~ stargazer(., type = "text")) # summary stats for each group

# T-test for average wage for females and males
t.test(formula = wage ~ female, data = wage1)
# The mean, t-statitic, and p-value are the same for the coefficient on female 
# in the regression and the t-test comparing wages for females and males.

# Generate indicator variable for male
wage1 %<>% mutate(male = 1 - female)

# Regression of wage on female and regression of wage on male
lm(wage ~ female, wage1) %>% summary
lm(wage ~ male, wage1) %>% summary
# The coefficient on male has the same magnitude and significance 
# but opposite sign from the coefficient on female.

# Regression with female and male cannot be estimated because of perfect collinearity
lm(wage ~ female + male, wage1) %>% summary

# Regression with female and male can be estimated with no constant
lm(wage ~ 0 + female + male, wage1) %>% summary


# Interaction terms with another indicator variable --------------------

# Generate indicator variables
wage1 %<>% mutate(single = 1 - married)

# Categories: female*single, male*single, female*married, male*married
wage1 %<>% mutate(female_single  = female*single,
                    male_single  =   male*single,
                  female_married = female*married,
                    male_married =   male*married)

# List indicator variables and interaction terms for indicator variables
wage1 %>% 
  select(female, male, single, married, female_single, male_single,
         female_married, male_married) %>%
  head(10)

# Regression with all four categories, R will drop one
lm(wage ~ female_single + male_single + female_married + male_married, 
   wage1) %>% summary

# Regression with male_single as reference category
model_1 <- lm(wage ~ female_single + female_married + male_married, wage1)
summary(model_1)

# Marginal effect for female and single on wage
coef(model_1)['female_single']

# Marginal effect for female and married on wage
coef(model_1)['female_married']

# Alternative categories: female, married, and femaleXmarried

# Generate interaction variable
wage1 %<>% mutate(femaleXmarried = female*married)

# Regression with interaction term
model_2 <- lm(wage ~ female + married + femaleXmarried, wage1)
summary(model_2)

# Marginal effect for female and single on wage
coef(model_2)['female']

# Marginal effect for female and married on wage
coef(model_2)['female'] + coef(model_2)['married'] + 
  coef(model_2)['femaleXmarried']


# Several related indicator variables ----------------------------------

# Regression with several related indicator variables needs to drop one as 
# the reference category.

# Indicator variables for region (northcentral, south, and west)
wage1 %<>% mutate(east = 1 - northcen - south - west)
wage1 %>% select(northcen, south, west, east) %>% stargazer(type = "text")

# Average wage by region
filter(wage1, northcen == 1) %>% select(wage) %>% stargazer(type = "text")
filter(wage1,    south == 1) %>% select(wage) %>% stargazer(type = "text")
filter(wage1,     west == 1) %>% select(wage) %>% stargazer(type = "text")
filter(wage1,     east == 1) %>% select(wage) %>% stargazer(type = "text")

# Regression with east as the reference category
lm(wage ~ northcen + south + west, wage1) %>% summary

# Regression with west as the reference category
lm(wage ~ northcen + south + east, wage1) %>% summary

# Regression with east as the reference category
lm(wage ~ educ + northcen + south + west, wage1) %>% summary

# Regression with west as the reference category
lm(wage ~ educ + northcen + south + east, wage1) %>% summary


# Interaction terms with a non-indicator variable ----------------------

# Regression of wage on educ 
# Model with same intercept and slope for females and males
model_3 <- lm(wage ~ educ, wage1) 
summary(model_3)

ggplot(wage1, aes(x = educ)) + 
  theme_bw() + 
  geom_point(aes(y = wage)) +
  geom_smooth(aes(y = wage), method = lm, se = F)

# Regression of wage on educ and female
# Model with same slope but different intercepts for females and males
model_4 <- lm(wage ~ educ + female, wage1)
summary(model_4)
wage1 %<>% mutate(wagehat = fitted(model_4))

# Graph of wage on education, same slope and different intercepts for females
# and males

# Convert female into a factor(categorical) variable for plotting
wage1 %<>%
  mutate(gender = factor(female, levels = 1:0, labels = c('female', 'male')))

ggplot(data = wage1, mapping = aes(x = educ, col = gender)) + 
  theme_bw() + 
  geom_point(aes(y = wage)) + 
  geom_line(aes(y = wagehat)) +
  guides(color = guide_legend(title = 'gender'))

# Regression of wage on education and male
model_5 <- lm(wage ~ educ + male, wage1)
summary(model_5)
# The coefficient on male has the same magnitude and significance 
# but opposite sign than coefficient on female.

# Regression of wage on educ for females
model_6 <- lm(wage ~ educ, wage1, subset = (female == 1))
summary(model_6)

# Regression of wage on education for males
model_7 <- lm(wage ~ educ, wage1, subset = (female == 0))
summary(model_7)

# Graph of wage on education, different slopes and intercepts for females 
# and males
ggplot(wage1, aes(x = educ, y = wage, col = gender, group = gender)) + 
  theme_bw() + 
  geom_point() + 
  geom_smooth(method = lm, se = F, lwd = .5) + # lwd = line width
  guides(color = guide_legend("gender")) # legend title

# Generate an interaction term between female and education
wage1 %<>% mutate(femaleXeduc = female*educ)

# Regression of wage on educ, female, and female*educ
# Model with different intercepts and slopes for females and males
model_8 <- lm(wage ~ educ + female + femaleXeduc, wage1)
summary(model_8)

# Intercepts and slopes for males and females
b <- coef(model_8)
print(paste0("intercept for males is ", b['(Intercept)']))
print(paste0("intercept for females is ", b['(Intercept)'] + b['female']))
print(paste0("marginal effect of education on wage for males is ", b['educ']))
print(paste0("marginal effect of education on wage for females is ", 
             b['educ'] + b['femaleXeduc']))
# Same coefficients as running two separate regressions for female and male.


# F-test for differences across groups ---------------------------------

# F-test for joint coefficient significance is used to test for several 
# coefficients to be jointly significantly different from zero.

# Generate interaction terms
wage1 %<>% mutate(femaleXeduc = female*educ,
                  femaleXexper = female*exper,
                  femaleXtenure = female*tenure)


# H0: delta0=0 and delta1=0 and delta2=0 and delta3=0

# Restricted model: wage = alpha0 + alpha1*educ + alpha2*exper + 
# alpha3*tenure + e
model_9 <- lm(wage ~ educ + exper + tenure, wage1)
ssr_r <- sum(resid(model_9)^2)

# Unrestricted model: wage = beta0 + beta1*educ + beta2*exper + beta3*tenure
# + delta0*female + delta1*female*educ + delta2*female*exper + 
# delta3*female*tenure + u
model_10 <- lm(wage ~ educ + exper + tenure + female + femaleXeduc + 
                 femaleXexper + femaleXtenure, wage1)
ssr_ur <- sum(resid(model_10)^2)
q <- 4

# Calculate F_stat 
# df_resid is degrees of freedom for residual for unrestricted model (n-k-1)
df_resid <- model_10$df.residual
(F_stat <- ((ssr_r - ssr_ur)/q) / (ssr_ur/df_resid))

# F-critical value
qf(0.95, q, df_resid)
# If F-stat > F-critical value then reject null, 
# coefficients are jointly significant

# p-value for F-test
(F_pvalue <- pf(F_stat, q, df_resid, lower.tail = F))
# If F p-value<0.05 then reject null, coefficients are jointly significant

# F-test using R commands
linearHypothesis(model_10, c('female        = 0', 
                             'femaleXeduc   = 0',
                             'femaleXexper  = 0',
                             'femaleXtenure = 0'))


# Chow test for differences across groups ------------------------------

# Chow test is an F-test for significantly different coefficients between 
# two groups. Instead of one unrestricted model, two separate models are 
# estimated for each group.  

# Wage example

# H0: beta0=alpha0 and beta1=alpha1 and beta2=alpha2 and beta3=alpha3

# Restricted model: wage = gamma0 + gamma1*educ + gamma2*exper + 
# gamma3*tenure + v
# Same as model_9
model_11 <- model_9
summary(model_11)
k <- 3
n <- nobs(model_11)
ssr_r <- sum(resid(model_11)^2)

# Unrestricted model for females: 
# wage = beta0 + beta1*educ + beta2*exper + beta3*tenure + u
model_12 <- update(model_11, subset = (female == 1))
ssr1 <- sum(resid(model_12)^2)

# Unrestricted model for males: 
# wage = aplha0 + alpha1*educ + alpha2*exper + alpha3*tenure + e
model_13 <- update(model_11, subset = (female == 0))
ssr2 <- sum(resid(model_13)^2)

# Calculate Chow F-statistic
(F_stat <- ((ssr_r-(ssr1+ssr2))/(k+1)) / ((ssr1+ssr2)/(n-2*(k+1))))

# F-critical value
qf(0.95, k+1, n-2*(k+1))
# If F-stat > F-critical value then reject null, coefficients are jointly significant

# p-value for F-test
(F_pvalue <- pf(F_stat, k+1, n-2*(k+1), lower.tail = F))
# If F p-value<0.05 then reject null, coefficients are jointly significant
