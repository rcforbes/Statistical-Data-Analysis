### Generalized Linear Models ###

#### Prepare Workspace ####
# Load Packages 
library(tidyverse) 
library(lme4) 
library(nlme) 
library(emmeans) 
library(dplyr)
library(haven)
library(pscl)
library(MASS)

# Read Data 
GSSData = read_spss('2010.sav')

# Look at the data
GSSData %>%
  View

# Logistic Regression 
# Prepare Data 
# See if any predictors are already centered
GSSData %>%
  summarise_at(c('childs', 'age', 'reliten', 'polviews'), mean, na.rm=T)

# Centre continuous predictors
GSSData = GSSData %>%
  mutate(c.childs = childs - mean(childs, na.rm=T),
         c.age = age- mean(age, na.rm = T),
         c.reliten = reliten - mean(reliten, na.rm = T),
         c.polviews = polviews - mean(polviews, na.rm = T)
  )

# Effect code categorical predictors
# See if sex and marital are already effect coded
GSSData %>%
  count(sex)
GSSData %>%
  count(marital)

# Effect-code marital
GSSData$marital = as.factor(GSSData$marital)
contrasts(GSSData$marital) = contr.sum(5)

# Recode colcom
GSSData$colcom[GSSData$colcom==4] <- 0
GSSData$colcom[GSSData$colcom==5] <- 1

# Logisitic Regression 
logistic_model = GSSData %>%
  glm(colcom ~ c.childs + c.age + sex + c.reliten + c.polviews + marital, 
      family = binomial, data = .) 
logistic_model %>%
  summary

GSSData$polviews
# Calculate the Odds Ratio of the outcome for a one-unit increase in each predictor
logistic_model %>%
  coefficients %>%
  exp

# Estimated means for plotting
emmeans(logistic_model,
        "c.polviews",
        at = list(c.polviews= c(sd(GSSData$c.polviews, na.rm = TRUE),
                            -1*sd(GSSData$c.polviews, na.rm = TRUE))))
# Figure
# Convert emmeans to probabilities
exp(0.2236295)/(1+exp(0.2236295))
exp(0.7363211)/(1+exp(0.7363211))

# Convert SE bounds to probabilities
exp(0.2236295+0.1158814)/(1+exp(0.2236295+0.1158814))
exp(0.2236295-0.1158814)/(1+exp(0.2236295-0.1158814))
exp(0.7363211+0.1210130)/(1+exp(0.7363211+0.1210130))
exp(0.7363211-0.1210130)/(1+exp(0.7363211-0.1210130))

# Create dataframe for plotting
dataframe <- read.table(text=
                          "x y
                        liberal 0.5557
                        conservative 0.6762", header= T)
# Create plot
library(ggplot2);
ggplot(dataframe, aes(as.factor(x), y), group =1) +
  geom_point() +
  ylim(0.4, 0.8) +
  geom_errorbar(ymin=c(0.5269, 0.6491), ymax=c(0.5841, 0.7021), width=.05)+
  labs(y="Probability of Firing Decision", x="Political Orientation")+
  geom_line(group=1)


# Poisson
# Prepare Data  

# Effect code categorical predictors
# See if marital and sexornt are already effect coded
GSSData %>%
  count(SEXORNT)

# Effect-code sexornt
GSSData$SEXORNT = as.factor(GSSData$SEXORNT)
contrasts(GSSData$SEXORNT) = contr.sum(3)

# Run the Poisson Regression
poisson_model = GSSData %>%
  glm(partnrs5 ~ c.age + sex + SEXORNT + marital + c.reliten + c.polviews, 
      family = poisson, 
      data = .)
poisson_model %>%
  summary

# Get effect size by going to https://stefany.shinyapps.io/RcountD/
# Effect size for var of interest (sex) is SMD = 0.165

# Estimated means for plotting
emmeans(poisson_model,
        "sex",
        at = list(sex = c(-1*sd(GSSData$sex, na.rm = TRUE),
                         sd(GSSData$sex, na.rm = TRUE))))

# Figure
# Convert emmeans to probabilities
exp(0.4678375)
exp(0.7006937)

# Convert SE bounds to probabilities
exp(0.4678375+0.06768608)
exp(0.4678375-0.06768608)
exp(0.7006937+0.07029666)
exp(0.7006937-0.07029666)

# Create dataframe for plotting
dataframe2 <- read.table(text=
                          "x y
                        female 1.5965
                        male 2.0151", header= T)

# Create plot
library(ggplot2);
ggplot(dataframe2, aes(as.factor(x), y), group =1) +
  geom_point() +
  ylim(1 ,2.5) +
  geom_errorbar(ymin=c(1.492051, 1.878356), ymax=c(1.708342, 2.161906), width=.05)+
  labs(y="Sexual Partners in the last 5 years", x="Sex")+
  geom_line(group=1)

# Negative binomial 
# Centre any predictors
GSSData %>%
  summarise_at(c('wrkhome', 'wordsum'), mean, na.rm=T)

GSSData = GSSData %>% 
  mutate(c.wrkhome = wrkhome - mean(wrkhome, na.rm=T),
         c.wordsum = wordsum - mean(wordsum, na.rm=T)
         )
  
# Analysis
negative_binomial = GSSData %>%
  glm.nb(wwwhr ~ c.wordsum + c.age + sex + c.reliten + c.polviews + c.wrkhome, data = .)
negative_binomial %>%
  summary

# Estimated means for plotting
emmeans(negative_binomial,
        "c.wordsum",
        at = list(c.wordsum = c(-1*sd(GSSData$c.wordsum, na.rm = TRUE),
                          sd(GSSData$c.wordsum, na.rm = TRUE))))

# Figure
# Convert emmeans to probabilities
exp(1.930367)
exp(2.3796590)

# Convert SE bounds to probabilities
exp(1.930367+0.07783645)
exp(1.930367-0.07783645)
exp(2.379659+0.06113062)
exp(2.379659-0.06113062)

# Create dataframe for plotting
dataframe3 <- read.table(text=
                           "x y
                         Low 6.892039
                         High 10.80122", header= T)

# Create plot
library(ggplot2);
ggplot(dataframe3, aes(as.factor(x), y), group =1) +
  geom_point() +
  ylim(5, 12) +
  geom_errorbar(ymin=c(6.375934, 10.16071), ymax=c(7.449921, 11.4821), width=.05)+
  labs(y="Hours on the Internet", x="Wordsum")+
  geom_line(group=1)

# Get effect size by going to https://stefany.shinyapps.io/RcountD/
# Effect size for var of interest (wordsum) is SMD = 0.108
