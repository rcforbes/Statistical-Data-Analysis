### Simple Multilevel Model ###

#### Prepare Workspace ####
# Load Packages
library(tidyverse) 
library(lme4) 
library(nlme) 
library(emmeans) 
library(e1071)
library(dplyr)
library(summarytools)

# Read Data
TobaccoData = read.delim("TobaccoLegislators.dat")

# Look at the data
TobaccoData %>%
  View

#  Prepare Data
# Check skewness 
TobaccoData %>%
  pull(money) %>%
  skewness(na.rm = TRUE)

TobaccoData %>%
  mutate(log_money = log(money+1)) %>%
  pull(log_money) %>%
  skewness(na.rm = TRUE)

TobaccoData %>%
  pull(acres) %>%
  skewness(na.rm = TRUE)

TobaccoData %>%
  mutate(root_5_acres = acres**(1/5)) %>%
  pull(root_5_acres) %>%
  skewness(na.rm = TRUE)

TobaccoData = TobaccoData %>%
  mutate(log_money = log(money+1))
TobaccoData = TobaccoData %>%
  mutate(root_5_acres = acres**(1/5))

# Grand and Group Mean Centre Predictors
TobaccoData = TobaccoData %>% 
  mutate(centered_root5_acres = root_5_acres - mean(root_5_acres, na.rm = TRUE)) %>% #Grand mean centre acres
  group_by(state) %>% 
  mutate(centered_log_money = log_money - mean(log_money, na.rm = TRUE)) %>%  #Group centre money
  ungroup
 
# Random intercept MLM
random_intercept_model = TobaccoData %>%
  lmer( voting ~ (1|state) + centered_root5_acres + centered_log_money,
        na.action = "na.exclude", data = .)
random_intercept_model %>%
  summary
Rintercept_emmeans = random_intercept_model %>%
  emmeans(c("centered_root5_acres", "centered_log_money"), 
          at = list(centered_root5_acres = c(-1*sd(TobaccoData$centered_root5_acres, na.rm = TRUE), 
                                                 sd(TobaccoData$centered_root5_acres, na.rm = TRUE)),
                    centered_log_money    = c(-1*sd(TobaccoData$centered_log_money, na.rm = TRUE), 
                                                 sd(TobaccoData$centered_log_money, na.rm = TRUE)))
  )

# Random slope MLM
random_slope_model = TobaccoData %>%
  lmer( voting ~ (1+centered_log_money|state) + centered_root5_acres + centered_log_money,
        na.action = "na.exclude", data = .)
random_slope_model %>%
  summary
          
# Model Comparison
anova(random_intercept_model, random_slope_model)

# Calculate the ICC 
calculate_icc_lmer = function(x) { # x is the MLM model object for your baseline model, created by lmer
  random_effects = data.frame(VarCorr(x))
  intercept_variance  = random_effects[random_effects$var1 == "(Intercept)", 'vcov'][1]
  residual_variance  = random_effects[random_effects$grp == "Residual", 'vcov']
  icc  = intercept_variance / (residual_variance + intercept_variance)
  return(icc)
}
random_slope_model %>% 
  calculate_icc_lmer

# Calculate Effect Sizes
# Print results to calculate partial R^2 values manually
library(car)
random_slope_model %>%
  Anova(type = 3, test = "F")

# Function to automatically calculate partial R2 for each predictor
calculate_partial_R2_lmer = function(x) { # x is any MLM model object created by lmer
  require(car)
  results = data.frame(Anova(x, type = 3, test = "F"))[-1,]
  parameters = row.names(results)
  numerator_df = results["Df"]
  denominator_df = results["Df.res"]
  F_values = results["F"]
  partial_R2 = (((numerator_df/denominator_df)*F_values)/(1+((numerator_df/denominator_df)*F_values)))
  names(partial_R2)[1] = "Partial_R2"
  return(partial_R2)
}
random_slope_model %>%
  calculate_partial_R2_lmer

# Figure
Rslope_emmeans <- random_slope_model %>%
  emmeans(c("centered_root5_acres", "centered_log_money"), 
          at = list(centered_root5_acres = c(-1*sd(TobaccoData$centered_root5_acres, na.rm = TRUE), 
                                             sd(TobaccoData$centered_root5_acres, na.rm = TRUE)),
                    centered_log_money    = c(-1*sd(TobaccoData$centered_log_money, na.rm = TRUE), 
                                              sd(TobaccoData$centered_log_money, na.rm = TRUE))))

emmip(Rslope_emmeans, centered_root5_acres~centered_log_money, xlab= "Money", ylab= "Voting", CIs = TRUE)

