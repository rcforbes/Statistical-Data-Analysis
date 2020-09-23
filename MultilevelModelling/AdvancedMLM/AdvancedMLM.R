### Advanced Multilevel Model ###

#### Prepare Workspace ####
# Load Packages
library(tidyverse) 
library(lme4) 
library(nlme) 
library(emmeans) 
library(e1071)
library(dplyr)

# Read Data
FRNData = read_csv("ERP_Data.csv")

# Look at the data
FRNData %>%
  View

#  Prepare Data
# Check skewness 
FRNData %>%
  pull(FRN) %>%
  skewness(na.rm = TRUE)
FRNData %>%
  pull(c.neuroticism) %>%
  skewness(na.rm = TRUE)

# Three-level multilevel model 
three_level_MLM = lmer(FRN ~ (1|participant/trial) + valence*c.neuroticism, data = FRNData, na.action = "na.exclude")
summary(three_level_MLM)

# Three-level multilevel model figure
plot_3way <- three_level_MLM %>%
  emmeans(c("c.neuroticism", "valence"),
          at = list(c.neuroticism = c(-1*sd(FRNData$c.neuroticism, na.rm=T),
                                      sd(FRNData$c.neuroticism, na.rm=T)),
                    valence= c(-1, 1)) 
  )
emmip(plot_3way, c.neuroticism~valence, ylab= "FRN", CIs = TRUE)

# Cross-classified model
cross_classified_model = FRNData %>%
  lmer( FRN ~ (1|participant/trial) + (1|participant/electrode) + 
          c.neuroticism*valence, 
        na.action="na.exclude", data=.)
cross_classified_model %>% 
  summary

# Cross-classified model figure
plot_cross <- cross_classified_model %>%
  emmeans(c("c.neuroticism", "valence"),
          at = list(c.neuroticism = c(-1*sd(FRNData$c.neuroticism, na.rm=T),
                                      sd(FRNData$c.neuroticism, na.rm=T)),
                    valence= c(-1, 1)) 
  )

emmip(plot_cross, c.neuroticism~valence, ylab = "FRN", CIs = TRUE)

# Model Comparison
# Compare by AIC
abs(AIC(three_level_MLM) - AIC(cross_classified_model))
AIC(three_level_MLM)
AIC(cross_classified_model)

# Compare BIC
abs(BIC(three_level_MLM) - BIC (cross_classified_model))
BIC(three_level_MLM)
BIC(cross_classified_model)

# Calculate the ICC 
calculate_icc_lmer = function(x) { # x is the MLM model object for your baseline model, created by lmer
  random_effects = data.frame(VarCorr(x))
  intercept_variance  = random_effects[random_effects$var1 == "(Intercept)", 'vcov'][1]
  residual_variance  = random_effects[random_effects$grp == "Residual", 'vcov']
  icc  = intercept_variance / (residual_variance + intercept_variance)
  return(icc)
}
cross_classified_model %>% 
  calculate_icc_lmer

# Calculate Effect Sizes
library(car)
cross_classified_model %>%
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
cross_classified_model %>%
  calculate_partial_R2_lmer

