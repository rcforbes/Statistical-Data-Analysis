### Simple and Multiple Mediation ###

#### Prepare Workspace ####
# Load Libraries
library(tidyverse)
library(mediation)
source("mediations_patch.r")

# Read Data
jobs_data = read_tsv("Jobs.csv") 

# Look at the data
jobs_data %>%
  View

# Prepare Data
# Centre predictors
jobs_data = jobs_data %>%
  mutate(financial.strain = financial.strain - mean(financial.strain, na.rm=T),
          search.confidence = search.confidence - mean(search.confidence, na.rm = T),
          income = income - mean(income, na.rm = T)
  )

# Simple mediation analysis
# Causal steps + product of coefficients approach

# Step 1: Regress the mediator on the independent variable
jobs_data %>%
  lm(financial.strain ~ income, data = .) %>%
  summary
# Step 2: Regress the dependent variable on the independent variable
jobs_data %>%
  lm(depression.t2 ~ income, data = .) %>%
  summary
# Step 3: Regress the dependent variable on the independent variable and the mediator at the same time
jobs_data %>%
  lm(depression.t2 ~ income + financial.strain, data = .) %>%
  summary

# Conduct Sobel test here: http://quantpsy.org/sobel/sobel.htm
# Sobel's z = -4.40, p< 0.001

# Simple Mediation using bootstrap approach
# 1000 resamples

step_1 = jobs_data %>%
  lm(financial.strain ~ income, data = .)
step_3 = jobs_data %>%
  lm(depression.t2 ~ income + financial.strain, data = .)
mediation_bootstrap = mediate(step_1, 
                              step_3, 
                              treat="income",
                              mediator="financial.strain", 
                              boot=TRUE, 
                              sims=1000)
mediation_bootstrap %>% summary
mediation_bootstrap %>% standard_errors

# 5000 resamples
step_1 = jobs_data %>%
  lm(financial.strain ~ income, data = .)
step_3 = jobs_data %>%
  lm(depression.t2 ~ income + financial.strain, data = .)
mediation_bootstrap = mediate(step_1, 
                              step_3, 
                              treat="income",
                              mediator="financial.strain", 
                              boot=TRUE, 
                              sims=5000)
mediation_bootstrap %>% summary
mediation_bootstrap %>% standard_errors
mediation_bootstrap %>% plot

# Multiple Mediation Analysis
mediation_data = jobs_data %>% dplyr::select(income, financial.strain, search.confidence, depression.t2)
datasets = list(dataset = mediation_data) 
multiple_mediation = mediations(datasets, 
                                "income", 
                                c("search.confidence", "financial.strain"), 
                                "depression.t2", 
                                boot=TRUE, 
                                sims = 5000)
multiple_mediation %>%
  augment_summary 
multiple_mediation %>%
  plot
