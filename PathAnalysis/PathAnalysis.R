### Path Analysis ###

#### Prepare Workspace ####
# Load Libraries
library(tidyverse)
library(lavaan)
library(semPlot)

# Read Data
setwd("/Users/rachelforbes/Google Drive/PhD1/PSY2002/Lecture 3/Lab1B")
interest_data = read_tsv("interest.csv") 

# Look at the data
interest_data %>%
  View

# Theoretical model path analysis
# Model specification
fireman_model = '
impulsivity~ a*age + b*thrill.seeking
fireman ~ c*impulsivity + sex
age.through.impulsivity:=a*c #looking at the paths
thrill.seeking.through.impulsivity:=b*c
'

# Estimate the path model
fireman_analysis = fireman_model %>%
  sem(data = interest_data)

# Print results, including the fit statistics
fireman_analysis %>%
  summary(fit.measures = TRUE, 
          standardized = TRUE)

# Figure 1: unconstrained
fireman_analysis %>%
  semPaths(whatLabels = "est", 
           rotation = 2, 
           nCharNodes = 0, 
           label.cex = 1
  )

locations = matrix(c(  0,   0, 
                       .5,   0, 
                       -.5,  .5, 
                       -.5,   0, 
                       -.5, -.5), 
                   ncol=2, 
                   byrow=2
)
labels = c("Impulsivity",
           "Fireman\nCareer",
           "Age",
           "Thrill\nSeeking",
           "Sex"
)
fireman_analysis %>%
  semPaths(whatLabels="est", 
           nodeLabels = labels, 
           layout = locations, 
           sizeMan = 12
  )

# Constrained model path analysis
# Model specification
fireman_model_constrained = '
impulsivity~ 0*age + b*thrill.seeking
fireman ~ c*impulsivity + sex
thrill.seeking.through.impulsivity:=b*c
'

# Estimate the path model
fireman_analysis_constrained = fireman_model_constrained %>%
  sem(data = interest_data)

# Print results, including the fit statistics
fireman_analysis_constrained %>%
  summary(fit.measures = TRUE, 
          standardized = TRUE)

# Figure 2: constrained
fireman_analysis_constrained %>%
  semPaths(whatLabels = "est", 
           rotation = 2, 
           nCharNodes = 0, 
           label.cex = 1
  )

locations = matrix(c(  0,   0, 
                       .5,   0, 
                       -.5,  .5, 
                       -.5,   0, 
                       -.5, -.5), 
                   ncol=2, 
                   byrow=2
)
labels = c("Impulsivity",
           "Fireman\nCareer",
           "Age",
           "Thrill\nSeeking",
           "Sex"
)
fireman_analysis_constrained %>%
  semPaths(whatLabels="est", 
           nodeLabels = labels, 
           layout = locations, 
           sizeMan = 12
  )


# Compare the two models (likelihood ratio test)
anova(fireman_analysis, fireman_analysis_constrained)

# Non-nested model path analysis
# Model specification
fireman_model_nonnested = '
impulsivity ~ thrill.seeking
fireman ~ impulsivity + stress.reactivity
'

# Estimate path model
fireman_analysis_nonnested = sem(fireman_model_nonnested, data=interest_data)

fireman_analysis_nonnested %>% 
  summary(fit.measures = TRUE, 
          standardized = TRUE) 

# Figure 3: non-nested model
nonnested_locations = matrix(c(0, 0, 
                               .5, 0, 
                               -.5, 0, 
                               -.5, .5), 
                             ncol=2, 
                             byrow=2)
nonnested_labels = c("Impulsivity",
                     "Fireman\nCareer",
                     "Thrill\nSeeking",
                     "Stress\nReactivity")
fireman_analysis_nonnested %>%
  semPaths(whatLabels = "est", 
           nodeLabels = nonnested_labels, 
           layout = nonnested_locations, 
           sizeMan = 12
  )

# Compare by AIC
abs(AIC(fireman_analysis) - AIC(fireman_analysis_nonnested))
AIC(fireman_analysis)
AIC(fireman_analysis_nonnested)

# Compare BIC
abs(BIC(fireman_analysis) - BIC (fireman_analysis_nonnested))
BIC(fireman_analysis)
BIC(fireman_analysis_nonnested)

