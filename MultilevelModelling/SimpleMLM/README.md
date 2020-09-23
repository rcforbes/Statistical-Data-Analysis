# Simple MLM

## Overview
In this project I analyze a dataset collected by Luke and Krauss (2004) on tobacco legislators. 

For more information on this study, see:
Luke, D. A., & Krauss, M. (2004). Where there's smoke there's money: tobacco industry campaign contributions and US Congressional voting. American Journal of Preventive Medicine, 27(5), 363-372.

## Variables
The dataset (TobaccoLegislators.dat) includes:
- the proportion of times each legislator voted in favour of big tobacco
companies on votes that were relevant to the tobacco companies (“voting) - the amount of monetary donations that each legislator received from tobacco companies (“money”)
- the amount of tobacco acreage in the legislator’s state (“acres”).

These variables are summarized in the accompanying data dictionary (“Tobacco Data Dictionary.pdf”).

## Analysis
In the associated R file:
- I prepared the  data by checking variable skew, mutate variables when necessary, and centre predictors
- I modelled pro-tobacco voting as a function of corporate donations and tobacco acreage. I modeled this effect both as a random intercept model and a random slope and intercept model.
- I compared both models, and chose the best model based on fit statistics.
- I calculated ICC and effect sizes for the random slope model
- I then created a Figure to visualize the reuslts

## Results
Results are written up in the associated .docx file.

