# Simple and Multiples Mediations

## Overview
In this project I analyze a subset of data from the Job Search Intervention Study (JOBSII), which was a longitudinal study of well-being among 899 residents of the U.S. stateof Michigan who were unemployed when they were recruited for the study. 

For more information on this study, see:
Vinokur, A. D., & Schul, Y. (1997). Mastery and inoculation against setbacks as active ingredients in the JOBS intervention for the unemployed. Journal of Consulting and Clinical Psychology, 65(5), 867-877.

## Variables
The dataset (Jobs.csv) includes:
- demographic variables of “age” and “sex” (1 = male, -1 = female)
- family household income on a 5-point ordinal scale (“income”)
- a measure of subjective financial strain (“financial.strain”)
- a measure of confidence in oneself regarding finding a job (“search.confidence”)
- two measurements of depression, one measured during an initial background survey (“depression.t1”) and the other measured 2 months later (“depression.t2”)

These variables are summarized in the accompanying data dictionary (“Jobs Data Dictionary.pdf”).

## Analysis
In the associated R file:
- I pre-process the data (predictors are centered).
- I conduct a simple mediation analysis for the relationship between income and depressive symptoms mediated by financial strain using causal steps and product of coefficients approach, followed up by a Sobel test (which tests the reliability of the indirect path) using an external calculator.
- I conduct the same mediation model using a bootstrap approach with 1000 then 5000 resamples.
- I conduct a multiple mediation analysis for the relationship between income and depressive symptoms mediated by financial strain and confidence in job search.

Note: the mediations_patch must be added to your R environment.

## Results
Results are written up in the associated .docx file.

