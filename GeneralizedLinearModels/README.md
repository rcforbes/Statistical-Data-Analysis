# Generalized Linear Models

## Overview
In this project I analyze the 2010 General Social Survey dataset, which is a large-scale, nationally-representative survey of demographics and attitudes among U.S. adults

For more information on this study, see:
https://gss.norc.org/

## Variables
The dataset (2010.sav) includes:
- the number of hours per week that a person spends on the Internet (“WWWHR”)
- vocabulary (“WORDSUM”)
- age (“AGE”)
- sex (“SEX”)
- religiosity (“RELITEN”)
- number of children (“CHILDS”)
- marital status (“MARITAL”)
- political orientation (“POLVIEWS”)
- sexual orientation (“SEXORNT”)
- how often they work from home (“WRKHOME”).
- whether they think that a university professor should be fired if it is discovered that he/she believes in communism (“COLCOM”)

Variables are described in the associated GSS_Codebook.pdf file

## Analysis
In the associated R file:
- I prepared the  data by centering continuous predictors or effect coding categorical predictors
- I conducted a logistic regression, where the likelihood that a respondent will think the
communist teacher should be fired was predicted by the respondent’s age, sex, number of children, marital status, religiosity, and political orientation. One predictor of interest (political orientation) was plotted.
- I conducted a poisson regression, where the number of sexual partners that a person has had in theprevious 5 years was predicted by their age , sex, sexual orientation, marital status, religiosity, and political orientation. One predictor of interest (sex) was plotted.
- I conducted a negative binomial regression, where the number of hours per week that a person spends on the internet was predicted by their vocabulary, age, sex, religiosity, political orientation, and how often they work from home. One predictor of interest (vocabulary) was plotted.

## Results
Results are written up in the associated .docx file.

