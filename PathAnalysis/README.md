# Path Analysis

## Overview
In this project I analyze a dataset containing ratings of different cognitive abilities, personality characteristics, and expressed interest in various careers.

## Variables
The dataset (interest.csv) includes:
### Cognitive abilities
- vocabulary (“vocabulary”)
- reading comprehension (“reading”)
- performance on a sentence completion task (“sentence.completion”)
- math skills (“math”)
- geometry skills (“geometry”)
- analytical reasoning abilities (“analytical.reasoning”)
### Personality characteristics
- social dominance (“social.dominance”)
- interest in others and interpersonal skills (“sociability”)
- general propensity to be react with stress (“stress.reactivity”)
- propensity to worry (“worry”)
- impulsivity (“impulsivity”)
- thrill-seeking (“thrill-seeking”). Finally,
### Expressed interest in various careers
Positive values indicate being interested in a career and negative values indicate being
disinterested in that career:
- carpenter (“carpentry”)
- forest ranger (“forest.ranger”)
- mortician (“mortician”)
- police officer (“policeman”)
- fire fighter (“fireman”)
- sales representative (“sales.rep”)
- teacher (“teacher”)
- business executive (“business.executive”)
- stock.broker (“stock.broker”)
- artist (“artist”)
- social worker (“social.worker”)
- truck driver (“truck.driver”)
- medical doctor (“doctor”)
- member of the clergy (“clergyman”)
- actor (“actor”)
- lawyer (“lawyer”)
- architect (“architect”)
- landscaper (“landscape”).
Note: the variable “sex” is coded such that men == 1 and women == 2. Further, “interests.csv” is tab-delimited.

These variables are summarized in the accompanying data dictionary (“Interests Data Dictionary.pdf”).

## Analysis
In the associated R file:
- I conduct a path analysis for three models predicting interest in the career of "fireman".
- The first model is my primary theoretical model, which predicts interest in a fireman.career with sex, age, thrill-seeking, and impulsivity, with age and thrill-seeking predicting interest through impulsivity.
- The second model is a constrained model, which posits no relationship between age and impulsivity.
- The third model is an alternative theoretical model, which predicts interest in a fireman career with thrill-seeking, stress-reactivity, and impulsivity, with thrill-seeking predicting interest through impulsivity. 
- Diagrams for each path model are included.
- Models were compared for fit.

## Results
Results are written up in the associated .docx file.

