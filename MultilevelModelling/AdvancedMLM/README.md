# Simple MLM

## Overview
In this project I analyze a dataset collected by Tritt, Page-Gould, Peterson, and Inzlicht (2014) on Event-Related Potentials from EEG.

For more information on this study, see:
Tritt, S. M., Page-Gould, E., Peterson, J. B., & Inzlicht, M. (2014). System justification and electrophysiological responses to feedback: Support for a positivity bias. Journal of Experimental Psychology: General, 143(3), 1004-1010.

## Variables
The dataset (ERP_Data.csv) includes:
- feedback type participants received on a repeated measures tasks where they would see a target enter the screen and they were instructed to hit the spacebar when it reached the exact centre of the screen for a small performance bonus ("positive" or "negative")
- ERP amplitudes after recieving feedback ("FRN", Feedback Related Negativity): higher numbers reflect greater deflection/FRN amplitude
- participants' scores on neuroticism ("neuroticism")

These variables are summarized in the accompanying data dictionary (“FRN Data Dictionary.pdf”)

## Analysis
In the associated R file:
- I prepared the  data by checking variable skew and mutated variables when necessary
- I modelled the relationship between feedback type and ERP, moderated by neuroticism with a 3-level model and a cross-classified model
- I compared both models, and chose the best model based on fit statistics.
- I calculated ICC and effect sizes for the random slope model
- I then created a Figure to visualize the reuslts

## Results
Results are written up in the associated .docx file.

