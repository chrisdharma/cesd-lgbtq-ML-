# cesd-lgbtq-ML-
Using machine learning analysis on the LGBTQI2S+ Tobacco Project Survey dataset on LGBTQI2S+ young adults in Ontario and Quebec, we identified important predictors for depressive symptoms and new mental health diagnoses within a one year time period. 

R codes shown for the two outcomes (depressive symptoms and new MH diagnoses) separately. Each dataset was entered with Multiple Imputation Chained Equation (MICE) 5 times. 

01_datastep = program where data cleaning and data manipulation was done. See codebook for more details
Depressive_symtpoms_CESD_to_post = program where analysis for CESD scores at 1 year was done (RF and LASSO)
diagnosis_changes_RF_to_Post = program where analysis for new MH diagnosis at 1 year was done (RF and LASSO)

