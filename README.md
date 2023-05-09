# SARS-CoV-2_viral-load_trajectories
We engineered a viral load change after infection with Omicron BA.5.2.48 in a population working in the public health industry. The response of Ct values over time was dynamically reflected by multiple imputation for missing values and the construction of a generalized additive model (GAM) of Ct values. The file contains data and implemented code.

"raw_data.csv" records the results of fluorogenic quantitative PCR after collecting throat swabs (sheet1) and nasal swabs (sheet2) for each person every day, and organizes them into a comparative format (sheet3). 
"all_1.csv" is the data after multiple imputation based on the raw data. 
The implementation codes of multiple imputation and GAM construction are all in "cohort_MI&GAM.R"
