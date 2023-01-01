# WQD7004 Programming for Data Science

### Notes: Please use dataset that start with prefix "processed_*.csv"
### Notes 2: The cleaned data is process with Group Project.R, please continue with the modeling with 'dataset' or 'training_set', 'testing_set' variable in Group Project.R 

## Problem Statement: 

A Company wants to automate the loan eligibility process based on customer details provided while filling online application form. The details filled by the customer are Gender, Marital Status, Education, Number of Dependents, Income of self and co applicant, Required Loan Amount, Required Loan Term, Credit History and others. The requirements are as follows:

1. Check eligibility of the Customer given the inputs described above.(Classification)
2. If customer is not eligible for the input required amount and duration, what can be amount for the given duration.(Regression)

---

**1. OSEMN**

Obtain\
Scrub - Samuel \
Exploratory - Mohan, Samuel \
Model 1 - Hafiza, Boon Sheng \
Model 2 - Boon Sheng, Auni \
Interpret

---

**Data preprocessing:**
1. Combine training and testing set and sample_submission
2. Fill in missing value/Remove (Loan amount empty)
3. Convert to 100k
4. Label or one-hot encoding for categorical data
5. Split into new training and testing set

**Exploration:**
1. Check for missing value
2. Visualization
3. Automated EDA

**Model:**
1. Classification: X - Married, Dependents, Education, etc Y - Loan_Status
2. Regression: X - Married, Dependent, Education, etc, Y - Loan_Amount
