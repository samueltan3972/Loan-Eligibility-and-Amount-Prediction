if(!require("dplyr")) { install.packages("dplyr")  }
if(!require('sjmisc')) { install.packages('sjmisc') }
if(!require('DescTools')) { install.packages('DescTools') }
if(!require('VIM')) { install.packages('VIM') }

library(dplyr)
library('sjmisc')
library('DescTools')
library('VIM')

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# # Prequel: Combine all dataset into one new dataset
# training_set = read.csv("./Data/training_set.csv")
# testing_set = read.csv("./Data/testing_set.csv")
# sample_submission = read.csv("./Data/sample_submission.csv")
# 
# head(sample_submission)
# head(testing_set)
# 
# ## combine testing set with its approval status
# combined_testing_sample = inner_join(testing_set, sample_submission[, c("Loan_ID", "Loan_Status")], by = "Loan_ID")
# head(combined_testing_sample)
# 
# 
# ## Check if colnames is matched
# print(colnames(training_set) == colnames(combined_testing_sample))
# combined_training_testing = rbind(combined_testing_sample,training_set) %>% arrange(Loan_ID)
# View(combined_training_testing)
# 
# ## Write new combined dataset
# write.csv(combined_training_testing, "./Data/combined_training_testing.csv", row.names = FALSE)


# Import Dataset
dataset = read.csv("./Data/combined_training_testing.csv")
head(dataset)
glimpse(dataset)

# 1. Data Scrub

# 1.1 Check and deal the missing value
# Check number of empty value in a dataset
print.numOfEmtpyValue <- function(data) {
  # create empty matrix
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df) = c("Column_Names", "Num_Of_Empty_Value")
  
  for(i in 1:ncol(data)) {
    df[nrow(df) + 1,] <- c(colnames(data)[i], sum(is.na(data[, i])|is.null(data[, i])|data[, i] == ""))
  }
  print(df)
}

print.numOfEmtpyValue(dataset)
View(dataset)

# Replace empty value with NA
# replaceEmptyValueWithNA <- function(data){
#   cname = names(data)
#   
#   for(i in cname){
#     data[!is.na(data[i]) & data[i] == "" | is.null(data[i]), i] = NA
#     print(paste(i, sum(data[i]=="" | is.null(data[i]))))
#   }
#   
#   return(data)
# }
# 
# dataset = replaceEmptyValueWithNA(dataset)

dataset = na_if(dataset, "")
print.numOfEmtpyValue(dataset)

colnames(dataset)

# 1.1.1 Sequential, Random Hot deck imputation on categorical Columns with less empty value (Married, Education)
dataset = hotdeck(dataset, variable = c("Married"), domain_var = c("Gender", "Dependents", "Education", "Self_Employed", "Credit_History", "Loan_Status"), imp_var=FALSE)
dataset = hotdeck(dataset, variable = c("Education"), domain_var = c("Gender", "Dependents", "Married", "Self_Employed", "Credit_History", "Loan_Status"), imp_var=FALSE)

print.numOfEmtpyValue(dataset)

# 1.1.2 KNN -> Dependents, Self_Employed, Credit_History, Loan_Amount_Term
dataset = kNN(dataset, variable = c("Dependents"), dist_var = c("Gender", "Education", "Married", "Self_Employed", "Credit_History", "Loan_Status"), imp_var=FALSE)
dataset = kNN(dataset, variable = c("Self_Employed"), dist_var = c("Gender", "Dependents", "Married", "Education", "Credit_History", "Loan_Status"), imp_var=FALSE)
dataset = kNN(dataset, variable = c("Credit_History"), dist_var = c("Gender", "Dependents", "Married", "Self_Employed", "Education", "Loan_Status"), imp_var=FALSE)
dataset = kNN(dataset, variable = c("Loan_Amount_Term"), dist_var = c("ApplicantIncome", "CoapplicantIncome", "LoanAmount", "property_Area", "Credit_History", "Loan_Status"), imp_var=FALSE)

print.numOfEmtpyValue(dataset)

# 1.1.3 Regression Imputation -> ApplicantIncome, CoapplicantIncome, LoanAmount
dataset = regressionImp(ApplicantIncome~Gender+Self_Employed, data=dataset, imp_var=FALSE)
# dataset = regressionImp(CoapplicantIncome~Gender+Married+Self_Employed, data=dataset, imp_var=FALSE)
dataset = regressionImp(LoanAmount~Gender+Dependents+Married+Self_Employed+ApplicantIncome+CoapplicantIncome+Credit_History+property_Area+Loan_Status, data=dataset, imp_var=FALSE)

print.numOfEmtpyValue(dataset)

# # Replace NA value in "Dependents" with 0 (default value)
# dataset["Dependents"] = replace_na(dataset["Dependents"], value=0)
# head(dataset)
# print.numOfEmtpyValue(dataset)
# 
# # Replace NA value in "Education" with mode
# dataset["Education"] = replace_na(dataset["Education"], value=Mode(dataset[,"Education"], na.rm=TRUE))
# head(dataset)
# print.numOfEmtpyValue(dataset)
# 
# # Replace NA value in "Self_Employed" with mode
# dataset["Self_Employed"] = replace_na(dataset["Self_Employed"], value=Mode(dataset[,"Self_Employed"], na.rm=TRUE))
# head(dataset)
# print.numOfEmtpyValue(dataset)
# 
# # Replace NA value in "ApplicantIncome" with 0 (default value)
# dataset["ApplicantIncome"] = replace_na(dataset["ApplicantIncome"], value=0)
# head(dataset)
# print.numOfEmtpyValue(dataset)

# 1.1.4 Replace NA value in "CoapplicantIncome" with 0 (default value)
dataset["CoapplicantIncome"] = replace_na(dataset["CoapplicantIncome"], value=0)
head(dataset)
print.numOfEmtpyValue(dataset)


dataset %>% count(property_Area)
# 
# which(dataset[,"ApplicantIncome"] == 0)
# View(dataset[293,])

# 1.2 Remove unimportant column
dataset <- subset (dataset, select = -c(Loan_ID, Gender))
head(dataset)

# 1.3 Convert LoanAmount into 100k base
dataset["LoanAmount"] = round(dataset["LoanAmount"] * 1000)
head(dataset)

# 1.4 Label encoding the categorical data
dataset$Married = ifelse(dataset$Married == "Yes", 1, 0)
dataset['Dependents'][dataset['Dependents'] == "3+"] = 3
dataset$Education = ifelse(dataset$Education == "Graduate", 1, 0)
dataset$Self_Employed = ifelse(dataset$Self_Employed == "Yes", 1, 0)
dataset$property_Area = as.numeric(factor(dataset$property_Area))
dataset$Loan_Status = ifelse(dataset$Loan_Status == "Y", 1, 0)

# 1.5 Split into training and testing set
sample <- sample(c(TRUE, FALSE), nrow(dataset), replace=TRUE, prob=c(0.8,0.2))
training_set  <- dataset[sample, ]
testing_set   <- dataset[!sample, ]

glimpse(training_set)
glimpse(testing_set)

write.csv(dataset, "./Data/processed_dataset.csv", row.names = FALSE)
write.csv(training_set, "./Data/processed_training_set.csv", row.names = FALSE)
write.csv(testing_set, "./Data/processed_testing_set.csv", row.names = FALSE)

# 2. EDA


# 3. Model

