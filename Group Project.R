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

# 1. Data Scrubcd 

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

reg_dataset=dataset[dataset$Loan_Status==1,]
reg_sample = sample(c(TRUE, FALSE), nrow(reg_dataset), replace=TRUE, prob=c(0.8,0.2))
reg_training_set  = reg_dataset[reg_sample, ]
reg_testing_set   = reg_dataset[!reg_sample, ]

write.csv(reg_dataset, "./Data/reg_processed_dataset.csv", row.names = FALSE)
write.csv(reg_training_set, "./Data/reg_processed_training_set.csv", row.names = FALSE)
write.csv(reg_testing_set, "./Data/reg_processed_testing_set.csv", row.names = FALSE)

# 2. EDA
summary(df)
#programming for data science


# Exploratory data anaysis


df$Married <- as.factor(df$Married)
df$Dependents <- as.factor(df$Dependents)
df$Education <- as.factor(df$Education)
df$Self_Employed <- as.factor(df$Self_Employed)
df$ApplicantIncome <- as.numeric(df$ApplicantIncome)
df$CoapplicantIncome <- as.numeric(df$CoapplicantIncome)
df$LoanAmount <- as.numeric(df$LoanAmount)
df$Loan_Amount_Term <- as.numeric(df$Loan_Amount_Term)
df$Credit_History <- as.factor(df$Credit_History)
df$property_Area <- as.factor(df$property_Area)
df$Loan_Status <- as.factor(df$Loan_Status)

install.packages('ggplot')
library(ggplot2)
library(tidyverse)

# Checking Missing Values from Dataset : df
num_na <- sapply(df, function(x) sum(is.na(x)))
num_na[num_na > 0]

#distribution of Applications Income
ggplot(df , aes(x=ApplicantIncome,bins=5,fill=Loan_Status))+
  geom_histogram()+theme_bw()+
  labs(x="Income of Applicants",
       y= "Number of Applicants",
       title ="Distribution of Applicant's Income")

# Number of Married People
plot.married <- ggplot(df, aes(x = Married, fill = Loan_Status)) +
  geom_bar()

ggplot(df, aes(x = Education, fill = Loan_Status)) +
  geom_bar(position = "fill") +
  labs(y = "Approval Status", x = 'Education Level') + ggtitle(' Education level vs Loan Status')

  #Education Level seems like does not make any difference.

ggplot(data = df, aes(LoanAmount)) + geom_histogram(binwidth = 10000)

# 3. Model
 
#3.1. Logistic Regression
# Install packages for data manipulation, visualization and model training
install.packages("tidyverse")
install.packages("caret")
install.packages("MLmetrics")

library(tidyverse)
library(caret)
library(MLmetrics)

#Performance function
get_performance_v1 <- function(y_true, y_pred){
  library(MLmetrics)
  # Convert predicted probabilities to binary labels
  y_pred_binary <- as.factor(ifelse(y_pred > 0.5, "1", "0"))
  # Convert true labels to factor
  y_true <- as.factor(y_true)
  # Make sure that the levels of y_true and y_pred_binary are the same
  levels(y_pred_binary) <- levels(y_true)
  # Confusion matrix
  cm <- confusionMatrix(y_pred_binary, y_true)
  # Accuracy
  acc <- cm$overall[1]
  # Precision
  prec <- cm$byClass[1]
  # Recall
  rec <- cm$byClass[2]
  # F1-score
  f1 <- F1_Score(y_pred_binary, y_true)
  # Return a list of evaluation metrics
  return(list(accuracy = acc, precision = prec, recall = rec, f1_score = f1, confusion_matrix = cm))
}

#Read split dataset
train_set <- read.csv("processed_training_set.csv")
test_set <- read.csv("processed_testing_set.csv")

# Split train set into data (x_train) and target(y_train)
x_train<-train_set[,!names(train_set) %in% "Loan_Status"]
y_train=train_set$Loan_Status

# Split test set into data (x_test) and target(y_test)
x_test<-test_set[,!names(test_set) %in% "Loan_Status"]
y_test<-test_set$Loan_Status

#Building of model
set.seed(1)
logistic_def <- glm(formula = y_train ~ ., data = x_train, family = "binomial")

#Prediction using model
logistic_def_pred <- predict(logistic_def, newdata = x_test, type = "response")

#Evaluate model performance
logistic_def_result <- get_performance_v1(y_test, logistic_def_pred)

#3.2. Support Vector Machine
#Install packages for data manipulation, visualization and model training
install.packages("e1071")

library(e1071)
library(caret)

#Building of model
set.seed(1)
svm_model_def <- svm(y_train ~ ., data = x_train, kernel = "linear", cost = 1)

#Prediction using model
svm_pred_def <- predict(svm_model_def, x_test)

#Evaluate model performance
svm_result_def <- get_performance_v1(y_test, svm_pred_def)

#3.3. Naive Bayes
#Install packages for data manipulation, visualization and model training
install.packages("e1071")

library(e1071)
library(caret)

#Building of model
set.seed(1)
nb_model_def <- naiveBayes(x_train, y_train)

#Prediction using model
nb_pred_def <- predict(nb_model_def, x_test)

#Evaluate model performance
nb_pred_def_numeric <- as.numeric(nb_pred_def)
nb_result_def <- get_performance_v1(y_test, nb_pred_def_numeric)

#3.4. Decision Tree
#Install packages for data manipulation, visualization and model training
install.packages("rpart")

library(rpart)

#Building of model
set.seed(1)
dt_model_def <- rpart(formula = y_train ~ ., data = x_train)

#Prediction using model
dt_pred_def <- predict(dt_model_def, x_test)

#Evaluate model performance
dt_result_def <- get_performance_v1(y_test, dt_pred_def)

#3.5. XGBoost
#Install packages for data manipulation, visualization and model training
install.packages("xgboost")

library(xgboost)

# convert data to matrix
x_train_matrix <- as.matrix(x_train)
x_test_matrix <- as.matrix(x_test)

#Building of model
set.seed(1)
xgb_model_def <- xgboost(data = x_train_matrix, label = y_train, nrounds = 100, objective = "binary:logistic")

#Prediction using model
xgb_pred_def <- predict(xgb_model_def, x_test_matrix)

#Evaluate model performance
xgb_result_def <- get_performance_v1(y_test, xgb_pred_def)

#Final result dataframe to evaluate all models under classification
model_results_df <- data.frame(
  +     Model = c("Logistic Regression", "SVM", "Naive Bayes", "Decision Tree", "XGBoost"),
  +     Accuracy = c(logistic_def_result$accuracy, svm_result_def$accuracy, nb_result_def$accuracy, dt_result_def$accuracy, xgb_result_def$accuracy),
  +     Precision = c(logistic_def_result$precision, svm_result_def$precision, nb_result_def$precision, dt_result_def$precision, xgb_result_def$precision),
  +     Recall = c(logistic_def_result$recall, svm_result_def$recall, nb_result_def$recall, dt_result_def$recall, xgb_result_def$recall),
  +     F1_Score = c(logistic_def_result$f1_score, svm_result_def$f1_score, nb_result_def$f1_score, dt_result_def$f1_score, xgb_result_def$f1_score))
