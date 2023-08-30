### Title: A2 Hospital Readmissions EDA & Modeling
### Purpose: To create the best model for the Great River Medical Center hospital readmission for diabetic patients.
#### Generating a list to be included in the pilot for improved treatment plans.
### Author: Eunice Brandares
### Date: April 2, 2023

## Setting the working directory
setwd("~/Hult/MBAN/Spring Sem/(7) Visualizing & Analyzing Data with R Methods & Tools/Hult_Visualizing-Analyzing-Data-with-R/personalFiles/A2")
options(scipen = 999)
set.seed(2023)


## Libraries
library(powerjoin)
library(dplyr)
library(vtreat)
library(ggplot2)
library(ggthemes)
library(corrplot)
library(tidyr)
library(naniar)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)


### Loading train datasets ###
# Including changing blanks to NA
pat_train <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTrain.csv', na.strings = "")
med_train <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTrain.csv', na.strings = "")
hos_train <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTrain.csv', na.strings = "")

## Joining all the datasets into one main train dataset
# List of the three datasets
df_list <- list(pat_train, med_train, hos_train)

# Merging all three datasets by tmpID
main_train <- data.frame(power_left_join(df_list, by = "tmpID"))


##############################
### SAMPLE & CLEAN ###
##############################

### Basic EDAs ###
# Checking data dimension
dim(main_train)

# Checking column names
names(main_train)

# Checking first few data
head(main_train)

# Checking datatype
str(main_train)


### Cleaning the data ###

## Checking distinct values per character variable - to check if needs to be changed to NA ##
# Race
# Counting frequency of values
table(main_train$race)
# Count frequency of values as percentages
prop.table(table(main_train$race)) * 100
# Change "?" to NA
main_train$race <- na_if(main_train$race,"?")

# Gender
# Counting frequency of values
table(main_train$gender)
# Count frequency of values as percentages
prop.table(table(main_train$gender)) * 100
# Nothing to change to NA

# Payer Code
# Counting frequency of values
table(main_train$payer_code)
# Count frequency of values as percentages
prop.table(table(main_train$payer_code)) * 100
# Change "?" to NA
main_train$payer_code <- na_if(main_train$payer_code,"?")

# max_glu_serum
# Counting frequency of values
table(main_train$max_glu_serum)
# Count frequency of values as percentages
prop.table(table(main_train$max_glu_serum)) * 100
# Nothing to change to NA

# A1Cresult
# Counting frequency of values
table(main_train$A1Cresult)
# Count frequency of values as percentages
prop.table(table(main_train$A1Cresult)) * 100
# Nothing to change to NA

# metformin
# Counting frequency of values
table(main_train$metformin)
# Count frequency of values as percentages
prop.table(table(main_train$metformin)) * 100
# Nothing to change to NA

# repaglinide
# Counting frequency of values
table(main_train$repaglinide)
# Count frequency of values as percentages
prop.table(table(main_train$repaglinide)) * 100
# Nothing to change to NA

# nateglinide
# Counting frequency of values
table(main_train$nateglinide)
# Count frequency of values as percentages
prop.table(table(main_train$nateglinide)) * 100
# Nothing to change to NA

# chlorpropamide
# Counting frequency of values
table(main_train$chlorpropamide)
# Count frequency of values as percentages
prop.table(table(main_train$chlorpropamide)) * 100
# Nothing to change to NA

# glimepiride
# Counting frequency of values
table(main_train$glimepiride)
# Count frequency of values as percentages
prop.table(table(main_train$glimepiride)) * 100
# Nothing to change to NA

# acetohexamide
# Counting frequency of values
table(main_train$acetohexamide)
# Count frequency of values as percentages
prop.table(table(main_train$acetohexamide)) * 100
# Nothing to change to NA

# glipizide
# Counting frequency of values
table(main_train$glipizide)
# Count frequency of values as percentages
prop.table(table(main_train$glipizide)) * 100
# Nothing to change to NA

# glyburide
# Counting frequency of values
table(main_train$glyburide)
# Count frequency of values as percentages
prop.table(table(main_train$glyburide)) * 100
# Nothing to change to NA

# tolbutamide
# Counting frequency of values
table(main_train$tolbutamide)
# Count frequency of values as percentages
prop.table(table(main_train$tolbutamide)) * 100
# Nothing to change to NA

# pioglitazone
# Counting frequency of values
table(main_train$pioglitazone)
# Count frequency of values as percentages
prop.table(table(main_train$pioglitazone)) * 100
# Nothing to change to NA

# rosiglitazone
# Counting frequency of values
table(main_train$rosiglitazone)
# Count frequency of values as percentages
prop.table(table(main_train$rosiglitazone)) * 100
# Nothing to change to NA

# acarbose
# Counting frequency of values
table(main_train$acarbose)
# Count frequency of values as percentages
prop.table(table(main_train$acarbose)) * 100
# Nothing to change to NA

# miglitol
# Counting frequency of values
table(main_train$miglitol)
# Count frequency of values as percentages
prop.table(table(main_train$miglitol)) * 100
# Nothing to change to NA

# troglitazone
# Counting frequency of values
table(main_train$troglitazone)
# Count frequency of values as percentages
prop.table(table(main_train$troglitazone)) * 100
# Nothing to change to NA

# tolazamide
# Counting frequency of values
table(main_train$tolazamide)
# Count frequency of values as percentages
prop.table(table(main_train$tolazamide)) * 100
# Nothing to change to NA

# examide
# Counting frequency of values
table(main_train$examide)
# Count frequency of values as percentages
prop.table(table(main_train$examide)) * 100
# Nothing to change to NA

# citoglipton
# Counting frequency of values
table(main_train$citoglipton)
# Count frequency of values as percentages
prop.table(table(main_train$citoglipton)) * 100
# Nothing to change to NA

# insulin
# Counting frequency of values
table(main_train$insulin)
# Count frequency of values as percentages
prop.table(table(main_train$insulin)) * 100
# Nothing to change to NA

# change
# Counting frequency of values
table(main_train$change)
# Count frequency of values as percentages
prop.table(table(main_train$change)) * 100
# Nothing to change to NA

# diabetesMed
# Counting frequency of values
table(main_train$diabetesMed)
# Count frequency of values as percentages
prop.table(table(main_train$diabetesMed)) * 100
# Nothing to change to NA

# admission_type_id 
# Counting frequency of values
table(main_train$admission_type_id)
# Count frequency of values as percentages
prop.table(table(main_train$admission_type_id)) * 100
# Change "Not Available" and "Not Mapped" to NA
main_train$admission_type_id <- na_if(main_train$admission_type_id,"Not Available")
main_train$admission_type_id <- na_if(main_train$admission_type_id,"Not Mapped")

# discharge_disposition_id
# Counting frequency of values
table(main_train$discharge_disposition_id)
# Count frequency of values as percentages
prop.table(table(main_train$discharge_disposition_id)) * 100
# Change "Not Mapped" to NA
main_train$discharge_disposition_id <- na_if(main_train$discharge_disposition_id,"Not Mapped")

# admission_source_id
# Counting frequency of values
table(main_train$admission_source_id)
# Count frequency of values as percentages
prop.table(table(main_train$admission_source_id)) * 100
# Change "Not Available" and "Not Mapped" to NA
main_train$admission_source_id <- na_if(main_train$admission_source_id,"Not Available")
main_train$admission_source_id <- na_if(main_train$admission_source_id,"Not Mapped")

# medical_specialty
# Counting frequency of values
table(main_train$medical_specialty)
# Count frequency of values as percentages
prop.table(table(main_train$medical_specialty)) * 100
# Change "?" to NA
main_train$medical_specialty <- na_if(main_train$medical_specialty,"?")

# From Meds dataset
# Asked ChatGPT to group these medicines:
# Biguanides: metformin
# Meglitinides: repaglinide, nateglinide
# Sulfonylureas: chlorpropamide, glimepiride, glipizide, glyburide, tolazamide
# Thiazolidinediones: pioglitazone, rosiglitazone
# Alpha-glucosidase inhibitors: acarbose, miglitol
# Insulin: insulin
# Other/Unknown/Not in Market: examide, citoglipton, troglitazone, acetohexamide, tolbutamide
## acetohexamide and tolbutamide - not in market; risky, older medicine
## troglitazone - not in market; high risk for serious liver toxicity
## examide and citoglipton - not a medication used for diabetes treatment

ans <- unique(main_train$metformin)
takingMeds <- setdiff(ans, "No")

# Make categorized medicine
main_train$Meglitinides <- ifelse(main_train$repaglinide %in% takingMeds,1,
                                  ifelse(main_train$nateglinide %in% takingMeds,1,0))
main_train$Sulfonylureas <- ifelse(main_train$chlorpropamide %in% takingMeds,1,
                                   ifelse(main_train$glimepiride %in% takingMeds,1,
                                          ifelse(main_train$glipizide %in% takingMeds,1,
                                                 ifelse(main_train$glyburide %in% takingMeds,1,
                                                        ifelse(main_train$tolazamide %in% takingMeds,1,0)))))
main_train$Thiazolidinediones <- ifelse(main_train$pioglitazone %in% takingMeds,1,
                                        ifelse(main_train$rosiglitazone %in% takingMeds,1,0))
main_train$Alphaglucosidase <- ifelse(main_train$acarbose %in% takingMeds,1,
                                      ifelse(main_train$miglitol %in% takingMeds,1,0))
main_train$RiskyMeds <- ifelse(main_train$acetohexamide %in% takingMeds,1,
                               ifelse(main_train$tolbutamide %in% takingMeds,1,
                                      ifelse(main_train$troglitazone %in% takingMeds,1,0)))
main_train$NotDiabMeds <- ifelse(main_train$examide %in% takingMeds,1,
                                 ifelse(main_train$citoglipton %in% takingMeds,1,0))

# Drop those medicines, except metformin and insulin (commonly used meds for diabetes)
main_train <- subset(main_train, select = -c(repaglinide, nateglinide, chlorpropamide, glimepiride, acetohexamide, glipizide, glyburide, tolbutamide, pioglitazone, rosiglitazone, acarbose, miglitol, troglitazone, tolazamide, examide, citoglipton))

main_train$metformin <- ifelse(main_train$metformin %in% takingMeds,1,0)
main_train$insulin <- ifelse(main_train$insulin %in% takingMeds,1,0)

# For admission_type_id, group those that have been transferred; referrals
main_train$admission_source_id0 <- main_train$admission_source_id
main_train$admission_source_id0[grepl("^Transfer", main_train$admission_source_id) == T] <- "Transfer"
main_train$admission_source_id0[grepl("Referral$", main_train$admission_source_id) == T] <- "Referral"
main_train$admission_source_id0[grepl("^Court", main_train$admission_source_id) == T]    <- "Law Enforcement"
main_train$admission_source_id <- main_train$admission_source_id0

# For discharge_disposition_id, group those that have been transferred; referrals
main_train$discharge_disposition_id0 <- main_train$discharge_disposition_id
main_train$discharge_disposition_id0[main_train$discharge_disposition_id =="Discharged to home"] <- "Discharged"
main_train$discharge_disposition_id0[grepl("^Discharged/transferred", main_train$discharge_disposition_id) == T] <- "Transferred"
main_train$discharge_disposition_id0[grepl("^Hospice", main_train$discharge_disposition_id) == T] <- "Hospice"
main_train$discharge_disposition_id0[grepl("^Admitted", main_train$discharge_disposition_id) == T] <- "Admitted"
main_train$discharge_disposition_id <- main_train$discharge_disposition_id0

# Drop the dummy variables made
main_train <- subset(main_train, select = -c(admission_source_id0, discharge_disposition_id0))



##############################
### EXPLORE ###
##############################

## Making EDA graphs ##

# Age by Gender
mean_age <- aggregate(main_train$age, by = list(main_train$gender), FUN = mean) #aggregate
f_ave    <- round(mean_age[mean_age$Group.1 == "Female", "x"],0)
m_ave    <- round(mean_age[mean_age$Group.1 == "Male", "x"],0)
f_pct    <- round(mean(main_train$gender == "Female") * 100,0)
m_pct    <- round(mean(main_train$gender == "Male") * 100,0)

ggplot(main_train, aes(x=age, col=gender))+
  geom_density()+
  theme_minimal()+
  geom_text(aes(x = 20,
                y = 0.02,
                label = paste0("Mean Ages\n   Female: ", f_ave, " (", f_pct, "%) \n   Male: ", m_ave, " (", m_pct, "%)")),
            color='grey50',
            hjust = 0,
            vjust = -0.5)+
  labs(x = "Age",
       y = "Density")

ggsave("EDA- Age_Gender.jpg", width = 20, height = 20, units = "cm")
dev.off()


# Time in hospital per patient x Readmission
ave_hostime <- main_train %>% group_by(readmitted_y) %>% summarize(ave_time = mean(time_in_hospital)) #group_by
y_hostime   <- round(ave_hostime$ave_time[ave_hostime$readmitted_y == T],1)
n_hostime   <- round(ave_hostime$ave_time[ave_hostime$readmitted_y == F],1)

ggplot(main_train, aes(x=time_in_hospital, col=readmitted_y))+
  geom_density()+
  theme_minimal()+
  labs(x = "Time in Hospital",
       y = "Density",
       color = "Readmitted")+
  geom_text(aes(x = 5,
                y = 0.13,
                label = paste0("Mean Time Spent in Hospital\n   Readmitted: ", y_hostime, " days\n   Not Readmitted: ", n_hostime," days")),
            color='grey50',
            hjust = 0,
            vjust = 0)

ggsave("EDA- Time_Status.jpg", width = 20, height = 20, units = "cm")
dev.off()


# Correlation of the numerical variables and the readmission status
# Creating DF with only numerical variables for a correlation graph
corrdf <- main_train
corrdf$a_readmitted <- as.numeric(corrdf$readmitted_y) # Converting readmitted_y to binary
num_vars <- sapply(corrdf, is.numeric) # Getting numeric tag
corr_data <- corrdf[, num_vars] # Keeping only numeric variable
corr_data <- subset(corr_data, select = -c(tmpID,NotDiabMeds)) # Removing tmpID and NotDiabMeds in correlation data

# Checking for correlation using spearman method since data has ordinal values
corr_plot = cor(corr_data, method = c("spearman"))
round(corr_plot,3)

# Plotting the correlation matrix
jpeg(file="EDA- Correlation.jpeg", height=20, width=20, units = "cm", res = 300, quality=90, type = "cairo")
corrplot(corr_plot, type = "lower", order = "alphabet", 
         tl.col = "black", tl.srt = 45, method = 'color')
dev.off()

## Doing basic EDA; summary statistics, checking data types and missing values
summary(main_train)
str(main_train)

# Checking for missing data - # per variable
colSums(is.na(main_train))

# Checking for missing data - % per variable
colMeans(is.na(main_train)) * 100

## Visualization for missing data
gg_miss_var(main_train, show_pct = TRUE)+ 
  labs(y = "Missing Values (%)")
ggsave("EDA - Missing1.jpg", width = 20, height = 20, units = "cm")
dev.off()

gg_miss_fct(main_train, readmitted_y)+
  labs(title = "Missing Values (%) in the Dataset",
       x = "Readmitted",
       y = "Factors")
ggsave("EDA - Missing2.jpg", width = 35, height = 20, units = "cm")
dev.off()


## Cleaning data ##
# Checking unique value per character variable
char_df <- main_train %>% select_if(is.character)
unique_counts <- sapply(char_df, function(x) length(unique(x)))
unique_counts

## 1: Removing observations with discharge_disposition_id = Expired
# Excluding these since expired==dead
# The analyst found no need to include these patients in the modeling process
traindf <- main_train %>% filter(!(discharge_disposition_id %in% c("Expired")))

## 2 Dropping unnecessary variables; using the following criteria (either/or)
## Criteria 1: With more than 100 distinct character values
traindf <- subset(traindf, select = -c(diag_1_desc, diag_2_desc, diag_3_desc))

## Criteria 2: With more than 30% missing values
# Saving percentage of missing value per variable in a data frame
missingval <- colMeans(is.na(traindf))
# Keeping variables with less than 30% missing values
traindf <- traindf[, missingval < 0.3]
names(traindf)
colMeans(is.na(traindf)) * 100



##############################
### MODIFY ###
##############################

## Imputing missing data ##

# Getting the variables with missing values
vars_miss <- colnames(traindf)[sapply(traindf, function(x) any(is.na(x)))]

# Getting data types of variables with missing values
var_dt <- sapply(traindf[vars_miss], class)

# Creating a list of variables with missing values and their data types
vars_missing <- list(variable = vars_miss, data_type = var_dt)
vars_missing

# For Numerical Variables, we will use "Median Imputation", since median imputation is considered to be more robust and less sensitive to extreme values compared to mean imputation.
# For Character Variables, we will use "Mode Imputation", since these variables do not have mean/median
traindf$race[is.na(traindf$race)]                                         <- names(which.max(table(traindf$race)))
traindf$admission_type_id[is.na(traindf$admission_type_id)]               <- names(which.max(table(traindf$admission_type_id)))
traindf$admission_source_id[is.na(traindf$admission_source_id)]           <- names(which.max(table(traindf$admission_source_id)))
traindf$discharge_disposition_id[is.na(traindf$discharge_disposition_id)] <- names(which.max(table(traindf$discharge_disposition_id)))


## Making treatment plan using designTreatmentsC of the vtreat package ##
# Getting the column names of our data frame
names(traindf)

# Treatment
targetVariable   <- names(subset(traindf, select = c(readmitted_y)))
featureVariables <- names(subset(traindf, select = -c(tmpID,readmitted_y))) #excluding the tmpID and the target variable

# Pre-processing & Automated Engineering; id & constant variable removal
# Designing a Categorical Variable Plan
varTypes = c('clean', 'isBAD', 'catB', 'poolC')
plan <- designTreatmentsC(traindf,
                          featureVariables,
                          targetVariable,
                          outcometarget= T,
                          codeRestriction = varTypes,
                          verbose=F)

# Partitioning 20% of the main train dataset to validation dataset to avoid over-fitting
splitPercent   <- round(nrow(traindf) %*% 0.8)
idx            <- sample(1:nrow(traindf), splitPercent)
trainSet       <- traindf[idx, ]
validationSet  <- traindf[-idx, ]

dim(trainSet)
dim(validationSet)


# Applying treatment plan to both trainSet and validationSet
treatedTrain      <- prepare(plan, trainSet)
treatedValidation <- prepare(plan, validationSet)



##############################
###    MODEL  &  ASSESS    ###
##############################

##############################################################################################################
### FUNCTION CREATION FOR ASSESSMENT -- START ###

# Making a function for Assessment
# Since prediction problem is binary, it is more appropriate to use evaluation metrics like accuracy, precision, recall, f1-score, roc instead of MAPE and RMSE
assessment <- function(results,confMat,acc,rcl,spc,prc,fsc,kpa,au1,mod) {
  # Making a confusion matrix
  output <- confusionMatrix(as.factor(results$classes), as.factor(results$actual)) #pred,actual
  CM  <- output$table
  # Getting evaluation methods
  ac1 <- output$overall["Accuracy"]
  rc1 <- output$byClass["Sensitivity"]
  sp1 <- output$byClass["Specificity"]
  pr1 <- output$byClass["Pos Pred Value"]
  fs1 <- output$byClass["F1"]
  kp1 <- output$overall["Kappa"]
  ac2 <- auc(roc(results$classes, results$actual*1))
  
  # Creating a list of the assessments
  m0d <- unname(c(ac1,rc1,sp1,pr1,fs1,kp1,ac2))
  
  # Saving the variables in the global environment
  assign(confMat, CM, envir = .GlobalEnv)
  assign(acc, ac1, envir = .GlobalEnv)
  assign(rcl, rc1, envir = .GlobalEnv)
  assign(spc, sp1, envir = .GlobalEnv)
  assign(prc, pr1, envir = .GlobalEnv)
  assign(fsc, fs1, envir = .GlobalEnv)
  assign(kpa, kp1, envir = .GlobalEnv)
  assign(au1, ac2, envir = .GlobalEnv)
  assign(mod, m0d, envir = .GlobalEnv)
}

### FUNCTION CREATION FOR ASSESSMENT -- END ###
################################################################################


### MODEL 1: Logistic Regression ###
## Since the target variable is binomial, it's more appropriate to use logit regression than the normal logistic regression
cutoff <- 0.5

# 1.1 Parsimonious Logit Regression #
basic_glm <- glm(readmitted_y~.,
                 data=treatedTrain,
                 family=binomial
                )

## Checking regression result
summary(basic_glm)

# Reducing multicollinearity
# By keeping only informative variables (p-value<0.1)
pVals <- data.frame(varNames = names(na.omit(coef(basic_glm))),
                    pValues  = summary(basic_glm)$coefficients[,4])

# Determine which variable names to keep 
keepVar <- subset(pVals$varNames, pVals$pValues<0.1)

# Keeping informative variables
treatedTrainPars <- treatedTrain[,names(treatedTrain) %in% keepVar]

# Appending the target variable
treatedTrainPars$readmitted_y <- treatedTrain$readmitted_y

# Refitting the logit model
model1_glm <- glm(as.factor(readmitted_y) ~ . ,
                  data=treatedTrainPars,
                  family=binomial
                 )

summary(model1_glm)

# Making predictions
pred_glm1t   <- predict(model1_glm,treatedTrainPars,type='response')
class_glm1t  <- ifelse(pred_glm1t>=cutoff,1,0)
result_glm1t <- data.frame(tmpID   = trainSet$tmpID,
                           actual  = treatedTrainPars$readmitted_y,
                           classes = as.logical(class_glm1t),
                           probs   = pred_glm1t,
                           result  = ifelse(treatedTrainPars$readmitted_y==as.logical(class_glm1t),"Correct","Incorrect")
                          )

head(result_glm1t)

# Evaluating
assessment(result_glm1t,"conf_glm1t","acc_glm1t","rcl_glm1t","spc_glm1t","prc_glm1t","fsc_glm1t","kpa_glm1t","auc_glm1t","model1t")

# Graphing ROC Curve
jpeg(file="Plot - ROC M1.jpeg", height=20, width=20, units = "cm", res = 300, quality=90, type = "cairo")
plot(roc(result_glm1t$classes, result_glm1t$actual*1), 
     print.auc = TRUE, 
     main = "ROC Curve for Parsimonious Logit Model",
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     col = "steelblue",
     lty = 2
    )
text(0.275, 0.38, 
     paste0("Accuracy: ", round(acc_glm1t, 3)),
     col = "steelblue"
    )
dev.off()



# 1.2 K-fold Cross-Validation Model: K=10
# Using the parsimonious variables
model2_glm <- train(as.factor(readmitted_y) ~ .,
                    data = treatedTrainPars,
                    method = "glm", 
                    trControl = trainControl(method = "cv", number = 10) # K=10
                   )

# Checking model output
summary(model2_glm)


# Making predictions
pred_glm2t   <- predict(model2_glm,treatedTrainPars)
result_glm2t <- data.frame(tmpID   = trainSet$tmpID,
                           actual  = treatedTrainPars$readmitted_y,
                           classes = pred_glm2t,
                           probs   = apply(predict(model2_glm,treatedTrainPars,type="prob"), 1, max),
                           result  = ifelse(treatedTrainPars$readmitted_y==pred_glm2t,"Correct","Incorrect")
                          )

head(result_glm2t)

# Evaluating
assessment(result_glm2t,"conf_glm2t","acc_glm2t","rcl_glm2t","spc_glm2t","prc_glm2t","fsc_glm2t","kpa_glm2t","auc_glm2t","model2t")

# Graphing ROC Curve
jpeg(file="Plot - ROC M2.jpeg", height=20, width=20, units = "cm", res = 300, quality=90, type = "cairo")
plot(roc(result_glm2t$classes, result_glm2t$actual*1),
     print.auc = TRUE, 
     main = "ROC Curve for 10-fold Logit Model",
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     col = "steelblue",
     lty = 2
    )
text(0.275, 0.38, 
     paste0("Accuracy: ", round(acc_glm2t, 3)),
     col = "steelblue"
    )
dev.off()



### MODEL 2: Decision Tree ###
# Decision Tree

# Making a decision tree with various cp
model3_tree <- train(as.factor(readmitted_y) ~.,
                     data = treatedTrain,
                     method = "rpart", 
                     tuneGrid = data.frame(cp = c(0.0001, 0.001, 0.0025, 0.005, 0.006, 0.0075, 0.008, 0.009, 0.01, 0.02, 0.025, 0.03, 0.05, 0.075, 0.1, 0.2, 0.25, 0.5)),
                     control = rpart.control(minsplit = 10, minbucket = 10)
                    )

# Result of decision tree
model3_tree

# Getting the cp with max accuracy
results0 <- model3_tree$results[, 1:2]
bestCP   <- results0[which.max(results0$Accuracy),1]
bestCP

xmin0 <- results0[which.max(results0$Accuracy),1]
ymin0 <- results0[which.max(results0$Accuracy),2]

# Plotting CP-Accuracy Relationship to adjust the tuneGrid inputs
ggplot(model3_tree)+
  theme_minimal()+
  geom_text(aes(x = xmin0,
                y = ymin0,
                label = paste0("Best CP: ", bestCP)),
            color='steelblue',
            hjust = 0,
            vjust = -0.5
            )
ggsave("Plot - CP_Acc.jpg", width = 20, height = 20, units = "cm")
dev.off()

# Plotting the tree
prp(model3_tree$finalModel, extra=101, col=2)

# Looking at improved variable importance
# Making a dataframe with var importance
varImpDF          <- as.data.frame(varImp(model3_tree$finalModel))  # Making a var importance df from final dec tree model
varImpDF$Variable <- rownames(varImpDF)  # Making the index a column
varImpDF          <- data.frame(varImpDF, row.names = NULL)  # Removing index
varImpDF          <- varImpDF[order(varImpDF$Overall, decreasing = T),] # Sorting df by decreasing importance

varPlot_dt        <- head(varImpDF,15) # Filter to first 15 variables so graph would not feel cramped

# Plotting the var importance df
ggplot(varPlot_dt, aes(x=Overall, y = reorder(Variable, Overall))) + 
  geom_bar(stat='identity', position = 'dodge', fill ="steelblue") +
  theme_gdocs() +
  theme_minimal() +
  labs(x = "Importance", y = "Variables")
ggsave("Plot - VarImp DT.jpg", width = 20, height = 20, units = "cm")
dev.off()


# Making predictions
pred_tree1t   <- predict(model3_tree,treatedTrain)
result_tree1t <- data.frame(tmpID   = trainSet$tmpID,
                            actual  = treatedTrain$readmitted_y,
                            classes = pred_tree1t,
                            probs   = apply(predict(model3_tree,treatedTrain,type="prob"), 1, max),
                            result  = ifelse(treatedTrain$readmitted_y==pred_tree1t,"Correct","Incorrect")
                           )

head(result_tree1t,15)

# Evaluating
assessment(result_tree1t,"conf_tree1t","acc_tree1t","rcl_tree1t","spc_tree1t","prc_tree1t","fsc_tree1t","kpa_tree1t","auc_tree1t","model3t")

# Graphing ROC Curve
jpeg(file="Plot - ROC M3.jpeg", height=20, width=20, units = "cm", res = 300, quality=90, type = "cairo")
plot(roc(result_tree1t$classes, result_tree1t$actual*1), 
     print.auc = TRUE, 
     main = "ROC Curve for Decision Tree",
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     col = "steelblue",
     lty = 2
    )
text(0.275, 0.38, 
     paste0("Accuracy: ", round(acc_tree1t, 3)),
     col = "steelblue"
    )
dev.off()




### MODEL 3: Random Forest ###
# Fitting a random forest model using Caret

# Defining the hyper-parameter grids
grid <- expand.grid(.mtry = c(1,2,3,4),
                    .splitrule = 'extratrees',
                    .min.node.size = c(1,2,5,10)
                   )

fitControl <- trainControl(method = "CV",
                           number = 5,
                           classProbs = T,
                           verboseIter = T
                          )

# Making a function to search for best number of trees in Random Forest
numTreesVec <- vector()
oobError  <- vector()
nTreeSearch <- seq(from = 100, to = 500, by=5)

# Converting class levels to valid R variable names
treatedTrain$readmitted_y <- factor(treatedTrain$readmitted_y)
levels(treatedTrain$readmitted_y) <- make.names(levels(treatedTrain$readmitted_y))

for(i in 1:length(nTreeSearch)){
  print(i)
  fit <- train(readmitted_y~.,
               data = treatedTrain,
               method = 'ranger',
               num.trees = nTreeSearch[i],
               tuneGrid = grid,
               trControl = fitControl
              )
  numTreesVec[i] <- fit$finalModel$num.trees
  oobError[i] <- fit$finalModel$prediction.error
}

results1 <- data.frame(Num_Trees = numTreesVec,
                       Predict_Error = oobError)

# Getting the tree with minimum error
numtree <- results1[which.min(results1$Predict_Error),1]
hjust1  <- ifelse(numtree<200, 0, ifelse(numtree>400, 1,0.5)) # for graph

# Plotting the # of trees and their corresponding error
xmin <- results1[which.min(results1$Predict_Error),1]
ymin <- results1[which.min(results1$Predict_Error),2]
ggplot(results1, aes(x=Num_Trees,y=Predict_Error)) + 
  geom_line(alpha =0.25, color = 'red') +
  theme_gdocs()+
  geom_smooth(method = "loess")+
  geom_text(aes(x = xmin,
                y = ymin,
                label = paste0("Best Tree: ", numtree)),
            color='steelblue',
            hjust = hjust1,
            vjust = 0
            )
ggsave("Plot - Tree_Error.jpg", width = 20, height = 20, units = "cm")
dev.off()

# Making a Random Forest ranger model using the best # of tree
model4_rf <- train(readmitted_y ~ .,
                   data = treatedTrain,
                   method = 'ranger',
                   num.trees = numtree,
                   tuneGrid = grid,
                   importance = "permutation",
                   trControl = fitControl
                  )

# Looking at variable importance
# Making a dataframe with var importance
varImpDF1              <- as.data.frame(model4_rf$finalModel$variable.importance)  # Making a var importance df from final rf model
colnames(varImpDF1)[1] <- "Importance"
varImpDF1$Variable     <- rownames(varImpDF1)  # Making the index a column
varImpDF1              <- data.frame(varImpDF1, row.names = NULL)  # Removing index
varImpDF1              <- varImpDF1[order(varImpDF1$Importance, decreasing = T),] # Sorting df by decreasing importance

varPlot_rf             <- head(varImpDF1,15) # Filter to first 15 variables so graph would not feel cramped

# Visualizing variable importance
ggplot(varPlot_rf, aes(x=Importance, y = reorder(Variable, Importance))) + 
  geom_bar(stat='identity', position = 'dodge', fill ="steelblue") +
  theme_gdocs() +
  theme_minimal() +
  labs(x = "Importance", y = "Variables")
ggsave("Plot - VarImp Rf.jpg", width = 20, height = 20, units = "cm")
dev.off()


# Making predictions
pred_rft    <- predict(model4_rf,treatedTrain,type="prob")
result_rft  <- data.frame(tmpID   = trainSet$tmpID,
                          actual  = treatedTrain$readmitted_y,
                          classes = predict(model4_rf,treatedTrain),
                          probs   = apply(pred_rft, 1, max),
                          result  = ifelse(treatedTrain$readmitted_y==predict(model4_rf,treatedTrain) ,"Correct","Incorrect")
                          )

result2_rft  <- data.frame(tmpID   = trainSet$tmpID,
                           actual  = as.numeric(treatedTrain$readmitted_y),
                           classes = as.numeric(predict(model4_rf,treatedTrain)),
                           probs   = apply(pred_rft, 1, max),
                           result  = ifelse(treatedTrain$readmitted_y==predict(model4_rf,treatedTrain) ,"Correct","Incorrect")
                          )

head(result2_rft ,15)

# Evaluating
assessment(result2_rft,"conf_rft1","acc_rft1","rcl_rft1","spc_rft1","prc_rft1","fsc_rft1","kpa_rft1","auc_rft1","model4t")

# Graphing ROC Curve
jpeg(file="Plot - ROC M4.jpeg", height=20, width=20, units = "cm", res = 300, quality=90, type = "cairo")
plot(roc(result2_rft$classes, result2_rft$actual*1), 
     print.auc = TRUE, 
     main = "ROC Curve for Random Forest",
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     col = "steelblue",
     lty = 2
    )
text(0.275, 0.38, 
     paste0("Accuracy: ", round(acc_rft1, 3)),
     col = "steelblue"
    )
dev.off()


### Comparing Models ###

# Creating a table of the evaluation metrics - using train dataset
evaluation_train <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1-Score", "Cohen's Kappa", "AUC"),
  Parsimonious_Log_Model = model1t,
  Log_Model_KFold = model2t,
  Decision_Tree = model3t,
  Random_Forest = model4t
)
evaluation_train

################################################################################

### Checking the models using the validation dataset ###
# 1.1 Parsimonious Logit Regression #
# Keeping informative variables
treatedValidPars <- treatedValidation[,names(treatedValidation) %in% keepVar]

# Appending the target variable
treatedValidPars$readmitted_y <- treatedValidation$readmitted_y

# Making predictions using the train model and validation dataset 
pred_glm1v   <- predict(model1_glm,treatedValidPars)
class_glm1v  <- ifelse(pred_glm1v>=cutoff,1,0)
result_glm1v <- data.frame(tmpID   = validationSet$tmpID,
                           actual  = treatedValidPars$readmitted_y,
                           classes = as.logical(class_glm1v),
                           probs   = pred_glm1v,
                           result  = ifelse(treatedValidPars$readmitted_y==as.logical(class_glm1v),"Correct","Incorrect")
)

head(result_glm1v)

# Evaluating
assessment(result_glm1v,"conf_glm1v","acc_glm1v","rcl_glm1v","spc_glm1v","prc_glm1v","fsc_glm1v","kpa_glm1v","auc_glm1v","model1v")


# 1.2 K-fold Cross-Validation Model: K=10
# Using the parsimonious variables
# Making predictions using the train model and validation dataset 
pred_glm2v   <- predict(model2_glm,treatedValidPars)
result_glm2v <- data.frame(tmpID  = validationSet$tmpID,
                           actual  = treatedValidPars$readmitted_y,
                           classes = pred_glm2v,
                           probs   = apply(predict(model2_glm,treatedValidPars,type="prob"), 1, max),
                           result  = ifelse(treatedValidPars$readmitted_y==pred_glm2v,"Correct","Incorrect")
)

head(result_glm2v)
# Evaluating
assessment(result_glm2v,"conf_glm2v ","acc_glm2v ","rcl_glm2v ","spc_glm2v ","prc_glm2v ","fsc_glm2v ","kpa_glm2v ","auc_glm2v ","model2v")



### MODEL 2: Decision Tree ###
# Decision Tree
# Making another decision tree for validation; since there might be some cases where some variables have new levels
# i.e. categorical variable admission_source_id == Law Enforcer only has 1 row in the entire dataset
# it might be split in validationSet instead of trainSet

# Making a decision tree with various cp
model3_treev <- train(as.factor(readmitted_y) ~.,
                      data = treatedValidation,
                      method = "rpart", 
                      tuneGrid = data.frame(cp = c(0.0001, 0.001, 0.0025, 0.005, 0.006, 0.0075, 0.008, 0.009, 0.01, 0.02, 0.025, 0.03, 0.05, 0.075, 0.1, 0.2, 0.25, 0.5)),
                      control = rpart.control(minsplit = 10, minbucket = 10)
                     )

# Result of decision tree
model3_treev

# Plotting CP-Accuracy Relationship to adjust the tuneGrid inputs
plot(model3_treev)

# Plotting the tree
prp(model3_treev$finalModel, extra=101, col=2)

# Looking at improved variable importance
# Making a dataframe with var importance
varImpDF2          <- as.data.frame(varImp(model3_treev$finalModel))  # Making a var importance df from final dec tree model
varImpDF2$Variable <- rownames(varImpDF2)  # Making the index a column
varImpDF2          <- data.frame(varImpDF2, row.names = NULL)  # Removing index
varImpDF2          <- varImpDF2[order(varImpDF2$Overall, decreasing = T),] # Sorting df by decreasing importance

varPlot_dt2        <- head(varImpDF2,15) # Filter to first 15 variables so graph would not feel cramped

# Plotting the var importance df
ggplot(varPlot_dt2, aes(x=Overall, y = reorder(Variable, Overall))) + 
  geom_bar(stat='identity', position = 'dodge', fill ="steelblue") +
  theme_gdocs() +
  theme_minimal() +
  labs(x = "Importance", y = "Variables")

# Making predictions using the train model and validation dataset 
pred_tree1v    <- predict(model3_treev,treatedValidation)
result_tree1v  <- data.frame(tmpID   = validationSet$tmpID,
                             actual  = treatedValidation$readmitted_y,
                             classes = pred_tree1v ,
                             probs   = apply(predict(model3_tree,treatedValidation,type="prob"), 1, max),
                             result  = ifelse(treatedValidation$readmitted_y==pred_tree1v ,"Correct","Incorrect")
                            )

head(result_tree1v,15)

# Evaluating
assessment(result_tree1v ,"conf_tree1v","acc_tree1v","rcl_tree1v","spc_tree1v","prc_tree1v","fsc_tree1v","kpa_tree1v","auc_tree1v","model3v")


### MODEL 3: Random Forest ###
# Fitting a random forest model using Caret

# Converting class levels to valid R variable names
treatedValidation$readmitted_y <- factor(treatedValidation$readmitted_y)
levels(treatedValidation$readmitted_y) <- make.names(levels(treatedValidation$readmitted_y))

# Making predictions using the train model and validation dataset 
pred_rfv   <- predict(model4_rf,treatedValidation,type="prob")
result_rfv  <- data.frame(tmpID   = validationSet$tmpID,
                          actual  = treatedValidation$readmitted_y,
                          classes = predict(model4_rf,treatedValidation),
                          probs   = apply(pred_rfv, 1, max),
                          result  = ifelse(treatedValidation$readmitted_y==predict(model4_rf,treatedValidation),"Correct","Incorrect")
                         )

result2_rfv  <- data.frame(tmpID   = validationSet$tmpID,
                           actual  = as.numeric(treatedValidation$readmitted_y),
                           classes = as.numeric(predict(model4_rf,treatedValidation)),
                           probs   = apply(pred_rfv, 1, max),
                           result  = ifelse(treatedValidation$readmitted_y==predict(model4_rf,treatedValidation) ,"Correct","Incorrect")
                           )

head(result_rfv,15)

# Evaluating
assessment(result2_rfv,"conf_rfv1","acc_rfv1","rcl_rfv1","spc_rfv1","prc_rfv1","fsc_rfv1","kpa_rfv1","auc_rfv1","model4v")




### Comparing Models ###

# Creating a table of the evaluation metrics - using validation dataset
evaluation_validation <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision", "F1-Score", "Cohen's Kappa", "AUC"),
  Parsimonious_Log_Model_v = model1v,
  Log_Model_KFold_v = model2v,
  Decision_Tree_v = model3v,
  Random_Forest_v = model4v
)

# Checking Side-by-Side to see which model is the best (in terms of accuracy, auc, etc)
# and the most stable (same performance for train and validation)
eval_output <- merge(evaluation_train, evaluation_validation, by = "Metric")
eval_output

# Exporting evaluations to csv
write.csv(eval_output, "Evaluation_Output.csv", row.names = F)

# Making graph comparison for accuracy, f1_score, and auc
etrain <- subset(evaluation_train, Metric %in% c("Accuracy","F1-Score", "AUC"))
evalid <- subset(evaluation_validation, Metric %in% c("Accuracy","F1-Score", "AUC"))
eval   <- merge(etrain, evalid, by = "Metric")

g_acc  <- data.frame(Model      = names(eval[1,2:5]),
                     Accuracy   = as.vector(round(t(eval[1,2:9]),3)),
                     Dataset    = "Train")
g_acc$Dataset[5:8] <- "Validation"
g_acc

ggplot(g_acc, aes(x = Model, y = Accuracy, fill = Dataset)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_label(aes(label = Accuracy), color = "white", 
             position=position_dodge(width=0.9), vjust = 0.3,
             show.legend = FALSE)+
  labs(title = "Model Accuracy Comparison", x = "Model", y = "Accuracy")

ggsave("Eval - Accuracy.jpg", width = 20, height = 12, units = "cm")
dev.off()


g_auc  <- data.frame(Model   = names(eval[1,2:5]),
                     AUC     = as.vector(round(t(eval[2,2:9]),3)),
                     Dataset = "Train")
g_auc$Dataset[5:8] <- "Validation"
g_auc

ggplot(g_auc, aes(x = Model, y = AUC, fill = Dataset)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_label(aes(label = AUC), color = "white", 
             position=position_dodge(width=0.9), vjust = 0.3,
             show.legend = FALSE)+
  labs(title = "Model AUC Comparison", x = "Model", y = "AUC")

ggsave("Eval - AUC.jpg", width = 20, height = 12, units = "cm")
dev.off()


g_fsc  <- data.frame(Model      = names(eval[1,2:5]),
                     F1_Score   = as.vector(round(t(eval[3,2:9]),3)),
                     Dataset    = "Train")
g_fsc$Dataset[5:8] <- "Validation"
g_fsc

ggplot(g_fsc, aes(x = Model, y = F1_Score, fill = Dataset)) +
  geom_bar(position = "dodge", stat = "identity") + 
  geom_label(aes(label = F1_Score), color = "white", 
             position=position_dodge(width=0.9), vjust = 0.3,
             show.legend = FALSE)+
  labs(title = "Model F1-Score Comparison", x = "Model", y = "F1 Score")

ggsave("Eval - F1_Score.jpg", width = 20, height = 12, units = "cm")
dev.off()



################################################################################
#          FINAL MODEL CHOSEN BASED ON THE ASSESSMENT : RANDOM FOREST          #
################################################################################

# Now for implementation in the test dataset #
# Test data #

### Loading test datasets ###
# Including changing blanks to NA
pat_test <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesPatientTest.csv', na.strings = "")
med_test <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesMedsTest.csv', na.strings = "")
hos_test <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A2_Hospital_Readmission/caseData/diabetesHospitalInfoTest.csv', na.strings = "")

## Joining all the datasets into one main test dataset
# List of the three datasets
df_list <- list(pat_test, med_test, hos_test)

# Merging all three datasets by tmpID
main_test <- data.frame(power_left_join(df_list, by = "tmpID"))

### Cleaning the data ###

## Checking distinct values per character variable - to check if needs to be changed to NA ##
# Race
# Change "?" to NA
main_test $race <- na_if(main_test $race,"?")

# Payer Code
# Change "?" to NA
main_test $payer_code <- na_if(main_test $payer_code,"?")

# admission_type_id 
# Change "Not Available" and "Not Mapped" to NA
main_test $admission_type_id <- na_if(main_test $admission_type_id,"Not Available")
main_test $admission_type_id <- na_if(main_test $admission_type_id,"Not Mapped")

# discharge_disposition_id
# Change "Not Mapped" to NA
main_test $discharge_disposition_id <- na_if(main_test $discharge_disposition_id,"Not Mapped")

# admission_source_id
# Change "Not Available" and "Not Mapped" to NA
main_test $admission_source_id <- na_if(main_test $admission_source_id,"Not Available")
main_test $admission_source_id <- na_if(main_test $admission_source_id,"Not Mapped")

# medical_specialty
# Change "?" to NA
main_test $medical_specialty <- na_if(main_test $medical_specialty,"?")

# Make a variable to count number of meds currently taking
main_test $med_count <- (18 - (rowSums(main_test [,10:27] == "No")))

ans <- unique(main_test $metformin)
takingMeds <- setdiff(ans, "No")

# Make categorized medicine
main_test $Meglitinides <- ifelse(main_test $repaglinide %in% takingMeds,1,
                                  ifelse(main_test $nateglinide %in% takingMeds,1,0))
main_test $Sulfonylureas <- ifelse(main_test $chlorpropamide %in% takingMeds,1,
                                   ifelse(main_test $glimepiride %in% takingMeds,1,
                                          ifelse(main_test $glipizide %in% takingMeds,1,
                                                 ifelse(main_test $glyburide %in% takingMeds,1,
                                                        ifelse(main_test $tolazamide %in% takingMeds,1,0)))))
main_test $Thiazolidinediones <- ifelse(main_test $pioglitazone %in% takingMeds,1,
                                        ifelse(main_test $rosiglitazone %in% takingMeds,1,0))
main_test $Alphaglucosidase <- ifelse(main_test $acarbose %in% takingMeds,1,
                                      ifelse(main_test $miglitol %in% takingMeds,1,0))
main_test $RiskyMeds <- ifelse(main_test $acetohexamide %in% takingMeds,1,
                               ifelse(main_test $tolbutamide %in% takingMeds,1,
                                      ifelse(main_test $troglitazone %in% takingMeds,1,0)))
main_test $NotDiabMeds <- ifelse(main_test $examide %in% takingMeds,1,
                                 ifelse(main_test $citoglipton %in% takingMeds,1,0))

# Drop those medicines, except metformin and insulin (commonly used meds for diabetes)
main_test  <- subset(main_test , select = -c(repaglinide, nateglinide, chlorpropamide, glimepiride, acetohexamide, glipizide, glyburide, tolbutamide, pioglitazone, rosiglitazone, acarbose, miglitol, troglitazone, tolazamide, examide, citoglipton))

main_test $metformin <- ifelse(main_test $metformin %in% takingMeds,1,0)
main_test $insulin <- ifelse(main_test $insulin %in% takingMeds,1,0)

# For admission_type_id, group those that have been transferred; referrals
main_test $admission_source_id0 <- main_test $admission_source_id
main_test $admission_source_id0[grepl("^Transfer", main_test $admission_source_id) == T] <- "Transfer"
main_test $admission_source_id0[grepl("Referral$", main_test $admission_source_id) == T] <- "Referral"
main_test $admission_source_id0[grepl("^Court", main_test $admission_source_id) == T]    <- "Law Enforcement"
main_test $admission_source_id <- main_test $admission_source_id0

# For discharge_disposition_id, group those that have been transferred; referrals
main_test $discharge_disposition_id0 <- main_test $discharge_disposition_id
main_test $discharge_disposition_id0[main_test $discharge_disposition_id =="Discharged to home"] <- "Discharged"
main_test $discharge_disposition_id0[grepl("^Discharged/transferred", main_test $discharge_disposition_id) == T] <- "Transferred"
main_test $discharge_disposition_id0[grepl("^Hospice", main_test $discharge_disposition_id) == T] <- "Hospice"
main_test $discharge_disposition_id0[grepl("^Admitted", main_test $discharge_disposition_id) == T] <- "Admitted"
main_test $discharge_disposition_id <- main_test $discharge_disposition_id0

# Drop the dummy variables made
main_test  <- subset(main_test , select = -c(admission_source_id0, discharge_disposition_id0))


## Cleaning data ##
# Checking unique value per character variable
char_df <- main_test  %>% select_if(is.character)
unique_counts <- sapply(char_df, function(x) length(unique(x)))
unique_counts

## 1: Removing observations with discharge_disposition_id = Expired
# Excluding these since expired==dead
# The analyst found no need to include these patients in the modeling process
testdf  <- main_test  %>% filter(!(discharge_disposition_id %in% c("Expired")))

## 2 Dropping unnecessary variables; using the following criteria (either/or)
## Criteria 1: With more than 100 distinct character values
testdf  <- subset(testdf, select = -c(diag_1_desc, diag_2_desc, diag_3_desc))

## Criteria 2: With more than 30% missing values
# Saving percentage of missing value per variable in a data frame
missingval <- colMeans(is.na(testdf))
# Keeping variables with less than 30% missing values
testdf  <- testdf[, missingval < 0.3]
names(testdf)
colMeans(is.na(testdf)) * 100

## Imputing missing data ##

# Getting the variables with missing values
vars_miss <- colnames(testdf)[sapply(testdf, function(x) any(is.na(x)))]

# Getting data types of variables with missing values
var_dt <- sapply(testdf[vars_miss], class)

# Creating a list of variables with missing values and their data types
vars_missing <- list(variable = vars_miss, data_type = var_dt)
vars_missing

# For Numerical Variables, we will use "Median Imputation", since median imputation is considered to be more robust and less sensitive to extreme values compared to mean imputation.
# For Character Variables, we will use "Mode Imputation", since these variables do not have mean/median
testdf$race[is.na(testdf$race)]                                         <- names(which.max(table(testdf$race)))
testdf$admission_type_id[is.na(testdf$admission_type_id)]               <- names(which.max(table(testdf$admission_type_id)))
testdf$admission_source_id[is.na(testdf$admission_source_id)]           <- names(which.max(table(testdf$admission_source_id)))
testdf$discharge_disposition_id[is.na(testdf$discharge_disposition_id)] <- names(which.max(table(testdf$discharge_disposition_id)))


# Applying treatment plan to testSet used for both trainSet and validationSet
testSet <- prepare(plan, testdf)
names(testdf)

### Implementation of the model
## Chose model is: Random Forest ##
## This is because it has the highest accuracy, sensitivity, f1-score, and auc ##
### MODEL 3: Random Forest ### 
pred_rftest <- predict(model4_rf,testSet,type="prob")

# Getting the value true only:
pred_df     <- data.frame(tmpID           = testdf$tmpID,
                          readmitted_pred = as.numeric(pred_rftest[,"TRUE."])
                         )

# Getting the top 100 patients most likely to be readmitted
prediction_rftest <- pred_df[order(pred_df$readmitted_pred, decreasing = TRUE),]
pred_out <- head(prediction_rftest,100)
pred_out

# Exporting prediction to csv
write.csv(pred_out, "Predictions-Brandares.csv", row.names = F)

## End