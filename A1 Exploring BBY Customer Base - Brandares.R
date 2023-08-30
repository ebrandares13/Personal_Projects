### Title: A1 Direct Mail Household EDA
### Purpose: To identify the best customer segment for the BBY loyalty program and give insights to the marketing team as to which customers to target to promote brand loyalty and increase revenue.
### Author: Eunice Brandares
### Date: March 27, 2023
###

## Setting the working directory
setwd("~/Hult/MBAN/Spring Sem/(7) Visualizing & Analyzing Data with R Methods & Tools/Hult_Visualizing-Analyzing-Data-with-R/personalFiles/A1")

## Libraries
options(scipen = 999)
library(dplyr)
library(powerjoin)
library(ggplot2)
library(ggthemes)
library(Hmisc)
library(corrplot)

### Loading the in-house data as: household_DF; Changing "" (blanks) as NA ###
household <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/inHouse_EDA_10k.csv', na.strings = "")

# Selecting only necessary variables - using subset()
hh_num <- subset(household, select = c(tmpID, Age,	storeVisitFrequency, EstHomeValue, y_householdSpend))
hh_char <- subset(household, select = c(Gender, PropertyType, DwellingUnitSize))

# Did not include location parameters, as we're focusing more on online buyers

dim(hh_num)
dim(hh_char)

## Dealing with numeric data
# Changing data type to Numerical (Age, EstHomeValue)
hh_num$EstHomeValue <- as.integer(sub("\\$", "", hh_num$EstHomeValue))
hh_num$Age <- as.numeric(sub("\\$", "", hh_num$Age))

# Rounding values for numerical variables
hh_rounded <- hh_num %>% 
                mutate(Age = round(Age, 0),
                       y_householdSpend = round(y_householdSpend, 2))

## Dealing with character data
# Changing "Unknown" or "NULL" as NA for character variables
repl <- c("Unknown","NULL","<NA>")
hh_character <- data.frame(sapply(hh_char, function(x) replace(x, x %in% repl, NA)))

dim(hh_rounded)
dim(hh_character)

## Concatenating both numeric & character dataframes into one household database
household_DF <- cbind(hh_rounded, hh_character)
head(household_DF)

# Update Property Type values
household_DF$PropertyType[household_DF$PropertyType == 'Unknown'] <- '00: Unknown'
household_DF$PropertyType[household_DF$PropertyType == 'Mobile Home'] <- '01: Mobile Home'
household_DF$PropertyType[household_DF$PropertyType == 'Apartment/Group Living'] <- '02: Apartment/Group Living'
household_DF$PropertyType[household_DF$PropertyType == 'Condominium'] <- '03: Condominium'
household_DF$PropertyType[household_DF$PropertyType == 'Duplex'] <- '04: Duplex'
household_DF$PropertyType[household_DF$PropertyType == 'Triplex'] <- '05: Triplex'
household_DF$PropertyType[household_DF$PropertyType == 'Residential'] <- '06: Residential'

# Update Dwelling Unit Size values
household_DF$DwellingUnitSize[household_DF$DwellingUnitSize == '1-Single Family Dwelling'] <- '01: 1-Single Family Dwelling'
household_DF$DwellingUnitSize[household_DF$DwellingUnitSize == '2-Duplex'] <- '02: 2-Duplex'
household_DF$DwellingUnitSize[household_DF$DwellingUnitSize == '3-Triplex'] <- '03: 3-Triplex'
household_DF$DwellingUnitSize[household_DF$DwellingUnitSize == '4'] <- '04: 4'
household_DF$DwellingUnitSize[household_DF$DwellingUnitSize == '5-9'] <- '05: 5-9'
household_DF$DwellingUnitSize[household_DF$DwellingUnitSize == '10-19'] <- '06: 10-19'
household_DF$DwellingUnitSize[household_DF$DwellingUnitSize == '20-49'] <- '07: 20-49'
household_DF$DwellingUnitSize[household_DF$DwellingUnitSize == '50-100'] <- '08: 50-100'
household_DF$DwellingUnitSize[household_DF$DwellingUnitSize == '101 and up'] <- '09: 101 and up'

## Doing Summary Statistics for the household dataset
summary(household_DF)

unique(household_DF$DwellingUnitSize)

# Checking number of missing value per variable - household_DF
colSums(is.na(household_DF))

# Checking percentage of missing value per variable - household_DF
colMeans(is.na(household_DF)) * 100

# Dealing with missing values - using imputer
# For numerical variables (Age and EstHomeValue), use mean_imputer
household_DF$Age[is.na(household_DF$Age)] <- mean(household_DF$Age, na.rm = TRUE)
household_DF$EstHomeValue[is.na(household_DF$EstHomeValue)] <- mean(household_DF$EstHomeValue, na.rm = TRUE)

# For categorical variables, first check unique values of each variable  (Gender, State, PropertyType)
unique(household_DF$Gender)
unique(household_DF$PropertyType)
unique(household_DF$DwellingUnitSize)

# Since there's no "Other" group, use mode_imputer
household_DF$Gender[is.na(household_DF$Gender)] <- names(which.max(table(household_DF$Gender)))
household_DF$PropertyType[is.na(household_DF$PropertyType)] <- names(which.max(table(household_DF$PropertyType)))
household_DF$DwellingUnitSize[is.na(household_DF$DwellingUnitSize)] <- names(which.max(table(household_DF$DwellingUnitSize)))

# Checking number of missing value per variable - household_DF
colSums(is.na(household_DF))
## Doing Summary Statistics for the household dataset
summary(household_DF)



## Loading supplemental data source - Consumer; Changing "" (blanks) as NA ###
consumer <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/consumerData_training15K_studentVersion.csv', na.strings = "")
# Filtering Consumer data using 'tmpID' from household data - using subset
cons_DF <- subset(consumer, tmpID %in% household_DF$tmpID)

## Doing Summary Statistics for the Consumer dataset
summary(cons_DF)
str(cons_DF)

## Dealing with character data
# Changing "Unknown" or "NULL" as NA for character variables
repl <- c("Unknown","NULL","<NA>", "NA")
cons_DF2 <- data.frame(sapply(cons_DF, function(x) replace(x, x %in% repl, NA)))
str(cons_DF2)

# Change back tmpID to integer (due to code above)
cons_DF2$tmpID <- as.integer(cons_DF2$tmpID)

# Changing numeric variables tagged as character (MedianEducationYears)
cons_DF2$MedianEducationYears <- as.integer(cons_DF2$MedianEducationYears)

# Create a variable for Pet owner (regardless if Horse/Dog/Cat/Others)
cons_DF2$PetOwner <- ifelse(cons_DF2$HorseOwner == "Yes" | 
                               cons_DF2$CatOwner == "Yes" | 
                               cons_DF2$DogOwner == "Yes" | 
                               cons_DF2$OtherPetOwner == "Yes", "Yes", "No")

# Fixing NetWorth variable, since it would not be sorted
cons_DF2$NetWorth[cons_DF2$NetWorth == '$1-4999'] <- '01: $1-4999'
cons_DF2$NetWorth[cons_DF2$NetWorth == '$5000-9999'] <- '02: $5000-9999'
cons_DF2$NetWorth[cons_DF2$NetWorth == '$10000-24999'] <- '03: $10000-24999'
cons_DF2$NetWorth[cons_DF2$NetWorth == '$25000-49999'] <- '04: $25000-49999'
cons_DF2$NetWorth[cons_DF2$NetWorth == '$50000-99999'] <- '05: $50000-99999'
cons_DF2$NetWorth[cons_DF2$NetWorth == '$100000-249999'] <- '06: $100000-249999'
cons_DF2$NetWorth[cons_DF2$NetWorth == '$250000-499999'] <- '07: $250000-499999'
cons_DF2$NetWorth[cons_DF2$NetWorth == '$499999+'] <- '08: $499999+'

# Fixing Education Level; Combining Extremely Likely and Likely; to have lesser groups
cons_DF2$Education[cons_DF2$Education == 'Unknown'] <- '00: Unknown'
cons_DF2$Education[cons_DF2$Education == 'Less than HS Diploma - Ex Like'] <- '01: Less than HS Diploma'
cons_DF2$Education[cons_DF2$Education == 'Less than HS Diploma - Likely'] <- '01: Less than HS Diploma'
cons_DF2$Education[cons_DF2$Education == 'HS Diploma - Likely'] <- '02: HS Diploma'
cons_DF2$Education[cons_DF2$Education == 'HS Diploma - Extremely Likely'] <- '02: HS Diploma'
cons_DF2$Education[cons_DF2$Education == 'Vocational Technical Degree - Extremely Likely'] <- '03: Vocational Technical Degree'
cons_DF2$Education[cons_DF2$Education == 'Some College - Likely'] <- '04: Some College'
cons_DF2$Education[cons_DF2$Education == 'Some College -Extremely Likely'] <- '04: Some College'
cons_DF2$Education[cons_DF2$Education == 'Bach Degree - Likely'] <- '05: Bach Degree'
cons_DF2$Education[cons_DF2$Education == 'Bach Degree - Extremely Likely'] <- '05: Bach Degree'
cons_DF2$Education[cons_DF2$Education == 'Grad Degree - Likely'] <- '06: Grad Degree'
cons_DF2$Education[cons_DF2$Education == 'Grad Degree - Extremely Likely'] <- '06: Grad Degree'


# Checking number of missing value per variable - consumer_DF
colSums(is.na(cons_DF2))

# Checking percentage of missing value per variable - consumer_DF
colMeans(is.na(cons_DF2)) * 100
# Many variables have more than 30% missing values. Remove those variables

# Saving percentage of missing value per variable in a data frame
missingval <- colMeans(is.na(cons_DF2))

# Drop columns with at least 30% missing values
consumer_DF <- cons_DF2[, missingval < 0.3]
colSums(is.na(consumer_DF))

# Dealing with missing values
# For categorical variables, first check unique values of each variable
# ComputerOwnerInHome,  ISPSA, HomeOwnerRenter, MedianEducationYears, EthnicDescription, BroadEthnicGroupings, PresenceOfChildrenCode, NetWorth, Education
unique(consumer_DF$ComputerOwnerInHome)
unique(consumer_DF$ISPSA)
unique(consumer_DF$HomeOwnerRenter)
unique(consumer_DF$MedianEducationYears)
unique(consumer_DF$EthnicDescription)
unique(consumer_DF$BroadEthnicGroupings)
unique(consumer_DF$PresenceOfChildrenCode)
unique(consumer_DF$NetWorth)
unique(consumer_DF$Education)

# For ComputerOwnerInHome, since it's a Yes/NA, change NA to "Unknown"
consumer_DF$ComputerOwnerInHome[is.na(consumer_DF$ComputerOwnerInHome)] <- "Unknown"

# For other categorical variables, use mode_imputer
consumer_DF$ISPSA[is.na(consumer_DF$ISPSA)] <- names(which.max(table(consumer_DF$ISPSA)))
consumer_DF$HomeOwnerRenter[is.na(consumer_DF$HomeOwnerRenter)] <- names(which.max(table(consumer_DF$HomeOwnerRenter)))
consumer_DF$MedianEducationYears[is.na(consumer_DF$MedianEducationYears)] <- names(which.max(table(consumer_DF$MedianEducationYears)))
consumer_DF$EthnicDescription[is.na(consumer_DF$EthnicDescription)] <- names(which.max(table(consumer_DF$EthnicDescription)))
consumer_DF$BroadEthnicGroupings[is.na(consumer_DF$BroadEthnicGroupings)] <- names(which.max(table(consumer_DF$BroadEthnicGroupings)))
consumer_DF$PresenceOfChildrenCode[is.na(consumer_DF$PresenceOfChildrenCode)] <- names(which.max(table(consumer_DF$PresenceOfChildrenCode)))
consumer_DF$NetWorth[is.na(consumer_DF$NetWorth)] <- names(which.max(table(consumer_DF$NetWorth)))
consumer_DF$Education[is.na(consumer_DF$Education)] <- names(which.max(table(consumer_DF$Education)))

# Change integer variables tagged as character (due to imputer code)
consumer_DF$MedianEducationYears <- as.integer(consumer_DF$MedianEducationYears)
consumer_DF$ISPSA <- as.integer(consumer_DF$ISPSA)

# Checking number of missing value per variable - consumer_DF
colSums(is.na(consumer_DF))
## Doing Summary Statistics for the consumer dataset
summary(consumer_DF)




## Loading supplemental data source - Donations; Changing "" (blanks) as NA ###
donation <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/DonationsData_training15K_studentVersion.csv', na.strings = "")
# Filtering Donations data using 'tmpID' from household data - using subset
donation_DF <- subset(donation, tmpID %in% household_DF$tmpID)

## Doing Summary Statistics for the Donations dataset
summary(donation_DF)
str(donation_DF)

## Dealing with character data
# Changing "Unknown" or "NULL" as NA for character variables
repl <- c("Unknown","NULL","<NA>")
donations_DF <- data.frame(sapply(donation_DF, function(x) replace(x, x %in% repl, NA)))
str(donations_DF)

# Change back tmpID to integer (due to code above)
donations_DF$tmpID <- as.integer(donations_DF$tmpID)

# Checking number of missing value per variable - donations_DF
colSums(is.na(donations_DF))

# Checking percentage of missing value per variable - donations_DF
colMeans(is.na(donations_DF)) * 100
# ALL variables have more than 30% missing values.

# To check how many are donating to ANY cause
df <- filter(donations_DF, rowSums(is.na(donations_DF)) != (ncol(donations_DF)-1)) # Apply filter function
summary(df)

# Split Causes - Religious/Political ; Other Causes
# Religious/Political
RP_df <- subset(donations_DF, select = c(tmpID, ReligiousContributorInHome, PoliticalContributerInHome))
donations_RP <- filter(RP_df, rowSums(is.na(RP_df)) != (ncol(RP_df)-1))
summary(donations_RP)

# Other Causes
Other_df <- subset(donations_DF, select = -c(ReligiousContributorInHome, PoliticalContributerInHome))
donations_Oth <- filter(Other_df, rowSums(is.na(Other_df)) != (ncol(Other_df)-1))
summary(donations_Oth)

# Use these datasets as tag, if donator or not



## Loading supplemental data source - Political; Changing "" (blanks) as NA ###
political <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/politicalData_training15K_studentVersion.csv', na.strings = "")
# Filtering Political data using 'tmpID' from household data - using subset
politic_DF <- subset(political, tmpID %in% household_DF$tmpID)

## Doing Summary Statistics for the Political dataset
summary(politic_DF)
str(politic_DF)

## Dealing with character data
# Changing "Unknown" or "NULL" as NA for character variables
repl <- c("Unknown","NULL","<NA>")
political_DF <- data.frame(sapply(politic_DF, function(x) replace(x, x %in% repl, NA)))
str(political_DF)

# Change back tmpID to integer (due to code above)
political_DF$tmpID <- as.integer(political_DF$tmpID)

# Checking number of missing value per variable - political_DF
colSums(is.na(political_DF))

# Checking percentage of missing value per variable - political_DF
colMeans(is.na(political_DF)) * 100
# Most of the variables have more than 30% missing values.
# Keep variables < 30% missing values

# Saving percentage of missing value per variable in a data frame
missingval <- colMeans(is.na(political_DF))

# Drop columns with at least 30% missing values
political_DF <- political_DF[, missingval < 0.3]
colSums(is.na(political_DF))

# Dealing with missing values
# For categorical variables, first check unique values of each variable
# ReligionsDescription
unique(political_DF$ReligionsDescription)

# For categorical variables, use mode_imputer
political_DF$ReligionsDescription[is.na(political_DF$ReligionsDescription)] <- names(which.max(table(political_DF$ReligionsDescription)))




## Loading supplemental data source - Magazine; Changing "" (blanks) as NA ###
magazine <- read.csv('https://raw.githubusercontent.com/kwartler/Hult_Visualizing-Analyzing-Data-with-R/main/BAN1_Case_Info/A1_Household_Direct_Mail/magazineData_training15K_studentVersion.csv', na.strings = "")
# Filtering Political data using 'tmpID' from household data - using subset
mag_DF <- subset(magazine, tmpID %in% household_DF$tmpID)

## Doing Summary Statistics for the magazine dataset
summary(mag_DF)
str(mag_DF)

## Dealing with character data
# Changing "Unknown" or "NULL" as NA for character variables
repl <- c("Unknown","NULL","<NA>")
magazine_DF <- data.frame(sapply(mag_DF, function(x) replace(x, x %in% repl, NA)))
str(magazine_DF)

# Change back tmpID to integer (due to code above)
magazine_DF$tmpID <- as.integer(magazine_DF$tmpID)

# Checking number of missing value per variable - magazine_DF
colSums(is.na(magazine_DF))

# Checking percentage of missing value per variable - magazine_DF
colMeans(is.na(magazine_DF)) * 100
# ALL variables have more than 30% missing values.

# To check how many have ANY subscriptions
magazine_sub <- filter(magazine_DF, rowSums(is.na(magazine_DF)) != (ncol(magazine_DF)-1)) # Apply filter function
summary(magazine_sub)

# NOT RECOMMENDING to use this dataset as is. Can be used as tag for magazine subscription



### Concatenating main dataset to supplementary datasets ###
# Check which supplementary datasets are non-null
colnames(consumer_DF)
colnames(donations_DF)
colnames(political_DF)
colnames(magazine_DF)

# List of the three datasets
df_list <- list(household_DF, consumer_DF, political_DF)

# Merge three datasets: household_DF, consumer_DF, political_DF ; by tmpID
main_base <- data.frame(power_left_join(df_list, by = "tmpID"))

# Checking if supporter of religious/political causes
main_base$RPSupporter <- ifelse(main_base$tmpID %in% donations_RP$tmpID, 1, 0) 
# Checking if supporter of other causes
main_base$CauseSupporter <- ifelse(main_base$tmpID %in% donations_Oth$tmpID, 1, 0)
main_base$CauseSupporter0 <- ifelse(main_base$tmpID %in% donations_Oth$tmpID, "Yes", "No") 
# Checking if with any magazine subscription
main_base$MagSubscriber <- ifelse(main_base$tmpID %in% magazine_sub$tmpID, 1, 0)
main_base$MagSub <- ifelse(main_base$tmpID %in% magazine_sub$tmpID, "Yes", "No")

# Making dummy variables for some categorical variables
main_base$ComputerOwner <- ifelse(main_base$ComputerOwnerInHome == "Yes", 1, 0)
main_base$Computer_Owner <- ifelse(main_base$ComputerOwnerInHome == "Yes", "Yes", "No")

main_base$HomeOwner <- ifelse(main_base$HomeOwnerRenter == "Likely Homeowner", 1, 0)
main_base$HomeRenter <- ifelse(main_base$HomeOwnerRenter == "Likely Renter", 1, 0)

main_base$Children_Likelihood <- ifelse(main_base$PresenceOfChildrenCode == "Not Likely to have a child",0,1)
main_base$Children_Likelihood <- ifelse(main_base$PresenceOfChildrenCode == "Modeled Not as Likely to have a child",0,1)
main_base$Children_Likelihood <- ifelse(main_base$PresenceOfChildrenCode == "Likely to have a child",1,0)
main_base$Children_Likelihood <- ifelse(main_base$PresenceOfChildrenCode == "Modeled Likely to have a child",1,0)

main_base$Net_Worth <- factor(main_base$NetWorth, levels = c("01: $1-4999", "02: $5000-9999", "03: $10000-24999", "04: $25000-49999", "05: $50000-99999", "06: $100000-249999", "07: $250000-499999", "08: $499999+"), ordered = TRUE) 
main_base$Net_Worth <- unclass(main_base$Net_Worth)

main_base$Educ_Level <- factor(main_base$Education, levels = c("00: Unknown", "01: Less than HS Diploma", "02: HS Diploma", "03: Vocational Technical Degree", "04: Some College", "05: Bach Degree", "06: Grad Degree"), ordered = TRUE) 
main_base$Educ_Level <- unclass(main_base$Educ_Level)

main_base$Property_Type <- factor(main_base$PropertyType, levels = c("00: Unknown", "01: Mobile Home", "02: Apartment/Group Living", "03: Condominium", "04: Duplex", "05: Triplex", "06: Residential"), ordered = TRUE) 
main_base$Property_Type <- unclass(main_base$Property_Type)

main_base$UnitSize <- factor(main_base$DwellingUnitSize, levels = c("00: Unknown", "01: 1-Single Family Dwelling", "02: 2-Duplex", "03: 3-Triplex", "04: 4", "05: 5-9", "06: 10-19", "07: 20-49", "08: 50-100", "09: 101 and up"), ordered = TRUE) 
main_base$UnitSize <- unclass(main_base$UnitSize)
unique(main_base$DwellingUnitSize)

# Checking number of missing value per variable - main_base
colSums(is.na(main_base))


###############################################################
### To avoid unethical bias, remove the following variables ###
###############################################################
final_base <- subset(main_base, select = -c(Gender, RPSupporter, ResidenceHHGenderDescription, EthnicDescription, BroadEthnicGroupings, PartiesDescription, ReligionsDescription, RPSupporter))

final_base


# Make bins for Age
bin1 <- c(0,25,35,45,55,65,Inf)
final_base <- final_base %>% mutate(AgeBin = cut(Age, breaks = bin1, c("Below 25","25-35","35-45","45-55","55-65","65 and Above")))

# Make bins for y_householdSpend
bin2 <- c(0,50,100,200,300,400,500,Inf)
final_base <- final_base %>% mutate(hhSpendBin = cut(y_householdSpend, breaks = bin2, c("Below $50","$50-100","$100-200", "$200-300", "$300-400", "$400-500", "$500 and up")))
head(final_base,10)


## Frequency Counts ##

## Doing Summary Statistics - using aggregate()
summary(final_base)
# Getting the aggregate mean of House Hold Spend
# by Property Type
aggregate(y_householdSpend~ PropertyType, final_base, mean)
# by Presence of Children
aggregate(y_householdSpend~ Children_Likelihood, final_base, mean)
# by Home Owner/Renter
aggregate(y_householdSpend~ HomeOwnerRenter, final_base, mean)
# by Net Worth
aggregate(y_householdSpend~ NetWorth, final_base, mean)
# by Cause Supporter Tag
aggregate(y_householdSpend~ CauseSupporter, final_base, mean)
# by Magazine Subscriber Tag
aggregate(y_householdSpend~ MagSubscriber, final_base, mean)
# by Age
aggregate(y_householdSpend~ AgeBin, final_base, mean)


## Graphs ##

# Plotting the average household spend for BBY
ggplot(final_base, aes(y_householdSpend)) +
  geom_bar(stat="bin", fill="steelblue3")+
  theme_minimal()+
  labs(x = "Average Household Spend in BBY per Group",
       y = "USD($)")

ggsave("G1_Average Spend.jpg", width = 35, height = 20, units = "cm")
dev.off()

# Creating DF with only numerical variables for a correlation graph
num_vars <- sapply(final_base, is.numeric)
corr_data <- final_base[, num_vars]

# Removing tmpID in correlation data
corr_data <- subset(corr_data, select = -tmpID)
str(corr_data)

# Checking for correlation using spearman method since data has ordinal values
corr_plot = cor(corr_data, method = c("spearman"))
#corr_plot = cor(corr_data)
round(corr_plot,3)

# Plotting and saving the correlation matrix
png(height=600, width=600, file="G2_Correlation Matrix.png", type = "cairo")
corrplot(corr_plot, type = "upper", order = "alphabet", 
         tl.col = "black", tl.srt = 45, method = 'color')
dev.off()

# MagSub and Computer_Owner have the highest correlation with the target variable: y_householdSpend

# Checking the highest correlation
## Checking relationship between HHSpend and MagSubscriber - using group by
final_base %>% 
  group_by(MagSub) %>% 
  summarise(Average_Spent = mean(y_householdSpend)) %>% 
  mutate(Average_Spent = format(round(Average_Spent,1), big.mark = ",")) %>% 
  ggplot(aes(MagSub, Average_Spent, fill = MagSub))+
  geom_col()+ 
  theme_minimal()+
  geom_label(aes(label = Average_Spent), color = "black", fill = "white")+
  labs(title = "Magazine Subscription by Average Spent ($) in BBY",
       x = "With Magazine Subscription",
       y = "Avg Spent ($)")+
  theme(axis.text.x=element_blank())
ggsave("G3_Mag Subscription.jpg", width = 30, height = 20, units = "cm")
dev.off()


## Checking relationship between HHSpend and Computer_Owner - using group by
final_base %>% 
  group_by(Computer_Owner) %>% 
  summarise(Average_Spent = mean(y_householdSpend)) %>% 
  mutate(Average_Spent = format(round(Average_Spent,1), big.mark = ",")) %>% 
  ggplot(aes(Computer_Owner, Average_Spent, fill = Computer_Owner))+
  geom_col()+ 
  theme_minimal()+
  geom_label(aes(label = Average_Spent), color = "black", fill = "white")+
  labs(title = "Computer Owner by Average Spent ($) in BBY",
       x = "Owns a Computer",
       y = "Avg Spent ($)")+
  theme(axis.text.x=element_blank())
ggsave("G4_Computer Owner.jpg", width = 30, height = 20, units = "cm")
dev.off()



# Checking other correlation (categorical variables)
## Checking relationship between HHSpend and Property Type - using group by
final_base %>% 
  group_by(PropertyType) %>% 
  summarise(Average_Spent = mean(y_householdSpend)) %>% 
  mutate(Average_Spent = format(round(Average_Spent,1), big.mark = ",")) %>% 
  ggplot(aes(PropertyType, Average_Spent, fill = PropertyType))+
  geom_col()+ 
  theme_minimal()+
  geom_label(aes(label = Average_Spent), color = "black", fill = "white")+
  labs(title = "Property Type by Average Spent ($) in BBY",
       x = "Property Type",
       y = "Avg Spent ($)")+
  theme(axis.text.x=element_blank())

ggsave("G5_Property Type.jpg", width = 30, height = 20, units = "cm")
dev.off()


## Checking relationship between HHSpend and Net Worth - using group by
final_base %>% 
  group_by(NetWorth) %>% 
  summarise(Average_Spent = mean(y_householdSpend)) %>% 
  mutate(Average_Spent = format(round(Average_Spent,1), big.mark = ",")) %>% 
  ggplot(aes(NetWorth, Average_Spent, fill = NetWorth))+
  geom_col()+ 
  theme_minimal()+
  geom_label(aes(label = Average_Spent), color = "black", fill = "white")+
  labs(title = "Net Worth by Average Spent ($) in BBY",
       x = "Net Worth",
       y = "Avg Spent ($)")+
  theme(axis.text.x=element_blank())

#ggsave("G6_Net Worth.jpg", width = 30, height = 20, units = "cm")
#dev.off()

table(final_base$NetWorth)

## Checking relationship between HHSpend and Net Worth - using group by
final_base %>% 
  group_by(NetWorth) %>% 
  summarise(Total_Spent = sum(y_householdSpend)) %>% 
  mutate(Total_Spent = format(round(Total_Spent,1), big.mark = ",")) %>% 
  ggplot(aes(NetWorth, Total_Spent, fill = NetWorth))+
  geom_col()+ 
  theme_minimal()+
  geom_label(aes(label = Total_Spent), color = "black", fill = "white")+
  labs(title = "Net Worth by Total Spent ($) in BBY",
       x = "Net Worth",
       y = "Total Spent ($)")+
  theme(axis.text.x=element_blank())

#ggsave("G6_Net Worth Total.jpg", width = 30, height = 20, units = "cm")
#dev.off()


## Checking relationship between HHSpend and Education Level - using group by
final_base %>% 
  group_by(Education) %>% 
  summarise(Average_Spent = mean(y_householdSpend)) %>% 
  mutate(Average_Spent = format(round(Average_Spent,1), big.mark = ",")) %>% 
  ggplot(aes(Education, Average_Spent, fill = Education))+
  geom_col()+ 
  theme_minimal()+
  geom_label(aes(label = Average_Spent), color = "black", fill = "white")+
  labs(title = "Education Level by Average Spent ($) in BBY",
       x = "Education Level",
       y = "Avg Spent ($)")+
  theme(axis.text.x=element_blank())

ggsave("G7_Education Level.jpg", width = 30, height = 20, units = "cm")
dev.off()


## Checking relationship between HHSpend and Age Group - using group by
final_base %>% 
  group_by(AgeBin) %>% 
  summarise(Average_Spent = mean(y_householdSpend)) %>% 
  mutate(Average_Spent = format(round(Average_Spent,1), big.mark = ",")) %>%
  ggplot(aes(AgeBin, Average_Spent, fill = AgeBin))+
  geom_col()+ 
  theme_minimal()+
  geom_label(aes(label = Average_Spent), color = "black", fill = "white")+
  labs(title = "Age Group by Average Spent ($) in BBY",
       x = "Age Group",
       y = "Avg Spent ($)")+
  theme(axis.text.x=element_blank())

ggsave("G8_Age Group.jpg", width = 30, height = 20, units = "cm")
dev.off()

## Checking relationship between HHSpend and PropertyType & Education Level - using group by
ggplot(final_base, aes(x = y_householdSpend, y = PropertyType)) +
  geom_point(color="steelblue3") +
  facet_wrap(~ Education, nrow = 1) +
  theme_bw()+
  labs(title = "Average Spent ($) in BBY by Property Type and Education Level",
       x = "Avg Spent ($)",
       y = "Property Type")

ggsave("G9_Property_Educ.jpg", width = 40, height = 20, units = "cm")
dev.off()



## Checking relationship between HHSpend and Education Level - using group by
final_base %>% 
  group_by(Education) %>% 
  summarise(Total_Spent = sum(y_householdSpend)) %>% 
  mutate(Total_Spent = format(round(Total_Spent,1), big.mark = ",")) %>% 
  ggplot(aes(Education, Total_Spent, fill = Education))+
  geom_col()+ 
  theme_minimal()+
  geom_label(aes(label = Total_Spent), color = "black", fill = "white")+
  labs(title = "Education Level by Total Spent ($) in BBY",
       x = "Education Level",
       y = "Total Spent ($)")+
  theme(axis.text.x=element_blank())

ggsave("G10_Educ Total.jpg", width = 30, height = 20, units = "cm")
dev.off()


## Checking relationship between HHSpend and Property Type - using group by
final_base %>% 
  group_by(PropertyType) %>% 
  summarise(Total_Spent = sum(y_householdSpend)) %>% 
  mutate(Total_Spent = format(round(Total_Spent,1), big.mark = ",")) %>% 
  ggplot(aes(PropertyType, Total_Spent, fill = PropertyType))+
  geom_col() +
  theme_minimal()+ 
  geom_label(aes(label = Total_Spent), color = "black", fill = "white")+
  labs(title = "Property Type by Total Spent ($) in BBY",
       x = "Property Type",
       y = "Total Spent ($)")+
  theme(axis.text.x=element_blank())

ggsave("G11_Property Total.jpg", width = 30, height = 20, units = "cm")
dev.off()



## Checking relationship between HHSpend and Dwelling Unit - using group by
final_base %>% 
  group_by(UnitSize) %>% 
  summarise(Average_Spent = mean(y_householdSpend)) %>% 
  mutate(Average_Spent = format(round(Average_Spent,1), big.mark = ",")) %>% 
  ggplot(aes(UnitSize, Average_Spent, fill = UnitSize))+
  geom_col()+ 
  theme_minimal()+
  geom_label(aes(label = Average_Spent), color = "black", fill = "white")+
  labs(title = "Household Size by Average Spent ($) in BBY",
       x = "Household Size",
       y = "Avg Spent ($)")+
  theme(axis.text.x=element_blank())

ggsave("G12_Household Size.jpg", width = 30, height = 20, units = "cm")
dev.off()