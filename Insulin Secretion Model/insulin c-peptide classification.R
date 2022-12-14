################################################################################
################################################################################
########### This document is the script used to investigate: ###################
################################################################################
### - StartRight dataset (n ~= 1800)
### - Patients who are not initially treated with insulin
### - Using Dr. Beverley Shields' and Dr. Angus Jones' type-1 diabetes
###   prediction models:
###    > Model 1 = Clinical features (Age + BMI)
###    > Model 2 = Clinical features + autoantibodies (Age, BMI, GAD, IA2)
### - Categorise patients into high and low risk
### - Compare high and low risk individuals NOT treated initially with insulin
###   to individuals initially treated WITH insulin
###    > Descriptive statistics on each group
###    > Graph model probabilities
### - MAIN investigation: UCPCR
###    > Do high risk patients lose insulin function (decrease in UCPCR)?
###    > How does high risk average rate of UCPCR change compare to low risk?
###    > Does high risk exhibit similar UCPCR trends compared to those
###      initially treated with insulin?
################################################################################
################################################################################

################################################################################
############################# Init section #####################################
################################################################################
library(tidyverse)
library(rio)

filepath = '~/Downloads/Insulin secretion model/startight Aug 22 comma separated.csv'
my_data <- import(filepath)
View(my_data)





################################################################################
########### Section for Cleaning and preparing Data: ###########################
################################################################################
### - no current clean GAD, ZNT8 code
### - This section creates new columns to be used in the models
### - autoantibodies considered positive if:
###    > GAD >= 11
###    > IA2 >= 7.5
###    > ZNT8 >= 65
################################################################################
################################################################################

#New column GADA_Status will default to NA, and if negative = 0, positive = 1
my_data$GADA_Status = NA
my_data$GADA_Status[my_data$GAD == 'negative'] = 0
my_data$GADA_Status[grepl("[0-9]",my_data$GAD) & as.numeric(my_data$GAD) >= 11] = 1

#New column ZNT8_Status will default to NA, and if negative = 0, positive = 1
my_data$ZNT8_Status = NA
my_data$ZNT8_Status[my_data$ZNT8 == 'negative'] = 0
my_data$ZNT8_Status[grepl("[0-9]",my_data$ZNT8) & as.numeric(my_data$ZNT8) >= 65] = 1

#New column IA2_Status will default to NA, and if negative = 0, positive = 1
my_data$IA2A_Status = NA
my_data$IA2A_Status[my_data$IA2 == 'negative'] = 0
my_data$IA2A_Status[grepl("[0-9]",my_data$IA2) & as.numeric(my_data$IA2) >= 7.5] = 1





################################################################################
#################### Section for testing Model 1: ##############################
################################################################################
### - Clinical features model        
### - Only takes in AgeatDiag and BMI
### - Selection and model based around following equation:
### = 37.94 + (-5.09 * log(age at diagnosis)) + (-6.34 * log(BMI))
################################################################################
################################################################################

#Create new dataframe for use that includes BMI data
model_data = my_data %>%
  filter( (!is.na(my_data$Height) & !is.na(my_data$Weight)) | !is.na(my_data$BMI) )

#Fill in any missing values for BMI
model_data = model_data %>%
  mutate(BMI = ifelse(is.na(BMI), Weight / (Height^2), BMI))

#Add new column containing model prediction score
model_data$Model1LogOR = 37.94 + (-5.09 * log(model_data$AgeatDiagnosis)) + (-6.34 * log(model_data$BMI))





################################################################################
#################### Section for testing Model 2: ##############################
################################################################################
### - Clinical features + GADA model 
### - Takes in AgeatDiag, BMI and GADA
### - Selection and model based around following equation, where GADA = 0 if negative, 1 if positive
### = 34.8057844720 + (-4.801441792 * log (Age at diagnosis)) + (-5.980577792 * log(BMI)) + (2.937107976 * GADA)
################################################################################
################################################################################

# Set default Model2LogOR as NA for those who do not have GAD data
model_data$Model2LogOR[is.na(model_data$GADA_Status)] = NA

# Use new Model2LogOR column and if != na, run equation
model_data$Model2LogOR[!is.na(model_data$GADA_Status)] = 34.8057844720 + (-4.801441792 * log (model_data$AgeatDiagnosis[!is.na(model_data$GADA_Status)])) + (-5.980577792 * log(model_data$BMI[!is.na(model_data$GADA_Status)])) + (2.937107976 * model_data$GADA_Status[!is.na(model_data$GADA_Status)])





################################################################################
#################### Section for testing Model 3: ##############################
################################################################################
### - Clinical features + IA2 model
### - Takes in AgeatDiag, BMI and IA2
### - Selection and model based around following equation, where IA2 = 0 if negative, 1 if positive
### = 37.26905033 + (3.194096 * IA2??? ) + (-5.047657308 * Log(Age)) + (-6.287258808 * Log(BMI))
################################################################################
################################################################################

# Set default Model3LogOR as NA for those who do not have IA2 data
model_data$Model3LogOR[is.na(model_data$IA2A_Status)] = NA

# Use new Model3LogOR column and if != na, run equation
model_data$Model3LogOR[!is.na(model_data$IA2A_Status)] = 37.26905033 + (3.194096 * model_data$IA2A_Status[!is.na(model_data$IA2A_Status)]) + (-5.047657308 * log(model_data$AgeatDiagnosis[!is.na(model_data$IA2A_Status)])) + (-6.287258808 * log(model_data$BMI[!is.na(model_data$IA2A_Status)]))





################################################################################
#################### Section for testing Model 4: ##############################
################################################################################
### - Clinical features + GAD + ZNT8 model
### - Takes in AgeatDiag, BMI, GAD and IA2
### - Selection and model based around following equation, where ??? AntiStatus1 = GADA positive only, AntiStatus2 = Znt8 positive only, AntiStatus3 = Both GADA and Znt8 positive. Please code all of these as 0=no and 1=yes
### = 33.49649577 + (-4.665598345 * Log(Age)) + (-5.81137397 * Log(BMI)) + (3.082366 * AntiStatus1???) + (3.494462 * AntiStatus2???) + (4.350717 * AntiStatus3???)
################################################################################
################################################################################

# Set default Model4LogOR as NA for those who do not have GAD data or IA2
model_data$Model4LogOR[is.na(model_data$IA2A_Status) | is.na(model_data$GADA_Status)] = NA

#Create new column to see if patient is positive for both antibodies
model_data$Both_Status[is.na(model_data$IA2A_Status) | is.na(model_data$GADA_Status)] = NA
model_data$Both_Status[!is.na(model_data$IA2A_Status) & !is.na(model_data$GADA_Status)] = 0
model_data$Both_Status[model_data$IA2A_Status == 1 & model_data$GADA_Status == 1] = 1

# Use new Model4LogOR column and if data != na, run equation
model_data$Model4LogOR[!is.na(model_data$Both_Status)] = 33.49649577 + (-4.665598345 * log(model_data$AgeatDiagnosis[!is.na(model_data$Both_Status)])) + (-5.81137397 * log(model_data$BMI[!is.na(model_data$Both_Status)])) + (3.082366 * model_data$GADA_Status[!is.na(model_data$Both_Status)]) + (3.494462 * model_data$IA2A_Status[!is.na(model_data$Both_Status)]) + (4.350717 * model_data$Both_Status[!is.na(model_data$Both_Status)])





################################################################################
############## Converting LogOR into Probability (risk) ########################
################################################################################
### - uses following equation
### = exp(LogOR) / (1 + exp(LogOR)
################################################################################
model_data$Model1Prob[!is.na(model_data$Model1LogOR)] = exp(model_data$Model1LogOR[!is.na(model_data$Model1LogOR)]) / (1 + exp(model_data$Model1LogOR[!is.na(model_data$Model1LogOR)]) )
model_data$Model2Prob[!is.na(model_data$Model2LogOR)] = exp(model_data$Model2LogOR[!is.na(model_data$Model2LogOR)]) / (1 + exp(model_data$Model2LogOR[!is.na(model_data$Model2LogOR)]) )
model_data$Model3Prob[!is.na(model_data$Model3LogOR)] = exp(model_data$Model3LogOR[!is.na(model_data$Model3LogOR)]) / (1 + exp(model_data$Model3LogOR[!is.na(model_data$Model3LogOR)]) )
model_data$Model4Prob[!is.na(model_data$Model4LogOR)] = exp(model_data$Model4LogOR[!is.na(model_data$Model4LogOR)]) / (1 + exp(model_data$Model4LogOR[!is.na(model_data$Model4LogOR)]) )





################################################################################
######################### Calculating UCPCR Slopes #############################
################################################################################
### - Calculate average annual change in UCPCR per person
###    > Taken as (V2-V1 + V3 - V2) / 2 if they have data, or simply V2-V1
################################################################################

# Calculate UCPCR Change from Diagnosis (V1) to V2
model_data$V1_V2_UCPCR_Change[!is.na(model_data$UCPCR) & !is.na(model_data$V2UCPCR)] = model_data$V2UCPCR[!is.na(model_data$UCPCR) & !is.na(model_data$V2UCPCR)] - model_data$UCPCR[!is.na(model_data$UCPCR) & !is.na(model_data$V2UCPCR)]

# Calculate UCPCR Change from V2 to V3
model_data$V2_V3_UCPCR_Change[!is.na(model_data$V2UCPCR) & !is.na(model_data$V3UCPCR)] = model_data$V3UCPCR[!is.na(model_data$V2UCPCR) & !is.na(model_data$V3UCPCR)] - model_data$V2UCPCR[!is.na(model_data$V2UCPCR) & !is.na(model_data$V3UCPCR)]

# Calculate annual avg rate of UCPCR Change for those who HAVE V1 -> V2 data
model_data$AvgAnnualRUCPCR[!is.na(model_data$V1_V2_UCPCR_Change)] = model_data$V1_V2_UCPCR_Change[!is.na(model_data$V1_V2_UCPCR_Change)]

#Calculate annual avg rate of UCPCR change for those who HAVE V2 -> V3 data
model_data$AvgAnnualRUCPCR[!is.na(model_data$V2_V3_UCPCR_Change)] = model_data$V2_V3_UCPCR_Change[!is.na(model_data$V2_V3_UCPCR_Change)]

#Calculate annual avg rate of UCPCR change for those who HAVE V1 -> V2 -> V3 data
model_data$AvgAnnualRUCPCR[!is.na(model_data$V1_V2_UCPCR_Change) & !is.na(model_data$V2_V3_UCPCR_Change)] = (model_data$V1_V2_UCPCR_Change[!is.na(model_data$V1_V2_UCPCR_Change) & !is.na(model_data$V2_V3_UCPCR_Change)] + model_data$V2_V3_UCPCR_Change[!is.na(model_data$V1_V2_UCPCR_Change) & !is.na(model_data$V2_V3_UCPCR_Change)]) / 2



 TODO:
   Change diagnosing method from type_of_diabetes to on/not on insulin 
   $insulin_initially 

################################################################################
############ Cleaning Data and Descriptive Stats ###############################
################################################################################
### - Segregating groups into type_1 and type_2
### - Obtaining descriptive stats on BMI, Age, HbA1c, Antibodies, UCPCR, and risk
### - Obtaining further descriptive stats on High/Low risk type_2
################################################################################

#People who self-report a clinical diagnosis of Type 1 AND are ON insulin
type_1_group = model_data %>%
  filter(Type_of_diabetes == "Type 1" & Insulin == "Yes")

#People who self-report a clinical diagnosis of Type 2 AND are NOT on insulin at treatment
type_2_group = model_data %>%
  filter(Type_of_diabetes == "Type 2" & Insulin == "No")

#General summary stats - Type_1
nrow(type_1_group) # n
nrow(type_1_group[type_1_group$Insulin == "Yes",]) # # on insulin
nrow(type_1_group[type_1_group$GADA_Status == 1 & type_1_group$IA2A_Status == 1 ,]) # # with both antibodies positive
type_1_group %>% #remainder of descriptive stats
  select(BMI, AgeatDiagnosis, GADA_Status, IA2A_Status, Insulin, HbA1c_at_diagnosis, AvgAnnualRUCPCR, Model4Prob) %>%
  describe()

#General summary stats - Type_2
nrow(type_2_group) # n
nrow(type_2_group[type_2_group$Insulin == "Yes",]) # # on insulin
nrow(type_2_group[type_2_group$GADA_Status == 1 & type_2_group$IA2A_Status == 1 ,]) # # with both antibodies positive
type_2_group %>% #remainder of descriptive stats
  select(BMI, AgeatDiagnosis, GADA_Status, IA2A_Status, Insulin, HbA1c_at_diagnosis, AvgAnnualRUCPCR, Model4Prob) %>%
  describe()

#Specific summary stats - High risk Type_2 (>0.6)
type_2_group %>% # n
  filter(Model4Prob > 0.6) %>%
  nrow()

type_2_group %>% # # where both antibody positive
  filter(Model4Prob > 0.6) %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()

type_2_group %>% #remainder of descriptive stats
  filter(Model4Prob > 0.6) %>%
  select(BMI, AgeatDiagnosis, GADA_Status, IA2A_Status, Insulin, HbA1c_at_diagnosis, AvgAnnualRUCPCR, Model4Prob) %>%
  describe()

#Specific summary stats - Low risk Type_2 (<0.4)
type_2_group %>% # n
  filter(Model4Prob < 0.4) %>%
  nrow()

type_2_group %>% # # where both antibody positive
  filter(Model4Prob < 0.4) %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()

type_2_group %>% #remainder of descriptive stats
  filter(Model4Prob < 0.4) %>%
  select(BMI, AgeatDiagnosis, GADA_Status, IA2A_Status, Insulin, HbA1c_at_diagnosis, AvgAnnualRUCPCR, Model4Prob) %>%
  describe()


################################################################################
#################### Graphing and Visualizing ##################################
################################################################################
### - Graphing type_1 vs type_2 UCPCR
### - Graphing high risk T2 vs low risk T2 UCPCR
################################################################################

#Graphing T1 UCPCR
type_1_group %>%
  filter(!is.na(UCPCR) & !is.na(V2UCPCR) & !is.na(V3UCPCR)) %>%
  select(UCPCR, V2UCPCR, V3UCPCR) %>%
  rename("Visit 1" = "UCPCR") %>%
  rename("Visit 2" = "V2UCPCR") %>%
  rename("Visit 3" = "V3UCPCR") %>%
  gather() %>%
  ggplot2::ggplot() +
  geom_boxplot(aes(x = key, y = value)) +
  xlab('Visits') +
  ylab('UCPCR mol/mmol') +
  geom_smooth(aes(x = key, y = value), method = 'lm')

#Graphing T2 UCPCR
type_2_group %>%
  filter(!is.na(UCPCR) & !is.na(V2UCPCR) & !is.na(V3UCPCR) & V3UCPCR < 20) %>%
  select(UCPCR, V2UCPCR, V3UCPCR) %>%
  rename("Visit 1" = "UCPCR") %>%
  rename("Visit 2" = "V2UCPCR") %>%
  rename("Visit 3" = "V3UCPCR") %>%
  gather() %>%
  ggplot2::ggplot() +
  geom_boxplot(aes(x = key, y = value)) +
  xlab('Visits') +
  ylab('UCPCR mol/mmol') +
  geom_smooth(aes(x = key, y = value), method = 'lm')


# Graphing LOW risk T2 UCPCR
type_2_group %>%
  filter(!is.na(UCPCR) & !is.na(V2UCPCR) & !is.na(V3UCPCR) & Model4Prob < 0.4 & V3UCPCR < 20) %>%
  select(UCPCR, V2UCPCR, V3UCPCR) %>%
  rename("Visit 1" = "UCPCR") %>%
  rename("Visit 2" = "V2UCPCR") %>%
  rename("Visit 3" = "V3UCPCR") %>%
  gather() %>%
  ggplot2::ggplot() +
  geom_boxplot(aes(x = key, y = value)) +
  xlab('Visits') +
  ylab('UCPCR mol/mmol') +
  geom_smooth(aes(x = key, y = value), method = 'lm')

# Graphing HIGH risk T2 UCPCR
type_2_group %>%
  filter(!is.na(UCPCR) & !is.na(V2UCPCR) & !is.na(V3UCPCR) & Model4Prob > 0.6 & V3UCPCR < 20) %>%
  select(UCPCR, V2UCPCR, V3UCPCR) %>%
  rename("Visit 1" = "UCPCR") %>%
  rename("Visit 2" = "V2UCPCR") %>%
  rename("Visit 3" = "V3UCPCR") %>%
  gather() %>%
  ggplot2::ggplot() +
  geom_boxplot(aes(x = key, y = value)) +
  xlab('Visits') +
  ylab('UCPCR mol/mmol') +
  geom_smooth(aes(x = key, y = value), method = 'lm')

################################################################################
################### Generating Individual Slopes ###############################
################################################################################
### - Generate idividual slopes for T1 individuals with 3 visits' UCPCR data
### - Generate individual slopes for T2 individuals with 3 visits' UCPCR data
### - Validate whether 'high risk' individuals match T1 UCPCR change
################################################################################

#Finding individuals with 3 visits worth of UCPCR -  T1
type_1_group$StartRightID[!is.na(type_1_group$UCPCR) & !is.na(type_1_group$V2UCPCR) & !is.na(type_1_group$V3UCPCR)]
type_1_IDs = list("SR0041", 'SR0045', 'SR0060', 'SR0076', 'SR0077', 'SR0078', 'SR0080', 'SR0096')

#Finding individuals with 3 visits worth of UCPCR - T2
type_2_group$StartRightID[!is.na(type_2_group$UCPCR) & !is.na(type_2_group$V2UCPCR) & !is.na(type_2_group$V3UCPCR) & type_2_group$Model4Prob > 0.6]
high_risk_IDs = list("SR0522", "SR0937", "SR1036", "SR1157", "SR1254", "SR1712", "SR1727", "SR2102")

# High Risk slopes { just change the array[index] up to a max of 8 }
type_2_group %>%
  filter(StartRightID == high_risk_IDs[8]) %>%
  select(UCPCR, V2UCPCR, V3UCPCR) %>%
  rename("Visit 1" = "UCPCR") %>%
  rename("Visit 2" = "V2UCPCR") %>%
  rename("Visit 3" = "V3UCPCR") %>%
  gather() %>%
  ggplot2::ggplot(aes(x = key, y = value, group = 1)) +
  #geom_point() +
  geom_line() +
  xlab('Visits') +
  ylab('UCPCR mol/mmol') +
  ylim(0,20)


#people with decline in UCPCR
(type_2_group$V1_V2_UCPCR_Change[type_2_group$V1_V2_UCPCR_Change < 0])
type_2_group %>%
  filter(type_2_group$V1_V2_UCPCR_Change < 0 & type_2_group$V2_V3_UCPCR_Change < 0) %>%
  ggplot() +
  geom_point(aes(x = Model4Prob, y= V1_V2_UCPCR_Change))

type_2_group %>%
  filter(type_2_group$V1_V2_UCPCR_Change < 0 & type_2_group$V2_V3_UCPCR_Change < 0) %>%
  filter(!is.na(UCPCR) & !is.na(V2UCPCR) & !is.na(V3UCPCR) & V3UCPCR < 20) %>% #remove no/bad UCPCR data
  #filter(Model4Prob > 0.2 & Model4Prob < 0.8) %>% # remove probability extremes
  ggplot() +
  geom_point(aes(x = Model4Prob, y= AvgAnnualRUCPCR))

#Graph boxplot UCPCR/visit data for those of high risk (<0.4)
type_2_group %>%
  filter(!is.na(UCPCR) & !is.na(V2UCPCR) & !is.na(V3UCPCR) & V3UCPCR < 20 & Model4Prob > 0.7) %>%
  select(UCPCR, V2UCPCR, V3UCPCR) %>%
  rename("Visit 1" = "UCPCR") %>%
  rename("Visit 2" = "V2UCPCR") %>%
  rename("Visit 3" = "V3UCPCR") %>%
  gather() %>%
  ggplot2::ggplot() +
  #geom_violin(aes(x = key, y = value)) +
  geom_boxplot(aes(x = key, y = value)) +
  #geom_point(aes(x = key, y = value)) +
  xlab('Visits') +
  ylab('UCPCR mol/mmol') +
  geom_smooth(aes(x = key, y = value), method = 'lm')




################################################################################
######################## Analyzing Risk Strata #################################
################################################################################
### - Create new column/variable Time_to_insulin
###   > done by finding difference between date of continuous insulin and
###     date of diagnosis
### - Create new dataframes for each strata
###   > Selects data used in descriptive stats
###   > BMI, IA2, GAD, Age, ModelProb, Insulin, Type of Diabetes
### - Run lots of descriptive stats on the different features for each strata
################################################################################

#Creating Time_to_insulin variable
model_data$Time_to_insulin[model_data$Date_continuous_insulin != '' & model_data$DateofDiagnosis != ''] = difftime(as.Date(model_data$Date_continuous_insulin[model_data$Date_continuous_insulin != '' & model_data$DateofDiagnosis != ''], "%d-%B-%Y"), as.Date(model_data$DateofDiagnosis[model_data$Date_continuous_insulin != '' & model_data$DateofDiagnosis != ''], "%d-%B-%Y") , units = 'weeks')
model_data$Time_to_insulin[model_data$Time_to_insulin < 0] = 0

#select everyone with switch to insulin OVER 3 months
model_data$Time_to_insulin[!is.na(model_data$Time_to_insulin) & model_data$Time_to_insulin > (52/12)*3]


#descriptive stats for High Risk group
high_risk = model_data %>%
  filter(Model4Prob >= 0.667) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model4Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(high_risk)
describe(high_risk)
high_risk %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()
high_risk %>%
  filter(Insulin == "Yes") %>%
  nrow()
high_risk%>%
  filter(Type_of_diabetes == "Type 1") %>%
  nrow()
high_risk%>%
  filter(Type_of_diabetes == "Type 2") %>%
  nrow()
describe(as.numeric(high_risk$Time_to_insulin))
#calculate how many switched to insulin
high_risk %>%
  filter(Insulin == "No") %>%
  filter(V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()
#calculate how many T2 switched to insulin
high_risk %>%
  filter(Insulin == "No" & Type_of_diabetes == "Type 2") %>%
  filter(V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()


#descriptive stats for Mid Risk group
mid_risk = model_data %>%
  filter(Model4Prob < 0.667 & Model4Prob > 0.333) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model4Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(mid_risk)
describe(mid_risk)
mid_risk %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()
mid_risk %>%
  filter(Insulin == "Yes") %>%
  nrow()
mid_risk%>%
  filter(Type_of_diabetes == "Type 1") %>%
  nrow()
mid_risk%>%
  filter(Type_of_diabetes == "Type 2") %>%
  nrow()
describe(as.numeric(mid_risk$Time_to_insulin))
mid_risk %>%
  filter(Insulin == "No") %>%
  filter(V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()
#calculate how many T2 switched to insulin
mid_risk %>%
  filter(Insulin == "No" & Type_of_diabetes == "Type 2") %>%
  filter(V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()

nrow(my_data)
my_data$RecruitmentSite




#descriptive stats for Low Risk group
low_risk = model_data %>%
  filter(Model4Prob <= 0.333) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model4Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(low_risk)
describe(low_risk)
low_risk %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()
low_risk %>%
  filter(Insulin == "Yes") %>%
  nrow()
low_risk%>%
  filter(Type_of_diabetes == "Type 1") %>%
  nrow()
low_risk%>%
  filter(Type_of_diabetes == "Type 2") %>%
  nrow()
describe(as.numeric(low_risk$Time_to_insulin))
low_risk %>%
  filter(Insulin == "No") %>%
  filter(V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()

#Type 1 diabetes risk score
model_data$GRS





################################################################################
##################### Graphing UCPCR vs Models #################################
################################################################################
### - Create new column/variable Time_to_insulin
################################################################################

#Visualise avg change of UCPCR
hist(model_data$AvgAnnualRUCPCR,
     xlim = c(-6,10))

#Plot UCPCR change against prediction Model
plot(model_data$Model1Prob, model_data$AvgAnnualRUCPCR,
     xlab = 'Clinical Features Model Probability',
     ylab = 'Average Rate of Change in C-Peptide',
     pch = 16)
plot(model_data$Model2Prob, model_data$AvgAnnualRUCPCR,
     xlab = 'Clinical + GAD AutoAntibody Model Probability',
     ylab = 'Average Rate of Change in C-Peptide', 
     pch = 16)
plot(model_data$Model3Prob, model_data$AvgAnnualRUCPCR,
     xlab = 'Clinical + ZNT8 AutoAntibody Model Probability',
     ylab = 'Average Rate of Change in C-Peptide', 
     pch = 16)
plot(model_data$Model4Prob, model_data$AvgAnnualRUCPCR,
     xlab = 'Clinical + AutoAntibody Model Probability',
     ylab = 'Average Rate of Change in C-Peptide', 
     pch = 16)


#Plot UCPCR LOSS against prediction model
plot(model_data$Model4Prob[model_data$AvgAnnualRUCPCR <0], model_data$AvgAnnualRUCPCR[model_data$AvgAnnualRUCPCR <0],
     xlab = 'Clinical + AutoAntibody Model Probability',
     ylab = 'Average Rate of Loss in C-Peptide', 
     pch = 16)




type_2_group %>%
  select(UCPCR, V2UCPCR, V3UCPCR) %>%
  rename("Visit 1" = "UCPCR") %>%
  rename("Visit 2" = "V2UCPCR") %>%
  rename("Visit 3" = "V3UCPCR") %>%
  gather() %>%
  filter(value < 20) %>%
  # mutate(key = as.numeric(key)) %>%
  ggplot2::ggplot() +
  geom_violin(aes(x = key, y = value)) +
  #geom_boxplot(aes(x = key, y = value)) +
  xlab('Visits') +
  ylab('UCPCR mol/mmol')
#geom_point(aes(x = key, y = value)) +
#geom_smooth(aes(x = key, y = value), method = 'lm')



