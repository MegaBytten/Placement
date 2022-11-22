################################################################################
##################### General Notes for the Dataset ############################
################################################################################
### - age_atdiag is the exact patient age in double form
### - AgeatDiagnosis is the patient age in int form (used)
### - In equations where AGE is used, AgeatDiag was used instead - need to fix!
################################################################################
################################################################################
library(tidyverse) # streamlining R coding since 2016
library(rio) # Import/Output master package
library(psych) # Essential for descriptive stats
library(pROC) # ROC curves and such.
library (rsq) # Used for R-squared values
library(val.prob) # Out dated for this version of R

filepath = '~/Downloads/Insulin secretion model/startight Aug 22 comma separated.csv'
my_data <- import(filepath)
View(my_data)
################################################################################
################ Section for Cleaning and preparing Data: ######################
################################################################################
### - no current clean GAD, ZNT8 code
### - This section creates new columns to be used in the models
################################################################################
#Islet autoantibodies were considered positive if: 
#GADA ≥11 units/mL, IA2A ≥7.5 units/mL and ZNT8A ≥65 units/mL in those aged up to 30 years 
#and ≥10 units/mL in those aged ≥30 years (17, 18)

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

#Fixing BMI
my_data = my_data %>%
  mutate(BMI = ifelse(is.na(BMI), Weight / (Height^2), BMI))
################################################################################
####################### Correcting GRS Scores ##################################
################################################################################
### - Current GRS scores in startright are case, not controlled
### - Need to fit GRS scores for individuals against a type-1 dataset to see
###   which centile our individuals would score
################################################################################
my_data = my_data %>%
  mutate(Number_Antibodies = as.numeric(GADA_Status + IA2A_Status + ZNT8_Status))

type_1_control_cohort = my_data %>%
  filter(Insulin == "Yes" & Number_Antibodies >= 2 & Type_of_diabetes == "Type 1") %>%
  filter(SCORE != '' & !is.na(SCORE)) # n = 456

# Calculating calibrated GRS centiles using z-score pnorm() ####
my_data$Calibrated_GRS_Centiles = pnorm(my_data$SCORE, mean = mean(type_1_control_cohort$SCORE), sd = sd(type_1_control_cohort$SCORE))
################################################################################
#################### Section for testing Model 1: ##############################
################################################################################
### - Clinical features model        
### - Only takes in AgeatDiag and BMI
### - Selection and model based around following equation:
### = 37.94 + (-5.09 * log(age at diagnosis)) + (-6.34 * log(BMI))
################################################################################
################################################################################
# Creating new data frame: model_data for use in models! ####
model_data = my_data %>%
  filter( (!is.na(my_data$Height) & !is.na(my_data$Weight)) | !is.na(my_data$BMI) )
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
#Exclude anyone without GAD data
model_data$Model2LogOR[is.na(model_data$GADA_Status)] = NA

#Add new column containing model prediction score
model_data$Model2LogOR[!is.na(model_data$GADA_Status)] = 34.8057844720 + (-4.801441792 * log (model_data$AgeatDiagnosis[!is.na(model_data$GADA_Status)])) + (-5.980577792 * log(model_data$BMI[!is.na(model_data$GADA_Status)])) + (2.937107976 * model_data$GADA_Status[!is.na(model_data$GADA_Status)])
################################################################################
#################### Section for testing Model 3: ##############################
################################################################################
### - Clinical features + IA2 model
### - Takes in AgeatDiag, BMI and IA2
### - Selection and model based around following equation, where IA2 = 0 if negative, 1 if positive
### = 37.26905033 + (3.194096 * IA2† ) + (-5.047657308 * Log(Age)) + (-6.287258808 * Log(BMI))
################################################################################
################################################################################
#Exclude anyone without IA2 data
model_data$Model3LogOR[is.na(model_data$IA2A_Status)] = NA

#Add new column containing model prediction score
model_data$Model3LogOR[!is.na(model_data$IA2A_Status)] = 37.26905033 + (3.194096 * model_data$IA2A_Status[!is.na(model_data$IA2A_Status)]) + (-5.047657308 * log(model_data$AgeatDiagnosis[!is.na(model_data$IA2A_Status)])) + (-6.287258808 * log(model_data$BMI[!is.na(model_data$IA2A_Status)]))
################################################################################
################################################################################
#################### Section for testing Model 4: ##############################
################################################################################
### - Clinical features + GAD + ZNT8 model
### - Takes in AgeatDiag, BMI, GAD and IA2
### - Selection and model based around following equation, where ‡ AntiStatus1 = GADA positive only, AntiStatus2 = Znt8 positive only, AntiStatus3 = Both GADA and Znt8 positive. Please code all of these as 0=no and 1=yes
### = 33.49649577 + (-4.665598345 * Log(Age)) + (-5.81137397 * Log(BMI)) + (3.082366 * AntiStatus1‡) + (3.494462 * AntiStatus2‡) + (4.350717 * AntiStatus3‡)
################################################################################
################################################################################

#Exclude anyone without ZNT8 data OR without GADA data
model_data$Model4LogOR[is.na(model_data$IA2A_Status) | is.na(model_data$GADA_Status)] = NA

#Create new column to see if patient is positive for both antibodies
model_data$Both_Status[is.na(model_data$IA2A_Status) | is.na(model_data$GADA_Status)] = NA
model_data$Both_Status[!is.na(model_data$IA2A_Status) & !is.na(model_data$GADA_Status)] = 0
model_data$Both_Status[model_data$IA2A_Status == 1 & model_data$GADA_Status == 1] = 1

#Add new column containing model prediction score
model_data$Model4LogOR[!is.na(model_data$Both_Status)] = 33.49649577 + (-4.665598345 * log(model_data$AgeatDiagnosis[!is.na(model_data$Both_Status)])) + (-5.81137397 * log(model_data$BMI[!is.na(model_data$Both_Status)])) + (3.082366 * model_data$GADA_Status[!is.na(model_data$Both_Status)]) + (3.494462 * model_data$IA2A_Status[!is.na(model_data$Both_Status)]) + (4.350717 * model_data$Both_Status[!is.na(model_data$Both_Status)])
################################################################################
#################### Section for testing Model 5: ##############################
################################################################################
### - Clinical features + Autoantibodies + GRS Model
### - Selection and model based around following equation
### = 21.57649882 + (-4.086215772 * log(Age)) + (-5.096252172 * log(BMI)) + (2.702010666 * [GAD POSITIVE ONLY 1 OR 0]) + (3.063255174 * [IA2 POSITIVE ONLY 1 OR 0]) + (3.813850704 * [BOTH GAD AND IA2 POSITIVE 1 OR 0]) + (30.11052 * GRS)
################################################################################
#Need to establish if patients are GAD positive only, IA2 positive only, or both positive
model_data$AntiStatus1 = NA #GAD positive only
model_data = model_data %>%
  mutate(AntiStatus1 = ifelse(GADA_Status == 1 & IA2A_Status == 0, 1, 0))

model_data$AntiStatus2 = NA #IA2 positive only
model_data = model_data %>%
  mutate(AntiStatus2 = ifelse(GADA_Status == 0 & IA2A_Status == 1, 1, 0))

model_data$AntiStatus3 = NA #Both positive
model_data = model_data %>%
  mutate(AntiStatus3 = ifelse(GADA_Status == 1 & IA2A_Status == 1, 1, 0))

#Calculate LogOddsRatio
model_data$Model5LogOR = NA
model_data = model_data %>%
  mutate(Model5LogOR = 21.57649882 + (-4.086215772 * log(AgeatDiagnosis)) + (-5.096252172 * log(BMI)) + (2.702010666 * AntiStatus1) + (3.063255174 * AntiStatus2) + (3.813850704 * AntiStatus3) + (30.11052 * Calibrated_GRS_Centiles/100), NA)
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

#Calculate model's probability: GRS MODEL
model_data = model_data %>%
  mutate(Model5Prob = ifelse(!is.na(Model5LogOR), exp(Model5LogOR) / (1 + exp(Model5LogOR)), NA))
################################################################################
##################### Calculating Time_to_insulin ##############################
################################################################################
#Creating Time_to_insulin variable
model_data$Time_to_insulin[model_data$Date_continuous_insulin != '' & model_data$DateofDiagnosis != ''] = difftime(as.Date(model_data$Date_continuous_insulin[model_data$Date_continuous_insulin != '' & model_data$DateofDiagnosis != ''], "%d-%B-%Y"), as.Date(model_data$DateofDiagnosis[model_data$Date_continuous_insulin != '' & model_data$DateofDiagnosis != ''], "%d-%B-%Y") , units = 'weeks')
model_data$Time_to_insulin[model_data$Time_to_insulin < 0] = 0
################################################################################
################### Descriptive Stats on Not Insulin ###########################
################################################################################
### - Segregated into 3 groups: progressed, didnt progress and total
### - Age at Diag, BMI, HbA1c at Diag, 
################################################################################
# Assigning/Creating new not_insulin dataframe ####
not_insulin = model_data %>%
  filter(Initial_diabetes_Insulin != "Insulin") %>%
  filter(!is.na(Model1Prob) & !is.na(Model2Prob) & !is.na(Model3Prob) & !is.na(Model4Prob) & !is.na(Model5Prob) & !is.na(Calibrated_GRS_Centiles) )
# Total Non-insulin group ###################################
# n =
nrow(not_insulin)

#General descriptive stats on GRS, BMI, Age, etc.
not_insulin %>%
  select(GRS, AgeatDiagnosis, BMI, HbA1c_at_diagnosis) %>%
  describe()

# How many were single antibody positive (GAD or IA2)
not_insulin %>%
  filter(GADA_Status == 1 & IA2A_Status == 0) %>%
  nrow()
not_insulin %>%
  filter(GADA_Status == 0 & IA2A_Status == 1) %>%
  nrow()

# How many were double antibody positive (GAD AND IA2)
not_insulin %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()

# How many were double antibody negative (GAD- AND IA2- )
not_insulin %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
  nrow()

# Progressed Non-insulin group ################################
# n =
not_insulin %>%
  filter(Insulin == 'Yes' | V2Insulin == 'Yes' | V3Insulin == 'Yes') %>%
  nrow()

#General descriptive stats on GRS, BMI, Age, etc.
not_insulin %>%
  filter(Insulin == 'Yes' | V2Insulin == 'Yes' | V3Insulin == 'Yes') %>%
  select(GRS, AgeatDiagnosis, BMI, HbA1c_at_diagnosis) %>%
  describe()

# How many were single antibody positive (GAD or IA2)
not_insulin %>%
  filter(Insulin == 'Yes' | V2Insulin == 'Yes' | V3Insulin == 'Yes') %>%
  filter(GADA_Status == 1 & IA2A_Status == 0) %>%
  nrow()
not_insulin %>%
  filter(Insulin == 'Yes' | V2Insulin == 'Yes' | V3Insulin == 'Yes') %>%
  filter(GADA_Status == 0 & IA2A_Status == 1) %>%
  nrow()

# How many were double antibody positive (GAD AND IA2)
not_insulin %>%
  filter(Insulin == 'Yes' | V2Insulin == 'Yes' | V3Insulin == 'Yes') %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()

# How many were double antibody negative (GAD- AND IA2- )
not_insulin %>%
  filter(Insulin == 'Yes' | V2Insulin == 'Yes' | V3Insulin == 'Yes') %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
  nrow()

# NO-progress Non-insulin group ###############################
# n =
not_insulin %>%
  filter(V3Insulin == 'No') %>%
  nrow()

#General descriptive stats on GRS, BMI, Age, etc.
not_insulin %>%
  filter(V3Insulin == 'No') %>%
  select(GRS, AgeatDiagnosis, BMI, HbA1c_at_diagnosis) %>%
  describe()

# How many were single antibody positive (GAD or IA2)
not_insulin %>%
  filter(V3Insulin == 'No') %>%
  filter(GADA_Status == 1 & IA2A_Status == 0) %>%
  nrow()
not_insulin %>%
  filter(V3Insulin == 'No') %>%
  filter(GADA_Status == 0 & IA2A_Status == 1) %>%
  nrow()

# How many were double antibody positive (GAD AND IA2)
not_insulin %>%
  filter(V3Insulin == 'No') %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()

# How many were double antibody negative (GAD- AND IA2- )
not_insulin %>%
  filter(V3Insulin == 'No') %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
  nrow()




################################################################################
################ Graphing Violion models against eachother #####################
################################################################################
### - Clin features model
### - Clin features + autoantibodies (GAD IA2)
### - compared using gg VIOLIN Plot
################################################################################
not_insulin %>%
  filter(!is.na(Model4Prob) & !is.na(Model1Prob) & !is.na(Model5Prob)) %>%
  select(Model1Prob, Model4Prob, Model5Prob) %>%
  rename("Clin Features Model" = "Model1Prob") %>%
  rename("Clin + Antibody Model" = "Model4Prob") %>%
  rename("Clin + Anti + GRS Model" = "Model5Prob") %>%
  gather() %>%
  ggplot2::ggplot() +
  geom_violin(aes(x = key, y = value)) +
  xlab('Models') +
  ylab('Probability') +
  geom_smooth(aes(x = key, y = value), method = 'lm')
################################################################################
################# Comparing 2 Models against antibodies ########################
################################################################################
### - Clin features model
### - Clin features + autoantibodies (GAD IA2)
### - compared to using GAD and IA2
################################################################################
# MODEL 1 #####################################  
#calculate how many patients progressed to insulin >0.5 prob
not_insulin %>% 
  filter(Model1Prob > 0.5) %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()

#descriptive stats for High Risk group
high_risk = not_insulin %>%
  filter(Model1Prob >= 0.667) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model1Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(high_risk)
describe(high_risk)
#How many were ONE antibody+
high_risk %>%
  filter(GADA_Status == 1 | IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody +
high_risk %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody -
high_risk %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
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
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()


#descriptive stats for Mid Risk group
mid_risk = not_insulin %>%
  filter(Model1Prob < 0.667 & Model1Prob > 0.333) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model1Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(mid_risk)
describe(mid_risk)
#How many were ONE antibody+
mid_risk %>%
  filter(GADA_Status == 1 | IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody +
mid_risk %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody -
mid_risk %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
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
# Calculate how many switched to insulin
mid_risk %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()


#descriptive stats for Low Risk group
low_risk = not_insulin %>%
  filter(Model1Prob <= 0.333) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model1Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(low_risk)
describe(low_risk)
#How many were ONE antibody+
low_risk %>%
  filter(GADA_Status == 1 | IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody +
low_risk %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody -
low_risk %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
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
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()



# MODEL 2 #####################################
#calculate how many patients progressed to insulin >0.5 prob
not_insulin %>% 
  filter(Model4Prob > 0.5) %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()

#descriptive stats for High Risk group
high_risk = not_insulin %>%
  filter(Model4Prob >= 0.667) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model4Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(high_risk)
describe(high_risk)
#How many were ONE antibody+
high_risk %>%
  filter(GADA_Status == 1 | IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody +
high_risk %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody -
high_risk %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
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
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()


#descriptive stats for Mid Risk group
mid_risk = not_insulin %>%
  filter(Model4Prob < 0.667 & Model1Prob > 0.333) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model4Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(mid_risk)
describe(mid_risk)
#How many were ONE antibody+
mid_risk %>%
  filter(GADA_Status == 1 | IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody +
mid_risk %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody -
mid_risk %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
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
# Calculate how many switched to insulin
mid_risk %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()


#descriptive stats for Low Risk group
low_risk = not_insulin %>%
  filter(Model4Prob <= 0.333) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model4Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(low_risk)
describe(low_risk)
#How many were ONE antibody+
low_risk %>%
  filter(GADA_Status == 1 | IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody +
low_risk %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody -
low_risk %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
  nrow()
low_risk%>%
  filter(Type_of_diabetes == "Type 1") %>%
  nrow()
low_risk%>%
  filter(Type_of_diabetes == "Type 2") %>%
  nrow()
describe(as.numeric(low_risk$Time_to_insulin))
low_risk %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()


# Antibodies #####################################  
#AUTO ANTIBODIES
#descriptive stats for High Risk group
positive = not_insulin %>%
  filter(GADA_Status == 1 | IA2A_Status == 1) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model4Prob, Model1Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(positive)
describe(positive)
positive%>%
  filter(Type_of_diabetes == "Type 1") %>%
  nrow()
positive%>%
  filter(Type_of_diabetes == "Type 2") %>%
  nrow()
describe(as.numeric(positive$Time_to_insulin))
#calculate how many switched to insulin
positive %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()


#descriptive stats for Mid Risk group
negative = not_insulin %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model4Prob, Model1Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)
nrow(negative)
describe(negative)
negative%>%
  filter(Type_of_diabetes == "Type 1") %>%
  nrow()
negative%>%
  filter(Type_of_diabetes == "Type 2") %>%
  nrow()
describe(as.numeric(negative$Time_to_insulin))
#calculate how many switched to insulin
negative %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()
################################################################################
################# Non-insulin low-risk antibody+ patients ######################
################################################################################
### - From previous results and descriptive stats:
###    > Clin features + antibody model was very good at discriminating antibody
###      positive patients who were not on insulin
###    > Model also calibrated some individuals who WERE antibody positive that 
###      were placed in low risk
### - Therefore, investigate these indiviudals and see what treatment their GP
###   prescribed
###    > is a heavier weight/emphasis placed on antibodies clinically?
###    > How do their GRS (only unbiased Type1/2 discriminating factor) compare?
################################################################################

low_risk_antibody_positive = not_insulin %>%
  filter(GADA_Status == 1 | IA2A_Status == 1) %>%
  filter(Model4Prob < 0.333) %>%
  select(BMI, GADA_Status, IA2A_Status, ZNT8_Status, AgeatDiagnosis,
         Initial_diabetes_Insulin, Type_of_diabetes, EthnicOrigin,
         Insulin, V2Insulin, V3Insulin, Time_to_insulin, HbA1c_at_diagnosis,
         GRS)

progressed = low_risk_antibody_positive %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes")
  
remained = low_risk_antibody_positive %>%
  filter(Insulin != "Yes" & V2Insulin != "Yes" & V3Insulin != "Yes")
         

# Running descriptive stats on Patients who PROGRESSED ################
# % of GAD, IA2, ZNT8 positive AND mean BMI AND mean AgeatDiag AND mean Time_to_insulin
progressed %>%
  describe()

# How many initially treated with insulin
progressed %>%
  filter(Initial_diabetes_Insulin != "Insulin") %>%
  nrow()

# Graph ethnicities
barplot( table(progressed$EthnicOrigin) )


# Running descriptive stats on Patients who DIDNT progress ##############
# % of GAD, IA2, ZNT8 positive AND mean BMI AND mean AgeatDiag AND mean Time_to_insulin
remained %>%
  describe()

# How many initially treated with insulin
remained %>%
  filter(Initial_diabetes_Insulin != "Insulin") %>%
  nrow()

# Graph ethnicities
barplot( table(remained$EthnicOrigin) )

# Calculating mean GRS of those ON insulin ######################
model_data %>%
  filter(Initial_diabetes_Insulin == "Insulin") %>%
  select(GRS) %>%
  describe()
################################################################################
################################ ROC Curves ####################################
################################################################################
### - Calculate new binary variable:
###    > 1 if patients PROGRESSED to insulin (V1, V2, V3 w/ Insulin)
###    > 0 if patients did NOT progress to insulin (V3 without Insulin)
### - Plot ROC curve for Clin features + antibody model against progressin (1|0)
### - Change antibody model from continuous to discrete = thresholds
###    > if modelProb > 0.6 = positive, else negative
###    > Allows testing of thresholds, which probability should mean 'positive'
################################################################################
# ROC Init ####################################
#If patients WERE  on insulin by V1, V2 or V3 then progressed = 1, otherwise 0
not_insulin = not_insulin %>%
  mutate(progressed = ifelse(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes",
                             1, 0))

roc_data = not_insulin %>%
  filter(!is.na(progressed) & !is.na(Model4Prob))

# Model 4 ROC Curve ##################################
#Storing the logistic regression results
glm.fit = glm(roc_data$progressed ~ roc_data$Model4Prob, family = binomial())
plot.new()
lines(roc_data$Model4Prob, glm.fit$fitted.values)

(roc = roc(roc_data$progressed, glm.fit$fitted.values, auc=TRUE, plot=TRUE))

#Getting threshold, sensitivity and specificity
coords(roc, x='best')


# Model 1 ROC Curve ##################################
#Storing the logistic regression results
glm.fit = glm(roc_data$progressed ~ roc_data$Model1Prob, family = binomial())
plot.new()
lines(roc_data$Model1Prob, glm.fit$fitted.values)

(roc = roc(roc_data$progressed, glm.fit$fitted.values, auc=TRUE, plot=TRUE))

#Getting threshold, sensitivity and specificity
coords(roc, x='best')

# Model 5 ROC Curve ##################################
#Storing the logistic regression results
roc_data = not_insulin %>%
  filter(!is.na(progressed) & !is.na(Model5Prob))

glm.fit = glm(roc_data$progressed ~ roc_data$Model5Prob, family = binomial())
plot.new()
lines(roc_data$Model5Prob, glm.fit$fitted.values)

(roc1 = roc(roc_data$progressed, glm.fit$fitted.values, auc=TRUE, plot=TRUE))

#Getting threshold, sensitivity and specificity
coords(roc1, x='best')
# P value for Mode 4 ROC vs Model 5 ROC ####
roc.test(roc, roc1)


# Num Antibodies ROC Curve ####
#If patients WERE  on insulin by V1, V2 or V3 then progressed = 1, otherwise 0
not_insulin = not_insulin %>%
  mutate(Number_Antibodies = as.numeric(GADA_Status + IA2A_Status + ZNT8_Status))


roc_data = not_insulin %>%
  filter(!is.na(progressed) & !is.na(Number_Antibodies))

#Storing the logistic regression results
glm.fit = glm(roc_data$progressed ~ roc_data$Number_Antibodies, family = binomial())
plot.new()
lines(roc_data$Model4Prob, glm.fit$fitted.values)

(roc1 = roc(roc_data$progressed, glm.fit$fitted.values, auc=TRUE, plot=TRUE))

#Getting threshold, sensitivity and specificity
coords(roc1, x='best')

# P value for Model 4 vs Number of Antibodies ####
roc.test(roc, roc1)
################################################################################
############################### Calibration ####################################
################################################################################
### - Calibrating the model to see how accurate it is
### - Break apart model probabilities into deciles
### - Test what % of people from each decile went on to have a positive outcome
###    > in this context what % of people went on to switch to insulin
###    > this tests how accurate the model is, if model predicts 10% do 10% of 
###     people switch within that decile?
################################################################################
# Clin Features + Antibody Model Calibration #######################
deciles = list(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
results = list()

for (x in 1:10){
  # Need to handle first and last number specially
  if (x == 1) {
    print('# Of patients progressed to Insulin within first Decile = ')
    total = not_insulin %>%
      filter(Model4Prob <= 0.1) %>%
      nrow()
    
    progressed = not_insulin %>%
      filter(Model4Prob <= 0.1 & progressed == 1) %>%
      nrow()
    results[1] = progressed/total
  # Need to handle first and last number specially
  } else if (x == 10){
    print('# Of patients progressed to Insulin within last Decile = ')
    total = not_insulin %>%
      filter(Model4Prob > 0.9) %>%
      nrow()
    
    progressed = not_insulin %>%
      filter(Model4Prob > 0.9 & progressed == 1) %>%
      nrow()
    results[10] = progressed / total
  # Index 2-9 = core loop
  } else {
    print(paste('# Of patients progressed to Insulin within Decile: ', x, ' = '))
    total = not_insulin %>%
      filter(Model4Prob > deciles[x-1] & Model4Prob <= deciles[x]) %>%
      nrow()
    
    progressed = not_insulin %>%
      filter(Model4Prob > deciles[x-1] & Model4Prob <= deciles[x] & progressed == 1) %>%
      nrow()
    
    results[x] = progressed / total
  }
}

plot(deciles, results,
     xlim = c(0,1),
     ylim = c(0,1),
     main = 'Clinical Features + Antibody Model',
     xlab = 'Model Probability',
     ylab = '% progressed to insulin')

#graph trendline and print equation
calibration_plot = lm(unlist(results) ~ unlist(deciles), data=data)
lines(unlist(deciles),
      predict(calibration_plot),
      col = 2,
      lwd = 2)
(paste("y =", coef(calibration_plot)[[1]], "+", coef(calibration_plot)[[2]], "* x"))

#calculate correlation coefficient
cor(unlist(results), unlist(deciles))

# Clin Features Calibration ############################
for (x in 1:10){
  # Need to handle first and last number specially
  if (x == 1) {
    print('# Of patients progressed to Insulin within first Decile = ')
    total = not_insulin %>%
      filter(Model1Prob <= 0.1) %>%
      nrow()
    
    progressed = not_insulin %>%
      filter(Model1Prob <= 0.1 & progressed == 1) %>%
      nrow()
    results[1] = progressed/total
    # Need to handle first and last number specially
  } else if (x == 10){
    print('# Of patients progressed to Insulin within last Decile = ')
    total = not_insulin %>%
      filter(Model1Prob > 0.9) %>%
      nrow()
    
    progressed = not_insulin %>%
      filter(Model1Prob > 0.9 & progressed == 1) %>%
      nrow()
    results[10] = progressed / total
    # Index 2-9 = core loop
  } else {
    print(paste('# Of patients progressed to Insulin within Decile: ', x, ' = '))
    total = not_insulin %>%
      filter(Model1Prob > deciles[x-1] & Model1Prob <= deciles[x]) %>%
      nrow()
    
    progressed = not_insulin %>%
      filter(Model1Prob > deciles[x-1] & Model1Prob <= deciles[x] & progressed == 1) %>%
      nrow()
    
    results[x] = progressed / total
  }
}

plot(deciles, results,
     xlim = c(0,1),
     ylim = c(0,1),
     main = 'Clinical Features Model',
     xlab = 'Model Probability',
     ylab = '% progressed to insulin')

#Grab r-Squared
summary(lm(unlist(results) ~ unlist(deciles), data=not_insulin))

#Caculate correlation coefficient
cor(unlist(results), unlist(deciles), method = "pearson")






# Clin F + Anti + GRS Calibration #########################
deciles = list(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
results = list()
for (x in 1:10){
  # Need to handle first and last number specially
  if (x == 1) {
    print('# Of patients progressed to Insulin within first Decile = ')
    total = not_insulin %>%
      filter(Model5Prob <= 0.1) %>%
      nrow()
    
    progressed = not_insulin %>%
      filter(Model5Prob <= 0.1 & progressed == 1) %>%
      nrow()
    results[1] = progressed/total
    # Need to handle first and last number specially
  } else if (x == 10){
    print('# Of patients progressed to Insulin within last Decile = ')
    total = not_insulin %>%
      filter(Model5Prob > 0.9) %>%
      nrow()
    
    progressed = not_insulin %>%
      filter(Model5Prob > 0.9 & progressed == 1) %>%
      nrow()
    results[10] = progressed / total
    # Index 2-9 = core loop
  } else {
    print(paste('# Of patients progressed to Insulin within Decile: ', x, ' = '))
    total = not_insulin %>%
      filter(Model5Prob > deciles[x-1] & Model5Prob <= deciles[x]) %>%
      nrow()
    
    progressed = not_insulin %>%
      filter(Model5Prob > deciles[x-1] & Model5Prob <= deciles[x] & progressed == 1) %>%
      nrow()
    
    results[x] = progressed / total
  }
}

plot(deciles, results,
     xlim = c(0,1),
     ylim = c(0,1),
     main = 'Clinical F + Anti + GRS Model',
     xlab = 'Model Probability',
     ylab = '% progressed to insulin')

#Grab r-Squared
summary(lm(unlist(results) ~ unlist(deciles), data=not_insulin))

calibration_plot = lm(unlist(results) ~ unlist(deciles), data=not_insulin)
lines(unlist(deciles),
      predict(calibration_plot),
      col = 2,
      lwd = 2)
(paste("y =", coef(calibration_plot)[[1]], "+", coef(calibration_plot)[[2]], "* x"))

################################################################################
################# Sub Investigation: Clin Features Model #######################
################ Able to Detect Antibody Positive Patients #####################
################################################################################
### - Clinical Model distinguishes pretty well for those who would be 
###   antibody positive
###    > Antibody positive OUT OF ALL 3 (GAD, IA2, ZNT8)
### - Therefore Clin model can be step 1 of 2 step process:
###    > Patient qualifies for antibody testing from Clinical Model
###    > Patient's antibody results come back and interpreted in second model
###    > future: create new model based on HOW MANY antibodies are positive
### - To test this:
###    > AUC ROC curve for Clinical Features model and outcome is antibody +
###    > Calibration plot for the same
################################################################################
# AUC ROC Curve for Antibody #################################
#Estabishing outcome = if ANY 3 antibody+ is positive outcome
not_insulin = not_insulin %>%
  mutate(antibody_positive = ifelse(GADA_Status == 1 | IA2A_Status == 1 | ZNT8_Status == 1, 1, 0))

#Estabishing outcome = if ALL 3 antibody- is negative outcome
not_insulin = not_insulin %>%
  mutate(antibody_positive = ifelse(GADA_Status == 0 & IA2A_Status == 0 & ZNT8_Status == 0, 0, 1))

#Filtering to ensure equal length
roc_data = not_insulin %>%
  filter(!is.na(antibody_positive) & !is.na(Model1Prob))

#Storing the logistic regression results
glm.fit = glm(roc_data$antibody_positive ~ roc_data$Model1Prob, family = binomial())
plot.new()
lines(roc_data$Model4Prob, glm.fit$fitted.values)

(roc = roc(roc_data$antibody_positive, glm.fit$fitted.values,
           #auc.polygon=TRUE,
           plot=TRUE,
           #smoothed=TRUE,
           #ci=TRUE,
           #ci.alpha = 0.9,
           #max.auc.polygon=TRUE,
           #print.auc=TRUE,
           #show.thres=TRUE
           ))

#Getting threshold, sensitivity and specificity
coords(roc, x='best')




# Clin Features Calibration #############################
deciles = list(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
results = list()

for (x in 1:10){
  # Need to handle first and last number specially
  if (x == 1) {
    print('# Of patients with Antibodies within first Decile = ')
    total = not_insulin %>%
      filter(Model1Prob <= 0.1) %>%
      nrow()
    
    antibody = not_insulin %>%
      filter(Model1Prob <= 0.1 & antibody_positive == 1) %>%
      nrow()
    results[1] = antibody/total
    # Need to handle first and last number specially
  } 
  else if (x == 10){
    print('# Of patients progressed to Insulin within last Decile = ')
    total = not_insulin %>%
      filter(Model1Prob > 0.9) %>%
      nrow()
    
    antibody = not_insulin %>%
      filter(Model1Prob > 0.9 & antibody_positive == 1) %>%
      nrow()
    results[10] = antibody / total
    # Index 2-9 = core loop
  } 
  else {
    print(paste('# Of patients antibody positive within Decile: ', x, ' = '))
    total = not_insulin %>%
      filter(Model1Prob > deciles[x-1] & Model1Prob <= deciles[x]) %>%
      nrow()
    
    antibody = not_insulin %>%
      filter(Model1Prob > deciles[x-1] & Model1Prob <= deciles[x] & antibody_positive == 1) %>%
      nrow()
    
    results[x] = antibody / total
  }
}

plot(deciles, results,
     xlim = c(0,1),
     ylim = c(0,1),
     main = 'Clinical Features Model',
     xlab = 'Model Probability',
     ylab = '% antibody positive')

#For r-squared value
summary(lm(unlist(results) ~ unlist(deciles), data=not_insulin))

#graph trendline and print equation
calibration_plot = lm(unlist(results) ~ unlist(deciles), data=not_insulin)
lines(unlist(deciles),
      predict(calibration_plot),
      col = 2,
      lwd = 2)
(paste("y =", coef(calibration_plot)[[1]], "+", coef(calibration_plot)[[2]], "* x"))


################################################################################
##################### Model 4 and 5 in Antibody+ ###############################
################################################################################
### - If those are already antibody positive, is there any use in model?
### - Run model in those antibody positive and see result
################################################################################
#Filtering for only those with antibodies
roc_data = not_insulin %>%
  filter(Number_Antibodies > 0)

#Descriptive Stats ########################################
nrow(roc_data)

#look at those who did progress
roc_data %>%
  filter(progressed == 0) %>%
  select(GRS, BMI, AgeatDiagnosis, HbA1c_at_diagnosis, Model4Prob) %>%
  describe()

roc_data %>%
  filter(progressed == 0) %>%
  nrow()

#ROC #########################################
roc_data = not_insulin %>%
  filter(!is.na(progressed) & !is.na(Model4Prob) & Number_Antibodies >= 1)

#Storing the logistic regression results
glm.fit = glm(roc_data$progressed ~ roc_data$Model4Prob, family = binomial())

#Graphing ROC
plot.new()
lines(roc_data$Model4Prob, glm.fit$fitted.values)
(roc = roc(roc_data$progressed, glm.fit$fitted.values, auc=TRUE, plot=TRUE))

#Getting threshold, sensitivity and specificity
coords(roc, x='best')

# Model 5 in Antibody Positive! ROC Curve ##################################
#Storing the logistic regression results
roc_data = not_insulin %>%
  filter(!is.na(progressed) & !is.na(Model5Prob) & Number_Antibodies >= 1)

glm.fit = glm(roc_data$progressed ~ roc_data$Model5Prob, family = binomial())
plot.new()
lines(roc_data$Model5Prob, glm.fit$fitted.values)

(roc1 = roc(roc_data$progressed, glm.fit$fitted.values, auc=TRUE, plot=TRUE))

#Getting threshold, sensitivity and specificity
coords(roc, x='best')

#COmpare two ROCS for p-value
roc.test(roc, roc1)

#Calibration ####################################
deciles = list(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
results = list()

for (x in 1:10){
  # Need to handle first and last number specially
  if (x == 1) {
    print('# Of patients progressed to Insulin within first Decile = ')
    total = roc_data %>%
      filter(Model4Prob <= 0.1) %>%
      nrow()
    
    progressed = roc_data %>%
      filter(Model4Prob <= 0.1 & progressed == 1) %>%
      nrow()
    results[1] = progressed/total
    # Need to handle first and last number specially
  } else if (x == 10){
    print('# Of patients progressed to Insulin within last Decile = ')
    total = roc_data %>%
      filter(Model4Prob > 0.9) %>%
      nrow()
    
    progressed = roc_data %>%
      filter(Model4Prob > 0.9 & progressed == 1) %>%
      nrow()
    results[10] = progressed / total
    # Index 2-9 = core loop
  } else {
    print(paste('# Of patients progressed to Insulin within Decile: ', x, ' = '))
    total = roc_data %>%
      filter(Model4Prob > deciles[x-1] & Model4Prob <= deciles[x]) %>%
      nrow()
    
    progressed = roc_data %>%
      filter(Model4Prob > deciles[x-1] & Model4Prob <= deciles[x] & progressed == 1) %>%
      nrow()
    
    results[x] = progressed / total
  }
}

plot(deciles, results,
     xlim = c(0,1),
     ylim = c(0,1),
     main = 'Clinical Features + Antibody Model',
     xlab = 'Model Probability',
     ylab = '% progressed to insulin')

#graph trendline and print equation
calibration_plot = lm(unlist(results) ~ unlist(deciles), data=roc_data)
lines(unlist(deciles),
      predict(calibration_plot),
      col = 2,
      lwd = 2)
(paste("y =", coef(calibration_plot)[[1]], "+", coef(calibration_plot)[[2]], "* x"))

#For r-squared value
summary(lm(unlist(results) ~ unlist(deciles), data=roc_data))

#Descriptive Stats for Calibration ############################
nrow(roc_data)

#How many progressed
roc_data %>%
  filter(progressed == 1) %>%
  nrow()

#How many were over threshold
roc_data %>%
  filter(Model4Prob >= 0.1159171 & progressed == 1) %>%
  nrow()

################################################################################
################## Finding Mean duration of follow up ##########################
################################################################################
### - How long was the average time from diagnosis to final follow up?
### - A LOT of date formatting :(
################################################################################
#Create new column
not_insulin$duration_follow_up = NA

#Binary variable if they have V3 data
not_insulin$HaveV3Data = NA
not_insulin = not_insulin %>%
  mutate(HaveV3Data = ifelse(V3Date_Contacted != '' & !is.na(V3Date_Contacted), 1, 0))

#Binary variable if they have V2 data
not_insulin$HaveV2Data = NA
not_insulin = not_insulin %>%
  mutate(HaveV2Data = ifelse(V2Date_Contacted != '' & !is.na(V2Date_Contacted), 1, 0))

#Calculating difference between dates for patients who have V3
V3_patients = not_insulin %>%
  filter(HaveV3Data == 1) %>%
  filter(DateofDiagnosis != '' & !is.na(DateofDiagnosis)) %>%
  mutate(duration_follow_up = difftime(
    as.Date(V3Date_Contacted, "%m/%d/%Y"),
    as.Date(DateofDiagnosis, "%d-%B-%y"), 
    units = 'weeks'), NA)

#Calculating difference between dates for patients who dont have V3 data
V2_patients = not_insulin %>%
  filter(HaveV3Data == 0 & HaveV2Data == 1) %>%
  filter(DateofDiagnosis != '' & !is.na(DateofDiagnosis)) %>%
  mutate(duration_follow_up = difftime(
    as.Date(V2Date_Contacted, "%d-%b-%y"),
    as.Date(DateofDiagnosis, "%d-%B-%y"), 
    units = 'weeks'), NA)

updated_not_insulin = rbind.data.frame(V3_patients, V2_patients)
updated_not_insulin %>%
  mutate(duration_follow_up = as.numeric(duration_follow_up)) %>%
  select(duration_follow_up) %>%
  describe()





###############Temp workspace ##################################################
hist(not_insulin$Model5Prob)
nrow(not_insulin)


     