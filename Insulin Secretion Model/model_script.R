################################################################################
##################### General Notes for the Dataset ############################
################################################################################
### - age_atdiag is the exact patient age in double form
### - AgeatDiagnosis is the patient age in int form (used)
### - In equations where AGE is used, AgeatDiag was used instead - need to fix!
################################################################################
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
################################################################################
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

################################################################################
#################### Section for testing Model 1: ##############################
################################################################################
### - Clinical features model        
### - Only takes in AgeatDiag and BMI
### - Selection and model based around following equation:
### = 37.94 + (-5.09 * log(age at diagnosis)) + (-6.34 * log(BMI))
################################################################################
################################################################################

#Creating a new dataframe filtered by AgeatDiagnosis and Height/weight or BMI
data_with_heightweight = my_data[!is.na(my_data$Height) & !is.na(my_data$Weight),] #Includes by height/weight values
data_with_BMI = my_data[!is.na(my_data$BMI),] #includes by BMI values
model_data = rbind.data.frame(data_with_heightweight, data_with_BMI)
model_data = model_data[!is.na(model_data$AgeatDiagnosis),] #Filters by removing anyone without ageatdiag
model_data = my_data
View(model_data)

#Fill in any missing values for BMI
model_data$BMI[is.na(model_data$BMI)] = model_data$Weight[is.na(model_data$BMI)] / (model_data$Height[is.na(model_data$BMI)]^2) 

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
################# Comparing 2 Models against antibodies ########################
################################################################################
### - Clin features model
### - Clin features + autoantibodies (GAD IA2)
### - compared to using GAD and IA2
################################################################################

#General descriptive stats about non-insulin group
model_data%>%
  filter(Initial_diabetes_Insulin != "Insulin") %>%
  nrow()

model_data %>%
  filter(Initial_diabetes_Insulin != "Insulin") %>%
  select(GRS) %>%
  describe()

#creating new dataframe for use 
not_insulin = model_data %>%
  filter(Initial_diabetes_Insulin != "Insulin")

# visualization
not_insulin %>%
  filter(!is.na(Model4Prob) & !is.na(Model1Prob)) %>%
  select(Model1Prob, Model4Prob) %>%
  rename("Clin Features Model" = "Model1Prob") %>%
  rename("Clin + Antibody Model" = "Model4Prob") %>%
  gather() %>%
  ggplot2::ggplot() +
  geom_violin(aes(x = key, y = value)) +
  xlab('Models') +
  ylab('Probability') +
  geom_smooth(aes(x = key, y = value), method = 'lm')

################################## MODEL 1 #####################################  
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



################################## MODEL 2 #####################################  
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


################################## MODEL 3 #####################################  
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


#Investigating group who switched to insulin
patients_switched = not_insulin %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  select(BMI, GADA_Status, IA2A_Status, AgeatDiagnosis, HbA1c_at_diagnosis, Model1Prob, Model4Prob, Insulin, Time_to_insulin,
         Type_of_diabetes, V2Insulin, V3Insulin)

nrow(patients_switched)
describe(patients_switched)
#How many were ONE antibody+
patients_switched %>%
  filter(GADA_Status == 1 | IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody +
patients_switched %>%
  filter(GADA_Status == 1 & IA2A_Status == 1) %>%
  nrow()
# How many were BOTH antibody -
patients_switched %>%
  filter(GADA_Status == 0 & IA2A_Status == 0) %>%
  nrow()
patients_switched%>%
  filter(Type_of_diabetes == "Type 1") %>%
  nrow()
patients_switched%>%
  filter(Type_of_diabetes == "Type 2") %>%
  nrow()
describe(as.numeric(patients_switched$Time_to_insulin))
patients_switched %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  nrow()


switched_list = list('SR3071', 'SR3083', 'SR3152', 'SR3156', 'SR3157')
#Investigating individuals who switched to insulin
not_insulin %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  select(StartRightID)

individual = not_insulin %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  filter(StartRightID == switched_list[5]) %>%
  select(BMI, AgeatDiagnosis, Time_to_insulin, HbA1c_at_diagnosis, GADA_Status,
         IA2A_Status, Model1Prob, Model4Prob, Type_of_diabetes)

describe(individual)
describe(as.numeric(individual$Time_to_insulin))
  




























































#plot slopes from anyone diagnosed as type 2
plot(type_2_group$Model4Prob, type_2_group$AvgAnnualRUCPCR)

#plot slopes from anyone high model risk
plot(type_2_group$Model4Prob[(type_2_group$Model4Prob > 0.6 & type_2_group$Model4Prob < 0.9) ], type_2_group$AvgAnnualRUCPCR[(type_2_group$Model4Prob > 0.6 & type_2_group$Model4Prob < 0.9)])
  

library(lme4)

type_2_group$V1 = 0
type_2_group$V1[!is.na(type_2_group$UCPCR)] = 1 

type_2_group$V2 = 0 
type_2_group$V2[!is.na(type_2_group$V2UCPCR)] = 1 

type_2_group$V3 = 0 
type_2_group$V3[!is.na(type_2_group$V3UCPCR)] = 1 

model_data$V1 = 0
model_data$V1[!is.na(type_2_group$UCPCR)] = 1 

model_data$V2 = 0 
model_data$V2[!is.na(type_2_group$V2UCPCR)] = 1 

model_data$V3 = 0 
model_data$V3[!is.na(type_2_group$V3UCPCR)] = 1 


#type_2 dataset
type_2 = type_2_group %>%
  select(AvgAnnualRUCPCR, V1, V2, V3, Time_to_insulin) %>%
  filter(!is.na(AvgAnnualRUCPCR) & !is.na(Time_to_insulin))

plot(type_2$Time_to_insulin, type_2$AvgAnnualRUCPCR)
type_2_lm = lm(unlist(AvgAnnualRUCPCR) ~ Time_to_insulin + (1|V1|V2|V3), data = type_2)

#Full dataset
type_1 = model_data %>%
  select(AvgAnnualRUCPCR, V1, V2, V3, Time_to_insulin, Type_of_diabetes) %>%
  filter(!is.na(AvgAnnualRUCPCR) & !is.na(Time_to_insulin) & Type_of_diabetes == "Type 1")

plot(type_1$Time_to_insulin, type_1$AvgAnnualRUCPCR)
type_1_lm = lm(unlist(AvgAnnualRUCPCR) ~ Time_to_insulin + (1|V1|V2|V3), data = type_1)
  

#VISUALIZE
ggplot(type_1,aes(y=AvgAnnualRUCPCR,x=Time_to_insulin))+geom_point()+geom_smooth(method="lm")
ggplot(type_2,aes(y=AvgAnnualRUCPCR,x=Time_to_insulin))+geom_point()+geom_smooth(method="lm")

#new_data = cbind.data.frame(type_2_group$AvgAnnualRUCPCR[!is.na(type_2_group$AvgAnnualRUCPCR & !is.na(type_2_group$Gender))], type_2_group$Gender[!is.na(type_2_group$AvgAnnualRUCPCR & !is.na(type_2_group$Gender))])
#lmer(new_data$`type_2_group$AvgAnnualRUCPCR[!is.na(type_2_group$AvgAnnualRUCPCR & ` ~ new_data$`type_2_group$Gender[!is.na(type_2_group$AvgAnnualRUCPCR & !is.na(type_2_group$Gender))]` + (1 | new_data$`type_2_group$Gender[!is.na(type_2_group$AvgAnnualRUCPCR & !is.na(type_2_group$Gender))]`))



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

type_2_group_a_positive %>%
  select(UCPCR, V2UCPCR, V3UCPCR) %>%
  rename("Visit 1" = "UCPCR") %>%
  rename("Visit 2" = "V2UCPCR") %>%
  rename("Visit 3" = "V3UCPCR") %>%
  gather() %>%
  filter(value < 20) %>%
  # mutate(key = as.numeric(key)) %>%
  ggplot2::ggplot() +
  #geom_violin(aes(x = key, y = value)) +
  geom_boxplot(aes(x = key, y = value)) +
  xlab('Visits') +
  ylab('UCPCR mol/mmol')
  #geom_point(aes(x = key, y = value)) +
  #geom_smooth(aes(x = key, y = value), method = 'lm')

type_2_group_a_negative %>%
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

################################################################################
########################### Visualise Results ##################################
################################################################################
#See how many people in dataset are on insulin
(table(model_data$Insulin))
barplot(table(model_data$Insulin), 
        xlab = 'Patient on Insulin', 
        ylab = 'Frequency',
        main = 'Bar Chart of StartRight dataset on Insulin')

#Reset plot
dev.off(dev.list()["RStudioGD"]) #Clears any previous dev plotting settings
par(mfrow = c(4,1)) #Allows us to visualise 4 plots at once

#Histograms for prediction probabilities
hist(model_data$Model1Prob,
     xlab = 'Age + BMI Probability',
     main = 'Histogram of Age + BMI Prediction Model')
hist(model_data$Model2Prob,
     xlab = 'Age, BMI and GAD Probability',
     main = 'Histogram of Age, BMI and GAD Prediction Model')
hist(model_data$Model3Prob,
     xlab = 'Age, BMI and ZNT8 Probability',
     main = 'Histogram of Age, BMI and ZNT8 Prediction Model')
hist(model_data$Model4Prob,
     xlab = 'Age, BMI and Auto-Antibody (GAD + ZNT8) Probability',
     main = 'Histogram of Age, BMI and Auto-Antibody Prediction Model')

#avg avg rate
#take avg annual rate of change then average if patient >=2 visits



################################################################################
################################# UCPCR ########################################
################################################################################
### - Measuring annual C-peptide change
### - Plotting prediction models against C-peptide change
################################################################################

par(mfrow = c(2,1)) #Allows us to visualise 4 plots at once
describe(model_data$V1_V2_UCPCR_Change[model_data$Model4Prob < 0.1])
describe(model_data$V1_V2_UCPCR_Change[model_data$Model4Prob > 0.9])

#filter insulin treated - because deficiency deteriorates
#Insulin at diagnsosis
#does prediction model identify those who develop severe insulin deficiency
#severe insulin deficiency = <0.2 v3_UCPCR

#date_continuous needs to be >4 weeks since diagnosis/v1
(model_data$Date_continuous_insulin[model_data$V2Insulin == "Yes" & model_data$Insulin == "No"])
(model_data$Date_continuous_insulin[model_data$V3Insulin == "Yes" & model_data$Insulin == "No"])
(model_data$Date_continuous_insulin[model_data$Insulin == "No"])

#how many people on insulin at diagnosis, v1 v2
#mean time to insulin
#on insulin 

dev.off()

plot(model_data$Model1Prob[model_data$Insulin == "No"], model_data$V3UCPCR[model_data$Insulin == "Yes"])

hist(model_data$V1_V2_UCPCR_Change[model_data$Model4LogOR < 0.1])
hist(model_data$V1_V2_UCPCR_Change[model_data$Model4LogOR > 0.9])

