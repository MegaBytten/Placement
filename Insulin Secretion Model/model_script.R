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
##################### Calculating Time_to_insulin ##############################
################################################################################
#Creating Time_to_insulin variable
model_data$Time_to_insulin[model_data$Date_continuous_insulin != '' & model_data$DateofDiagnosis != ''] = difftime(as.Date(model_data$Date_continuous_insulin[model_data$Date_continuous_insulin != '' & model_data$DateofDiagnosis != ''], "%d-%B-%Y"), as.Date(model_data$DateofDiagnosis[model_data$Date_continuous_insulin != '' & model_data$DateofDiagnosis != ''], "%d-%B-%Y") , units = 'weeks')
model_data$Time_to_insulin[model_data$Time_to_insulin < 0] = 0





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
         

########## Running descriptive stats on Patients who PROGRESSED ################
# % of GAD, IA2, ZNT8 positive AND mean BMI AND mean AgeatDiag AND mean Time_to_insulin
progressed %>%
  describe()

# How many initially treated with insulin
progressed %>%
  filter(Initial_diabetes_Insulin != "Insulin") %>%
  nrow()

# Graph ethnicities
barplot( table(progressed$EthnicOrigin) )


######## Running descriptive stats on Patients who DIDNT progress ##############
# % of GAD, IA2, ZNT8 positive AND mean BMI AND mean AgeatDiag AND mean Time_to_insulin
remained %>%
  describe()

# How many initially treated with insulin
remained %>%
  filter(Initial_diabetes_Insulin != "Insulin") %>%
  nrow()

# Graph ethnicities
barplot( table(remained$EthnicOrigin) )

################ Calculating mean GRS of those ON insulin ######################
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


########################### Model 4 ROC Curve ##################################
#initialize new column as NA
not_insulin$progressed = NA

#If patients WERE  on insulin by V1, V2 or V3 then progressed = 1, otherwise 0
not_insulin = not_insulin %>%
  mutate(progressed = ifelse(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes",
                             1, 0))

roc_data = not_insulin %>%
  filter(!is.na(progressed) & !is.na(Model4Prob))

#Storing the logistic regression results
glm.fit = glm(roc_data$progressed ~ roc_data$Model4Prob, family = binomial())
plot.new()
lines(roc_data$Model4Prob, glm.fit$fitted.values)

roc(roc_data$progressed, glm.fit$fitted.values, auc=TRUE, plot=TRUE)


########################### Model 1 ROC Curve ##################################
#Storing the logistic regression results
glm.fit = glm(roc_data$progressed ~ roc_data$Model1Prob, family = binomial())
plot.new()
lines(roc_data$Model1Prob, glm.fit$fitted.values)

roc(roc_data$progressed, glm.fit$fitted.values, auc=TRUE, plot=TRUE)

TODO:
  need help troubleshooting this
  Applying a binary numeric vector to a binary outcome?? does it work?
########################### Antibodies ROC Curve ##################################
# Create new variable if patients were antibody positive or not
not_insulin$antibody_positive = NA

#If patients WERE antibody+ then antibody_positive = 1, otherwise 0
roc_data = not_insulin %>%
  mutate(antibody_positive = ifelse(GADA_Status == "1" | IA2A_Status == "1",
                             1, 0))
#Filter NA values
roc_data = roc_data %>%
  filter(!is.na(progressed) & !is.na(antibody_positive))

#Storing the logistic regression results
glm.fit = glm(roc_data$progressed ~ roc_data$antibody_positive, family = binomial())
plot.new()
lines(roc_data$progressed, glm.fit$fitted.values)

roc(roc_data$progressed, glm.fit$fitted.values, auc=TRUE, plot=TRUE)


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

############# Clin Features + Antibody Model Calibration #######################
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
     main = 'Clinical Features + Antibody Model',
     xlab = 'Model Probability',
     ylab = '% progressed to insulin')


######################## Clin Features  Calibration ############################
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
     main = 'Clinical Features Model',
     xlab = 'Model Probability',
     ylab = '% progressed to insulin')



################################################################################
################################ FUTURE ########################################
################################################################################

################################################################################
####################### Investigating individuals ##############################
################################################################################
### - Looking at individual-level data for those who switched to insulin
################################################################################

#See how many people in dataset are on insulin
not_insulin %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  select(StartRightID)

switched_list = list('SR3071', 'SR3083', 'SR3152', 'SR3156', 'SR3157') #based on previous filtering and selection

individual = not_insulin %>%
  filter(Insulin == "Yes" | V2Insulin == "Yes" | V3Insulin == "Yes") %>%
  filter(StartRightID == switched_list[5]) %>%
  select(BMI, AgeatDiagnosis, Time_to_insulin, HbA1c_at_diagnosis, GADA_Status,
         IA2A_Status, Model1Prob, Model4Prob, Type_of_diabetes)

describe(individual)
describe(as.numeric(individual$Time_to_insulin))




################################################################################
############################# Util Functions ###################################
################################################################################

#Reset plot
dev.off(dev.list()["RStudioGD"]) #Clears any previous dev plotting settings
par(mfrow = c(4,1)) #Allows us to visualise 4 plots at once

