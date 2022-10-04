################################################################################
##################### General Notes for the Dataset ############################
################################################################################
### - age_atdiag is the exact patient age in double form
### - AgeatDiagnosis is the patient age in int form (used)
################################################################################
################################################################################
install.packages('pacman')
require('pacman')
p_load('rio')

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

#Fill in any missing values for BMI
model_data$BMI[is.na(model_data$BMI)] = model_data$Weight[is.na(model_data$BMI)] / (model_data$Height[is.na(model_data$BMI)]^2) 

#Add new column containing model prediction score
model_data$Model1Prediction = 37.94 + (-5.09 * log(model_data$AgeatDiagnosis)) + (-6.34 * log(model_data$BMI))

#Visualise the data below
plot(model_data$Model1Prediction, model_data$BMI)



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
model_data = model_data[!is.na(model_data$GADA_Status),] 

#Add new column containing model prediction score
model_data$Model2Prediction = 34.8057844720 + (-4.801441792 * log (model_data$AgeatDiagnosis)) + (-5.980577792 * log(model_data$BMI)) + (2.937107976 * model_data$GADA_Status)



################################################################################
#################### Section for testing Model 3: ##############################
################################################################################
### - Clinical features + ZNT8 model
### - Takes in AgeatDiag, BMI and ZNT8
### - Selection and model based around following equation, where ZNT8 = 0 if negative, 1 if positive
### = 37.26905033 + (3.194096 * Znt8† ) + (-5.047657308 * Log(Age)) + (-6.287258808 * Log(BMI))
################################################################################
################################################################################

CANT REASSIGN HERE! Reassigning loses model2 and model 1 data!!!
#Reassign model_data as some people without GAD but with ZNT8 may have been excluded
model_data = rbind.data.frame(data_with_heightweight, data_with_BMI)
model_data = model_data[!is.na(model_data$AgeatDiagnosis),] #Filters by removing anyone without ageatdiag

#Exclude anyone without ZNT8 data
model_data = model_data[!is.na(model_data$ZNT8_Status),]

#Add new column containing model prediction score
model_data$Model3Prediction = 37.26905033 + (3.194096 * model_data$ZNT8_Status ) + (-5.047657308 * log(model_data$AgeatDiagnosis)) + (-6.287258808 * log(model_data$BMI))
#Potential error here! Not sure if AgeatDiagnosis or current Age!!


#data_with_ZNT8 = my_data[!is.na(my_data$ZNT8),]


################################################################################
################################ Utility #######################################
################################################################################
(model_data$Model3Prediction)
(model_data$Model2)







