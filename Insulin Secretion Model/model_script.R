################################################################################
##################### General Notes for the Dataset ############################
################################################################################
### - age_atdiag is the exact patient age in double form
### - AgeatDiagnosis is the patient age in int form (used)
### - In equations where AGE is used, AgeatDiag was used instead - need to fix!
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
model_data = my_data
View(model_data)

#Fill in any missing values for BMI
model_data$BMI[is.na(model_data$BMI)] = model_data$Weight[is.na(model_data$BMI)] / (model_data$Height[is.na(model_data$BMI)]^2) 

#Add new column containing model prediction score
model_data$Model1LogOR = 37.94 + (-5.09 * log(model_data$AgeatDiagnosis)) + (-6.34 * log(model_data$BMI))

#Visualise the data below
plot(model_data$Model1LogOR, model_data$BMI)


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
### - Clinical features + ZNT8 model
### - Takes in AgeatDiag, BMI and ZNT8
### - Selection and model based around following equation, where ZNT8 = 0 if negative, 1 if positive
### = 37.26905033 + (3.194096 * Znt8† ) + (-5.047657308 * Log(Age)) + (-6.287258808 * Log(BMI))
################################################################################
################################################################################

#Exclude anyone without ZNT8 data
model_data$Model3LogOR[is.na(model_data$ZNT8_Status)] = NA

#Add new column containing model prediction score
model_data$Model3LogOR[!is.na(model_data$ZNT8_Status)] = 37.26905033 + (3.194096 * model_data$ZNT8_Status[!is.na(model_data$ZNT8_Status)]) + (-5.047657308 * log(model_data$AgeatDiagnosis[!is.na(model_data$ZNT8_Status)])) + (-6.287258808 * log(model_data$BMI[!is.na(model_data$ZNT8_Status)]))


################################################################################
#################### Section for testing Model 4: ##############################
################################################################################
### - Clinical features + GAD + ZNT8 model
### - Takes in AgeatDiag, BMI, GAD and ZNT8
### - Selection and model based around following equation, where ‡ AntiStatus1 = GADA positive only, AntiStatus2 = Znt8 positive only, AntiStatus3 = Both GADA and Znt8 positive. Please code all of these as 0=no and 1=yes
### = 33.49649577 + (-4.665598345 * Log(Age)) + (-5.81137397 * Log(BMI)) + (3.082366 * AntiStatus1‡) + (3.494462 * AntiStatus2‡) + (4.350717 * AntiStatus3‡)
################################################################################
################################################################################

#Exclude anyone without ZNT8 data OR without GADA data
model_data$Model4LogOR[is.na(model_data$ZNT8_Status) | is.na(model_data$GADA_Status)] = NA

#Create new column to see if patient is positive for both antibodies
model_data$Both_Status[is.na(model_data$ZNT8_Status) | is.na(model_data$GADA_Status)] = NA
model_data$Both_Status[!is.na(model_data$ZNT8_Status) & !is.na(model_data$GADA_Status)] = 0
model_data$Both_Status[model_data$ZNT8_Status == 1 & model_data$GADA_Status == 1] = 1

#Add new column containing model prediction score
model_data$Model4LogOR[!is.na(model_data$Both_Status)] = 33.49649577 + (-4.665598345 * log(model_data$AgeatDiagnosis[!is.na(model_data$Both_Status)])) + (-5.81137397 * log(model_data$BMI[!is.na(model_data$Both_Status)])) + (3.082366 * model_data$GADA_Status[!is.na(model_data$Both_Status)]) + (3.494462 * model_data$ZNT8_Status[!is.na(model_data$Both_Status)]) + (4.350717 * model_data$Both_Status[!is.na(model_data$Both_Status)])


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
############# Export results for Descriptive stats in Python ###################
################################################################################

write.csv(model_data$Model1Prob[!is.na(model_data$Model1Prob)], '~/Downloads/autoantibody_model_1_data.csv')
write.csv(model_data$Model2Prob[!is.na(model_data$Model2Prob)], '~/Downloads/autoantibody_model_2_data.csv')
write.csv(model_data$Model3Prob[!is.na(model_data$Model3Prob)], '~/Downloads/autoantibody_model_3_data.csv')
write.csv(model_data$Model4Prob[!is.na(model_data$Model4Prob)], '~/Downloads/autoantibody_model_4_data.csv')


################################################################################
################## Descriptive Statistics on groups ############################
################################################################################
### - Finding % in each decile
################################################################################

p_load(psych)
describe(model_data$Model1Prob[!is.na(model_data$Model2Prob)])


(nrow(model_data[model_data$Model1Prob <= 0.1,])/nrow(model_data))
(nrow(model_data[model_data$Model1Prob > 0.1 & model_data$Model1Prob <= 0.2,])/nrow(model_data))
(nrow(model_data[model_data$Model1Prob > 0.2 & model_data$Model1Prob <= 0.3,])/nrow(model_data))
(nrow(model_data[model_data$Model1Prob > 0.3 & model_data$Model1Prob <= 0.4,])/nrow(model_data))
(nrow(model_data[model_data$Model1Prob > 0.4 & model_data$Model1Prob <= 0.5,])/nrow(model_data))
(nrow(model_data[model_data$Model1Prob > 0.5 & model_data$Model1Prob <= 0.6,])/nrow(model_data))
(nrow(model_data[model_data$Model1Prob > 0.6 & model_data$Model1Prob <= 0.7,])/nrow(model_data))
(nrow(model_data[model_data$Model1Prob > 0.7 & model_data$Model1Prob <= 0.8,])/nrow(model_data))
(nrow(model_data[model_data$Model1Prob > 0.8 & model_data$Model1Prob <= 0.9,])/nrow(model_data))
(nrow(model_data[model_data$Model1Prob > 0.9 & model_data$Model1Prob <= 1.0,])/nrow(model_data))


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

# Calculate UCPCR Change from Diagnosis (V1) to V2
model_data$V1_V2_UCPCR_Change[!is.na(model_data$UCPCR) & !is.na(model_data$V2UCPCR)] = model_data$V2UCPCR[!is.na(model_data$UCPCR) & !is.na(model_data$V2UCPCR)] - model_data$UCPCR[!is.na(model_data$UCPCR) & !is.na(model_data$V2UCPCR)]

# Calculate UCPCR Change from V2 to V3
model_data$V2_V3_UCPCR_Change[!is.na(model_data$V2UCPCR) & !is.na(model_data$V3UCPCR)] = model_data$V3UCPCR[!is.na(model_data$V2UCPCR) & !is.na(model_data$V3UCPCR)] - model_data$V2UCPCR[!is.na(model_data$V2UCPCR) & !is.na(model_data$V3UCPCR)]

# Calculate annual avg rate of UCPCR Change for those who HAVE V1 -> V2 data
model_data$AvgAnnualRUCPCR[!is.na(model_data$V1_V2_UCPCR_Change)] = model_data$V1_V2_UCPCR_Change[!is.na(model_data$V1_V2_UCPCR_Change)] / 2

#Calculate annual avg rate of UCPCR change for those who HAVE V2 -> V3 data
model_data$AvgAnnualRUCPCR[!is.na(model_data$V2_V3_UCPCR_Change)] = model_data$V2_V3_UCPCR_Change[!is.na(model_data$V2_V3_UCPCR_Change)] / 2

#Calculate annual avg rate of UCPCR change for those who HAVE V1 -> V2 -> V3 data
model_data$AvgAnnualRUCPCR[!is.na(model_data$V1_V2_UCPCR_Change) & !is.na(model_data$V2_V3_UCPCR_Change)] = (model_data$V1_V2_UCPCR_Change[!is.na(model_data$V1_V2_UCPCR_Change) & !is.na(model_data$V2_V3_UCPCR_Change)] + model_data$V2_V3_UCPCR_Change[!is.na(model_data$V1_V2_UCPCR_Change) & !is.na(model_data$V2_V3_UCPCR_Change)]) / 3

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

