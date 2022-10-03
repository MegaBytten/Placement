install.packages('pacman')
require('pacman')
p_load('rio')

my_data <- import("~/Downloads/MODY Interview Task.xlsx")
View(my_data)


#Splitting data group into 2, insulin treatment before/after 6 months
before_data = my_data[my_data$TTI == '<6m',]
View(before_data)
after_data = my_data[my_data$TTI == '>6m' | my_data$TTI == 'Not insulin treated' ,]
View(after_data)

################################################################################
################### Calculation section for before_data ########################
################################################################################
#Based on the following equation:
#

#Fill in HbA1c % data using hba1c mmol/mol
before_data$HbA1c[is.na(before_data$HbA1c)] = (0.0915*before_data$`HbA1c(mmol/mol)`[is.na(before_data$HbA1c)])+2.15

#Create a new column named 'logOR' for the Log Odds Ratio for <6 month patients
before_data['logOR'] = 1.8196 + (3.1404* ifelse(before_data$ParDM == 'Y', 1, 0)) - (0.0829*before_data$Age) - (0.6598*before_data$HbA1c) + (0.1011*before_data$Agediag) + (1.3131* ifelse(before_data$Sex == "M", 1, 2) )

#Create a new column named 'prob' for the Probability using the following equation:
#Probability = exp(LogOR) / (1 + exp(LogOR))
before_data['prob'] = (exp(before_data$logOR) / (1 + exp(before_data$logOR)))

#Raw probability must be adjusted for prevalence.
#initializing the adjusted_prob column
before_data$adjusted_prob[before_data$prob <= 0.1] = 0.07
before_data$adjusted_prob[before_data$prob > 0.1 & before_data$prob <= 0.2] = 0.019
before_data$adjusted_prob[before_data$prob > 0.2 & before_data$prob <= 0.3] = 0.026
before_data$adjusted_prob[before_data$prob > 0.3 & before_data$prob <= 0.4] = 0.04
before_data$adjusted_prob[before_data$prob > 0.4 & before_data$prob <= 0.5] = 0.049
before_data$adjusted_prob[before_data$prob > 0.5 & before_data$prob <= 0.6] = 0.064
before_data$adjusted_prob[before_data$prob > 0.6 & before_data$prob <= 0.7] = 0.072
before_data$adjusted_prob[before_data$prob > 0.7 & before_data$prob <= 0.8] = 0.082
before_data$adjusted_prob[before_data$prob > 0.8 & before_data$prob <= 0.9] = 0.126
before_data$adjusted_prob[before_data$prob > 0.9 & before_data$prob <= 1.0] = 0.494

################################################################################
################### Calculation section for after_data #########################
################################################################################
#Based on the following equation:
# Log OR for MODY = 19.28 - (0.3154*agedx) - (0.2324*bmi) - (0.6276*hba1c) + (1.7473*pardm) - (0.0352*age) - (0.9952*insortabs) + (0.6943*sex)

#Fill in HbA1c % data using hba1c mmol/mol
after_data$HbA1c[is.na(after_data$HbA1c)] = (0.0915*after_data$`HbA1c(mmol/mol)`[is.na(after_data$HbA1c)])+2.15

#BMI column datatype is char NOT numeric so need to convert to numeric for operation
#unlist() is required to break the list, and convert each ITEM rather than the LIST into numeric
after_data$BMI = as.numeric(unlist(after_data$BMI))

#Create a new column named 'logOR' for the Log Odds Ratio for >6 month patients
after_data['logOR'] = 19.28 - (0.3154*after_data$Agediag) - (0.2324*after_data$BMI) - (0.6276*after_data$HbA1c) + (1.7473*ifelse(after_data$ParDM == 'Y', 1, 0)) - (0.0352*after_data$Age) - (0.9952*ifelse(after_data$InsorOHA == "Y", 1, 0)) + (0.6943*ifelse(after_data$Sex == "M", 1, 2))

#Create a new column named 'prob' for the Probability using the following equation:
#Probability = exp(LogOR) / (1 + exp(LogOR))
after_data['prob'] = (exp(after_data$logOR) / (1 + exp(after_data$logOR)))

#Raw probability must be adjusted for prevalence.
#initializing the adjusted_prob column
after_data$adjusted_prob[after_data$prob <= 0.1] = 0.046
after_data$adjusted_prob[after_data$prob > 0.1 & after_data$prob <= 0.2] = 0.151
after_data$adjusted_prob[after_data$prob > 0.2 & after_data$prob <= 0.3] = 0.21
after_data$adjusted_prob[after_data$prob > 0.3 & after_data$prob <= 0.4] = 0.244
after_data$adjusted_prob[after_data$prob > 0.4 & after_data$prob <= 0.5] = 0.329
after_data$adjusted_prob[after_data$prob > 0.5 & after_data$prob <= 0.6] = 0.358
after_data$adjusted_prob[after_data$prob > 0.6 & after_data$prob <= 0.7] = 0.455
after_data$adjusted_prob[after_data$prob > 0.7 & after_data$prob <= 0.8] = 0.580
after_data$adjusted_prob[after_data$prob > 0.8 & after_data$prob <= 0.9] = 0.624
after_data$adjusted_prob[after_data$prob > 0.9 & after_data$prob <= 1.0] = 0.755

#Fully Finalised Data!
View(before_data)
View(after_data)

#Export as CSV
write.csv(before_data,"~/Downloads/MODY_interview_before_data.csv", row.names = TRUE)
write.csv(after_data,"~/Downloads/MODY_interview_after_data.csv", row.names = TRUE)

