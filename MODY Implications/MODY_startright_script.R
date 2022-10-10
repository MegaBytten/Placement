install.packages('pacman')
require('pacman')
p_load('rio')

my_data <- import("~/Downloads/R practice - Startright Sept/startight Aug 22 comma separated.csv")

################################################################################################
####################### Prelim methods to visualize data #######################################
################################################################################################
View(my_data)
summary(my_data)

################################################################################################
######################## Filtering Data: ETHNICITY & Age #######################################
################################################################################################
#Todo: concatenate lists to 1 large group including everyone for further checks
# ---------------------------------------------------------------------------- #


#Filters the data down to only white british and white irish patients under the age of 35
ethnicities_list = list('White British', 'White Irish')
model_data$BMI[model_data$EthnicOrigin %in%{ethnicities_list}] = model_data$Weight[model_data$EthnicOrigin %in%{ethnicities_list}] / model_data$Height[model_data$EthnicOrigin %in%{ethnicities_list}]^2
filtered_group_white_european = my_data [my_data$EthnicOrigin == 'White British' & my_data$AgeatDiagnosis <= 35 | my_data$EthnicOrigin == 'White Irish' & my_data$AgeatDiagnosis <= 35,]
# View(filtered_group_white_european)

#Extracts the important columns (Ethnicity, age, insulin, height/weight, parental diabetes)
#from the above filtered data into a new data frame
filtered_data_white_european_df = cbind.data.frame(filtered_group_white_european$EthnicOrigin,
                                                   filtered_group_white_european$AgeatDiagnosis,
                                                   filtered_group_white_european$Insulin,
                                                   filtered_group_white_european$Height,
                                                   filtered_group_white_european$Weight,
                                                   filtered_group_white_european$Mother_diabetes,
                                                   filtered_group_white_european$Father_diabetes)
View(filtered_data_white_european_df)

#Filters the data down to only South Asian and African patients under the age of 30
filtered_group_high_risk = my_data [my_data$EthnicOrigin == 'Indian' & my_data$AgeatDiagnosis <= 30 | my_data$EthnicOrigin == 'Bangladeshi' & my_data$AgeatDiagnosis <= 30 | my_data$EthnicOrigin == 'White & Black African' & my_data$AgeatDiagnosis <= 30 | my_data$EthnicOrigin == 'Pakistani' & my_data$AgeatDiagnosis <= 30 | my_data$EthnicOrigin == 'Black African' & my_data$AgeatDiagnosis <= 30,]



################################################################################################
########################## Filtering Data:  ###########################################
################################################################################################


