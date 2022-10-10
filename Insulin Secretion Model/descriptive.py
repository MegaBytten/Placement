import csv, os
import numpy as np
model_1_data = []
data = []

#assign filepath
filepath = os.path.expanduser('~/RStudio/Placement/Insulin Secretion Model/autoantibody_model_1_data.csv')

#open data.csv 
with open(filepath, newline='') as csvfile:
    reader = csv.reader(csvfile, delimiter=',', quotechar='|')
    for row in reader:
        data.append(row)

#clean data into model_1_data object (excluse title column)
for row in data:
    if row[1] != '"x"':
        model_1_data.append(row[1])

# convert strings-array into float array
model_1_data = np.asarray(model_1_data, dtype=float)
        
# Loop through probability deciles and track decile frequency
deciles = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
count = [0,0,0,0,0,0,0,0,0,0]
for datapoint in model_1_data:
    for index in range(10):
        if index == 0:
            if datapoint <= 0.1: count[0] += 1
        else:
            if datapoint > deciles[index-1] and datapoint <= deciles[index]:
                count[index] += 1
    
#calculate percentages
n = len(model_1_data)
percentages = []
for decile in count:
    percentages.append(decile/n)

#Print results
print('N, Count and Percentages:')
print(len(model_1_data))
print(count)
print (percentages)
