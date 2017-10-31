# Read Me about Deepak's script

Created for my data science class on coursera

The functions built into run_anaysis.R R Script are described here :

## 1. ReadSensorData
ReadSensorData function does the following: 
    1. Read text files
    2. Merge the datasets appropriately
    3. Name the columns appropriately
    4. Extracts mean and standard deviation data alone
    5. Rename the "Activity" appropriately
After above processes, it returns a dataframe with parameters shown in "summary_features_info.md"

## 2. SummarizeData(setdata)
summararizedata function takes in the processed output dataframe of ReadSensorData and does:
    1. Means Based on ACTIVITIES for ALL subjects
    2. Means Based on SUBJECTS for ALL activities
    3. Based on BOTH activities and subjects
    
    
## 3. ProcessAndSaveSummary(filename)
A Single Function to read, process and save data summary into a file of given filename
