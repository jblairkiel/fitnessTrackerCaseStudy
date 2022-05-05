source("./summaryFunctions.R")
source("./DailyActivites.R")
source("./DailyCalories.R")

dataPath <- "./FitabaseData"

dailyActivityFilename <- paste(dataPath, "/dailyActivity_merged.csv", sep="")
dailyCaloriesFilename <- paste(dataPath, "/dailyCalories_merged.csv", sep="")
dailyIntensities <- paste(dataPath, "/dailyIntensities_merged.csv", sep="")
 
print(head(dailyActivityFilename))

#summaryPlots(dailyActivityFilename)
#dailyActivityAnalysis(dailyActivityFilename)

summaryPlots(dailyCaloriesFilename)
dailyCaloriesAnalysis(dailyCaloriesFilename)
#summaryPlots(dailyIntensities)