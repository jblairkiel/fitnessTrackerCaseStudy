source("./summaryFunctions.R")
source("./DailyActivites.R")
source("./DailyCalories.R")
source("./heartRateAndIntensities.R")
source("./caloriesAndIntensities.R")

dataPath <- "./FitabaseData"

#dailyActivityFilename <- paste(dataPath, "/dailyActivity_merged.csv", sep="")
#dailyCaloriesFilename <- paste(dataPath, "/dailyCalories_merged.csv", sep="")
#dailyIntensitiesFilename <- paste(dataPath, "/dailyIntensities_merged.csv", sep="")
#heartRateSecondsFilename <- paste(dataPath, "/heartrate_seconds_merged.csv", sep="")
minuteIntesitiesFilename <- paste(dataPath, "/minuteIntensitiesNarrow_merged.csv", sep="")
minuteCaloriesFilename <- paste(dataPath, "/minuteCaloriesNarrow_merged.csv", sep="")

#summaryPlots(dailyActivityFilename)
#dailyActivityAnalysis(dailyActivityFilename)

#summaryPlots(dailyCaloriesFilename)
#dailyCaloriesAnalysis(dailyCaloriesFilename)

#summaryPlots(dailyIntensitiesFilename)
#dailyIntensities(dailyIntensitiesFilename)

#summaryPlots(heartRateSecondsFilename)
#summaryPlots(minuteIntesitiesFilename)
#heartRateAnalysis(heartRateSecondsFilename, minuteIntesitiesFilename)

#summaryPlots(heartRateSecondsFilename)
#summaryPlots(minuteIntesitiesFilename)
intensitiesAndCaloriesAnalysis(minuteIntesitiesFilename, minuteCaloriesFilename)