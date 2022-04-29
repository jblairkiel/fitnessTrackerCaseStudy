library("tidyr", help, pos = 2, lib.loc = NULL)
library("dplyr", help, pos = 2, lib.loc = NULL)
library("ggplot2")

# Common Resources
dataPath <- "./FitabaseData"

dailyActivityFilename <- paste(dataPath, "/dailyActivity_merged.csv", sep="")
 
# Reading
dailyActivity_merged <- read.csv(dailyActivityFilename)
ActivityDate <- dailyActivity_merged$ActivityDate
FairlyActiveMinutes <- dailyActivity_merged$FairlyActiveMinutes
LightlyActiveMinutes <- dailyActivity_merged$LightlyActiveMinutes
SedentaryMinutes <- dailyActivity_merged$SedentaryMinutes
Calories <- dailyActivity_merged$Calories
dailyActivity_merged$activeTime <- as.Date(dailyActivity_merged$ActivityDate, "%m/%d/%Y")
print(colnames(dailyActivity_merged))
# contents of the csv file
#print(head(dailyActivity_merged))
#print(ActivityDate)
#print(class(dailyActivity_merged))

for(c in colnames(dailyActivity_merged)) {
    print(paste(c, "this is class:", class(dailyActivity_merged[[c]])))
    if(class(dailyActivity_merged[[c]])=='numeric' || class(dailyActivity_merged[[c]])=='integer') {
        curPlot <- ggplot(data=dailyActivity_merged, aes(x=.data[[c]])) + geom_histogram(color='#4d4d50', fill='blue', cex.lab = 3)
        ggsave(filename=paste("summaryHist_", c,".png",sep="" ), plot=curPlot,  device = "png")

        
        curPlot <- ggplot(data=dailyActivity_merged, aes(x=activeTime, y=.data[[c]])) + geom_jitter(color="blue") + scale_x_date(date_labels = "%Y-%m-%d")
        ggsave(filename=paste("summaryScatter_", c,".png",sep="" ), plot=curPlot,  device = "png")

    
    }
}

#hist(x=ActivityDate, main="Hist", col="blue")
#hist(x=LightlyActiveMinutes, main="Hist", col="blue")
#hist(x=LightlyActiveMinutes, main="Hist", col="blue")
#hist(x=SedentaryMinutes, main="Hist", col="blue")
#hist(x=Calories, main="Hist", col="blue")````
#print(head(dailyActivity_merged$DateTime))
#print(head(dailyActivity_merged))

print(ggplot(data=dailyActivity_merged, aes(x=activeTime, y=Calories)) + geom_jitter(color="blue") + scale_x_date(date_labels = "%Y-%m-%d"))