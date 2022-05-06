

heartRateAnalysis <- function(heartRateFilename, dailyIntensitiesFilename){
    library(viridis)
    library(tidyverse)
    library("tidyr", help, pos = 2, lib.loc = NULL)
    library("dplyr", help, pos = 2, lib.loc = NULL)
    library("ggplot2")

    # Reading
    hrDataFrame <- read.csv(heartRateFilename)
    #Find time value
    
    for(c in colnames(hrDataFrame)) {
        if(class(hrDataFrame[[c]])=='character') {
            hrDataFrame$dfTime <- format(hrDataFrame$hrDataFrame[[c]],'%m/%d/%Y:%H:%M:%S %p')  #strptime(hrDataFrame[[c]],format='%b/%d/%Y:%H:%M:%S %p') # as.POSIXct(hrDataFrame[[c]], format="%m/%d/%Y% %H:%M:%S", tz="UTC") 
        } else if (class(hrDataFrame[[c]])=='Date'){
            hrDataFrame$dfTime <- hrDataFrame[[c]]
        }
    }
    #print(colnames(hrDataFrame))
    #print(head(hrDataFrame))
    #[1] "Id"       "DateTime"  "Value



    # Reading
    diDataFrame <- read.csv(dailyIntensitiesFilename)
    #Find time value
    
    for(c in colnames(diDataFrame)) {
        if(class(diDataFrame[[c]])=='character') {
            diDataFrame$dfTime <-  diDataFrame[[c]] #as.POSIXct(diDataFrame[[c]], format="%m/%d/%Y% %H:%M:%S", tz="UTC") 
        } else if (class(diDataFrame[[c]])=='Date'){
            diDataFrame$dfTime <- diDataFrame[[c]]
        }
    }
    #print(colnames(diDataFrame))
    #print(head(diDataFrame))
    #[1] "Id"        "DateTime"  "Intensity"


    mergeDFx <- merge(hrDataFrame, diDataFrame, by="dfTime", all.x=TRUE)
    mergeDFfilt <- filter(mergeDFx, Id.x == '4020332650')
    #colnames(mergeDF) <- c("dfTime", "Id")

    print(colnames(mergeDFx))
    print(head(mergeDFx))

    curPlot <- ggplot(data=mergeDFfilt, aes(x=DateTime.x, y=Value, group=Id.x)) + 
        scale_color_viridis() +
        geom_line()  
        #scale_x_date(date_breaks = "1 week", date_labels = "%D") 
    ggsave(filename=paste('./plots/',substr(heartRateFilename,16,nchar(heartRateFilename)-4),'/',"HROverTime.png",sep="" ), plot=curPlot,  device = "png")

    
    # mostActiveDaysDF <- aggregate(cbind(TotalSteps) ~ dfTime, data=hrDataFrame, mean)
    # curPlot <- ggplot(data=mostActiveDaysDF, aes(x=dfTime, y=TotalSteps, fill=TotalSteps)) + 
    #     scale_fill_viridis() +
    #     geom_bar(position='stack', stat='identity') + 
    #     scale_x_date(date_breaks = "1 week", date_labels = "%D") 
    # ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"StepsTraveledPerDay.png",sep="" ), plot=curPlot,  device = "png")

    # #Calories and Step/Distance Ratio
    # hrDataFrame$stepsDistanceRatio <- (hrDataFrame$TotalSteps / hrDataFrame$TotalDistance)
    # sdRat <- aggregate(cbind(stepsDistanceRatio, Calories) ~ Id, data=hrDataFrame, mean, na.rm = TRUE)
    # sdRat <- arrange(sdRat, stepsDistanceRatio)
    # sdRat$Id <- seq(1, length(sdRat$Id), 1)
    # curPlot <- ggplot(data=sdRat, aes(x=Id, y=stepsDistanceRatio, fill=Calories)) +
    #     scale_fill_viridis() +
    #     geom_bar(position='stack', stat='identity') 
    # ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"CaloriesandStep-DistanceRatio.png",sep="" ), plot=curPlot,  device = "png")




}
