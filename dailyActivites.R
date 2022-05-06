

dailyActivityAnalysis <- function(fileNameInput){
    library(viridis)
    library(tidyverse)
    library("tidyr", help, pos = 2, lib.loc = NULL)
    library("dplyr", help, pos = 2, lib.loc = NULL)
    library("ggplot2")

    # Reading
    dataFrame <- read.csv(fileNameInput)
    #Find time value
    
    for(c in colnames(dataFrame)) {
        if(class(dataFrame[[c]])=='character') {
            dataFrame$dfTime <- as.Date(dataFrame[[c]], "%m/%d/%Y")
        } else if (class(dataFrame[[c]])=='Date'){
            dataFrame$dfTime <- dataFrame[[c]]
        }
    }
    ##print(colnames(dataFrame))
    ##print(head(dataFrame))
    #"Id"                       "ActivityDate"
    # [3] "TotalSteps"               "TotalDistance"
    # [5] "TrackerDistance"          "LoggedActivitiesDistance"
    # [7] "VeryActiveDistance"       "ModeratelyActiveDistance"
    # [9] "LightActiveDistance"      "SedentaryActiveDistance"
    # [11] "VeryActiveMinutes"        "FairlyActiveMinutes"
    # [13] "LightlyActiveMinutes"     "SedentaryMinutes"
    # [15] "Calories"                 "dfTime"

    # DistanceTraveledPerDay days
    mostActiveDaysDF <- aggregate(cbind(TotalDistance) ~ dfTime, data=dataFrame, mean)
    curPlot <- ggplot(data=mostActiveDaysDF, aes(x=dfTime, y=TotalDistance, fill=TotalDistance)) + 
        scale_fill_viridis() +
        geom_bar(position='stack', stat='identity') + 
        scale_x_date(date_breaks = "1 week", date_labels = "%D") 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"DistanceTraveledPerDay.png",sep="" ), plot=curPlot,  device = "png")

    
    mostActiveDaysDF <- aggregate(cbind(TotalSteps) ~ dfTime, data=dataFrame, mean)
    curPlot <- ggplot(data=mostActiveDaysDF, aes(x=dfTime, y=TotalSteps, fill=TotalSteps)) + 
        scale_fill_viridis() +
        geom_bar(position='stack', stat='identity') + 
        scale_x_date(date_breaks = "1 week", date_labels = "%D") 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"StepsTraveledPerDay.png",sep="" ), plot=curPlot,  device = "png")

    #Calories and Step/Distance Ratio
    dataFrame$stepsDistanceRatio <- (dataFrame$TotalSteps / dataFrame$TotalDistance)
    sdRat <- aggregate(cbind(stepsDistanceRatio, Calories) ~ Id, data=dataFrame, mean, na.rm = TRUE)
    sdRat <- arrange(sdRat, stepsDistanceRatio)
    sdRat$Id <- seq(1, length(sdRat$Id), 1)
    curPlot <- ggplot(data=sdRat, aes(x=Id, y=stepsDistanceRatio, fill=Calories)) +
        scale_fill_viridis() +
        geom_bar(position='stack', stat='identity') 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"CaloriesandStep-DistanceRatio.png",sep="" ), plot=curPlot,  device = "png")

    


    #Distances

    #
    #Distance by types
    #
    curPlot <- ggplot(data=dataFrame, aes(x=SedentaryActiveDistance, y=SedentaryMinutes, color=Calories)) +
        scale_color_viridis(option = "D")+  geom_point() +
        geom_smooth(aes(x=SedentaryActiveDistance, y=SedentaryMinutes, color=Calories), method = "lm")
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"DistanceandMinutes_SedentaryActive_byCalories.png",sep="" ), plot=curPlot,  device = "png")

    curPlot <- ggplot(data=dataFrame, aes(x=LightActiveDistance, y=LightlyActiveMinutes, color=Calories)) + 
        scale_color_viridis(option = "D")+   geom_jitter()  +
        geom_smooth(aes(x=LightActiveDistance, y=LightlyActiveMinutes, color=Calories), method = "lm")
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"DistanceandMinutes_LightlyActive_byCalories.png",sep="" ), plot=curPlot,  device = "png")
  

    curPlot <- ggplot(data=dataFrame, aes(x=ModeratelyActiveDistance, y=FairlyActiveMinutes, color=Calories)) +
        scale_color_viridis(option = "D")+    geom_jitter()  +
        geom_smooth(aes(x=ModeratelyActiveDistance, y=FairlyActiveMinutes, color=Calories), method = "lm")
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"DistanceandMinutes_FairlyActive_byCalories.png",sep="" ), plot=curPlot,  device = "png")

 
    curPlot <- ggplot(data=dataFrame, aes(x=VeryActiveDistance, y=VeryActiveMinutes, color=Calories)) +
        scale_color_viridis(option = "D")+   geom_jitter()  +
        geom_smooth(aes(x=VeryActiveDistance, y=VeryActiveMinutes, color=Calories), method = "lm")
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"DistanceandMinutes_VeryActive_byCalories.png",sep="" ), plot=curPlot,  device = "png")
  
    distanceDF <- pivot_longer(data=dataFrame, 4, names_to = "type", values_to = "distance", values_drop_na = FALSE)
    distanceDF <- pivot_longer(data=dataFrame, 7:10, names_to = "type", values_to = "distance", values_drop_na = FALSE)
    curPlot <- ggplot(data=distanceDF,  aes(x=distance, fill=type)) + geom_histogram()
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"Distance_ByTypeHist.png",sep="" ), plot=curPlot,  device = "png")

    #Average Distance_ByTypeBar
    distanceDF <- filter(dataFrame, (ActivityDate >= "4/12/2016" & ActivityDate <= "4/14/2016" )) 
    distanceDF$sum <- distanceDF$SedentaryActiveDistance + distanceDF$LightActiveDistance + distanceDF$ModeratelyActiveDistance + distanceDF$VeryActiveDistance
    #distanceDF <-aggregate(. ~Id, data=distanceDF, FUN=mean, na.rm=FALSE)
    distanceDF <- arrange(distanceDF, sum)
    distanceDF$Id <- seq(1, length(distanceDF$Id), 1)
    distanceDF <- pivot_longer(data=distanceDF, 7:10, names_to = "type", values_to = "distance", values_drop_na = FALSE)
    curPlot <- ggplot(data=distanceDF ,  aes(x=Id, y=distance,  fill=type)) + geom_bar(position='stack', stat='identity')
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"Distance_ByTypeBar.png",sep="" ), plot=curPlot,  device = "png")


    #
    # Minutes by Type and Calories
    #
    curPlot <- ggplot(data=dataFrame, aes(x=SedentaryMinutes, y=TotalDistance, group=Calories)) +
        scale_color_viridis(option = "D")+  aes(color=Calories) + geom_point() 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"SedentaryMinutes_TotalDistance_byCalories.png",sep="" ), plot=curPlot,  device = "png")

    curPlot <- ggplot(data=dataFrame, aes(x=LightlyActiveMinutes, y=TotalDistance, group=Calories)) +
        scale_color_viridis(option = "D")+  aes(color=Calories) + geom_jitter() 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"LightlyActiveMinutes_TotalDistance_byCalories.png",sep="" ), plot=curPlot,  device = "png")
  

    curPlot <- ggplot(data=dataFrame, aes(x=FairlyActiveMinutes, y=TotalDistance, group=Calories)) +
        scale_color_viridis(option = "D")+  aes(color=Calories) + geom_jitter() 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"FairlyActiveMinutes_TotalDistance_byCalories.png",sep="" ), plot=curPlot,  device = "png")

 
    curPlot <- ggplot(data=dataFrame, aes(x=VeryActiveMinutes, y=TotalDistance, group=Calories)) +
        scale_color_viridis(option = "D")+  aes(color=Calories) + geom_jitter() 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"VeryActiveMinutes_TotalDistance_byCalories.png",sep="" ), plot=curPlot,  device = "png")
    
    #print(class(dataFrame$dfTime))

    #X  = Date, Y = TotalDistance
    curPlot <- ggplot(data=dataFrame, aes(y=TotalDistance, x=Calories, group=Id)) + aes(color=Id) + geom_jitter() + geom_smooth(method='lm') + #scale_x_date(date_labels = "%m/%d/%Y") +
    scale_color_viridis(option = "D")

    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/', "TotalDistancevsCalories_byId.png",sep="" ), plot=curPlot,  device = "png")

    #X  = Date, Y = TotalSteps
    curPlot <- ggplot(data=dataFrame, aes(y=TotalSteps, x=Calories, group=Id)) + aes(color=Id) + geom_jitter() + geom_smooth(method='lm') + #scale_x_date(date_labels = "%m/%d/%Y") +
    scale_color_viridis(option = "D")

    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/', "TotalStepsvsCalories_byId.png",sep="" ), plot=curPlot,  device = "png")

    #X  = Date, Y = Calories
    curPlot <- ggplot(data=dataFrame, aes(y=TotalSteps, x=Calories, group=Id)) + aes(color=Id) + geom_jitter() + geom_smooth(method='lm') + #scale_x_date(date_labels = "%m/%d/%Y") +
    scale_color_viridis(option = "D")

    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/', "TotalStepsvsCalories_byId.png",sep="" ), plot=curPlot,  device = "png")

    #Mintues and Calories 
    #minutesDF <- arrange(minutesDF, MostlyInactiveMinutes)
    minutesDF <- pivot_longer(data=dataFrame, 11:14, names_to = "type", values_to = "minutes", values_drop_na = FALSE)
    #minutesDF$type <- factor(minutesDF$type, levels=c('SedentaryMinutes','LightlyActiveMinutes','FairlyActiveMinutes', 'VeryActiveMinutes'))
    curPlot <- ggplot(data=minutesDF, aes(x=minutes,  fill = type)) +
        geom_histogram()
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"CaloriesbyMinutes_ByTypeHist.png",sep="" ), plot=curPlot,  device = "png")

    curPlot <- ggplot(data=minutesDF,  
        aes(x=minutes,  y=Calories,   color = type)) +
        geom_jitter() + 
        geom_smooth(aes(x=minutes, y=Calories, color=type), method = "lm")
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"CaloriesbyMinutes_ByTypeTrend.png",sep="" ), plot=curPlot,  device = "png")

    
    curPlot <- ggplot(data=minutesDF,  
        aes(x=minutes, y=Calories, color = type)) + 
        geom_smooth(aes(x=minutes, y=Calories, color=type), method = "lm") #No Jitter
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"CaloriesbyMinutes_ByTypeTrendOnly.png",sep="" ), plot=curPlot,  device = "png")


    #Average the dates

    avgSedentaryMinutes <- aggregate(dataFrame$SedentaryMinutes, list(dataFrame$Id), 
        FUN=(function(x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))
    # names(averagedList)[1] <- 'AvgSedentaryMinutes'

    avgLightlyActiveMinutes <- aggregate(dataFrame$LightlyActiveMinutes, list(dataFrame$Id), 
        FUN=(function(x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))

    avgFairlyActiveMinutes <- aggregate(dataFrame$FairlyActiveMinutes, list(dataFrame$Id), 
    FUN=(function(x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))

    avgVeryActiveMinutes <- aggregate(dataFrame$VeryActiveMinutes, list(dataFrame$Id), 
        FUN=(function(x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))
        
    avgCalories <- aggregate(dataFrame$Calories, list(dataFrame$Id), 
        FUN=(function(x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))
        
    avgDF <- aggregate(cbind(SedentaryMinutes, LightlyActiveMinutes, FairlyActiveMinutes,VeryActiveMinutes) ~ Id , data=dataFrame, FUN = mean, na.rm = TRUE)
    total <- rowSums(avgDF[,2:5])
    avgDF$Id <- seq(1, length(avgDF$Id), 1)
    avgDF <- pivot_longer(data=avgDF, 2:5, names_to = "Type", values_to = "Minutes", values_drop_na = FALSE)
  
    curPlot <- ggplot(avgDF,            # Create ggplot2 plot scaled to 1.00
              aes(x = Id, #seq(1, length(avgDF$Id), by = 1),
                  y = Minutes,
                  fill = factor(Type, levels=c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes")))) +
                  labs(fill = "Type") + 
    geom_bar(position = "fill", stat = "identity") +  scale_y_continuous(labels = scales::percent_format())
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"TypeMinutes_ByID.png",sep="" ), plot=curPlot,  device = "png")

    # Sum Very Active and Fairly Active by Calories

    # avgCalories <- aggregate(dataFrame$Calories, list(dataFrame$Id), 
    #     FUN=(function(x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))
    
    avgDF <- aggregate(cbind(FairlyActiveMinutes,VeryActiveMinutes, SedentaryMinutes, LightlyActiveMinutes, Calories) ~ Id , data=dataFrame, FUN = mean, na.rm = TRUE)
    avgDF$Id <- seq(1, length(avgDF$Id), 1)
    avgDF$MostlyActiveMinutes <- rowSums(avgDF[,2:3])
    avgDF$MostlyInactiveMinutes <- rowSums(avgDF[,4:5])
    avgDF <- arrange(avgDF, MostlyInactiveMinutes)
    avgDF$Id <- seq(1, length(avgDF$Id), 1)
    avgDF <- pivot_longer(data=avgDF, 7:8, names_to = "Type", values_to = "Minutes", values_drop_na = FALSE)
    
    curPlot <- ggplot(avgDF,            # Create ggplot2 plot scaled to 1.00
              aes(x = Id, 
                  y = Minutes,
                  fill =  Type) ) +
            geom_bar(position = "stack", stat = "identity") + scale_fill_viridis(discrete = T) 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"SemiActivevsCalories.png",sep="" ), plot=curPlot,  device = "png")


    #TODO Types bin vs total distance grouped by Calories
    #distanceDF <- select(data=dataFrame, Id)
    #distanceDF <- distanceDF + pivot_longer(data=dataFrame, 4, names_to = "type", values_to = "distance", values_drop_na = FALSE)
    #distanceDF <- pivot_longer(data=dataFrame, 7:10, names_to = "type", values_to = "distance", values_drop_na = FALSE)
    # minutesDF <- pivot_longer(data=dataFrame, 11:14, names_to = "type", values_to = "minutes", values_drop_na = FALSE)
    
    #TODO
    #Very-Fairly and Lightly and Sedentary vs Cals



}
