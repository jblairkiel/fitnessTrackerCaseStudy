

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
    #print(colnames(dataFrame))
    #print(head(dataFrame))
    #"Id"                       "ActivityDate"
    # [3] "TotalSteps"               "TotalDistance"
    # [5] "TrackerDistance"          "LoggedActivitiesDistance"
    # [7] "VeryActiveDistance"       "ModeratelyActiveDistance"
    # [9] "LightActiveDistance"      "SedentaryActiveDistance"
    # [11] "VeryActiveMinutes"        "FairlyActiveMinutes"
    # [13] "LightlyActiveMinutes"     "SedentaryMinutes"
    # [15] "Calories"                 "dfTime"

    #
    #Distance by types
    #
    curPlot <- ggplot(data=dataFrame, aes(x=SedentaryActiveDistance, y=TotalDistance, group=Calories)) +
        scale_color_viridis(option = "D")+  aes(color=Calories) + geom_point() 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"SedentaryActiveDistance_TotalDistance_byCalories.png",sep="" ), plot=curPlot,  device = "png")

    curPlot <- ggplot(data=dataFrame, aes(x=LightActiveDistance, y=TotalDistance, group=Calories)) + 
        scale_color_viridis(option = "D")+  aes(color=Calories) + geom_jitter() 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"LightlyActiveDistance_TotalDistance_byCalories.png",sep="" ), plot=curPlot,  device = "png")
  

    curPlot <- ggplot(data=dataFrame, aes(x=ModeratelyActiveDistance, y=TotalDistance, group=Calories)) +
        scale_color_viridis(option = "D")+  aes(color=Calories) + geom_jitter() 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"ModeratelyActive_TotalDistance_byCalories.png",sep="" ), plot=curPlot,  device = "png")

 
    curPlot <- ggplot(data=dataFrame, aes(x=VeryActiveDistance, y=TotalDistance, group=Calories)) +
        scale_color_viridis(option = "D")+  aes(color=Calories) + geom_jitter() 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"VeryActiveDistance_TotalDistance_byCalories.png",sep="" ), plot=curPlot,  device = "png")
  
    distanceDF <- pivot_longer(data=dataFrame, 4, names_to = "type", values_to = "distance", values_drop_na = FALSE)
    distanceDF <- pivot_longer(data=dataFrame, 7:10, names_to = "type", values_to = "distance", values_drop_na = FALSE)
    curPlot <- ggplot(data=distanceDF,  aes(x=distance, fill=type)) + geom_histogram()
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"Distance_ByType.png",sep="" ), plot=curPlot,  device = "png")

    


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
    
    print(class(dataFrame$dfTime))

    #X  = Date, Y = TotalDistance
    curPlot <- ggplot(data=dataFrame, aes(y=TotalDistance, x=Calories, group=Id)) + aes(color=Id) + geom_point() + geom_smooth() + #scale_x_date(date_labels = "%m/%d/%Y") +
    scale_color_viridis(option = "D")

    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/', "TotalDistancevsCalories_byId.png",sep="" ), plot=curPlot,  device = "png")

    #X  = Date, Y = TotalSteps
    curPlot <- ggplot(data=dataFrame, aes(y=TotalSteps, x=Calories, group=Id)) + aes(color=Id) + geom_point() + geom_smooth() + #scale_x_date(date_labels = "%m/%d/%Y") +
    scale_color_viridis(option = "D")

    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/', "TotalStepsvsCalories_byId.png",sep="" ), plot=curPlot,  device = "png")

    #X  = Date, Y = Calories
    curPlot <- ggplot(data=dataFrame, aes(y=TotalSteps, x=Calories, group=Id)) + aes(color=Id) + geom_point() + geom_smooth() + #scale_x_date(date_labels = "%m/%d/%Y") +
    scale_color_viridis(option = "D")

    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/', "TotalStepsvsCalories_byId.png",sep="" ), plot=curPlot,  device = "png")

    #TODO
    minutesDF <- pivot_longer(data=dataFrame, 11:14, names_to = "type", values_to = "minutes", values_drop_na = FALSE)
    curPlot <- ggplot(data=minutesDF,  aes(x=minutes, fill=type)) + geom_histogram()
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"Minutes_ByTypeHist.png",sep="" ), plot=curPlot,  device = "png")

    curPlot <- ggplot(data=minutesDF,  aes(x=minutes, y=Calories, color=type)) + geom_jitter()
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"Minutes_ByType.png",sep="" ), plot=curPlot,  device = "png")

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
              aes(x = avgDF$Id, #seq(1, length(avgDF$Id), by = 1),
                  y = Minutes,
                  fill = factor(Type, levels=c("VeryActiveMinutes", "FairlyActiveMinutes", "LightlyActiveMinutes", "SedentaryMinutes")))) +
                  labs(fill = "Type") + 
    geom_bar(position = "fill", stat = "identity") +  scale_y_continuous(labels = scales::percent_format())
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"TypeMinutes_ByID.png",sep="" ), plot=curPlot,  device = "png")

    # Sum Very Active and Fairly Active by Calories

    # avgCalories <- aggregate(dataFrame$Calories, list(dataFrame$Id), 
    #     FUN=(function(x){ifelse(sum(x==0)>0 & sum(x !=0) >0, mean(x[x>0]), mean(x))}))
    
    avgDF <- aggregate(cbind(FairlyActiveMinutes,VeryActiveMinutes, Calories) ~ Id , data=dataFrame, FUN = mean, na.rm = TRUE)
    avgDF$SemiActiveMinutes <- rowSums(avgDF[,2:3])
    print(avgDF)

    avgDF$Id <- seq(1, length(avgDF$Id), 1)
    curPlot <- ggplot(avgDF,            # Create ggplot2 plot scaled to 1.00
              aes(x = Calories, 
                  y = SemiActiveMinutes)) +
        scale_color_viridis(option = "D") + geom_point() + geom_smooth(method=lm)
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"SemiActivevsCalories.png",sep="" ), plot=curPlot,  device = "png")


    #TODO Types bin vs total distance grouped by Calories
    #distanceDF <- select(data=dataFrame, Id)
    #distanceDF <- distanceDF + pivot_longer(data=dataFrame, 4, names_to = "type", values_to = "distance", values_drop_na = FALSE)
    #distanceDF <- pivot_longer(data=dataFrame, 7:10, names_to = "type", values_to = "distance", values_drop_na = FALSE)
    # minutesDF <- pivot_longer(data=dataFrame, 11:14, names_to = "type", values_to = "minutes", values_drop_na = FALSE)
    
    #TODO
    #Very-Fairly and Lightly and Sedentary vs Cals



}
