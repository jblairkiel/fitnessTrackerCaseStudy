

dailyCaloriesAnalysis <- function(fileNameInput){
    library(viridis)
    library(tidyverse)
    library("tidyr", help, pos = 2, lib.loc = NULL)
    library("dplyr", help, pos = 2, lib.loc = NULL)
    library("ggplot2")
    library("zoo")
    library("scales")

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
    # [3] "Calories"               
    
    #Calories Over Time
    curPlot <- ggplot(data=dataFrame, aes(x=dfTime, y=Calories, group=Id)) + 
        scale_color_viridis() +
        aes(color=Calories) +
        ylim(-50, 5050) + 
        geom_line() + 
        scale_x_date(date_breaks = "1 week", date_labels = "%D") 
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"CaloriesOverTime.png",sep="" ), plot=curPlot,  device = "png")


    #Sum and Mean of Calories of All
    aggFrame <- aggregate(cbind(Calories) ~ dfTime, data=dataFrame, FUN=sum)
    names(aggFrame)[names(aggFrame)=="Calories"] <- "SumCalories"


    KCal <- 25
    avgFrame <- aggregate(cbind(Calories) ~ dfTime, data=dataFrame, FUN=mean)
    aggFrame$AvgCalories <- avgFrame$Calories
    aggFrame$SumCalories <- aggFrame$SumCalories / KCal
    aggFrame <- pivot_longer(data=aggFrame, 2:3, names_to = "type", values_to = "Calories", values_drop_na = FALSE)

    # write.csv(aggFrame, 'aggFrame.csv')
    # curPlot <- ggplot(data=aggFrame, aes(x=dfTime, y=Calories, color=factor(type))) +
    #     scale_color_viridis()
    #     # geom_line(aes(y=rollmean(AvgCalories, 7, na.pad=TRUE), color='red') ) +
    #     # geom_line(aes(y=rollmean((SumCalories / KCal), 7, na.pad=TRUE), color='blue') )
    #     # scale_color_manual(name='Sum and Mean Calories',
    #     #                     breaks=c('Avg ', 'Sum / 25 Cal'),
    #     #                     values=c('Avg'='red', 'Sum / 25 Cal'='blue'))
    # # KCal <- 25
    # # curPlot <- ggplot(data=aggFrame, aes(x=dfTime )) + 
    # #     geom_point(aes(y=AvgCalories, color=AvgCalories)) +
    # #     geom_point(aes(y=(SumCalories / KCal), color=SumCalories/KCal)) + 
    # #     geom_line(aes(y=rollmean(AvgCalories, 7, na.pad=TRUE), color='red'), show.legend = TRUE) +
    # #     geom_line(aes(y=rollmean((SumCalories / KCal), 7, na.pad=TRUE), color='blue'), show.legend = TRUE) +
    # #     #scale_color_viridis())+
    # #     #scale_fill_viridis()
    # #     ylim(-50, 5050) + 
    # #     scale_x_date(date_breaks = "1 week", date_labels = "%D") 
    # #     # scale_y_continuous(

    # #     #     # Features of the first axis
    # #     #     name = "Calories",
            
    # #     #     # Add a second axis and specify its features
    # #     #     sec.axis = sec_axis( trans=~./(KCal*40), name="KCal")
    # #     # )   

    # ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"CaloriesOverTime_SumMean.png",sep="" ), plot=curPlot,  device = "png")


    #Calories Over Time who quit early

    #Early Quitters
    quittersDF2 <- dataFrame
    quittersDF <- dataFrame
    quittersDF <-aggregate(quittersDF$Calories, by=list(quittersDF$Id), FUN=length)
    colnames(quittersDF) <- c("Id", "Count")
    quittersDF <- filter(quittersDF, Count < 25)    
    mergeDF <- merge(quittersDF2, quittersDF, by="Id", all.y=TRUE)

    curPlot <- ggplot(data=mergeDF, aes(x=dfTime, y=Calories, group=Id)) + 
        scale_color_viridis() +
        aes(color=Id) +
        geom_point() + 
        geom_smooth(aes(x=dfTime, y=Calories, color=Id), method = "lm") +
        ylim(-50, 5050) + 
        scale_x_date(date_breaks = "1 week", 
                 labels=date_format("%m/%d/%Y"),
                 limits = as.Date(c('2016-04-12','2016-05-12')))
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"CaloriesOverTime_EarlyQuitters.png",sep="" ), plot=curPlot,  device = "png")

    #Late quitters (tapering off)
    quittersDF2 <- dataFrame
    quittersDF <- dataFrame
    quittersDF <-aggregate(quittersDF$Calories, by=list(quittersDF$Id), FUN=length)
    colnames(quittersDF) <- c("Id", "Count")
    quittersDF <- filter(quittersDF, Count < 30)    
    mergeDF <- merge(quittersDF2, quittersDF, by="Id", all.y=TRUE)

    curPlot <- ggplot(data=mergeDF, aes(x=dfTime, y=Calories, group=Id)) + 
        scale_color_viridis() +
        aes(color=Id) +
        geom_point() + 
        ylim(-50, 5050) + 
        geom_smooth(aes(x=dfTime, y=Calories, color=Id), method = "lm") +
        scale_x_date(date_breaks = "1 week", 
                 labels=date_format("%m/%d/%Y"),
                 limits = as.Date(c('2016-04-12','2016-05-12')))
    ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',"CaloriesOverTime_LateQuitters.png",sep="" ), plot=curPlot,  device = "png")





}
