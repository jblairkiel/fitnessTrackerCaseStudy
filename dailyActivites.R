

dailyActivityAnalysis <- function(fileNameInput){
    library(viridis)
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
    print(colnames(dataFrame))
    print(head(dataFrame))
    #"Id"                       "ActivityDate"
    # [3] "TotalSteps"               "TotalDistance"
    # [5] "TrackerDistance"          "LoggedActivitiesDistance"
    # [7] "VeryActiveDistance"       "ModeratelyActiveDistance"
    # [9] "LightActiveDistance"      "SedentaryActiveDistance"
    # [11] "VeryActiveMinutes"        "FairlyActiveMinutes"
    # [13] "LightlyActiveMinutes"     "SedentaryMinutes"
    # [15] "Calories"                 "dfTime"


    curPlot <- ggplot(data=dataFrame, aes(x=ModeratelyActiveDistance, y=TotalDistance, group=Calories)) +
        scale_color_viridis(option = "D")+  aes(color=Calories) + geom_jitter() 
    curPlot  <- curPlot 
    #View(curPlot)

    
    curPlot <- ggplot(data=dataFrame, aes(x=ModeratelyActiveDistance, y=TotalDistance, group=Calories)) +
        scale_color_viridis(option = "D")+  aes(color=Calories) + geom_point() 
    curPlot  <- curPlot 
    View(curPlot)


    
    curPlot <- ggplot(data=dataFrame, aes(x=TotalDistance, group=Calories)) 
    curPlot <- curPlot + scale_color_viridis(option = "D")+  aes(color=Calories) + geom_histogram(bins=10) #+ scale_x_date(date_labels = "%Y-%m-%d")
    mid <- mean(dataFrame$Calories)
    View(curPlot)




            # numBins <- 10
            # # breakDiff <- (max(dataFrame[[c]]) - min(dataFrame[[c]]))
            # # print(breakDiff)
            # # numBreaks <- cut(dataFrame[[c]], breaks= seq(min(dataFrame[[c]]), max(dataFrame[[c]]),  breakDiff/10))
            # # print(numBreaks)

            # #By Count
            # curPlot <- ggplot(data=dataFrame, aes(x=.data[[c]], fill=group)) + geom_histogram(aes(fill=..count..), bins=numBins) 
            # curPlot <- curPlot + scale_fill_gradient("Count", low="blue", high="red")
            # ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"_summaryHist_byCount.png",sep="" ), plot=curPlot,  device = "png")


     
            # #By Id
            # curPlot <- ggplot(data=dataFrame, aes(x=.data[[c]], group=Id)) + geom_histogram(aes(fill=Id)) 
            # mid<-mean(dataFrame$Id)
            # curPlot <- curPlot + scale_fill_gradient("Id",low="blue",  high="red")
            # ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/',c,"_summaryHist_byId.png",sep="" ), plot=curPlot,  device = "png")

            # curPlot <- ggplot(data=dataFrame, aes(x=.data[[c]], y=dfTime, group=Id)) + aes(color=Id) + geom_jitter() + scale_y_date(date_labels = "%Y-%m-%d")
            # #curPlot + scale_fill_brewer(palette="Dark2")
            # curPlot  <- curPlot + scale_color_gradient(low="blue", high="red")
            # mid<-mean(dataFrame$Id)
            # curPlot  <- curPlot + scale_color_gradient2(midpoint=mid, low="blue",  high="red", space ="Lab" )
            
            # ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/', c,"_summaryScatter_byId.png",sep="" ), plot=curPlot,  device = "png")

        
            
            # # if (c != "Id"){
            # #     for(user in unique(dataFrame[c("Id")])){
            # #         curPlot <- ggplot(data=dataFrame, aes(x=.data[[c]], y=dfTime)) + geom_jitter(color="blue") + scale_y_date(date_labels = "%Y-%m-%d")
            # #         ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/', c,"_summaryScatter_.png",sep="" ), plot=curPlot,  device = "png")
            # #     }
            # # }
    #}
}
