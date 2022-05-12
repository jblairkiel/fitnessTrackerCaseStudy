

intensitiesAndCaloriesAnalysis <- function(minuteIntesitiesFilename, minuteCaloriesFilename){
    library(viridis)
    library(tidyverse)
    library("tidyr", help, pos = 2, lib.loc = NULL)
    library("dplyr", help, pos = 2, lib.loc = NULL)
    library("ggplot2")
    library("scales")

    # Reading
    minIntDataFrame <- read.csv(minuteIntesitiesFilename)
    #Find time value
    
    for(c in colnames(minIntDataFrame)) {
        if(class(minIntDataFrame[[c]])=='character') {
            minIntDataFrame$dfTime <- format(as.POSIXct(minIntDataFrame[[c]],format='%m/%d/%Y %H:%M:%S %p'), format='%m/%d/%Y %H:%M:%S %p')
            #minIntDataFrame$dfTime <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", minIntDataFrame$minIntDataFrame[[c]]), 
            #                     format="%b/%d/%Y %H%M"), 12, 16), '%b/%d/%Y %H:%M'), '%b/%d/%Y %I:%M')
            #format(minIntDataFrame$minIntDataFrame[[c]], '') #'%b/%d/%Y %H:%M')  #strptime(minIntDataFrame[[c]],format='%b/%d/%Y:%H:%M:%S %p') # as.POSIXct(minIntDataFrame[[c]], format="%m/%d/%Y% %H:%M:%S", tz="UTC") 
        } else if (class(minIntDataFrame[[c]])=='Date'){
            minIntDataFrame$dfTime <- minIntDataFrame[[c]]
        }
    }
    #print(colnames(minIntDataFrame))
    print(head(minIntDataFrame))
    #print(class(minIntDataFrame$DateTime))
    #[1] "Id"       "DateTime"  "Value



    # Reading
    minCalDataFrame <- read.csv(minuteCaloriesFilename)
    #Find time value
    
    for(c in colnames(minCalDataFrame)) {
        if(class(minCalDataFrame[[c]])=='character') {
            minCalDataFrame$dfTime <- format(as.POSIXct(minCalDataFrame[[c]],format='%m/%d/%Y %H:%M:%S %p'), format='%m/%d/%Y %H:%M:%S %p')
            #minCalDataFrame$dfTime <-  minCalDataFrame[[c]] #as.POSIXct(minCalDataFrame[[c]], format="%m/%d/%Y% %H:%M:%S", tz="UTC") 
        } else if (class(minCalDataFrame[[c]])=='Date'){
            minCalDataFrame$dfTime <- minCalDataFrame[[c]]
        }
    }
    #print(colnames(minCalDataFrame))
    print(head(minCalDataFrame))
    #[1] "Id"        "DateTime"  "Intensity"

    minIntDataFrame <- filter(minIntDataFrame, Id == '2347167796')
    minIntDataFrame <- filter(minIntDataFrame, grepl("4/12/2016", DateTime, fixed = TRUE) ) #str_detect(DateTime, regex='4/1[0-9]{1}/2016'))
    minIntDataFrame <- filter(minIntDataFrame, row_number() <= floor(n()/2))

    minCalDataFrame <- filter(minCalDataFrame, Id == '2347167796')
    minCalDataFrame <- filter(minCalDataFrame, grepl("4/12/2016", DateTime, fixed = TRUE) ) #str_detect(DateTime, regex='4/1[0-9]{1}/2016'))
    minCalDataFrame <- filter(minCalDataFrame, row_number() <= floor(n()/2))
    #mergeDFx <- filter(mergeDFx, )
    mergeDFx <- merge(minIntDataFrame, minCalDataFrame, by="dfTime", all.y=TRUE)
    print(head(mergeDFx))
    
    #mergeDFx$datedate <- as.POSIXct(mergeDFx$dfTime, format='%m/%d/%Y %H:%M:%S %p')
    mergeDFx$datedate <- as.Date(mergeDFx$dfTime, format='%m/%d/%Y %H:%M:%S %p')
    #mergeDFx$datedate <- format(mergeDFx$datedate, format='%m/%d/%Y %H:%M:%S %p')
    # print(head(mergeDFx))
    
    write.csv(mergeDFx, 'mergeDFfilt.csv')
    # print(colnames(mergeDFfilt))
    # print(head(mergeDFfilt))
    # print(class(mergeDFfilt$dateagain))
    
    # curPlot <- ggplot(data=mergeDFx, aes(x=dfTime, y=Calories, color='red')) + 
    #     scale_color_viridis() +
    #     geom_jitter() #+ 
    #     # scale_x_datetime(
    #     #     #limits = c(time.start, time.end),
    #     #     breaks = date_breaks("12 hours"),
    #     #     labels = date_format("%H:%M %d-%b")
    #     # )
    # ggsave(filename=paste('./plots/',"caloriesAndIntensities",'/',"caloriesOverTime.png",sep="" ), plot=curPlot,  device = "png")

    
    curPlot <- ggplot(data=mergeDFx, aes(x=Intensity, color=Calories)) + 
        scale_color_viridis() +
        geom_histogram() #(stat="identity", colour="white") #+ 
        # scale_x_datetime(
        #     #limits = c(time.start, time.end),
        #     breaks = date_breaks("12 hours"),
        #     labels = date_format("%H:%M %d-%b")
        # )
    ggsave(filename=paste('./plots/',"caloriesAndIntensities",'/',"caloriesIntensitiesBar.png",sep="" ), plot=curPlot,  device = "png")
    
    curPlot <- ggplot(data=mergeDFx, aes(x=datedate, y=Calories, fill=Intensity)) + 
        scale_fill_viridis() +
        geom_dotplot(binaxis = "y", stackdir = "center") #+ 
        # scale_x_datetime(
        #     #limits = c(time.start, time.end),
        #     breaks = date_breaks("12 hours"),
        #     labels = date_format("%H:%M %d-%b")
        # )
    ggsave(filename=paste('./plots/',"caloriesAndIntensities",'/',"CaloriesGrouped.png",sep="" ), plot=curPlot,  device = "png")

    


}
