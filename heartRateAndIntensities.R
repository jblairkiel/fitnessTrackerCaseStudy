

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
            hrDataFrame$dfTime <- format(as.POSIXct(hrDataFrame[[c]],format='%m/%d/%Y %H:%M'), format='%m/%d/%Y %I:%M:%S %p')
            #hrDataFrame$dfTime <- format(strptime(substr(as.POSIXct(sprintf("%04.0f", hrDataFrame$hrDataFrame[[c]]), 
            #                     format="%b/%d/%Y %H%M"), 12, 16), '%b/%d/%Y %H:%M'), '%b/%d/%Y %I:%M')
            #format(hrDataFrame$hrDataFrame[[c]], '') #'%b/%d/%Y %H:%M')  #strptime(hrDataFrame[[c]],format='%b/%d/%Y:%H:%M:%S %p') # as.POSIXct(hrDataFrame[[c]], format="%m/%d/%Y% %H:%M:%S", tz="UTC") 
        } else if (class(hrDataFrame[[c]])=='Date'){
            hrDataFrame$dfTime <- hrDataFrame[[c]]
        }
    }
    print(colnames(hrDataFrame))
    #print(head(hrDataFrame))
    #print(class(hrDataFrame$DateTime))
    #[1] "Id"       "DateTime"  "Value



    # Reading
    diDataFrame <- read.csv(dailyIntensitiesFilename)
    #Find time value
    
    for(c in colnames(diDataFrame)) {
        if(class(diDataFrame[[c]])=='character') {
            diDataFrame$dfTime <- format(as.POSIXct(diDataFrame[[c]],format='%m/%d/%Y %I:%M:%S %p'), format='%m/%d/%Y %I:%M:%S %p')
            #diDataFrame$dfTime <-  diDataFrame[[c]] #as.POSIXct(diDataFrame[[c]], format="%m/%d/%Y% %H:%M:%S", tz="UTC") 
        } else if (class(diDataFrame[[c]])=='Date'){
            diDataFrame$dfTime <- diDataFrame[[c]]
        }
    }
    print(colnames(diDataFrame))
    #print(head(diDataFrame))
    #[1] "Id"        "DateTime"  "Intensity"

    hrDataFrame
    mergeDFx <- merge(hrDataFrame, diDataFrame, by="dfTime", all.x=TRUE)
    print(head(mergeDFx))
    
    mergeDFx$datedate <- as.Date(mergeDFx$dfTime, format='%m/%d/%Y %I:%M:%S %p')
    mergeDFfilt <- filter(mergeDFx, Id.x == '2347167796')
    
    print(head(mergeDFfilt))
    
    write.csv(mergeDFfilt, 'mergeDFfilt.csv')
    print(colnames(mergeDFfilt))
    print(head(mergeDFfilt))
    print(class(mergeDFfilt$dateagain))
    
    curPlot <- ggplot(data=mergeDFfilt, aes(x=datedate, y=Value, group=Id.x)) + 
        scale_color_viridis() +
        geom_jitter() +
        scale_x_date(date_breaks = "1 week", date_labels = "%D") 
    ggsave(filename=paste('./plots/',substr(heartRateFilename,16,nchar(heartRateFilename)-4),'/',"HROverTime.png",sep="" ), plot=curPlot,  device = "png")

    


}
