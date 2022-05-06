

summaryPlots <- function(fileNameInput){

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

    for(c in colnames(dataFrame)) {
        #print(paste(c, "this is class:", class(dataFrame[[c]])))
        if(class(dataFrame[[c]])=='numeric' || class(dataFrame[[c]])=='integer') {

            numBins <- 15
            # breakDiff <- (max(dataFrame[[c]]) - min(dataFrame[[c]]))
            # print(breakDiff)
            # numBreaks <- cut(dataFrame[[c]], breaks= seq(min(dataFrame[[c]]), max(dataFrame[[c]]),  breakDiff/10))
            # print(numBreaks)

            #By Count
            curPlot <- ggplot(data=dataFrame, aes(x=.data[[c]], fill=group)) + geom_histogram(aes(fill=..count..), bins=numBins) 
            curPlot <- curPlot + scale_fill_gradient("Count", low="blue", high="red")
            ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/SummaryHist_',c,"_byCount.png",sep="" ), plot=curPlot,  device = "png")


     
            #By Id
            curPlot <- ggplot(data=dataFrame, aes(x=.data[[c]], group=Id)) + geom_histogram(aes(fill=Id), bins=numBins) 
            mid<-mean(dataFrame$Id)
            curPlot <- curPlot + scale_fill_gradient("Id",low="blue",  high="red")
            ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/SummaryHist_',c,"_byId.png",sep="" ), plot=curPlot,  device = "png")

            curPlot <- ggplot(data=dataFrame, aes(x=.data[[c]], y=dfTime, group=Id)) + aes(color=Id) + geom_jitter() + scale_y_date(date_labels = "%Y-%m-%d")
            #curPlot + scale_fill_brewer(palette="Dark2")
            #curPlot  <- curPlot + scale_color_gradient(low="blue", high="red")
            mid<-mean(dataFrame$Id)
            curPlot  <- curPlot + scale_color_gradient2(midpoint=mid, low="blue",  high="red", space ="Lab" )
            
            ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/SummaryScatter_', c,"_byId.png",sep="" ), plot=curPlot,  device = "png")

        
            
            # if (c != "Id"){
            #     for(user in unique(dataFrame[c("Id")])){
            #         curPlot <- ggplot(data=dataFrame, aes(x=.data[[c]], y=dfTime)) + geom_jitter(color="blue") + scale_y_date(date_labels = "%Y-%m-%d")
            #         ggsave(filename=paste('./plots/',substr(fileNameInput,16,nchar(fileNameInput)-4),'/', c,"_summaryScatter_.png",sep="" ), plot=curPlot,  device = "png")
            #     }
            # }
        }
    }
}
