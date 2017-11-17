library(ggplot2)
library(scales)
library(RColorBrewer)




makeDistanceTraveledGraph <- function(legs=data.frame()) {
    #legs is a dataframe of trip legs
    #columns are distance and avg speed for that distance
    #only works for trips that are less than 24 hours
    cols <- brewer.pal(3,"Set1") ; names(cols) <- c("by leg","whole trip")
    funcList <- getTotalsFromLegs(legs)
    times <- funcList[['times']]
    segments <- funcList[['segments']]
    ggplot(data.frame(x=c(0,1),y=c(0,1)))+
      geom_segment(data=segments,aes(colour='by leg',x=x,xend=xend,y=y,yend=yend)) +
      geom_segment(aes(colour='whole trip',x=min(segments$x),xend=max(segments$xend),y=min(as.POSIXct(segments$y,format="%H:%M")),yend=max(as.POSIXct(segments$yend,format='%H:%M'))),linetype='dashed') +
      scale_y_datetime(breaks=date_breaks("30 min"),labels=date_format('%H:%M',tz="EST")) +
      guides(colour=guide_legend("Time to Travel")) + ylab("Hours") + xlab("Miles") +
      labs(
          title = 'Distance Traveled over Time',
          caption = paste0(
            'Total Distance Traveled: ',funcList[['totals']][1],'; ',
            'Hours for trip: ',round(funcList[['totals']][2],2),'; ',
            'Average Speed for Whole Trip: ',round(funcList[["average"]],1),' mph'))
}
legs <- data.frame(distance=c(130,5,20,30,25), avgspeed=c(90,5,20,65,35))
makeDistanceTraveledGraph(legs)
average.speed.trip <- getTotalsFromLegs(legs)[["average"]] ; average.speed.trip
total.time.trip    <- getTotalsFromLegs(legs)[["totals"]]  ; total.time.trip



ggplot(data.frame(x=c(0,1),y=c(0,1)))+
  geom_segment(data=segments,aes(colour='by leg',x=x,xend=xend,y=y,yend=yend)) +
  geom_segment(aes(colour='whole trip',x=min(segments$x),xend=max(segments$xend),y=min(segments$y),yend=max(segments$yend)),linetype='dashed') 
