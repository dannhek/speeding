library(ggplot2)
library(scales)
library(RColorBrewer)



getTotalsFromLegs <- function(legs) {
     times <- legs$distance/legs$avgspeed
     segments <- data.frame(
          y = cumsum(legs$distance)-legs$distance,
          yend = cumsum(legs$distance),
          x = hoursToHHMM(cumsum(times) - times),
          xend = hoursToHHMM(cumsum(times))
     )
     totals <- data.frame(
          distance = sum(legs$distance),
          hours     = sum(times)
     )
     list(
          "times" = times,
          "segments" = segments,
          "average" = sum(legs$distance)/sum(legs$distance/legs$avgspeed),
          "totals"  = totals
     )
}



cols <- brewer.pal(3,"Set1")

legs <- rbind(
     data.frame(scenario='A',distance=c(130,5,20,30,25), avgspeed=c(70,5,20,65,35)),
     data.frame(scenario='B',distance=c(130,5,20,30,25), avgspeed=c(90,5,20,65,35)),
     data.frame(scenario='C',distance=c(130,5,20,30,25), avgspeed=c(75,5,20,70,40))
) ; funcList <- by(legs,legs[,'scenario'],getTotalsFromLegs)
segments <- rbind(
     cbind(scenario='A',funcList$A$segments),
     cbind(scenario='B',funcList$B$segments),
     cbind(scenario='C',funcList$C$segments)
)

ggplot(data=segments) +
     geom_segment(aes(colour=scenario,linetype='by leg',x=x,xend=xend,y=y,yend=yend)) +
     geom_segment(data=ddply(segments,.(scenario),summarize,x=min(x),y=min(y),xend=max(xend),yend=max(yend)),
                  aes(colour=scenario,linetype='whole trip',x=as.POSIXct('00:00',format='%H:%M',tz='UTC'),xend=xend,y=0,yend=yend),alpha=0.5) +
     scale_x_datetime(breaks=date_breaks("30 min"),labels=date_format('%H:%M',tz="UTC")) +
     guides(linetype=guide_legend("Time to Travel"),colour=guide_legend("Scenario")) + ylab("Miles Traveled") + xlab("Time Traveled")

legs <- rbind(
     data.frame(scenario='A',distance=c(130,5,20,30,25), avgspeed=c(70,5,20,65,35)),
     data.frame(scenario='B',distance=c(130,5,20,30,25), avgspeed=c(90,5,20,65,35))
)
makeDistanceTraveledGraph(legs)
average.speed.trip <- getTotalsFromLegs(legs)[["average"]] ; average.speed.trip
total.time.trip    <- getTotalsFromLegs(legs)[["totals"]]  ; total.time.trip

ggplot(data.frame(x=c(0,1),y=c(0,1)))+
  geom_segment(data=segments,aes(colour='by leg',x=x,xend=xend,y=y,yend=yend)) +
  geom_segment(aes(colour='whole trip',x=min(segments$x),xend=max(segments$xend),y=min(segments$y),yend=max(segments$yend)),linetype='dashed') 
