#Figures for Mathematics of Speeding
source('Functions for Mathematics of Speeding.R')


#Figure 1: Basics
brew <- brewer.pal(4,"Set2")
distances <- c("60"=brew[1],"120"=brew[2],"180"=brew[3],"200"=brew[4])
dist<-as.numeric(names(distances))
fig1 <- ggplot(data.frame(x = c(0, 120)), aes(x, colour=as.character(dist))) +
  stat_function(aes(colour=as.character(dist[1])),size=1,fun = function(x,d=dist[1]){d/x}, geom = "line") +
  stat_function(aes(colour=as.character(dist[2])),size=1,fun = function(x,d=dist[2]){d/x}, geom = "line") +
  stat_function(aes(colour=as.character(dist[3])),size=1,fun = function(x,d=dist[3]){d/x}, geom = "line") +
  stat_function(aes(colour=as.character(dist[4])),size=1,fun = function(x,d=dist[4]){d/x}, geom = "line") +
  scale_colour_manual(name="distance",values=distances,breaks=dist) +
  ylab("hours") + xlab("mph") + ylim(c(0,10)) +
  ggtitle("Time to Drive Different Distances as a Function of Speed")

#Figure 2: A Table of the same thing
fig2 <- data.frame(Speed = seq(40, 90, 10))
fig2$Hours = round(60/fig2$Speed,4)
fig2$Time= hoursToHHMM(fig2$Hours,T)

#Figure 3 - A position over time graph
legs1 <- data.frame(distance=c(130,5,20,30,25), avgspeed=c(70,5,20,65,35))
fig3  <- makeDistanceTraveledGraph(legs1)

#Figure 4 - a table showing the comparison
cols <- brewer.pal(3,"Set1")
legs <- rbind(
     data.frame(scenario='A',leg=c(1:5),distance=c(130,5,20,30,25), avgspeed=c(70,5,20,65,35)),
     data.frame(scenario='B',leg=c(1:5),distance=c(130,5,20,30,25), avgspeed=c(90,5,20,65,35)),
     data.frame(scenario='C',leg=c(1:5),distance=c(130,5,20,30,25), avgspeed=c(75,5,20,70,40))
) ; funcList <- by(legs,legs[,'scenario'],getTotalsFromLegs)
segments <- rbind(
     cbind(scenario='A',funcList$A$segments),
     cbind(scenario='B',funcList$B$segments),
     cbind(scenario='C',funcList$C$segments)
)

fig4 <- ggplot(data=segments) +
     geom_segment(aes(colour=scenario,linetype='by leg',x=x,xend=xend,y=y,yend=yend)) +
     geom_segment(data=ddply(segments,.(scenario),summarize,x=min(x),y=min(y),xend=max(xend),yend=max(yend)),
                  aes(colour=scenario,linetype='whole trip',x=as.POSIXct('00:00',format='%H:%M',tz='UTC'),xend=xend,y=0,yend=yend),alpha=0.5) +
     scale_x_datetime(breaks=date_breaks("30 min"),labels=date_format('%H:%M',tz="UTC")) +
     guides(linetype=guide_legend("Time to Travel"),colour=guide_legend("Scenario")) + ylab("Miles Traveled") + xlab("Time Traveled") +
     ggtitle("Distance Traveled Over Time with Different Speeds for Similar Legs")


fig5a <- subset(legs,scenario %in% c('A','B'))
fig5b <- data.frame(
            Trip=as.character(c('A','B')),
            AvgSpeed=round(c(funcList$A[['average']],funcList$B[['average']]),2),
            TotalDist=c(funcList$A[['totals']]$distance,funcList$B[['totals']]$distance),
            TotalTime=as.character(c(hoursToHHMM(funcList$A[['totals']]$hours,TRUE),hoursToHHMM(funcList$B[['totals']]$hours,TRUE)))
        )
fig5b <- rbind(fig5b,data.frame(
          Trip="Diff",
          AvgSpeed=as.numeric(subset(fig5b,Trip=='B',AvgSpeed))-as.numeric(subset(fig5b,Trip=='A',AvgSpeed)),
          TotalDist=as.numeric(subset(fig5b,Trip=='B',TotalDist))-as.numeric(subset(fig5b,Trip=='A',TotalDist)),
          TotalTime=hoursToHHMM(funcList$A[['totals']]$hours-funcList$B[['totals']]$hours,T)
        ))


#Formula
#going d miles. Time = d/s
#increase speed by 20%
#newtime = d/(1.2s)
#newtime=1/1.2 * oldtime



#Print out the images
###Plots 
if (!file.exists('images')) dir.create('images')
png('./images/fig1.png') ; fig1 ; dev.off()
png('./images/fig3.png') ; fig3 ; dev.off()
png('./images/fig4.png') ; fig4 ; dev.off()

###Tables
png('images/fig2.png') 
     textplot(fig2,show.rownames=F,cex=1,valign='top')
     title('Time to Travel 60 Miles')
     dev.off()
png('images/fig5a.png') 
     textplot(fig5a,show.rownames=F,cex=1,valign='top')
     title('Two Trips by Leg')
     dev.off()
png('images/fig5b.png') 
     textplot(fig5b,show.rownames=F,cex=1,valign='top')
     title('Comparison of two trips')
     dev.off()