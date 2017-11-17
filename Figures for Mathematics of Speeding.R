#Figures for Mathematics of Speeding
source('Functions for Mathematics of Speeding.R')


#Figure 1: Basics
distances <- c("60"=brew[1],"120"=brew[2],"180"=brew[3],"200"=brew[4])
dist<-as.numeric(names(distances))
fig1 <- ggplot(data.frame(x = c(0, 120)), aes(x, colour=as.character(dist))) +
  stat_function(aes(colour=as.character(dist[1])),size=1,fun = function(x,d=dist[1]){d/x}, geom = "line") +
  stat_function(aes(colour=as.character(dist[2])),size=1,fun = function(x,d=dist[2]){d/x}, geom = "line") +
  stat_function(aes(colour=as.character(dist[3])),size=1,fun = function(x,d=dist[3]){d/x}, geom = "line") +
  stat_function(aes(colour=as.character(dist[4])),size=1,fun = function(x,d=dist[4]){d/x}, geom = "line") +
  scale_colour_manual(name="distance",values=distances,breaks=dist) +
  ylab("hours") + xlab("mph") + ylim(c(0,10)) +
  ggtitle("Time to drive Different Distances as a Function of Speed")

#Figure 2: A Table of the same thing
fig2 <- data.frame(Speed = seq(50, 90, 10))
fig2$Hours = round(60/fig2$Speed,4)
fig2$Time= hoursToHHMM(fig2$Hours,T)




#Figure 3 - A position over time graph
legs1 <- data.frame(distance=c(130,5,20,30,25), avgspeed=c(70,5,20,65,35))
makeDistanceTraveledGraph(legs1)

#Figure 4 - a table showing the comparison
legs2 <- data.frame(distance=c(130,5,20,30,25), avgspeed=c(90,5,20,65,35))
makeDistanceTraveledGraph(legs2)
ltwoTrips <- rbind(
                cbind(data.frame(Trip='A'),legs1),
                cbind(data.frame(Trip='B'),legs2)
            )
legs1Tot <- getTotalsFromLegs(legs1)
legs2Tot <- getTotalsFromLegs(legs2)
fig4 <- data.frame(
            Trip=as.character(c('A','B')),
            AvgSpeed=round(c(legs1Tot[['average']],legs2Tot[['average']]),2),
            TotalDist=c(legs1Tot[['totals']]$distance,legs2Tot[['totals']]$distance),
            TotalTime=as.character(c(hoursToHHMM(legs1Tot[['totals']]$hours,TRUE),hoursToHHMM(legs2Tot[['totals']]$hours,TRUE)))
        )
fig4 <- rbind(fig4,data.frame(
          Trip="Diff",
          AvgSpeed=as.numeric(subset(fig4,Trip=='B',AvgSpeed))-as.numeric(subset(fig4,Trip=='A',AvgSpeed)),
          TotalDist=as.numeric(subset(fig4,Trip=='B',TotalDist))-as.numeric(subset(fig4,Trip=='A',TotalDist)),
          TotalTime=hoursToHHMM(legs2Tot[['totals']]$hours-legs1Tot[['totals']]$hours,T)
        ))


#Formula
#going d miles. Time = d/s
#increase speed by 20%
#newtime = d/(1.2s)
#newtime=1/1.2 * oldtime



