#Functions for Mathematics of Speeding
for (pkg in c('ggplot2','scales','RColorBrewer')) { 
  if (!is.element(pkg,installed.packages()[,1])) { 
    r <- getOption("repos") 
    r["CRAN"] <- "http://cran.us.r-project.org" 
    options(repos = r) 
    rm(r) 
    install.packages(pkg) 
  } 
  library(pkg,character.only = T) 
} 




hoursToHHMM <-function(hours,formatted=FALSE) {
  isNeg <- ifelse(hours<0,'-','')
  hours<-abs(hours)
  HH <- hours%/%1
  MM <- (60*(hours-HH))%/%1
  SS <- round(60*(hours-HH-(MM/60)),3)
  fmtTime <- paste0(HH,":",MM,":",SS)
  t <- as.POSIXct(strptime(fmtTime,format="%H:%M:%S"),tz="UTC",origin=Sys.Date())
  if (formatted) {return(paste0(isNeg,unlist(strsplit(as.character(t),split=' '))[seq(2,2*length(hours),2)]))}
  return(t)
}



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
    geom_segment(aes(colour='whole trip',x=min(segments$x),xend=max(segments$xend),y=min(segments$y),yend=max(segments$yend)),linetype='dashed') +
    scale_x_datetime(breaks=date_breaks("30 min"),labels=date_format('%H:%M',tz="UTC")) +
    guides(colour=guide_legend("Time to Travel")) + ylab("Miles Traveled") + xlab("Time Traveled") +
    labs(
      title = 'Distance Traveled over Time',
      caption = paste0(
        'Total Distance Traveled: ',funcList[['totals']][1],'; ',
        'Time for trip: ',round(funcList[['totals']][2],2),'; '
        ,'Average Speed for Whole Trip: ',round(funcList[["average"]],1),' mph'))
}