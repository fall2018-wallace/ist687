
#data00
traveldata<-data00
traveldata
lm1<-scatter.smooth(x=traveldata$Satisfaction, y=traveldata$Flight.time.in.minutes, main="Custsat ~ Flighttimeinminutes") 
