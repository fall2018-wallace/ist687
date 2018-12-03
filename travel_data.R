
#data00
traveldata<-data00
#traveldata
traveldata$Satisfaction <- as.numeric(as.character(traveldata$Satisfaction))
traveldata$Satisfaction


#ggplots

#lm
lmFlight <- lm(formula=Satisfaction~Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes, data = data0)
summary(lmFlight)
