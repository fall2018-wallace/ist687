
#data00
traveldata<-data00
#traveldata
traveldata$Satisfaction <- as.numeric(as.character(traveldata$Satisfaction))
traveldata$Satisfaction
lmFlight <- lm(formula=Satisfaction~Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes, data = data00
summary(lmFlight)

#ggplots
#14 Scheduled.Departure.Hour vs Satisfaction
a25<-ggplot(data=datasatbyname, aes(Scheduled.Departure.Hour,Satisfaction)) + geom_col()
a26<-a25+geom_line()
a26<-a26+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Scheduled.Departure.Hour")
a26

#15 Departure.Delay.in.Minutes vs Satisfaction
a27<-ggplot(data=datasatbyname, aes(Departure.Delay.in.Minutes,Satisfaction)) + geom_col()
a28<-a27+geom_line()
a28<-a28+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Departure.Delay.in.Minutes")
a28

#16 Arrival.Delay.in.Minutes vs Satisfaction
a29<-ggplot(data=datasatbyname, aes(Departure.Delay.in.Minutes,Satisfaction)) + geom_col()
a30<-a29+geom_line()
a30<-a30+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Departure.Delay.in.Minutes")
a30

#17 Flight.cancelled vs Satisfaction
a31<-ggplot(data=datasatbyname, aes(Flight.cancelled,Satisfaction)) + geom_col()
a32<-a31+geom_line()
a32<-a32+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Flight.cancelled")
a32

#18 Flight.time.in.minutes vs Satisfaction
a33<-ggplot(data=datasatbyname, aes(Flight.time.in.minutes,Satisfaction)) + geom_col()
a34<-a33+geom_line()
a34<-a34+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Flight.time.in.minutes")
a34



#linear model
lmFlight <- lm(formula=Satisfaction~Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes, data = data0)
summary(lmFlight)
