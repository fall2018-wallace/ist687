
#data00
traveldata<-data00
#traveldata
sapply(traveldata, mode)
sapply(traveldata, class)

transform(traveldata$Satisfaction=as.numeric(traveldata$Satisfaction))
traveldata$Satisfaction <- as.numeric(as.character(traveldata$Satisfaction))
traveldata$Price.Sensitivity <- as.numeric(as.character(traveldata$Price.Sensitivity))
traveldata$Year.of.First.Flight <- as.numeric(as.character(traveldata$Year.of.First.Flight))
traveldata$No.of.Flights.p.a. <- as.numeric(as.character(traveldata$No.of.Flights.p.a.))
traveldata$X..of.Flight.with.other.Airlines <- as.numeric(as.character(traveldata$X..of.Flight.with.other.Airlines))
traveldata$Price.Sensitivity <- as.numeric(as.character(traveldata$Price.Sensitivity))
traveldata$No..of.other.Loyalty.Cards <- as.numeric(as.character(traveldata$No..of.other.Loyalty.Cards))
traveldata$Shopping.Amount.at.Airport <- as.numeric(as.character(traveldata$Shopping.Amount.at.Airport))
traveldatatraveldata$Eating.and.Drinking.at.Airport <- as.numeric(as.character(traveldata$Eating.and.Drinking.at.Airport))
traveldata$Day.of.Month <- as.numeric(as.character(traveldata$Day.of.Month))
traveldata$Scheduled.Departure.Hour <- as.numeric(as.character(traveldata$Scheduled.Departure.Hour))
traveldata$Departure.Delay.in.Minutes <- as.numeric(as.character(traveldata$Departure.Delay.in.Minutes))
traveldata$Arrival.Delay.in.Minutes <- as.numeric(as.character(traveldata$Arrival.Delay.in.Minutes))
traveldata$Flight.time.in.minutes <- as.numeric(as.character(traveldata$Flight.time.in.minutes))
traveldata$Flight.Distance <- as.numeric(as.character(traveldata$Flight.Distance))

traveldata<- na.omit(Satisfaction)

