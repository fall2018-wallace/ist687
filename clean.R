
clean_data <- raw_data
clean_data
data0<- clean_data
data0<- data.frame(null.values= NA)
data0<- data0[,-4]

data0<-data0[,-13]



data0<-data0[,-26]



data0<-data0[,-25]


data0<- data0[,-17]


data0<- data0[,-18]


data1<-data0[,10]


data0<-data0[,-10]


data0


v<-data0[,21]
v



data0<- data0[-c(21:22)] #extra arrival delay greater than 5 mins



#clean
data0$Flight.time.in.minutes <- as.character(data0$Flight.time.in.minutes)
data0 <- data0[!is.na(data0$Flight.time.in.minutes),]


data0$Satisfaction <- as.numeric(as.character(data0$Satisfaction))
data0$Price.Sensitivity <- as.numeric(as.character(data0$Price.Sensitivity))
data0$Year.of.First.Flight <- as.numeric(as.character(data0$Year.of.First.Flight))
data0$No.of.Flights.p.a. <- as.numeric(as.character(data0$No.of.Flights.p.a.))
data0$X..of.Flight.with.other.Airlines <- as.numeric(as.character(data0$X..of.Flight.with.other.Airlines))
data0$Price.Sensitivity <- as.numeric(as.character(data0$Price.Sensitivity))
data0$No..of.other.Loyalty.Cards <- as.numeric(as.character(data0$No..of.other.Loyalty.Cards))


data0$Eating.and.Drinking.at.Airport <- as.numeric(as.character(data0$Eating.and.Drinking.at.Airport))

data0$Scheduled.Departure.Hour <- as.numeric(as.character(data0$Scheduled.Departure.Hour))
data0$Departure.Delay.in.Minutes <- as.numeric(as.character(data0$Departure.Delay.in.Minutes))
data0$Arrival.Delay.in.Minutes <- as.numeric(as.character(data0$Arrival.Delay.in.Minutes))
data0$Flight.time.in.minutes <- as.numeric(as.character(data0$Flight.time.in.minutes))

data0<-na.omit(data0)

sapply(data0,function(x) sum(length(which(is.na(x)))))









#ggplots


