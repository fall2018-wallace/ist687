
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


#1 Airline Status vs Overall Customer Satisfaction
a1<-ggplot(data=datasatbyname, aes(Airline.Status,Satisfaction)) + geom_col()
a2<-a1+geom_line()
a2<-a2+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Airline Status")
a2

#2 age  vs Customer Satisfaction


#3 Price Sensitivity  vs Satisfaction
a3<-ggplot(data=datasatbyname, aes(Price.Sensitivity,Satisfaction)) + geom_col()
a4<-a3+geom_line()
a4<-a4+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Price Sensitivity")
a4


#4 Year of First Flight vs Satisfaction


#Number of flights per anum vs Satisfaction
a5<-ggplot(data=datasatbyname, aes(No.of.Flights.p.a.,Satisfaction)) + geom_col()
a6<-a5+geom_line()
a6<-a6+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per number of flights per anum")
a6


#5 X..of.Flight.with.other.Airlines vs Satisfaction
a7<-ggplot(data=datasatbyname, aes(No.of.Flights.p.a.,Satisfaction)) + geom_col()
a8<-a7+geom_line()
a8<-a8+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per number of flights with other airlines")
a8



#8 Eating.and.Drinking.at.Airport vs Satisfaction
a13<-ggplot(data=datasatbyname, aes(Eating.and.Drinking.at.Airport,Satisfaction)) + geom_col()
a14<-a13+geom_line()
a14<-a14+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Eating.and.Drinking.at.Airport")
a14


#10 Flight.date vs Satisfaction
a17<-ggplot(data=datasatbyname, aes(Class,Satisfaction)) + geom_col()
a18<-a17+geom_line()
a18<-a18+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Class")
a18



#11 Airline.Code vs Satisfaction
a19<-ggplot(data=datasatbyname, aes(Airline.Code,Satisfaction)) + geom_col()
a20<-a19+geom_line()
a20<-a20+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Airline.Code")
a20

#12 Orgin.City vs Satisfaction
a21<-ggplot(data=datasatbyname, aes(Orgin.City,Satisfaction)) + geom_col()
a22<-a21+geom_line()
a22<-a22+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Orgin.City")
a22

#13 Destination.City vs Satisfaction
a23<-ggplot(data=datasatbyname, aes(Destination.City,Satisfaction)) + geom_col()
a24<-a23+geom_line()
a24<-a24+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Destination.City")
a24


