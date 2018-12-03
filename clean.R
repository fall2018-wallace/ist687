
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

w<-ggplot(data=datasatbyname, aes(Airline.Name,Satisfaction)) + geom_col()
w1<-w+geom_line()
w1<-w1+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Airline")
w1






#ggplots2


#Airline Status vs Overall Customer Satisfaction
a1<-ggplot(data=datasatbyname, aes(Airline.Status,Satisfaction)) + geom_col()
a2<-a1+geom_line()
a2<-a2+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Airline Status")
a2

#age  vs Customer Satisfaction


#Price Sensitivity  vs Customer Satisfaction
a3<-ggplot(data=datasatbyname, aes(Price.Sensitivity,Satisfaction)) + geom_col()
a4<-a3+geom_line()
a4<-a4+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Price Sensitivity")
a4


#Year of First Flight vs Customer Satisfaction


#Number of flights per anum vs Customer Satisfaction
a5<-ggplot(data=datasatbyname, aes(No.of.Flights.p.a.,Satisfaction)) + geom_col()
a6<-a5+geom_line()
a6<-a6+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per number of flights per anum")
a6


#X..of.Flight.with.other.Airlines vs Custome Satisfaction
a7<-ggplot(data=datasatbyname, aes(No.of.Flights.p.a.,Satisfaction)) + geom_col()
a8<-a7+geom_line()
a8<-a8+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per number of flights with other airlines")
a8


#Type.of.Travel vs Custome Satisfaction
a9<-ggplot(data=datasatbyname, aes(Type.of.Travel,Satisfaction)) + geom_col()
a10<-a9+geom_line()
a10<-a10+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Type.of.Travel")
a10


#No..of.other.Loyalty.Cards vs Custome Satisfaction

#Eating.and.Drinking.at.Airport vs Custome Satisfaction

#Class vs Custome Satisfaction

#Flight.date vs Custome Satisfaction

#Airline.Code vs Custome Satisfaction

#Orgin.City vs Custome Satisfaction

#Destination.City vs Custome Satisfaction

#Flight.date vs Custome Satisfaction

#Flight.date vs Custome Satisfaction

#Flight.date vs Custome Satisfaction

#Flight.date vs Custome Satisfaction


