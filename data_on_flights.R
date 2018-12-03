
data_on_flights<-data01
data_on_flights

data_on_flights$Flight.time.in.minutes <- as.character(data_on_flights$Flight.time.in.minutes)
data_on_flights <- data_on_flights[!is.na(data_on_flights$Flight.time.in.minutes),]



datasatbyname<-data.frame(aggregate( Satisfaction ~ Airline.Name, data_on_flights, mean ))


#ggplots
w<-ggplot(data=datasatbyname, aes(Airline.Name,Satisfaction)) + geom_col()
w1<-w+geom_line()
w1<-w1+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Airline")
w1

#6 Type.of.Travel vs Satisfaction
a9<-ggplot(data=datasatbyname, aes(Type.of.Travel,Satisfaction)) + geom_col()
a10<-a9+geom_line()
a10<-a10+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Type.of.Travel")
a10


#7 No..of.other.Loyalty.Cards vs Satisfaction
a11<-ggplot(data=datasatbyname, aes(No..of.other.Loyalty.Cards,Satisfaction)) + geom_col()
a12<-a11+geom_line()
a12<-a12+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per No..of.other.Loyalty.Cards")
a12


#linear modelling
lmExperience <- lm(formula=Satisfaction~Year.of.First.Flight+No.of.Flights.p.a.+No..of.other.Loyalty.Cards+Airline.Status+Type.of.Travel+Class, data = data0)
summary(lmExperience)
