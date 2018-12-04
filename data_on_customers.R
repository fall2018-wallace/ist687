
data11<-data0


#linear modelling
lmCustomers <- lm(formula=Satisfaction~Age+Price.Sensitivity+Eating.and.Drinking.at.Airport, data = data0)
summary(lmCustomers)

#ggplots

#2 age  vs Customer Satisfaction
age<-ggplot(data=datasatbyname, aes(age,Satisfaction)) + geom_col()
age1<-age+geom_line()
age1<-age1+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per age")
age1

#3 Price Sensitivity  vs Satisfaction
a3<-ggplot(data=datasatbyname, aes(Price.Sensitivity,Satisfaction)) + geom_col()
a4<-a3+geom_line()
a4<-a4+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Price Sensitivity")
a4

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
a17<-ggplot(data=datasatbyname, aes(Flight.date,Satisfaction)) + geom_col()
a18<-a17+geom_line()
a18<-a18+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Flight.date")
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
