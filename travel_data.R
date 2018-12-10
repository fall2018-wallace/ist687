

dataCleaned<-raw_data#[(trimws(data$Airline.Name,which="right")=="Cheapseats Airlines Inc."),]
traveldata<-dataCleaned

traveldata$Flight.time.in.minutes <- ifelse(is.na(traveldata$Flight.time.in.minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Flight.time.in.minutes)
traveldatacleaned<- traveldata[-(is.na(traveldata$Arrival.Delay.in.Minutes)),] 
traveldatacleaned$Satisfaction<-as.numeric(traveldatacleaned$Satisfaction)
traveldatacleaned<- na.omit(traveldata) 

Satisfaction<-traveldatacleaned
summary(traveldatacleaned)

Satisfaction$Satisfaction <- as.numeric(as.character(Satisfaction$Satisfaction))
Satisfaction$Price.Sensitivity <- as.numeric(as.character(Satisfaction$Price.Sensitivity))
Satisfaction$Year.of.First.Flight <- as.numeric(as.character(Satisfaction$Year.of.First.Flight))
Satisfaction$No.of.Flights.p.a. <- as.numeric(as.character(Satisfaction$No.of.Flights.p.a.))
Satisfaction$X..of.Flight.with.other.Airlines <- as.numeric(as.character(Satisfaction$X..of.Flight.with.other.Airlines))
Satisfaction$Price.Sensitivity <- as.numeric(as.character(Satisfaction$Price.Sensitivity))
Satisfaction$No..of.other.Loyalty.Cards <- as.numeric(as.character(Satisfaction$No..of.other.Loyalty.Cards))
Satisfaction$Shopping.Amount.at.Airport <- as.numeric(as.character(Satisfaction$Shopping.Amount.at.Airport))
Satisfaction$Eating.and.Drinking.at.Airport <- as.numeric(as.character(Satisfaction$Eating.and.Drinking.at.Airport))
Satisfaction$Day.of.Month <- as.numeric(as.character(Satisfaction$Day.of.Month))
Satisfaction$Scheduled.Departure.Hour <- as.numeric(as.character(Satisfaction$Scheduled.Departure.Hour))
Satisfaction$Departure.Delay.in.Minutes <- as.numeric(as.character(Satisfaction$Departure.Delay.in.Minutes))
Satisfaction$Arrival.Delay.in.Minutes <- as.numeric(as.character(Satisfaction$Arrival.Delay.in.Minutes))
Satisfaction$Flight.time.in.minutes <- as.numeric(as.character(Satisfaction$Flight.time.in.minutes))
Satisfaction$Flight.Distance <- as.numeric(as.character(Satisfaction$Flight.Distance))
Satisfaction <- Satisfaction[ ! Satisfaction$Satisfaction %in% c(1.5,2.5,3.5,4.5), ]
View(Satisfaction)

summary(Satisfaction)
traveldatacleaned<-Satisfaction

###############################################################################
modelOfLoyalty<-lm(formula = Satisfaction~No..of.other.Loyalty.Cards,data = traveldatacleaned)
summary(modelOfLoyalty)
modelOfAirlineStatus<-lm(formula = Satisfaction~Airline.Status, data = traveldatacleaned)
summary(modelOfAirlineStatus)
lmCustomers <- lm(formula=Satisfaction~Age+Price.Sensitivity+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, data = traveldatacleaned)
summary(lmCustomers)
lmCustomers <- lm(formula=Satisfaction~Price.Sensitivity, data = traveldatacleaned)
summary(lmCustomers)
modelOfFTIM<-lm(formula = Satisfaction~Flight.time.in.minutes,data = traveldatacleaned)
summary(modelOfFTIM)
modelOfADDIM<-lm(formula = Satisfaction~Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes,data = traveldatacleaned)
summary(modelOfADDIM)
modelOfDOM<-lm(formula = Satisfaction~Day.of.Month,data = traveldatacleaned)
summary(modelOfDOM)
modelOfQ<-lm(formula = Satisfaction~Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport,data = traveldatacleaned)
summary(modelOfQ)
modelOfClass<-lm(formula = Satisfaction~Class,data = traveldatacleaned)
summary(modelOfClass)
modelOfNOF<-lm(formula = Satisfaction~No.of.Flights.p.a.,data = traveldatacleaned)
summary(modelOfNOF)
modelOfNOF<-lm(formula = Satisfaction~No.of.Flights.p.a.,data = traveldatacleaned)
summary(modelOfNOF)
modelOfAge<-lm(formula = Satisfaction~Age,data = traveldatacleaned)
summary(modelOfAge)
modelOfGender<-lm(formula = Satisfaction~Gender,data = traveldatacleaned)
summary(modelOfGender)
modelOfSE<-lm(formula = Satisfaction~Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport,data = traveldatacleaned)
summary(modelOfSE)

##########################################################################
ggplot(Satisfaction,aes(y=Satisfaction, x=Age))+
  geom_point()+
  stat_smooth(method = "lm", col="red")
ggplot(Satisfaction,aes(y=Satisfaction, x=Gender))+
  geom_point()+
  stat_smooth(method = "lm", col="red")
ggplot(Satisfaction,aes(y=Satisfaction, x=Shopping.Amount.at.Airport))+
  geom_point()+
  stat_smooth(method = "lm", col="red")
ggplot(Satisfaction,aes(y=Satisfaction, x=Eating.and.Drinking.at.Airport))+
  geom_point()+
  stat_smooth(method = "lm", col="red")
ggplot(Satisfaction,aes(y=Satisfaction, x=Price.Sensitivity))+
  geom_point()+
  stat_smooth(method = "lm", col="red")
#################################################################
ggplot(Satisfaction,aes(y=Satisfaction, x=Year.of.First.Flight))+
  geom_point()+
  stat_smooth(method = "lm", col="blue")
ggplot(Satisfaction,aes(y=Satisfaction, x=No.of.Flights.p.a.))+
  geom_point()+
  stat_smooth(method = "lm", col="blue")
ggplot(Satisfaction,aes(y=Satisfaction, x=No..of.other.Loyalty.Cards))+
  geom_point()+
  stat_smooth(method = "lm", col="blue")
ggplot(Satisfaction,aes(y=Satisfaction, x=Class))+
  geom_point()+
  stat_smooth(method = "lm", col="blue")
ggplot(Satisfaction,aes(y=Satisfaction, x=Airline.Status))+
  geom_point()+
  stat_smooth(method = "lm", col="blue")
ggplot(Satisfaction,aes(y=Satisfaction, x=Type.of.Travel))+
  geom_point()+
  stat_smooth(method = "lm", col="blue")



#################################################################
#################################################################
ggplot(Satisfaction,aes(y=Satisfaction, x=Scheduled.Departure.Hour))+
  geom_point()+
  stat_smooth(method = "lm", col="green")
ggplot(Satisfaction,aes(y=Satisfaction, x=Departure.Delay.in.Minutes))+
  geom_point()+
  stat_smooth(method = "lm", col="green")
ggplot(Satisfaction,aes(x=Arrival.Delay.in.Minutes,y=Satisfaction ))+
  geom_point()+
  stat_smooth(method = "lm", col="green")
ggplot(Satisfaction,aes(y=Satisfaction, x=Flight.time.in.minutes))+
  geom_point()+
  stat_smooth(method = "lm", col="green")
ggplot(Satisfaction,aes(y=Satisfaction, x=Flight.Distance))+
  geom_point()+
  stat_smooth(method = "lm", col="green")

#####################################################################

#####################################################################
modelOfFD<-lm(formula = Satisfaction~Flight.Distance,data = traveldatacleaned)
summary(modelOfFD)
modelOfALL<-lm(formula = Satisfaction~.,data = traveldatacleaned)
summary(modelOfALL)
modelOfCancelled<-lm(formula = Satisfaction~Flight.cancelled,data = traveldatacleaned)
summary(modelOfCancelled)
modelOfSDH<-lm(formula = Satisfaction~Scheduled.Departure.Hour,data = traveldatacleaned)
summary(modelOfSDH)
modelOfOC<-lm(formula = Satisfaction~Orgin.City,data = traveldatacleaned)
summary(modelOfOC)
modelOfOS<-lm(formula = Satisfaction~Origin.State,data = traveldatacleaned)
summary(modelOfOS)
modelOfDC<-lm(formula = Satisfaction~Destination.City,data = traveldatacleaned)
summary(modelOfDC)
modelOfDS<-lm(formula = Satisfaction~Destination.State,data = traveldatacleaned)
summary(modelOfDS)
modelOfFD<-lm(formula = Satisfaction~Flight.date,data = traveldatacleaned)
summary(modelOfFD)
lmAll <- lm(formula=Satisfaction~Age+Gender+Price.Sensitivity+Shopping.Amount.at.Airport+Year.of.First.Flight+No.of.Flights.p.a.+No..of.other.Loyalty.Cards+Airline.Status+Type.of.Travel+Class+Scheduled.Departure.Hour+Arrival.Delay.greater.5.Mins, data = traveldatacleaned)
summary(lmAll)
lmDemoandCust <- lm(formula=Satisfaction~Age+Gender+Price.Sensitivity+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, data = traveldatacleaned)
summary(lmDemoandCust )
lmFlightexp <- lm(formula=Satisfaction~Year.of.First.Flight+No.of.Flights.p.a.+No..of.other.Loyalty.Cards+Airline.Status+Type.of.Travel+Class, data = traveldatacleaned)
summary(lmFlightexp)
lmFlighttimedetails <- lm(formula=Satisfaction~Scheduled.Departure.Hour+Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Flight.time.in.minutes+Arrival.Delay.greater.5.Mins+Flight.Distance, data = traveldatacleaned)
summary(lmFlighttimedetails)

modelOfAS<-lm(formula = Satisfaction~Airline.Status,data = traveldatacleaned)
library(ggplot2)
viz1 <- ggplot(traveldatacleaned, aes(x= Satisfaction))+
  geom_histogram(binwidth = 0.2)+
  labs(title="Satisfaction")+
  labs(y="Number of Customers")
viz1


ggplot(Satisfaction,aes(x=Satisfaction, y=No.of.Flights.p.a.))+
  geom_point()+
  stat_smooth(method = "lm", col="red")

ggplot(Satisfaction,aes(x=Shopping.Amount.at.Airport, y=Age))+
  geom_point()+
  stat_smooth(method = "lm", col="red")

ggplot(Satisfaction,aes(x=Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, y=Age))+
  geom_point()+
  stat_smooth(method = "lm", col="red")

ggplot(Satisfaction,aes(x=Gender, y=Eating.and.Drinking.at.Airport+Shopping.Amount.at.Airport))+
  geom_point()+
  stat_smooth(method = "lm", col="red")



ggplot(Satisfaction,aes(x=Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, y=Age))+
  geom_point()+
  stat_smooth(method = "lm", col="red")


#install.packages("maps")
#library(ggplot2)



d <- ggplot(traveldatacleaned, aes(Satisfaction,Eating.and.Drinking.at.Airport+Shopping.Amount.at.Airport, fill = ..density..)) +
 stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ "red")

d <- ggplot(traveldatacleaned, aes(Satisfaction,Class, fill = ..density..)) +
  stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ "Satisfaction vs Class")

d <- ggplot(traveldatacleaned, aes(Satisfaction,Airline.Status, fill = ..density..)) +
  stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ "Satisfaction vs Class")
summary(traveldatacleaned$Satisfaction)

d <- ggplot(traveldatacleaned, aes(Age,No.of.Flights.p.a., fill = ..density..)) +
  stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ "Satisfaction vs Class")

d <- ggplot(traveldatacleaned, aes(Age,Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, fill = ..density..)) +
  stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ "Satisfaction vs Class")

d <- ggplot(traveldatacleaned, aes(Satisfaction,Gender, fill = ..density..)) +
  stat_binhex(na.rm = TRUE) + theme(aspect.ratio = 1)
d + facet_wrap(~ "Satisfaction vs Class")


#install.packages("MASS")
#library(MASS)
SteroidLM<- stepAIC(lmAll,direction = "backward", trace = TRUE)



Satisfaction$Airline.Status<-factor(Satisfaction$Airline.Status,levels = c("Silver","Gold","Platinum","Blue"))
Satisfaction$Type.of.Travel<-factor(Satisfaction$Type.of.Travel,levels = c("Personal","Business","Mileage"))
lmAll <- lm(formula=Satisfaction~Age+Gender+Price.Sensitivity+Shopping.Amount.at.Airport+Year.of.First.Flight+No.of.Flights.p.a.+No..of.other.Loyalty.Cards+Airline.Status+Type.of.Travel+Class+Scheduled.Departure.Hour+Arrival.Delay.greater.5.Mins, data = Satisfaction)
summary(lmAll)
SteroidLM<- stepAIC(lmAll,direction = "backward", trace = TRUE)
summary(SteroidLM)
################################################################
linear_Interesting<-lm(formula = Arrival.Delay.in.Minutes ~ Flight.date + Scheduled.Departure.Hour +
               Departure.Delay.in.Minutes + Flight.Distance, data=traveldatacleaned)
summary(linear_Interesting)


