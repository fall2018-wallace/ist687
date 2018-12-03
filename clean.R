
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


#Hotel size vs Overall Customer Satisfaction
a1<-ggplot(data=datasatbyname, aes(Airline.Status,Satisfaction)) + geom_col()
a2<-a1+geom_line()
a2<-a2+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Satisfaction per Airline Status")
a2

#Check_in satisfaction vs Customer Satisfaction
CheckInSatplot<-ggplot(data = hotelSurvey1,aes(x=checkInSat,y=overallCustSat))+geom_point()
CheckInSatplot +geom_jitter()+labs(x="Check-In Satisfacton",y="Overall Customer Satisfaction")

#Hotel State vs Customer Satisfaction
hotelStateplot<-ggplot(data = hotelSurvey1,aes(x=hotelState,y=overallCustSat))+geom_point()
hotelStateplot+geom_jitter() +labs(x="Hotel State",y="Overall Customer Satisfaction")


#Hotel cleanliness vs Customer Satisfaction
hotelCleanplot<-ggplot(data = hotelSurvey1,aes(x=hotelClean,y=overallCustSat))+geom_point()
hotelCleanplot +geom_jitter()+labs(x="Hotel State",y="Overall Customer Satisfaction")

#Hotel Friendliness vs Custome Satisfaction
hotelFriendlyplot<-ggplot(data = hotelSurvey1,aes(x=hotelFriendly,y=overallCustSat))+geom_point()
hotelFriendlyplot +geom_jitter()+labs(x="hotel Friendliness",y="Overall Customer Satisfaction")

#Gender vs Custome Satisfaction
genderplot<-ggplot(data = hotelSurvey1,aes(x=gender,y=overallCustSat))+geom_point()
genderplot +geom_jitter()+labs(x="hotelFriendliness",y="Overall Customer Satisfaction")


#Guest Age vs Custome Satisfaction
guestAgeplot<-ggplot(data = hotelSurvey1,aes(x=guestAge,y=overallCustSat))+geom_point()
guestAgeplot +geom_jitter()+labs(x="Guest Age",y="Overall Customer Satisfaction")

#Length of stay vs Custome Satisfaction
lengthOfStayplot<-ggplot(data = hotelSurvey1,aes(x=lengthOfStay,y=overallCustSat))+geom_point()
lengthOfStayplot +geom_jitter()+labs(x="Length of stay",y="Overall Customer Satisfaction")

#When booked Trip vs Custome Satisfaction
whenBookedTripplot<-ggplot(data = hotelSurvey1,aes(x=whenBookedTrip,y=overallCustSat))+geom_point()
whenBookedTripplot +geom_jitter()+labs(x="When Booked",y="Overall Customer Satisfaction")


