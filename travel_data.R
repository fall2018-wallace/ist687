
#data00
traveldata<-data00
traveldata$Departure.Delay.in.Minutes <- ifelse(is.na(traveldata$Departure.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Departure.Delay.in.Minutes)
traveldata$Arrival.Delay.in.Minutes <- ifelse(is.na(traveldata$Arrival.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Arrival.Delay.in.Minutes)
traveldata$Flight.time.in.minutes <- ifelse(is.na(traveldata$Flight.time.in.minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Flight.time.in.minutes)
traveldatacleaned<- traveldata[-(is.na(traveldata$Arrival.Delay.in.Minutes)),] 
traveldatacleaned<- na.omit(traveldata) 
traveldatacleaned$Satisfaction<-as.numeric(traveldatacleaned$Satisfaction)
#summary(traveldatacleaned) 
 
modelOfAirlineStatus<-lm(formula = Satisfaction~Airline.Status, data = traveldatacleaned)
#modelOfAirlineStatus
#summary(modelOfAirlineStatus)

 modelOfAge<-lm(formula = Satisfaction~Age, data = traveldatacleaned)
 #summary(modelOfAge)


 #modelOfInfluencingFactors<-lm(formula = Satisfaction ~ Arrival_Delay_greater_5_Mins+Flight_Distance+Flight_time_in_minutes+Flight_cancelled+Arrival_Delay_in_Minutes+Departure_Delay_in_Minutes+Scheduled_Departure_Hour+Destination_State+Destination_City+Origin_State+Orgin_City+Flight_date+Day_of_Month+Class+Eating_and_Drinking_at_Airport+Shopping_Amount_at_Airport+No__of_other_Loyalty_Cards+Type_of_Travel+X__of_Flight_with_other_Airlines+No_of_Flights_p_a_+Year_of_First_Flight+Price_Sensitivity+Gender+Age+Airline_Status,data= traveldatacleaned)
 #summary(modelOfInfluencingFactors)
