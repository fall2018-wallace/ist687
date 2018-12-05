
#data00
traveldata<-data00
traveldata$Departure.Delay.in.Minutes <- ifelse(is.na(traveldata$Departure.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Departure.Delay.in.Minutes)
traveldata$Arrival.Delay.in.Minutes <- ifelse(is.na(traveldata$Arrival.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Arrival.Delay.in.Minutes)
traveldata$Flight.time.in.minutes <- ifelse(is.na(traveldata$Flight.time.in.minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Flight.time.in.minutes)
traveldatacleaned<- traveldata[-(is.na(traveldata$Arrival.Delay.in.Minutes)),] 
traveldatacleaned<- na.omit(traveldata) 
traveldatacleaned$Satisfaction<-as.numeric(traveldatacleaned$Satisfaction)
 summary(traveldatacleaned) 
