
#data00
traveldata<-data00
traveldata$Departure.Delay.in.Minutes <- ifelse(is.na(traveldata$Departure.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Departure.Delay.in.Minutes)
traveldata$Arrival.Delay.in.Minutes <- ifelse(is.na(traveldata$Arrival.Delay.in.Minutes) & straveldataFlight.cancelled == "Yes", 9999, traveldata$Arrival.Delay.in.Minutes)
traveldata$Flight.time.in.minutes <- ifelse(is.na(traveldata$Flight.time.in.minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Flight.time.in.minutes)
# df<- sw[-(is.na(sw$Arrival.Delay.in.Minutes)),] df<- na.omit(sw) # (is.na(sw$Arrival.Delay.in.Minutes)) summary(df) str(df) View(df) head(order(-df$X..of.Flight.with.other.Airlines)) write.csv(df, file= 'cleanSatisfactionData.csv') if(sw$Flight.cancelled = 'Yes') { is.na() } 
summary(data00)
