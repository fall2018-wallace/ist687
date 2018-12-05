
#data00
traveldata<-data00
traveldata$Departure.Delay.in.Minutes <- ifelse(is.na(sw$Departure.Delay.in.Minutes) & sw$Flight.cancelled == "Yes", 9999, sw$Departure.Delay.in.Minutes)
traveldata$Arrival.Delay.in.Minutes <- ifelse(is.na(sw$Arrival.Delay.in.Minutes) & sw$Flight.cancelled == "Yes", 9999, sw$Arrival.Delay.in.Minutes)
traveldataFlight.time.in.minutes <- ifelse(is.na(sw$Flight.time.in.minutes) & sw$Flight.cancelled == "Yes", 9999, sw$Flight.time.in.minutes)
# df<- sw[-(is.na(sw$Arrival.Delay.in.Minutes)),] df<- na.omit(sw) # (is.na(sw$Arrival.Delay.in.Minutes)) summary(df) str(df) View(df) head(order(-df$X..of.Flight.with.other.Airlines)) write.csv(df, file= 'cleanSatisfactionData.csv') if(sw$Flight.cancelled = 'Yes') { is.na() } 
summary(data00)
