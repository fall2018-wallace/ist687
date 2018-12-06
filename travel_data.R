
#data00
traveldata<-data00
traveldata$Departure.Delay.in.Minutes <- ifelse(is.na(traveldata$Departure.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Departure.Delay.in.Minutes)
traveldata$Arrival.Delay.in.Minutes <- ifelse(is.na(traveldata$Arrival.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Arrival.Delay.in.Minutes)
traveldata$Flight.time.in.minutes <- ifelse(is.na(traveldata$Flight.time.in.minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Flight.time.in.minutes)
traveldatacleaned<- traveldata[-(is.na(traveldata$Arrival.Delay.in.Minutes)),] 
traveldatacleaned<- na.omit(traveldata) 
traveldatacleaned$Satisfaction<-as.numeric(traveldatacleaned$Satisfaction)


modelofwhole<-lm(formula= Satisfaction~.,data = traveldatacleaned)
#summary(modelofwhole)


modelOfAirlineStatus<-lm(formula = Satisfaction~Airline.Status, data = traveldatacleaned)
#modelOfAirlineStatus
#summary(modelOfAirlineStatus)

modelOfAge<-lm(formula = Satisfaction~Age, data = traveldatacleaned)
#summary(modelOfAge)


#modelOfInfluencingFactors<-lm(formula = Satisfaction ~ Arrival_Delay_greater_5_Mins+Flight_Distance+Flight_time_in_minutes+Flight_cancelled+Arrival_Delay_in_Minutes+Departure_Delay_in_Minutes+Scheduled_Departure_Hour+Destination_State+Destination_City+Origin_State+Orgin_City+Flight_date+Day_of_Month+Class+Eating_and_Drinking_at_Airport+Shopping_Amount_at_Airport+No__of_other_Loyalty_Cards+Type_of_Travel+X__of_Flight_with_other_Airlines+No_of_Flights_p_a_+Year_of_First_Flight+Price_Sensitivity+Gender+Age+Airline_Status,data= traveldatacleaned)
#summary(modelOfInfluencingFactors)

# Plotting line graph of Age VS Shopping Amount

ggplot(AgeVsShop, aes(x = AgeVsShop$Age, y = AgeVsShop$Shopping_Amount_at_Airport)) + 
geom_line(stat = "identity", color = "Black") + 
ggtitle("Age vs Shopping Amount Spent") + 
scale_x_continuous(name="Age") + 
scale_y_continuous(name = "Average Amount spent")

# Plotting line graph of Age VS Eating and Drinking Amount

ggplot(AvgVsE_D, aes(x = AvgVsE_D$Age , y = AvgVsE_D$Eating_and_Drinking_at_Airport )) + 
geom_line(stat = "identity", color = "Blue") + 
ggtitle("Age vs Eating & Drinking amount") + 
scale_x_continuous(name="Age") + 
scale_y_continuous(name = "Average Amount spent")

# 
ggplot(dataCleaned, aes(x = dataCleaned$Age, y = dataCleaned$Shopping_Amount_at_Airport)) +
geom_point(size = 3) +
ggtitle("Average Amount spent in Shopping at Airport by different age groups") + 
scale_x_continuous(name="Age") + 
scale_y_continuous(name = "Average Amount spent")

plot(x=store$Age,y=store$Shopping_Amount_at_Airport)

# ggplot(store, aes(x = store1$Age, y = store1$Eating_and_Drinking_at_Airport)) + 
# geom_bar(stat = "identity", color = "Black", fill = "Yellow") + 
# ggtitle("Average Amount spent in Eating & Driking at Airport by different age groups") + 
# scale_x_continuous(name="Age") + 
# scale_y_continuous(name = "Average Amount spent")

