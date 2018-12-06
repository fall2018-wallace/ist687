
#data00
traveldata<-raw_data
traveldata$Departure.Delay.in.Minutes <- ifelse(is.na(traveldata$Departure.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Departure.Delay.in.Minutes)
traveldata$Arrival.Delay.in.Minutes <- ifelse(is.na(traveldata$Arrival.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Arrival.Delay.in.Minutes)
traveldata$Flight.time.in.minutes <- ifelse(is.na(traveldata$Flight.time.in.minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Flight.time.in.minutes)
traveldatacleaned<- traveldata[-(is.na(traveldata$Arrival.Delay.in.Minutes)),] 
traveldatacleaned<- na.omit(traveldata) 
traveldatacleaned$Satisfaction<-as.numeric(traveldatacleaned$Satisfaction)

summary(traveldata)
#modelofwhole<-lm(formula= Satisfaction~.,data = traveldatacleaned)
#summary(modelofwhole)


#modelOfAirlineStatus<-lm(formula = Satisfaction~Airline.Status, data = traveldatacleaned)
#modelOfAirlineStatus
#summary(modelOfAirlineStatus)

#modelOfAge<-lm(formula = Satisfaction~Age, data = traveldatacleaned)
#summary(modelOfAge)
#Flight.cancelled   #Flight.time.in.minutes #Flight.Distance
#modelOfLoyalty<-lm(formula = Satisfaction~No..of.other.Loyalty.Cards,data = traveldatacleaned)
#modelOfInfluencingFactors<-lm(formula = Satisfaction ~ Flight_time_in_minutes+Flight_cancelled+Arrival_Delay_in_Minutes+Departure_Delay_in_Minutes+Scheduled_Departure_Hour+Destination_State+Destination_City+Origin_State+Orgin_City+Flight_date+Day_of_Month+Class+Eating_and_Drinking_at_Airport+Shopping_Amount_at_Airport+No__of_other_Loyalty_Cards+Type_of_Travel+X__of_Flight_with_other_Airlines+No_of_Flights_p_a_+Year_of_First_Flight+Price_Sensitivity+Gender+Age+Airline_Status,data= traveldatacleaned)
#summary(modelOfInfluencingFactors)
###############################################################################
#modelOfLoyalty<-lm(formula = Satisfaction~No..of.other.Loyalty.Cards,data = traveldatacleaned)
#summary(modelOfLoyalty)
#modelOfAirlineStatus<-lm(formula = Satisfaction~Airline.Status, data = traveldatacleaned)
#summary(modelOfAirlineStatus)
#lmCustomers <- lm(formula=Satisfaction~Age+Price.Sensitivity+Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport, data = traveldatacleaned)
#summary(lmCustomers)
#lmCustomers <- lm(formula=Satisfaction~Price.Sensitivity, data = traveldatacleaned)
#summary(lmCustomers)
#modelOfFTIM<-lm(formula = Satisfaction~Flight.time.in.minutes,data = traveldatacleaned)
#summary(modelOfFTIM)
#modelOfADDIM<-lm(formula = Satisfaction~Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes,data = traveldatacleaned)
#summary(modelOfADDIM)
#modelOfDOM<-lm(formula = Satisfaction~Day.of.Month,data = traveldatacleaned)
#summary(modelOfDOM)
#modelOfQ<-lm(formula = Satisfaction~Shopping.Amount.at.Airport+Eating.and.Drinking.at.Airport,data = traveldatacleaned)
#summary(modelOfQ)
#modelOfClass<-lm(formula = Satisfaction~Class,data = traveldatacleaned)
#summary(modelOfClass)
#modelOfNOF<-lm(formula = Satisfaction~No.of.Flights.p.a.,data = traveldatacleaned)
#summary(modelOfNOF)
#modelOfNOF<-lm(formula = Satisfaction~No.of.Flights.p.a.,data = traveldatacleaned)
#summary(modelOfNOF)
#modelOfAge<-lm(formula = Satisfaction~Age,data = traveldatacleaned)
#summary(modelOfAge)
#modelOfGender<-lm(formula = Satisfaction~Gender,data = traveldatacleaned)
#summary(modelOfGender)
#modelOfAS<-lm(formula = Satisfaction~Airline.Status,data = traveldatacleaned)
#summary(modelOfAS)
#modelOfFD<-lm(formula = Satisfaction~Flight.Distance,data = traveldatacleaned)
#summary(modelOfFD)
#modelOfALL<-lm(formula = Satisfaction~.,data = traveldatacleaned)
#summary(modelOfALL)
#modelOfCancelled<-lm(formula = Satisfaction~Flight.cancelled,data = traveldatacleaned)
#summary(modelOfCancelled)
#modelOfSDH<-lm(formula = Satisfaction~Scheduled.Departure.Hour,data = traveldatacleaned)
#summary(modelOfSDH)
#modelOfOC<-lm(formula = Satisfaction~Orgin.City,data = traveldatacleaned)
#summary(modelOfOC)
#modelOfOS<-lm(formula = Satisfaction~Origin.State,data = traveldatacleaned)
#summary(modelOfOS)
#modelOfDC<-lm(formula = Satisfaction~Destination.City,data = traveldatacleaned)
#summary(modelOfDC)
#modelOfDS<-lm(formula = Satisfaction~Destination.State,data = traveldatacleaned)
#summary(modelOfDS)
#modelOfFD<-lm(formula = Satisfaction~Flight.date,data = traveldatacleaned)
#summary(modelOfFD)
##########################################################################################                 
#index <- 1:nrow(traveldatacleaned)
#testindex <- sample(index, trunc(length(index)/3))
#testset <- traveldatacleaned[testindex,]
#trainset <- traveldatacleaned[-testindex,]
#svm.model <- svm(Satisfaction ~ ., data = trainset, cost = 100, gamma = 1)
#svm.pred <- predict(svm.model, testset[,-10])


