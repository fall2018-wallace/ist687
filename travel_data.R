
#data00
traveldata<-data00
traveldata$Departure.Delay.in.Minutes <- ifelse(is.na(traveldata$Departure.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Departure.Delay.in.Minutes)
traveldata$Arrival.Delay.in.Minutes <- ifelse(is.na(traveldata$Arrival.Delay.in.Minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Arrival.Delay.in.Minutes)
traveldata$Flight.time.in.minutes <- ifelse(is.na(traveldata$Flight.time.in.minutes) & traveldata$Flight.cancelled == "Yes", 9999, traveldata$Flight.time.in.minutes)
traveldatacleaned<- traveldata[-(is.na(traveldata$Arrival.Delay.in.Minutes)),] 
traveldatacleaned<- na.omit(traveldata) 
traveldatacleaned$Satisfaction<-as.numeric(traveldatacleaned$Satisfaction)
#summary(traveldatacleaned) 
 
modelofwhole<-lm(formula= Satisfaction~.,data = traveldatacleaned)
#summary(modelofwhole)


modelOfAirlineStatus<-lm(formula = Satisfaction~Airline.Status, data = traveldatacleaned)
#modelOfAirlineStatus
#summary(modelOfAirlineStatus)

modelOfAge<-lm(formula = Satisfaction~Age, data = traveldatacleaned)
#summary(modelOfAge)


#modelOfInfluencingFactors<-lm(formula = Satisfaction ~ Arrival_Delay_greater_5_Mins+Flight_Distance+Flight_time_in_minutes+Flight_cancelled+Arrival_Delay_in_Minutes+Departure_Delay_in_Minutes+Scheduled_Departure_Hour+Destination_State+Destination_City+Origin_State+Orgin_City+Flight_date+Day_of_Month+Class+Eating_and_Drinking_at_Airport+Shopping_Amount_at_Airport+No__of_other_Loyalty_Cards+Type_of_Travel+X__of_Flight_with_other_Airlines+No_of_Flights_p_a_+Year_of_First_Flight+Price_Sensitivity+Gender+Age+Airline_Status,data= traveldatacleaned)
#summary(modelOfInfluencingFactors)
Survey_dataSVM<-traveldatacleaned

dummy<-ifelse(as.numeric(Survey_dataSVM$Satisfaction) > 3, "Happy", "Not Happy") 

Survey_dataSVM$HappyCust <-dummy # Creating a new column and insrting the above generated value.

dim(Survey_dataSVM)

randIndex<-sample(1:dim(Survey_dataSVM)[1]) # Creating a dataframe of random indices

cutPoint2_3<-floor(2*dim(Survey_dataSVM)[1]/3) # Creating a breakpoint of 2/3rd and 1/3rd part

trainData<-Survey_dataSVM[randIndex[1:cutPoint2_3],] # Creating traindata with 2/3rd

testData <-Survey_dataSVM[randIndex[(cutPoint2_3+1):dim(Survey_dataSVM)[1]],] # Creating testdata with 1/3rd 

dim(trainData) # Checking the dimension

dim(testData) # Checking the dimension

svmOutput<-ksvm(HappyCust~Airline_Status+Age , data = trainData, kernel= "rbfdot", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
svmOutput

svmPred <- predict(svmOutput, testData, type = "votes") # Making a Prediction variable based on number of votes

str(svmPred)
head(svmPred[2,])


compTable<-data.frame(testData$HappyCust,svmPred[2,]) # Creating a composite table based on HappyCustomer and svmPrediction
conMatrix<-table(compTable) # Creating a confusion matrix
conMatrix # Displaying the result onto console

errorSum<-conMatrix[1,2]+conMatrix[2,1] # Creating a dataframe containing sum of errors
errorRate<-errorSum/sum(conMatrix)*100 # Creating percentage of error rate
errorRate
