
clean_data <- raw_data
clean_data
data0<- clean_data
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

data0$Eating
createFuncition<- function(v)
{
  low<- quantile(v,0.25)
  average<- quantile(v,0.50)
  high<- quantile(v,1)
  vcat<-v
  vcat[1:length(v)]<-"average"
  vcat[v<=low]<-"low"
  vcat[v>low & v<=average]<-"average"
  vcat[v>average & v<=high]<- "high"
  return(vcat)
  }

 quality<-createFuncition(v)
   

   data0<- data.frame(data0, quality)


data0<- data0[-c(21:22)]

###########################
#---------LM---------------
###########################

#Hotel size vs Overall Customer Satisfaction
hsp<-ggplot(data = hotelSurvey1,aes(x=hotelSize,y=overallCustSat))+geom_point()
hsp +geom_jitter()+labs(x="Hotel Size",y="Overall Customer Satisfaction")

#Check_in satisfaction vs Customer Satisfaction
cisp<-ggplot(data = hotelSurvey1,aes(x=checkInSat,y=overallCustSat))+geom_point()
cisp +geom_jitter()+labs(x="Check-In Satisfacton",y="Overall Customer Satisfaction")

#Hotel State vs Customer Satisfaction
hstp<-ggplot(data = hotelSurvey1,aes(x=hotelState,y=overallCustSat))+geom_point()
hstp+geom_jitter() +labs(x="Hotel State",y="Overall Customer Satisfaction")


#Hotel cleanliness vs Customer Satisfaction
hcp<-ggplot(data = hotelSurvey1,aes(x=hotelClean,y=overallCustSat))+geom_point()
hcp +geom_jitter()+labs(x="Hotel State",y="Overall Customer Satisfaction")

#Hotel Friendliness vs Custome Satisfaction
hfp<-ggplot(data = hotelSurvey1,aes(x=hotelFriendly,y=overallCustSat))+geom_point()
hfp +geom_jitter()+labs(x="hotel Friendliness",y="Overall Customer Satisfaction")

#Gender vs Custome Satisfaction
gp<-ggplot(data = hotelSurvey1,aes(x=gender,y=overallCustSat))+geom_point()
gp +geom_jitter()+labs(x="hotelFriendliness",y="Overall Customer Satisfaction")


#Guest Age vs Custome Satisfaction
gap<-ggplot(data = hotelSurvey1,aes(x=guestAge,y=overallCustSat))+geom_point()
gap +geom_jitter()+labs(x="Guest Age",y="Overall Customer Satisfaction")

#Length of stay vs Custome Satisfaction
losp<-ggplot(data = hotelSurvey1,aes(x=lengthOfStay,y=overallCustSat))+geom_point()
losp +geom_jitter()+labs(x="Length of stay",y="Overall Customer Satisfaction")

#When booked Trip vs Custome Satisfaction
wbtp<-ggplot(data = hotelSurvey1,aes(x=whenBookedTrip,y=overallCustSat))+geom_point()
wbtp +geom_jitter()+labs(x="When Booked",y="Overall Customer Satisfaction")

#5.	Next, create one regression model predicting the overall customer
#satisfaction from the other variables (but not the freeText response).
# Refer to page 202 in the text for syntax and explanations of lm( ). 
#Make sure to include all predictors in one model - NOT different models
# each with one predictor.

model1<-lm(formula =overallCustSat~.,data=hotelSurvey1)
summary(model1)

#important aspects to check
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 -- significance
#Multiple R-squared:  0.6702,	Adjusted R-squared:  0.6682  -- 

#6.	Report the R-Squared in a comment. Which of the predictors are statistically significant in the model? 
#In a comment, report the coefficients (AKA slopes or B-weights) for each predictor that is statistically significant. 

#     checkInSat   -  -2.381e-01  Significance 0 which means it has high significance
#     hotelClean  -    4.042e-02  Significance 0 which means it has high significance
#    hotelFriendly-    1.122e+00  Significance 0 which means it has high significance
#    guestAge   -     -1.205e-01  Significance 0 which means it has high significance
#    lengthOfStay-    -3.284e-1   Significance 0 which means it has high significance
#    whenBookedTrip-   6.421e-03  Significance 0 which means it has high significance


#7.	Write a block comment that explains in a narrative your overall
# interpretation of the model. Make sure to refer to each variable 
#(one dependent and three independent) by a descriptive name 
#(i.e., not X1, X2, etc.).

model2 <- lm(formula= overallCustSat ~ checkInSat,data= hotelSurvey)
summary(model2)
#Multiple R-squared:  0.001325	

model3 <- lm(formula= overallCustSat ~ hotelClean,data= hotelSurvey)
summary(model3)
#Multiple R-squared:  0.125

model4 <- lm(formula= overallCustSat ~ hotelFriendly,data= hotelSurvey)
summary(model4)
#Multiple R-squared:  0.3785

mmodel5 <- lm(formula= overallCustSat ~ guestAge,data= hotelSurvey)
summary(model5)
#Multiple R-squared:  0.003735

model6 <- lm(formula= overallCustSat ~ lengthOfStay,data= hotelSurvey)
summary(model6)
#Multiple R-squared:  2.089e-05

model7 <- lm(formula= overallCustSat ~ whenBookedTrip,data= hotelSurvey)
summary(model7)
#Multiple R-squared:  4.482e-05

#The R-Squared value is usually between 0 and 1
#In order to have a credible Linear Model, The R-Squared value should be as near
# as possible to 1 and out of the 6 significant values, guestAge, hotelFriendly, hotelClean
# have R-Squared values near to 1.

#hotelClean    0.125
#hotelFriendly 0.3785
#guestAge      0.003735

#8.	Next, create a different regression model predicting the overall 
#customer satisfaction from the one variable you think is best.
#Then create another using two variables.


# HotelFriendly has best R-Squared value i.e 0.3785 amongst other varaiables
#Modelling for HotelFriendly and Overallcustsat 
model4 <- lm(formula= overallCustSat ~ hotelFriendly,data= hotelSurvey)
summary(model4)

#Creating model with 2 variables

model9<-lm(formula = overallCustSat ~ hotelFriendly + hotelClean,data=hotelSurvey1)
summary(model9)

model10<-lm(formula = overallCustSat ~ hotelFriendly + guestAge,data=hotelSurvey1)
summary(model10)

# the models tell us that hotel-friendly and guest age gives us better prediction 







#################################
#--------------ARM---------------
#################################
