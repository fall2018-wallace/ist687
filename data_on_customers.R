
data11<-data0



lmCustomers <- lm(formula=Satisfaction~Age+Price.Sensitivity+Eating.and.Drinking.at.Airport, data = data0)
summary(lmCustomers)
