
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

hist(data0)
