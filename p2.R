get.count_data <- function(my_set_net,my_set_week,week)
{
count<-0
for(i in 1:nrow(my_set_net))
{
if(isTRUE(my_set_net[i,1]!="NA"))
{
  if(isTRUE(my_set_week[i,1]==week)){
    count<-count+1}
  }
}
return(count)
}
reform_data_predict <- function(data_set,new_set,week,max_week)
{
for( i in 1:max_week)
{
tem<-as.data.frame(matrix(ncol = length(network_types)+1,nrow = 1))
name_data<-c("Week",network_types)
names(tem)<-name_data
tem['Week']<-i
for(j in network_types)
{
  tem[j]<-get.count_data(data_set[j],data_set['Week'],i)
}
new_set<-insertRow2(new_set,tem,nrow(new_set)+1)
}
return(new_set)
}

new_set <- as.data.frame(matrix(ncol = length(network_types)+1,nrow = 1))
name_data<-c("Week",network_types)
names(new_set)<-name_data
new_set<-reform_data_predict(fb_data,new_set,1,22)
new_set<-new_set[-1,]
new_set
new_set<-ts(new_set)
plot(new_set[,-1])
url<-readline("Enter the Network : ")
predict_arima(new_set[,url],4)
