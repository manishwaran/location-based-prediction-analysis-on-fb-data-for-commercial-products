require(ggplot2)

network_types <- c("Airtel","Reliance","Vodafone","BSNL","Idea","Aircel","Tata_Docomo")

read.dataset <- function(url_string)
{
  dataset <- read.csv(file = url_string,header = TRUE,sep = ",")
  return(dataset)
}


insertRow2 <- function(existingDF, newrow, r) 
{
  existingDF <- rbind(existingDF,newrow)
  existingDF <- existingDF[order(c(1:(nrow(existingDF)-1),r-0.5)),]
  row.names(existingDF) <- 1:nrow(existingDF)
  return(existingDF)  
}

get.location_data<- function(data_set,location)
{
  
  tem_dataset<-as.data.frame(matrix(ncol = ncol(data_set),nrow = 1))
  names(tem_dataset)<-c(names(data_set))
  pattern<-sprintf("*%s*",location)
  print(pattern)
  for(i in 1:nrow(data_set))
  {
    print(data_set[i,"Location"])
    print(i)
    if(isTRUE(length(grep(pattern = pattern,data_set[i,"Location"],ignore.case = TRUE))!=0))
    {
      tem<-c(data_set[i,])
      tem_dataset<-insertRow2(tem_dataset,tem,nrow(tem_dataset)+1)
    }
  }
  return(tem_dataset)
}

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

reform_data_predict <- function(data_set,new_set,location,week,max_week)
{
  for( i in 1:max_week)
  {
    tem<-as.data.frame(matrix(ncol = length(network_types)+2,nrow = 1))
    name_data<-c("Week","Location",network_types)
    names(tem)<-name_data
    tem['Week']<-i
    tem['Location']<-location
    for(j in network_types)
    {
        tem[j]<-get.count_data(data_set[j],data_set['Week'],i)
    }
    new_set<-insertRow2(new_set,tem,nrow(new_set)+1)
  }
  return(new_set)
}

url_string<-readline("Enter the data set path : ")
fb_data<-read.dataset(url_string = url_string)
fb_data
loc<-readline("Enter the city u want to mine : ")
location_data<-get.location_data(fb_data,loc)
new_dataset<-reform_data(location_data)
new_dataset
ggplot(new_dataset, aes(Location, fill=Network)) + geom_bar(position="dodge")
new_data<-reform_data(fb_data)
ggplot(new_data, aes(Location, fill=Network)) + geom_bar(position="dodge")


new_set<-ts(new_set)
plot(new_set,)
new_set <- as.data.frame(matrix(ncol = length(network_types)+2,nrow = 1))
name_data<-c("Week","Location",network_types)
names(new_set)<-name_data
new_set<-reform_data_predict(location_data,new_set,loc,1,5)



#instead of location_data give the dataset having appropriate column name i.e week column