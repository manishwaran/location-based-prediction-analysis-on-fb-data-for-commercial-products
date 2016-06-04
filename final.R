require(ggplot2)
require(curl)

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

get.count <- function(col_set,not)
{
  count<-0
  for(i in 1:length(col_set))
  {
    if(isTRUE(not!=col_set[i]))
    {
      count<-count+1
    }  
  }
  return(count)
}

get.plot_point <- function(data_set)
{
  data_point<-as.list.default(rep(0,length(network_types)))
  names(data_point)<-network_types
  for(i in 1:length(network_types))
  {
    data_point[network_types[i]] <- get.count(data_set[[network_types[i]]],"NA")
  }
  return(data_point)
}

reform_data <- function(data_set)
{
  new_set <- as.data.frame(matrix(ncol = 3,nrow = 1))
  name_data<-c("Name","Location","Network")
  names(new_set)<-name_data
  count<-0
  for(i in 1:nrow(data_set))
  {
    name<-data_set[i,"Names"]
    location<-data_set[i,"Location"]
    for(j in network_types)
    {
      if(isTRUE(data_set[i,j]!="NA"))
      {
        tem<-list(name,location,data_set[[i,j]])
        new_set<-insertRow2(new_set,tem,nrow(new_set)+1)
        count<-count+1
      }
    }
  }
  return(new_set)
}

#loc<-readline("Enter the location : ")
main <- function()
{
  url_string<-readline("Enter the data set path : ")
  fb_data<-read.dataset(url_string = url_string)
  hit<-readline("Hit a key to see the data set : ")
  fb_data
  loc<-readline("Enter the city u want to mine : ")
  #loc<-c("chennai")
  location_data<-get.location_data(fb_data,loc)
  #plot_point<-get.plot_point(location_data)
  new_dataset<-reform_data(location_data)
  print("New Dataset According to user selection is...")
  hit<-readline("Hit a key to see the new dataset : ")
  new_dataset
  #most_reached <- get.most_reached(plot_point)
  hit<-readline("Hit a key to continue..")
  ggplot(new_dataset, aes(Location, fill=Network)) + geom_bar(position="dodge")
  hit<-readline("Hit a key to overall statistics")
  new_data<-reform_data(fb_data)
  ggplot(new_data, aes(Location, fill=Network)) + geom_bar(position="dodge")
}
main()