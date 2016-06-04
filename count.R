network_types <- c("Airtel","Reliance","Vodafone","BSNL","Idea","Aircel","Tata_Docomo")

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
  for(i in length(network_types))
  {
    data_point[network_types[i]] <- get.count(data_set[network_types[i]],"NA")
  }
  return(data_point)
}