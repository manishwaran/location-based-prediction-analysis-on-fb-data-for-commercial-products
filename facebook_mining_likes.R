require(Rfacebook)
require(rjson)
require(httpuv)
require(RCurl)
require(httr)
require(graphics)
require(ggplot2)

app_id <- "1670385699866744"
sec_id <- "bb1f9c8cfbcca322a46f5f23a32b7a31"
email <- "manishwaran.np@gmail.com"
password <- "waranmanish54321"

network_types <- c("Airtel","Reliance","Vodafone","BSNL","Idea","Aircel","Tata_Docomo")

url.string <- sprintf("https://graph.facebook.com/oauth/access_token?client_id=%s&client_secret=%s&grant_type=client_credentials",app_id,sec_id)
access_token <- getURL(url.string)

facebook <- function( path = "me", access_token, options)
{
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=", unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  if( regexpr("access_token=", path) <= 0 ){
    data <- getURL( sprintf( "https://graph.facebook.com/%s%s?limit=2&access_token=%s&format=json", path, options, access_token ) )
  } else {
    data <- getURL( sprintf(path) )
    
  }
  fromJSON( data )
}

facebook.login <- function(email, password, verbose=TRUE) 
{
  
  if (!require(RCurl)) stop("Install RCurl before using this function!")
  
  cookie <- tempfile()
  
  CHandle = getCurlHandle()
  curlSetOpt(.opts=list(
    useragent = "Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.0.6) Gecko/2009011913 Firefox/3.0.6 (.NET CLR 3.5.30729)",
    header = 0,
    followlocation = 1,
    ssl.verifypeer = TRUE,
    cookiejar = cookie,
    cookiefile = cookie),
    curl = CHandle, verbose = verbose)
  
  getURL("https://login.facebook.com/login.php?login_attempt=1", curl = CHandle)
  x<-c(1);
  postForm("https://login.facebook.com/login.php?login_attempt=1", curl = CHandle,
           locale = "en_US",
           email = email,
           pass = password)
  return(CHandle) 
}      

get.ids <- function(id)
{
  id_set <- vector(id)
  count <- c(150)
  for(i in 1:count)
  {
    tem_url<-sprintf("%s/friends",id_set[i])
    tem_id <- facebook(tem_url,access_token = access_token)
    id_set<-c(id_set,tem_id$id)
  }
  return(id_set)
}

get.likes<-function(id_vector)
{
  count <- c(length(network_types))
  open.connection("dataset.csv",open = "a+",blocking = TRUE)
  name <- c()
  location <- c()
  nw<-c()
  for(i in 1:length(id_vector))
  {
    nw<-c()
    tem_id<-c(id_vector[i])
    tem_user<-facebook(path=sprintf("%s",id_vector[i]),token = access_token)
    name<-tem_user$name
    location<-tem_user$location
    gender<-tem_user$gender
    tem_url<-sprintf("https://www.facebook.com/%s/likes",tem_name)
    html_file<-getURL(tem_url)
    for(j in 1:count)
    {
      x<-grep(sprintf("*%s*",network_types[j]),html_file,ignore.case = TRUE)
      if(length(x)==0)
      {
        y<-c("NA")
      }
      else
      {
        y<-network_types[j]
      }
      nw<-c(nw,y)
    }
    user_data<-c(name,location,gender,tem_id,y) 
    write.csv(user_data,"dataset.csv",sep = ",")
  }
  close.connection("dataset.csv")
}

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
      print("hai")
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

get.most_reached <- function(data_set)
{
  result_set<-list()
  max_index<-0
  max_value<-0
  for(i in 1:length(x))
  {
    if(as.numeric(data_set[i])>as.numeric(max_value))
    {
      max_value<-data_set[i]
      max_index<-i
    }
  }
  result_set<-max_value
  names(result_set)<-names(data_set[max_index])
  return(result_set)
}

main <- function()
{
  url_string<-readline("Enter the data set path : ")
  fb_data<-read.dataset(url_string = url_string)
  hit<-readline("Hit a key to see the data set : ")
  fb_data
  loc<-readline("Enter the city u want to mine : ")
  loc<-c("chennai")
  location_data<-get.location_data(fb_data,loc)
  plot_point<-get.plot_point(location_data)
  most_reached<-get.most_reached(plot_point)
  print("Most reached product is ")
  print(most_reached)
  tem<-plot_point
  tem[names(most_reached)]<--1
  competitor<-get.most_reached(tem)
  out<-sprintf("Competitor of %s(%d) is %s(%d)",names(most_reached),as.numeric(most_reached),names(competitor),as.numeric(competitor))
  print(out)
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