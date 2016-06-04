getDomain<-function(x)
{
  domain <- gsub("Tracing route to |*.com |\\[.*\\]", "", x)
  return(domain)
}

getIp <- function(x)
{
  ip <- gsub(".*\\[|\\]", "", x)
  return(ip)
}

getPartion <- function(x)
{
  domain<-getDomain(x[1,])
  ip<-getIp(x[1,])
  #tem<-as.data.frame(matrix(nrow = 1,ncol = 32,data = "*"))
  data_set<-as.data.frame(matrix(nrow = 1,ncol = 32))
  for(i in 1:32)
    data_set[1,i]<-"*"
  data_set[1,31]<-domain
  data_set[1,32]<-ip
  h<-nrow(x)
  for(i in 3:(h-1))
  {
    y<-substr(x[i,],1,4)
    z<-gsub(".*ms *","",x[i,])
    z1<-getIp(x[i,])
    if(isTRUE(length(grep(pattern = "request",z,ignore.case = TRUE))!=0))
      z<-"*"
    if(z!="*")
      data_set[1,as.integer(y)]<-z
    if((nchar(getIp(x[i,])<=15))&&(z>15))
    data_set[1,as.integer(y)]<-z1
  }
  return(data_set)
}
