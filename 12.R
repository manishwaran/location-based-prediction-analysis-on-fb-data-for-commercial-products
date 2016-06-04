require(RCurl)
require(bitops)

facebook.login <- function(email="manishwaran.np@gmail.com", password="waranmanish54321", verbose=TRUE) {
  # login user to facebook, return curl handle (CHandle)
  
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



get.friends <- function(id="100002137599179", CHandle) {
  # get all friends of 'id'
  
  n.friends <- function(id, CHandle) {
    # return number of friends of user 'id'
    url.id <- paste("http://www.facebook.com/friends/?id=",id,sep="")
    tmp <- getURL(url.id, curl=CHandle)
    as.numeric(sub(".*has ([0-9]*) friends.*","\\1",tmp))
  }
  
  get.friends.start.x <- function(id, x, CHandle) {
    # return friends of user 'id' starting from x
    
    url.id <- paste("http://www.facebook.com/friends/?id=",id,"&flid=&view=everyone&q=&nt=0&nk=0&s=",x,"&st=0", sep="")
    tmp <- getURL(url.id, curl=CHandle)
    
    tempname<-tempfile()
    writeLines(tmp,tempname) # jsem prase
    eee<-readLines(tempname)
    file.remove(tempname)
    ddd <- eee[grep("popup.php\\?id=[0-9]*",eee)]
    if (length(ddd)==0) {warning("No friends found."); return(c())}
    
    mask <- 'id=\\\\"f[0-9][0-9]*'
    coords <- gregexpr(mask, ddd)
    start <- unlist(coords)
    end <- start + attr(coords[[1]],"match.length") - 1
    friends <- substr(rep(ddd,length(start)), start+6, end)
    
    as.numeric(friends)
  }  
  
  # get.friends starts here
  
  N <- n.friends(id, CHandle) # number of friends
  output <- c()
  
  if (!is.na(N))  
    for (i in 0:floor((N-1)/400)) { 
      # Facebook reports in blocks of 400 friends
      output <- c(output, get.friends.start.x(id, i*400, CHandle))
    }
  
  output
}  

get.members <- function(id, N, CHandle) { 
  # get all members of group or page, n = total number of members/fans
  # not working if you are an admin of the group/page
  
  get.members.start.x<-function(id,x,CHandle) {
    url.id <- paste("http://www.facebook.com/social_graph.php?node_id=",id,"&class=FanManager&start=", x, sep="")
    tmp <- getURL(url.id, curl=CHandle)
    
    tempname<-tempfile()
    writeLines(tmp,tempname) # jsem prase
    eee<-readLines(tempname)
    file.remove(tempname)
    
    # found all members that can become an admin
    ddd <- eee[grep("actionspro_li",eee)]
    if (length(ddd)==0) {warning("No members found."); return(c())}
    
    members <- sub(".*id=([0-9]*).*","\\1",ddd)
    # hack, if only add memder posible, use this one
    problematic.members <- substr(members,1,1)=="<" # sub is not working
    members[problematic.members] <- sub(".*;:([0-9]*)...>Add as Friend.*","\\1",members[problematic.members])
    as.numeric(members) 
  }
  
  output <- c()
  
  for (i in 0:floor((N-1)/10)) { 
    # Facebook reports in blocks of 400 friends
    output <- c(output, get.members.start.x(id, i*10, CHandle))
  }
  
  output  
  
} 


