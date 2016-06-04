#access token from https://developers.facebook.com/tools/explorer
access_token="CAAXvNPpHSHgBAE38x6nzBuIAzQYgn0IQliN8Bgk3ujbg4NSxTzJxxvau0ZAnz5YcNQ5PK7DHN1qUduSZCQHi4OkaSa8L8Y5LyZA1lDC4W0ZBD5LK7kBL7MUgIv1HjK11e3LHN5AZCw1v7zzjDyskU35ZAYOzEZBxY0jfoeGI1KZAOaBUS5cDZCD8yS2EVkOIMN7KUk5E2mXfl7h1KRZAywD7Fm"
require(RCurl)
require(rjson)
# download the file needed for authentication http://www.brocktibert.com/blog/2012/01/19/358/
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
# http://romainfrancois.blog.free.fr/index.php?post/2012/01/15/Crawling-facebook-with-R
facebook <- function( path = "me", access_token = token, options){
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=", unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  data <- getURL( sprintf( "https://graph.facebook.com/%s%s&access_token=%s", path, options, access_token ), cainfo="cacert.pem" )
  fromJSON( data )
}

# see http://applyr.blogspot.in/2012/01/mining-facebook-data-most-liked-status.html

# scrape the list of friends
friends <- facebook( path="me/friends" , access_token=access_token)
# extract Facebook IDs
friends.id <- sapply(friends$data, function(x) x$id)
# extract names 
friends.name <- sapply(friends$data, function(x)  iconv(x$name,"UTF-8","ASCII//TRANSLIT"))
# short names to initials 
initials <- function(x) paste(substr(x,1,1), collapse="")
friends.initial <- sapply(strsplit(friends.name," "), initials)

# friendship relation matrix
#N <- length(friends.id)
N <- 200
friendship.matrix <- matrix(0,N,N)
for (i in 1:N) {
  tmp <- facebook( path=paste("me/mutualfriends", friends.id[i], sep="/") , access_token=access_token)
  mutualfriends <- sapply(tmp$data, function(x) x$id)
  friendship.matrix[i,friends.id %in% mutualfriends] <- 1
}
require(network)
net1<- as.network(friendship.matrix)
plot(net1, label=friends.initial, arrowhead.cex=0)