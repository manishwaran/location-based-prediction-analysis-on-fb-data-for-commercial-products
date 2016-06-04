# the code uses 'facebook' function from the previous gist (https://gist.github.com/1634662) or 
# see the original http://romainfrancois.blog.free.fr/index.php?post/2012/01/15/Crawling-facebook-with-R

access_token <- 'CAACEdEose0cBAMKZAJyEZBuKcOSGsHyvZBBGb28DCH9mPZCwemhPS7rzl3XgCXSjUy6ywRJ4JVDZAdCHS8ZBbr3vFfQiaJUf1NZAiKkiQF6KfZC3wm7C2YNZC93fCIaJeyaXnHZCePgvS49cSaW8b67VoSMtXMCtttIGx8x4pCOSVdisGTlXX2HnpurOtSxJtYOKSIEdBUGnCGJKZBpbZBWThLGg'
require(RCurl)
require(rjson)

# Facebook json function copied from original (Romain Francois) post
facebook <- function( path = "me", access_token, options){
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=", unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  data <- getURL( sprintf( "https://graph.facebook.com/%s%s?access_token=%s", path, options, access_token ) )
  fromJSON( data )
}

# scrape the list of friends
friends <- facebook( path="me/friends" , access_token=access_token)
# extract Facebook IDs
friends.id <- sapply(friends$data, function(x) x$id)
# extract names 
friends.name <- sapply(friends$data, function(x)  iconv(x$name,"UTF-8","ASCII//TRANSLIT"))
# short names to initials 
initials <- function(x) paste(substr(x,1,1), collapse="")
#friends.initial <- sapply(strsplit(friends.name,split = " "), initials) 

# friendship relation matrix
N <- length(friends.id)
friendship.matrix <- matrix(0,N,N)
for (i in 1:N) {
  tmp <- facebook( path=paste("me/mutualfriends", friends.id[i], sep="/") , access_token=access_token)
  mutualfriends <- sapply(tmp$data, function(x) x$id)
  friendship.matrix[i,friends.id %in% mutualfriends] <- 1
}

require(Rgraphviz)
# convert relation matrix to graph
g <- new("graphAM", adjMat=friendship.matrix)

# ellipse graph with initials
pdf(file="facebook1.pdf", width=25, height=25)
attrs <- list(node=list(shape="ellipse", fixedsize=FALSE))
nAttrs <- list(label=initial)
names(nAttrs$label) <- nodes(g)
plot(g, "neato", attrs=attrs, nodeAttrs=nAttrs)
dev.off()