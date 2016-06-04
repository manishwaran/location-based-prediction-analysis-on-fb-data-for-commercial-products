library(Rfacebook)

library(Rook)

require(RCurl)

require(rjson)

access_token = "CAACEdEose0cBALbfuuWmtv43HxcqOQM83h1G3XKZBfoQl8ATqlVjbIklYqd6mp6kuMHRwcDdmzfLnqrJ6ezgudLuQUTrRKXPPTlXwnLH1q03VCfaYS7yveM2mWocZCRUHoSBSDhpZBE29Ib9rzMarqFd2AztB2HvkDziSaHLrD8iR7G0bx4CVwQxSvtdgaJZCUPsFHEdHsTJQ7kMCNCg"

myFB = getUsers("me", token = access_token)

myFriends = getFriends(access_token, simplify=FALSE)

myFriends_info <- getUsers(myFriends$id, token=access_token, private_info=TRUE)

table(myFriends_info$relationship_status)
