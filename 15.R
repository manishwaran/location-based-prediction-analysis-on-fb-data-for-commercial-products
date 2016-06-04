checkfun <- function()
{
   ip<-readline("Enter the string : ")
   i<-1
   for(i in 1:length(str1)){
    if(grepl(str1[i],ip,ignore.case=TRUE)==TRUE)
    {
           print("The String is matching !")
    }
    i<-i+1
  }
   print("The string is not matching")
}