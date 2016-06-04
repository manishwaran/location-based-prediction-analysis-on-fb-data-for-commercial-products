x<-c(17,23,21,29,33,26,32,31,28)
data<-ts(x,start = 1,frequency = 1)

plot(data)

plot(diff(log10(data)))