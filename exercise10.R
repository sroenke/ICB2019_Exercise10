#load libraries
library("ggplot2")
library("cowplot")
library("reshape2")

#set initial vectors/parameters
N<-100
M<-1
r<-0.1
rdN<-(-0.1)
rdM<-0.05
K<-10^6
t<-2
#simulation to equilibrium
while (t<1000) {
  N[t]<-N[t-1]+r*N[t-1]*(1-(N[t-1]+M[t-1])/K)
  M[t]<-M[t-1]+r*M[t-1]*(1-(N[t-1]+M[t-1])/K)
  if (abs(N[t-1]-N[t])>0.1 || abs(M[t-1]-M[t])>0.05){
    t<-t+1
  }
  else{
    t<-t+1000
  }
}

  for (t in (length(N)+1):(length(N)+400)){
    N[t]<-N[t-1]+rdN*N[t-1]*(1-(N[t-1]+M[t-1])/K)
    M[t]<-M[t-1]+rdM*M[t-1]*(1-(N[t-1]+M[t-1])/K)
  }
  
data<-data.frame(t=1:length(M),M=M,N=N)
ggplot(data=data,mapping=aes(x=t, y=N))+geom_line(aes(y=data$M,x=data$t),col='red')+geom_line(aes(y=data$N,x=data$t),col='black')
