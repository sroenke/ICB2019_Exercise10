#load libraries
library("ggplot2")
library("cowplot")
library("reshape2")

#set initial vectors/parameters
N<-99
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
  #if else to stop the simulation when equilibrium is reached
  if (abs(N[t-1]-N[t])>0.1 || abs(M[t-1]-M[t])>0.05){
    t<-t+1
  }
  else{
    t<-t+1000
  }
}
#simulation with drug-induced growthrates 
  for (t in (length(N)+1):(length(N)+400)){
    N[t]<-N[t-1]+rdN*N[t-1]*(1-(N[t-1]+M[t-1])/K)
    M[t]<-M[t-1]+rdM*M[t-1]*(1-(N[t-1]+M[t-1])/K)
  }
#put data together for plotting
data<-data.frame(time=1:length(M),Mutant=M,Normal=N)
ggplot(data=data,mapping=aes(x=time, y=Normal))+geom_line(aes(y=data$Mutant,x=data$t),col='red')+geom_line(aes(y=data$Normal,x=data$t),col='black')+theme(legend.position = "top")
