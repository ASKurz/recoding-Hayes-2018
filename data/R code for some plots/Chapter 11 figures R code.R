
#This generates a plot of the indirect and direct effects like Figure 11.7
#This is the code on page 423

x<-c(0,1,0,1,0,1)
w<-c(-0.531,-0.531,-0.060,-0.060,0.600,0.600)
y<-c(0.366,-0.161,0.366,-0.405,0.366,-0.746)
plot(y=y,x=w,pch=15,col="white",
xlab="Nonverbal negative expressivity",
ylab="Effect of dysfunctional team behavior")
legend.txt<-c("Direct effect","Indirect effect")
legend("bottomleft",legend=legend.txt,lty=c(1,3),lwd=c(4,3))
lines(w[x==0],y[x==0],lwd=4,lty=1)
lines(w[x==1],y[x==1],lwd=4,lty=3)
abline(0,0,lwd=0.5,lty=2)