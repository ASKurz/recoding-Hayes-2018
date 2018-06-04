
#This generates a plot of the indirect and direct effects like Figure 12.7
#This is the code on page 452

x<-c(0,1,0,1,0,1)
w<-c(1.592,1.592,2.800,2.800,5.200,5.200)
y<-c(0.184,0.224,0.202,-0.001,0.238,-0.446)
plot(y=y,x=w,pch=15,col="white",
xlab="Climate change skepticism",
ylab="Effect of frame on willingness to donate")
legend.txt<-c("Direct effect","Indirect effect")
legend("bottomleft",legend=legend.txt,lty=c(1,3),lwd=c(4,3))
lines(w[x==0],y[x==0],lwd=4,lty=1)
lines(w[x==1],y[x==1],lwd=4,lty=3)
abline(0,0,lwd=0.5,lty=2)