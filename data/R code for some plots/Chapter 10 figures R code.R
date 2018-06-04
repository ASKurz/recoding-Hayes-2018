
#This generates an interaction plot like Figure 10.3
#This is the code on pages 366

x<-c(0,1,2,0,1,2,0,1,2)
w<-c(4.25,4.25,4.25,5.12,5.12,5.12,5.896,5.896,5.896)
y<-c(5.698,5.400,5.513,5.287,5.773,5.779,4.920,6.105,6.016)
plot(y=y,x=w,pch=15,col="white",xlab="Perceived pervasiveness of sex 
discrimination (W)",ylab="Liking of the attorney (Y)")
legend.txt<-c("No protest","Individual protest","Collective protest")
legend("topleft",legend=legend.txt,lty=c(1,3,1),lwd=c(4,4,1))
lines(w[x==0],y[x==0],lwd=4,lty=1)
lines(w[x==1],y[x==1],lwd=4,lty=3)
lines(w[x==2],y[x==2],lwd=1,lty=1)
