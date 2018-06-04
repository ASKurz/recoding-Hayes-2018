
#This generates an interaction plot like Figure 7.7
#This is the code on page 247

x<-c(0,1,0,1,0,1)
w<-c(1.592,1.592,2.80,2.80,5.20,5.20)
y<-c(2.619,2.377,2.746,2.747,2.998,3.482)
plot(y=y,x=w,pch=15,col="white",xlab="Climate Change Skepticism (W)",
ylab="Negative Justifications (Y)")
legend.txt<-c("Natural causes (X=0)","Climate change (X=1)")
legend("topleft",legend=legend.txt,lty=c(3,1),lwd=c(3,2))
lines(w[x==0],y[x==0],lwd=3,lty=3)
lines(w[x==1],y[x==1],lwd=2,lty=1)

#This generates a JN plot like Figure 7.9
#This is the code on pages 258-259
 
skeptic<-c(1,1.171,1.4,1.8,2.2,2.6,3,3.4,3.8,3.934,4.2,
        4.6,5,5.4,5.8,6.2,6.6,7,7.4,7.8,8.2,8.6,9)
effect<-c(-.361,-.327,-.281,-.200,-.120,-.039,.041,.122,.202,.229,.282,
       .363,.443,.524,.604,.685,.765,.846,.926,1.007,1.087,1.168,1.248)
llci<-c(-.702,-.654,-.589,-.481,-.376,-.276,-.184,-.099,-.024,0,.044,.105,
       .161,.212,.261,.307,.351,.394,.436,.477,.518,.558,.597)
ulci<-c(-.021,0,.028,.080,.136,.197,.266,.343,.428,.458,.521,.621,.726,.836,
       .948,1.063,1.180,1.298,1.417,1.537,1.657,1.778,1.899)  
plot(skeptic,effect,type="l",pch=19,ylim=c(-1,1.5),xlim=c(1,6),lwd=3,
ylab="Conditional effect of disaster frame",
xlab="Climate Change Skepticism (W)")
points(skeptic,llci,lwd=2,lty=2,type="l")
points(skeptic,ulci,lwd=2,lty=2,type="l")
abline(h=0,untf=FALSE,lty=3,lwd=1)
abline(v=1.171,untf=FALSE,lty=3,lwd=1)
abline(v=3.934,untf=FALSE,lty=3,lwd=1)
text(1.171,-1,"1.171",cex=0.8)
text(3.934,-1,"3.934",cex=0.8)
