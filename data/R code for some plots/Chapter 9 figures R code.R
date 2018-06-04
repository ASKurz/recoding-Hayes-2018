#This generates an interaction plot like Figure 9.3
#This is the code on page 327

par(mfrow = c(3, 1))
par(mar = c(3, 4, 0, 0),oma = c(2,2,2,2))
par(mgp = c(5, 0.5, 0))
x<-c(1.67,3.67,5.33,1.67,3.67,5.33)
w<-c(0,0,0,1,1,1)
yage30<-c(4.2003,4.6714,5.0624,3.800,4.6801,5.4106)
yage50<-c(3.9943,4.6554,5.2041,3.5941,4.6641,5.5523)
yage70<-c(3.7883,4.6394,5.3458,3.3881,4.6481,5.6940)
legend.txt<-c("Female (W=0)","Male (W=1)")
for (i in 1:3){
if (i==1)
  {y<-yage30
  legend2.txt<-c("Age (Z) = 30")}
if (i==2)
  {y<-yage50
  legend2.txt<-c("Age (Z) = 50")}
if (i==3)
  {y<-yage70
  legend2.txt<-c("Age (Z) = 70")}
plot(y=y,x=x,col="white",ylim=c(3,6),cex=1.5,xlim=c(1,6),tcl=-0.5)
lines(x[w==0],y[w==0],lwd=2,lty=2)
lines(x[w==1],y[w==1],lwd=2,lty=1)
legend("topleft", legend=legend.txt,lwd=2,lty=c(2,1))
legend("bottomright",legend=legend2.txt)
box}
mtext("Negative Emotions (X)",side=1,outer=TRUE)
mtext("Support for Government Action",side=2,outer=TRUE)

#This generates an interaction plot like Figure 9.6.
#This is the code on page 336-338

par(mfrow = c(3, 1))
par(mar = c(3, 4, 0, 0),oma = c(2,2,2,2))
par(mgp = c(5, 0.5, 0))
x<-c(1.67,3.67,5.33,1.67,3.67,5.33)
w<-c(0,0,0,1,1,1)
yage30<-c(4.0561,4.6566,5.1551,3.9422,4.6819,5.2959)
yage50<-c(4.0190,4.6562,5.1850,3.6220,4.6654,5.5314)
yage70<-c(3.9820,4.6557,5.2149,3.3017,4.6489,5.7670)
legend.txt<-c("Female (W=0)","Male (W=1)")
for (i in 1:3){
if (i==1)
  {y<-yage30
  legend2.txt<-c("Age (Z) = 30")}
if (i==2)
  {y<-yage50
  legend2.txt<-c("Age (Z) = 50")}
if (i==3)
  {y<-yage70
  legend2.txt<-c("Age (Z) = 70")}
plot(y=y,x=x,col="white",ylim=c(3,6),cex=1.5,xlim=c(1,6),tcl=-0.5)
lines(x[w==0],y[w==0],lwd=2,lty=2)
lines(x[w==1],y[w==1],lwd=2,lty=1)
legend("topleft", legend=legend.txt,lwd=2,lty=c(2,1))
legend("bottomright",legend=legend2.txt)
box}
mtext("Negative Emotions (X)",side=1,outer=TRUE)
mtext("Support for Government Action",side=2,outer=TRUE)

#This generates a JN plot like Figure 9.7.
#This is the code on pages 340-341

age<-c(17,20.5,24,27.5,31,34.5,38,38.11,41.5,45,48.5,52,55.5,59,62.5,66,69.5,
        73,76.5,80,83.5,87)
effect<-c(-.017,.006,.030,.053,.076,.100,.123,.124,.146,.170,.193,.217,
       .240,.263,.287,.310,.333,.357,.380,.404,.427,.450)
llci<-c(-.247,-.204,-.161,-.120,-.078,-.039,-.001,0,.034,.066,.094,.118,
        .136,.151,.162,.171,.178,.184,.188,.193,.197,.200)
ulci<-c(.212,.216,.220,.225,.231,.238,.247,.248,.259,.273,.292,.315,.344,
        .376,.411,.449,.489,.530,.572,.614,.657,.701)  
plot(age,effect,type="l",pch=19,ylim=c(-1,1.5),xlim=c(15,90),lwd=3,
ylab="Conditional negative emotions by sex interaction",
xlab="Age")
points(age,llci,lwd=2,lty=2,type="l")
points(age,ulci,lwd=2,lty=2,type="l")
abline(h=0,untf=FALSE,lty=3,lwd=1)
abline(v=38.114,untf=FALSE,lty=3,lwd=1)
text(38.114,-1,"38.114",cex=0.8)


