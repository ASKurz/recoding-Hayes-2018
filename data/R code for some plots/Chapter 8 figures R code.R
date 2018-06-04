
#This generates an interaction plot like Figure 8.6
#This is the code on pages 284-285

x<-c(1.67,3.67,5.33,1.67,3.67,5.33,1.67,3.67,5.33)
w<-c(30,30,30,50,50,50,70,70,70)
y<-c(4.038,4.657,5.171,3.772,4.644,5.362,3.506,4.631,5.565)
wmarker<-c(15,15,15,16,16,16,17,17,17)
plot(y=y,x=x,cex=1.2,pch=wmarker,xlab="Negative Emotions (X)",
ylab="Support for Government Action (Y)")
legend.txt<-c("30 years old","50 years old", "70 years old")
legend("topleft", legend = legend.txt,cex=1,lty=c(1,3,6),lwd=c(2,3,2),pch=c(15,16,17))
lines(x[w==30],y[w==30],lwd=2)
lines(x[w==50],y[w==50],lwd=3,lty=3)
lines(x[w==70],y[w==70],lwd=2,lty=6)



#This generates a JN plot like Figure 8.7.
#This is the code on page 289

age<-c(17,20.5,24,27.5,31,34.5,38,41.5,45,48.5,52,55.5,59,62.5,66,69.5,
        73,76.5,80,83.5,87)
effect<-c(.227,.249,.272,.294,.316,.338,.360,.382,.405,.427,.449,.471,
       .493,.515,.537,.560,.582,.604,.626,.648,.670)
llci<-c(.113,.144,.176,.207,.237,.267,.296,.324,.350,.375,.397,.418,
       .436,.453,.468,.483,.497,.511,.524,.537,.549)
ulci<-c(.342,.354,.367,.381,.395,.409,.424,.441,.459,.479,.500,.524,
       .551,.578,.607,.636,.667,.697,.729,.760,.792)  
plot(age,effect,type="l",pch=19,ylim=c(-.5,1),xlim=c(15,90),lwd=3,
ylab="Conditional effect of negative emotions",
xlab="Age (W)")
points(age,llci,lwd=2,lty=2,type="l")
points(age,ulci,lwd=2,lty=2,type="l")
abline(h=0,untf=FALSE,lty=3,lwd=1)
