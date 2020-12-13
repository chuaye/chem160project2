alpha<-0.00218 #(person-1day-1)
gamma.male<-0.05 #(day-1)
gamma.female<-0.007 #(day-1)
alpha.male<-0.000006
alpha.female<-0.0000009
Im.hist<- c()
If.hist<- c()
Sm.hist<- c()
Sf.hist<- c()
Im<-1000
If<-1000
Sm<-14000
Sf<-9000

for (day in 1:2000) {
    Im.hist[day]<-Im  	
    If.hist[day]<-If
    Sm.hist[day]<-Sm
    Sf.hist[day]<-Sf
    
    #Equations from the PDF
    delta.Im <- (alpha.male*Sm*If-gamma.male*Im)
    delta.If <- (alpha.female*Sf*Im-gamma.female*If)
    delta.Sm <- (gamma.male*Im-alpha.male*Sm*If)  
    delta.Sf <- (gamma.female*If-alpha.female*Sf*Im)
    
    Im<-Im+delta.Im
    If<-If+delta.If
    Sm<-Sm+delta.Sm
    Sf<-Sf+delta.Sf
    

    ## Ensure Sm, Sf, Im, If > 0
    Im<-max(Im,0)
    If<-max(If,0)
    Sm<-max(Sm,0) 
    Sf<-max(Sf,0) 
    
}
#blue(men) pink(women)
plot(Im.hist,col=4,type="l",main="Multi-Population SIS Model",ylim=c(0,5000),ylab="Population of Infected",xlab="Days")
lines(If.hist,col=6)


