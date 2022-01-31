### clean up by removing all variables
rm(list=ls())

library(readr)
library(tidyverse)
## "/Users/eleonoradicarluccio/Desktop/Practicum/Eleonora_Di_Carluccio/data_intermediate/daily_rate_EU.csv"
##"C:/GUMC/Teaching/Practicum_projects/2020/Eleonora_Di_Carluccio/data_intermediate/daily_rate_EU.csv"
time_series_Covid_19 <- read_csv("/Users/eleonoradicarluccio/Desktop/Practicum/Eleonora_Di_Carluccio/data_intermediate/daily_rate_EU.csv",
                                 col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                  Incident_rate_FR = col_number(),
                                                  Incident_rate_GE = col_number(),
                                                  Incident_rate_IT = col_number(),
                                                  Incident_rate_SP = col_number(),
                                                  Incident_rate_UK = col_number(),
                                                  
                                                  Recovered_rate_FR = col_number(),
                                                  Recovered_rate_GE = col_number(),
                                                  Recovered_rate_IT = col_number(),
                                                  Recovered_rate_SP = col_number(),
                                                  Recovered_rate_UK = col_number(),
                                                  
                                                  Testing_rate_FR = col_number(),
                                                  Testing_rate_GE = col_number(),
                                                  Testing_rate_IT = col_number(),
                                                  Testing_rate_SP = col_number(),
                                                  Testing_rate_UK = col_number(),
                                                  
                                                  Mortality_rate_FR = col_number(),
                                                  Mortality_rate_GE = col_number(),
                                                  Mortality_rate_IT = col_number(),
                                                  Mortality_rate_SP = col_number(),
                                                  Mortality_rate_UK = col_number()))
                                                  
###draw a plot by 2*2
##"/Users/eleonoradicarluccio/Desktop/Practicum/Eleonora_Di_Carluccio/Figs/plot_daily_rates_EU.pdf"
##"C:/GUMC/Teaching/Practicum_projects/2020/Eleonora_Di_Carluccio/Figs/plot_daily_rates_EU.pdf"
filename <- sprintf("/Users/eleonoradicarluccio/Desktop/Practicum/Eleonora_Di_Carluccio/Figs/plot_daily_rates_EU.pdf")
pdf(file = filename, height=15, width=20) #, horizontal = T)
par(mfrow=c(2,2)) 

###plot of testing_rate###
x<-1:length(time_series_Covid_19$Date)
f1<-c(time_series_Covid_19$Testing_rate_FR)
g1<-c(time_series_Covid_19$Testing_rate_GE)
i1<-c(time_series_Covid_19$Testing_rate_IT)
s1<-c(time_series_Covid_19$Testing_rate_SP)
u1<-c(time_series_Covid_19$Testing_rate_UK)

plot(x, f1, type = "l", col="red", lwd=2.5,
     ylim=c(0,350),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Testing rate (%)",
     main="(a) Testing rates",cex.lab=1.75,cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g1,col="blue",type="l",lwd=2.5)
lines(x,i1,col="green",type="l",lwd=2.5)
lines(x,s1,col="cyan",type="l",lwd=2.5)
lines(x,u1,col="orange",type="l",lwd=2.5)
legend(40,350,c("France","Germany", "Italy", "Spain", "UK"),
       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")


###plot of Incidence_rate###
x<-1:length(time_series_Covid_19$Date)
f2<-c(time_series_Covid_19$Incident_rate_FR)
g2<-c(time_series_Covid_19$Incident_rate_GE)
i2<-c(time_series_Covid_19$Incident_rate_IT)
s2<-c(time_series_Covid_19$Incident_rate_SP)
u2<-c(time_series_Covid_19$Incident_rate_UK)

plot(x,f2, type = "l", col="red", lwd=2.5,
     ylim=c(0,30),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Test positivity rate (%)",
     main="(b) Test positivity rates",cex.lab=1.75,cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g2,col="blue",type="l",lwd=2.5)
lines(x,i2,col="green",type="l",lwd=2.5)
lines(x,s2,col="cyan",type="l",lwd=2.5)
lines(x,u2,col="orange",type="l",lwd=2.5)
legend(144,30,c("France","Germany", "Italy", "Spain", "UK"),
       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")


###plot of mortality_rate###
x<-1:length(time_series_Covid_19$Date)
f3<-c(time_series_Covid_19$Mortality_rate_FR)
g3<-c(time_series_Covid_19$Mortality_rate_GE)
i3<-c(time_series_Covid_19$Mortality_rate_IT)
s3<-c(time_series_Covid_19$Mortality_rate_SP)
u3<-c(time_series_Covid_19$Mortality_rate_UK)

plot(x, f3, type = "l", col="red", lwd=2.5,
     ylim=c(0,25),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Mortality rate (%)",
     main="(c) Mortality rates",cex.lab=1.75,cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g3,col="blue",type="l",lwd=2.5)
lines(x,i3,col="green",type="l",lwd=2.5)
lines(x,s3,col="cyan",type="l",lwd=2.5)
lines(x,u3,col="orange",type="l",lwd=2.5)
legend(183,25,c("France","Germany", "Italy", "Spain", "UK"),
       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")


###plot of recovered_rate###
x<-1:length(time_series_Covid_19$Date)
f4<-c(time_series_Covid_19$Recovered_rate_FR)
g4<-c(time_series_Covid_19$Recovered_rate_GE)
i4<-c(time_series_Covid_19$Recovered_rate_IT)
s4<-c(time_series_Covid_19$Recovered_rate_SP)
u4<-c(time_series_Covid_19$Recovered_rate_UK)

plot(x, f4, type = "l", col="red", lwd=2.5,
     ylim=c(0,100),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Recovery rate (%)",
     main="(d) Recovery rates",cex.lab=1.75,cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g4,col="blue",type="l",lwd=2.5)
lines(x,i4,col="green",type="l",lwd=2.5)
lines(x,s4,col="cyan",type="l",lwd=2.5)
lines(x,u4,col="orange",type="l",lwd=2.5)
legend(365,50,c("France","Germany", "Italy", "Spain", "UK"),
       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")


dev.off()
