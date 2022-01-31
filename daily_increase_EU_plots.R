### clean up by removing all variables
rm(list=ls())

## "/Users/eleonoradicarluccio/Desktop/Practicum/Eleonora_Di_Carluccio/data_intermediate/daily_increase_EU.csv"
## "C:/GUMC/Teaching/Practicum_projects/2020/Eleonora_Di_Carluccio/data_intermediate/daily_increase_EU.csv"
library(readr)
library(tidyverse)
daily_increase_EU <- read_csv("C:/GUMC/Teaching/Practicum_projects/2020/Eleonora_Di_Carluccio/data_intermediate/daily_increase_EU.csv",
                                 col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                                  increase_testing_FR = col_number(),
                                                  increase_testing_GE = col_number(),
                                                  increase_testing_IT = col_number(),
                                                  increase_testing_SP = col_number(),
                                                  increase_testing_UK= col_number(),
                                                  
                                                  increase_cases_FR = col_number(),
                                                  increase_cases_GE = col_number(),
                                                  increase_cases_IT = col_number(),
                                                  increase_cases_SP = col_number(),
                                                  increase_cases_UK= col_number(),
                                                  
                                                  increase_death_FR = col_number(),
                                                  increase_death_GE = col_number(),
                                                  increase_death_IT = col_number(),
                                                  increase_death_SP = col_number(),
                                                  increase_death_UK= col_number(),
                                                  
                                                  increase_recovered_FR = col_number(),
                                                  increase_recovered_GE = col_number(),
                                                  increase_recovered_IT = col_number(),
                                                  increase_recovered_SP = col_number(),
                                                  increase_recovered_UK= col_number()))

###draw a plot by 2*2
##"/Users/eleonoradicarluccio/Desktop/Practicum/Eleonora_Di_Carluccio/Figs/plot_daily_increase_EU.pdf"
##"C:/GUMC/Teaching/Practicum_projects/2020/Eleonora_Di_Carluccio/Figs/plot_daily_increase_EU.pdf"
filename <- sprintf("C:/GUMC/Teaching/Practicum_projects/2020/Eleonora_Di_Carluccio/Figs/plot_daily_increase_EU.pdf")
pdf(file = filename, height=15, width=20) #, horizontal = T)
par(mfrow=c(2,2)) 

###plot of daily increasing testing cases###
x<-1:length(daily_increase_EU$Date)
f1<-c(daily_increase_EU$increase_testing_FR)/1000
g1<-c(daily_increase_EU$increase_testing_GE)/1000
i1<-c(daily_increase_EU$increase_testing_IT)/1000
s1<-c(daily_increase_EU$increase_testing_SP)/1000
u1<-c(daily_increase_EU$increase_testing_UK)/1000

plot(x,f1,col='red',type="l",lwd=2.5,
     ylim=c(0, 2000),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Daily testing numbers (k)",
     main="(a) Daily testing numbers",cex.lab=1.75,cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g1,col="blue",type="l",lwd=2.5)
lines(x,i1,col="green",type="l",lwd=2.5)
lines(x,s1,col="cyan",type="l",lwd=2.5)
lines(x,u1,col="orange",type="l",lwd=2.5)
legend(40,2000,c("France","Germany", "Italy", "Spain", "UK"),
       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")


###plot of daily increasing confirmed cases###
x<-1:length(daily_increase_EU$Date)
f2<-c(daily_increase_EU$increase_cases_FR)/1000
g2<-c(daily_increase_EU$increase_cases_GE)/1000
i2<-c(daily_increase_EU$increase_cases_IT)/1000
s2<-c(daily_increase_EU$increase_cases_SP)/1000
u2<-c(daily_increase_EU$increase_cases_UK)/1000

plot(x,f2,col='red',type="l",lwd=2.5,
     ylim=c(0, 120),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Daily confirmed cases (k)",
     main="(b) Daily confirmed cases",cex.lab=1.75,cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g2,col="blue",type="l",lwd=2.5)
lines(x,i2,col="green",type="l",lwd=2.5)
lines(x,s2,col="cyan",type="l",lwd=2.5)
lines(x,u2,col="orange",type="l",lwd=2.5)
##legend(40,120,c("France","Germany", "Italy", "Spain", "UK"),
##       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")


###plot of daily increasing death cases###
x<-1:length(daily_increase_EU$Date)
f3<-c(daily_increase_EU$increase_death_FR)/1000
g3<-c(daily_increase_EU$increase_death_GE)/1000
i3<-c(daily_increase_EU$increase_death_IT)/1000
s3<-c(daily_increase_EU$increase_death_SP)/1000
u3<-c(daily_increase_EU$increase_death_UK)/1000

plot(x,f3,col='red',type="l",lwd=2.5,
     ylim=c(0, 2),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Daily deaths (k)",
     main="(c) Daily deaths",cex.lab=1.75,cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g3,col="blue",type="l",lwd=2.5)
lines(x,i3,col="green",type="l",lwd=2.5)
lines(x,s3,col="cyan",type="l",lwd=2.5)
lines(x,u3,col="orange",type="l",lwd=2.5)
##legend(40,2,c("France","Germany", "Italy", "Spain", "UK"),
##       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")


###plot of daily increasing recovered cases###

x<-1:length(daily_increase_EU$Date)
f4<-c(daily_increase_EU$increase_recovered_FR)/1000
g4<-c(daily_increase_EU$increase_recovered_GE)/1000
i4<-c(daily_increase_EU$increase_recovered_IT)/1000
s4<-c(daily_increase_EU$increase_recovered_SP)/1000
u4<-c(daily_increase_EU$increase_recovered_UK)/1000

plot(x,f4,col='red',type="l",lwd=2.5,
     ylim=c(0, 55),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Daily recovered numbers (k)",
     main="(d) Daily recovered numbers",cex.lab=1.75,cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g4,col="blue",type="l",lwd=2.5)
lines(x,i4,col="green",type="l",lwd=2.5)
lines(x,s4,col="cyan",type="l",lwd=2.5)
lines(x,u4,col="orange",type="l",lwd=2.5)
##legend(40,55,c("France","Germany", "Italy", "Spain", "UK"),
##       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")

graphics.off()
