### clean up by removing all variables
rm(list=ls())

library(readr)
library(tidyverse)
### install.packages('zoo')
require(zoo)
COVID_EU_GEN2020_AUGUST2021 <- read_csv(".../COVID_data_EU.csv", 
                     col_types = cols(Date = col_date(format = "%m/%d/%y"), 
                                      France_recovered = col_number(),
                                      Germany_recovered = col_number(),
                                      Italy_recovered = col_number(),
                                      Spain_recovered = col_number(),
                                      UK_recovered = col_number(), 
                                      
                                      France_cases = col_number(),
                                      Germany_cases = col_number(),
                                      Italy_cases = col_number(),
                                      Spain_cases = col_number(),
                                      UK_cases = col_number(), 
                                      
                                      France_tests = col_number(),
                                      Germany_tests = col_number(),
                                      Italy_tests = col_number(),
                                      Spain_tests = col_number(),
                                      UK_tests = col_number(), 
                                      
                                      France_deaths = col_number(),
                                      Germany_deaths = col_number(),
                                      Italy_deaths = col_number(),
                                      Spain_deaths = col_number(),
                                      UK_deaths = col_number()))
##Extracting data from 2020-04-01 to 2021-05-29
COVID_EU <- COVID_EU_GEN2020_AUGUST2021 %>% slice(71:579)

###draw a plot by 2*2
filename <- sprintf(".../plot_daily_total_EU.pdf")
pdf(file = filename, height=15, width=20) #, horizontal = T)
par(mfrow=c(2,2)) 

###plot of daily testing numbers###
x<-1:length(COVID_EU$Date)
f1<-c(COVID_EU$France_tests)/10000
g1<-c(COVID_EU$Germany_tests)/10000
i1<-c(COVID_EU$Italy_tests)/10000
s1<-c(COVID_EU$Spain_tests)/10000
u1<-c(COVID_EU$UK_tests)/10000

plot(x,na.approx(f1, rule=2),col="red",type="l",lwd=2.5,
     ylim=c(0,25000),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Total numbers of people tested (10k)",
     main="(a) Total numbers of people tested", cex.lab=1.65, cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,na.approx(g1, rule=2),col="blue",type="l",lwd=2.5)
lines(x,i1,col="green",type="l",lwd=2.5)
lines(x,na.approx(s1, rule=2),col="cyan",type="l",lwd=2.5)
lines(x,u1,col="orange",type="l",lwd=2.5)
legend(40,25000,c("France","Germany", "Italy", "Spain", "UK"),
       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")


###plot of daily confirmed cases ###
x<-1:length(COVID_EU$Date)
f2<-c(COVID_EU$France_cases)/10000
g2<-c(COVID_EU$Germany_cases)/10000
i2<-c(COVID_EU$Italy_cases)/10000
s2<-c(COVID_EU$Spain_cases)/10000
u2<-c(COVID_EU$UK_cases)/10000

plot(x,f2,col="red",type="l",lwd=2.5,
     ylim=c(0,700),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Total confirmed cases (10k)",
     main="(b) Total confirmed cases", cex.lab=1.65, cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g2,col="blue",type="l",lwd=2.5)
lines(x,i2,col="green",type="l",lwd=2.5)
lines(x,s2,col="cyan",type="l",lwd=2.5)
lines(x,u2,col="orange",type="l",lwd=2.5)
##legend(40,700,c("France","Germany", "Italy", "Spain", "UK"),
##       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")



###plot of daily death numbers###
x<-1:length(COVID_EU$Date)
f3<-c(COVID_EU$France_deaths)/1000
g3<-c(COVID_EU$Germany_deaths)/1000
i3<-c(COVID_EU$Italy_deaths)/1000
s3<-c(COVID_EU$Spain_deaths)/1000
u3<-c(COVID_EU$UK_deaths)/1000

plot(x,f3,col="red",type="l",lwd=2.5,
     ylim=c(0,150),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Total numbers of deaths (k)",
     main="(c) Total numbers of deaths", cex.lab=1.65, cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g3,col="blue",type="l",lwd=2.5)
lines(x,i3,col="green",type="l",lwd=2.5)
lines(x,s3,col="cyan",type="l",lwd=2.5)
lines(x,u3,col="orange",type="l",lwd=2.5)
##legend(40,150,c("France","Germany", "Italy", "Spain", "UK"),
##       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")


###plot of daily recovered numbers###
x<-1:length(COVID_EU$Date)
f4<-c(COVID_EU$France_recovered)/1000
g4<-c(COVID_EU$Germany_recovered)/1000
i4<-c(COVID_EU$Italy_recovered)/1000
s4<-c(COVID_EU$Spain_recovered)/1000
u4<-c(COVID_EU$UK_recovered)/1000

plot(x,f4,col="red",type="l",lwd=2.5,
     ylim=c(0,4500),xaxt="n",
     xlab = "Date (From April 1st, 2020 to August 22nd, 2021)",ylab ="Total numbers of recoverd people (k)",
     main="(d) Total recoverd numbers", cex.lab=1.65, cex.main=3.0)
xtick<-c(1,30,61,91,122,153,183,214,244,275,306, 334, 365, 395, 426, 456, 487)
axis(side=1, at=xtick, labels = FALSE)
lab =c("Apr 1","May 1","June 1","July 1","Aug 1","Sep 1","Oct 1","Nov 1","Dec 1","Jan 1","Feb 1", "Mar 1", "Apr 1", "May 1", "June 1", "July 1", "Aug 1") # label
text(x=xtick, par("usr")[3], labels = lab, srt = 0, pos = 1, xpd = TRUE,cex=1.15)
lines(x,g4,col="blue",type="l",lwd=2.5)
lines(x,i4,col="green",type="l",lwd=2.5)
lines(x,s4,col="cyan",type="l",lwd=2.5)
lines(x,u4,col="orange",type="l",lwd=2.5)
##legend(40,4500,c("France","Germany", "Italy", "Spain", "UK"),
##       lty = c(1,1,1,1,1),lwd=c(2,2,2,2,2),col=c("red","blue", "green", "cyan", "orange"),cex=2,text.width=90,bty="o")


graphics.off()

