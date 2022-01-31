### clean up by removing all variables
rm(list=ls())

library(readr)
library(tidyverse)
## "/Users/eleonoradicarluccio/Desktop/Practicum/Eleonora_Di_Carluccio/data/COVID_data_EU.csv"
## "C:/GUMC/Teaching/Practicum_projects/2020/Eleonora_Di_Carluccio/data/COVID_data_EU.csv"
COVID_EU_GEN2020_AUGUST2021 <- read_csv("C:/GUMC/Teaching/Practicum_projects/2020/Eleonora_Di_Carluccio/data/COVID_data_EU.csv", 
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
##Supplementary data
France_tests_approx <- na.approx(COVID_EU$France_tests, rule=2)
Germany_tests_approx <- na.approx(COVID_EU$Germany_tests, rule=2)
Spain_tests_approx <- na.approx(COVID_EU$Spain_tests, rule=2)
Date <- COVID_EU$Date

###increase testing###
increase_testing_FR<- c(0,diff(France_tests_approx))
increase_testing_GE<- c(0,diff(Germany_tests_approx))
increase_testing_IT<- c(0,diff(COVID_EU$Italy_tests))
increase_testing_SP<- c(0,diff(Spain_tests_approx))
increase_testing_UK<- c(0,diff(COVID_EU$UK_tests))

increase_testing_EU <- data.frame (increase_testing_FR, increase_testing_GE, increase_testing_IT, increase_testing_SP, increase_testing_UK)
increase_testing_EU[increase_testing_EU<0]=0

###increase confirmed###
increase_cases_FR<- c(0,diff(COVID_EU$France_cases))
increase_cases_GE<- c(0,diff(COVID_EU$Germany_cases))
increase_cases_IT<- c(0,diff(COVID_EU$Italy_cases))
increase_cases_SP<- c(0,diff(COVID_EU$Spain_cases))
increase_cases_UK<- c(0,diff(COVID_EU$UK_cases))

increase_cases_EU <- data.frame(increase_cases_FR,increase_cases_GE,increase_cases_IT,increase_cases_SP, increase_cases_UK)
increase_cases_EU[increase_cases_EU<0]=0

###increase deaths###
increase_death_FR<- c(0,diff(COVID_EU$France_deaths))
increase_death_GE<- c(0,diff(COVID_EU$Germany_deaths))
increase_death_IT<- c(0,diff(COVID_EU$Italy_deaths))
increase_death_SP<- c(0,diff(COVID_EU$Spain_deaths))
increase_death_UK<- c(0,diff(COVID_EU$UK_deaths))

increase_death_EU <- data.frame(increase_death_FR, increase_death_GE, increase_death_IT, increase_death_SP, increase_death_UK)
increase_death_EU[increase_death_EU<0]=0

###increase recovered###
increase_recovered_FR <- c(0,diff(COVID_EU$France_recovered))
increase_recovered_GE <- c(0,diff(COVID_EU$Germany_recovered))
increase_recovered_IT <- c(0,diff(COVID_EU$Italy_recovered))
increase_recovered_SP <- c(0,diff(COVID_EU$Spain_recovered))
increase_recovered_UK <- c(0,diff(COVID_EU$UK_recovered))

increase_recovered_EU <- data.frame(increase_recovered_FR, increase_recovered_GE, increase_recovered_IT, increase_recovered_SP, increase_recovered_UK)
increase_recovered_EU[increase_recovered_EU<0]=0

##Daily rate dataset:
Daily_increase <- data.frame (Date, increase_testing_EU, increase_cases_EU,  increase_death_EU, increase_recovered_EU )
write.csv(Daily_increase, file="C:/GUMC/Teaching/Practicum_projects/2020/Eleonora_Di_Carluccio/data_intermediate/daily_increase_EU.csv", row.names = FALSE) 

##"C:/GUMC/Teaching/Practicum_projects/2020/Eleonora_Di_Carluccio/data_intermediate/daily_increase_EU.csv"
##"/Users/eleonoradicarluccio/Desktop/Practicum/Eleonora_Di_Carluccio/data_intermediate/daily_increase_EU.csv"
