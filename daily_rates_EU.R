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

##Population size (refer to READ_ME.md)
France_population <- 65273511
Germany_population <- 83783942
Italy_population <- 60461826
Spain_population <- 46754778
UK_population <- 67886011
##Extracting data from 2020-04-01 to 2021-05-29
COVID_EU <- COVID_EU_GEN2020_AUGUST2021 %>% slice(71:579)
##Daily rate dataset:
Date <- COVID_EU$Date
##Daily Recovered rate per 100 people
Recovered_rate_FR <- COVID_EU$France_recovered/COVID_EU$France_cases*100
Recovered_rate_GE <- COVID_EU$Germany_recovered/COVID_EU$Germany_cases*100
Recovered_rate_IT <- COVID_EU$Italy_recovered/COVID_EU$Italy_cases*100
Recovered_rate_SP <- COVID_EU$Spain_recovered/COVID_EU$Spain_cases*100
Recovered_rate_UK <- COVID_EU$UK_recovered/COVID_EU$UK_cases*100

Recovered_Rate_EU <- data.frame(Recovered_rate_FR,Recovered_rate_GE,Recovered_rate_IT, Recovered_rate_SP, Recovered_rate_UK)
##Daily Incident rate per 100 people
Incident_rate_FR_app <- COVID_EU$France_cases/COVID_EU$France_tests*100
Incident_rate_FR <- na.approx(Incident_rate_FR_app, rule=2)
Incident_rate_GE_app <- COVID_EU$Germany_cases/COVID_EU$Germany_tests*100
Incident_rate_GE <- na.approx(Incident_rate_GE_app, rule=2)
Incident_rate_IT <- COVID_EU$Italy_cases/COVID_EU$Italy_tests*100
Incident_rate_SP_app <- COVID_EU$Spain_cases/COVID_EU$Spain_tests*100
Incident_rate_SP <- na.approx(Incident_rate_SP_app, rule=2)
Incident_rate_UK_app <- COVID_EU$UK_cases/COVID_EU$UK_tests*100
Incident_rate_UK <- na.approx(Incident_rate_UK_app, rule=2)

Incident_rate_EU <- data.frame(Incident_rate_FR,Incident_rate_GE,Incident_rate_IT,Incident_rate_SP,Incident_rate_UK)
##Daily Testing rate per 100 people
Testing_rate_FR_app <- COVID_EU$France_tests/France_population*100
Testing_rate_FR <- na.approx(Testing_rate_FR_app, rule=2)
Testing_rate_GE_app <- COVID_EU$Germany_tests/Germany_population*100
Testing_rate_GE <- na.approx(Testing_rate_GE_app, rule=2)
Testing_rate_IT <- COVID_EU$Italy_tests/Italy_population*100
Testing_rate_SP_app <- COVID_EU$Spain_tests/Spain_population*100
Testing_rate_SP <- na.approx(Testing_rate_SP_app, rule=2)
Testing_rate_UK_app <- COVID_EU$UK_tests/UK_population*100
Testing_rate_UK <- na.approx(Testing_rate_UK_app, rule=2)

Testing_rate_EU <- data.frame(Testing_rate_FR,Testing_rate_GE,Testing_rate_IT,Testing_rate_SP,Testing_rate_UK)
##Daily Mortality rate per 100 people
Mortality_rate_FR <- COVID_EU$France_deaths/COVID_EU$France_cases*100
Mortality_rate_GE <- COVID_EU$Germany_deaths/COVID_EU$Germany_cases*100
Mortality_rate_IT <- COVID_EU$Italy_deaths/COVID_EU$Italy_cases*100
Mortality_rate_SP <- COVID_EU$Spain_deaths/COVID_EU$Spain_cases*100
Mortality_rate_UK <- COVID_EU$UK_deaths/COVID_EU$UK_cases*100

Mortality_rate_EU <- data.frame(Mortality_rate_FR ,Mortality_rate_GE,Mortality_rate_IT,Mortality_rate_SP,Mortality_rate_UK)
##Daily rate dataset:
Daily_rates <- data.frame (Date, Incident_rate_EU, Recovered_Rate_EU, Testing_rate_EU, Mortality_rate_EU)
write.csv(Daily_rates, file=".../daily_rate_EU.csv", row.names = FALSE) 
