####Inclusion and Well Being 

library(tidyverse)
library(dplyr)
library(wbstats)
library(readxl)
library(countrycode)
library(googlesheets4)


####2.7 -- Population living in a slum (% of urban Population) (2.7)
### Data from UN Habitat available here: https://data.unhabitat.org/pages/housing-slums-and-informal-settlements 
Slum <- read_excel("Data/UNH_Slum_Estimates_2000-2020.xlsx") 
Slum <- 
### Number of people working in the renewable energy sector (5.5)