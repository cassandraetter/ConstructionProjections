### Operational Carbon 

library(tidyverse)
library(dplyr)
library(wbstats)
library(readxl)
library(countrycode)




### % with primary reliance on clean fuels and technology for cooking  
###from WHO 
cooking <- read_csv("cooking.csv") 

##create a growth rate function for use. 
cooking <- cooking %>%
        select(SpatialDimValueCode, Period, Dim1, FactValueNumeric) %>%
        arrange(SpatialDimValueCode, Dim1, Period) %>%
        rename("iso3c" = SpatialDimValueCode) 


##### Average growth rate from 1991 to 2020 to be used in predictions

cooking_total <- cooking %>%
        filter(Dim1 == "Total") %>%
        mutate(growth_rate = (FactValueNumeric/lag(FactValueNumeric)) / 100) %>%
        group_by(iso3c) %>%
        summarise(average_growth = mean(growth_rate[!is.infinite(growth_rate)], na.rm = TRUE))

cooking_rural <- cooking %>%
        filter(Dim1 == "Rural") %>%
        mutate(growth_rate = (FactValueNumeric/lag(FactValueNumeric)) / 100 ) %>%
        group_by(iso3c) %>%
        summarise(average_growth = mean (growth_rate[!is.infinite(growth_rate)], na.rm = TRUE)) 
        

cooking_urban <- cooking %>%
        filter(Dim1 == "Urban") %>%
        mutate(growth_rate = (FactValueNumeric/lag(FactValueNumeric)) /100 ) %>%
        group_by(iso3c) %>%
        summarise(average_growth = mean(growth_rate[!is.infinite(growth_rate)], na.rm = TRUE))

#### rejoin dataset and predict using average growth rate assuming linear rate of change 

cooking_total_merged <- cooking %>%
        filter(Period == 2020 & Dim1 == "Total" ) %>%
        left_join(cooking_total, by = "iso3c")

cooking_rural_merged <- cooking %>%
        filter(Period == 2020 & Dim1 == "Rural") %>%
        left_join(cooking_rural, by = "iso3c")

cooking_urban_merged <- cooking %>%
        filter(Period == 2020 & Dim1 == "Urban") %>%
        left_join(cooking_urban, by = "iso3c")

years_to_predict <- seq(2020, 2050, by = 1)

cooking_rural_2050 <- cooking_rural_merged %>%
        crossing(year = years_to_predict) %>%
        mutate(Access = pmin( FactValueNumeric * (1 + average_growth) ^ (year - 2020), 100)) %>%
        rename(Rural_Access = Access) %>%
        select(iso3c, year, Rural_Access)

cooking_urban_2050 <- cooking_urban_merged %>%
        crossing(year = years_to_predict) %>%
        mutate(Access = pmin(FactValueNumeric * (1 + average_growth) ^ (year - 2020), 100)) %>%
        rename(Urban_Access = Access) %>%
        select(iso3c, year, Urban_Access)

cooking_total_2050 <- cooking_total_merged %>%
        crossing(year = years_to_predict) %>%
        mutate(Access = pmin(FactValueNumeric * (1 + average_growth) ^ (year - 2020), 100)) %>%
        rename(Total_Access = Access) %>%
        select(iso3c, year, Total_Access)
###merge datasets back together 
Access_Percent <- cooking_total_2050 %>%
        left_join(cooking_urban_2050, by = c("iso3c", "year")) %>%
        left_join(cooking_rural_2050, by = c("iso3c", "year"))


####Electricity emissions data####

##Find average growth rate for 1980-2021 ## 
elec <- read.csv("Electricity_Consumption_EIA.csv") %>%
        pivot_longer(!c(API, X), names_to = "year", values_to = "bkWh") %>%
        mutate(iso3c = sub(".*-([A-Z]{3})-.*", "\\1", API)) %>%
        mutate(year = str_replace(year, "X", "")) %>%
        rename(Country = X) %>%
        mutate(kWh = as.numeric(bkWh) * 1e9) %>%
        select(Country, iso3c, year, kWh)

world_average_elec<- elec %>%
        filter(str_detect(Country, "World" ))
        
elec_final <- elec %>%
        slice(-(1:84)) %>%
        filter(year >= 1990) %>%
        mutate(growth_rate = kWh/lag(kWh)) %>%
        group_by(iso3c) %>%
        filter(year!= 1990) 

Avg_growth_elec <- elec_final %>%
        summarise(average_growth = coalesce(mean(growth_rate, na.rm = TRUE))/ 100) 

elec_final <- elec_final %>%
        left_join(Avg_growth_elec, by = "iso3c") %>%
        filter(year == 2020) %>%
        filter(!is.na(growth_rate) & !is.nan(growth_rate))

### create a dataframe for years 2021 - 2050 and create predicted Kwh using average growth figure
future_years <- 2020:2050
countries <- unique(elec_final$iso3c)

future_combinations <- crossing(year = future_years, iso3c = countries)

elec_2050 <- elec_final %>%
        left_join(future_combinations, by = "iso3c") %>%
        mutate(predicted_kwh = kWh * (1 + average_growth) ^ (year.y - 2020)) %>%
        select(Country, iso3c, year.y, predicted_kwh)

##Leave this in there but realize this isn't useful, look for other sources not EIA that are more specific to Buildings
###found a paper that takes this data for 2017. 
####Building Operational Emissions Data ####

##data file from Global comparison of building energy use data within the context of climate change
##IEA Database data 2017 pull for 138 countries 

Global_Buildings_Data <- read_excel("Global_Buildings_Data.xlsx") %>%
        rename(Country = 1) %>%
        mutate(iso3c = countrycode(Country, "country.name", "iso3c")) %>%
        subset(Country != "Kosovo" & Country != "World") 

##create regional identifier for continent  
countries <- unique(Global_Buildings_Data$iso3c)
regions <- countrycode(countries, "iso3c", "region")
result <- data.frame(Country = countries, Region = regions)

        
Final_Energy <- Global_Buildings_Data %>%
        select(Country, iso3c, `FEC-Total`, FEC_Residential, `FEC_Commerce and public services`, FEC_Buildings) %>%
        subset(Country != "Unit") %>%
        pivot_longer(c("FEC-Total", "FEC_Residential", "FEC_Commerce and public services", "FEC_Buildings"), names_to = "Final Energy Consumption", values_to = "ktoe") %>%
        mutate(tCO2e = as.numeric(ktoe) * 100) %>%
        left_join(result, by = c("iso3c" = "Country")) %>%
        filter(`Final Energy Consumption` == "FEC_Buildings")


Total_Carbon <- Global_Buildings_Data %>%
        select(Country, iso3c, Total_CarbonE, Residential_CarbonE, `Commerce and public services_CarbonE`, Building_Carbon) %>%
        subset(Country != "Unit") %>%
        pivot_longer(c("Total_CarbonE", "Residential_CarbonE", "Commerce and public services_CarbonE", "Building_Carbon"), names_to = "Carbon Emissions", values_to = "MtCO2e") %>%
        mutate(Year = 2017, tCO2e = as.numeric(MtCO2e) * 1e6)%>%
        left_join(result, by = c("iso3c" = "Country")) %>%
        filter(`Carbon Emissions` == "Building_Carbon")
       


Direct_Carbon_Emissions <- Global_Buildings_Data %>%
        select(Country, iso3c, Residential_DirectCE, `Commerce and public services_DirectCE`, Building_DirectCE) %>%
        subset(Country != "Unit") %>%
        pivot_longer(c("Residential_DirectCE", "Commerce and public services_DirectCE", "Building_DirectCE"), names_to = "Direct Carbon Emissions", values_to = "MtCO2e") %>%
        mutate(Year = 2017, tCO2e = as.numeric(MtCO2e) * 1e6) %>%
        left_join(result, by = c("iso3c" = "Country")) %>%
        filter(`Direct Carbon Emissions` == "Building_DirectCE")


###approach to 2050 taking operational emissions to be constant. building floor space to change of total emissions 

bs_2050 <- read_csv("~/Downloads/BS_2050.csv") %>%
        select(`Country Name`, `iso3c`, year, `total_building_stock_m2` ) 


operational_2050 <- bs_2050 %>%
        %>%
        # rename("Building Energy Consumption (toe)" = tCO2e.x, "Building Carbon Emissions (tCO2e)" = tCO2e.y, "Direct Building Emissions (tCO2e)"= tCO2e) %>%
        mutate(`BEC (toe/m2)` = `Building Energy Consumption (toe)`/ `total_building_stock_m2`, 
               `BCE (tCO2e/m2)` = `Building Carbon Emissions (tCO2e)` / `total_building_stock_m2`, 
               `DBE (tCO2e/m2)` = `Direct Building Emissions (tCO2e)` / `total_building_stock_m2`) %>%
        select(`Country Name`, `iso3c`,`year`, `total_building_stock_m2`, `Building Energy Consumption (toe)`, 
               `BEC (toe/m2)`,`Building Carbon Emissions (tCO2e)`, `BCE (tCO2e/m2)`, 
               `Direct Building Emissions (tCO2e)` , `DBE (tCO2e/m2)`)


final_bs <- bs_2050 %>% filter(year == 2020) %>%
        left_join(Final_Energy %>% select(iso3c, `Building Energy Consumption (toe)` = tCO2e), by = "iso3c") %>%
        left_join(Total_Carbon %>% select(iso3c, `Building Carbon Emissions (tCO2e)` = tCO2e), by = "iso3c") %>%
        left_join(Direct_Carbon_Emissions %>% select(iso3c, `Direct Building Emissions (tCO2e)` = tCO2e), by = "iso3c") %>%
        mutate(`BEC (toe/m2)` = `Building Energy Consumption (toe)`/ `total_building_stock_m2`, 
               `BCE (tCO2e/m2)` = `Building Carbon Emissions (tCO2e)` / `total_building_stock_m2`, 
               `DBE (tCO2e/m2)` = `Direct Building Emissions (tCO2e)` / `total_building_stock_m2`) %>%
        select(iso3c, `BEC (toe/m2)`, `BCE (tCO2e/m2)`, `DBE (tCO2e/m2)`) %>%
        left_join(bs_2050, by = "iso3c") %>%
        mutate(`Building Energy Consumption (toe)` = `BEC (toe/m2)` * total_building_stock_m2,
               `Building Carbon Emissions (tCO2e)` = `BCE (tCO2e/m2)` * total_building_stock_m2,
               `Direct Building Emissions (tCO2e)` = `DBE (tCO2e/m2)` * total_building_stock_m2) %>%
        arrange(iso3c, year) %>%
        select(`Country Name`, iso3c, year, everything())


###this provides a 2017 figure for Final Energy Consumption, Total Carbon Emissions, and Direct Carbon Emissions. To find 2050 estimate need to secure how they are projected to decrease or increase to 2050. 

#Need rate of change information for FEC (Total, Residential, Commercial, Buildings)
#Need rate of change information for Total Carbon (Total, Residential, Commercial, Buildings)
#Need rate of change information for Direct Emissions (Residential, Commercial, Buildings)

#Europe and Central Asia
#Middle East and North Africa
#Sub saharan Africa
#LATAM and Caribbean
#East Asia and Pacific
#South Asia
#North America



####Combine Datasets for Operational Carbon ####

access <- read_csv("Access_2050.csv") %>%
        rename("iso3c" = "Country", "year" = "Year")
operational_2050_combined <- access %>% 
        left_join(operational_2050, by = c("iso3c", "year"))

write_csv(operational_2050_combined, "Operational_Emissions_Estimates.csv")        
#write_csv(Access_Percent, "Access_Percent.csv")
