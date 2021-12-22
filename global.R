library(shiny)
library(tidyverse)
# library(flexdashboard)
library(shinyWidgets)
library(countrycode)
library(shinythemes)
library(data.table)
library(bslib)
library(echarts4r)
# library(shinycssloaders)
library(Cairo)
options(shiny.usecairo=T)

# setwd("/Users/Kate/OneDrive/Vaccine report/Vaccine-projections")

# Load data and income group classification
data_owid <- fread("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(iso_code, location, date, people_fully_vaccinated_per_hundred, continent) %>% as.data.frame()
income <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/scripts/input/wb/income_groups.csv") %>%
  rename("iso3c" = "Code", "income" = "Income group") %>% select(-Year, -Country)
wb_names <- readRDS("./data/country_name_world_bank.RDS")

income <- rbind(income, data.frame(iso3c = "OWID_WRL", income = "World"))

# Change country names and filter by start of vaccination campaign
data_owid <- data_owid %>%
  filter(date > as.Date("2021-01-01")) %>%
  select(-location) %>%
  merge(., wb_names, by.x="iso_code", by.y="iso3c", all=T) %>%
  rename("location" = "country") %>%
  arrange(location, date)

# Change continent designations and create averages
continent_calculation <- data_owid %>%
  mutate(continent = ifelse(!iso_code %in% c("USA", "CAN") & continent == "North America", "Latin America and Caribbean",
                           ifelse(continent == "South America", "Latin America and Caribbean", continent))) %>%
  filter(continent %in% c("North America", "Latin America and Caribbean")) %>%
  group_by(iso_code) %>%
  mutate(people_fully_vaccinated_per_hundred = zoo::na.locf(people_fully_vaccinated_per_hundred, na.rm=F)) %>%
  group_by(continent, date) %>%
  mutate(people_fully_vaccinated_per_hundred = mean(people_fully_vaccinated_per_hundred, na.rm=T)) %>%
  slice(1) %>%
  mutate(location = continent,
         iso_code = ifelse(location == "Latin America and Caribbean", "OWID_SAM", "OWID_NAM")) %>%
  drop_na()

continent_calculation <- data_owid %>%
  filter(!location %in% c("South America", "North America", "Asia", "Africa", "Europe", "Oceania")) %>%
  filter(!continent %in% c("South America", "North America")) %>%
  filter(!nchar(continent) < 1) %>%
  arrange(iso_code, date) %>%
  group_by(iso_code) %>%
  mutate(people_fully_vaccinated_per_hundred = zoo::na.locf(people_fully_vaccinated_per_hundred, na.rm=F)) %>%
  group_by(continent, date) %>%
  mutate(people_fully_vaccinated_per_hundred = mean(people_fully_vaccinated_per_hundred, na.rm=T)) %>%
  slice(1) %>%
  mutate(location = continent,
         iso_code = ifelse(location == "Africa", "OWID_AFR", 
                           ifelse(location == "Asia", "OWID_ASI",
                                  ifelse(location == "Europe", "OWID_EUR", "OWID_OCE")))) %>%
  rbind(., continent_calculation)

data_owid <- data_owid %>%
  filter(!location %in% c("South America", "North America", "Asia", "Africa", "Europe", "Oceania")) %>%
  rbind(., continent_calculation)

rm(continent_calculation)

# Take lag of 2 days to get closer to full reporting
# Pull last observation forward and calculate average for income groups
data_tidy_raw <- data_owid %>%
  rename("iso3c" = "iso_code") %>%
  merge(., income, by="iso3c") %>%
  filter(date > as.Date("2020-12-01") & date < (Sys.Date() - 2)) %>%
  select(location, income, date, people_fully_vaccinated_per_hundred) %>%
  arrange(location, date) %>%
  group_by(location) %>%
  mutate(people_fully_vaccinated_per_hundred = zoo::na.locf(people_fully_vaccinated_per_hundred, na.rm=F)) %>%
  group_by(income, date) %>%
  summarize(people_fully_vaccinated_per_hundred = mean(people_fully_vaccinated_per_hundred, na.rm=T)) %>%
  ungroup()

# Create dynamic date list
date_choices <- data_tidy_raw %>%
  select(date) %>%
  mutate(date = as.Date(date),
         date2 = format(date, format="%B %Y")) %>%
  arrange(date) %>%
  group_by(date2) %>%
  slice_head(n=1) %>%
  arrange(date) %>%
  mutate(date = as.character(date)) %>%
  filter(!date2 == "December 2020")

date_choices <- unique(date_choices$date2)

# Create region list
continent_list <- list("Regions" = c("Africa", "Asia", "Europe", "Oceania", "Latin America and Caribbean", "North America"))

# Remove small countries & create country list 
drops <- c("Hong Kong SAR, China", "Isle of Man", "Saint Martin (French part)",
           "Macao SAR, China", "Palestinian Territories", "Sint Maarten", "Solomon Islands",
           "Gibraltar", "Northern Cyprus", "West Bank and Gaza", "Korea, Dem. People's Rep.")

names_list <- income %>%
  merge(., wb_names, by="iso3c", all = T) %>%
  drop_na() %>%
  select(-iso3c) %>%
  filter(!country %in% drops)

country_list <- list("Countries" = unique(names_list$country))

# Set blank options and create input choice lists
blank_text1 <- "Select option 1"
blank_text2 <- "Select option 2"

choice_list1 <- c(blank_text1,
                 continent_list,
                 country_list)

choice_list2 <- c(blank_text2,
                  continent_list,
                  country_list)

# Miscellaneous values
date_with_lag <- format(Sys.Date()-2, format="%d %B, %Y")
max_val <- 105

# Verify data is arranged correctly
data_owid <- data_owid %>% arrange(location, date)
data_tidy_raw <- data_tidy_raw %>% arrange(income, date)

# Clean environment
rm(wb_names, income, drops, country_list)

