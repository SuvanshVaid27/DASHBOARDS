# ---- Loading libraries ----
library("shiny")
library("shinydashboard")
library("tidyverse")
library("leaflet")
library("plotly")
library("DT")
library("fs")
library("wbstats")
library("fpp3")
library("htmltools")
library("highcharter")


source("utils.R", local = T)


downloadData <- function() {
  download.file(
    url      = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip",
    destfile = "data/covid19_data.zip"
  )
  
  data_path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
  unzip(
    zipfile   = "data/covid19_data.zip",  
    files     = paste0(data_path, c("confirmed_global.csv", "deaths_global.csv", "recovered_global.csv")),
    exdir     = "data",
    junkpaths = T
  )
}

updateData <- function() {
  # Download data from Johns Hopkins (https://github.com/CSSEGISandData/COVID-19) if the data is older than 0.5h
  if (!dir_exists("data")) {
    dir.create('data')
    downloadData()
  } else if ((!file.exists("data/covid19_data.zip")) || (as.double(Sys.time() - file_info("data/covid19_data.zip")$change_time, units = "hours") > 0.5)) {
    downloadData()
  }
}

# Update with start of app
updateData()

data_confirmed <- read_csv("data/time_series_covid19_confirmed_global.csv")
data_deceased  <- read_csv("data/time_series_covid19_deaths_global.csv")
data_recovered <- read_csv("data/time_series_covid19_recovered_global.csv")

# Confirmed cases Australia
confirmed_aus <- data_confirmed %>%
  filter(`Country/Region` == "Australia") %>%
  pivot_longer(cols = -c('Country/Region', 'Province/State', 'Lat', 'Long'),
               names_to = "Date",
               values_to = "count") %>%  
  mutate(State = `Province/State`) %>%
  select(c('State', 'Date', 'Lat', 'Long', 'count'))

confirmed_aus['Date'] <- as.Date(confirmed_aus$Date, "%m/%d/%Y")

confirmed_aus <- as_tsibble(confirmed_aus, key = State, index = Date)

confirmed_latest <- confirmed_aus %>% filter(Date == max(Date))

total_confirmed <- sum(confirmed_latest$count)

# Deaths Australia
deaths_aus <- data_deceased %>%
  filter(`Country/Region` == "Australia") %>%
  pivot_longer(cols = -c('Country/Region', 'Province/State', 'Lat', 'Long'),
               names_to = "Date",
               values_to = "count") %>%  
  mutate(State = `Province/State`) %>%
  select(c('State', 'Date', 'Lat', 'Long', 'count'))

deaths_aus['Date'] <- as.Date(deaths_aus$Date, "%m/%d/%Y")

deaths_aus <- as_tsibble(deaths_aus, key = State, index = Date)

deaths_latest <- deaths_aus %>% filter(Date == max(Date))

total_deaths <- sum(deaths_latest$count)

# Recovered cases Australia
recovered_aus <- data_recovered %>%
  filter(`Country/Region` == "Australia") %>%
  pivot_longer(cols = -c('Country/Region', 'Province/State', 'Lat', 'Long'),
               names_to = "Date",
               values_to = "count") %>%  
  mutate(State = `Province/State`) %>%
  select(c('State', 'Date', 'Lat', 'Long', 'count'))

recovered_aus['Date'] <- as.Date(recovered_aus$Date, "%m/%d/%Y")

recovered_aus <- as_tsibble(recovered_aus, key = State, index = Date)

recovered_latest <- recovered_aus %>% filter(Date == max(Date))

total_recovered <- sum(recovered_latest$count)


# latest cases

latest_confirmed <- confirmed_aus %>% filter(Date==max(Date)) %>% rename(Confirmed = count)
latest_recovered <- recovered_aus %>% filter(Date==max(Date)) %>% rename(Recovered = count)
latest_deaths <- deaths_aus %>% filter(Date==max(Date)) %>% rename(Deaths = count)

latest_data <- cbind(latest_confirmed, Recovered = latest_recovered$Recovered, Deaths = latest_deaths$Deaths)

total_active <- sum(latest_data$Confirmed) - sum(latest_data$Recovered)

# Data from previous day

prev_confirmed <- confirmed_aus %>% filter(Date == max(Date) - 1) %>% rename(Confirmed = count)
prev_recovered <- recovered_aus %>% filter(Date == (max(Date) - 1)) %>% rename(Recovered = count)
prev_deaths <- deaths_aus %>% filter(Date == (max(Date) - 1))  %>% rename(Deaths = count)

prev_data <- cbind(prev_confirmed, Recovered = prev_recovered$Recovered, Deaths = prev_deaths$Deaths)

# Increased cases 

final_data <- latest_data %>% mutate(Confirmed_new = Confirmed - prev_data$Confirmed, Recovered_new = Recovered - prev_data$Recovered, Deaths_new = Deaths - prev_data$Deaths)

remove(prev_confirmed, prev_recovered, prev_deaths, latest_confirmed, latest_recovered, latest_deaths, confirmed_latest, recovered_latest, deaths_latest)


