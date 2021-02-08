# Conor Cuddihy  
# 14415362
# Assignment 4 - GGPLOT2 

library(lubridate)
library(tibble)
library(aimsir17)
library(dplyr)
library(ggplot2)

get_daily_summary <- function(dayNo, stat, attribute, f){
  filtered_data <- subset(my_obs, DayNumber==dayNumber & station == stat)
  sapply(filtered_data[,attribute],f, na.rm=TRUE)
}

# Create you own copy
my_obs <- subset(observations,
                 station %in% c("DUBLIN AIRPORT","MACE HEAD","SherkinIsland","BELMULLET"))

view(my_obs)

my_obs$DayNumber <- yday(my_obs$date)
# (2) ADD NEW COLUMN HERE FOR SEASON
seasons = function(x){
  if(x %in% 2:4) return("02_Spring")
  if(x %in% 5:7) return("03_Summer")
  if(x %in% 8:10) return("04_Autumn")
  if(x %in% c(11,12,1)) return("01_Winter") 
}
my_obs$season <- sapply(month(my_obs$month), seasons)

view(my_obs)

# Create an aggregate tibble for the summaries                  
daily_obs <- tibble(
  DayNumber       = numeric(),
  Day             = numeric(),
  Month           = numeric(),
  Station         = character(),
  TotalRainfall   = numeric(),
  MaxRainfall     = numeric(),
  MinRainfall     = numeric(),
  MeanMSL         = numeric(),
  MaxMSL          = numeric(),
  MinMSL          = numeric(),
  MeanTemperature = numeric(),
  MaxTemperature  = numeric(),
  MinTemperature  = numeric(),
  MeanWindspeed   = numeric(),
  MaxWindspeed    = numeric(),
  MinWindspeed    = numeric(),
  Season          = character()
)

# Populate the new tibble
for (dayNumber in unique(my_obs$DayNumber)){
  cat("Processing Day Number",dayNumber,"\n")
  for(stations in unique(my_obs$station)){
    cat("Processing station",stations,"\n")
    # (3) Add row of data here - all tibble columns must be added.
    daily_obs <- dplyr::add_row(daily_obs,
                                DayNumber = dayNumber,
                                Day             = my_obs[which(my_obs$DayNumber==dayNumber),"day",drop=T][1],
                                Month           = my_obs[which(my_obs$DayNumber==dayNumber),"month",drop=T][1],
                                Station         = my_obs[which(my_obs$station==stations),"station",drop=T][1],
                                TotalRainfall   = get_daily_summary(dayNumber, stations, "rain",sum),
                                MaxRainfall     = get_daily_summary(dayNumber, stations, "rain",max),
                                MinRainfall     = get_daily_summary(dayNumber, stations, "rain",min),
                                MeanMSL         = get_daily_summary(dayNumber, stations, "msl",mean),
                                MaxMSL          = get_daily_summary(dayNumber, stations, "msl",max),
                                MinMSL          = get_daily_summary(dayNumber, stations, "msl",min),
                                MeanTemperature = get_daily_summary(dayNumber, stations, "temp",mean),
                                MaxTemperature  = get_daily_summary(dayNumber, stations, "temp",max),
                                MinTemperature  = get_daily_summary(dayNumber, stations, "temp",min),
                                MeanWindspeed   = get_daily_summary(dayNumber, stations, "wdsp",mean),
                                MaxWindspeed    = get_daily_summary(dayNumber, stations, "wdsp",max),
                                MinWindspeed    = get_daily_summary(dayNumber, stations, "wdsp",min),
                                Season          = my_obs[which(my_obs$DayNumber==dayNumber),"season",drop=T][1])
  }
}

view(daily_obs)

#### starting Plots

## 
plot_1 <- ggplot(daily_obs, aes(Day, MeanTemperature, color = Station)) + 
  geom_line() +
facet_grid(vars(daily_obs$Month), vars(daily_obs$Station)) +
  ggtitle("Plot 01: Annual Mean Temperatures for each Station") +
  xlab("Day of Month") + 
  ylab("Mean Temperature")
plot_1

## 
plot_2 <- ggplot(daily_obs, aes(MeanMSL, TotalRainfall, color = Station)) + 
  geom_point() +
  facet_grid(vars(daily_obs$Season), vars(daily_obs$Station)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  ggtitle("Plot 02: Mean Sea Level Pressure v Total Rainfall by Station and Season") +
  xlab("Mean Sea Level Pressure") + 
  ylab("Total Rainfall")
plot_2

## 
plot_3 <- ggplot(daily_obs, aes(DayNumber, MeanTemperature, color = Season)) + 
  geom_point() +
  facet_wrap(daily_obs$Station ~ .) +
  ggtitle("Plot 03: Mean Daily Temperature by Station and Season") +
  xlab("Day of Year") + 
  ylab("Mean Daily Temperature")
plot_3

## 
plot_4 <- ggplot(daily_obs, aes(Season, MeanTemperature, color = Season)) + 
  geom_boxplot()  +
  facet_wrap(daily_obs$Station ~ .) +
  ggtitle("Plot 04: Mean Daily Temperature Distribution by Station and Season") +
  xlab("Season") + 
  ylab("Mean Daily Temperature")
plot_4

## 
plot_5 <- ggplot(daily_obs, aes(Season, MaxTemperature, color = Season)) + 
  geom_boxplot()  +
  facet_wrap(daily_obs$Station ~ .) +
  ggtitle("Plot 05: Max Daily Temperature Distribution by Station and Season") +
  xlab("Season") + 
  ylab("Max Daily Temperature")
plot_5

## 
plot_6 <- ggplot(daily_obs, aes(Season, MinTemperature, color = Season)) + 
  geom_boxplot()  +
  facet_wrap(daily_obs$Station ~ .) +
  ggtitle("Plot 06: Min Daily Temperature Distribution by Station and Season") +
  xlab("Season") + 
  ylab("Min Daily Temperature")
plot_6

## 
plot_7 <- ggplot(daily_obs, aes(Season, TotalRainfall, color = Season)) + 
  geom_boxplot()  +
  facet_wrap(daily_obs$Station ~ .) +
  ggtitle("Plot 07: Total Rainfall Distribution by Station and Season") +
  xlab("Season") + 
  ylab("Total Rainfall")
plot_7

##  
plot_8 <- ggplot(daily_obs, aes(Station,MaxWindspeed, color = Station)) + 
  geom_boxplot()  +
  facet_grid(cols = vars(Month)) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Plot 08: Max Daily Windspeed by Month") +
  xlab("Month") + 
  ylab("Maximum Wind Speed")
plot_8 

##  
plot_9 <- ggplot(daily_obs, aes(Station,MeanMSL, color = Station)) + 
  geom_boxplot()  +
  facet_grid(cols = vars(Month)) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Plot 09: Mean Sea Level Pressure Per Month") +
  xlab("Month") + 
  ylab("Mean Sea Level Pressure")
plot_9

##  
plot_10 <- ggplot(daily_obs, aes(MeanMSL,MaxWindspeed, color = Station)) + 
  geom_point()  +
  facet_grid(rows = vars(Station), cols = vars(Season)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Plot 10: Mean Sea Level v Maximum Windspeed") +
  xlab("Mean Sea Level Pressure") + 
  ylab("Maximum Windspeed")
plot_10

##  
plot_11 <- ggplot(daily_obs, aes(MaxWindspeed,TotalRainfall, color = Station)) + 
  geom_point()  +
  facet_grid(rows = vars(Station), cols = vars(Season)) +
  geom_smooth(method = "loess", formula = y ~ x) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Plot 11: Max Windspeed v Total Rainfall") +
  xlab("Maximum Windspeed") + 
  ylab("Total Rainfall")
plot_11 

## Generate own plot
## For my own plot I chose to do a heat map of the Mean Temperature depending on the 
## station and DayNumber("Day of the Year"). 

my_plot_12 <- ggplot(daily_obs, aes(x = Station, y = DayNumber)) +
  geom_tile(aes(fill = MeanTemperature)) +
  scale_fill_continuous(low = "violetred", high = "aquamarine") +
  ggtitle("Plot 12: Heat Map of Mean Temperature depending on station and DayNumber(Day of the Year)") +
  xlab("Station") + 
  ylab("DayNumber")
my_plot_12 
















