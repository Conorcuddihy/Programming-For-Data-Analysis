# Conor Cuddihy 
# 14415362
# MSc Data Analytics

install.packages("aimsir17")
install.packages("dplyr")
install.packages("ggplot2")
library(lubridate)
library(tibble)
library(aimsir17)
library(dplyr)
library(ggplot2)

target <- c("MACE HEAD","JOHNSTOWNII","ROCHES POINT")

my_obs <- filter(observations, station == target)

my_obs <- my_obs %>% 
  mutate(quarter = case_when(month == 1 ~ "Q1",
                        month == 2 ~"Q1",
                        month == 3 ~ "Q1",
                        month == 4 ~"Q2",
                        month == 5 ~ "Q2",
                        month == 6 ~"Q2",
                        month == 7 ~ "Q3",
                        month == 8 ~"Q3",
                        month == 9 ~ "Q3",
                        month == 10 ~"Q4",
                        month == 11 ~ "Q4",
                        month == 12 ~"Q4"))

View(my_obs)

weather  <- my_obs %>%
  group_by(station, month, day, quarter) %>%
  summarise(avrMSL = mean(msl,na.rm=T)) 

weather <- weather[,c("station", "month", "day", "avrMSL", "quarter")]
weather

power <- eirgrid17 %>%
  group_by(month, day) %>%
  summarise(MeanWindPower = mean(IEWindGeneration,na.rm=T)) 
power

j_data <- full_join(power, weather)
j_data

plot_1 <- ggplot(j_data, aes(x=avrMSL, y=MeanWindPower)) + 
  geom_point() + 
  geom_smooth() +
  ggtitle("Plot 1: avrMSL v MeanWindPower") +
  xlab("avrMSL") + 
  ylab("MeanWindPower")
plot_1

plot_2 <- ggplot(j_data, aes(x=avrMSL, y=MeanWindPower, colour = quarter)) + 
  geom_point() + 
  geom_smooth(method = 'loess',formula = 'y ~ x') +
  ggtitle("Plot 2: avrMSL v MeanWindPower Depending on Quarters") +
  xlab("avrMSL") + 
  ylab("MeanWindPower")
plot_2

plot_3 <- ggplot(j_data, aes(x=avrMSL, y=MeanWindPower, colour = station)) + 
  geom_point() + 
  geom_smooth(method = 'loess',formula = 'y ~ x') +
  ggtitle("Plot 3: avrMSL v MeanWindPower Depending on Station") +
  xlab("avrMSL") + 
  ylab("MeanWindPower")
plot_3

plot_4 <- ggplot(j_data, aes(x=avrMSL, y=MeanWindPower)) + 
  geom_point() + 
  facet_grid(rows = vars(station), cols = vars(quarter)) +
  geom_smooth(method = 'loess',formula = 'y ~ x') +
  ggtitle("Plot 4: avrMSL v MeanWindPower Depending on Station by Quarter") +
  xlab("avrMSL") + 
  ylab("MeanWindPower")
plot_4                          
        
