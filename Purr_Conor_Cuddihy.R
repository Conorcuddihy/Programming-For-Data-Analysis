# Conor Cuddihy 
# 14415362
# MSc Data Analytics

install.packages("aimsir17")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("purrr")
install.packages("tidyr")
library(tidyr)
library(lubridate)
library(tibble)
library(aimsir17)
library(dplyr)
library(ggplot2)
library(purrr)
library(ggpubr)

target <- c("BELMULLET", "MACE HEAD","NEWPORT","ROCHES POINT","SherkinIsland", "VALENTIA OBSERVATORY")

daily<-observations %>% 
  filter(station %in% target)  %>% 
  group_by(station,month,day) %>%  
  summarize(avrMSL = mean(msl, na.rm = TRUE),MaxWind=max(wdsp))

daily

unique(daily$station)

plot_1 <- ggplot(daily, aes(x=avrMSL, y=MaxWind)) + 
  geom_point() + 
  facet_wrap(daily$station ~ .) +
  ylim(0, 60) +
  geom_smooth(method = 'lm',formula = 'y ~ x') +
  ggtitle("Plot 1: avrMSL v MaxWind Depending on Station") +
  xlab("Mean Daily Sea Level Pressure") + 
  ylab("Maximun Daily Wind Speed")
plot_1

nested <- daily %>%
  group_by(station) %>%
  nest()
nested

models <- nested %>%
  mutate(lm_obs = map(data,
                      ~lm(avrMSL ~ MaxWind, data = .)))
models

results <- daily %>%
  group_by(station) %>%
  group_split(station) %>%
  map_df(~{
    mod <- lm(.$MaxWind ~ .$avrMSL)
    summ <- summary(mod)
    tibble(station = first(.$station),
           Intercept = mod$coefficients[1],
           Slope = mod$coefficients[2],
           RSquared = summ$r.squared,
           AdjRsquared = summ$adj.r.squared)
  })
View(results)

all <- full_join(models, results)
all

plots <- all %>% 
  select(station, data, Intercept, Slope, AdjRsquared) %>% 
  pmap(~{
    ggplot(..2,aes(x=.data$avrMSL,y=.data$MaxWind))+
      geom_point()+
      geom_abline(slope=..4,intercept = ..3,size=3.0,colour="red",...)+
      geom_smooth(method="lm") +
      ylab("Maximum Daily Wind Speed")  + 
      xlab("Mean Daily Sea Level Pressure") +
      ggtitle(paste(..1,'[Beta1=',round(..4,digits=3),']','[Beta0=',round(..3,digits=3),']','[RSqr=',round(..5,digits=3),']'))+
      theme(plot.title = element_text(face='italic',size=9))+
      theme(axis.title.x = element_text(size=10))+
      theme(axis.title.y = element_text(size=8))
  })

ggarrange(plotlist=plots, 
          ncol = 2,
          nrow = 3)

