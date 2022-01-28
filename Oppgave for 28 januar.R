library(dplyr)
library(ggplot2)
library(readr)
library(zoo)
library(tidyverse)
library(plyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
#Oppgave 1
lt_data <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt", 
                       col_types = cols(Year = col_number(), 
                                        Mo = col_number(), Globe = col_number(), 
                                        Land = col_number(), Ocean = col_number(), 
                                        NH = col_number(), Land_1 = col_number(), 
                                        Ocean_1 = col_number(), SH = col_number(), 
                                        Land_2 = col_number(), Ocean_2 = col_number(), 
                                        Trpcs = col_number(), Land_3 = col_number(), 
                                        Ocean_3 = col_number(), NoExt = col_number(), 
                                        Land_4 = col_number(), Ocean_4 = col_number(), 
                                        SoExt = col_number(), Land_5 = col_number(), 
                                        Ocean_5 = col_number(), NoPol = col_number(), 
                                        Land_6 = col_number(), Ocean_6 = col_number(), 
                                        SoPol = col_number(), Land_7 = col_number(), 
                                        Ocean_7 = col_number(), USA48 = col_number(), 
                                        USA49 = col_number(), AUST = col_number()))
View(lt_data)


lt_pt <- lt_data %>% 
  slice(1:517) 


lt_pt <- lt_pt %>% 
  mutate(roll_mean = rollmean(x = Globe, k = 13, align = c("left"), fill = NA))


lt_pt %>% 
  ggplot(aes(x = 1:517, group = 1, y = Globe)) +
  geom_point(aes( y= Globe),color='lightblue') +
  geom_line(aes(y= Globe),color='lightblue') +
  geom_line(aes(y=roll_mean), color = "red") + 
  labs(y= "T Departure from '91`-20 Avf. (deg. C')" +
  scale_y_continuous( limits = c(-0.7,0.9), breaks = seq(-0.7,0.9,0.1))+
  theme(axis.text.x = element_text(angle = 90),axis.ticks.length=unit(0, "pt")) 






#Oppgave 2
