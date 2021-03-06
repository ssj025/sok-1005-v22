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
library(reshape2)
library(xts)
library(lubridate)
library(cowplot)
library(gridExtra)
#Oppgave 1

#Benytt R og last ned dataene fra "Lower-Troposphere" og lag et ggplot som ligner figuren p� nettsiden. 
#I datasettet er det variabelen Globe som er benyttet. Merk at dataene inneholder noe "un�dig" informasjon i slutten av fila som du ikke skal ha med. 
#N�r du skriver kode som leser data, fors�k � gj�re koden s� generell at den ogs� fungerer neste m�ned n�r det er kommet en ny rad inn p� slutten av datafila.
#For � beregne et 13-m�neders glidende-gjennomsnitt ("centered moving-average") kan du f.eks benytte funksjonen zoo::rollmean(). 
#"Zoo" er R biblioteket, og "rollmean" er navnet p� funksjonen. Gj�r ditt beste for � f� plottet til � se bra og selvforklarende ut.
#Det m� ha en tittel, og meningsfulle titler p� akser.



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

#Pr�vde med Year p� x men ender opp med en helt anenrledes graf. s� klarte 
#desverre ikke � gjenskape den.
lt_pt %>% 
  ggplot(aes(x = 1:517,group = 1,  y = Globe)) +
  geom_point(aes( y= Globe),color='lightblue', cex=2) +
  geom_line(aes(y= Globe),color='lightblue', cex = 1) + 
  labs(x = "�r 1978 - 2021", y = "T Departure from '91-'20 Avg. (deg. C)") +
  geom_line(aes(y=roll_mean), color = "red", cex = 1) +
  labs(title = "Latest Global Average Tropspheric Tempratures") + 
  scale_y_continuous(expand = c(0,0), limits = c(-0.7,0.9), breaks = seq(-0.7,0.9,0.1))+
  scale_x_continuous(expand = c(0,0))+ 
  theme(axis.text.x = element_text(angle = 90)) +
  annotate("text", x = 320, y = -0.45, label = "Running, centered 13 month average", color = "red")+ 
  geom_segment(aes(x = 300, y = -0.4, xend = 330, yend = -0.2),
               arrow = arrow(length = unit(0.5, "cm")), color = "red", cex = 1)


#Oppgave 2

#P� nettsiden finner vi lenker til temperaturdata p� fire niv� i atmosf�ren. I denne oppgaven skal du skrive kode som leser alle fire datasettene,
#og sl�r dem sammen til et datasett. Fra disse sammensl�tte dataene skal vi trekke ut variabelen NoPol som er temperatur fra 60 til 90 grader nord.
#Lag et ggplot som viser temperaturen fra disse fire niv�ene samtidig, og legg til et femte niv� som er gjennomsnittet av de fire niv�ene i atmosf�ren.
#Gj�r ditt beste for � f� plottet til � se bra og selvforklarende ut. Det m� ha en tittel og meningsfulle titler p� akser og kategorier.
#N�r du skriver kode som leser data, fors�k � gj�re koden s� generell at den ogs� fungerer neste m�ned n�r det er kommet en ny rad inn p� slutten av datafila.
#Fors�k ogs� � gj�re koden din s� effektiv at du ikke kopierer all kode fra et atmosf�risk niv� til det neste, dvs at koden din blir 4 ganger s� stor som n�dvendig.

mt_tp <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt",
                     col_types = cols(Year = col_number(),NoPol = col_number()))
View(mt_tp)

tp <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt",
                  col_types = cols(Year = col_number(),NoPol = col_number()))
View(tp)

ls_pt <- read_table2("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt",
                     col_types = cols(Year = col_number(),NoPol = col_number()))
View(lt_pt)

ltpt_pt_NoPol <- lt_pt %>% 
  slice(1:517) %>% 
  select(1,2,21) %>% 
  dplyr::rename(LowerTropoNoPol = NoPol)

mttp_NoPol <- mt_tp %>% 
  slice(1:517) %>% 
  select(21) %>% 
  dplyr::rename(MiddleTropoNoPol = NoPol)

tp_Nopol <- tp %>% 
  slice(1:517) %>% 
  select(21) %>% 
  dplyr::rename(TropoNoPol = NoPol)

ls_pt_NoPol <- ls_pt %>% 
  slice(1:517) %>% 
  select(21) %>% 
  dplyr::rename(LowerStratoNoPol = NoPol)

all_nopol <- cbind(ltpt_pt_NoPol,mttp_NoPol,tp_Nopol, ls_pt_NoPol)

LT <- all_nopol %>% 
  ggplot(aes(x = Year,  y = LowerTropoNoPol)) +
  geom_line(color='green', cex = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(-10,10), breaks = seq(-10,10,1.5))+
  labs(title = "NoPol Lower Troposphere")+
  labs( y = "NoPol")



MT<- all_nopol %>% 
  ggplot(aes(x = Year,  y = MiddleTropoNoPol)) +
  geom_line(color='red', cex = 1)+
  scale_y_continuous(expand = c(0,0), limits = c(-10,10), breaks = seq(-10,10,1.5))+
  labs(title = "NoPol Middle Troposphere")+
  labs( y = "NoPol")



TR<- all_nopol %>% 
  ggplot(aes(x = Year,  y = TropoNoPol)) +
  geom_line(color='lightblue', cex = 1)+
  scale_y_continuous(expand = c(0,0), limits = c(-10,10), breaks = seq(-10,10,1.5))+
  labs(title = "NoPol Troposphere")+
  labs( y = "NoPol")


LS<- all_nopol %>% 
  ggplot(aes(x = Year,  y =LowerStratoNoPol)) +
  geom_line(color='yellow', cex = 1)+
  scale_y_continuous(expand = c(0,0), limits = c(-10,10), breaks = seq(-10,10,1.5))+
  labs(title = "NoPol Lower Stratosphere")+
  labs( y = "NoPol")



all_nopol$MeanNoPolAll <- rowMeans(all_nopol[ , c(3,6)], na.rm=TRUE)

Mean_NP<- all_nopol %>% 
  ggplot(aes(x = Year,  y =MeanNoPolAll)) +
  geom_line(color='purple', cex = 1)+
  scale_y_continuous(expand = c(0,0), limits = c(-10,10), breaks = seq(-10,10,1.5)) +
  labs(title = "NoPol Mean all graph") +
  labs( y = "NoPol")



ALT <- all_nopol %>% 
  ggplot(aes(x = Year)) + 
  geom_line(y =all_nopol$LowerStratoNoPol, color='yellow', cex = 1)+
  geom_line(y =all_nopol$MeanNoPolAll, color='purple', cex = 1)+
  geom_line(y = all_nopol$TropoNoPol, color='lightblue', cex = 1)+
  geom_line(y = all_nopol$MiddleTropoNoPol, color='green', cex = 1) +
  geom_line(y = all_nopol$LowerTropoNoPol, color='red', cex = 1)+
  scale_y_continuous(expand = c(0,0), limits = c(-10,10), breaks = seq(-10,10,1.5)) +
  labs(title = "NoPol all graphs in one") +
  labs( y = "NoPol")

ALT2 <- all_nopol %>% 
  ggplot(aes(x = Year)) + 
  geom_line(y =all_nopol$LowerStratoNoPol, color='yellow', cex = 1)+
  geom_line(y = all_nopol$TropoNoPol, color='lightblue', cex = 1)+
  geom_line(y = all_nopol$MiddleTropoNoPol, color='green', cex = 1) +
  geom_line(y = all_nopol$LowerTropoNoPol, color='red', cex = 1) +
  scale_y_continuous(expand = c(0,0), limits = c(-10,10), breaks = seq(-10,10,1.5)) +
  labs(title = "NoPol All Troposphere And Stratosphere") +
  labs( y = "NoPol")

#Var usikker p� om oppgaven deres mente alle grafene s� mean s� alle sammen felles
grid.arrange(LT, MT, TR, LS, Mean_NP, ALT,  ncol=3, nrow =2)

#eller om det var alle de forgje grafene i en s� legge mean inn p� den igjen
grid.arrange(ALT2, ALT, ncol=2, nrow=1)
