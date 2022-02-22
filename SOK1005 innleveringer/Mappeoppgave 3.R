library(rvest)
library(ggplot2)
library(tidyverse)
library(rlist)

#OPPGAVE1
motor <-"https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"

motorside <- read_html(motor)

motorside2 <- motorside %>% 
  html_table( header = TRUE, fill = TRUE) 

#fjerner de to radene med bagasje på seg som mangler y value
test <- motorside2[[1]] %>% 
  slice(-c(19, 26))

str(test)

#brukt en løsning som er inspirert fra https://www.delftstack.com/howto/r/remove-last-character-in-r/

test$`WLTP-tall`<-gsub('.{9}$', '', test$`WLTP-tall`)

test$`WLTP-tall`<-gsub("km","",as.character(test$`WLTP-tall`))

test$STOPP<-gsub("km","",as.character(test$STOPP))

test[2] <- as.numeric(unlist(test[2]))
test[3] <- as.numeric(unlist(test[3]))

str(test)

test %>% 
  ggplot(aes(x=`WLTP-tall`,y=STOPP)) + geom_point() +
  scale_x_continuous(expand = c(0,0), limits = c(200,675)) + 
  scale_y_continuous(expand = c(0,0), limits = c(200,675)) +
  geom_abline(slope = 1, color = "red", cex = 1) +
  labs(x = "WLTP", y = "stopp") +
  labs(title = "Forhold til stopp og WLTP sammenlignet med forventet kjørelengde i km")

#OPPGAVE 2
lm(STOPP~`WLTP-tall`, data = test)

#vi kan tolke at tallene forteller oss at når x øker så får vi et høyere stopp fart.
# den tilpassende linjen er også relativt lavere en linjen vår med at vi har -26, ulikt
# x = 0 som er det som vi har på vår forventet kjørelengde.

test %>% 
  ggplot(aes(x=`WLTP-tall`,y=STOPP)) + geom_point() +
  scale_x_continuous(expand = c(0,0), limits = c(200,675)) + 
  scale_y_continuous(expand = c(0,0), limits = c(200,675)) +
  geom_abline(slope = 1, color = "red", cex = 1) +
  labs(x = "WLTP", y = "stopp") +
  labs(title = "Forhold til stopp og WLTP sammenlignet med forventet kjørelengde i km")+
  geom_smooth(method = lm)
