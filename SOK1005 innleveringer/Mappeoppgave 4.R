library(rvest)
library(tidyverse)
library(rlist)
library(purrr)
library(janitor)
timenow<-Sys.time()

#browseURL("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list")
#browseURL("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module[]=SOK-1006-1&week=1-20&View=list")
#browseURL("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module[]=SOK-1016-1&week=1-20&View=list")

SOK1005 <- "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list" 

SOK1006 <- "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module[]=SOK-1006-1&week=1-20&View=list"

SOK1016 <- "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module[]=SOK-1016-1&week=1-20&View=list"

timeplan1 <- read_html(SOK1005)
timeplan1 <- html_nodes(timeplan1, 'table') 
timeplan1 <- html_table(timeplan1, fill=TRUE) 
timeplan1 <- list.stack(timeplan1)
#prøvde mitt beste med map å få det til men fikk bare en haug med
#Error: Index 1 must have length 1, not 6 eller Error in test[1, ] : incorrect number of dimensions

colnames(timeplan1) <- timeplan1[1,]


timeplan2 <- read_html(SOK1006)
timeplan2 <- html_nodes(timeplan2, 'table') 
timeplan2 <- html_table(timeplan2, fill=TRUE) 
timeplan2 <- list.stack(timeplan2)
colnames(timeplan2) <- timeplan2[1,]

timeplan3 <- read_html(SOK1016)
timeplan3 <- html_nodes(timeplan3, 'table') 
timeplan3 <- html_table(timeplan3, fill=TRUE)  
timeplan3 <- list.stack(timeplan3)
colnames(timeplan3) <- timeplan3[1,]


tp <- list(timeplan1,timeplan2,timeplan3)


tp2 <- imap(tp, ~ .x %>% 
                filter(!Dato=="Dato"))
  


tp2 <- imap(tp2, ~.x %>% 
         separate(Dato, 
                into = c("Dag", "Dato"), 
                sep = "(?<=[A-Za-z])(?=[0-9])"))


tp2 <- imap_dfr(tp2, ~.x %>% 
                fill(Dato, .direction = "down"))

tp2$Dato <- as.Date(tp2$Dato, format="%d.%m.%Y")


tp2$Uke <- strftime(tp2$Dato, format = "%V") 


timeplanferdig <- tp2 %>% 
  select(Dato,Uke,Tid,Emnekode,Rom,Lærer)


#sortert etter fag  
timeplanferdig


#sortert etter uke
timeplanferdig[order(timeplanferdig$Dato),]
     
