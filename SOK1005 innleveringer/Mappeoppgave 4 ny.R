library(rvest)
library(tidyverse)
library(rlist)
library(purrr)
library(janitor)
timenow<-Sys.time()

#browseURL("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list")
#browseURL("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module[]=SOK-1006-1&week=1-20&View=list")
#browseURL("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module[]=SOK-1016-1&week=1-20&View=list")

#for å holde enklere styr på alt
SOK1005 <- "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list" 

SOK1006 <- "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module[]=SOK-1006-1&week=1-20&View=list"

SOK1016 <- "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module[]=SOK-1016-1&week=1-20&View=list"

#funksjon for en enkel timeplan
Timeplan_Lenke <- function(x) {
  timeplan <- read_html(x) 
  timeplan <- html_nodes(timeplan, 'table') 
  timeplan <- html_table(timeplan, fill=TRUE) 
  timeplan <- list.stack(timeplan)
  colnames(timeplan) <- timeplan[1,]
  timeplannavn  <- timeplan %>% 
    filter(!Dato=="Dato") %>% 
    separate(Dato, 
             into = c("Dag", "Dato"),
             sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
    fill(Dato, .direction = "down")
  timeplannavn$Dato <- as.Date(timeplannavn$Dato, format="%d.%m.%Y") 
  timeplannavn$Uke <- strftime(timeplannavn$Dato, format = "%V") 
  
  timeplanferdignå <- timeplannavn %>% 
    select(Dato,Uke,Tid,Emnekode,Rom,Lærer)
  return(timeplanferdignå)
}

Timeplan_Lenke(SOK1016)

#Funksjon for alle 3 timeplanene
Timeplan_Alle <- function(x,y,z) {
  timeplan1 <- read_html(x) 
  timeplan1 <- html_nodes(timeplan1, 'table') 
  timeplan1 <- html_table(timeplan1, fill=TRUE) 
  timeplan1 <- list.stack(timeplan1)
  colnames(timeplan1) <- timeplan1[1,]
  
  timeplan2 <- read_html(y) 
  timeplan2 <- html_nodes(timeplan2, 'table') 
  timeplan2 <- html_table(timeplan2, fill=TRUE) 
  timeplan2 <- list.stack(timeplan2)
  colnames(timeplan2) <- timeplan2[1,]
  
  timeplan3 <- read_html(z) 
  timeplan3 <- html_nodes(timeplan3, 'table') 
  timeplan3 <- html_table(timeplan3, fill=TRUE) 
  timeplan3 <- list.stack(timeplan3)
  colnames(timeplan3) <- timeplan3[1,]
  
  
  
  timeplannavn <- list(timeplan1,timeplan2,timeplan3)
  
  
  timeplannavn <- imap(timeplannavn, ~ .x %>% 
                         filter(!Dato=="Dato"))
  
  
  
  timeplannavn <- imap(timeplannavn, ~.x %>% 
                         separate(Dato, 
                                  into = c("Dag", "Dato"), 
                                  sep = "(?<=[A-Za-z])(?=[0-9])"))
  
  
  timeplannavn <- imap_dfr(timeplannavn, ~.x %>% 
                             fill(Dato, .direction = "down"))
  
  timeplannavn$Dato <- as.Date(timeplannavn$Dato, format="%d.%m.%Y")
  
  
  timeplannavn$Uke <- strftime(timeplannavn$Dato, format = "%V") 
  
  timeplanferdignå <- timeplannavn %>% 
    select(Dato,Uke,Tid,Emnekode,Rom,Lærer)
  
  
  return(timeplanferdignå)
}

Timeplan_Alle(SOK1005,SOK1006,SOK1016)

#Sortert etter dato
Timeplan_Alle_Dato <- function(x,y,z) {
  timeplan1 <- read_html(x) 
  timeplan1 <- html_nodes(timeplan1, 'table') 
  timeplan1 <- html_table(timeplan1, fill=TRUE) 
  timeplan1 <- list.stack(timeplan1)
  colnames(timeplan1) <- timeplan1[1,]
  
  timeplan2 <- read_html(y) 
  timeplan2 <- html_nodes(timeplan2, 'table') 
  timeplan2 <- html_table(timeplan2, fill=TRUE) 
  timeplan2 <- list.stack(timeplan2)
  colnames(timeplan2) <- timeplan2[1,]
  
  timeplan3 <- read_html(z) 
  timeplan3 <- html_nodes(timeplan3, 'table') 
  timeplan3 <- html_table(timeplan3, fill=TRUE) 
  timeplan3 <- list.stack(timeplan3)
  colnames(timeplan3) <- timeplan3[1,]
  
  
  
  timeplannavn <- list(timeplan1,timeplan2,timeplan3)
  
  
  timeplannavn <- imap(timeplannavn, ~ .x %>% 
                         filter(!Dato=="Dato"))
  
  
  
  timeplannavn <- imap(timeplannavn, ~.x %>% 
                         separate(Dato, 
                                  into = c("Dag", "Dato"), 
                                  sep = "(?<=[A-Za-z])(?=[0-9])"))
  
  
  timeplannavn <- imap_dfr(timeplannavn, ~.x %>% 
                             fill(Dato, .direction = "down"))
  
  timeplannavn$Dato <- as.Date(timeplannavn$Dato, format="%d.%m.%Y")
  
  
  timeplannavn$Uke <- strftime(timeplannavn$Dato, format = "%V") 
  
  timeplanferdignå <- timeplannavn %>% 
    select(Dato,Uke,Tid,Emnekode,Rom,Lærer)
  
  timeplanferdignå <- timeplanferdignå[order(timeplanferdignå$Dato),]
  
  
  return(timeplanferdignå)
}

Timeplan_Alle_Dato(SOK1005,SOK1006,SOK1016)
