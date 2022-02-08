library(tidyverse)
library(jsonlite)
library(ggplot2)

Covid <-fromJSON("https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")
head(Covid)
tail(Covid)

#inspirert av løsningen til https://stackoverflow.com/questions/27433798/how-can-i-change-the-y-axis-figures-into-percentages-in-a-barplot


lvhd <- paste("Lower vaccination rate,
              higher death rate")

ldhv <- paste("Higher vaccination rate,
               lower death rate")
#prøvde mitt beste men klarte ikke å forkorte navenene med en kode, eller å få bort 0 tallet på y aksen.
 Covidplot<- Covid %>% 
  ggplot(aes(x=fully_vaccinated_pct_of_pop, y=deaths_per_100k, group=name, label=name))+ 
  geom_point(size = 2.5, color= "turquoise") +geom_text(hjust=0.5, vjust=-1.2, size = 3, check_overlap = TRUE, ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),expand = c(0,0),  breaks = seq(0.45,0.8,0.05),limits = c(0.45,0.8)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,20,5)) +
  scale_linetype_manual(name="Legend",values=c(MNAC="solid", CaixaForum="dashed")) +
  labs(title = "Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       subtitle = "20 avg. monthly deaths per 100,000") +
  annotate("text",0.6,18,label=lvhd,color="black",hjust= 1, size = 3) + 
  annotate("text",0.78,9,label=ldhv,color="black",hjust= 1, size = 3,) +
  geom_segment(aes(x=0.78, y= 8, xend = 0.79,yend=7.5),arrow = arrow(length = unit(0.3, "cm")), size = 1) +
  geom_segment(aes(x=0.535, y= 18.5, xend = 0.525,yend=19),arrow = arrow(length = unit(0.3, "cm")), size =1) +
  labs(x = "Share of total population fully vaccinated", y = "")
print(Covidplot)

#Oppgave 2
lm(deaths_per_100k~fully_vaccinated_pct_of_pop, data = Covid)
print(lm(deaths_per_100k~fully_vaccinated_pct_of_pop, data = Covid))



#En med allt fra oppg1 
oppg2a <- ggplot(Covid, aes(fully_vaccinated_pct_of_pop,deaths_per_100k, label=name)) + geom_point(size = 2.5, color= "turquoise") +
  geom_text(hjust=0.5, vjust=-1.2, size = 3, check_overlap = TRUE, ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),expand = c(0,0),  breaks = seq(0.45,0.8,0.05),limits = c(0.45,0.8)) +
  scale_y_continuous(expand = c(0,0), limits= c(0,20,5)) +
  scale_linetype_manual(name="Legend",values=c(MNAC="solid", CaixaForum="dashed")) +
  labs(title = "Covid-19 deaths since universal adult vaccine eligibility compared with vaccination rates",
       subtitle = "20 avg. monthly deaths per 100,000") +
  annotate("text",0.6,18,label=lvhd,color="black",hjust= 1, size = 3) + 
  annotate("text",0.78,9,label=ldhv,color="black",hjust= 1, size = 3,) +
  geom_segment(aes(x=0.78, y= 8, xend = 0.79,yend=7.5),arrow = arrow(length = unit(0.3, "cm")), size = 1) +
  geom_segment(aes(x=0.535, y= 18.5, xend = 0.525,yend=19),arrow = arrow(length = unit(0.3, "cm")), size =1) +
  labs(x = "Share of total population fully vaccinated", y = "")

oppg2a + geom_smooth(method = lm)

#samme graf men er ikke lagt inn scale limits visste ikke helt hva som spurtes etter med plot + geom smooth om
# det var ny eller samme.
oppg2b <- ggplot(Covid, aes(fully_vaccinated_pct_of_pop,deaths_per_100k, label=name)) + geom_point() +
  geom_text(hjust=0.5, vjust=-1.2, size = 3, check_overlap = TRUE)

oppg2b + geom_smooth(method = lm)

# jeg tolker at for at linjen er en formel som består av intercept og fvpop(fully vaccinated pct of pop)
#der disse er de konstante tallene vi har i formelen. jeg antar når intercepten øker med 31.15 så endres fvpop med 
#-36.66 * x som ender med linjen vi ser i grafen.

