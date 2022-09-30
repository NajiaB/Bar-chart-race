# JE VEUX : creer une animation (bar chart) des 10 departements les plus touchés de fr

#JE CHARGE LES DONNEES: (deces) -------------------------------------------------

setwd('C:/Users/Najia/Documents/R')
options(max.print=100000000)

library(questionr)
setwd('C:/Users/Najia/Documents/R/démarche scientifique')
newcovid=read.table("donnees-hospitalieres-nouveaux-covid19-2021-01-12-19h03.csv",sep=';',header = TRUE,stringsAsFactors = TRUE,dec=',')
names(newcovid)[1]='code_insee'  #psk dans l'autre tableau de la carte j'ai code insee,pour faciliter le merge
newcovid$code_insee <- sprintf("%02d", as.numeric(as.character(newcovid$code_insee)))
newcovid$jour = as.Date(newcovid$jour,format="%d/%m/%y")
str(newcovid)

#pour avoir le nom des deps je join avec l'autre tableau

library(dplyr)

a =  read.csv('departements-france.csv',sep=',',header = TRUE, stringsAsFactors = TRUE)##j'importe les données des departement fr
names(a)[1:2]=c('code_insee','nom')
str(a)
joined=select(a,code_insee,nom,nom_region) %>%
  inner_join(newcovid,'code_insee')  #contient date dep et num de dep + nombre de deces

montableau= joined %>%
  select(code_insee,nom,nom_region,jour,incid_dc)%>%
  group_by(nom,jour)%>%
  summarise(total=sum(incid_dc)) %>%
  mutate(cumul=cumsum(total)) %>%#cree nvl colonne de somme cumulée 
  group_by(jour) %>% #comme ça je pourrai slctn based on the date
  arrange(jour,-cumul) %>%
  mutate(rang= 1:n()) %>%
  filter(rang<= 10)  #je prends uniquemn rang>10

# JE PLOT --------------------------
library(tidyverse)
montableau %>%
  ggplot() +
  aes(xmin=0,xmax=cumul) +
  aes(ymin=rang -0.45 , ymax = rang +0.45, y=rang) +
  facet_wrap(~ jour)+
  geom_rect(alpha=.7)+
  aes(fill=nom)+ #CICICICICIC
  scale_fill_viridis_d(option="magma",direction= -1)+
  scale_x_continuous(
    limits=c(-5000,10000),
    breaks=c(-5000,0,1000,2000,3000,4000,5000))+
  
  geom_text(col="darkblue",hjust="right",aes(label=nom),
            x=-100)+
  geom_text(col="darkblue",hjust="right",
            aes(label=paste(cumul),x=5000))+
  scale_y_reverse() +
  labs(fill=NULL)+
  
  
  ggtitle("l'évolution du nombre de morts par département") +
  theme_classic() -> 
  plotstatique

#mnt je l'anime -----------
#install.packages("gifski")
library(gifski)
library(ggplot2)
library(tidyverse)
library(gganimate)

plotanime= plotstatique +
  facet_null() +
  geom_text( x=4000, y=-10, family="TT Times New Roman",
             aes(label=as.character(jour)),
             size=10 ,col="grey18")+
  aes(group=nom) +
  transition_time(jour)
  #gganimate::transition_time(jour) 
  
animate(plotanime,nframes=300, fps=10,width=1000,height=700)

#gif=animate(plotanime,nframes=300, fps=10,width=1000,renderer = gifski_renderer("gganim2.gif"),end_pause=15)


#mp4=animate(plotanime,nframes=300, fps=10,width=1000,renderer = ffmpeg_renderer("gganim2.mp4"))
anim_save("gganim3.mp4")








