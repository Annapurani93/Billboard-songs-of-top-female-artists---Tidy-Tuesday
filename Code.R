library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-09-14')
library(tidyverse)
library(dplyr)
library(patchwork)
tuesdata$billboard->billboard
tuesdata$audio_features->audio
library(cowplot)

glimpse(billboard)
glimpse(audio)
glimpse(song)


billboard%>%left_join(audio, by="song_id")->song
lubridate::mdy(song$week_id)->song$week_id

song

song%>%filter(performer.x=="Taylor Swift")->ts
ts%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->tssongs1

song%>%filter(performer.x=="Billie Eilish")->be
be%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->besongs1

song%>%filter(performer.x=="Dua Lipa")->dl
dl%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->dlsongs1
dlsongs1

song%>%filter(performer.x=="Megan Thee Stallion")->mts
mts%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->mtssongs1
mtssongs1

song%>%filter(performer.x=="Lizzo")->l
l%>%filter(peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->lsongs1
lsongs1

song%>%filter(performer.x=="Ariana Grande")->ag
ag%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))%>%arrange(-weeks_on_chart)->agsongs1
agsongs1

song%>%filter(performer.x=="Halsey")->h
h%>%filter(peak_position>=1 & peak_position<=10)%>%select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%slice(which.max(weeks_on_chart))->hsongs1
hsongs1

song%>%filter(performer.x=="Tones And I")->ti
ti%>%
  filter(peak_position>=1 & peak_position<=10)%>%
  select(performer.x, song.x,peak_position, weeks_on_chart)%>%
  group_by(song.x)%>%
  slice(which.max(weeks_on_chart))->tisongs1
tisongs1


rbind(tssongs1,besongs1,dlsongs1,mtssongs1,lsongs1,agsongs1,hsongs1,tisongs1)->s4

ggplot(s4, 
       aes(x=weeks_on_chart,
           y=reorder(song.x,weeks_on_chart, decreasing=TRUE)))+
  geom_col(fill="coral1")+
  facet_wrap(~performer.x, scales = "free", ncol = 1)+
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme(axis.text.y = element_text(size = 14, colour="white", hjust = 1, face="bold", family = "Fira Sans"),
        plot.margin = margin(rep(15, 4)))+
  theme(axis.text.x=element_text(size=12, colour="white", face="bold"))+
  theme(axis.line = element_blank())+
  labs(title="Billboard Hits of female artists", subtitle = "These plots map the songs - that peaked in 1-10 spots - of the 2020 top 10 Billboard female artists. They are ordered based on the number of weeks the songs stayed in the 1-10 spots",
       caption = 'Data: Data.World via Sean Miller|Design and Analysis: @annapurani93')+
  theme(plot.title = element_text(hjust = 0.5, size = 30, colour="white", face = "bold"),
        plot.subtitle = element_text(hjust= 0.5, size = 12, colour = "white", face="bold" ),
        plot.caption =  element_text(size = 16, colour = "white"))+
  theme(panel.grid = element_blank() )+
  theme(axis.title.y = element_blank())+
  theme(plot.background  = element_rect(fill = "black"), panel.background = element_rect(fill="black"))->sgraph
sgraph


ggsave("sgraph.png",sgraph, width=20, height=40)
