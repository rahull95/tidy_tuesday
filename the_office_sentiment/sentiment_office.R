#install.packages("schrute")
#install.packages("ggrepel")

library(schrute)
library(tidytext)
library(tidyverse)
library(knitr)
library(textdata)
library(ggthemes)
library(ggdark)
library(gridExtra)
library(ggrepel)

df1 = schrute::theoffice
View(df1)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
View(office_ratings)

token_df = df1 %>% unnest_tokens(word, text)
head(token_df)

afinn = get_sentiments("afinn")

df2 = token_df %>% select(season, episode, word) %>% inner_join(afinn)
head(df2)

df2$season = as.character(df2$season)
df2$episode = as.character(df2$episode)

df3 = df2 %>% group_by(season, episode) %>% summarise(avg_sentiment = mean(value))

df3$season = as.numeric(df3$season)
df3$episode = as.numeric(df3$episode)

#df3 %>% filter(season == "2") %>% ggplot(aes(x=episode, y=avg_sentiment)) + geom_line() + geom_point()
#ggplot(df3, aes(x=episode, y=avg_sentiment)) + geom_line() + facet_wrap(~season, ncol=3) + theme_fivethirtyeight()

df4 = df3 %>% inner_join(office_ratings, by = c("season" = "season", "episode" = "episode"))

#Season1
df5 = df4 %>% filter(season == 1)
p1 = df5 %>% ggplot(aes(x=episode, y=avg_sentiment)) + geom_point() + geom_line() +
    scale_x_continuous(breaks=seq(min(df5$episode), max(df5$episode), by = 1)) + dark_theme_minimal() +
    geom_text(aes(label=ifelse(episode %in% c(1,6),as.character(title),'')),hjust=0.4,vjust=0)
    
#Season2
df5 = df4 %>% filter(season == 2)
p2 = df5 %>% ggplot(aes(x=episode, y=avg_sentiment)) + geom_point() + geom_line() +
    scale_x_continuous(breaks=seq(min(df5$episode), max(df5$episode), by = 1)) + dark_theme_minimal() +
    geom_text(aes(label=ifelse(episode %in% c(6,22),as.character(title),'')),hjust=0.3,vjust=0)

#Season3
df5 = df4 %>% filter(season == 3)
p3 = df5 %>% ggplot(aes(x=episode, y=avg_sentiment)) + geom_point() + geom_line() +
    scale_x_continuous(breaks=seq(min(df5$episode), max(df5$episode), by = 1)) + dark_theme_minimal() +
    geom_text(aes(label=ifelse(episode %in% c(2,4,13),as.character(title),'')),hjust=0.3,vjust=0)

#Season4
df5 = df4 %>% filter(season == 4)
p4 = df5 %>% ggplot(aes(x=episode, y=avg_sentiment)) + geom_point() + geom_line() +
    scale_x_continuous(breaks=seq(min(df5$episode), max(df5$episode), by = 1)) + dark_theme_minimal() +
    geom_text(aes(label=ifelse(episode %in% c(3,14),as.character(title),'')),hjust=0.3,vjust=0)

#Season5
df5 = df4 %>% filter(season == 5)
p5 = df5 %>% ggplot(aes(x=episode, y=avg_sentiment)) + geom_point() + geom_line() +
    scale_x_continuous(breaks=seq(min(df5$episode), max(df5$episode), by = 1)) + dark_theme_minimal() +
    geom_text(aes(label=ifelse(episode %in% c(6,9,14),as.character(title),'')),hjust=0.3,vjust=0)

#Season6
df5 = df4 %>% filter(season == 6)
p6 = df5 %>% ggplot(aes(x=episode, y=avg_sentiment)) + geom_point() + geom_line() +
    scale_x_continuous(breaks=seq(min(df5$episode), max(df5$episode), by = 1)) + dark_theme_minimal() +
    geom_text(aes(label=ifelse(episode %in% c(10,14,21),as.character(title),'')),hjust=0.3,vjust=0)

#Season7
df5 = df4 %>% filter(season == 7)
p7 = df5 %>% ggplot(aes(x=episode, y=avg_sentiment)) + geom_point() + geom_line() +
    scale_x_continuous(breaks=seq(min(df5$episode), max(df5$episode), by = 1)) + dark_theme_minimal() +
    geom_text(aes(label=ifelse(episode %in% c(1,13,24),as.character(title),'')),hjust=0.3,vjust=0)

#Season8
df5 = df4 %>% filter(season == 8)
p8 = df5 %>% ggplot(aes(x=episode, y=avg_sentiment)) + geom_point() + geom_line() +
    scale_x_continuous(breaks=seq(min(df5$episode), max(df5$episode), by = 1)) + dark_theme_minimal() +
    geom_text(aes(label=ifelse(episode %in% c(4,5,11,17,20),as.character(title),'')),hjust=0.3,vjust=0)

#Season9
df5 = df4 %>% filter(season == 9)
p9 = df5 %>% ggplot(aes(x=episode, y=avg_sentiment)) + geom_point() + geom_line() +
    scale_x_continuous(breaks=seq(min(df5$episode), max(df5$episode), by = 1)) + dark_theme_minimal() +
    geom_text(aes(label=ifelse(episode %in% c(3,8,10,17,20),as.character(title),'')),hjust=0.3,vjust=0)
